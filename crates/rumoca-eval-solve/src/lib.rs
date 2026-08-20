//! Solve-IR row evaluation.
//!
//! ## Threading and Test Isolation
//!
//! Simulation facades should pass model-local table data through
//! [`RowEvalContext::external_tables`]. Impure random-generator streams are
//! carried by [`SimulationRuntimeState`] and are never process-global.

// SPEC_0021 file-size exception - split plan: extract external-table access and the impure random-stream runtime state into eval-solve/src/runtime_state.rs, leaving this file as the row-evaluation facade; tracked as RDD2/GALEC cleanup debt (SPEC_0021 follow-up).

use std::{
    cell::RefCell,
    collections::BTreeMap,
    sync::{
        Arc, Mutex, OnceLock,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
    time::Instant,
};

use rumoca_ir_solve::{
    BinaryOp, CompareOp, LinearOp, Reg, ScalarProgramBlock, ScalarProgramRegisterFlow,
    SolveEventActionKind, SolveEventMessagePart, SolveEventPartition,
    SolveProblemShapeContractError, SolvePureCallDirectionalSite, SolvePureCallSite,
    SolvePureCallTable, SolveScalarType, SolveStringConversionFormat, SolveStringConversionSource,
    SolveValueKind, SolveValueType, UnaryOp, resolve_indexed_slot,
};

mod compute_block_scalarize;
pub mod linear_solve;
pub mod nan_trace;
mod ops;
mod prepared;
mod prepared_event_transaction;
mod prepared_guarded_assignment;
mod random_runtime;
pub mod refresh_plan;
pub mod reverse;
#[cfg(test)]
mod scalar_program_contract_tests;
mod sparsity;
mod table_runtime;
pub mod tensor_policy;
mod typed_program;
mod update_rows;
pub use compute_block_scalarize::{
    ScalarProgramProjection, ScalarizeError, checked_contiguous_output_count,
    checked_tensor_output_count, scalar_program_output_count, scalar_program_output_indices,
    tensor_output_indices, to_scalar_program_block, to_scalar_program_projection,
};
use linear_solve::{solve_component_op, solve_component_unchecked};
pub(crate) use ops::{eval_binary, eval_compare, eval_unary};
pub use prepared::{
    ComputeNodeOutputRangeRequest, PreparedComputeBlock, PreparedScalarProgramBlock,
    TargetAssignmentOutputRequest, target_assignment_shape, target_assignment_shapes,
};
pub use prepared_event_transaction::PreparedEventTransactionProgram;
pub use prepared_guarded_assignment::PreparedGuardedAssignmentProgram;
use random_runtime::{
    ImpureRandomState, impure_random_mutex, impure_random_sample, impure_random_stream_id,
    initial_state_values, projected_random_value, random_result_and_state, read_reg_range,
};
pub use sparsity::{
    derive_column_coloring, derive_jacobian_pattern_from_jvp,
    derive_jacobian_pattern_from_scalar_jvp, derive_solve_structural_artifacts,
    row_seed_dependencies,
};
pub use table_runtime::{
    TableRuntimeError, eval_table_bound_value_in, eval_table_lookup_slope_value_in,
    eval_table_lookup_value_in, eval_time_table_next_event_value_in,
};
pub use typed_program::{
    TypedProgramEvalError, TypedValue, TypedValueConstructionError, eval_pure_call,
    eval_pure_call_directional,
};
pub use update_rows::{
    UpdateRowApplication, apply_scalar_slot_value, apply_scalar_slot_value_exact,
    apply_scalar_slot_values, apply_scalar_slot_values_exact, eval_and_apply_update_rows,
};

#[cfg(test)]
pub(crate) fn fixture_pattern(
    rows: usize,
    columns: usize,
    diagonal: bool,
) -> rumoca_ir_solve::StructuralPattern {
    let dependencies = (0..rows)
        .map(|row| {
            if diagonal {
                (row < columns).then_some(row).into_iter().collect()
            } else {
                (0..columns).collect()
            }
        })
        .collect::<Vec<_>>();
    let provenance = rumoca_ir_solve::PatternProvenance::derived(
        rumoca_ir_solve::PatternDerivation::TensorOperand,
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("eval_solve_pattern_fixture.mo"),
            0,
            1,
        ),
    )
    .expect("fixture provenance");
    rumoca_ir_solve::StructuralPattern::from_row_dependencies(
        rows,
        columns,
        &dependencies,
        provenance,
    )
    .expect("fixture pattern")
}

static ROW_EVAL_CALLS: AtomicU64 = AtomicU64::new(0);
static ROW_EVAL_NANOS: AtomicU64 = AtomicU64::new(0);
static ROW_EVAL_TRACE_ACTIVE: AtomicBool = AtomicBool::new(false);

#[derive(Clone, Copy, Debug, Default)]
struct BlockEvalStats {
    calls: u64,
    rows: u64,
}

type BlockEvalStatsMap = BTreeMap<(&'static str, usize), BlockEvalStats>;

#[derive(Debug, Clone, PartialEq)]
pub enum EvalSolveError {
    ExternalTable {
        operation: &'static str,
        table_id: f64,
        column: Option<f64>,
        reason: String,
    },
    MissingInput {
        vector: &'static str,
        index: usize,
        len: usize,
        span: Option<rumoca_core::Span>,
    },
    RegisterOutOfBounds {
        access: &'static str,
        register: Reg,
        len: usize,
        span: Option<rumoca_core::Span>,
    },
    UninitializedRegister {
        register: Reg,
        span: Option<rumoca_core::Span>,
    },
    OutputTooSmall {
        required: usize,
        len: usize,
        span: Option<rumoca_core::Span>,
    },
    UpdateRowTargetMismatch {
        rows: usize,
        targets: usize,
    },
    UpdateDidNotConverge {
        t: f64,
        max_iters: usize,
    },
    SingularTargetAssignment {
        row: usize,
        target_y_index: usize,
        coefficient: f64,
        span: Option<rumoca_core::Span>,
    },
    EventActionConditionMismatch {
        rows: usize,
        actions: usize,
    },
    MissingRuntimeState {
        operation: &'static str,
    },
    RandomStateProjectionOutOfBounds {
        index: usize,
        len: usize,
    },
    InvalidLinearOp {
        helper: &'static str,
        op: &'static str,
    },
    LinearSolve {
        size: usize,
        component: Option<usize>,
        reason: &'static str,
        span: Option<rumoca_core::Span>,
    },
    InvalidRow {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    Scalarization {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    ShapeContract {
        message: String,
        span: Option<rumoca_core::Span>,
    },
}

impl EvalSolveError {
    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::MissingInput { span, .. } => *span,
            Self::RegisterOutOfBounds { span, .. } => *span,
            Self::UninitializedRegister { span, .. } => *span,
            Self::OutputTooSmall { span, .. } => *span,
            Self::SingularTargetAssignment { span, .. } => *span,
            Self::LinearSolve { span, .. } => *span,
            Self::InvalidRow { span, .. } => *span,
            Self::Scalarization { span, .. } => *span,
            Self::ShapeContract { span, .. } => *span,
            _ => None,
        }
    }

    pub(crate) fn with_source_span(self, span: Option<rumoca_core::Span>) -> Self {
        match self {
            Self::MissingInput {
                vector,
                index,
                len,
                span: None,
            } => Self::MissingInput {
                vector,
                index,
                len,
                span,
            },
            Self::RegisterOutOfBounds {
                access,
                register,
                len,
                span: None,
            } => Self::RegisterOutOfBounds {
                access,
                register,
                len,
                span,
            },
            Self::UninitializedRegister {
                register,
                span: None,
            } => Self::UninitializedRegister { register, span },
            Self::OutputTooSmall {
                required,
                len,
                span: None,
            } => Self::OutputTooSmall {
                required,
                len,
                span,
            },
            Self::SingularTargetAssignment {
                row,
                target_y_index,
                coefficient,
                span: None,
            } => Self::SingularTargetAssignment {
                row,
                target_y_index,
                coefficient,
                span,
            },
            Self::LinearSolve {
                size,
                component,
                reason,
                span: None,
            } => Self::LinearSolve {
                size,
                component,
                reason,
                span,
            },
            Self::InvalidRow {
                message,
                span: None,
            } => Self::InvalidRow { message, span },
            error => error,
        }
    }
}

impl std::fmt::Display for EvalSolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExternalTable {
                operation,
                table_id,
                column,
                reason,
            } => {
                if let Some(column) = column {
                    write!(
                        f,
                        "external table {operation} failed for table id {table_id} column {column}: {reason}"
                    )
                } else {
                    write!(
                        f,
                        "external table {operation} failed for table id {table_id}: {reason}"
                    )
                }
            }
            Self::MissingInput {
                vector, index, len, ..
            } => write!(
                f,
                "missing {vector}[{index}] while evaluating Solve-IR row; vector length is {len}"
            ),
            Self::RegisterOutOfBounds {
                access,
                register,
                len,
                ..
            } => write!(
                f,
                "cannot {access} Solve-IR register r{register}; register file length is {len}"
            ),
            Self::UninitializedRegister { register, .. } => {
                write!(f, "cannot read uninitialized Solve-IR register r{register}")
            }
            Self::OutputTooSmall { required, len, .. } => write!(
                f,
                "output buffer too small while evaluating Solve-IR row block: {len} < {required}"
            ),
            Self::UpdateRowTargetMismatch { rows, targets } => write!(
                f,
                "update RHS row count {rows} does not match target count {targets}"
            ),
            Self::UpdateDidNotConverge { t, max_iters } => write!(
                f,
                "update equations did not converge at t={t} after {max_iters} iterations"
            ),
            Self::SingularTargetAssignment {
                row,
                target_y_index,
                coefficient,
                ..
            } => write!(
                f,
                "cannot isolate target y[{target_y_index}] from Solve-IR row {row}: singular coefficient {coefficient}"
            ),
            Self::EventActionConditionMismatch { rows, actions } => write!(
                f,
                "event action condition row count {rows} does not match event action count {actions}"
            ),
            Self::MissingRuntimeState { operation } => write!(
                f,
                "missing simulation runtime state while evaluating Solve-IR {operation}"
            ),
            Self::RandomStateProjectionOutOfBounds { index, len } => write!(
                f,
                "random state projection index {index} is out of bounds for state length {len}"
            ),
            Self::InvalidLinearOp { helper, op } => {
                write!(f, "Solve-IR {helper} helper cannot evaluate {op} op")
            }
            Self::LinearSolve {
                size,
                component,
                reason,
                ..
            } => match component {
                Some(component) => write!(
                    f,
                    "Solve-IR linear solve of size {size} cannot evaluate component {component}: {reason}"
                ),
                None => write!(f, "Solve-IR linear solve of size {size} failed: {reason}"),
            },
            Self::InvalidRow { message, .. } => write!(f, "invalid Solve-IR row: {message}"),
            Self::Scalarization { message, .. } => {
                write!(f, "Solve-IR scalarization failed: {message}")
            }
            Self::ShapeContract { message, .. } => {
                write!(f, "Solve-IR shape contract failed: {message}")
            }
        }
    }
}

impl std::error::Error for EvalSolveError {}

impl From<ScalarizeError> for EvalSolveError {
    fn from(value: ScalarizeError) -> Self {
        Self::Scalarization {
            message: value.to_string(),
            span: value.source_span(),
        }
    }
}

impl From<SolveProblemShapeContractError> for EvalSolveError {
    fn from(value: SolveProblemShapeContractError) -> Self {
        Self::ShapeContract {
            message: value.to_string(),
            span: value.source_span(),
        }
    }
}

pub fn reset_solve_row_eval_trace() {
    let enabled = solve_row_eval_trace_requested();
    ROW_EVAL_TRACE_ACTIVE.store(enabled, Ordering::Relaxed);
    if !enabled {
        return;
    }
    ROW_EVAL_CALLS.store(0, Ordering::Relaxed);
    ROW_EVAL_NANOS.store(0, Ordering::Relaxed);
    block_eval_stats()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .clear();
}

pub fn trace_solve_row_eval_snapshot(label: &str) {
    if !solve_row_eval_trace_active() {
        return;
    }
    let calls = ROW_EVAL_CALLS.load(Ordering::Relaxed);
    let nanos = ROW_EVAL_NANOS.load(Ordering::Relaxed);
    tracing::debug!(
        target: "rumoca_eval_solve::row",
        "{label}: rows={} total={:.3}ms avg={:.3}us",
        calls,
        nanos_to_ms(nanos),
        if calls == 0 {
            0.0
        } else {
            nanos as f64 / calls as f64 / 1.0e3
        }
    );
    let mut block_stats = block_eval_stats()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .iter()
        .map(|(&(kind, len), &stats)| (kind, len, stats))
        .collect::<Vec<_>>();
    block_stats.sort_by_key(|(_, _, stats)| std::cmp::Reverse(stats.rows));
    for (kind, len, stats) in block_stats.into_iter().take(8) {
        tracing::debug!(
            target: "rumoca_eval_solve::row",
            "{label}: block kind={kind} len={len} calls={} rows={}",
            stats.calls, stats.rows
        );
    }
}

fn solve_row_eval_trace_requested() -> bool {
    tracing::enabled!(target: "rumoca_eval_solve::row", tracing::Level::DEBUG)
}

#[inline(always)]
fn solve_row_eval_trace_active() -> bool {
    ROW_EVAL_TRACE_ACTIVE.load(Ordering::Relaxed)
}

#[inline(always)]
pub(crate) fn record_solve_block_eval(kind: &'static str, len: usize, rows: usize) {
    if !solve_row_eval_trace_active() {
        return;
    }
    let mut stats = block_eval_stats()
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    let entry = stats.entry((kind, len)).or_default();
    entry.calls = entry.calls.saturating_add(1);
    entry.rows = entry.rows.saturating_add(rows as u64);
}

fn block_eval_stats() -> &'static Mutex<BlockEvalStatsMap> {
    static STATS: OnceLock<Mutex<BlockEvalStatsMap>> = OnceLock::new();
    STATS.get_or_init(|| Mutex::new(BTreeMap::new()))
}

fn elapsed_nanos_u64(start: Instant) -> u64 {
    start.elapsed().as_nanos().min(u128::from(u64::MAX)) as u64
}

fn nanos_to_ms(nanos: u64) -> f64 {
    nanos as f64 / 1.0e6
}

pub fn hydrate_solve_model_external_tables(model: &rumoca_ir_solve::SolveModel) {
    let _ = model;
}

/// Reset process-global solve evaluator state before a simulation boundary.
pub fn clear_runtime_state() {}

#[derive(Clone, Default)]
pub struct SimulationRuntimeState {
    impure_random: Arc<Mutex<ImpureRandomState>>,
}

/// Opaque continuation state for one solve evaluator instance.
///
/// The contents deliberately remain evaluator-owned: callers can only apply
/// them through [`SimulationRuntimeState::restore`].
#[derive(Clone)]
pub struct SimulationRuntimeStateSnapshot {
    impure_random: ImpureRandomState,
}

impl SimulationRuntimeState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clear(&self) {
        let mut state = self
            .impure_random
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        state.clear();
    }

    pub fn snapshot(&self) -> SimulationRuntimeStateSnapshot {
        let impure_random = self
            .impure_random
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .clone();
        SimulationRuntimeStateSnapshot { impure_random }
    }

    pub fn restore(&self, snapshot: &SimulationRuntimeStateSnapshot) {
        let mut state = self
            .impure_random
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        state.clone_from(&snapshot.impure_random);
    }

    pub fn matches_snapshot(&self, snapshot: &SimulationRuntimeStateSnapshot) -> bool {
        self.impure_random
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .bit_eq(&snapshot.impure_random)
    }
}

/// Runtime-state guard for one simulation/evaluation boundary.
pub struct SimulationContext {
    runtime_state: SimulationRuntimeState,
}

impl SimulationContext {
    pub fn new() -> Self {
        clear_runtime_state();
        Self {
            runtime_state: SimulationRuntimeState::new(),
        }
    }

    pub fn hydrate_solve_model(&self, model: &rumoca_ir_solve::SolveModel) {
        hydrate_solve_model_external_tables(model);
    }

    pub fn runtime_state(&self) -> &SimulationRuntimeState {
        &self.runtime_state
    }
}

impl Default for SimulationContext {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for SimulationContext {
    fn drop(&mut self) {
        self.runtime_state.clear();
        clear_runtime_state();
    }
}

#[derive(Clone, Copy, Default)]
pub struct RowEvalContext<'a> {
    pub seed: Option<&'a [f64]>,
    pub external_tables: Option<&'a [rumoca_core::ExternalTableData]>,
    pub pure_calls: Option<&'a SolvePureCallTable>,
    pub runtime_state: Option<&'a SimulationRuntimeState>,
}

impl<'a> RowEvalContext<'a> {
    fn with_runtime_state(self, runtime_state: &'a SimulationRuntimeState) -> Self {
        Self {
            runtime_state: Some(runtime_state),
            ..self
        }
    }
}

pub fn eval_scalar_program_block(
    block: &ScalarProgramBlock,
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: Option<&[f64]>,
    out: &mut [f64],
) -> Result<(), EvalSolveError> {
    eval_scalar_program_block_with_context(
        block,
        y,
        p,
        t,
        RowEvalContext {
            seed,
            ..Default::default()
        },
        out,
    )
}

pub fn eval_scalar_program_block_with_context(
    block: &ScalarProgramBlock,
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
    out: &mut [f64],
) -> Result<(), EvalSolveError> {
    let local_runtime_state;
    let context = match context.runtime_state {
        Some(_) => context,
        None => {
            local_runtime_state = SimulationRuntimeState::new();
            context.with_runtime_state(&local_runtime_state)
        }
    };
    validate_scalar_program_block_io(block, y, p, context.seed, out)?;
    out.fill(0.0);
    let mut scratch = RowEvalScratch::default();
    let mut sink = OutputCursor::with_output_indices(out, block.output_indices());
    for (row_idx, row) in block.programs().iter().enumerate() {
        eval_row_prepared_with_context(
            PreparedRowEval::new(
                row,
                required_registers(row)
                    .map_err(|error| error.with_source_span(block.program_span(row_idx)))?,
                y,
                p,
                t,
                context,
            )
            .with_source_span(block.program_span(row_idx)),
            &mut scratch,
            &mut sink,
        )?;
    }
    Ok(())
}

/// Output sink for a (possibly multi-output) scalar program block.
///
/// `StoreOutput` ops write to the next free slot in order (the running-counter
/// invariant on [`rumoca_ir_solve::ScalarProgramBlock::output_count`]). One
/// cursor is shared across all programs of a block so a matmul/linsolve program
/// that emits several outputs lands in consecutive slots.
pub(crate) struct OutputCursor<'out> {
    out: &'out mut [f64],
    output_indices: Option<&'out [usize]>,
    cursor: usize,
    discard_outputs: bool,
}

impl<'out> OutputCursor<'out> {
    pub(crate) fn new(out: &'out mut [f64]) -> Self {
        Self {
            out,
            output_indices: None,
            cursor: 0,
            discard_outputs: false,
        }
    }

    pub(crate) fn with_output_indices(out: &'out mut [f64], output_indices: &'out [usize]) -> Self {
        Self {
            out,
            output_indices: Some(output_indices),
            cursor: 0,
            discard_outputs: false,
        }
    }

    fn discarding() -> Self {
        Self {
            out: &mut [],
            output_indices: None,
            cursor: 0,
            discard_outputs: true,
        }
    }

    fn store(&mut self, value: f64) -> Result<(), EvalSolveError> {
        if self.discard_outputs {
            self.cursor = self
                .cursor
                .checked_add(1)
                .ok_or_else(|| invalid_row("discarded scalar output count exceeds host limits"))?;
            return Ok(());
        }
        let output_index = match self.output_indices {
            Some(indices) => indices
                .get(self.cursor)
                .copied()
                .ok_or_else(|| invalid_row("StoreOutput exceeded scalar program output_indices"))?,
            None => self.cursor,
        };
        match self.out.get_mut(output_index) {
            Some(slot) => {
                *slot = value;
                self.cursor += 1;
                Ok(())
            }
            None => {
                let required = output_index
                    .checked_add(1)
                    .ok_or_else(|| invalid_row("scalar output index exceeds host limits"))?;
                Err(EvalSolveError::OutputTooSmall {
                    required,
                    len: self.out.len(),
                    span: None,
                })
            }
        }
    }
}

#[derive(Default)]
pub(crate) struct RowEvalScratch {
    pub(crate) regs: Vec<f64>,
    pub(crate) initialized: Vec<bool>,
    pub(crate) affine_ops: Vec<LinearOp>,
    lazy_computed: Vec<bool>,
    lazy_tasks: Vec<LazyEvalTask>,
    lazy_trace: Vec<LazyTraceStep>,
    lazy_outputs: Vec<f64>,
}

#[derive(Clone, Copy)]
enum LazyEvalTask {
    Eval(Reg),
    Apply(usize),
    Select {
        dst: Reg,
        cond: Reg,
        if_true: Reg,
        if_false: Reg,
    },
    FinishSelect {
        dst: Reg,
        src: Reg,
    },
    Store(Reg),
}

#[derive(Clone, Copy)]
enum LazyTraceStep {
    Apply(usize),
    Select {
        dst: Reg,
        cond: Reg,
        expected: bool,
        src: Reg,
    },
    Store(Reg),
}

#[derive(Clone)]
pub(crate) struct PreparedLazyRowPlan {
    definitions: Box<[usize]>,
    outputs: Box<[Reg]>,
    trace: RefCell<Option<Box<[LazyTraceStep]>>>,
    trace_native_specialization: bool,
}

/// One branch-specialized, still-valid Solve-IR program derived from a lazy
/// reference-evaluator trace. The first `output_count` stores are the row's
/// ordinary outputs; trailing stores expose the condition registers whose
/// expected truth values guard this specialization.
#[derive(Clone, Debug)]
pub struct SpecializedRowProgram {
    pub program: Vec<LinearOp>,
    pub output_count: usize,
    pub guard_expectations: Box<[bool]>,
}

impl PreparedLazyRowPlan {
    pub(crate) fn new(row: &[LinearOp], register_count: usize) -> Option<Self> {
        if row.len() < 64 || !row.iter().any(|op| matches!(op, LinearOp::Select { .. })) {
            return None;
        }
        if !row.iter().all(lazy_row_op_supported) {
            return None;
        }
        let mut definitions = vec![usize::MAX; register_count];
        let mut outputs = Vec::new();
        for (op_index, op) in row.iter().enumerate() {
            if let Some(dst) = op.dst_register() {
                let start = dst as usize;
                let end = start.saturating_add(op.dst_register_count());
                definitions[start..end].fill(op_index);
            }
            if let LinearOp::StoreOutput { src } = *op {
                outputs.push(src);
            }
        }
        Some(Self {
            definitions: definitions.into_boxed_slice(),
            outputs: outputs.into_boxed_slice(),
            trace: RefCell::new(None),
            trace_native_specialization: !row.iter().any(|operation| {
                matches!(
                    operation,
                    LinearOp::FunctionConditional { .. } | LinearOp::GuardedFunctionFold { .. }
                )
            }),
        })
    }

    fn specialization(&self, row: &[LinearOp]) -> Option<SpecializedRowProgram> {
        if !self.trace_native_specialization {
            return None;
        }
        let trace = self.trace.borrow();
        let trace = trace.as_deref()?;
        let (selected, outputs) = specialization_trace(trace);
        let (needed_ops, guards) = self.specialization_requirements(row, &selected, &outputs)?;
        let mut program = build_specialized_program(row, &selected, &needed_ops)?;
        let mut guard_expectations = Vec::with_capacity(guards.len());
        for (condition, expected) in guards {
            program.push(LinearOp::StoreOutput { src: condition });
            guard_expectations.push(expected);
        }
        Some(SpecializedRowProgram {
            program,
            output_count: outputs.len(),
            guard_expectations: guard_expectations.into_boxed_slice(),
        })
    }

    fn specialization_requirements(
        &self,
        row: &[LinearOp],
        selected: &SpecializedSelections,
        outputs: &[Reg],
    ) -> Option<(Vec<bool>, BTreeMap<Reg, bool>)> {
        let mut guards = BTreeMap::new();
        let mut needed_registers = vec![false; self.definitions.len()];
        let mut needed_ops = vec![false; row.len()];
        let mut tasks = outputs.to_vec();
        while let Some(register) = tasks.pop() {
            if needed_registers[register as usize] {
                continue;
            }
            needed_registers[register as usize] = true;
            let op_index = self.definitions[register as usize];
            if op_index == usize::MAX {
                return None;
            }
            needed_ops[op_index] = true;
            if let Some(&(cond, expected, src)) = selected.get(&register) {
                tasks.push(cond);
                tasks.push(src);
                guards.insert(cond, expected);
                continue;
            }
            if matches!(row[op_index], LinearOp::Select { .. }) {
                return None;
            }
            let mut dependency_tasks = Vec::new();
            push_lazy_dependencies(&mut dependency_tasks, row[op_index].clone());
            tasks.extend(dependency_tasks.into_iter().filter_map(|task| match task {
                LazyEvalTask::Eval(source) => Some(source),
                _ => None,
            }));
        }
        while let Some(source) = first_missing_specialization_dependency(
            row,
            selected,
            &needed_ops,
            self.definitions.len(),
        ) {
            let definition = self.definitions[source as usize];
            if definition == usize::MAX || needed_ops[definition] {
                return None;
            }
            needed_ops[definition] = true;
        }
        Some((needed_ops, guards))
    }
}

type SpecializedSelections = BTreeMap<Reg, (Reg, bool, Reg)>;

fn specialization_trace(trace: &[LazyTraceStep]) -> (SpecializedSelections, Vec<Reg>) {
    let mut selected = BTreeMap::new();
    let mut outputs = Vec::new();
    for step in trace.iter().copied() {
        match step {
            LazyTraceStep::Select {
                dst,
                cond,
                expected,
                src,
            } => {
                selected.insert(dst, (cond, expected, src));
            }
            LazyTraceStep::Store(src) => outputs.push(src),
            LazyTraceStep::Apply(_) => {}
        }
    }
    (selected, outputs)
}

fn first_missing_specialization_dependency(
    row: &[LinearOp],
    selected: &SpecializedSelections,
    needed_ops: &[bool],
    register_count: usize,
) -> Option<Reg> {
    let mut defined = vec![false; register_count];
    for (op_index, op) in row.iter().cloned().enumerate() {
        let Some(effective) = effective_specialization_op(op, selected, needed_ops[op_index])
        else {
            continue;
        };
        let mut dependency_tasks = Vec::new();
        match effective {
            LinearOp::StoreOutput { src } => dependency_tasks.push(LazyEvalTask::Eval(src)),
            _ => push_lazy_dependencies(&mut dependency_tasks, effective.clone()),
        }
        if let Some(source) = dependency_tasks.into_iter().find_map(|task| match task {
            LazyEvalTask::Eval(source) if !defined[source as usize] => Some(source),
            _ => None,
        }) {
            return Some(source);
        }
        if let Some(dst) = effective.dst_register() {
            let start = dst as usize;
            let end = start.saturating_add(effective.dst_register_count());
            defined[start..end].fill(true);
        }
    }
    None
}

fn effective_specialization_op(
    op: LinearOp,
    selected: &SpecializedSelections,
    needed: bool,
) -> Option<LinearOp> {
    if !needed {
        return matches!(op, LinearOp::StoreOutput { .. }).then_some(op);
    }
    Some(match op {
        LinearOp::Select { dst, .. } => selected
            .get(&dst)
            .map_or(op, |(_, _, src)| LinearOp::Move { dst, src: *src }),
        _ => op,
    })
}

fn build_specialized_program(
    row: &[LinearOp],
    selected: &SpecializedSelections,
    needed_ops: &[bool],
) -> Option<Vec<LinearOp>> {
    let mut program = Vec::with_capacity(row.len());
    for (op_index, op) in row.iter().cloned().enumerate() {
        if let LinearOp::StoreOutput { src } = op {
            program.push(LinearOp::StoreOutput { src });
            continue;
        }
        if !needed_ops[op_index] {
            continue;
        }
        match op {
            LinearOp::Select { dst, .. } => {
                let &(_, _, src) = selected.get(&dst)?;
                program.push(LinearOp::Move { dst, src });
            }
            _ => program.push(op),
        }
    }
    Some(program)
}

fn lazy_row_op_supported(op: &LinearOp) -> bool {
    matches!(
        op,
        LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadY { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadIndexedP { .. }
            | LinearOp::LoadIndexedRegister { .. }
            | LinearOp::Move { .. }
            | LinearOp::LinearSolveComponent { .. }
            | LinearOp::DotProduct { .. }
            | LinearOp::MatrixMultiply { .. }
            | LinearOp::TensorBinary { .. }
            | LinearOp::TensorTranspose { .. }
            | LinearOp::TensorConcatenate { .. }
            | LinearOp::TensorUpdate { .. }
            | LinearOp::TensorFill { .. }
            | LinearOp::TensorIdentity { .. }
            | LinearOp::TensorLoad { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::FunctionFold { .. }
            | LinearOp::GuardedFunctionFold { .. }
            | LinearOp::FunctionConditional { .. }
            | LinearOp::StoreOutput { .. }
    )
}

#[derive(Clone, Copy)]
pub(crate) struct PreparedRowEval<'row, 'ctx> {
    row: &'row [LinearOp],
    register_count: usize,
    y: &'row [f64],
    p: &'row [f64],
    t: f64,
    context: RowEvalContext<'ctx>,
    source_span: Option<rumoca_core::Span>,
    lazy_plan: Option<&'row PreparedLazyRowPlan>,
    fold_carried: Option<&'row [f64]>,
    fold_indices: Option<&'row [i64]>,
    fold_captures: Option<&'row [f64]>,
    conditional_captures: Option<&'row [f64]>,
}

impl<'row, 'ctx> PreparedRowEval<'row, 'ctx> {
    pub(crate) fn new(
        row: &'row [LinearOp],
        register_count: usize,
        y: &'row [f64],
        p: &'row [f64],
        t: f64,
        context: RowEvalContext<'ctx>,
    ) -> Self {
        Self {
            row,
            register_count,
            y,
            p,
            t,
            context,
            source_span: None,
            lazy_plan: None,
            fold_carried: None,
            fold_indices: None,
            fold_captures: None,
            conditional_captures: None,
        }
    }

    pub(crate) fn with_lazy_plan(mut self, plan: Option<&'row PreparedLazyRowPlan>) -> Self {
        self.lazy_plan = plan;
        self
    }

    pub(crate) fn with_source_span(mut self, span: Option<rumoca_core::Span>) -> Self {
        self.source_span = span;
        self
    }

    fn with_fold_context(
        mut self,
        carried: &'row [f64],
        indices: &'row [i64],
        captures: &'row [f64],
    ) -> Self {
        self.fold_carried = Some(carried);
        self.fold_indices = Some(indices);
        self.fold_captures = Some(captures);
        self
    }

    fn with_conditional_captures(mut self, captures: &'row [f64]) -> Self {
        self.conditional_captures = Some(captures);
        self
    }

    fn missing_input(&self, vector: &'static str, index: usize, len: usize) -> EvalSolveError {
        EvalSolveError::MissingInput {
            vector,
            index,
            len,
            span: self.source_span,
        }
    }

    fn read_input(
        &self,
        vector: &'static str,
        values: &[f64],
        index: usize,
    ) -> Result<f64, EvalSolveError> {
        values
            .get(index)
            .copied()
            .ok_or_else(|| self.missing_input(vector, index, values.len()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EventActionRequest {
    Continue,
    AssertionFailed { message: String },
    Terminate { message: String },
}

pub fn eval_event_action_request(
    events: &SolveEventPartition,
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
) -> Result<EventActionRequest, EvalSolveError> {
    if events.actions.is_empty() {
        return Ok(EventActionRequest::Continue);
    }
    if events.action_conditions.len() != events.actions.len() {
        return Err(EvalSolveError::EventActionConditionMismatch {
            rows: events.action_conditions.len(),
            actions: events.actions.len(),
        });
    }
    let mut values =
        eval_solve_f64_values(events.actions.len(), 0.0, "event action condition values")?;
    eval_scalar_program_block_with_context(
        &events.action_conditions,
        y,
        p,
        t,
        context,
        &mut values,
    )?;
    event_action_request_from_values(events, y, p, t, context, values)
}

/// Evaluate event-action guards through a block whose register-flow and input
/// requirements were validated once at runtime construction.
pub fn eval_prepared_event_action_request(
    events: &SolveEventPartition,
    action_conditions: &PreparedScalarProgramBlock,
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
) -> Result<EventActionRequest, EvalSolveError> {
    if events.actions.is_empty() {
        return Ok(EventActionRequest::Continue);
    }
    if action_conditions.len() != events.actions.len() {
        return Err(EvalSolveError::EventActionConditionMismatch {
            rows: action_conditions.len(),
            actions: events.actions.len(),
        });
    }
    let mut values =
        eval_solve_f64_values(events.actions.len(), 0.0, "event action condition values")?;
    action_conditions.eval_with_context(y, p, t, context, &mut values)?;
    event_action_request_from_values(events, y, p, t, context, values)
}

/// Interpret already-evaluated action guard values in source order.
pub fn event_action_request_from_values(
    events: &SolveEventPartition,
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
    values: Vec<f64>,
) -> Result<EventActionRequest, EvalSolveError> {
    for (value, action) in values.into_iter().zip(&events.actions) {
        if value <= 0.5 {
            continue;
        }
        match action.kind {
            SolveEventActionKind::Assert => {
                return Ok(EventActionRequest::AssertionFailed {
                    message: eval_event_action_message(action, y, p, t, context)?,
                });
            }
            SolveEventActionKind::Terminate => {
                return Ok(EventActionRequest::Terminate {
                    message: eval_event_action_message(action, y, p, t, context)?,
                });
            }
        }
    }
    Ok(EventActionRequest::Continue)
}

fn eval_event_action_message(
    action: &rumoca_ir_solve::SolveEventAction,
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
) -> Result<String, EvalSolveError> {
    let mut message = String::new();
    let eval = MessageEvalContext {
        y,
        p,
        t,
        row: context,
        span: action.span,
    };
    for part in &action.message.parts {
        match part {
            SolveEventMessagePart::Text(text) => {
                append_event_message_part(&mut message, text, action.span)?;
            }
            SolveEventMessagePart::Conversion {
                value,
                source,
                format,
            } => {
                let rendered = eval_event_message_conversion(value, *source, format, eval)?;
                append_event_message_part(&mut message, &rendered, action.span)?;
            }
        }
    }
    Ok(message)
}

#[derive(Clone, Copy)]
struct MessageEvalContext<'a> {
    y: &'a [f64],
    p: &'a [f64],
    t: f64,
    row: RowEvalContext<'a>,
    span: rumoca_core::Span,
}

const MAX_EVENT_MESSAGE_BYTES: usize = 1_048_576;
const MAX_STRING_SIGNIFICANT_DIGITS: i64 = 1_024;

fn eval_event_message_conversion(
    row: &[LinearOp],
    source: SolveStringConversionSource,
    format: &SolveStringConversionFormat,
    eval: MessageEvalContext<'_>,
) -> Result<String, EvalSolveError> {
    let value = eval_row_with_context(row, eval.y, eval.p, eval.t, eval.row)?;
    let SolveStringConversionFormat::Options {
        minimum_length,
        left_justified,
        significant_digits,
    } = format;
    let minimum_length =
        eval_message_integer_option(minimum_length.as_deref(), 0, "minimumLength", eval)?;
    let left_justified =
        eval_message_boolean_option(left_justified.as_deref(), true, "leftJustified", eval)?;
    let significant_digits =
        eval_message_integer_option(significant_digits.as_deref(), 6, "significantDigits", eval)?;
    if !(1..=MAX_STRING_SIGNIFICANT_DIGITS).contains(&significant_digits) {
        return Err(invalid_message_option(
            format!("significantDigits must be in 1..={MAX_STRING_SIGNIFICANT_DIGITS}"),
            eval.span,
        ));
    }
    let converted = match source {
        SolveStringConversionSource::Real => {
            format_significant_digits(value, significant_digits as usize)
        }
        SolveStringConversionSource::Integer => {
            if !value.is_finite() || value.fract() != 0.0 {
                return Err(invalid_message_option(
                    "Integer String conversion received a non-integer runtime value",
                    eval.span,
                ));
            }
            format!("{value:.0}")
        }
        SolveStringConversionSource::Boolean => {
            if value == 0.0 {
                "false".to_string()
            } else if value == 1.0 {
                "true".to_string()
            } else {
                return Err(invalid_message_option(
                    "Boolean String conversion received a value outside {0,1}",
                    eval.span,
                ));
            }
        }
    };
    let width = usize::try_from(minimum_length)
        .map_err(|_| invalid_message_option("minimumLength must be nonnegative", eval.span))?;
    if width > MAX_EVENT_MESSAGE_BYTES {
        return Err(invalid_message_option(
            format!("minimumLength exceeds the {MAX_EVENT_MESSAGE_BYTES}-byte message limit"),
            eval.span,
        ));
    }
    if converted.len() >= width {
        return Ok(converted);
    }
    let padding_len = width - converted.len();
    let mut padded = String::new();
    padded
        .try_reserve_exact(width)
        .map_err(|_| invalid_message_option("event message allocation failed", eval.span))?;
    if left_justified {
        padded.push_str(&converted);
        padded.extend(std::iter::repeat_n(' ', padding_len));
    } else {
        padded.extend(std::iter::repeat_n(' ', padding_len));
        padded.push_str(&converted);
    }
    Ok(padded)
}

fn eval_message_integer_option(
    row: Option<&[LinearOp]>,
    default: i64,
    name: &'static str,
    eval: MessageEvalContext<'_>,
) -> Result<i64, EvalSolveError> {
    let Some(row) = row else {
        return Ok(default);
    };
    let value = eval_row_with_context(row, eval.y, eval.p, eval.t, eval.row)?;
    if !value.is_finite()
        || value.fract() != 0.0
        || value < i64::MIN as f64
        || value >= 9_223_372_036_854_775_808.0
    {
        return Err(invalid_message_option(
            format!("{name} must evaluate to an Integer"),
            eval.span,
        ));
    }
    Ok(value as i64)
}

fn eval_message_boolean_option(
    row: Option<&[LinearOp]>,
    default: bool,
    name: &'static str,
    eval: MessageEvalContext<'_>,
) -> Result<bool, EvalSolveError> {
    let Some(row) = row else {
        return Ok(default);
    };
    match eval_row_with_context(row, eval.y, eval.p, eval.t, eval.row)? {
        0.0 => Ok(false),
        1.0 => Ok(true),
        _ => Err(invalid_message_option(
            format!("{name} must evaluate to a Boolean"),
            eval.span,
        )),
    }
}

fn append_event_message_part(
    message: &mut String,
    part: &str,
    span: rumoca_core::Span,
) -> Result<(), EvalSolveError> {
    let total = message
        .len()
        .checked_add(part.len())
        .filter(|total| *total <= MAX_EVENT_MESSAGE_BYTES)
        .ok_or_else(|| {
            invalid_message_option(
                format!("event message exceeds the {MAX_EVENT_MESSAGE_BYTES}-byte limit"),
                span,
            )
        })?;
    message
        .try_reserve_exact(total - message.len())
        .map_err(|_| invalid_message_option("event message allocation failed", span))?;
    message.push_str(part);
    Ok(())
}

fn invalid_message_option(message: impl Into<String>, span: rumoca_core::Span) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: message.into(),
        span: Some(span),
    }
}

fn format_significant_digits(value: f64, digits: usize) -> String {
    if !value.is_finite() || value == 0.0 {
        return value.to_string();
    }
    let exponent = value.abs().log10().floor() as i32;
    if exponent < -4 || exponent >= digits as i32 {
        let mut formatted = format!("{:.*e}", digits.saturating_sub(1), value);
        trim_fraction_zeros(&mut formatted, 'e');
        return formatted;
    }
    let fractional = (digits as i32 - exponent - 1).max(0) as usize;
    let mut formatted = format!("{value:.fractional$}");
    trim_fraction_zeros(&mut formatted, '\0');
    formatted
}

fn trim_fraction_zeros(value: &mut String, exponent_marker: char) {
    let exponent = (exponent_marker != '\0')
        .then(|| value.find(exponent_marker))
        .flatten()
        .unwrap_or(value.len());
    let mut end = exponent;
    while end > 0 && value.as_bytes()[end - 1] == b'0' {
        end -= 1;
    }
    if end > 0 && value.as_bytes()[end - 1] == b'.' {
        end -= 1;
    }
    if end != exponent {
        value.replace_range(end..exponent, "");
    }
}

pub fn eval_row(
    row: &[LinearOp],
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: Option<&[f64]>,
) -> Result<f64, EvalSolveError> {
    eval_row_with_context(
        row,
        y,
        p,
        t,
        RowEvalContext {
            seed,
            ..Default::default()
        },
    )
}

pub fn eval_row_with_context(
    row: &[LinearOp],
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
) -> Result<f64, EvalSolveError> {
    let local_runtime_state;
    let context = match context.runtime_state {
        Some(_) => context,
        None => {
            local_runtime_state = SimulationRuntimeState::new();
            context.with_runtime_state(&local_runtime_state)
        }
    };
    validate_row_inputs(row, y, p, context.seed)?;
    let mut scratch = RowEvalScratch::default();
    eval_program_single(
        PreparedRowEval::new(row, required_registers(row)?, y, p, t, context),
        false,
        &mut scratch,
    )
}

/// Evaluate a single-output program and return its stored value.
///
/// Convenience for callers that operate on programs with exactly one
/// `StoreOutput` (residual rows, root conditions, event-message numbers,
/// target-assignment rows).
pub(crate) fn eval_program_single(
    input: PreparedRowEval<'_, '_>,
    register_safe: bool,
    scratch: &mut RowEvalScratch,
) -> Result<f64, EvalSolveError> {
    require_program_output_count(input.row, 1, input.source_span)?;
    eval_prevalidated_single_output_program(input, register_safe, scratch)
}

/// Evaluate a program whose prepared block metadata already proves that it
/// stores exactly one output.
///
/// Construction through `PreparedScalarProgramBlock` owns that proof. Keeping
/// the unchecked primitive crate-private prevents an unprepared row from
/// bypassing the ordinary shape check.
pub(crate) fn eval_prevalidated_single_output_program(
    input: PreparedRowEval<'_, '_>,
    register_safe: bool,
    scratch: &mut RowEvalScratch,
) -> Result<f64, EvalSolveError> {
    let mut buf = [0.0f64];
    {
        let mut sink = OutputCursor::new(&mut buf);
        eval_row_prepared_maybe_fast(input, register_safe, scratch, &mut sink)?;
    }
    Ok(buf[0])
}

pub(crate) fn eval_program_no_output(
    input: PreparedRowEval<'_, '_>,
    register_safe: bool,
    scratch: &mut RowEvalScratch,
) -> Result<(), EvalSolveError> {
    require_program_output_count(input.row, 0, input.source_span)?;
    eval_prevalidated_no_output_program(input, register_safe, scratch)
}

/// Evaluate a program prefix whose prepared assignment shape proves that it
/// stores no outputs.
pub(crate) fn eval_prevalidated_no_output_program(
    input: PreparedRowEval<'_, '_>,
    register_safe: bool,
    scratch: &mut RowEvalScratch,
) -> Result<(), EvalSolveError> {
    let mut sink = OutputCursor::new(&mut []);
    eval_row_prepared_maybe_fast(input, register_safe, scratch, &mut sink)
}

/// Evaluate an assignment prefix while discarding earlier outputs from the
/// same retained multi-output source program.
pub(crate) fn eval_prevalidated_discard_output_program(
    input: PreparedRowEval<'_, '_>,
    register_safe: bool,
    scratch: &mut RowEvalScratch,
) -> Result<(), EvalSolveError> {
    let mut sink = OutputCursor::discarding();
    eval_row_prepared_maybe_fast(input, register_safe, scratch, &mut sink)
}

fn require_program_output_count(
    row: &[LinearOp],
    expected: usize,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    let actual = ScalarProgramBlock::program_output_count(row);
    if actual == expected {
        return Ok(());
    }
    Err(EvalSolveError::InvalidRow {
        message: format!("single-program evaluation expected {expected} outputs, found {actual}"),
        span,
    })
}

#[inline(always)]
pub(crate) fn eval_row_prepared_maybe_fast(
    input: PreparedRowEval<'_, '_>,
    register_safe: bool,
    scratch: &mut RowEvalScratch,
    sink: &mut OutputCursor<'_>,
) -> Result<(), EvalSolveError> {
    let start = solve_row_eval_trace_active().then(Instant::now);
    let result = if register_safe && input.lazy_plan.is_some() {
        eval_row_prepared_lazy(input, scratch, sink)
    } else if register_safe {
        eval_row_prepared_fast(input, scratch, sink)
    } else {
        eval_row_prepared_with_context(input, scratch, sink)
    };
    if let Some(start) = start {
        ROW_EVAL_CALLS.fetch_add(1, Ordering::Relaxed);
        ROW_EVAL_NANOS.fetch_add(elapsed_nanos_u64(start), Ordering::Relaxed);
    }
    result
}

fn eval_row_prepared_lazy(
    input: PreparedRowEval<'_, '_>,
    scratch: &mut RowEvalScratch,
    sink: &mut OutputCursor<'_>,
) -> Result<(), EvalSolveError> {
    let plan = input
        .lazy_plan
        .expect("lazy row evaluation requires a prepared plan");
    scratch.regs.resize(input.register_count, 0.0);
    scratch.lazy_outputs.clear();
    if let Some(trace) = plan.trace.borrow().as_deref()
        && eval_cached_lazy_trace(input, &mut scratch.regs, trace, &mut scratch.lazy_outputs)?
    {
        for value in scratch.lazy_outputs.iter().copied() {
            sink.store(value)?;
        }
        return Ok(());
    }
    scratch.lazy_computed.resize(input.register_count, false);
    scratch.lazy_computed.fill(false);
    scratch.lazy_tasks.clear();
    scratch.lazy_trace.clear();
    for &output in &plan.outputs {
        scratch.lazy_tasks.push(LazyEvalTask::Store(output));
        scratch.lazy_tasks.push(LazyEvalTask::Eval(output));
        while let Some(task) = scratch.lazy_tasks.pop() {
            execute_lazy_task(input, plan, scratch, task)?;
        }
    }
    *plan.trace.borrow_mut() = Some(scratch.lazy_trace.clone().into_boxed_slice());
    for value in scratch.lazy_outputs.iter().copied() {
        sink.store(value)?;
    }
    Ok(())
}

fn execute_lazy_task(
    input: PreparedRowEval<'_, '_>,
    plan: &PreparedLazyRowPlan,
    scratch: &mut RowEvalScratch,
    task: LazyEvalTask,
) -> Result<(), EvalSolveError> {
    match task {
        LazyEvalTask::Eval(register) => {
            if scratch.lazy_computed[register as usize] {
                return Ok(());
            }
            let op_index = plan.definitions[register as usize];
            let op = input.row[op_index].clone();
            if let LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } = op
            {
                scratch.lazy_tasks.push(LazyEvalTask::Select {
                    dst,
                    cond,
                    if_true,
                    if_false,
                });
                scratch.lazy_tasks.push(LazyEvalTask::Eval(cond));
            } else {
                scratch.lazy_tasks.push(LazyEvalTask::Apply(op_index));
                push_lazy_dependencies(&mut scratch.lazy_tasks, op);
            }
        }
        LazyEvalTask::Apply(op_index) => {
            let op = input.row[op_index].clone();
            eval_lazy_pure_op(input, &mut scratch.regs, op.clone())?;
            scratch.lazy_trace.push(LazyTraceStep::Apply(op_index));
            let dst = op
                .dst_register()
                .expect("a lazy apply task owns a destination");
            let start = dst as usize;
            let end = start.saturating_add(op.dst_register_count());
            scratch.lazy_computed[start..end].fill(true);
        }
        LazyEvalTask::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => {
            let src = if scratch.regs[cond as usize] != 0.0 {
                if_true
            } else {
                if_false
            };
            scratch
                .lazy_tasks
                .push(LazyEvalTask::FinishSelect { dst, src });
            scratch.lazy_tasks.push(LazyEvalTask::Eval(src));
        }
        LazyEvalTask::FinishSelect { dst, src } => {
            scratch.regs[dst as usize] = scratch.regs[src as usize];
            scratch.lazy_computed[dst as usize] = true;
            let LinearOp::Select { cond, .. } = input.row[plan.definitions[dst as usize]] else {
                unreachable!("a lazy selected destination owns a Select op")
            };
            scratch.lazy_trace.push(LazyTraceStep::Select {
                dst,
                cond,
                expected: scratch.regs[cond as usize] != 0.0,
                src,
            });
        }
        LazyEvalTask::Store(src) => {
            scratch.lazy_trace.push(LazyTraceStep::Store(src));
            scratch.lazy_outputs.push(scratch.regs[src as usize]);
        }
    }
    Ok(())
}

fn eval_cached_lazy_trace(
    input: PreparedRowEval<'_, '_>,
    regs: &mut [f64],
    trace: &[LazyTraceStep],
    outputs: &mut Vec<f64>,
) -> Result<bool, EvalSolveError> {
    outputs.clear();
    for step in trace.iter().copied() {
        match step {
            LazyTraceStep::Apply(op_index) => {
                eval_lazy_pure_op(input, regs, input.row[op_index].clone())?;
            }
            LazyTraceStep::Select {
                dst,
                cond,
                expected,
                src,
            } => {
                if (regs[cond as usize] != 0.0) != expected {
                    outputs.clear();
                    return Ok(false);
                }
                regs[dst as usize] = regs[src as usize];
            }
            LazyTraceStep::Store(src) => outputs.push(regs[src as usize]),
        }
    }
    Ok(true)
}

fn push_lazy_dependencies(tasks: &mut Vec<LazyEvalTask>, op: LinearOp) {
    match op {
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadY { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::TensorIdentity { .. }
        | LinearOp::TensorLoad { .. } => {}
        LinearOp::LoadIndexedP { index, .. } => push_lazy_register(tasks, index),
        LinearOp::LoadIndexedRegister {
            base,
            stride,
            dimensions,
            indices,
            ..
        } => {
            for index in indices.iter().rev() {
                if let rumoca_ir_solve::TensorIndex::Runtime(register) = index {
                    push_lazy_register(tasks, *register);
                }
            }
            let count = dimensions.iter().fold(stride, |count, extent| {
                count.saturating_mul(*extent as usize)
            });
            push_lazy_register_range(tasks, base, count, 1);
        }
        LinearOp::Move { src, .. } | LinearOp::Unary { arg: src, .. } => {
            push_lazy_register(tasks, src);
        }
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
            push_lazy_register(tasks, rhs);
            push_lazy_register(tasks, lhs);
        }
        LinearOp::LinearSolveComponent {
            matrix_start,
            rhs_start,
            n,
            ..
        } => {
            push_lazy_register_range(tasks, rhs_start, n, 1);
            push_lazy_register_range(tasks, matrix_start, n.saturating_mul(n), 1);
        }
        LinearOp::DotProduct {
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            ..
        } => {
            for term in (0..count).rev() {
                push_lazy_register(tasks, (rhs_start as usize + term * rhs_stride) as Reg);
                push_lazy_register(tasks, (lhs_start as usize + term * lhs_stride) as Reg);
            }
        }
        op @ (LinearOp::MatrixMultiply { .. }
        | LinearOp::TensorBinary { .. }
        | LinearOp::TensorTranspose { .. }
        | LinearOp::TensorConcatenate { .. }
        | LinearOp::TensorUpdate { .. }
        | LinearOp::TensorFill { .. }) => push_lazy_tensor_dependencies(tasks, op),
        op @ (LinearOp::FunctionFold { .. }
        | LinearOp::GuardedFunctionFold { .. }
        | LinearOp::FunctionConditional { .. }) => push_lazy_region_dependencies(tasks, op),
        LinearOp::Select { .. } | LinearOp::StoreOutput { .. } => {
            unreachable!("select and output stores have dedicated lazy tasks")
        }
        _ => unreachable!("unsupported operations do not receive a lazy plan"),
    }
}

fn push_lazy_tensor_dependencies(tasks: &mut Vec<LazyEvalTask>, op: LinearOp) {
    match op {
        LinearOp::MatrixMultiply {
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes,
            ..
        } => {
            push_lazy_register_range(
                tasks,
                rhs_start,
                inner.saturating_mul(columns).saturating_mul(lanes),
                1,
            );
            push_lazy_register_range(
                tasks,
                lhs_start,
                rows.saturating_mul(inner).saturating_mul(lanes),
                1,
            );
        }
        LinearOp::TensorBinary {
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
            ..
        } => {
            push_lazy_tensor_elements(tasks, rhs_start, count, rhs_stride, lanes);
            push_lazy_tensor_elements(tasks, lhs_start, count, lhs_stride, lanes);
        }
        LinearOp::TensorTranspose {
            src_start,
            rows,
            columns,
            element_width,
            lanes,
            ..
        } => push_lazy_register_range(
            tasks,
            src_start,
            rows.saturating_mul(columns)
                .saturating_mul(element_width)
                .saturating_mul(lanes),
            1,
        ),
        LinearOp::TensorConcatenate { sources, lanes, .. } => {
            for source in sources.iter().rev() {
                let count = source.dimensions.iter().fold(lanes, |count, extent| {
                    count.saturating_mul(*extent as usize)
                });
                push_lazy_register_range(tasks, source.start, count, 1);
            }
        }
        LinearOp::TensorUpdate {
            base_start,
            value_start,
            dimensions,
            subscripts,
            lanes,
            ..
        } => push_lazy_tensor_update_dependencies(
            tasks,
            base_start,
            value_start,
            &dimensions,
            &subscripts,
            lanes,
        ),
        LinearOp::TensorFill {
            value_start, lanes, ..
        } => push_lazy_register_range(tasks, value_start, lanes, 1),
        _ => unreachable!("lazy tensor dependency dispatch receives only tensor operations"),
    }
}

fn push_lazy_tensor_update_dependencies(
    tasks: &mut Vec<LazyEvalTask>,
    base_start: Reg,
    value_start: Reg,
    dimensions: &[u32],
    subscripts: &[rumoca_ir_solve::TensorUpdateSubscript],
    lanes: usize,
) {
    let base_count = dimensions.iter().fold(lanes, |count, extent| {
        count.saturating_mul(*extent as usize)
    });
    let mut value_count = lanes;
    for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()).rev() {
        match subscript {
            rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                value_count = value_count.saturating_mul(extent as usize);
            }
            rumoca_ir_solve::TensorUpdateSubscript::Index(
                rumoca_ir_solve::TensorIndex::Runtime(register),
            ) => push_lazy_register(tasks, *register),
            rumoca_ir_solve::TensorUpdateSubscript::Index(
                rumoca_ir_solve::TensorIndex::Constant(_),
            ) => {}
            rumoca_ir_solve::TensorUpdateSubscript::Slice { start, dimensions } => {
                let count = dimensions.iter().fold(1usize, |count, extent| {
                    count.saturating_mul(*extent as usize)
                });
                push_lazy_register_range(tasks, *start, count, 1);
                value_count = value_count.saturating_mul(count);
            }
        }
    }
    push_lazy_register_range(tasks, value_start, value_count, 1);
    push_lazy_register_range(tasks, base_start, base_count, 1);
}

fn push_lazy_region_dependencies(tasks: &mut Vec<LazyEvalTask>, op: LinearOp) {
    match op {
        LinearOp::FunctionFold {
            initial_start,
            capture_start,
            program,
            ..
        } => {
            push_lazy_register_range(tasks, capture_start, program.capture_count, 1);
            push_lazy_register_range(tasks, initial_start, program.carried_count, 1);
        }
        LinearOp::GuardedFunctionFold {
            initial_start,
            capture_start,
            activation,
            program,
            ..
        } => {
            push_lazy_register_range(tasks, capture_start, program.capture_count, 1);
            push_lazy_register_range(tasks, initial_start, program.carried_count, 1);
            push_lazy_register(tasks, activation);
        }
        LinearOp::FunctionConditional {
            capture_start,
            program,
            ..
        } => push_lazy_register_range(tasks, capture_start, program.capture_count, 1),
        _ => unreachable!("lazy region dependency dispatch receives only region operations"),
    }
}

fn push_lazy_register(tasks: &mut Vec<LazyEvalTask>, register: Reg) {
    tasks.push(LazyEvalTask::Eval(register));
}

fn push_lazy_register_range(
    tasks: &mut Vec<LazyEvalTask>,
    start: Reg,
    count: usize,
    stride: usize,
) {
    for offset in (0..count).rev() {
        tasks.push(LazyEvalTask::Eval(
            start + offset.saturating_mul(stride) as Reg,
        ));
    }
}

fn push_lazy_tensor_elements(
    tasks: &mut Vec<LazyEvalTask>,
    start: Reg,
    count: usize,
    stride: usize,
    lanes: usize,
) {
    for element in (0..count).rev() {
        for lane in (0..lanes).rev() {
            tasks.push(LazyEvalTask::Eval(
                start + (element.saturating_mul(stride).saturating_mul(lanes) + lane) as Reg,
            ));
        }
    }
}

fn eval_lazy_pure_op(
    input: PreparedRowEval<'_, '_>,
    regs: &mut [f64],
    op: LinearOp,
) -> Result<(), EvalSolveError> {
    match op {
        op @ (LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadY { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadIndexedP { .. }
        | LinearOp::LoadIndexedRegister { .. }
        | LinearOp::Move { .. }
        | LinearOp::LinearSolveComponent { .. }
        | LinearOp::DotProduct { .. }
        | LinearOp::Unary { .. }
        | LinearOp::Binary { .. }
        | LinearOp::Compare { .. }) => eval_lazy_scalar_op(input, regs, op)?,
        op @ (LinearOp::MatrixMultiply { .. }
        | LinearOp::TensorBinary { .. }
        | LinearOp::TensorTranspose { .. }
        | LinearOp::TensorConcatenate { .. }
        | LinearOp::TensorUpdate { .. }) => eval_lazy_tensor_algebra_op(regs, op)?,
        op @ (LinearOp::TensorFill { .. }
        | LinearOp::TensorIdentity { .. }
        | LinearOp::TensorLoad { .. }) => eval_lazy_tensor_constructor_op(input, regs, op)?,
        op @ (LinearOp::FunctionFold { .. }
        | LinearOp::GuardedFunctionFold { .. }
        | LinearOp::FunctionConditional { .. }) => eval_lazy_region_op(input, regs, op)?,
        _ => unreachable!("unsupported operations do not receive a lazy apply task"),
    }
    Ok(())
}

fn eval_lazy_scalar_op(
    input: PreparedRowEval<'_, '_>,
    regs: &mut [f64],
    op: LinearOp,
) -> Result<(), EvalSolveError> {
    match op {
        LinearOp::Const { dst, value } => regs[dst as usize] = value,
        LinearOp::LoadTime { dst } => regs[dst as usize] = input.t,
        LinearOp::LoadY { dst, index } => regs[dst as usize] = input.y[index],
        LinearOp::LoadP { dst, index } => regs[dst as usize] = input.p[index],
        LinearOp::LoadIndexedP {
            dst,
            base,
            count,
            index,
        } => {
            let slot = resolve_indexed_slot(regs[index as usize], base, count);
            regs[dst as usize] = input.read_input("p", input.p, slot)?;
        }
        LinearOp::LoadIndexedRegister {
            dst,
            base,
            stride,
            dimensions,
            indices,
        } => {
            let offset = tensor_register_offset(&dimensions, &indices, |register| {
                Ok::<f64, EvalSolveError>(regs[register as usize])
            })?;
            regs[dst as usize] = offset
                .map(|offset| regs[base as usize + offset * stride])
                .unwrap_or(f64::NAN);
        }
        LinearOp::Move { dst, src } => regs[dst as usize] = regs[src as usize],
        LinearOp::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            component,
        } => {
            regs[dst as usize] =
                solve_component_unchecked(regs, matrix_start, rhs_start, n, component)
                    .map_err(|error| error.with_source_span(input.source_span))?;
        }
        LinearOp::DotProduct {
            dst,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
        } => {
            let mut value = 0.0;
            for term in 0..count {
                value += regs[lhs_start as usize + term * lhs_stride]
                    * regs[rhs_start as usize + term * rhs_stride];
            }
            regs[dst as usize] = value;
        }
        LinearOp::Unary { dst, op, arg } => {
            regs[dst as usize] = eval_unary(op, regs[arg as usize]);
        }
        LinearOp::Binary { dst, op, lhs, rhs } => {
            regs[dst as usize] = eval_binary(op, regs[lhs as usize], regs[rhs as usize]);
        }
        LinearOp::Compare { dst, op, lhs, rhs } => {
            regs[dst as usize] = eval_compare(op, regs[lhs as usize], regs[rhs as usize]);
        }
        _ => unreachable!("lazy scalar dispatch receives only scalar operations"),
    }
    Ok(())
}

fn eval_lazy_tensor_algebra_op(regs: &mut [f64], op: LinearOp) -> Result<(), EvalSolveError> {
    match op {
        LinearOp::MatrixMultiply {
            dst_start,
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes,
        } => eval_matrix_multiply(
            regs, dst_start, lhs_start, rhs_start, rows, inner, columns, lanes,
        ),
        LinearOp::TensorBinary {
            dst_start,
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
        } => eval_tensor_binary(
            regs, dst_start, op, lhs_start, rhs_start, count, lhs_stride, rhs_stride, lanes,
        ),
        LinearOp::TensorTranspose {
            dst_start,
            src_start,
            rows,
            columns,
            element_width,
            lanes,
        } => eval_tensor_transpose(
            regs,
            dst_start,
            src_start,
            rows,
            columns,
            element_width,
            lanes,
        ),
        LinearOp::TensorConcatenate {
            dst_start,
            sources,
            dimensions,
            axis,
            lanes,
        } => {
            visit_tensor_concatenate(&sources, &dimensions, axis, lanes, |source, destination| {
                regs[dst_start as usize + destination] = regs[source as usize];
                Ok::<(), EvalSolveError>(())
            })?
        }
        LinearOp::TensorUpdate {
            dst_start,
            base_start,
            value_start,
            dimensions,
            subscripts,
            lanes,
        } => eval_tensor_update(
            regs,
            dst_start,
            base_start,
            value_start,
            &dimensions,
            &subscripts,
            lanes,
        )?,
        _ => unreachable!("lazy tensor algebra dispatch receives only tensor operations"),
    }
    Ok(())
}

fn eval_lazy_tensor_constructor_op(
    input: PreparedRowEval<'_, '_>,
    regs: &mut [f64],
    op: LinearOp,
) -> Result<(), EvalSolveError> {
    match op {
        LinearOp::TensorFill {
            dst_start,
            value_start,
            count,
            lanes,
        } => {
            for element in 0..count {
                for lane in 0..lanes {
                    regs[dst_start as usize + element * lanes + lane] =
                        regs[value_start as usize + lane];
                }
            }
        }
        LinearOp::TensorIdentity {
            dst_start,
            size,
            lanes,
        } => {
            let output = &mut regs[dst_start as usize..dst_start as usize + size * size * lanes];
            output.fill(0.0);
            for diagonal in 0..size {
                output[(diagonal * size + diagonal) * lanes] = 1.0;
            }
        }
        LinearOp::TensorLoad {
            dst_start,
            input: source,
            input_start,
            count,
            seed_start,
            lanes,
        } => eval_lazy_tensor_load(
            input,
            regs,
            LazyTensorLoadRange {
                dst_start,
                source,
                input_start,
                count,
                seed_start,
                lanes,
            },
        )?,
        _ => unreachable!("lazy tensor constructor dispatch receives only constructors"),
    }
    Ok(())
}

struct LazyTensorLoadRange {
    dst_start: Reg,
    source: rumoca_ir_solve::TensorInputKind,
    input_start: usize,
    count: usize,
    seed_start: Option<usize>,
    lanes: usize,
}

fn eval_lazy_tensor_load(
    input: PreparedRowEval<'_, '_>,
    regs: &mut [f64],
    range: LazyTensorLoadRange,
) -> Result<(), EvalSolveError> {
    let values = match range.source {
        rumoca_ir_solve::TensorInputKind::Y => input.y,
        rumoca_ir_solve::TensorInputKind::P => input.p,
    };
    for element in 0..range.count {
        regs[range.dst_start as usize + element * range.lanes] =
            values[range.input_start + element];
        if range.lanes == 2 {
            regs[range.dst_start as usize + element * range.lanes + 1] = match range.seed_start {
                Some(seed_start) => input
                    .context
                    .seed
                    .and_then(|seed| seed.get(seed_start + element))
                    .copied()
                    .ok_or_else(|| input.missing_input("seed", seed_start + element, 0))?,
                None => 0.0,
            };
        }
    }
    Ok(())
}

fn eval_lazy_region_op(
    input: PreparedRowEval<'_, '_>,
    regs: &mut [f64],
    op: LinearOp,
) -> Result<(), EvalSolveError> {
    match op {
        LinearOp::FunctionFold {
            dst_start,
            initial_start,
            capture_start,
            program,
        } => {
            let initial = regs
                [initial_start as usize..initial_start as usize + program.carried_count]
                .to_vec();
            let captures = regs
                [capture_start as usize..capture_start as usize + program.capture_count]
                .to_vec();
            let carried = eval_function_fold(input, &program, &initial, &captures)?;
            regs[dst_start as usize..dst_start as usize + program.carried_count]
                .copy_from_slice(&carried);
        }
        LinearOp::GuardedFunctionFold {
            dst_start,
            initial_start,
            capture_start,
            activation,
            program,
        } => {
            let initial = regs
                [initial_start as usize..initial_start as usize + program.carried_count]
                .to_vec();
            if regs[activation as usize] != 0.0 {
                let captures = regs
                    [capture_start as usize..capture_start as usize + program.capture_count]
                    .to_vec();
                let carried = eval_function_fold(input, &program, &initial, &captures)?;
                regs[dst_start as usize..dst_start as usize + program.carried_count]
                    .copy_from_slice(&carried);
            } else {
                regs[dst_start as usize..dst_start as usize + program.carried_count]
                    .copy_from_slice(&initial);
            }
        }
        LinearOp::FunctionConditional {
            dst_start,
            capture_start,
            program,
        } => {
            let captures =
                &regs[capture_start as usize..capture_start as usize + program.capture_count];
            let values = eval_function_conditional(input, &program, captures)?;
            regs[dst_start as usize..dst_start as usize + program.result_count]
                .copy_from_slice(&values);
        }
        _ => unreachable!("lazy region dispatch receives only region operations"),
    }
    Ok(())
}

fn eval_row_prepared_with_context(
    input: PreparedRowEval<'_, '_>,
    scratch: &mut RowEvalScratch,
    sink: &mut OutputCursor<'_>,
) -> Result<(), EvalSolveError> {
    scratch.regs.resize(input.register_count, 0.0);
    scratch.initialized.resize(input.register_count, false);
    scratch.regs.fill(0.0);
    scratch.initialized.fill(false);
    CheckedRowEvaluator {
        regs: &mut scratch.regs,
        initialized: &mut scratch.initialized,
        input,
        sink,
    }
    .eval()
}

struct CheckedRowEvaluator<'scratch, 'row, 'ctx, 'out> {
    regs: &'scratch mut [f64],
    initialized: &'scratch mut [bool],
    input: PreparedRowEval<'row, 'ctx>,
    sink: &'scratch mut OutputCursor<'out>,
}

impl CheckedRowEvaluator<'_, '_, '_, '_> {
    fn eval(mut self) -> Result<(), EvalSolveError> {
        for op in self.input.row {
            self.eval_op(op.clone())?;
        }
        Ok(())
    }

    // SPEC_0021: Exception - exhaustive checked execution dispatch over every LinearOp variant.
    #[allow(clippy::excessive_nesting, clippy::too_many_lines)]
    fn eval_op(&mut self, op: LinearOp) -> Result<(), EvalSolveError> {
        match op {
            LinearOp::Const { dst, value } => {
                self.set(dst, value)?;
            }
            LinearOp::LoadTime { dst } => self.set(dst, self.input.t)?,
            LinearOp::LoadY { dst, index } => {
                self.set(dst, self.input.read_input("y", self.input.y, index)?)?;
            }
            LinearOp::LoadP { dst, index } => {
                self.set(dst, self.input.read_input("p", self.input.p, index)?)?;
            }
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => self.eval_load_indexed_p(dst, base, count, index)?,
            LinearOp::LoadIndexedRegister {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let offset =
                    tensor_register_offset(&dimensions, &indices, |register| self.get(register))?;
                let value = match offset {
                    Some(offset) => self.get(base + (offset * stride) as Reg)?,
                    None => f64::NAN,
                };
                self.set(dst, value)?;
            }
            LinearOp::LoadIndexedFoldCarried {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let carried = self.input.fold_carried.ok_or_else(|| {
                    invalid_row("indexed function-fold carried load escaped its update body")
                })?;
                let offset =
                    tensor_register_offset(&dimensions, &indices, |register| self.get(register))?;
                let value = offset
                    .and_then(|offset| carried.get(base + offset * stride).copied())
                    .unwrap_or(f64::NAN);
                self.set(dst, value)?;
            }
            LinearOp::LoadIndexedFoldCapture {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let captures = self.input.fold_captures.ok_or_else(|| {
                    invalid_row("indexed function-fold capture load escaped its update body")
                })?;
                let offset =
                    tensor_register_offset(&dimensions, &indices, |register| self.get(register))?;
                let value = offset
                    .and_then(|offset| captures.get(base + offset * stride).copied())
                    .unwrap_or(f64::NAN);
                self.set(dst, value)?;
            }
            LinearOp::LoadIndexedSeed {
                dst,
                base,
                count,
                index,
            } => self.eval_load_indexed_seed(dst, base, count, index)?,
            LinearOp::LoadSeed { dst, index } => {
                let seed = self
                    .input
                    .context
                    .seed
                    .ok_or_else(|| self.input.missing_input("seed", index, 0))?;
                self.set(dst, self.input.read_input("seed", seed, index)?)?;
            }
            LinearOp::LoadFoldCarried { dst, index } => {
                let carried = self.input.fold_carried.ok_or_else(|| {
                    invalid_row("function-fold carried load escaped its update body")
                })?;
                self.set(dst, self.input.read_input("fold carried", carried, index)?)?;
            }
            LinearOp::LoadFoldIndex { dst, dimension } => {
                let indices = self.input.fold_indices.ok_or_else(|| {
                    invalid_row("function-fold binder load escaped its update body")
                })?;
                let value = indices
                    .get(dimension)
                    .copied()
                    .ok_or_else(|| invalid_row("function-fold binder dimension is out of range"))?;
                self.set(dst, value as f64)?;
            }
            LinearOp::LoadFoldCapture { dst, index } => {
                let captures = self.input.fold_captures.ok_or_else(|| {
                    invalid_row("function-fold capture load escaped its update body")
                })?;
                self.set(dst, self.input.read_input("fold capture", captures, index)?)?;
            }
            LinearOp::LoadFunctionConditionalCapture { dst, index } => {
                let captures = self.input.conditional_captures.ok_or_else(|| {
                    invalid_row("function-conditional capture load escaped its region")
                })?;
                self.set(
                    dst,
                    self.input
                        .read_input("function conditional capture", captures, index)?,
                )?;
            }
            LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start,
                index_start,
                count,
            } => {
                let captures = self.input.conditional_captures.ok_or_else(|| {
                    invalid_row("function-conditional capture range load escaped its region")
                })?;
                for offset in 0..count {
                    let value = self.input.read_input(
                        "function conditional capture",
                        captures,
                        index_start + offset,
                    )?;
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::Move { dst, src } => {
                self.set(dst, self.get(src)?)?;
            }
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => {
                let value = solve_component_op(
                    self.regs,
                    self.initialized,
                    LinearOp::LinearSolveComponent {
                        dst,
                        matrix_start,
                        rhs_start,
                        n,
                        component,
                    },
                )
                .map_err(|error| error.with_source_span(self.input.source_span))?;
                self.set(dst, value)?;
            }
            LinearOp::DotProduct {
                dst,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
            } => {
                let mut value = 0.0;
                for term in 0..count {
                    let lhs = lhs_start as usize + term * lhs_stride;
                    let rhs = rhs_start as usize + term * rhs_stride;
                    value += self.get(lhs as Reg)? * self.get(rhs as Reg)?;
                }
                self.set(dst, value)?;
            }
            LinearOp::MatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
            } => {
                let mut values = vec![0.0; rows * columns * lanes];
                for row in 0..rows {
                    for column in 0..columns {
                        let output = (row * columns + column) * lanes;
                        for term in 0..inner {
                            let lhs = (row * inner + term) * lanes;
                            let rhs = (term * columns + column) * lanes;
                            let lhs_re = self.get(lhs_start + lhs as Reg)?;
                            let rhs_re = self.get(rhs_start + rhs as Reg)?;
                            values[output] += lhs_re * rhs_re;
                            if lanes == 2 {
                                values[output + 1] += self.get(lhs_start + lhs as Reg + 1)?
                                    * rhs_re
                                    + lhs_re * self.get(rhs_start + rhs as Reg + 1)?;
                            }
                        }
                    }
                }
                for (offset, value) in values.into_iter().enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::TensorBinary {
                dst_start,
                op,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                lanes,
            } => {
                let mut values = Vec::with_capacity(count * lanes);
                for element in 0..count {
                    let lhs = lhs_start + (element * lhs_stride * lanes) as Reg;
                    let rhs = rhs_start + (element * rhs_stride * lanes) as Reg;
                    let lhs_re = self.get(lhs)?;
                    let rhs_re = self.get(rhs)?;
                    values.push(eval_tensor_binary_primal(op, lhs_re, rhs_re, lanes));
                    if lanes == 2 {
                        values.push(eval_tensor_binary_tangent(
                            op,
                            lhs_re,
                            self.get(lhs + 1)?,
                            rhs_re,
                            self.get(rhs + 1)?,
                        ));
                    }
                }
                for (offset, value) in values.into_iter().enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::TensorCross {
                dst_start,
                lhs_start,
                rhs_start,
                lanes,
            } => {
                let mut lhs = [0.0; 6];
                let mut rhs = [0.0; 6];
                for offset in 0..3 * lanes {
                    lhs[offset] = self.get(lhs_start + offset as Reg)?;
                    rhs[offset] = self.get(rhs_start + offset as Reg)?;
                }
                let values = tensor_cross_values(&lhs, &rhs, lanes);
                for (offset, value) in values.into_iter().take(3 * lanes).enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::TensorTranspose {
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes,
            } => {
                let value_width = element_width * lanes;
                let mut values = vec![0.0; rows * columns * value_width];
                for row in 0..rows {
                    for column in 0..columns {
                        for value in 0..value_width {
                            let dst = (row * columns + column) * value_width + value;
                            let src = (column * rows + row) * value_width + value;
                            values[dst] = self.get(src_start + src as Reg)?;
                        }
                    }
                }
                for (offset, value) in values.into_iter().enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis,
                lanes,
            } => {
                let count = dimensions
                    .iter()
                    .fold(lanes, |count, extent| count * *extent as usize);
                let mut values = vec![0.0; count];
                visit_tensor_concatenate(
                    &sources,
                    &dimensions,
                    axis,
                    lanes,
                    |source, destination| {
                        values[destination] = self.get(source)?;
                        Ok::<(), EvalSolveError>(())
                    },
                )?;
                for (offset, value) in values.into_iter().enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::TensorUpdate {
                dst_start,
                base_start,
                value_start,
                dimensions,
                subscripts,
                lanes,
            } => {
                let count = dimensions
                    .iter()
                    .fold(1usize, |count, extent| count * *extent as usize);
                let mut values = Vec::with_capacity(count * lanes);
                for element in 0..count {
                    let value_offset = tensor_update_register_value_offset(
                        &dimensions,
                        &subscripts,
                        element,
                        |register| self.get(register),
                    )?;
                    for lane in 0..lanes {
                        values.push(match value_offset {
                            Some(offset) => {
                                self.get(value_start + (offset * lanes + lane) as Reg)?
                            }
                            None => self.get(base_start + (element * lanes + lane) as Reg)?,
                        });
                    }
                }
                for (offset, value) in values.into_iter().enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::TensorFill {
                dst_start,
                value_start,
                count,
                lanes,
            } => {
                let values = (0..lanes)
                    .map(|lane| self.get(value_start + lane as Reg))
                    .collect::<Result<Vec<_>, _>>()?;
                for element in 0..count {
                    for (lane, &value) in values.iter().enumerate() {
                        self.set(dst_start + (element * lanes + lane) as Reg, value)?;
                    }
                }
            }
            LinearOp::TensorIdentity {
                dst_start,
                size,
                lanes,
            } => {
                for row in 0..size {
                    for column in 0..size {
                        for lane in 0..lanes {
                            let value = f64::from(lane == 0 && row == column);
                            self.set(
                                dst_start + ((row * size + column) * lanes + lane) as Reg,
                                value,
                            )?;
                        }
                    }
                }
            }
            LinearOp::TensorLoad {
                dst_start,
                input,
                input_start,
                count,
                seed_start,
                lanes,
            } => {
                for element in 0..count {
                    let value = match input {
                        rumoca_ir_solve::TensorInputKind::Y => self.input.y[input_start + element],
                        rumoca_ir_solve::TensorInputKind::P => self.input.p[input_start + element],
                    };
                    self.set(dst_start + (element * lanes) as Reg, value)?;
                    if lanes == 2 {
                        let tangent = match seed_start {
                            Some(seed_start) => self
                                .input
                                .context
                                .seed
                                .and_then(|seed| seed.get(seed_start + element))
                                .copied()
                                .ok_or_else(|| {
                                    self.input.missing_input("seed", seed_start + element, 0)
                                })?,
                            None => 0.0,
                        };
                        self.set(dst_start + (element * lanes + 1) as Reg, tangent)?;
                    }
                }
            }
            LinearOp::Unary { dst, op, arg } => {
                self.set(dst, eval_unary(op, self.get(arg)?))?;
            }
            LinearOp::Binary { dst, op, lhs, rhs } => {
                self.set(dst, eval_binary(op, self.get(lhs)?, self.get(rhs)?))?;
            }
            LinearOp::Compare { dst, op, lhs, rhs } => {
                self.set(dst, eval_compare(op, self.get(lhs)?, self.get(rhs)?))?;
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let value = if self.get(cond)? != 0.0 {
                    self.get(if_true)?
                } else {
                    self.get(if_false)?
                };
                self.set(dst, value)?;
            }
            LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            } => {
                let mut initial = Vec::with_capacity(program.carried_count);
                for offset in 0..program.carried_count {
                    initial.push(self.get(initial_start + offset as Reg)?);
                }
                let mut captures = Vec::with_capacity(program.capture_count);
                for offset in 0..program.capture_count {
                    captures.push(self.get(capture_start + offset as Reg)?);
                }
                let carried = eval_function_fold(self.input, &program, &initial, &captures)?;
                for (offset, value) in carried.into_iter().enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            } => {
                let mut carried = Vec::with_capacity(program.carried_count);
                for offset in 0..program.carried_count {
                    carried.push(self.get(initial_start + offset as Reg)?);
                }
                if self.get(activation)? != 0.0 {
                    let mut captures = Vec::with_capacity(program.capture_count);
                    for offset in 0..program.capture_count {
                        captures.push(self.get(capture_start + offset as Reg)?);
                    }
                    carried = eval_function_fold(self.input, &program, &carried, &captures)?;
                }
                for (offset, value) in carried.into_iter().enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::FunctionConditional {
                dst_start,
                capture_start,
                program,
            } => {
                let captures = (0..program.capture_count)
                    .map(|offset| self.get(capture_start + offset as Reg))
                    .collect::<Result<Vec<_>, _>>()?;
                let values = eval_function_conditional(self.input, &program, &captures)?;
                for (offset, value) in values.into_iter().enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } => {
                let values = eval_pure_call_payload(
                    self.input.context.pure_calls,
                    &site,
                    &input_starts,
                    |register| self.get(register),
                )?;
                for (offset, value) in values.into_iter().enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::PureCallDirectional {
                dst_start,
                input_starts,
                site,
            } => {
                let values = eval_pure_call_directional_payload(
                    self.input.context.pure_calls,
                    &site,
                    &input_starts,
                    |register| self.get(register),
                )?;
                for (offset, value) in values.into_iter().enumerate() {
                    self.set(dst_start + offset as Reg, value)?;
                }
            }
            LinearOp::StoreOutputFoldTensorUpdate {
                source_base,
                source_stride,
                dimensions,
                updates,
                nodes,
                result,
                lanes,
            } => {
                let carried = self.input.fold_carried.ok_or_else(|| {
                    invalid_row("aggregate output escaped its function-fold update body")
                })?;
                let count = dimensions.iter().fold(1usize, |count, extent| {
                    count.saturating_mul(*extent as usize)
                });
                for element in 0..count {
                    let mut offsets = Vec::with_capacity(updates.len());
                    for update in &updates {
                        offsets.push(tensor_update_value_offset(
                            &dimensions,
                            &update.subscripts,
                            element,
                            |register| self.get(register),
                        )?);
                    }
                    for lane in 0..lanes {
                        let source = *carried
                            .get(source_base + element * source_stride + lane)
                            .ok_or_else(|| {
                                invalid_row("tensor update carried source is out of range")
                            })?;
                        let mut values = Vec::with_capacity(nodes.len() + 1);
                        values.push(source);
                        for node in &nodes {
                            let value = match *node {
                                rumoca_ir_solve::FoldTensorNode::Update { base, update } => {
                                    let patch = &updates[update as usize];
                                    let enabled = patch
                                        .condition
                                        .map(|condition| {
                                            self.get(condition).map(|value| value != 0.0)
                                        })
                                        .transpose()?
                                        .unwrap_or(true);
                                    match (enabled, offsets[update as usize]) {
                                        (true, Some(value_element)) => self.get(
                                            patch.value_start
                                                + (value_element * patch.value_stride + lane)
                                                    as Reg,
                                        )?,
                                        _ => values[base as usize],
                                    }
                                }
                                rumoca_ir_solve::FoldTensorNode::Select {
                                    condition,
                                    if_true,
                                    if_false,
                                } => {
                                    if self.get(condition)? != 0.0 {
                                        values[if_true as usize]
                                    } else {
                                        values[if_false as usize]
                                    }
                                }
                            };
                            values.push(value);
                        }
                        self.sink.store(values[result as usize])?;
                    }
                }
            }
            LinearOp::StoreOutputFunctionFold {
                initial,
                capture_start,
                program,
                result_base,
                count,
                condition,
                nested_when_true,
            } => {
                let parent = self.input.fold_carried.ok_or_else(|| {
                    invalid_row("nested aggregate fold escaped its parent update body")
                })?;
                let mut values = Vec::with_capacity(program.carried_count);
                for source in initial.iter() {
                    match *source {
                        rumoca_ir_solve::FoldInitialSource::Registers { start, count } => {
                            for offset in 0..count {
                                values.push(self.get(start + offset as Reg)?);
                            }
                        }
                        rumoca_ir_solve::FoldInitialSource::ParentCarried { base, count } => {
                            values.extend_from_slice(&parent[base..base + count]);
                        }
                    }
                }
                let output_base = self.sink.cursor;
                let use_nested = match condition {
                    Some(condition) => (self.get(condition)? != 0.0) == nested_when_true,
                    None => true,
                };
                if use_nested {
                    let mut captures = Vec::with_capacity(program.capture_count);
                    for offset in 0..program.capture_count {
                        captures.push(self.get(capture_start + offset as Reg)?);
                    }
                    let folded = eval_function_fold(self.input, &program, &values, &captures)?;
                    for &nested in &folded[result_base..result_base + count] {
                        self.sink.store(nested)?;
                    }
                } else {
                    for offset in 0..count {
                        let value = *parent.get(output_base + offset).ok_or_else(|| {
                            invalid_row("conditional nested fold parent range is invalid")
                        })?;
                        self.sink.store(value)?;
                    }
                }
            }
            LinearOp::TableBounds { .. }
            | LinearOp::TableLookup { .. }
            | LinearOp::TableLookupSlope { .. }
            | LinearOp::TableNextEvent { .. } => {
                self.eval_table_op(&op)?;
            }
            LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. } => {
                self.eval_random_op(&op)?;
            }
            LinearOp::StoreOutput { src } => {
                let value = self.get(src)?;
                self.sink.store(value)?;
            }
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                for ordinal in 0..count {
                    let source = start
                        + Reg::try_from(ordinal * stride).map_err(|_| {
                            invalid_row("conditional output range exceeds registers")
                        })?;
                    self.sink.store(self.get(source)?)?;
                }
            }
        }
        Ok(())
    }

    fn eval_table_op(&mut self, op: &LinearOp) -> Result<(), EvalSolveError> {
        apply_table_op(
            self.regs,
            self.initialized,
            op,
            self.input.context,
            self.input.source_span,
        )
    }

    fn eval_random_op(&mut self, op: &LinearOp) -> Result<(), EvalSolveError> {
        apply_random_op(
            self.regs,
            self.initialized,
            op,
            self.input.t,
            self.input.context,
            self.input.source_span,
        )
    }

    fn eval_load_indexed_p(
        &mut self,
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<(), EvalSolveError> {
        let slot = resolve_indexed_slot(self.get(index)?, base, count);
        self.set(dst, self.input.read_input("p", self.input.p, slot)?)
    }

    fn eval_load_indexed_seed(
        &mut self,
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<(), EvalSolveError> {
        let seed = self
            .input
            .context
            .seed
            .ok_or_else(|| self.input.missing_input("seed", base, 0))?;
        let slot = resolve_indexed_slot(self.get(index)?, base, count);
        self.set(dst, self.input.read_input("seed", seed, slot)?)
    }

    fn get(&self, reg: Reg) -> Result<f64, EvalSolveError> {
        get(self.regs, self.initialized, reg, self.input.source_span)
    }

    fn set(&mut self, reg: Reg, value: f64) -> Result<(), EvalSolveError> {
        set(
            self.regs,
            self.initialized,
            reg,
            value,
            self.input.source_span,
        )
    }
}

// SPEC_0021: Exception - exhaustive fast execution dispatch over every LinearOp variant.
#[allow(clippy::excessive_nesting, clippy::too_many_lines)]
fn eval_row_prepared_fast(
    input: PreparedRowEval<'_, '_>,
    scratch: &mut RowEvalScratch,
    sink: &mut OutputCursor<'_>,
) -> Result<(), EvalSolveError> {
    scratch.regs.resize(input.register_count, 0.0);
    let regs = &mut scratch.regs;
    for op in input.row {
        if let LinearOp::StoreOutputFoldTensorUpdate {
            source_base,
            source_stride,
            dimensions,
            updates,
            nodes,
            result,
            lanes,
        } = op
        {
            eval_fold_tensor_update(
                regs,
                input,
                sink,
                *source_base,
                *source_stride,
                dimensions,
                updates,
                nodes,
                *result,
                *lanes,
            )?;
            continue;
        }
        match op {
            LinearOp::Const { dst, value } => regs[*dst as usize] = *value,
            LinearOp::LoadTime { dst } => regs[*dst as usize] = input.t,
            LinearOp::LoadY { dst, index } => {
                regs[*dst as usize] = input.y[*index];
            }
            LinearOp::LoadP { dst, index } => {
                regs[*dst as usize] = input.p[*index];
            }
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => {
                let slot = resolve_indexed_slot(regs[*index as usize], *base, *count);
                regs[*dst as usize] = input.read_input("p", input.p, slot)?;
            }
            LinearOp::LoadIndexedRegister {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let offset = tensor_register_offset(dimensions, indices, |register| {
                    Ok::<f64, EvalSolveError>(regs[register as usize])
                })?;
                regs[*dst as usize] = offset
                    .map(|offset| regs[*base as usize + offset * *stride])
                    .unwrap_or(f64::NAN);
            }
            LinearOp::LoadIndexedFoldCarried {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let carried = input.fold_carried.ok_or_else(|| {
                    invalid_row("indexed function-fold carried load escaped its update body")
                })?;
                let offset = tensor_register_offset(dimensions, indices, |register| {
                    Ok::<f64, EvalSolveError>(regs[register as usize])
                })?;
                regs[*dst as usize] = offset
                    .and_then(|offset| carried.get(*base + offset * *stride).copied())
                    .unwrap_or(f64::NAN);
            }
            LinearOp::LoadIndexedFoldCapture {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let captures = input.fold_captures.ok_or_else(|| {
                    invalid_row("indexed function-fold capture load escaped its update body")
                })?;
                let offset = tensor_register_offset(dimensions, indices, |register| {
                    Ok::<f64, EvalSolveError>(regs[register as usize])
                })?;
                regs[*dst as usize] = offset
                    .and_then(|offset| captures.get(*base + offset * *stride).copied())
                    .unwrap_or(f64::NAN);
            }
            LinearOp::LoadIndexedSeed {
                dst,
                base,
                count,
                index,
            } => {
                let seed = input
                    .context
                    .seed
                    .ok_or_else(|| input.missing_input("seed", *base, 0))?;
                let slot = resolve_indexed_slot(regs[*index as usize], *base, *count);
                regs[*dst as usize] = input.read_input("seed", seed, slot)?;
            }
            LinearOp::LoadSeed { dst, index } => {
                let seed = input
                    .context
                    .seed
                    .ok_or_else(|| input.missing_input("seed", *index, 0))?;
                regs[*dst as usize] = seed[*index];
            }
            LinearOp::LoadFoldCarried { dst, index } => {
                regs[*dst as usize] = input
                    .fold_carried
                    .and_then(|values| values.get(*index))
                    .copied()
                    .ok_or_else(|| invalid_row("invalid function-fold carried load"))?;
            }
            LinearOp::LoadFoldIndex { dst, dimension } => {
                regs[*dst as usize] = input
                    .fold_indices
                    .and_then(|values| values.get(*dimension))
                    .copied()
                    .ok_or_else(|| invalid_row("invalid function-fold binder load"))?
                    as f64;
            }
            LinearOp::LoadFoldCapture { dst, index } => {
                regs[*dst as usize] = input
                    .fold_captures
                    .and_then(|values| values.get(*index))
                    .copied()
                    .ok_or_else(|| invalid_row("invalid function-fold capture load"))?;
            }
            LinearOp::LoadFunctionConditionalCapture { dst, index } => {
                regs[*dst as usize] = input
                    .conditional_captures
                    .and_then(|values| values.get(*index))
                    .copied()
                    .ok_or_else(|| invalid_row("invalid function-conditional capture load"))?;
            }
            LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start,
                index_start,
                count,
            } => {
                let captures = input.conditional_captures.ok_or_else(|| {
                    invalid_row("invalid function-conditional capture range load")
                })?;
                regs[*dst_start as usize..*dst_start as usize + *count]
                    .copy_from_slice(&captures[*index_start..*index_start + *count]);
            }
            LinearOp::Move { dst, src } => regs[*dst as usize] = regs[*src as usize],
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => {
                regs[*dst as usize] =
                    solve_component_unchecked(regs, *matrix_start, *rhs_start, *n, *component)
                        .map_err(|error| error.with_source_span(input.source_span))?;
            }
            LinearOp::DotProduct {
                dst,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
            } => {
                let mut value = 0.0;
                for term in 0..*count {
                    value += regs[*lhs_start as usize + term * *lhs_stride]
                        * regs[*rhs_start as usize + term * *rhs_stride];
                }
                regs[*dst as usize] = value;
            }
            LinearOp::MatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
            } => eval_matrix_multiply(
                regs, *dst_start, *lhs_start, *rhs_start, *rows, *inner, *columns, *lanes,
            ),
            LinearOp::TensorBinary {
                dst_start,
                op,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                lanes,
            } => eval_tensor_binary(
                regs,
                *dst_start,
                *op,
                *lhs_start,
                *rhs_start,
                *count,
                *lhs_stride,
                *rhs_stride,
                *lanes,
            ),
            LinearOp::TensorCross {
                dst_start,
                lhs_start,
                rhs_start,
                lanes,
            } => eval_tensor_cross(regs, *dst_start, *lhs_start, *rhs_start, *lanes),
            LinearOp::TensorTranspose {
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes,
            } => eval_tensor_transpose(
                regs,
                *dst_start,
                *src_start,
                *rows,
                *columns,
                *element_width,
                *lanes,
            ),
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis,
                lanes,
            } => {
                visit_tensor_concatenate(
                    sources,
                    dimensions,
                    *axis,
                    *lanes,
                    |source, destination| {
                        regs[*dst_start as usize + destination] = regs[source as usize];
                        Ok::<(), EvalSolveError>(())
                    },
                )?;
            }
            LinearOp::TensorUpdate {
                dst_start,
                base_start,
                value_start,
                dimensions,
                subscripts,
                lanes,
            } => eval_tensor_update(
                regs,
                *dst_start,
                *base_start,
                *value_start,
                dimensions,
                subscripts,
                *lanes,
            )?,
            LinearOp::TensorFill {
                dst_start,
                value_start,
                count,
                lanes,
            } => {
                for element in 0..*count {
                    for lane in 0..*lanes {
                        regs[*dst_start as usize + element * *lanes + lane] =
                            regs[*value_start as usize + lane];
                    }
                }
            }
            LinearOp::TensorIdentity {
                dst_start,
                size,
                lanes,
            } => {
                let output =
                    &mut regs[*dst_start as usize..*dst_start as usize + *size * *size * *lanes];
                output.fill(0.0);
                for diagonal in 0..*size {
                    output[(diagonal * *size + diagonal) * *lanes] = 1.0;
                }
            }
            LinearOp::TensorLoad {
                dst_start,
                input: source,
                input_start,
                count,
                seed_start,
                lanes,
            } => {
                let input_values = match source {
                    rumoca_ir_solve::TensorInputKind::Y => input.y,
                    rumoca_ir_solve::TensorInputKind::P => input.p,
                };
                for element in 0..*count {
                    regs[*dst_start as usize + element * *lanes] =
                        input_values[*input_start + element];
                    if *lanes == 2 {
                        regs[*dst_start as usize + element * *lanes + 1] = match seed_start {
                            Some(seed_start) => input
                                .context
                                .seed
                                .and_then(|seed| seed.get(*seed_start + element))
                                .copied()
                                .ok_or_else(|| {
                                    input.missing_input("seed", *seed_start + element, 0)
                                })?,
                            None => 0.0,
                        };
                    }
                }
            }
            LinearOp::Unary { dst, op, arg } => {
                regs[*dst as usize] = eval_unary(*op, regs[*arg as usize]);
            }
            LinearOp::Binary { dst, op, lhs, rhs } => {
                regs[*dst as usize] = eval_binary(*op, regs[*lhs as usize], regs[*rhs as usize]);
            }
            LinearOp::Compare { dst, op, lhs, rhs } => {
                regs[*dst as usize] = eval_compare(*op, regs[*lhs as usize], regs[*rhs as usize]);
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                regs[*dst as usize] = if regs[*cond as usize] != 0.0 {
                    regs[*if_true as usize]
                } else {
                    regs[*if_false as usize]
                };
            }
            LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            } => {
                let initial = regs
                    [*initial_start as usize..*initial_start as usize + program.carried_count]
                    .to_vec();
                let captures = regs
                    [*capture_start as usize..*capture_start as usize + program.capture_count]
                    .to_vec();
                let carried = eval_function_fold(input, program, &initial, &captures)?;
                regs[*dst_start as usize..*dst_start as usize + program.carried_count]
                    .copy_from_slice(&carried);
            }
            LinearOp::GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            } => {
                let initial =
                    &regs[*initial_start as usize..*initial_start as usize + program.carried_count];
                if regs[*activation as usize] != 0.0 {
                    let captures = &regs
                        [*capture_start as usize..*capture_start as usize + program.capture_count];
                    let carried = eval_function_fold(input, program, initial, captures)?;
                    regs[*dst_start as usize..*dst_start as usize + program.carried_count]
                        .copy_from_slice(&carried);
                } else {
                    regs.copy_within(
                        *initial_start as usize..*initial_start as usize + program.carried_count,
                        *dst_start as usize,
                    );
                }
            }
            LinearOp::FunctionConditional {
                dst_start,
                capture_start,
                program,
            } => {
                let captures =
                    &regs[*capture_start as usize..*capture_start as usize + program.capture_count];
                let values = eval_function_conditional(input, program, captures)?;
                regs[*dst_start as usize..*dst_start as usize + program.result_count]
                    .copy_from_slice(&values);
            }
            LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } => {
                let values = eval_pure_call_payload(
                    input.context.pure_calls,
                    site,
                    input_starts,
                    |register| Ok(regs[register as usize]),
                )?;
                regs[*dst_start as usize..*dst_start as usize + values.len()]
                    .copy_from_slice(&values);
            }
            LinearOp::PureCallDirectional {
                dst_start,
                input_starts,
                site,
            } => {
                let values = eval_pure_call_directional_payload(
                    input.context.pure_calls,
                    site,
                    input_starts,
                    |register| Ok(regs[register as usize]),
                )?;
                regs[*dst_start as usize..*dst_start as usize + values.len()]
                    .copy_from_slice(&values);
            }
            LinearOp::StoreOutputFoldTensorUpdate { .. } => {
                unreachable!("tensor updates are evaluated through their borrowed owner")
            }
            LinearOp::StoreOutputFunctionFold {
                initial,
                capture_start,
                program,
                result_base,
                count,
                condition,
                nested_when_true,
            } => {
                let parent = input.fold_carried.ok_or_else(|| {
                    invalid_row("nested aggregate fold escaped its parent update body")
                })?;
                let mut values = Vec::with_capacity(program.carried_count);
                for source in initial.iter() {
                    match *source {
                        rumoca_ir_solve::FoldInitialSource::Registers { start, count } => {
                            values.extend_from_slice(&regs[start as usize..start as usize + count]);
                        }
                        rumoca_ir_solve::FoldInitialSource::ParentCarried { base, count } => {
                            values.extend_from_slice(&parent[base..base + count]);
                        }
                    }
                }
                let output_base = sink.cursor;
                let use_nested = condition
                    .map(|condition| (regs[condition as usize] != 0.0) == *nested_when_true)
                    .unwrap_or(true);
                if use_nested {
                    let captures = &regs
                        [*capture_start as usize..*capture_start as usize + program.capture_count];
                    let folded = eval_function_fold(input, program, &values, captures)?;
                    for &nested in &folded[*result_base..*result_base + *count] {
                        sink.store(nested)?;
                    }
                } else {
                    for offset in 0..*count {
                        let value = *parent.get(output_base + offset).ok_or_else(|| {
                            invalid_row("conditional nested fold parent range is invalid")
                        })?;
                        sink.store(value)?;
                    }
                }
            }
            LinearOp::TableBounds { .. }
            | LinearOp::TableLookup { .. }
            | LinearOp::TableLookupSlope { .. }
            | LinearOp::TableNextEvent { .. } => {
                eval_fast_table_op(regs, &mut scratch.initialized, input, op)?
            }
            LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. } => {
                eval_fast_random_op(regs, &mut scratch.initialized, input, op)?
            }
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                for ordinal in 0..*count {
                    sink.store(regs[*start as usize + ordinal * *stride])?;
                }
            }
            LinearOp::StoreOutput { src } => sink.store(regs[*src as usize])?,
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn eval_fold_tensor_update(
    regs: &[f64],
    input: PreparedRowEval<'_, '_>,
    sink: &mut OutputCursor<'_>,
    source_base: usize,
    source_stride: usize,
    dimensions: &[u32],
    updates: &[rumoca_ir_solve::FoldTensorUpdate],
    nodes: &[rumoca_ir_solve::FoldTensorNode],
    result: u32,
    lanes: usize,
) -> Result<(), EvalSolveError> {
    let carried = input
        .fold_carried
        .ok_or_else(|| invalid_row("aggregate output escaped its function-fold update body"))?;
    let count = dimensions.iter().fold(1usize, |count, extent| {
        count.saturating_mul(*extent as usize)
    });
    let mut offsets = Vec::with_capacity(updates.len());
    let mut values = Vec::with_capacity(nodes.len() + 1);
    for element in 0..count {
        offsets.clear();
        for update in updates {
            offsets.push(tensor_update_value_offset(
                dimensions,
                &update.subscripts,
                element,
                |register| Ok::<_, EvalSolveError>(regs[register as usize]),
            )?);
        }
        for lane in 0..lanes {
            values.clear();
            values.push(carried[source_base + element * source_stride + lane]);
            for node in nodes {
                values.push(fold_tensor_node_value(
                    regs, updates, &offsets, &values, lane, node,
                ));
            }
            sink.store(values[result as usize])?;
        }
    }
    Ok(())
}

fn fold_tensor_node_value(
    regs: &[f64],
    updates: &[rumoca_ir_solve::FoldTensorUpdate],
    offsets: &[Option<usize>],
    values: &[f64],
    lane: usize,
    node: &rumoca_ir_solve::FoldTensorNode,
) -> f64 {
    match *node {
        rumoca_ir_solve::FoldTensorNode::Update { base, update } => {
            let patch = &updates[update as usize];
            let enabled = patch
                .condition
                .map(|condition| regs[condition as usize] != 0.0)
                .unwrap_or(true);
            if !enabled {
                return values[base as usize];
            }
            offsets[update as usize].map_or(values[base as usize], |offset| {
                regs[patch.value_start as usize + offset * patch.value_stride + lane]
            })
        }
        rumoca_ir_solve::FoldTensorNode::Select {
            condition,
            if_true,
            if_false,
        } => {
            if regs[condition as usize] != 0.0 {
                values[if_true as usize]
            } else {
                values[if_false as usize]
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn eval_matrix_multiply(
    regs: &mut [f64],
    dst_start: Reg,
    lhs_start: Reg,
    rhs_start: Reg,
    rows: usize,
    inner: usize,
    columns: usize,
    lanes: usize,
) {
    for element in 0..rows.saturating_mul(columns) {
        let row = element / columns;
        let column = element % columns;
        let output = element * lanes;
        let mut re = 0.0;
        let mut du = 0.0;
        for term in 0..inner {
            let lhs = (row * inner + term) * lanes;
            let rhs = (term * columns + column) * lanes;
            let lhs_re = regs[lhs_start as usize + lhs];
            let rhs_re = regs[rhs_start as usize + rhs];
            re += lhs_re * rhs_re;
            if lanes == 2 {
                du += regs[lhs_start as usize + lhs + 1] * rhs_re
                    + lhs_re * regs[rhs_start as usize + rhs + 1];
            }
        }
        regs[dst_start as usize + output] = re;
        if lanes == 2 {
            regs[dst_start as usize + output + 1] = du;
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn eval_tensor_binary(
    regs: &mut [f64],
    dst_start: Reg,
    op: BinaryOp,
    lhs_start: Reg,
    rhs_start: Reg,
    count: usize,
    lhs_stride: usize,
    rhs_stride: usize,
    lanes: usize,
) {
    for element in 0..count {
        let lhs = lhs_start as usize + element * lhs_stride * lanes;
        let rhs = rhs_start as usize + element * rhs_stride * lanes;
        let dst = dst_start as usize + element * lanes;
        let lhs_re = regs[lhs];
        let rhs_re = regs[rhs];
        regs[dst] = eval_tensor_binary_primal(op, lhs_re, rhs_re, lanes);
        if lanes == 2 {
            regs[dst + 1] =
                eval_tensor_binary_tangent(op, lhs_re, regs[lhs + 1], rhs_re, regs[rhs + 1]);
        }
    }
}

fn eval_tensor_transpose(
    regs: &mut [f64],
    dst_start: Reg,
    src_start: Reg,
    rows: usize,
    columns: usize,
    element_width: usize,
    lanes: usize,
) {
    let value_width = element_width * lanes;
    for row in 0..rows {
        for column in 0..columns {
            for value in 0..value_width {
                let dst = dst_start as usize + (row * columns + column) * value_width + value;
                let src = src_start as usize + (column * rows + row) * value_width + value;
                regs[dst] = regs[src];
            }
        }
    }
}

fn visit_tensor_concatenate<E>(
    sources: &[rumoca_ir_solve::TensorConcatenateSource],
    dimensions: &[u32],
    axis: usize,
    lanes: usize,
    mut visit: impl FnMut(Reg, usize) -> Result<(), E>,
) -> Result<(), E> {
    let inner = dimensions[axis + 1..]
        .iter()
        .fold(1usize, |count, extent| count * *extent as usize);
    let result_axis = dimensions[axis] as usize;
    let mut axis_offset = 0usize;
    for source in sources {
        let source_axis = source.dimensions[axis] as usize;
        let source_count = source
            .dimensions
            .iter()
            .fold(1usize, |count, extent| count * *extent as usize);
        let source_block = source_axis * inner;
        for element in 0..source_count {
            let outer = element / source_block;
            let within = element % source_block;
            let destination = outer * result_axis * inner + axis_offset * inner + within;
            for lane in 0..lanes {
                visit(
                    source.start + (element * lanes + lane) as Reg,
                    destination * lanes + lane,
                )?;
            }
        }
        axis_offset += source_axis;
    }
    Ok(())
}

fn eval_tensor_update(
    regs: &mut [f64],
    dst_start: Reg,
    base_start: Reg,
    value_start: Reg,
    dimensions: &[u32],
    subscripts: &[rumoca_ir_solve::TensorUpdateSubscript],
    lanes: usize,
) -> Result<(), EvalSolveError> {
    let count = dimensions
        .iter()
        .fold(1usize, |count, extent| count * *extent as usize);
    for element in 0..count {
        let value_offset =
            tensor_update_register_value_offset(dimensions, subscripts, element, |register| {
                Ok::<_, EvalSolveError>(regs[register as usize])
            })?;
        for lane in 0..lanes {
            let source = match value_offset {
                Some(offset) => value_start as usize + offset * lanes + lane,
                None => base_start as usize + element * lanes + lane,
            };
            regs[dst_start as usize + element * lanes + lane] = regs[source];
        }
    }
    Ok(())
}

fn eval_tensor_binary_primal(op: BinaryOp, lhs_re: f64, rhs_re: f64, lanes: usize) -> f64 {
    let raw = eval_binary(op, lhs_re, rhs_re);
    if lanes == 2 && op == BinaryOp::Div && rhs_re == 0.0 && lhs_re == 0.0 {
        0.0
    } else {
        raw
    }
}

fn eval_tensor_cross(
    regs: &mut [f64],
    dst_start: Reg,
    lhs_start: Reg,
    rhs_start: Reg,
    lanes: usize,
) {
    let mut lhs = [0.0; 6];
    let mut rhs = [0.0; 6];
    let count = 3 * lanes;
    lhs[..count].copy_from_slice(&regs[lhs_start as usize..lhs_start as usize + count]);
    rhs[..count].copy_from_slice(&regs[rhs_start as usize..rhs_start as usize + count]);
    let values = tensor_cross_values(&lhs, &rhs, lanes);
    regs[dst_start as usize..dst_start as usize + count].copy_from_slice(&values[..count]);
}

fn tensor_cross_values(lhs: &[f64; 6], rhs: &[f64; 6], lanes: usize) -> [f64; 6] {
    let mut output = [0.0; 6];
    for (component, (first, second)) in [(1, 2), (2, 0), (0, 1)].into_iter().enumerate() {
        let dst = component * lanes;
        let first = first * lanes;
        let second = second * lanes;
        output[dst] = lhs[first] * rhs[second] - lhs[second] * rhs[first];
        if lanes == 2 {
            output[dst + 1] = lhs[first + 1] * rhs[second] + lhs[first] * rhs[second + 1]
                - lhs[second + 1] * rhs[first]
                - lhs[second] * rhs[first + 1];
        }
    }
    output
}

fn eval_tensor_binary_tangent(
    op: BinaryOp,
    lhs_re: f64,
    lhs_du: f64,
    rhs_re: f64,
    rhs_du: f64,
) -> f64 {
    match op {
        BinaryOp::Add => lhs_du + rhs_du,
        BinaryOp::Sub => lhs_du - rhs_du,
        BinaryOp::Mul => lhs_du * rhs_re + lhs_re * rhs_du,
        BinaryOp::Div => {
            if rhs_re == 0.0 {
                0.0
            } else {
                (lhs_du * rhs_re - lhs_re * rhs_du) / (rhs_re * rhs_re)
            }
        }
        _ => {
            unreachable!("TensorBinary validation accepts only add, subtract, multiply, and divide")
        }
    }
}

fn eval_function_fold(
    input: PreparedRowEval<'_, '_>,
    program: &rumoca_ir_solve::FunctionFoldProgram,
    initial: &[f64],
    captures: &[f64],
) -> Result<Vec<f64>, EvalSolveError> {
    let register_count = program.register_count;
    let mut carried = initial.to_vec();
    let mut next = vec![0.0; program.carried_count];
    let points = program
        .domain
        .index_tuple_iter()
        .map_err(|error| invalid_row(format!("invalid function-fold domain: {error}")))?;
    let mut scratch = RowEvalScratch::default();
    for indices in points {
        next.fill(0.0);
        let nested = PreparedRowEval::new(
            &program.update,
            register_count,
            input.y,
            input.p,
            input.t,
            input.context,
        )
        .with_source_span(input.source_span)
        .with_fold_context(&carried, &indices, captures);
        let mut sink = OutputCursor::new(&mut next);
        eval_row_prepared_fast(nested, &mut scratch, &mut sink)?;
        std::mem::swap(&mut carried, &mut next);
    }
    Ok(carried)
}

fn eval_function_conditional(
    input: PreparedRowEval<'_, '_>,
    program: &rumoca_ir_solve::FunctionConditionalProgram,
    captures: &[f64],
) -> Result<Vec<f64>, EvalSolveError> {
    for (arm_index, arm) in program.arms.iter().enumerate() {
        let condition = eval_function_conditional_region(
            input,
            &arm.condition,
            arm.condition_register_count,
            captures,
            1,
        )?;
        tracing::trace!(
            target: "rumoca_eval_solve::function_conditional",
            arm_index,
            condition = condition[0],
            result_count = program.result_count,
            capture_count = program.capture_count,
            "evaluate function-conditional arm"
        );
        if condition[0] != 0.0 {
            return eval_function_conditional_region(
                input,
                &arm.result,
                arm.result_register_count,
                captures,
                program.result_count,
            );
        }
    }
    eval_function_conditional_region(
        input,
        &program.fallback,
        program.fallback_register_count,
        captures,
        program.result_count,
    )
}

fn eval_function_conditional_region(
    input: PreparedRowEval<'_, '_>,
    region: &[LinearOp],
    register_count: usize,
    captures: &[f64],
    output_count: usize,
) -> Result<Vec<f64>, EvalSolveError> {
    let mut nested = PreparedRowEval::new(
        region,
        register_count,
        input.y,
        input.p,
        input.t,
        input.context,
    )
    .with_source_span(input.source_span)
    .with_conditional_captures(captures);
    nested.fold_carried = input.fold_carried;
    nested.fold_indices = input.fold_indices;
    nested.fold_captures = input.fold_captures;
    let mut values = vec![0.0; output_count];
    let mut scratch = RowEvalScratch::default();
    let mut sink = OutputCursor::new(&mut values);
    eval_row_prepared_fast(nested, &mut scratch, &mut sink)?;
    Ok(values)
}

fn tensor_register_offset<E>(
    dimensions: &[u32],
    indices: &[rumoca_ir_solve::TensorIndex],
    mut read: impl FnMut(Reg) -> Result<f64, E>,
) -> Result<Option<usize>, E> {
    let mut offset = 0usize;
    for (&extent, index) in dimensions.iter().zip(indices) {
        let Some(coordinate) = tensor_index_coordinate(*index, extent, &mut read)? else {
            return Ok(None);
        };
        offset = offset * extent as usize + coordinate;
    }
    Ok(Some(offset))
}

fn tensor_update_value_offset<E>(
    dimensions: &[u32],
    subscripts: &[rumoca_ir_solve::TensorSubscript],
    element: usize,
    mut read: impl FnMut(Reg) -> Result<f64, E>,
) -> Result<Option<usize>, E> {
    let mut value_offset = 0usize;
    let mut axis_stride = dimensions.iter().fold(1usize, |count, extent| {
        count.saturating_mul(*extent as usize)
    });
    for (&extent, subscript) in dimensions.iter().zip(subscripts) {
        axis_stride /= extent as usize;
        let coordinate = (element / axis_stride) % extent as usize;
        match *subscript {
            rumoca_ir_solve::TensorSubscript::Whole => {
                value_offset = value_offset * extent as usize + coordinate;
            }
            rumoca_ir_solve::TensorSubscript::Index(index) => {
                let Some(selected) = tensor_index_coordinate(index, extent, &mut read)? else {
                    return Ok(None);
                };
                if selected != coordinate {
                    return Ok(None);
                }
            }
        }
    }
    Ok(Some(value_offset))
}

fn tensor_update_register_value_offset<E>(
    dimensions: &[u32],
    subscripts: &[rumoca_ir_solve::TensorUpdateSubscript],
    element: usize,
    mut read: impl FnMut(Reg) -> Result<f64, E>,
) -> Result<Option<usize>, E> {
    let mut value_offset = 0usize;
    let mut axis_stride = dimensions.iter().fold(1usize, |count, extent| {
        count.saturating_mul(*extent as usize)
    });
    for (&extent, subscript) in dimensions.iter().zip(subscripts) {
        axis_stride /= extent as usize;
        let coordinate = (element / axis_stride) % extent as usize;
        match subscript {
            rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                value_offset = value_offset * extent as usize + coordinate;
            }
            rumoca_ir_solve::TensorUpdateSubscript::Index(index) => {
                let Some(selected) = tensor_index_coordinate(*index, extent, &mut read)? else {
                    return Ok(None);
                };
                if selected != coordinate {
                    return Ok(None);
                }
            }
            rumoca_ir_solve::TensorUpdateSubscript::Slice { start, dimensions } => {
                let slice_count = dimensions.iter().fold(1usize, |count, extent| {
                    count.saturating_mul(*extent as usize)
                });
                let Some(selected) =
                    tensor_slice_coordinate_offset(*start, slice_count, coordinate, &mut read)?
                else {
                    return Ok(None);
                };
                value_offset = value_offset * slice_count + selected;
            }
        }
    }
    Ok(Some(value_offset))
}

fn tensor_index_coordinate<E>(
    index: rumoca_ir_solve::TensorIndex,
    extent: u32,
    read: &mut impl FnMut(Reg) -> Result<f64, E>,
) -> Result<Option<usize>, E> {
    let value = match index {
        rumoca_ir_solve::TensorIndex::Constant(coordinate) => return Ok(Some(coordinate as usize)),
        rumoca_ir_solve::TensorIndex::Runtime(register) => read(register)?,
    };
    let rounded = value.round();
    Ok(
        (value.is_finite() && rounded == value && rounded >= 1.0 && rounded <= f64::from(extent))
            .then(|| rounded as usize - 1),
    )
}

fn tensor_slice_coordinate_offset<E>(
    start: Reg,
    count: usize,
    coordinate: usize,
    read: &mut impl FnMut(Reg) -> Result<f64, E>,
) -> Result<Option<usize>, E> {
    for offset in 0..count {
        let value = read(start + offset as Reg)?;
        if value.is_finite() && value.round() == value && value == (coordinate + 1) as f64 {
            return Ok(Some(offset));
        }
    }
    Ok(None)
}

fn eval_fast_table_op(
    regs: &mut [f64],
    initialized: &mut Vec<bool>,
    input: PreparedRowEval<'_, '_>,
    op: &LinearOp,
) -> Result<(), EvalSolveError> {
    initialized.resize(input.register_count, true);
    apply_table_op(regs, initialized, op, input.context, input.source_span)
}

fn eval_fast_random_op(
    regs: &mut [f64],
    initialized: &mut Vec<bool>,
    input: PreparedRowEval<'_, '_>,
    op: &LinearOp,
) -> Result<(), EvalSolveError> {
    initialized.resize(input.register_count, true);
    apply_random_op(
        regs,
        initialized,
        op,
        input.t,
        input.context,
        input.source_span,
    )
}

fn apply_table_op(
    regs: &mut [f64],
    initialized: &mut [bool],
    op: &LinearOp,
    context: RowEvalContext<'_>,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    match *op {
        LinearOp::TableBounds { dst, table_id, max } => {
            let table_id = get(regs, initialized, table_id, span)?;
            let tables = context.external_tables.unwrap_or(&[]);
            let operation = if max { "bounds max" } else { "bounds min" };
            let value = eval_table_bound_value_in(table_id, max, tables)
                .map_err(|error| external_table_error(operation, table_id, None, error))?;
            set(regs, initialized, dst, value, span)?;
        }
        LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        } => {
            let table_id = get(regs, initialized, table_id, span)?;
            let column = get(regs, initialized, column, span)?;
            let input = get(regs, initialized, input, span)?;
            let tables = context.external_tables.unwrap_or(&[]);
            let value = eval_table_lookup_value_in(table_id, column, input, tables)
                .map_err(|error| external_table_error("lookup", table_id, Some(column), error))?;
            set(regs, initialized, dst, value, span)?;
        }
        LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => {
            let table_id = get(regs, initialized, table_id, span)?;
            let column = get(regs, initialized, column, span)?;
            let input = get(regs, initialized, input, span)?;
            let tables = context.external_tables.unwrap_or(&[]);
            let value = eval_table_lookup_slope_value_in(table_id, column, input, tables).map_err(
                |error| external_table_error("lookup slope", table_id, Some(column), error),
            )?;
            set(regs, initialized, dst, value, span)?;
        }
        LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        } => {
            let table_id = get(regs, initialized, table_id, span)?;
            let time = get(regs, initialized, time, span)?;
            let tables = context.external_tables.unwrap_or(&[]);
            let value = eval_time_table_next_event_value_in(table_id, time, tables)
                .map_err(|error| external_table_error("next event", table_id, None, error))?;
            set(regs, initialized, dst, value, span)?;
        }
        _ => {
            return Err(EvalSolveError::InvalidLinearOp {
                helper: "table",
                op: linear_op_name(op),
            });
        }
    }
    Ok(())
}

fn external_table_error(
    operation: &'static str,
    table_id: f64,
    column: Option<f64>,
    error: TableRuntimeError,
) -> EvalSolveError {
    EvalSolveError::ExternalTable {
        operation,
        table_id,
        column,
        reason: error.to_string(),
    }
}

fn apply_random_op(
    regs: &mut [f64],
    initialized: &mut [bool],
    op: &LinearOp,
    t: f64,
    context: RowEvalContext<'_>,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    match *op {
        LinearOp::RandomInitialState {
            dst,
            generator,
            local_seed,
            global_seed,
            state_len,
            state_index,
        } => {
            let values = initial_state_values(
                generator,
                get(regs, initialized, local_seed, span)?.round() as i64,
                get(regs, initialized, global_seed, span)?.round() as i64,
                state_len,
            )?;
            set(
                regs,
                initialized,
                dst,
                projected_random_value(&values, state_index)?,
                span,
            )?;
        }
        LinearOp::RandomResult {
            dst,
            generator,
            state_start,
            state_len,
        } => {
            let state = read_reg_range(regs, initialized, state_start, state_len)
                .map_err(|error| error.with_source_span(span))?;
            let (result, _) = random_result_and_state(generator, &state)?;
            set(regs, initialized, dst, result, span)?;
        }
        LinearOp::RandomState {
            dst,
            generator,
            state_start,
            state_len,
            state_index,
        } => {
            let state = read_reg_range(regs, initialized, state_start, state_len)
                .map_err(|error| error.with_source_span(span))?;
            let (_, next_state) = random_result_and_state(generator, &state)?;
            set(
                regs,
                initialized,
                dst,
                projected_random_value(&next_state, state_index)?,
                span,
            )?;
        }
        LinearOp::ImpureRandomInit { dst, seed } => {
            let stream_id =
                impure_random_stream_id(get(regs, initialized, seed, span)?.round() as i64);
            set(regs, initialized, dst, stream_id as f64, span)?;
        }
        LinearOp::ImpureRandom { dst, id, call_site } => {
            let id = get(regs, initialized, id, span)?.round() as i64;
            set(
                regs,
                initialized,
                dst,
                impure_random_sample(id, call_site, t, impure_random_mutex(context)?),
                span,
            )?;
        }
        LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            call_site,
        } => {
            let id = get(regs, initialized, id, span)?.round() as i64;
            let imin = get(regs, initialized, imin, span)?.round() as i64;
            let imax = get(regs, initialized, imax, span)?.round() as i64;
            let (lo, hi) = if imin <= imax {
                (imin, imax)
            } else {
                (imax, imin)
            };
            let random_span = (hi - lo + 1).max(1) as f64;
            let y = lo as f64
                + (impure_random_sample(id, call_site, t, impure_random_mutex(context)?)
                    * random_span)
                    .floor();
            set(regs, initialized, dst, y.clamp(lo as f64, hi as f64), span)?;
        }
        _ => {
            return Err(EvalSolveError::InvalidLinearOp {
                helper: "random",
                op: linear_op_name(op),
            });
        }
    }
    Ok(())
}

fn linear_op_name(op: &LinearOp) -> &'static str {
    match op {
        LinearOp::Const { .. } => "Const",
        LinearOp::LoadTime { .. } => "LoadTime",
        LinearOp::LoadY { .. } => "LoadY",
        LinearOp::LoadP { .. } => "LoadP",
        LinearOp::LoadIndexedP { .. } => "LoadIndexedP",
        LinearOp::LoadIndexedRegister { .. } => "LoadIndexedRegister",
        LinearOp::LoadIndexedFoldCarried { .. } => "LoadIndexedFoldCarried",
        LinearOp::LoadIndexedFoldCapture { .. } => "LoadIndexedFoldCapture",
        LinearOp::LoadSeed { .. } => "LoadSeed",
        LinearOp::LoadIndexedSeed { .. } => "LoadIndexedSeed",
        LinearOp::LoadFoldCarried { .. } => "LoadFoldCarried",
        LinearOp::LoadFoldIndex { .. } => "LoadFoldIndex",
        LinearOp::LoadFoldCapture { .. } => "LoadFoldCapture",
        LinearOp::LoadFunctionConditionalCapture { .. } => "LoadFunctionConditionalCapture",
        LinearOp::LoadFunctionConditionalCaptureRange { .. } => {
            "LoadFunctionConditionalCaptureRange"
        }
        LinearOp::Move { .. } => "Move",
        LinearOp::LinearSolveComponent { .. } => "LinearSolveComponent",
        LinearOp::DotProduct { .. } => "DotProduct",
        LinearOp::MatrixMultiply { .. } => "MatrixMultiply",
        LinearOp::TensorBinary { .. } => "TensorBinary",
        LinearOp::TensorCross { .. } => "TensorCross",
        LinearOp::TensorTranspose { .. } => "TensorTranspose",
        LinearOp::TensorConcatenate { .. } => "TensorConcatenate",
        LinearOp::TensorUpdate { .. } => "TensorUpdate",
        LinearOp::TensorFill { .. } => "TensorFill",
        LinearOp::TensorIdentity { .. } => "TensorIdentity",
        LinearOp::TensorLoad { .. } => "TensorLoad",
        LinearOp::TableBounds { .. } => "TableBounds",
        LinearOp::TableLookup { .. } => "TableLookup",
        LinearOp::TableLookupSlope { .. } => "TableLookupSlope",
        LinearOp::TableNextEvent { .. } => "TableNextEvent",
        LinearOp::RandomInitialState { .. } => "RandomInitialState",
        LinearOp::RandomResult { .. } => "RandomResult",
        LinearOp::RandomState { .. } => "RandomState",
        LinearOp::ImpureRandomInit { .. } => "ImpureRandomInit",
        LinearOp::ImpureRandom { .. } => "ImpureRandom",
        LinearOp::ImpureRandomInteger { .. } => "ImpureRandomInteger",
        LinearOp::Unary { .. } => "Unary",
        LinearOp::Binary { .. } => "Binary",
        LinearOp::Compare { .. } => "Compare",
        LinearOp::Select { .. } => "Select",
        LinearOp::FunctionFold { .. } => "FunctionFold",
        LinearOp::GuardedFunctionFold { .. } => "GuardedFunctionFold",
        LinearOp::FunctionConditional { .. } => "FunctionConditional",
        LinearOp::PureCall { .. } => "PureCall",
        LinearOp::PureCallDirectional { .. } => "PureCallDirectional",
        LinearOp::StoreOutputFoldTensorUpdate { .. } => "StoreOutputFoldTensorUpdate",
        LinearOp::StoreOutputFunctionFold { .. } => "StoreOutputFunctionFold",
        LinearOp::StoreOutputRange { .. } => "StoreOutputRange",
        LinearOp::StoreOutput { .. } => "StoreOutput",
    }
}

fn eval_pure_call_payload(
    table: Option<&SolvePureCallTable>,
    site: &SolvePureCallSite,
    input_starts: &[Reg],
    mut read: impl FnMut(Reg) -> Result<f64, EvalSolveError>,
) -> Result<Vec<f64>, EvalSolveError> {
    let table = table.ok_or(EvalSolveError::MissingRuntimeState {
        operation: "PureCall table",
    })?;
    if !table.matches_site(site) || input_starts.len() != site.inputs().len() {
        return Err(invalid_row("pure-call site does not match its model owner"));
    }
    eval_typed_call_payload(
        table,
        site.owner(),
        site.inputs(),
        site.output_scalar_count(),
        input_starts,
        &mut read,
        false,
    )
}

fn eval_pure_call_directional_payload(
    table: Option<&SolvePureCallTable>,
    site: &SolvePureCallDirectionalSite,
    input_starts: &[Reg],
    mut read: impl FnMut(Reg) -> Result<f64, EvalSolveError>,
) -> Result<Vec<f64>, EvalSolveError> {
    let table = table.ok_or(EvalSolveError::MissingRuntimeState {
        operation: "PureCallDirectional table",
    })?;
    if !table.matches_directional_site(site) || input_starts.len() != site.inputs().len() {
        return Err(invalid_row(
            "directional pure-call site does not match its model owner",
        ));
    }
    eval_typed_call_payload(
        table,
        site.owner(),
        site.inputs(),
        site.output_scalar_count(),
        input_starts,
        &mut read,
        true,
    )
}

#[allow(clippy::too_many_arguments)]
fn eval_typed_call_payload(
    table: &SolvePureCallTable,
    owner: rumoca_ir_solve::SolvePureCallOwnerId,
    inputs: &[SolveValueType],
    output_count: Option<usize>,
    input_starts: &[Reg],
    read: &mut impl FnMut(Reg) -> Result<f64, EvalSolveError>,
    directional: bool,
) -> Result<Vec<f64>, EvalSolveError> {
    let mut arguments = Vec::with_capacity(inputs.len());
    for (&start, value_type) in input_starts.iter().zip(inputs) {
        let mut elements = Vec::with_capacity(value_type.scalar_count() as usize);
        for offset in 0..value_type.scalar_count() as usize {
            let register =
                start
                    .checked_add(Reg::try_from(offset).map_err(|_| {
                        invalid_row("pure-call input range exceeds register identity")
                    })?)
                    .ok_or_else(|| invalid_row("pure-call input range overflows"))?;
            elements.push(typed_kind_from_scalar(read(register)?, value_type)?);
        }
        arguments.push(
            TypedValue::construct(value_type.clone(), elements)
                .map_err(|error| invalid_row(error.to_string()))?,
        );
    }
    let outputs = if directional {
        typed_program::eval_pure_call_directional(table, owner, &arguments)
    } else {
        typed_program::eval_pure_call(table, owner, &arguments)
    }
    .map_err(|error| invalid_row(error.to_string()))?;
    let mut flattened = Vec::with_capacity(
        output_count.ok_or_else(|| invalid_row("pure-call output width overflows"))?,
    );
    for output in outputs {
        flattened.extend(output.elements().iter().copied().map(typed_kind_to_scalar));
    }
    Ok(flattened)
}

pub(crate) fn typed_kind_from_scalar(
    value: f64,
    value_type: &SolveValueType,
) -> Result<SolveValueKind, EvalSolveError> {
    Ok(match value_type.element_type() {
        SolveScalarType::Real {
            format: rumoca_ir_solve::SolveRealFormat::Binary32,
            ..
        } => SolveValueKind::Real32((value as f32).to_bits()),
        SolveScalarType::Real {
            format: rumoca_ir_solve::SolveRealFormat::Binary64,
            ..
        } => SolveValueKind::Real64(value.to_bits()),
        SolveScalarType::Integer(domain) => {
            if !value.is_finite() || value.fract() != 0.0 {
                return Err(invalid_row("pure-call Integer input is not integral"));
            }
            let integer = value as i64;
            if !domain.contains(integer) || integer as f64 != value {
                return Err(invalid_row("pure-call Integer input is outside its domain"));
            }
            SolveValueKind::Integer(integer)
        }
        SolveScalarType::Boolean => SolveValueKind::Boolean(value != 0.0),
    })
}

pub(crate) fn typed_kind_to_scalar(value: SolveValueKind) -> f64 {
    match value {
        SolveValueKind::Real32(bits) => f32::from_bits(bits) as f64,
        SolveValueKind::Real64(bits) => f64::from_bits(bits),
        SolveValueKind::Integer(value) => value as f64,
        SolveValueKind::Boolean(value) => f64::from(value),
    }
}

pub(crate) fn required_registers(row: &[LinearOp]) -> Result<usize, EvalSolveError> {
    ScalarProgramRegisterFlow::derive(row)
        .map(ScalarProgramRegisterFlow::register_count)
        .map_err(|error| invalid_row(error.to_string()))
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct RowInputRequirements {
    pub y_len: usize,
    pub p_len: usize,
    pub seed_len: usize,
}

impl RowInputRequirements {
    pub fn merge(self, other: Self) -> Self {
        Self {
            y_len: self.y_len.max(other.y_len),
            p_len: self.p_len.max(other.p_len),
            seed_len: self.seed_len.max(other.seed_len),
        }
    }
}

pub fn scalar_program_block_input_requirements(
    block: &ScalarProgramBlock,
) -> Result<RowInputRequirements, EvalSolveError> {
    block
        .programs()
        .iter()
        .enumerate()
        .map(|(row_idx, row)| (row_idx, row_input_requirements(row)))
        .try_fold(
            RowInputRequirements::default(),
            |requirements, (row_idx, row)| {
                row.map(|row_requirements| requirements.merge(row_requirements))
                    .map_err(|error| error.with_source_span(block.program_span(row_idx)))
            },
        )
}

pub fn row_input_requirements(row: &[LinearOp]) -> Result<RowInputRequirements, EvalSolveError> {
    row.iter()
        .cloned()
        .map(input_requirements_for_op)
        .try_fold(RowInputRequirements::default(), |requirements, op| {
            op.map(|op_requirements| requirements.merge(op_requirements))
        })
}

fn input_requirements_for_op(op: LinearOp) -> Result<RowInputRequirements, EvalSolveError> {
    match op {
        LinearOp::LoadY { index, .. } => Ok(RowInputRequirements {
            y_len: checked_required_len("y", index)?,
            ..Default::default()
        }),
        LinearOp::LoadP { index, .. } => Ok(RowInputRequirements {
            p_len: checked_required_len("p", index)?,
            ..Default::default()
        }),
        LinearOp::LoadIndexedP { base, count, .. } => Ok(RowInputRequirements {
            p_len: checked_required_indexed_len("p", base, count)?,
            ..Default::default()
        }),
        LinearOp::LoadSeed { index, .. } => Ok(RowInputRequirements {
            seed_len: checked_required_len("seed", index)?,
            ..Default::default()
        }),
        LinearOp::LoadIndexedSeed { base, count, .. } => Ok(RowInputRequirements {
            seed_len: checked_required_indexed_len("seed", base, count)?,
            ..Default::default()
        }),
        // A tensor load reads `count` contiguous entries of its input vector,
        // and its dual lane reads the same width of the seed vector. Reporting
        // that width is what makes the caller size the seed buffer it passes:
        // a row whose only seed reader is a tensor load would otherwise be
        // evaluated against a buffer sized for no seed at all.
        LinearOp::TensorLoad {
            input,
            input_start,
            count,
            seed_start,
            lanes,
            ..
        } => {
            let vector_len = checked_required_indexed_len(
                match input {
                    rumoca_ir_solve::TensorInputKind::Y => "y",
                    rumoca_ir_solve::TensorInputKind::P => "p",
                },
                input_start,
                count,
            )?;
            let seed_len = match seed_start {
                Some(seed_start) if lanes == 2 => {
                    checked_required_indexed_len("seed", seed_start, count)?
                }
                _ => 0,
            };
            Ok(match input {
                rumoca_ir_solve::TensorInputKind::Y => RowInputRequirements {
                    y_len: vector_len,
                    seed_len,
                    ..Default::default()
                },
                rumoca_ir_solve::TensorInputKind::P => RowInputRequirements {
                    p_len: vector_len,
                    seed_len,
                    ..Default::default()
                },
            })
        }
        LinearOp::FunctionFold { program, .. }
        | LinearOp::GuardedFunctionFold { program, .. }
        | LinearOp::StoreOutputFunctionFold { program, .. } => {
            row_input_requirements(&program.update)
        }
        _ => Ok(RowInputRequirements::default()),
    }
}

fn checked_required_len(vector: &'static str, index: usize) -> Result<usize, EvalSolveError> {
    index
        .checked_add(1)
        .ok_or_else(|| invalid_row(format!("{vector} input requirement overflow")))
}

fn checked_required_indexed_len(
    vector: &'static str,
    base: usize,
    count: usize,
) -> Result<usize, EvalSolveError> {
    let last_offset = if count == 0 { 0 } else { count - 1 };
    let last_index = base
        .checked_add(last_offset)
        .ok_or_else(|| invalid_row(format!("{vector} indexed input requirement overflow")))?;
    checked_required_len(vector, last_index)
}

pub fn validate_scalar_program_block_io(
    block: &ScalarProgramBlock,
    y: &[f64],
    p: &[f64],
    seed: Option<&[f64]>,
    out: &[f64],
) -> Result<(), EvalSolveError> {
    validate_output_len(out, block.output_count())?;
    validate_input_requirements(scalar_program_block_input_requirements(block)?, y, p, seed)
}

pub fn validate_row_inputs(
    row: &[LinearOp],
    y: &[f64],
    p: &[f64],
    seed: Option<&[f64]>,
) -> Result<(), EvalSolveError> {
    validate_input_requirements(row_input_requirements(row)?, y, p, seed)
}

pub(crate) fn validate_output_len(out: &[f64], required: usize) -> Result<(), EvalSolveError> {
    if out.len() >= required {
        return Ok(());
    }
    Err(EvalSolveError::OutputTooSmall {
        required,
        len: out.len(),
        span: None,
    })
}

/// Validate that `y`/`p`/`seed` are long enough for `requirements`.
///
/// `pub` for `rumoca_solver::runtime::solve_runtime`, which validates solver
/// vectors before handing them to a prepared row.
pub fn validate_input_requirements(
    requirements: RowInputRequirements,
    y: &[f64],
    p: &[f64],
    seed: Option<&[f64]>,
) -> Result<(), EvalSolveError> {
    validate_input_requirements_with_span(requirements, y, p, seed, None)
}

pub(crate) fn validate_input_requirements_with_span(
    requirements: RowInputRequirements,
    y: &[f64],
    p: &[f64],
    seed: Option<&[f64]>,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    validate_input_len("y", y.len(), requirements.y_len, span)?;
    validate_input_len("p", p.len(), requirements.p_len, span)?;
    if requirements.seed_len == 0 {
        return Ok(());
    }
    validate_input_len(
        "seed",
        seed.map_or(0, <[f64]>::len),
        requirements.seed_len,
        span,
    )
}

fn validate_input_len(
    vector: &'static str,
    actual_len: usize,
    required_len: usize,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    if actual_len >= required_len {
        return Ok(());
    }
    Err(EvalSolveError::MissingInput {
        vector,
        index: required_len - 1,
        len: actual_len,
        span,
    })
}

fn invalid_row(message: impl Into<String>) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: message.into(),
        span: None,
    }
}

fn eval_solve_f64_values(
    len: usize,
    value: f64,
    context: &'static str,
) -> Result<Vec<f64>, EvalSolveError> {
    let mut values = eval_solve_vec_with_capacity(len, context)?;
    values.resize(len, value);
    Ok(values)
}

fn eval_solve_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
) -> Result<Vec<T>, EvalSolveError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| invalid_row(format!("{context} exceeds host memory limits")))?;
    Ok(values)
}

fn set(
    regs: &mut [f64],
    initialized: &mut [bool],
    reg: Reg,
    value: f64,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    let index = reg as usize;
    let len = regs.len();
    let slot = regs
        .get_mut(index)
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "write",
            register: reg,
            len,
            span,
        })?;
    *slot = value;
    if let Some(init) = initialized.get_mut(index) {
        *init = true;
    }
    Ok(())
}

fn get(
    regs: &[f64],
    initialized: &[bool],
    reg: Reg,
    span: Option<rumoca_core::Span>,
) -> Result<f64, EvalSolveError> {
    let index = reg as usize;
    let value = regs
        .get(index)
        .copied()
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: reg,
            len: regs.len(),
            span,
        })?;
    if !initialized.get(index).copied().unwrap_or(false) {
        return Err(EvalSolveError::UninitializedRegister {
            register: reg,
            span,
        });
    }
    Ok(value)
}

#[cfg(test)]
mod tests;
