//! Solve-IR row evaluation.
//!
//! ## Threading and Test Isolation
//!
//! Simulation facades should pass model-local table data through
//! [`RowEvalContext::external_tables`]. Impure random-generator streams are
//! carried by [`SimulationRuntimeState`] and are never process-global.

use std::{
    collections::BTreeMap,
    sync::{
        Arc, Mutex, OnceLock,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
    time::Instant,
};

use rumoca_ir_solve::{
    BinaryOp, CompareOp, LinearOp, Reg, ScalarProgramBlock, SolveEventActionKind,
    SolveEventMessagePart, SolveEventPartition, SolveProblemShapeContractError, UnaryOp,
    resolve_indexed_slot,
};

mod compute_block_scalarize;
pub mod eval_at;
mod inspect_alloc;
mod iterative_solve;
pub mod jacobian;
mod linear_solve;
pub mod nan_trace;
mod prepared;
mod random_runtime;
mod refresh_plan;
mod reverse;
mod runtime;
mod runtime_events;
pub mod sim_driver;
mod sparsity;
mod table_runtime;
mod update_rows;
pub use compute_block_scalarize::{
    ScalarizeError, checked_contiguous_output_count, checked_tensor_output_count,
    scalar_program_output_count, scalar_program_output_indices, tensor_output_indices,
    to_scalar_program_block,
};
pub use eval_at::{EvalAtReport, EvalAtSlot};
pub use jacobian::{
    JacobianReport, ObjectiveGradientReport, ParameterJacobianReport, SteadyStateSensitivityReport,
};
use linear_solve::{solve_component_op, solve_component_unchecked};
pub use prepared::{
    PreparedComputeBlock, PreparedScalarProgramBlock, TargetAssignmentShape,
    target_assignment_shape,
};
use random_runtime::{
    ImpureRandomState, impure_random_mutex, impure_random_sample, impure_random_stream_id,
    initial_state_values, projected_random_value, random_result_and_state, read_reg_range,
};
pub use runtime::{
    AlgebraicLinearization, AlgebraicSettle, EventUpdateRowFilter, InitialEventObservation,
    ProjectedEventUpdateInput, ProjectedInitialEventInput, ProjectedInitialEventOutcome,
    SolveRuntime, apply_discrete_slot_value,
};
pub use runtime_events::{
    apply_discrete_slot_values, current_dynamic_time_event_stop, eval_event_actions_with_context,
    next_runtime_event_stop, visible_values_with_context,
};
pub use sparsity::{
    JacobianSparsity, jacobian_sparsity_from_jvp, jacobian_sparsity_from_scalar_jvp,
    row_seed_dependencies,
};
pub use table_runtime::{
    TableRuntimeError, eval_table_bound_value_in, eval_table_lookup_slope_value_in,
    eval_table_lookup_value_in, eval_time_table_next_event_value_in,
};
pub use update_rows::{
    UpdateRowApplication, apply_scalar_slot_value, apply_scalar_slot_values,
    eval_and_apply_update_rows,
};

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
    let mut sink = OutputCursor::with_output_indices(out, &block.output_indices);
    for (row_idx, row) in block.programs.iter().enumerate() {
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
}

impl<'out> OutputCursor<'out> {
    pub(crate) fn new(out: &'out mut [f64]) -> Self {
        Self {
            out,
            output_indices: None,
            cursor: 0,
        }
    }

    pub(crate) fn with_output_indices(out: &'out mut [f64], output_indices: &'out [usize]) -> Self {
        Self {
            out,
            output_indices: Some(output_indices),
            cursor: 0,
        }
    }

    fn store(&mut self, value: f64) -> Result<(), EvalSolveError> {
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
        }
    }

    pub(crate) fn with_source_span(mut self, span: Option<rumoca_core::Span>) -> Self {
        self.source_span = span;
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
    for part in &action.message.parts {
        match part {
            SolveEventMessagePart::Text(text) => message.push_str(text),
            SolveEventMessagePart::Number(row) => {
                message.push_str(&eval_event_message_number(row, y, p, t, context)?);
            }
        }
    }
    Ok(message)
}

fn eval_event_message_number(
    row: &[LinearOp],
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
) -> Result<String, EvalSolveError> {
    let value = eval_row_with_context(row, y, p, t, context)?;
    Ok(format!("{value}"))
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
/// target-assignment rows). Returns the last value stored, matching the
/// historical "last `StoreOutput` wins" behavior for those programs.
pub(crate) fn eval_program_single(
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

#[inline(always)]
pub(crate) fn eval_row_prepared_maybe_fast(
    input: PreparedRowEval<'_, '_>,
    register_safe: bool,
    scratch: &mut RowEvalScratch,
    sink: &mut OutputCursor<'_>,
) -> Result<(), EvalSolveError> {
    let start = solve_row_eval_trace_active().then(Instant::now);
    let result = if register_safe {
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
            self.eval_op(*op)?;
        }
        Ok(())
    }

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

fn eval_row_prepared_fast(
    input: PreparedRowEval<'_, '_>,
    scratch: &mut RowEvalScratch,
    sink: &mut OutputCursor<'_>,
) -> Result<(), EvalSolveError> {
    scratch.regs.resize(input.register_count, 0.0);
    let regs = &mut scratch.regs;
    for op in input.row {
        match *op {
            LinearOp::Const { dst, value } => regs[dst as usize] = value,
            LinearOp::LoadTime { dst } => regs[dst as usize] = input.t,
            LinearOp::LoadY { dst, index } => {
                regs[dst as usize] = input.y[index];
            }
            LinearOp::LoadP { dst, index } => {
                regs[dst as usize] = input.p[index];
            }
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => {
                let slot = resolve_indexed_slot(regs[index as usize], base, count);
                regs[dst as usize] = input.read_input("p", input.p, slot)?;
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
                    .ok_or_else(|| input.missing_input("seed", base, 0))?;
                let slot = resolve_indexed_slot(regs[index as usize], base, count);
                regs[dst as usize] = input.read_input("seed", seed, slot)?;
            }
            LinearOp::LoadSeed { dst, index } => {
                let seed = input
                    .context
                    .seed
                    .ok_or_else(|| input.missing_input("seed", index, 0))?;
                regs[dst as usize] = seed[index];
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
            LinearOp::Unary { dst, op, arg } => {
                regs[dst as usize] = eval_unary(op, regs[arg as usize]);
            }
            LinearOp::Binary { dst, op, lhs, rhs } => {
                regs[dst as usize] = eval_binary(op, regs[lhs as usize], regs[rhs as usize]);
            }
            LinearOp::Compare { dst, op, lhs, rhs } => {
                regs[dst as usize] = eval_compare(op, regs[lhs as usize], regs[rhs as usize]);
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                regs[dst as usize] = if regs[cond as usize] != 0.0 {
                    regs[if_true as usize]
                } else {
                    regs[if_false as usize]
                };
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
            LinearOp::StoreOutput { src } => sink.store(regs[src as usize])?,
        }
    }
    Ok(())
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
        LinearOp::LoadSeed { .. } => "LoadSeed",
        LinearOp::LoadIndexedSeed { .. } => "LoadIndexedSeed",
        LinearOp::Move { .. } => "Move",
        LinearOp::LinearSolveComponent { .. } => "LinearSolveComponent",
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
        LinearOp::StoreOutput { .. } => "StoreOutput",
    }
}

pub(crate) fn required_registers(row: &[LinearOp]) -> Result<usize, EvalSolveError> {
    row.iter()
        .map(max_register)
        .try_fold(None, |max_reg: Option<u32>, reg| {
            let reg = reg?;
            Ok::<Option<u32>, EvalSolveError>(Some(max_reg.map_or(reg, |current| current.max(reg))))
        })?
        .map_or(Ok(0), checked_required_register_count)
}

fn checked_required_register_count(reg: Reg) -> Result<usize, EvalSolveError> {
    usize::try_from(reg)
        .ok()
        .and_then(|reg| reg.checked_add(1))
        .ok_or_else(|| invalid_row(format!("register index {reg} overflows register count")))
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
        .programs
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
        .copied()
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
    block.validate_shape_contract("scalar program block eval")?;
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

pub(crate) fn validate_input_requirements(
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

fn max_register(op: &LinearOp) -> Result<u32, EvalSolveError> {
    match *op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. } => Ok(dst),
        LinearOp::LoadIndexedP { dst, index, .. }
        | LinearOp::LoadIndexedSeed { dst, index, .. } => Ok(dst.max(index)),
        LinearOp::Move { dst, src } | LinearOp::Unary { dst, arg: src, .. } => Ok(dst.max(src)),
        LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
            Ok(dst.max(lhs).max(rhs))
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => Ok(dst.max(cond).max(if_true).max(if_false)),
        LinearOp::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            ..
        } => Ok(dst
            .max(checked_reg_range_last(
                matrix_start,
                checked_square_len(n, "linear solve matrix")?,
                "linear solve matrix",
            )?)
            .max(checked_reg_range_last(rhs_start, n, "linear solve rhs")?)),
        LinearOp::TableBounds { dst, table_id, .. } => Ok(dst.max(table_id)),
        LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        }
        | LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => Ok(dst.max(table_id).max(column).max(input)),
        LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        } => Ok(dst.max(table_id).max(time)),
        LinearOp::RandomInitialState {
            dst,
            local_seed,
            global_seed,
            ..
        } => Ok(dst.max(local_seed).max(global_seed)),
        LinearOp::RandomResult {
            dst,
            state_start,
            state_len,
            ..
        }
        | LinearOp::RandomState {
            dst,
            state_start,
            state_len,
            ..
        } => Ok(dst.max(checked_reg_range_last(
            state_start,
            state_len,
            "random state",
        )?)),
        LinearOp::ImpureRandomInit { dst, seed } => Ok(dst.max(seed)),
        LinearOp::ImpureRandom { dst, id, .. } => Ok(dst.max(id)),
        LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            ..
        } => Ok(dst.max(id).max(imin).max(imax)),
        LinearOp::StoreOutput { src } => Ok(src),
    }
}

fn checked_square_len(n: usize, kind: &'static str) -> Result<usize, EvalSolveError> {
    n.checked_mul(n)
        .ok_or_else(|| invalid_row(format!("{kind} size overflow")))
}

fn checked_reg_range_last(
    start: Reg,
    len: usize,
    kind: &'static str,
) -> Result<Reg, EvalSolveError> {
    let Some(offset) = len.checked_sub(1) else {
        return Ok(start);
    };
    let offset = Reg::try_from(offset).map_err(|_| {
        invalid_row(format!(
            "{kind} offset {offset} exceeds register index type"
        ))
    })?;
    start
        .checked_add(offset)
        .ok_or_else(|| invalid_row(format!("{kind} range starting at {start} overflows")))
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

fn eval_solve_bool_values(
    len: usize,
    value: bool,
    context: &'static str,
) -> Result<Vec<bool>, EvalSolveError> {
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

pub(crate) fn row_register_flow_is_valid(row: &[LinearOp]) -> Result<bool, EvalSolveError> {
    let register_count = required_registers(row)?;
    let mut initialized = eval_solve_bool_values(register_count, false, "row register flow state")?;
    for op in row {
        if !op_sources_initialized(op, &initialized) {
            return Ok(false);
        }
        mark_op_dests_initialized(op, &mut initialized);
    }
    Ok(true)
}

fn op_sources_initialized(op: &LinearOp, initialized: &[bool]) -> bool {
    match *op {
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadY { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadSeed { .. } => true,
        LinearOp::Move { src, .. }
        | LinearOp::Unary { arg: src, .. }
        | LinearOp::LoadIndexedP { index: src, .. }
        | LinearOp::LoadIndexedSeed { index: src, .. }
        | LinearOp::StoreOutput { src } => reg_initialized(initialized, src),
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
            reg_initialized(initialized, lhs) && reg_initialized(initialized, rhs)
        }
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            reg_initialized(initialized, cond)
                && reg_initialized(initialized, if_true)
                && reg_initialized(initialized, if_false)
        }
        LinearOp::LinearSolveComponent {
            matrix_start,
            rhs_start,
            n,
            component,
            ..
        } => {
            let Some(matrix_len) = n.checked_mul(n) else {
                return false;
            };
            component < n
                && reg_range_initialized(initialized, matrix_start, matrix_len)
                && reg_range_initialized(initialized, rhs_start, n)
        }
        LinearOp::TableBounds { table_id, .. } => reg_initialized(initialized, table_id),
        LinearOp::TableLookup {
            table_id,
            column,
            input,
            ..
        }
        | LinearOp::TableLookupSlope {
            table_id,
            column,
            input,
            ..
        } => {
            reg_initialized(initialized, table_id)
                && reg_initialized(initialized, column)
                && reg_initialized(initialized, input)
        }
        LinearOp::TableNextEvent { table_id, time, .. } => {
            reg_initialized(initialized, table_id) && reg_initialized(initialized, time)
        }
        LinearOp::RandomInitialState {
            local_seed,
            global_seed,
            ..
        } => reg_initialized(initialized, local_seed) && reg_initialized(initialized, global_seed),
        LinearOp::RandomResult {
            state_start,
            state_len,
            ..
        }
        | LinearOp::RandomState {
            state_start,
            state_len,
            ..
        } => reg_range_initialized(initialized, state_start, state_len),
        LinearOp::ImpureRandomInit { seed, .. } => reg_initialized(initialized, seed),
        LinearOp::ImpureRandom { id, .. } => reg_initialized(initialized, id),
        LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
            reg_initialized(initialized, id)
                && reg_initialized(initialized, imin)
                && reg_initialized(initialized, imax)
        }
    }
}

fn mark_op_dests_initialized(op: &LinearOp, initialized: &mut [bool]) {
    if let Some(dst) = op.dst_register()
        && let Some(slot) = initialized.get_mut(dst as usize)
    {
        *slot = true;
    }
}

fn reg_initialized(initialized: &[bool], reg: Reg) -> bool {
    initialized.get(reg as usize).copied().unwrap_or(false)
}

fn reg_range_initialized(initialized: &[bool], start: Reg, len: usize) -> bool {
    let start = start as usize;
    start
        .checked_add(len)
        .is_some_and(|end| end <= initialized.len() && initialized[start..end].iter().all(|v| *v))
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

pub(crate) fn eval_unary(op: UnaryOp, value: f64) -> f64 {
    match op {
        UnaryOp::Neg => -value,
        UnaryOp::Not => (value == 0.0) as u8 as f64,
        UnaryOp::Abs => value.abs(),
        UnaryOp::Sign => value.signum(),
        UnaryOp::Sqrt => value.sqrt(),
        UnaryOp::Floor => value.floor(),
        UnaryOp::Ceil => value.ceil(),
        UnaryOp::Trunc => value.trunc(),
        UnaryOp::Sin => value.sin(),
        UnaryOp::Cos => value.cos(),
        UnaryOp::Tan => value.tan(),
        UnaryOp::Asin => value.asin(),
        UnaryOp::Acos => value.acos(),
        UnaryOp::Atan => value.atan(),
        UnaryOp::Sinh => value.sinh(),
        UnaryOp::Cosh => value.cosh(),
        UnaryOp::Tanh => value.tanh(),
        UnaryOp::Exp => value.exp(),
        UnaryOp::Log => value.ln(),
        UnaryOp::Log10 => value.log10(),
    }
}

pub(crate) fn eval_binary(op: BinaryOp, lhs: f64, rhs: f64) -> f64 {
    match op {
        BinaryOp::Add => lhs + rhs,
        BinaryOp::Sub => lhs - rhs,
        BinaryOp::Mul => lhs * rhs,
        BinaryOp::Div => guarded_division(lhs, rhs),
        BinaryOp::Pow => lhs.powf(rhs),
        BinaryOp::And => ((lhs != 0.0) && (rhs != 0.0)) as u8 as f64,
        BinaryOp::Or => ((lhs != 0.0) || (rhs != 0.0)) as u8 as f64,
        BinaryOp::Atan2 => lhs.atan2(rhs),
        BinaryOp::Min => lhs.min(rhs),
        BinaryOp::Max => lhs.max(rhs),
    }
}

fn guarded_division(lhs: f64, rhs: f64) -> f64 {
    if rhs == 0.0 {
        if lhs == 0.0 { 0.0 } else { f64::INFINITY }
    } else {
        lhs / rhs
    }
}

pub(crate) fn eval_compare(op: CompareOp, lhs: f64, rhs: f64) -> f64 {
    op.compare_as_f64(lhs, rhs)
}

#[cfg(test)]
mod tests;
