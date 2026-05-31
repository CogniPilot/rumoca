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
        atomic::{AtomicU64, Ordering},
    },
    time::Instant,
};

use rumoca_ir_solve::{
    BinaryOp, CompareOp, LinearOp, Reg, ScalarProgramBlock, SolveEventActionKind,
    SolveEventPartition, UnaryOp,
};

mod compute_block_scalarize;
pub mod eval_at;
pub mod jacobian;
mod linear_solve;
pub mod nan_trace;
mod prepared;
mod random_runtime;
mod runtime;
mod sparsity;
mod table_runtime;
mod update_rows;
pub use compute_block_scalarize::to_scalar_program_block;
pub use eval_at::{EvalAtReport, EvalAtSlot};
pub use jacobian::JacobianReport;
use linear_solve::{solve_component, solve_component_unchecked};
pub use prepared::{PreparedComputeBlock, PreparedScalarProgramBlock};
use random_runtime::{
    ImpureRandomState, impure_random_mutex, impure_random_sample, impure_random_stream_id,
    initial_state_values, projected_random_value, random_result_and_state, read_reg_range,
};
pub use runtime::{
    AlgebraicSettle, SolveRuntime, apply_discrete_slot_value, apply_discrete_slot_values,
    eval_event_actions_with_context, next_runtime_event_stop, visible_values_with_context,
};
pub use sparsity::{
    JacobianSparsity, jacobian_sparsity_from_jvp, jacobian_sparsity_from_scalar_jvp,
    row_seed_dependencies,
};
pub use table_runtime::{
    try_eval_table_bound_value_in, try_eval_table_lookup_slope_value_in,
    try_eval_table_lookup_value_in, try_eval_time_table_next_event_value_in,
};
pub use update_rows::{
    UpdateRowApplication, apply_scalar_slot_value, apply_scalar_slot_values,
    eval_and_apply_update_rows,
};

static ROW_EVAL_CALLS: AtomicU64 = AtomicU64::new(0);
static ROW_EVAL_NANOS: AtomicU64 = AtomicU64::new(0);

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
    },
    MissingInput {
        vector: &'static str,
        index: usize,
        len: usize,
    },
    RegisterOutOfBounds {
        access: &'static str,
        register: Reg,
        len: usize,
    },
    UninitializedRegister {
        register: Reg,
    },
    OutputTooSmall {
        required: usize,
        len: usize,
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
    },
    EventActionConditionMismatch {
        rows: usize,
        actions: usize,
    },
}

impl std::fmt::Display for EvalSolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExternalTable {
                operation,
                table_id,
                column,
            } => {
                if let Some(column) = column {
                    write!(
                        f,
                        "external table {operation} failed for table id {table_id} column {column}"
                    )
                } else {
                    write!(
                        f,
                        "external table {operation} failed for table id {table_id}"
                    )
                }
            }
            Self::MissingInput { vector, index, len } => write!(
                f,
                "missing {vector}[{index}] while evaluating Solve-IR row; vector length is {len}"
            ),
            Self::RegisterOutOfBounds {
                access,
                register,
                len,
            } => write!(
                f,
                "cannot {access} Solve-IR register r{register}; register file length is {len}"
            ),
            Self::UninitializedRegister { register } => {
                write!(f, "cannot read uninitialized Solve-IR register r{register}")
            }
            Self::OutputTooSmall { required, len } => write!(
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
            } => write!(
                f,
                "cannot isolate target y[{target_y_index}] from Solve-IR row {row}: singular coefficient {coefficient}"
            ),
            Self::EventActionConditionMismatch { rows, actions } => write!(
                f,
                "event action condition row count {rows} does not match event action count {actions}"
            ),
        }
    }
}

impl std::error::Error for EvalSolveError {}

pub fn reset_solve_row_eval_trace() {
    if !solve_row_eval_trace_enabled() {
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
    if !solve_row_eval_trace_enabled() {
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

fn solve_row_eval_trace_enabled() -> bool {
    tracing::enabled!(target: "rumoca_eval_solve::row", tracing::Level::DEBUG)
}

pub(crate) fn record_solve_block_eval(kind: &'static str, len: usize, rows: usize) {
    if !solve_row_eval_trace_enabled() {
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
    for (row_idx, row) in block.programs.iter().enumerate() {
        out[row_idx] = eval_row_prepared_with_context(
            PreparedRowEval::new(row, required_registers(row), y, p, t, context),
            &mut scratch,
        )?;
    }
    Ok(())
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
        }
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
    let mut values = vec![0.0; events.actions.len()];
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
                    message: action
                        .message
                        .clone()
                        .unwrap_or_else(|| "assertion failed".to_string()),
                });
            }
            SolveEventActionKind::Terminate => {
                return Ok(EventActionRequest::Terminate {
                    message: action
                        .message
                        .clone()
                        .unwrap_or_else(|| "simulation terminated".to_string()),
                });
            }
        }
    }
    Ok(EventActionRequest::Continue)
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
    eval_row_prepared_with_context(
        PreparedRowEval::new(row, required_registers(row), y, p, t, context),
        &mut scratch,
    )
}

pub(crate) fn eval_row_prepared_maybe_fast(
    input: PreparedRowEval<'_, '_>,
    register_safe: bool,
    scratch: &mut RowEvalScratch,
) -> Result<f64, EvalSolveError> {
    let start = solve_row_eval_trace_enabled().then(Instant::now);
    let result = if register_safe {
        eval_row_prepared_fast(input, scratch)
    } else {
        eval_row_prepared_with_context(input, scratch)
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
) -> Result<f64, EvalSolveError> {
    scratch.regs.resize(input.register_count, 0.0);
    scratch.initialized.resize(input.register_count, false);
    scratch.regs.fill(0.0);
    scratch.initialized.fill(false);
    CheckedRowEvaluator {
        regs: &mut scratch.regs,
        initialized: &mut scratch.initialized,
        input,
        output: 0.0,
    }
    .eval()
}

struct CheckedRowEvaluator<'scratch, 'row, 'ctx> {
    regs: &'scratch mut [f64],
    initialized: &'scratch mut [bool],
    input: PreparedRowEval<'row, 'ctx>,
    output: f64,
}

impl CheckedRowEvaluator<'_, '_, '_> {
    fn eval(mut self) -> Result<f64, EvalSolveError> {
        for op in self.input.row {
            self.eval_op(*op)?;
        }
        Ok(self.output)
    }

    fn eval_op(&mut self, op: LinearOp) -> Result<(), EvalSolveError> {
        match op {
            LinearOp::Const { dst, value } => {
                self.set(dst, value)?;
            }
            LinearOp::LoadTime { dst } => self.set(dst, self.input.t)?,
            LinearOp::LoadY { dst, index } => {
                self.set(dst, read_input("y", self.input.y, index)?)?;
            }
            LinearOp::LoadP { dst, index } => {
                self.set(dst, read_input("p", self.input.p, index)?)?;
            }
            LinearOp::LoadSeed { dst, index } => {
                let seed = self
                    .input
                    .context
                    .seed
                    .ok_or(EvalSolveError::MissingInput {
                        vector: "seed",
                        index,
                        len: 0,
                    })?;
                self.set(dst, read_input("seed", seed, index)?)?;
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
                let value = solve_component(
                    self.regs,
                    self.initialized,
                    matrix_start,
                    rhs_start,
                    n,
                    component,
                )?;
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
                apply_table_op(self.regs, self.initialized, &op, self.input.context)?;
            }
            LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. } => {
                apply_random_op(
                    self.regs,
                    self.initialized,
                    &op,
                    self.input.t,
                    self.input.context,
                )?;
            }
            LinearOp::StoreOutput { src } => {
                self.output = self.get(src)?;
            }
        }
        Ok(())
    }

    fn get(&self, reg: Reg) -> Result<f64, EvalSolveError> {
        get(self.regs, self.initialized, reg)
    }

    fn set(&mut self, reg: Reg, value: f64) -> Result<(), EvalSolveError> {
        set(self.regs, self.initialized, reg, value)
    }
}

fn eval_row_prepared_fast(
    input: PreparedRowEval<'_, '_>,
    scratch: &mut RowEvalScratch,
) -> Result<f64, EvalSolveError> {
    scratch.regs.resize(input.register_count, 0.0);
    let regs = &mut scratch.regs;
    let mut output = 0.0;
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
            LinearOp::LoadSeed { dst, index } => {
                let seed = input.context.seed.ok_or(EvalSolveError::MissingInput {
                    vector: "seed",
                    index,
                    len: 0,
                })?;
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
                    solve_component_unchecked(regs, matrix_start, rhs_start, n, component);
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
                scratch.initialized.resize(input.register_count, true);
                apply_table_op(regs, &mut scratch.initialized, op, input.context)?
            }
            LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. } => {
                scratch.initialized.resize(input.register_count, true);
                apply_random_op(regs, &mut scratch.initialized, op, input.t, input.context)?
            }
            LinearOp::StoreOutput { src } => output = regs[src as usize],
        }
    }
    Ok(output)
}

fn apply_table_op(
    regs: &mut [f64],
    initialized: &mut [bool],
    op: &LinearOp,
    context: RowEvalContext<'_>,
) -> Result<(), EvalSolveError> {
    match *op {
        LinearOp::TableBounds { dst, table_id, max } => {
            let table_id = get(regs, initialized, table_id)?;
            let tables = context.external_tables.unwrap_or(&[]);
            let value = try_eval_table_bound_value_in(table_id, max, tables).ok_or(
                EvalSolveError::ExternalTable {
                    operation: if max { "bounds max" } else { "bounds min" },
                    table_id,
                    column: None,
                },
            )?;
            set(regs, initialized, dst, value)?;
        }
        LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        } => {
            let table_id = get(regs, initialized, table_id)?;
            let column = get(regs, initialized, column)?;
            let input = get(regs, initialized, input)?;
            let tables = context.external_tables.unwrap_or(&[]);
            let value = try_eval_table_lookup_value_in(table_id, column, input, tables).ok_or(
                EvalSolveError::ExternalTable {
                    operation: "lookup",
                    table_id,
                    column: Some(column),
                },
            )?;
            set(regs, initialized, dst, value)?;
        }
        LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => {
            let table_id = get(regs, initialized, table_id)?;
            let column = get(regs, initialized, column)?;
            let input = get(regs, initialized, input)?;
            let tables = context.external_tables.unwrap_or(&[]);
            let value = try_eval_table_lookup_slope_value_in(table_id, column, input, tables)
                .ok_or(EvalSolveError::ExternalTable {
                    operation: "lookup slope",
                    table_id,
                    column: Some(column),
                })?;
            set(regs, initialized, dst, value)?;
        }
        LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        } => {
            let table_id = get(regs, initialized, table_id)?;
            let time = get(regs, initialized, time)?;
            let tables = context.external_tables.unwrap_or(&[]);
            let value = try_eval_time_table_next_event_value_in(table_id, time, tables).ok_or(
                EvalSolveError::ExternalTable {
                    operation: "next event",
                    table_id,
                    column: None,
                },
            )?;
            set(regs, initialized, dst, value)?;
        }
        _ => unreachable!("table opcode helper called with non-table opcode"),
    }
    Ok(())
}

fn apply_random_op(
    regs: &mut [f64],
    initialized: &mut [bool],
    op: &LinearOp,
    t: f64,
    context: RowEvalContext<'_>,
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
                get(regs, initialized, local_seed)?.round() as i64,
                get(regs, initialized, global_seed)?.round() as i64,
                state_len,
            );
            set(
                regs,
                initialized,
                dst,
                projected_random_value(&values, state_index),
            )?;
        }
        LinearOp::RandomResult {
            dst,
            generator,
            state_start,
            state_len,
        } => {
            let state = read_reg_range(regs, initialized, state_start, state_len)?;
            let (result, _) = random_result_and_state(generator, &state);
            set(regs, initialized, dst, result)?;
        }
        LinearOp::RandomState {
            dst,
            generator,
            state_start,
            state_len,
            state_index,
        } => {
            let state = read_reg_range(regs, initialized, state_start, state_len)?;
            let (_, next_state) = random_result_and_state(generator, &state);
            set(
                regs,
                initialized,
                dst,
                projected_random_value(&next_state, state_index),
            )?;
        }
        LinearOp::ImpureRandomInit { dst, seed } => {
            let stream_id = impure_random_stream_id(get(regs, initialized, seed)?.round() as i64);
            set(regs, initialized, dst, stream_id as f64)?;
        }
        LinearOp::ImpureRandom { dst, id, call_site } => {
            let id = get(regs, initialized, id)?.round() as i64;
            set(
                regs,
                initialized,
                dst,
                impure_random_sample(id, call_site, t, impure_random_mutex(context)),
            )?;
        }
        LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            call_site,
        } => {
            let id = get(regs, initialized, id)?.round() as i64;
            let imin = get(regs, initialized, imin)?.round() as i64;
            let imax = get(regs, initialized, imax)?.round() as i64;
            let (lo, hi) = if imin <= imax {
                (imin, imax)
            } else {
                (imax, imin)
            };
            let span = (hi - lo + 1).max(1) as f64;
            let y = lo as f64
                + (impure_random_sample(id, call_site, t, impure_random_mutex(context)) * span)
                    .floor();
            set(regs, initialized, dst, y.clamp(lo as f64, hi as f64))?;
        }
        _ => unreachable!("random opcode helper called with non-random opcode"),
    }
    Ok(())
}

pub(crate) fn required_registers(row: &[LinearOp]) -> usize {
    row.iter()
        .map(max_register)
        .max()
        .map_or(0, |reg| reg as usize + 1)
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

pub fn scalar_program_block_input_requirements(block: &ScalarProgramBlock) -> RowInputRequirements {
    block
        .programs
        .iter()
        .map(|row| row_input_requirements(row))
        .fold(RowInputRequirements::default(), RowInputRequirements::merge)
}

pub fn row_input_requirements(row: &[LinearOp]) -> RowInputRequirements {
    row.iter()
        .copied()
        .map(input_requirements_for_op)
        .fold(RowInputRequirements::default(), RowInputRequirements::merge)
}

fn input_requirements_for_op(op: LinearOp) -> RowInputRequirements {
    match op {
        LinearOp::LoadY { index, .. } => RowInputRequirements {
            y_len: index.saturating_add(1),
            ..Default::default()
        },
        LinearOp::LoadP { index, .. } => RowInputRequirements {
            p_len: index.saturating_add(1),
            ..Default::default()
        },
        LinearOp::LoadSeed { index, .. } => RowInputRequirements {
            seed_len: index.saturating_add(1),
            ..Default::default()
        },
        _ => RowInputRequirements::default(),
    }
}

pub fn validate_scalar_program_block_io(
    block: &ScalarProgramBlock,
    y: &[f64],
    p: &[f64],
    seed: Option<&[f64]>,
    out: &[f64],
) -> Result<(), EvalSolveError> {
    validate_output_len(out, block.programs.len())?;
    validate_input_requirements(scalar_program_block_input_requirements(block), y, p, seed)
}

pub fn validate_row_inputs(
    row: &[LinearOp],
    y: &[f64],
    p: &[f64],
    seed: Option<&[f64]>,
) -> Result<(), EvalSolveError> {
    validate_input_requirements(row_input_requirements(row), y, p, seed)
}

pub(crate) fn validate_output_len(out: &[f64], required: usize) -> Result<(), EvalSolveError> {
    if out.len() >= required {
        return Ok(());
    }
    Err(EvalSolveError::OutputTooSmall {
        required,
        len: out.len(),
    })
}

pub(crate) fn validate_input_requirements(
    requirements: RowInputRequirements,
    y: &[f64],
    p: &[f64],
    seed: Option<&[f64]>,
) -> Result<(), EvalSolveError> {
    validate_input_len("y", y.len(), requirements.y_len)?;
    validate_input_len("p", p.len(), requirements.p_len)?;
    if requirements.seed_len == 0 {
        return Ok(());
    }
    validate_input_len("seed", seed.map_or(0, <[f64]>::len), requirements.seed_len)
}

fn validate_input_len(
    vector: &'static str,
    actual_len: usize,
    required_len: usize,
) -> Result<(), EvalSolveError> {
    if actual_len >= required_len {
        return Ok(());
    }
    Err(EvalSolveError::MissingInput {
        vector,
        index: required_len.saturating_sub(1),
        len: actual_len,
    })
}

fn max_register(op: &LinearOp) -> u32 {
    match *op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. }
        | LinearOp::Move { dst, .. }
        | LinearOp::Unary { dst, .. }
        | LinearOp::Binary { dst, .. }
        | LinearOp::Compare { dst, .. }
        | LinearOp::Select { dst, .. }
        | LinearOp::LinearSolveComponent { dst, .. }
        | LinearOp::TableBounds { dst, .. }
        | LinearOp::TableLookup { dst, .. }
        | LinearOp::TableLookupSlope { dst, .. }
        | LinearOp::TableNextEvent { dst, .. } => dst,
        LinearOp::RandomInitialState {
            dst,
            local_seed,
            global_seed,
            ..
        } => dst.max(local_seed).max(global_seed),
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
        } => dst.max(state_start.saturating_add(state_len.saturating_sub(1) as u32)),
        LinearOp::ImpureRandomInit { dst, seed } => dst.max(seed),
        LinearOp::ImpureRandom { dst, id, .. } => dst.max(id),
        LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            ..
        } => dst.max(id).max(imin).max(imax),
        LinearOp::StoreOutput { src } => src,
    }
}

pub(crate) fn row_register_flow_is_valid(row: &[LinearOp]) -> bool {
    let register_count = required_registers(row);
    let mut initialized = vec![false; register_count];
    for op in row {
        if !op_sources_initialized(op, &initialized) {
            return false;
        }
        mark_op_dests_initialized(op, &mut initialized);
    }
    true
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
            component < n
                && reg_range_initialized(initialized, matrix_start, n.saturating_mul(n))
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

fn read_input(vector: &'static str, values: &[f64], index: usize) -> Result<f64, EvalSolveError> {
    values
        .get(index)
        .copied()
        .ok_or(EvalSolveError::MissingInput {
            vector,
            index,
            len: values.len(),
        })
}

fn set(
    regs: &mut [f64],
    initialized: &mut [bool],
    reg: Reg,
    value: f64,
) -> Result<(), EvalSolveError> {
    let index = reg as usize;
    let len = regs.len();
    let slot = regs
        .get_mut(index)
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "write",
            register: reg,
            len,
        })?;
    *slot = value;
    if let Some(init) = initialized.get_mut(index) {
        *init = true;
    }
    Ok(())
}

fn get(regs: &[f64], initialized: &[bool], reg: Reg) -> Result<f64, EvalSolveError> {
    let index = reg as usize;
    let value = regs
        .get(index)
        .copied()
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: reg,
            len: regs.len(),
        })?;
    if !initialized.get(index).copied().unwrap_or(false) {
        return Err(EvalSolveError::UninitializedRegister { register: reg });
    }
    Ok(value)
}

fn eval_unary(op: UnaryOp, value: f64) -> f64 {
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

fn eval_binary(op: BinaryOp, lhs: f64, rhs: f64) -> f64 {
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

fn eval_compare(op: CompareOp, lhs: f64, rhs: f64) -> f64 {
    op.compare_as_f64(lhs, rhs)
}

#[cfg(test)]
mod tests;
