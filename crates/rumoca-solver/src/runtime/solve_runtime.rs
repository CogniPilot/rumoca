//! Runtime orchestration for projection, events, and visible outputs.

use indexmap::IndexMap;
use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;
use rustc_hash::FxHashMap;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use crate::runtime::delay::{DelayRuntime, DelayRuntimeSnapshot};
use crate::runtime::pre_params::{
    advance_event_iteration_pre_params, event_iteration_plan_settled, seed_event_entry_pre_params,
};
use crate::runtime::projection::{
    ImplicitProjectionModel, ManifoldProjectionModel, project_algebraic_seed_with_plan,
    project_algebraics_with_plan, project_algebraics_with_plan_certified,
};
use crate::runtime::solve_events::{
    current_dynamic_time_event_stop, event_action_params, next_runtime_event_stop,
    visible_values_with_context,
};
use crate::runtime::solve_ops::write_clock_activation_params;
use crate::{
    EventActionOutcome, RuntimeEventStop, RuntimeSolveError, SolveStopSchedule,
    push_visible_values, relation_memory_value_from_root, replace_last_visible_values,
    timeline::sample_time_match_with_tol,
};
use rumoca_eval_solve::refresh_plan::trace_refresh_plan;
use rumoca_eval_solve::{
    EvalSolveError, PreparedComputeBlock, PreparedEventTransactionProgram,
    PreparedGuardedAssignmentProgram, PreparedScalarProgramBlock, RowEvalContext,
    to_scalar_program_block,
};

mod coupled_event;
mod discrete_rows;
mod event_transactions;
pub use event_transactions::EventTransactionExecution;
use event_transactions::PreparedEventTransactionCoverage;
mod event_update;
mod guarded_assignments;
mod initial_continuation;
mod initial_event;
mod initial_projection;
mod native_specialization;
mod plans;
mod refresh_batch;
mod refresh_execution;
mod refresh_projection;
mod relation_memory;
mod sensitivity;
mod support;
use discrete_rows::PreparedStructuredDiscreteRows;
pub use discrete_rows::SeededConditionMemory;
#[cfg(test)]
pub(crate) use discrete_rows::{
    ConditionMemorySeedInput, seed_condition_memory_for_initialization_core,
};
use event_update::{DiscretePreSnapshot, DiscreteRowsSettleInput};
pub use event_update::{EventUpdateRowFilter, ProjectedEventUpdateInput};
use initial_continuation::InitialContinuationCoverage;
pub use initial_event::{
    InitialEventObservation, ProjectedInitialEventInput, ProjectedInitialEventOutcome,
    ProjectedPostInitialEventInput,
};
use plans::{
    RootConditionPlan, RootConditionPlanEntry, VisibleValuePlan, VisibleValuePlanEntry,
    copy_grouped_expression_values, direct_time_root_search_default, direct_time_root_time,
    direct_time_root_value, direct_visible_value, prepare_manifold_projection_programs,
    root_condition_plan, total_root_condition_count, visible_value_plan,
};
use refresh_execution::static_refresh_parameter_indices;
use refresh_projection::*;
use support::{
    build_visible_name_index, copy_runtime_values, copy_runtime_values_into,
    fill_inactive_root_output, optional_compiled, reserve_runtime_index_map_capacity,
    reserve_runtime_vec_capacity, resize_runtime_values, validate_finite_runtime_output,
    validate_runtime_output_len, visible_value_index_error, zero_runtime_values,
};

/// Backend-neutral callable produced from one checked Solve-IR expression
/// block. Native execution adapters implement this contract; the runtime
/// retains the prepared evaluator as the correctness fallback.
pub trait CompiledSolveExpression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String>;
}

/// Backend-neutral callable for a checked forward-mode Solve-IR expression.
pub trait CompiledSolveJacobianExpression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String>;
}

/// Backend-neutral callable for a causally ordered set of exact assignments.
/// Program outputs write directly to the flattened solver-Y target list, so
/// subsequent programs observe earlier assignments while multiple outputs of
/// one source program commit together.
pub trait CompiledSolveAssignmentSchedule {
    fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<(), String>;
}

/// Backend-neutral callable for one checked aggregate event transaction.
pub trait CompiledSolveEventTransaction {
    fn call(&self, input: &[f64], output: &mut [f64]) -> Result<(), String>;
}

/// Optional execution adapter injected by a concrete simulation backend.
pub trait SolveExecutionBackend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String>;

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String>;

    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String>;

    fn compile_event_transaction(
        &self,
        program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String>;
}

#[derive(Clone)]
struct CompiledDiscreteSpecialization {
    expression: Rc<dyn CompiledSolveExpression>,
    output_count: usize,
    guard_expectations: Box<[bool]>,
}

#[derive(Clone, Default)]
struct ClockActivationCache {
    time_bits: Option<u64>,
    active: Vec<bool>,
}

impl From<solve_eval::EvalSolveError> for RuntimeSolveError {
    fn from(value: solve_eval::EvalSolveError) -> Self {
        match value {
            EvalSolveError::SingularTargetAssignment {
                row,
                target_y_index,
                coefficient,
                span,
            } => Self::RefreshTargetSingular {
                row,
                target_y_index,
                coefficient,
                span,
            },
            error => Self::solve_ir_with_span(error.to_string(), error.source_span()),
        }
    }
}

fn set_initial_event_flag(model: &solve::SolveModel, p: &mut [f64], value: bool) {
    let Some(index) = model.problem.solve_layout.initial_event_parameter_index else {
        return;
    };
    if let Some(slot) = p.get_mut(index) {
        *slot = f64::from(value);
    }
}

fn validate_discrete_event_rows(model: &solve::SolveModel) -> Result<(), RuntimeSolveError> {
    let rows = model.problem.discrete.rhs.len();
    let targets = model.problem.discrete.update_targets.len();
    let roles = model.problem.discrete.row_roles.len();
    let pre_modes = model.problem.discrete.pre_modes.len();
    let observation = model.problem.discrete.observation_refresh.len();
    let history_effects = model.problem.discrete.integrator_history_effects.len();
    let clock_owners = model.problem.discrete.clock_owners.len();
    if rows == targets
        && rows == roles
        && rows == pre_modes
        && rows == observation
        && rows == history_effects
        && rows == clock_owners
    {
        // SOLVE-C57: the issued clock-partition order must cover every
        // clock-owned producer outside an event transaction exactly once.
        // Execution replays the issued order; nothing is discovered here.
        return model
            .problem
            .discrete
            .validate_clock_partition_order()
            .map_err(|error| RuntimeSolveError::solve_ir(error.to_string()));
    }
    Err(RuntimeSolveError::solve_ir(format!(
        "discrete row columns differ: rhs={rows}, targets={targets}, roles={roles}, \
         pre_modes={pre_modes}, observation_refresh={observation}, \
         integrator_history_effects={history_effects}, clock_owners={clock_owners}"
    )))
}

#[derive(Clone)]
pub struct SolveRuntime {
    pub model: solve::SolveModel,
    pub state_count: usize,
    pub solver_count: usize,
    implicit_rhs: PreparedComputeBlock,
    implicit_projection_jacobian_v: PreparedComputeBlock,
    implicit_projection_scalar_jacobian_v: PreparedScalarProgramBlock,
    implicit_scalar_rhs: PreparedScalarProgramBlock,
    refresh_program_rows: FxHashMap<solve::RefreshScalarProgramSource, usize>,
    manifold_residual: PreparedComputeBlock,
    manifold_jacobian_v: PreparedComputeBlock,
    initial_residual: PreparedComputeBlock,
    initial_residual_jacobian_v: PreparedComputeBlock,
    initial_scalar_residual: PreparedScalarProgramBlock,
    compiled_implicit_rhs: Option<Rc<dyn CompiledSolveExpression>>,
    compiled_implicit_projection_jacobian_v: Option<Rc<dyn CompiledSolveJacobianExpression>>,
    compiled_implicit_full_jacobian_v: Option<Rc<dyn CompiledSolveJacobianExpression>>,
    compiled_initial_residual: Option<Rc<dyn CompiledSolveExpression>>,
    compiled_initial_residual_jacobian_v: Option<Rc<dyn CompiledSolveJacobianExpression>>,
    derivative_rhs: PreparedComputeBlock,
    compiled_derivative_rhs: Option<Rc<dyn CompiledSolveExpression>>,
    /// Forward-mode AD Jacobian-vector product of `derivative_rhs`
    /// (`d(der)/d(y)·v`), lowered to `LinearOp`s with `LoadSeed`. Applied — with a
    /// seed completed by `seed_refresh_derivative_dependencies` — to form
    /// the exact state Jacobian for the state-only BDF path.
    derivative_jacobian_v: PreparedScalarProgramBlock,
    /// Primal state-derivative scalar program `der = f(solver_y, p, t)`. Reversed
    /// by [`Self::reverse_state_derivative_vjp`] to form the reverse-mode VJP
    /// `(∂der/∂[solver_y|p])ᵀ·λ` (Track A scalar reverse core).
    derivative_scalar: PreparedScalarProgramBlock,
    /// Per-row forward-mode AD Jacobian-vector product of `implicit_rhs`
    /// (`d(residual_row)/d[y|p]·v`). Used to propagate state and parameter seeds
    /// through the algebraic projection row by row.
    implicit_jacobian_v: PreparedScalarProgramBlock,
    continuous_structural: solve::ContinuousStructuralArtifacts,
    initialization_structural: solve::InitializationStructuralArtifacts,
    algebraic_newton_caches: Vec<RefCell<crate::runtime::projection::SparseNewtonCache>>,
    algebraic_refresh: solve::RefreshPlan,
    derivative_refresh: solve::RefreshPlan,
    root_refresh: solve::RefreshPlan,
    event_refresh: solve::RefreshPlan,
    root_refresh_after_derivative: Option<solve::RefreshRemainderRelation>,
    clock_event_refresh_after_event: Vec<solve::RefreshRemainderRelation>,
    /// Certified coverage for the initialization homotopy continuation; the
    /// single source of truth shared by the sweep driver and the acceptance
    /// check in [`InitialContinuationCoverage::certify`].
    initial_continuation: Option<InitialContinuationCoverage>,
    root_condition_rows: PreparedScalarProgramBlock,
    compiled_root_conditions: Option<Rc<dyn CompiledSolveExpression>>,
    event_action_conditions: PreparedScalarProgramBlock,
    event_action_active_row_indices: RefCell<Vec<usize>>,
    root_condition_plan: Option<RootConditionPlan>,
    discrete_rhs: PreparedScalarProgramBlock,
    /// Compiler-selected scalar rows for public observation, indexed once.
    observation_refresh_scalar_rows: Box<[usize]>,
    observation_refresh_p_scratch: RefCell<Vec<f64>>,
    observation_refresh_values_scratch: RefCell<Vec<(solve::ScalarSlot, f64)>>,
    /// SOLVE-C57 intermediate-definition refresh rows (private work state only).
    clock_partition_intermediates: PreparedScalarProgramBlock,
    /// The periodic clocks the issued SOLVE-C57 order carries, collected once.
    clock_partition_clocks: Vec<solve::PeriodicClockId>,
    /// Structured rows of each structured update, indexed once (the ordered
    /// pass would otherwise be quadratic in the partition size).
    clock_partition_structured_rows: Vec<Vec<usize>>,
    /// Reused private work state for the issued SOLVE-C57 pass.
    clock_partition_work_y: RefCell<Vec<f64>>,
    clock_partition_work_p: RefCell<Vec<f64>>,
    guarded_assignment_programs: Vec<PreparedGuardedAssignmentProgram>,
    event_transaction_programs: Vec<PreparedEventTransactionProgram>,
    event_transaction_coverage: PreparedEventTransactionCoverage,
    runtime_assignment_rhs: PreparedScalarProgramBlock,
    post_commit_assignment_rhs: PreparedScalarProgramBlock,
    update_values_scratch: RefCell<Vec<f64>>,
    structured_discrete_rows: PreparedStructuredDiscreteRows,
    visible_name_index: FxHashMap<String, usize>,
    visible_value_rows: PreparedScalarProgramBlock,
    visible_value_plan: Option<VisibleValuePlan>,
    visible_scratch: RefCell<Vec<f64>>,
    refresh_snapshot_scratch: RefCell<Vec<f64>>,
    refresh_probe_scratch: RefCell<Vec<f64>>,
    refresh_tensor_scratch: RefCell<Vec<f64>>,
    static_refresh_cache: RefCell<StaticRefreshCache>,
    static_refresh_parameter_indices: Box<[usize]>,
    parameter_static_gradient_cache: RefCell<ParameterStaticGradientCache>,
    runtime_state: solve_eval::SimulationRuntimeState,
    delay_runtime: DelayRuntime,
    root_condition_count: usize,
    derivative_scratch: RefCell<StateDerivativeScratch>,
    root_scratch: RefCell<Vec<f64>>,
    /// Reusable register tape / adjoint buffers for the reverse-mode VJP sweep,
    /// kept across calls so a hot reverse loop stays allocation-free.
    reverse_scratch: RefCell<solve_eval::reverse::ReverseScratch>,
    execution_backend: Option<Rc<dyn SolveExecutionBackend>>,
    compiled_discrete_rows: RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_discrete_rows: RefCell<BTreeSet<usize>>,
    /// Native specializations of the SOLVE-C57 intermediate refresh rows. They
    /// run once per tick per row on the deadline path, so they take the same
    /// compiled route the discrete rows do rather than staying interpreted.
    compiled_clock_partition_intermediates:
        RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_clock_partition_intermediates: RefCell<BTreeSet<usize>>,
    clock_partition_intermediate_scratch: RefCell<Vec<f64>>,
    compiled_guarded_assignments: RefCell<FxHashMap<usize, Rc<dyn CompiledSolveExpression>>>,
    failed_guarded_assignments: RefCell<BTreeSet<usize>>,
    compiled_event_transactions: Vec<Option<Rc<dyn CompiledSolveEventTransaction>>>,
    event_transaction_input_scratch: RefCell<Vec<f64>>,
    event_transaction_output_scratch: RefCell<Vec<Vec<f64>>>,
    compiled_root_rows: RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_root_rows: RefCell<BTreeSet<usize>>,
    compiled_visible_rows: RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_visible_rows: RefCell<BTreeSet<usize>>,
    compiled_event_action_rows: RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_event_action_rows: RefCell<BTreeSet<usize>>,
    compiled_assignment_schedules: RefCell<
        FxHashMap<solve::RefreshSequenceId, Option<Rc<dyn CompiledSolveAssignmentSchedule>>>,
    >,
    compiled_output_scratch: RefCell<Vec<f64>>,
    clock_activation_cache: RefCell<ClockActivationCache>,
}

/// Opaque mutable state shared by the evaluators behind one ME component.
#[derive(Clone)]
pub(crate) struct SolveRuntimeSnapshot {
    evaluator: solve_eval::SimulationRuntimeStateSnapshot,
    delay: DelayRuntimeSnapshot,
}

impl SolveRuntime {
    pub fn new(model: &solve::SolveModel) -> Result<Self, EvalSolveError> {
        Self::new_with_execution_backend(model, None)
    }

    #[cfg(test)]
    pub(crate) fn new_fixture(model: &solve::SolveModel) -> Result<Self, EvalSolveError> {
        let mut model = model.clone();
        model.problem.continuous.refresh_owners =
            solve_eval::refresh_plan::build_continuous_refresh_owners(&model.problem)?;
        Self::new(&model)
    }

    // SPEC_0021: Exception - this top-level runtime entry point binds the
    // complete construction-issued Solve owner inventory into the flat fields
    // used by the hot path. Keeping the one-to-one binding visible makes
    // coverage review safer than distributing it across partial builders.
    // SPEC_0021: Exception - cohesive exhaustive flow stays contiguous so ordering remains auditable.
    #[allow(clippy::too_many_lines)]
    pub fn new_with_execution_backend(
        model: &solve::SolveModel,
        execution_backend: Option<Rc<dyn SolveExecutionBackend>>,
    ) -> Result<Self, EvalSolveError> {
        if !model.problem.continuous.refresh_owners.is_issued() {
            return Err(EvalSolveError::InvalidRow {
                message: "Solve model has no construction-issued continuous refresh owners"
                    .to_string(),
                span: None,
            });
        }
        let continuous_structural = model.artifacts.continuous.structural.clone();
        let initialization_structural = model.artifacts.initialization.structural.clone();
        let algebraic_newton_caches = (0..continuous_structural.algebraic_projection().len())
            .map(|_| RefCell::new(crate::runtime::projection::SparseNewtonCache::default()))
            .collect();
        let implicit_scalar_projection =
            solve_eval::to_scalar_program_projection(&model.problem.continuous.implicit_rhs)?;
        let mut refresh_program_rows = FxHashMap::default();
        for (row, source) in implicit_scalar_projection
            .sources()
            .iter()
            .copied()
            .enumerate()
        {
            let Some(source) = source else {
                continue;
            };
            if refresh_program_rows.insert(source, row).is_some() {
                return Err(EvalSolveError::InvalidRow {
                    message: "canonical refresh scalar-program identity is duplicated".to_string(),
                    span: implicit_scalar_projection.block().program_span(row),
                });
            }
        }
        let implicit_scalar_programs = implicit_scalar_projection.into_block();
        let compiled_implicit_rhs = execution_backend.as_ref().and_then(|backend| {
            optional_compiled(
                "implicit_rhs",
                backend.compile_expression(&implicit_scalar_programs),
            )
        });
        let implicit_projection_scalar_jacobian =
            to_scalar_program_block(&model.artifacts.continuous.implicit_jacobian_v)?;
        let compiled_implicit_projection_jacobian_v =
            execution_backend.as_ref().and_then(|backend| {
                optional_compiled(
                    "implicit_projection_jacobian_v",
                    backend.compile_jacobian_expression(&implicit_projection_scalar_jacobian),
                )
            });
        let implicit_full_jacobian_v = model
            .artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .clone();
        let compiled_implicit_full_jacobian_v = execution_backend.as_ref().and_then(|backend| {
            optional_compiled(
                "implicit_full_jacobian_v",
                backend.compile_jacobian_expression(&implicit_full_jacobian_v),
            )
        });
        let implicit_scalar_rhs = PreparedScalarProgramBlock::new(implicit_scalar_programs)?;
        let (manifold_residual, manifold_jacobian_v) = prepare_manifold_projection_programs(model)?;
        let derivative_scalar_rhs =
            to_scalar_program_block(&model.problem.continuous.derivative_rhs)?;
        // Scalarization is an evaluator-boundary view of the compact
        // structured map.  Build it once and share it between dependency
        // planning and the prepared runtime adapter.
        let structured_discrete_scalar_rhs =
            to_scalar_program_block(&model.problem.discrete.structured_rhs)?;
        let guarded_assignment_programs = model
            .problem
            .discrete
            .guarded_assignments
            .iter()
            .map(PreparedGuardedAssignmentProgram::new)
            .collect::<Result<Vec<_>, _>>()?;
        let event_transaction_programs = model
            .problem
            .discrete
            .event_transactions
            .iter()
            .map(|program| PreparedEventTransactionProgram::new(program, &model.pure_calls))
            .collect::<Result<Vec<_>, _>>()?;
        let event_transaction_output_scratch = event_transaction_programs
            .iter()
            .map(|program| vec![0.0; program.output_scalar_count()])
            .collect();
        let event_transaction_coverage = PreparedEventTransactionCoverage::new(model);
        let observation_refresh_scalar_rows = model
            .problem
            .discrete
            .observation_refresh
            .iter()
            .enumerate()
            .filter_map(|(row, selected)| selected.then_some(row))
            .collect::<Box<[_]>>();
        let compiled_event_transactions = event_transaction_programs
            .iter()
            .map(|prepared| {
                execution_backend.as_ref().and_then(|backend| {
                    optional_compiled(
                        "event_transaction",
                        backend.compile_event_transaction(prepared.program()),
                    )
                })
            })
            .collect();
        let compiled_derivative_rhs = execution_backend.as_ref().and_then(|backend| {
            optional_compiled(
                "derivative_rhs",
                backend.compile_expression(&derivative_scalar_rhs),
            )
        });
        let refresh_owners = &model.problem.continuous.refresh_owners;
        let algebraic_refresh = refresh_owners.algebraic().clone();
        let derivative_refresh = refresh_owners.derivative().clone();
        let root_refresh = refresh_owners.root().clone();
        let event_refresh = refresh_owners.event().clone();
        let clock_event_refresh = refresh_owners.clock_events().to_vec();
        for row in [
            &algebraic_refresh,
            &derivative_refresh,
            &root_refresh,
            &event_refresh,
        ]
        .into_iter()
        .flat_map(|plan| &plan.rows)
        .chain(clock_event_refresh.iter().flat_map(|plan| &plan.rows))
        {
            if !refresh_program_rows.contains_key(&row.source()) {
                return Err(EvalSolveError::InvalidRow {
                    message: "refresh row source does not replay its final scalar projection"
                        .to_string(),
                    span: None,
                });
            }
        }
        trace_refresh_plan(model, "algebraic", &algebraic_refresh);
        trace_refresh_plan(model, "derivative", &derivative_refresh);
        trace_refresh_plan(model, "root", &root_refresh);
        trace_refresh_plan(model, "event", &event_refresh);
        for (clock, plan) in clock_event_refresh.iter().enumerate() {
            trace_refresh_plan(model, &format!("clock-event-{clock}"), plan);
        }
        let mut refresh_plans = vec![
            &algebraic_refresh,
            &derivative_refresh,
            &root_refresh,
            &event_refresh,
        ];
        refresh_plans.extend(clock_event_refresh.iter());
        let static_refresh_parameter_indices = static_refresh_parameter_indices(
            &implicit_scalar_rhs,
            refresh_plans,
            &refresh_program_rows,
        );
        let root_refresh_after_derivative = refresh_owners.root_after_derivative().cloned();
        let clock_event_refresh_after_event = refresh_owners.clock_events_after_event().to_vec();
        if clock_event_refresh_after_event.len() != clock_event_refresh.len() {
            return Err(EvalSolveError::InvalidRow {
                message: "clock refresh remainder inventory does not match clock owners"
                    .to_string(),
                span: None,
            });
        }
        trace_reverse_projection_coverage(model, &implicit_scalar_rhs);
        let visible_value_plan = visible_value_plan(model);
        let root_condition_plan = root_condition_plan(model, &root_refresh);
        let compiled_root_conditions = execution_backend.as_ref().and_then(|backend| {
            optional_compiled(
                "root_conditions",
                backend.compile_expression(&model.problem.events.root_conditions),
            )
        });
        let (initial_scalar_residual, initial_continuation) =
            InitialContinuationCoverage::certify_runtime_blocks(
                model,
                &implicit_scalar_rhs,
                &algebraic_refresh,
            )?;
        let compiled_initial_residual = execution_backend.as_ref().and_then(|backend| {
            optional_compiled(
                "initial_residual",
                backend.compile_expression(&initial_scalar_residual),
            )
        });
        let initial_scalar_jacobian =
            to_scalar_program_block(&model.artifacts.initialization.residual_jacobian_v)?;
        let compiled_initial_residual_jacobian_v = execution_backend.as_ref().and_then(|backend| {
            optional_compiled(
                "initial_residual_jacobian_v",
                backend.compile_jacobian_expression(&initial_scalar_jacobian),
            )
        });
        let delay_runtime = DelayRuntime::new(&model.problem.events.delays)?;
        let root_condition_count =
            total_root_condition_count(model, delay_runtime.event_root_count())?;
        let structured_discrete_rows =
            PreparedStructuredDiscreteRows::new(model, structured_discrete_scalar_rhs)?;
        let clock_partition_structured_rows = discrete_rows::clock_partition_structured_rows(
            model.problem.discrete.structured_updates.len(),
            structured_discrete_rows.rows(),
        );
        let clock_partition_clocks = discrete_rows::clock_partition_clocks(&model.problem.discrete);
        Ok(Self {
            model: model.clone(),
            state_count: model.state_scalar_count(),
            solver_count: model.solver_scalar_count(),
            implicit_rhs: PreparedComputeBlock::new_with_label(
                &model.problem.continuous.implicit_rhs,
                "runtime_implicit_rhs",
            )?,
            implicit_projection_jacobian_v: PreparedComputeBlock::new_with_label(
                &model.artifacts.continuous.implicit_jacobian_v,
                "runtime_implicit_projection_jacobian_v",
            )?,
            implicit_projection_scalar_jacobian_v: PreparedScalarProgramBlock::new(
                implicit_projection_scalar_jacobian,
            )?,
            implicit_scalar_rhs,
            refresh_program_rows,
            manifold_residual,
            manifold_jacobian_v,
            initial_residual: PreparedComputeBlock::new_with_label(
                &model.problem.initialization.residual,
                "runtime_initial_residual",
            )?,
            initial_residual_jacobian_v: PreparedComputeBlock::new_with_label(
                &model.artifacts.initialization.residual_jacobian_v,
                "runtime_initial_residual_jacobian_v",
            )?,
            initial_scalar_residual: PreparedScalarProgramBlock::new(initial_scalar_residual)?,
            compiled_implicit_rhs,
            compiled_implicit_projection_jacobian_v,
            compiled_implicit_full_jacobian_v,
            compiled_initial_residual,
            compiled_initial_residual_jacobian_v,
            derivative_rhs: PreparedComputeBlock::new_with_label(
                &model.problem.continuous.derivative_rhs,
                "runtime_derivative_rhs",
            )?,
            compiled_derivative_rhs,
            derivative_jacobian_v: PreparedScalarProgramBlock::new(
                model.artifacts.continuous.full_jacobian_v.clone(),
            )?,
            derivative_scalar: PreparedScalarProgramBlock::new(derivative_scalar_rhs)?,
            implicit_jacobian_v: PreparedScalarProgramBlock::new(implicit_full_jacobian_v)?,
            continuous_structural,
            initialization_structural,
            algebraic_newton_caches,
            algebraic_refresh,
            derivative_refresh,
            root_refresh,
            event_refresh,
            root_refresh_after_derivative,
            clock_event_refresh_after_event,
            initial_continuation,
            root_condition_rows: PreparedScalarProgramBlock::new(
                model.problem.events.root_conditions.clone(),
            )?,
            compiled_root_conditions,
            event_action_conditions: PreparedScalarProgramBlock::new(
                model.problem.events.action_conditions.clone(),
            )?,
            event_action_active_row_indices: RefCell::new(Vec::new()),
            root_condition_plan,
            discrete_rhs: PreparedScalarProgramBlock::new(model.problem.discrete.rhs.clone())?,
            observation_refresh_scalar_rows,
            observation_refresh_p_scratch: RefCell::new(Vec::new()),
            observation_refresh_values_scratch: RefCell::new(Vec::new()),
            clock_partition_intermediates: PreparedScalarProgramBlock::new(
                model.problem.discrete.clock_partition_intermediates.clone(),
            )?,
            clock_partition_clocks,
            clock_partition_structured_rows,
            clock_partition_work_y: RefCell::new(Vec::new()),
            clock_partition_work_p: RefCell::new(Vec::new()),
            guarded_assignment_programs,
            event_transaction_programs,
            event_transaction_coverage,
            runtime_assignment_rhs: PreparedScalarProgramBlock::new(
                model.problem.discrete.runtime_assignment_rhs.clone(),
            )?,
            post_commit_assignment_rhs: PreparedScalarProgramBlock::new(
                model.problem.discrete.post_commit_assignment_rhs.clone(),
            )?,
            update_values_scratch: RefCell::new(Vec::new()),
            structured_discrete_rows,
            visible_name_index: build_visible_name_index(model),
            visible_value_rows: PreparedScalarProgramBlock::new(model.visible_value_rows.clone())?,
            visible_value_plan,
            visible_scratch: RefCell::new(Vec::new()),
            refresh_snapshot_scratch: RefCell::new(Vec::new()),
            refresh_probe_scratch: RefCell::new(Vec::new()),
            refresh_tensor_scratch: RefCell::new(Vec::new()),
            static_refresh_cache: RefCell::new(StaticRefreshCache::default()),
            static_refresh_parameter_indices,
            parameter_static_gradient_cache: RefCell::new(ParameterStaticGradientCache::default()),
            runtime_state: solve_eval::SimulationRuntimeState::new(),
            delay_runtime,
            root_condition_count,
            derivative_scratch: RefCell::new(StateDerivativeScratch::default()),
            root_scratch: RefCell::new(Vec::new()),
            reverse_scratch: RefCell::new(solve_eval::reverse::ReverseScratch::default()),
            execution_backend,
            compiled_discrete_rows: RefCell::new(FxHashMap::default()),
            failed_discrete_rows: RefCell::new(BTreeSet::new()),
            compiled_clock_partition_intermediates: RefCell::new(FxHashMap::default()),
            failed_clock_partition_intermediates: RefCell::new(BTreeSet::new()),
            clock_partition_intermediate_scratch: RefCell::new(Vec::new()),
            compiled_guarded_assignments: RefCell::new(FxHashMap::default()),
            failed_guarded_assignments: RefCell::new(BTreeSet::new()),
            compiled_event_transactions,
            event_transaction_input_scratch: RefCell::new(Vec::new()),
            event_transaction_output_scratch: RefCell::new(event_transaction_output_scratch),
            compiled_root_rows: RefCell::new(FxHashMap::default()),
            failed_root_rows: RefCell::new(BTreeSet::new()),
            compiled_visible_rows: RefCell::new(FxHashMap::default()),
            failed_visible_rows: RefCell::new(BTreeSet::new()),
            compiled_event_action_rows: RefCell::new(FxHashMap::default()),
            failed_event_action_rows: RefCell::new(BTreeSet::new()),
            compiled_assignment_schedules: RefCell::new(FxHashMap::default()),
            compiled_output_scratch: RefCell::new(Vec::new()),
            clock_activation_cache: RefCell::new(ClockActivationCache::default()),
        })
    }

    /// Classify each periodic clock once per exact event instant.
    ///
    /// A grouped discrete program can have hundreds of scalar outputs sharing
    /// one clock. Repeating exact-lattice conversion and division for every
    /// output was measurable in the event hot path even though the answer is
    /// owned by the clock, not by the row.
    pub(super) fn discrete_row_active_at(
        &self,
        row_idx: usize,
        t: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let owner = self
            .model
            .problem
            .discrete
            .clock_owners
            .get(row_idx)
            .copied()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "discrete clock-owner row index {row_idx} is out of bounds"
                ))
            })?;
        let Some(owner) = owner else {
            return Ok(true);
        };
        self.periodic_clock_active(owner, t, "discrete row")
    }

    fn periodic_clock_active(
        &self,
        owner: solve::PeriodicClockId,
        t: f64,
        context: &str,
    ) -> Result<bool, RuntimeSolveError> {
        let schedules = &self.model.problem.clocks.periodic_event_schedules;
        if owner.index() >= schedules.len() {
            return Err(RuntimeSolveError::solve_ir(format!(
                "{context} refers to periodic clock {} outside the clock partition",
                owner.index()
            )));
        }
        let mut cache = self.clock_activation_cache.borrow_mut();
        if cache.time_bits != Some(t.to_bits()) {
            cache.active.clear();
            cache.active.extend(
                schedules
                    .iter()
                    .map(|schedule| crate::timeline::periodic_schedule_matches_time(schedule, t)),
            );
            cache.time_bits = Some(t.to_bits());
        }
        Ok(cache.active[owner.index()])
    }

    fn eval_discrete_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        let mut compiled_rows = self.compiled_discrete_rows.borrow_mut();
        let had_compiled = compiled_rows.contains_key(&program);
        let used_compiled = match compiled_rows.get_mut(&program) {
            Some(compiled_variants) => {
                self.try_cached_discrete_program(compiled_variants, y, p, t, out)?
            }
            None => false,
        };
        if used_compiled {
            self.report_nonfinite_discrete_program_outputs(program, t, out);
            return Ok(());
        }
        if compiled_rows.contains_key(&program) {
            tracing::debug!(
                target: "rumoca_solver::native_execution",
                program,
                "discarding invalid discrete specialization"
            );
        }
        drop(compiled_rows);

        if !had_compiled && !self.discrete_rhs.has_lazy_row_plan(program) {
            self.failed_discrete_rows.borrow_mut().remove(&program);
            self.compile_discrete_specialization(program);
            if self.compiled_discrete_rows.borrow().contains_key(&program) {
                return self.eval_discrete_program_outputs(program, y, p, t, out);
            }
        }

        self.discrete_rhs.eval_row_outputs_unchecked_with_context(
            program,
            y,
            p,
            t,
            self.row_eval_context(),
            out,
        )?;
        self.report_nonfinite_discrete_program_outputs(program, t, out);
        self.compile_discrete_specialization(program);
        Ok(())
    }

    fn try_cached_discrete_program(
        &self,
        compiled_variants: &mut Vec<CompiledDiscreteSpecialization>,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        for variant_index in (0..compiled_variants.len()).rev() {
            let compiled = &compiled_variants[variant_index];
            if !self.call_compiled_discrete_program(compiled, y, p, t, out)? {
                continue;
            }
            if variant_index + 1 != compiled_variants.len() {
                let compiled = compiled_variants.remove(variant_index);
                compiled_variants.push(compiled);
            }
            return Ok(true);
        }
        Ok(false)
    }

    fn call_compiled_discrete_program(
        &self,
        compiled: &CompiledDiscreteSpecialization,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        let total_outputs = compiled
            .output_count
            .checked_add(compiled.guard_expectations.len())
            .ok_or_else(|| RuntimeSolveError::solve_ir("compiled output count overflow"))?;
        let mut scratch = self.compiled_output_scratch.borrow_mut();
        scratch.resize(total_outputs, 0.0);
        let called =
            compiled
                .expression
                .call(y, p, t, self.model.external_tables.as_slice(), &mut scratch);
        let guards_match = scratch[compiled.output_count..]
            .iter()
            .zip(&compiled.guard_expectations)
            .all(|(actual, expected)| (*actual != 0.0) == *expected);
        if called.is_err() || !guards_match {
            return Ok(false);
        }
        out.clear();
        out.extend_from_slice(&scratch[..compiled.output_count]);
        Ok(true)
    }

    fn report_nonfinite_discrete_program_outputs(&self, program: usize, t: f64, values: &[f64]) {
        if !solve_eval::nan_trace::nan_trace_enabled() {
            return;
        }
        for (offset, value) in values.iter().copied().enumerate() {
            if value.is_finite() {
                continue;
            }
            let row = (0..self.model.problem.discrete.update_targets.len())
                .find(|&row| self.discrete_rhs.row_output_position(row) == Some((program, offset)));
            let target =
                row.and_then(|row| self.model.problem.discrete.update_targets.get(row).copied());
            let name = target.and_then(|target| {
                self.model
                    .problem
                    .layout
                    .bindings()
                    .iter()
                    .rev()
                    .find_map(|(name, slot)| (*slot == target).then(|| name.to_string()))
            });
            eprintln!(
                "[nan-trace] discrete program {program} output {offset} (row {}, target {}, slot {target:?}) @ t={t} = {}",
                row.map_or_else(|| "?".to_string(), |row| row.to_string()),
                name.as_deref().unwrap_or("<unnamed>"),
                if value.is_nan() { "NaN" } else { "inf" },
            );
        }
    }

    fn report_nonfinite_implicit_residual_inputs(&self, t: f64, y: &[f64], residual: &[f64]) {
        if !solve_eval::nan_trace::nan_trace_enabled() {
            return;
        }
        for (row, value) in residual.iter().copied().enumerate() {
            self.report_nonfinite_implicit_residual_row_inputs(t, y, row, value);
        }
    }

    fn report_nonfinite_implicit_residual_row_inputs(
        &self,
        t: f64,
        y: &[f64],
        row: usize,
        value: f64,
    ) {
        if value.is_finite() || !solve_eval::nan_trace::nan_trace_enabled() {
            return;
        }
        let Some((program, _)) = self.implicit_scalar_rhs.row_output_position(row) else {
            return;
        };
        let Some(ops) = self.implicit_scalar_rhs.block().programs().get(program) else {
            return;
        };
        let mut inputs = BTreeSet::new();
        for op in ops {
            match op {
                solve::LinearOp::LoadY { index, .. } => {
                    inputs.insert(*index);
                }
                solve::LinearOp::TensorLoad {
                    input: solve::TensorInputKind::Y,
                    input_start,
                    count,
                    ..
                } => {
                    inputs.extend(*input_start..input_start.saturating_add(*count));
                }
                _ => {}
            }
        }
        eprintln!(
            "[nan-trace] implicit residual row {row} ({}) @ t={t} = {}",
            self.solver_name(row),
            if value.is_nan() { "NaN" } else { "inf" },
        );
        for index in inputs {
            eprintln!(
                "[nan-trace]   input y[{index}] ({}) = {}",
                self.solver_name(index),
                y.get(index)
                    .map_or_else(|| "<missing>".to_string(), ToString::to_string),
            );
        }
    }

    pub fn row_eval_context(&self) -> RowEvalContext<'_> {
        RowEvalContext {
            external_tables: Some(self.model.external_tables.as_slice()),
            pure_calls: Some(&self.model.pure_calls),
            runtime_state: Some(&self.runtime_state),
            ..Default::default()
        }
    }

    pub fn has_delay_channels(&self) -> bool {
        !self.delay_runtime.is_empty()
    }

    pub fn reset_delay_history(&self) {
        self.delay_runtime.reset();
    }

    pub(crate) fn snapshot(&self) -> SolveRuntimeSnapshot {
        SolveRuntimeSnapshot {
            evaluator: self.runtime_state.snapshot(),
            delay: self.delay_runtime.snapshot(),
        }
    }

    pub(crate) fn restore(&self, snapshot: &SolveRuntimeSnapshot) {
        // Parameter-static refresh values are exact-keyed derived data, not
        // component continuation state. Keeping them across an FMU-state
        // restore is safe because every read validates the complete parameter
        // key, and avoids copying the model-width cache for every host probe.
        self.runtime_state.restore(&snapshot.evaluator);
        self.delay_runtime.restore(&snapshot.delay);
    }

    #[cfg(test)]
    pub(crate) fn matches_snapshot(&self, snapshot: &SolveRuntimeSnapshot) -> bool {
        self.runtime_state.matches_snapshot(&snapshot.evaluator)
            && self.delay_runtime.matches_snapshot(&snapshot.delay)
    }

    pub fn initialize_delay_history(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .initialize(time, solver_y, params, self.row_eval_context())
            .map_err(Into::into)
    }

    pub fn refresh_delay_values(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.delay_runtime
            .refresh(time, solver_y, params, self.row_eval_context())
            .map_err(Into::into)
    }

    pub fn commit_delay_history(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .commit(time, solver_y, params, self.row_eval_context())
            .map_err(Into::into)
    }

    /// Commit an accepted delay-history point whose source expression must be
    /// evaluated at a different time coordinate.
    ///
    /// Event left limits are owned by the event instant in accepted history,
    /// but their expressions are evaluated at the previous representable time.
    pub fn commit_delay_history_evaluated_at(
        &self,
        accepted_time: f64,
        evaluation_time: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .commit_evaluated_at(
                accepted_time,
                evaluation_time,
                solver_y,
                params,
                self.row_eval_context(),
            )
            .map_err(Into::into)
    }

    pub fn delay_step_limit(&self) -> Option<f64> {
        self.delay_runtime.step_limit()
    }

    pub fn root_condition_count(&self) -> usize {
        self.root_condition_count
    }

    pub fn derivative_settled_coordinate_can_refresh_roots(&self) -> bool {
        self.root_refresh_after_derivative.is_some()
    }

    pub fn full_solver_y(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut solver_y = Vec::new();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        self.refresh_algebraic_and_output_slots(t, &mut solver_y, params, tol, max_iters)?;
        Ok(solver_y)
    }

    pub fn full_solver_y_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        solver_y: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        self.populate_solver_y_from_state(solver_y, state)?;
        self.refresh_algebraic_and_output_slots(t, solver_y, params, tol, max_iters)
    }

    pub fn full_solver_y_with_guess(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.refresh_algebraic_and_output_slots(t, guess, params, tol, max_iters)
    }

    pub fn eval_state_derivatives(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut derivative = zero_runtime_values(self.state_count, "state derivative output")?;
        self.eval_state_derivatives_into(t, state, params, tol, max_iters, &mut derivative)?;
        Ok(derivative)
    }

    pub fn eval_state_derivatives_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut scratch = self.derivative_scratch.borrow_mut();
        let solver_y = &mut scratch.solver_y;
        self.populate_solver_y_from_state(solver_y, state)?;
        self.eval_state_derivatives_at_solver_y(t, params, tol, max_iters, solver_y, out)
    }

    pub fn eval_state_derivatives_with_guess(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut derivative = zero_runtime_values(self.state_count, "state derivative output")?;
        self.eval_state_derivatives_with_guess_into(
            t,
            state,
            params,
            guess,
            tol,
            max_iters,
            &mut derivative,
        )?;
        Ok(derivative)
    }

    // SPEC_0021: Exception - public runtime API mirrors solver callback inputs
    // without hiding mutable scratch/output buffers behind allocation.
    // SPEC_0021: Exception - validated boundary keeps proof-relevant inputs explicit.
    #[allow(clippy::too_many_arguments)]
    pub fn eval_state_derivatives_with_guess_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.refresh_derivative_dependencies(t, guess, params, tol, max_iters)?;
        self.eval_derivative_rhs_from_solver_y(t, guess, params, out)
    }

    pub fn eval_root_conditions(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return Ok(Vec::new());
        }
        let mut values = zero_runtime_values(root_count, "root condition output")?;
        self.eval_root_conditions_into(t, state, params, tol, max_iters, &mut values)?;
        Ok(values)
    }

    pub fn eval_root_conditions_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root condition output", root_count, out.len())?;
        let model_root_count = self.model.problem.events.root_conditions.len();
        let mut solver_y = self.root_scratch.borrow_mut();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        self.refresh_slots_with_plan(
            &self.root_refresh,
            RefreshSlotArgs {
                t,
                solver_y: &mut solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: false,
            },
        )?;
        self.eval_root_conditions_from_refreshed_solver_y(
            t,
            &solver_y,
            params,
            &mut out[..model_root_count],
        )?;
        self.delay_runtime
            .evaluate_event_roots(
                t,
                &solver_y,
                params,
                self.row_eval_context(),
                &mut out[model_root_count..],
            )
            .map_err(RuntimeSolveError::from)?;
        validate_finite_runtime_output("root condition output", out)
    }

    pub fn eval_root_search_conditions_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut solver_y = self.root_scratch.borrow_mut();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        self.eval_root_search_conditions_at_solver_y(t, params, tol, max_iters, out, &mut solver_y)
    }

    // SPEC_0021: Exception - public runtime API mirrors solver callback inputs
    // without hiding the certified warm-start and output buffers.
    // SPEC_0021: Exception - validated boundary keeps proof-relevant inputs explicit.
    #[allow(clippy::too_many_arguments)]
    pub fn eval_root_search_conditions_with_guess_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.eval_root_search_conditions_at_solver_y(t, params, tol, max_iters, out, guess)
    }

    fn eval_root_search_conditions_at_solver_y(
        &self,
        t: f64,
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
        solver_y: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root search output", root_count, out.len())?;
        let model_root_count = self.model.problem.events.root_conditions.len();
        let model_roots_need_refresh = model_root_count > 0
            && self
                .root_condition_plan
                .as_ref()
                .is_none_or(|plan| !plan.search_rows.is_empty());
        if model_roots_need_refresh || self.delay_runtime.event_root_count() > 0 {
            self.refresh_slots_with_plan(
                &self.root_refresh,
                RefreshSlotArgs {
                    t,
                    solver_y,
                    params,
                    tol,
                    max_iters,
                    certify_coordinates: false,
                },
            )?;
        }
        if model_root_count > 0 {
            let model_out = &mut out[..model_root_count];
            let Some(plan) = &self.root_condition_plan else {
                self.eval_root_conditions_from_refreshed_solver_y(t, solver_y, params, model_out)?;
                self.delay_runtime
                    .evaluate_event_roots(
                        t,
                        solver_y,
                        params,
                        self.row_eval_context(),
                        &mut out[model_root_count..],
                    )
                    .map_err(RuntimeSolveError::from)?;
                return validate_finite_runtime_output("root search output", out);
            };
            self.validate_root_plan_output_len(plan, model_out)?;
            if plan.search_rows.is_empty() {
                self.write_planned_root_search_defaults(plan, params, t, model_out)?;
            } else {
                self.write_planned_root_search_conditions(plan, solver_y, params, t, model_out)?;
            }
        }
        self.delay_runtime
            .evaluate_event_roots(
                t,
                solver_y,
                params,
                self.row_eval_context(),
                &mut out[model_root_count..],
            )
            .map_err(RuntimeSolveError::from)?;
        validate_finite_runtime_output("root search output", out)
    }

    pub fn eval_root_search_conditions_after_derivative_settle_into(
        &self,
        t: f64,
        params: &[f64],
        solver_y: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if !self.derivative_settled_coordinate_can_refresh_roots() {
            return Err(RuntimeSolveError::solve_ir(
                "root refresh has no certified derivative-settled remainder".to_string(),
            ));
        }
        if solver_y.len() != self.solver_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "derivative-settled solver-y length mismatch: expected {}, got {}",
                self.solver_count,
                solver_y.len()
            )));
        }
        let relation = self.root_refresh_after_derivative.as_ref().ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "root refresh derivative-settled remainder disappeared".to_string(),
            )
        })?;
        let remainder = relation.remainder();
        if !remainder.value_stages.is_empty() {
            self.refresh_slots_with_plan(
                remainder,
                RefreshSlotArgs {
                    t,
                    solver_y,
                    params,
                    tol,
                    max_iters,
                    certify_coordinates: false,
                },
            )?;
        }
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root search output", root_count, out.len())?;
        let model_root_count = self.model.problem.events.root_conditions.len();
        if model_root_count > 0 {
            let model_out = &mut out[..model_root_count];
            match &self.root_condition_plan {
                Some(plan) if plan.search_rows.is_empty() => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    self.write_planned_root_search_defaults(plan, params, t, model_out)?;
                }
                Some(plan) => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    self.write_planned_root_search_conditions(
                        plan, solver_y, params, t, model_out,
                    )?;
                }
                None => self
                    .eval_root_conditions_from_refreshed_solver_y(t, solver_y, params, model_out)?,
            }
        }
        self.delay_runtime
            .evaluate_event_roots(
                t,
                solver_y,
                params,
                self.row_eval_context(),
                &mut out[model_root_count..],
            )
            .map_err(RuntimeSolveError::from)?;
        validate_finite_runtime_output("root search output", out)
    }

    pub fn next_planned_time_root(
        &self,
        params: &[f64],
        current_t: f64,
        target: f64,
        tol: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(plan) = &self.root_condition_plan else {
            return Ok(None);
        };
        let mut next = None;
        for entry in &plan.entries {
            let RootConditionPlanEntry::DirectTime(root) = entry else {
                continue;
            };
            let event_time = direct_time_root_time(*root, params)?;
            if !event_time.is_finite() {
                continue;
            }
            if event_time > current_t + tol
                && (event_time < target || sample_time_match_with_tol(event_time, target))
            {
                next = Some(next.map_or(event_time, |current: f64| current.min(event_time)));
            }
        }
        Ok(next)
    }

    fn eval_root_conditions_from_refreshed_solver_y(
        &self,
        t: f64,
        y: &[f64],
        p: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if let Some(plan) = &self.root_condition_plan {
            return self.write_planned_root_conditions(plan, y, p, t, out);
        }
        if let Some(compiled) = &self.compiled_root_conditions {
            compiled
                .call(y, p, t, self.model.external_tables.as_slice(), out)
                .map_err(RuntimeSolveError::solve_ir)?;
            return validate_finite_runtime_output("root condition output", out);
        }
        self.root_condition_rows
            .eval_with_context(y, p, t, self.row_eval_context(), out)
            .map_err(Into::into)
    }

    fn write_planned_root_conditions(
        &self,
        plan: &RootConditionPlan,
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.validate_root_plan_output_len(plan, out)?;
        for (slot, entry) in out.iter_mut().zip(plan.entries.iter().copied()) {
            *slot = match entry {
                RootConditionPlanEntry::ConstantNonZero(value) => value,
                RootConditionPlanEntry::DirectTime(root) => {
                    direct_time_root_value(root, params, t)?
                }
                RootConditionPlanEntry::ContinuousStatic => 0.0,
                RootConditionPlanEntry::Dynamic => 0.0,
            };
        }
        self.eval_planned_root_rows(&plan.evaluated_rows, y, params, t, out)
    }

    fn write_planned_root_search_conditions(
        &self,
        plan: &RootConditionPlan,
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.write_planned_root_search_defaults(plan, params, t, out)?;
        self.eval_planned_root_rows(&plan.search_rows, y, params, t, out)
    }

    fn write_planned_root_search_defaults(
        &self,
        plan: &RootConditionPlan,
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.validate_root_plan_output_len(plan, out)?;
        for (slot, entry) in out.iter_mut().zip(plan.entries.iter().copied()) {
            *slot = match entry {
                RootConditionPlanEntry::ConstantNonZero(_)
                | RootConditionPlanEntry::ContinuousStatic
                | RootConditionPlanEntry::Dynamic => 1.0,
                RootConditionPlanEntry::DirectTime(root) => {
                    direct_time_root_search_default(root, params, t)?
                }
            };
        }
        Ok(())
    }

    fn eval_planned_root_rows(
        &self,
        output_indices: &[usize],
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if output_indices.is_empty() {
            return Ok(());
        }
        self.eval_selected_outputs_with_native(
            &self.root_condition_rows,
            &self.compiled_root_rows,
            &self.failed_root_rows,
            output_indices,
            y,
            params,
            t,
            out,
        )
    }

    fn validate_root_plan_output_len(
        &self,
        plan: &RootConditionPlan,
        out: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if out.len() >= plan.entries.len() {
            return Ok(());
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "root condition plan output index {} out of bounds for {} values",
            plan.entries.len().saturating_sub(1),
            out.len()
        )))
    }
}

fn trace_native_execution_failure(program: usize, reason: &str) {
    tracing::debug!(
        target: "rumoca_solver::native_execution",
        program,
        reason,
        "could not compile discrete specialization"
    );
}

fn cached_row_program(
    prepared: &PreparedScalarProgramBlock,
    row: usize,
) -> Option<(Vec<solve::LinearOp>, usize, Box<[bool]>)> {
    if let Some(specialization) = prepared.specialized_row_program(row) {
        return Some((
            specialization.program,
            specialization.output_count,
            specialization.guard_expectations,
        ));
    }
    Some((
        prepared.block().programs().get(row)?.clone(),
        prepared.row_output_count(row)?,
        Vec::new().into_boxed_slice(),
    ))
}

fn restore_after_causal_seed_error(
    error: RuntimeSolveError,
    solver_y: &mut [f64],
    incoming: &[f64],
) -> Result<(), RuntimeSolveError> {
    solver_y.copy_from_slice(incoming);
    if !seed_error_allows_projection(&error) {
        return Err(error);
    }
    tracing::debug!(
        target: "rumoca_eval_solve::refresh",
        "causal algebraic seed was unavailable; projecting the preserved residual system: {error}"
    );
    Ok(())
}

#[derive(Clone, Default)]
struct StateDerivativeScratch {
    /// Full solver vector reconstructed from the state slots, reused across
    /// derivative and Jacobian evaluations to avoid per-call allocation.
    solver_y: Vec<f64>,
    /// State-space probe direction expanded to a full solver-length seed, with
    /// the algebraic slots completed by the projection forward-sensitivity, for
    /// the AD Jacobian-vector product.
    seed_buf: Vec<f64>,
    /// Scratch unit seed used to read a single residual row's diagonal
    /// sensitivity `∂g_row/∂y_target`; kept all-zero between uses.
    unit_seed: Vec<f64>,
}

/// Tolerances for the algebraic projection's fixed-point settle (shared by the
/// value refresh and the seed/forward-sensitivity refresh).
#[derive(Debug, Clone, Copy)]
pub struct AlgebraicSettle {
    pub tol: f64,
    pub max_iters: usize,
}

/// Shared linearization context for the reconstruct-then-JVP entry points: the
/// evaluation time, the parameter vector, and the algebraic-settle tolerance
/// used to project algebraics from the state before linearizing. Bundling these
/// keeps the sensitivity entry points within the argument-count budget and threads
/// the same context through every layer without repetition.
#[derive(Debug, Clone, Copy)]
pub struct AlgebraicLinearization<'a> {
    pub t: f64,
    pub params: &'a [f64],
    pub settle: AlgebraicSettle,
}

/// Diagonal magnitude below which a seed residual row is treated as singular for
/// its paired target slot, matching the value refresh's residual-slope check.
fn validate_derivative_output_len(
    out: &[f64],
    state_count: usize,
) -> Result<(), RuntimeSolveError> {
    if out.len() == state_count {
        return Ok(());
    }
    Err(RuntimeSolveError::solve_ir(format!(
        "state derivative output length {} does not match state count {}",
        out.len(),
        state_count
    )))
}

#[cfg(test)]
mod tests;
