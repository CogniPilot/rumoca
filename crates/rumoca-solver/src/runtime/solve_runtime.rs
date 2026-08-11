//! Runtime orchestration for projection, events, and visible outputs.

use indexmap::IndexMap;
use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, HashMap},
    rc::Rc,
};

use crate::runtime::delay::{DelayRuntime, DelayRuntimeSnapshot};
use crate::runtime::pre_params::{
    advance_event_iteration_pre_params, event_iteration_plan_settled, seed_event_entry_pre_params,
};
use crate::runtime::solve_events::{
    apply_discrete_slot_values, current_dynamic_time_event_stop, event_action_params,
    next_runtime_event_stop, visible_values_with_context,
};
use crate::runtime::solve_ops::write_clock_activation_params;
use crate::{
    EventActionOutcome, ImplicitProjectionModel, ManifoldProjectionModel, RuntimeEventStop,
    RuntimeSolveError, SolveStopSchedule, project_algebraic_seed_with_plan,
    project_algebraics_with_plan, project_algebraics_with_plan_certified, push_visible_values,
    relation_memory_value_from_root, replace_last_visible_values,
    timeline::sample_time_match_with_tol,
};
use rumoca_eval_solve::refresh_plan::{
    AlgebraicRefreshRow, RefreshPlan, RefreshStage, build_algebraic_refresh_plan,
    build_derivative_refresh_plan, build_root_refresh_plan,
    build_scalar_dependency_refresh_plan_with_outputs_and_programs, merge_dependency_refresh_plans,
    trace_refresh_plan,
};
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
mod plans;
mod refresh_batch;
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
use refresh_projection::*;
use support::{
    copy_runtime_values, copy_runtime_values_into, reserve_runtime_index_map_capacity,
    reserve_runtime_vec_capacity, resize_runtime_values, zero_runtime_values,
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
        programs: &[Vec<solve::LinearOp>],
        target_y_indices: &[usize],
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

fn optional_compiled<T>(label: &str, result: Result<T, String>) -> Option<T> {
    match result {
        Ok(compiled) => Some(compiled),
        Err(error) => {
            if std::env::var_os("RUMOCA_PROFILE_COMPILED").is_some() {
                eprintln!("rumoca-compiled-profile label={label} error={error}");
            }
            None
        }
    }
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
        return Ok(());
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
    manifold_residual: PreparedComputeBlock,
    manifold_jacobian_v: PreparedComputeBlock,
    initial_residual: PreparedComputeBlock,
    initial_residual_jacobian_v: PreparedComputeBlock,
    initial_scalar_residual: PreparedScalarProgramBlock,
    compiled_implicit_rhs: Option<Rc<dyn CompiledSolveExpression>>,
    compiled_implicit_projection_jacobian_v: Option<Rc<dyn CompiledSolveJacobianExpression>>,
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
    algebraic_refresh: RefreshPlan,
    derivative_refresh: RefreshPlan,
    root_refresh: RefreshPlan,
    event_refresh: RefreshPlan,
    clock_event_refresh: Vec<RefreshPlan>,
    combined_event_refresh: RefCell<HashMap<u64, Rc<RefreshPlan>>>,
    root_refresh_after_derivative: Option<RefreshPlan>,
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
    guarded_assignment_programs: Vec<PreparedGuardedAssignmentProgram>,
    event_transaction_programs: Vec<PreparedEventTransactionProgram>,
    event_transaction_coverage: PreparedEventTransactionCoverage,
    runtime_assignment_rhs: PreparedScalarProgramBlock,
    post_commit_assignment_rhs: PreparedScalarProgramBlock,
    update_values_scratch: RefCell<Vec<f64>>,
    structured_discrete_rows: PreparedStructuredDiscreteRows,
    visible_name_index: HashMap<String, usize>,
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
    compiled_discrete_rows: RefCell<HashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_discrete_rows: RefCell<BTreeSet<usize>>,
    compiled_guarded_assignments: RefCell<HashMap<usize, Rc<dyn CompiledSolveExpression>>>,
    failed_guarded_assignments: RefCell<BTreeSet<usize>>,
    compiled_event_transactions: Vec<Option<Rc<dyn CompiledSolveEventTransaction>>>,
    event_transaction_input_scratch: RefCell<Vec<f64>>,
    event_transaction_output_scratch: RefCell<Vec<Vec<f64>>>,
    compiled_root_rows: RefCell<HashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_root_rows: RefCell<BTreeSet<usize>>,
    compiled_visible_rows: RefCell<HashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_visible_rows: RefCell<BTreeSet<usize>>,
    compiled_event_action_rows: RefCell<HashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_event_action_rows: RefCell<BTreeSet<usize>>,
    compiled_refresh_specializations:
        RefCell<HashMap<(usize, usize), Vec<CompiledDiscreteSpecialization>>>,
    mixed_refresh_segments: RefCell<HashMap<(usize, usize), Option<Rc<[(usize, usize, bool)]>>>>,
    compiled_assignment_schedules:
        RefCell<HashMap<(usize, usize), Option<Rc<dyn CompiledSolveAssignmentSchedule>>>>,
    compiled_output_scratch: RefCell<Vec<f64>>,
    clock_activation_cache: RefCell<ClockActivationCache>,
}

/// Opaque mutable state shared by the evaluators behind one ME component.
#[derive(Clone)]
pub(crate) struct SolveRuntimeSnapshot {
    static_refresh_cache: StaticRefreshCache,
    evaluator: solve_eval::SimulationRuntimeStateSnapshot,
    delay: DelayRuntimeSnapshot,
}

fn static_refresh_parameter_indices<'a>(
    implicit: &PreparedScalarProgramBlock,
    plans: impl IntoIterator<Item = &'a RefreshPlan>,
) -> Box<[usize]> {
    let mut row_indices = BTreeSet::new();
    for plan in plans {
        row_indices.extend(plan.static_causal_seed_rows.iter().map(|row| row.row_idx));
        for stage in &plan.value_stages {
            match stage {
                RefreshStage::CausalSeedSweep { static_rows, .. }
                | RefreshStage::ExactAssignments { static_rows, .. } => {
                    row_indices.extend(static_rows.iter().map(|row| row.row_idx));
                }
                RefreshStage::ProjectionBlock { .. } => {}
            }
        }
    }
    let mut parameters = BTreeSet::new();
    for row in row_indices {
        if let Some(indices) = implicit.row_parameter_indices(row) {
            parameters.extend(indices.iter().copied());
        }
    }
    parameters
        .into_iter()
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

impl SolveRuntime {
    pub fn new(model: &solve::SolveModel) -> Result<Self, EvalSolveError> {
        Self::new_with_execution_backend(model, None)
    }

    pub fn new_with_execution_backend(
        model: &solve::SolveModel,
        execution_backend: Option<Rc<dyn SolveExecutionBackend>>,
    ) -> Result<Self, EvalSolveError> {
        let continuous_structural = model.artifacts.continuous.structural.clone();
        let initialization_structural = model.artifacts.initialization.structural.clone();
        let algebraic_newton_caches = (0..continuous_structural.algebraic_projection().len())
            .map(|_| RefCell::new(crate::runtime::projection::SparseNewtonCache::default()))
            .collect();
        let implicit_scalar_programs =
            to_scalar_program_block(&model.problem.continuous.implicit_rhs)?;
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
        let (
            algebraic_refresh,
            derivative_refresh,
            root_refresh,
            event_refresh,
            clock_event_refresh,
        ) = build_runtime_refresh_plans(
            model,
            &implicit_scalar_rhs,
            &derivative_scalar_rhs,
            &structured_discrete_scalar_rhs,
        )?;
        let mut refresh_plans = vec![
            &algebraic_refresh,
            &derivative_refresh,
            &root_refresh,
            &event_refresh,
        ];
        refresh_plans.extend(clock_event_refresh.iter());
        let static_refresh_parameter_indices =
            static_refresh_parameter_indices(&implicit_scalar_rhs, refresh_plans);
        let root_refresh_after_derivative =
            root_refresh.certified_value_remainder_after(&derivative_refresh);
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
            implicit_jacobian_v: PreparedScalarProgramBlock::new(
                model
                    .artifacts
                    .continuous
                    .implicit_jacobian_v_scalar
                    .clone(),
            )?,
            continuous_structural,
            initialization_structural,
            algebraic_newton_caches,
            algebraic_refresh,
            derivative_refresh,
            root_refresh,
            combined_event_refresh: RefCell::new(HashMap::from([(
                0,
                Rc::new(event_refresh.clone()),
            )])),
            event_refresh,
            clock_event_refresh,
            root_refresh_after_derivative,
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
            structured_discrete_rows: PreparedStructuredDiscreteRows::new(
                model,
                structured_discrete_scalar_rhs,
            )?,
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
            compiled_discrete_rows: RefCell::new(HashMap::new()),
            failed_discrete_rows: RefCell::new(BTreeSet::new()),
            compiled_guarded_assignments: RefCell::new(HashMap::new()),
            failed_guarded_assignments: RefCell::new(BTreeSet::new()),
            compiled_event_transactions,
            event_transaction_input_scratch: RefCell::new(Vec::new()),
            event_transaction_output_scratch: RefCell::new(event_transaction_output_scratch),
            compiled_root_rows: RefCell::new(HashMap::new()),
            failed_root_rows: RefCell::new(BTreeSet::new()),
            compiled_visible_rows: RefCell::new(HashMap::new()),
            failed_visible_rows: RefCell::new(BTreeSet::new()),
            compiled_event_action_rows: RefCell::new(HashMap::new()),
            failed_event_action_rows: RefCell::new(BTreeSet::new()),
            compiled_refresh_specializations: RefCell::new(HashMap::new()),
            mixed_refresh_segments: RefCell::new(HashMap::new()),
            compiled_assignment_schedules: RefCell::new(HashMap::new()),
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
        if let Some(compiled_variants) = compiled_rows.get_mut(&program) {
            for variant_index in (0..compiled_variants.len()).rev() {
                let compiled = &compiled_variants[variant_index];
                let total_outputs = compiled
                    .output_count
                    .checked_add(compiled.guard_expectations.len())
                    .ok_or_else(|| RuntimeSolveError::solve_ir("compiled output count overflow"))?;
                let mut scratch = self.compiled_output_scratch.borrow_mut();
                scratch.resize(total_outputs, 0.0);
                if compiled
                    .expression
                    .call(y, p, t, self.model.external_tables.as_slice(), &mut scratch)
                    .is_ok()
                    && scratch[compiled.output_count..]
                        .iter()
                        .zip(&compiled.guard_expectations)
                        .all(|(actual, expected)| (*actual != 0.0) == *expected)
                {
                    out.clear();
                    out.extend_from_slice(&scratch[..compiled.output_count]);
                    drop(scratch);
                    if variant_index + 1 != compiled_variants.len() {
                        let compiled = compiled_variants.remove(variant_index);
                        compiled_variants.push(compiled);
                    }
                    self.report_nonfinite_discrete_program_outputs(program, t, out);
                    return Ok(());
                }
            }
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

    fn compile_discrete_specialization(&self, program: usize) {
        let Some(backend) = &self.execution_backend else {
            return;
        };
        if self.failed_discrete_rows.borrow().contains(&program) {
            return;
        }
        let (program_ops, output_count, guard_expectations) =
            if let Some(specialization) = self.discrete_rhs.specialized_row_program(program) {
                (
                    specialization.program,
                    specialization.output_count,
                    specialization.guard_expectations,
                )
            } else {
                let Some(program_ops) = self.discrete_rhs.block().programs().get(program).cloned()
                else {
                    return;
                };
                let Some(output_count) = self.discrete_rhs.row_output_count(program) else {
                    return;
                };
                (program_ops, output_count, Vec::new().into_boxed_slice())
            };
        let Some(total_outputs) = output_count.checked_add(guard_expectations.len()) else {
            return;
        };
        let Some(span) = self.discrete_rhs.block().program_span(program) else {
            return;
        };
        let block = match solve::ScalarProgramBlock::with_output_indices(
            vec![program_ops],
            vec![span],
            (0..total_outputs).collect(),
        ) {
            Ok(block) => block,
            Err(error) => {
                trace_native_execution_failure(program, &error.to_string());
                self.failed_discrete_rows.borrow_mut().insert(program);
                return;
            }
        };
        let expression = match backend.compile_expression(&block) {
            Ok(expression) => expression,
            Err(error) => {
                trace_native_execution_failure(program, &error);
                self.failed_discrete_rows.borrow_mut().insert(program);
                return;
            }
        };
        tracing::debug!(
            target: "rumoca_solver::native_execution",
            program,
            ops = block.programs()[0].len(),
            outputs = output_count,
            guards = guard_expectations.len(),
            "compiled discrete specialization"
        );
        self.compiled_discrete_rows
            .borrow_mut()
            .entry(program)
            .or_default()
            .push(CompiledDiscreteSpecialization {
                expression,
                output_count,
                guard_expectations,
            });
    }

    #[allow(clippy::too_many_arguments)]
    fn eval_single_output_rows_with_native(
        &self,
        block: &PreparedScalarProgramBlock,
        cache: &RefCell<HashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
        failed: &RefCell<BTreeSet<usize>>,
        row_indices: &[usize],
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for &row in row_indices {
            if self.try_compiled_single_output(cache, row, y, p, t, out)? {
                continue;
            }
            // Retained tensor/fold programs can be dramatically more expensive
            // in the reference interpreter than in the native loop backend.
            // Try the complete checked program before paying for a profiling /
            // specialization pass. If that eager form is unsupported, clear
            // only this provisional failure so the interpreter can learn a
            // guarded specialization and compile it below.
            failed.borrow_mut().remove(&row);
            self.compile_cached_row(block, cache, failed, row);
            if self.try_compiled_single_output(cache, row, y, p, t, out)? {
                continue;
            }
            let value =
                block.eval_row_unchecked_with_context(row, y, p, t, self.row_eval_context())?;
            let out_len = out.len();
            let slot = out.get_mut(row).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "selected expression row {row} is outside output length {out_len}"
                ))
            })?;
            *slot = value;
            self.compile_cached_row(block, cache, failed, row);
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn eval_selected_outputs_with_native(
        &self,
        block: &PreparedScalarProgramBlock,
        cache: &RefCell<HashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
        failed: &RefCell<BTreeSet<usize>>,
        output_indices: &[usize],
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut programs = BTreeMap::<usize, Vec<(usize, usize)>>::new();
        for &output in output_indices {
            let (program, offset) = block.row_output_position(output).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "selected expression output {output} has no owning program"
                ))
            })?;
            programs.entry(program).or_default().push((output, offset));
        }
        for (program, selected) in programs {
            if self.try_compiled_program_outputs(cache, program, &selected, y, p, t, out)? {
                continue;
            }
            failed.borrow_mut().remove(&program);
            self.compile_cached_row(block, cache, failed, program);
            if self.try_compiled_program_outputs(cache, program, &selected, y, p, t, out)? {
                continue;
            }
            let mut values = Vec::new();
            block.eval_row_outputs_unchecked_with_context(
                program,
                y,
                p,
                t,
                self.row_eval_context(),
                &mut values,
            )?;
            for &(output, offset) in &selected {
                let value = values.get(offset).copied().ok_or_else(|| {
                    RuntimeSolveError::solve_ir(format!(
                        "selected expression output {output} offset {offset} is outside program {program}"
                    ))
                })?;
                let out_len = out.len();
                *out.get_mut(output).ok_or_else(|| {
                    RuntimeSolveError::solve_ir(format!(
                        "selected expression output {output} is outside output length {out_len}"
                    ))
                })? = value;
            }
            self.compile_cached_row(block, cache, failed, program);
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn try_compiled_program_outputs(
        &self,
        cache: &RefCell<HashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
        program: usize,
        selected: &[(usize, usize)],
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let mut cache = cache.borrow_mut();
        let Some(compiled_variants) = cache.get_mut(&program) else {
            return Ok(false);
        };
        for variant_index in (0..compiled_variants.len()).rev() {
            let compiled = &compiled_variants[variant_index];
            let total_outputs = compiled
                .output_count
                .checked_add(compiled.guard_expectations.len())
                .ok_or_else(|| RuntimeSolveError::solve_ir("compiled output count overflow"))?;
            let mut scratch = self.compiled_output_scratch.borrow_mut();
            scratch.resize(total_outputs, 0.0);
            let called = compiled.expression.call(
                y,
                p,
                t,
                self.model.external_tables.as_slice(),
                &mut scratch,
            );
            if let Err(error) = &called
                && std::env::var_os("RUMOCA_PROFILE_NATIVE").is_some()
            {
                eprintln!("[native-profile] program={program} call failed: {error}");
            }
            let valid = called.is_ok()
                && scratch[compiled.output_count..]
                    .iter()
                    .zip(&compiled.guard_expectations)
                    .all(|(actual, expected)| (*actual != 0.0) == *expected);
            if !valid {
                continue;
            }
            for &(output, offset) in selected {
                let value = scratch.get(offset).copied().ok_or_else(|| {
                    RuntimeSolveError::solve_ir(format!(
                        "compiled expression output {offset} is outside program {program}"
                    ))
                })?;
                let out_len = out.len();
                *out.get_mut(output).ok_or_else(|| {
                    RuntimeSolveError::solve_ir(format!(
                        "selected expression output {output} is outside output length {out_len}"
                    ))
                })? = value;
            }
            drop(scratch);
            if variant_index + 1 != compiled_variants.len() {
                let compiled = compiled_variants.remove(variant_index);
                compiled_variants.push(compiled);
            }
            return Ok(true);
        }
        Ok(false)
    }

    #[allow(clippy::too_many_arguments)]
    fn try_compiled_single_output(
        &self,
        cache: &RefCell<HashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
        row: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let mut cache = cache.borrow_mut();
        let Some(compiled_variants) = cache.get_mut(&row) else {
            return Ok(false);
        };
        for variant_index in (0..compiled_variants.len()).rev() {
            let compiled = &compiled_variants[variant_index];
            let total_outputs = compiled
                .output_count
                .checked_add(compiled.guard_expectations.len())
                .ok_or_else(|| RuntimeSolveError::solve_ir("compiled output count overflow"))?;
            let mut scratch = self.compiled_output_scratch.borrow_mut();
            scratch.resize(total_outputs, 0.0);
            let valid = compiled
                .expression
                .call(y, p, t, self.model.external_tables.as_slice(), &mut scratch)
                .is_ok()
                && scratch[compiled.output_count..]
                    .iter()
                    .zip(&compiled.guard_expectations)
                    .all(|(actual, expected)| (*actual != 0.0) == *expected);
            if !valid {
                continue;
            }
            let out_len = out.len();
            let slot = out.get_mut(row).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "selected expression row {row} is outside output length {out_len}"
                ))
            })?;
            *slot = scratch[0];
            drop(scratch);
            if variant_index + 1 != compiled_variants.len() {
                let compiled = compiled_variants.remove(variant_index);
                compiled_variants.push(compiled);
            }
            return Ok(true);
        }
        Ok(false)
    }

    fn compile_cached_row(
        &self,
        prepared: &PreparedScalarProgramBlock,
        cache: &RefCell<HashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
        failed: &RefCell<BTreeSet<usize>>,
        row: usize,
    ) {
        let Some(backend) = &self.execution_backend else {
            return;
        };
        if failed.borrow().contains(&row) {
            return;
        }
        let Some((program, output_count, guard_expectations)) = cached_row_program(prepared, row)
        else {
            failed.borrow_mut().insert(row);
            return;
        };
        let Some(total_outputs) = output_count.checked_add(guard_expectations.len()) else {
            failed.borrow_mut().insert(row);
            return;
        };
        let Some(span) = prepared.block().program_span(row) else {
            failed.borrow_mut().insert(row);
            return;
        };
        if std::env::var_os("RUMOCA_PROFILE_RESIDUAL_KERNELS").is_some() {
            let owner = if std::ptr::eq(prepared, &self.discrete_rhs) {
                "discrete_rhs"
            } else if std::ptr::eq(prepared, &self.root_condition_rows) {
                "root_conditions"
            } else if std::ptr::eq(prepared, &self.visible_value_rows) {
                "visible_values"
            } else if std::ptr::eq(prepared, &self.event_action_conditions) {
                "event_action_conditions"
            } else if std::ptr::eq(prepared, &self.runtime_assignment_rhs) {
                "runtime_assignments"
            } else if std::ptr::eq(prepared, &self.post_commit_assignment_rhs) {
                "post_commit_assignments"
            } else {
                "other"
            };
            eprintln!(
                "rumoca-native-specialization owner={owner} row={row} source={} start={} end={} direct_ops={} outputs={output_count} guards={}",
                span.source.0,
                span.start.0,
                span.end.0,
                program.len(),
                guard_expectations.len(),
            );
        }
        let block = match solve::ScalarProgramBlock::with_output_indices(
            vec![program],
            vec![span],
            (0..total_outputs).collect(),
        ) {
            Ok(block) => block,
            Err(error) => {
                trace_native_execution_failure(row, &error.to_string());
                failed.borrow_mut().insert(row);
                return;
            }
        };
        let expression = match backend.compile_expression(&block) {
            Ok(expression) => expression,
            Err(error) => {
                trace_native_execution_failure(row, &error);
                failed.borrow_mut().insert(row);
                return;
            }
        };
        cache
            .borrow_mut()
            .entry(row)
            .or_default()
            .push(CompiledDiscreteSpecialization {
                expression,
                output_count,
                guard_expectations,
            });
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
            static_refresh_cache: self.static_refresh_cache.borrow().clone(),
            evaluator: self.runtime_state.snapshot(),
            delay: self.delay_runtime.snapshot(),
        }
    }

    pub(crate) fn restore(&self, snapshot: &SolveRuntimeSnapshot) {
        self.static_refresh_cache
            .borrow_mut()
            .clone_from(&snapshot.static_refresh_cache);
        self.runtime_state.restore(&snapshot.evaluator);
        self.delay_runtime.restore(&snapshot.delay);
    }

    #[cfg(test)]
    pub(crate) fn matches_snapshot(&self, snapshot: &SolveRuntimeSnapshot) -> bool {
        self.static_refresh_cache
            .borrow()
            .bit_eq(&snapshot.static_refresh_cache)
            && self.runtime_state.matches_snapshot(&snapshot.evaluator)
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
        self.root_refresh_after_derivative.is_some() && self.delay_runtime.event_root_count() == 0
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

    /// Update an established full-layout guess for state-derivative evaluation.
    ///
    /// State-only integrators use this after an accepted step to preserve a
    /// warm start for the next RHS/Jacobian call. Only the compiler-proven
    /// derivative dependency closure is refreshed; observation-only
    /// algebraics are reconstructed at output or event boundaries instead of
    /// entering the integration hot loop.
    pub fn refresh_derivative_solver_y_with_guess(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.refresh_derivative_dependencies(t, guess, params, tol, max_iters)
    }

    fn refresh_derivative_dependencies(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_slots_with_plan(
            &self.derivative_refresh,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: false,
            },
        )
    }

    pub fn refresh_algebraic_and_output_slots_certified(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_slots_with_plan(
            &self.algebraic_refresh,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: true,
            },
        )
    }

    /// Refresh exactly the algebraic closure consumed by event iteration.
    ///
    /// The returned coordinate is certified for discrete assignments,
    /// conditions, relation memory, and event actions.  Unrelated visible
    /// outputs remain lazy until the caller requests the canonical full view.
    pub fn refresh_event_dependency_slots_certified(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        if self.clock_event_refresh.len() <= u64::BITS as usize {
            let mut active_mask = 0u64;
            for clock_index in 0..self.clock_event_refresh.len() {
                let owner = self
                    .model
                    .problem
                    .clocks
                    .periodic_clock_id(clock_index)
                    .ok_or_else(|| RuntimeSolveError::solve_ir("invalid event refresh clock"))?;
                if self.periodic_clock_active(owner, t, "event refresh")? {
                    active_mask |= 1u64 << clock_index;
                }
            }
            let plan = if let Some(plan) = self
                .combined_event_refresh
                .borrow()
                .get(&active_mask)
                .cloned()
            {
                plan
            } else {
                let mut parts = Vec::with_capacity(active_mask.count_ones() as usize + 1);
                parts.push(&self.event_refresh);
                parts.extend(
                    self.clock_event_refresh
                        .iter()
                        .enumerate()
                        .filter(|(clock, _)| active_mask & (1u64 << clock) != 0)
                        .map(|(_, plan)| plan),
                );
                let plan = Rc::new(merge_dependency_refresh_plans(
                    &self.model,
                    &self.implicit_scalar_rhs,
                    &self.algebraic_refresh,
                    &parts,
                )?);
                self.combined_event_refresh
                    .borrow_mut()
                    .insert(active_mask, Rc::clone(&plan));
                plan
            };
            return self.refresh_slots_with_plan(
                &plan,
                RefreshSlotArgs {
                    t,
                    solver_y,
                    params,
                    tol,
                    max_iters,
                    certify_coordinates: true,
                },
            );
        }

        // More than 64 independent typed clocks cannot use the allocation-free
        // mask cache. Preserve correctness with one base closure plus each
        // active clock closure; practical generated models use far fewer.
        self.refresh_slots_with_plan(
            &self.event_refresh,
            RefreshSlotArgs {
                t,
                solver_y: &mut *solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: true,
            },
        )?;
        for (clock_index, plan) in self.clock_event_refresh.iter().enumerate() {
            let owner = self
                .model
                .problem
                .clocks
                .periodic_clock_id(clock_index)
                .ok_or_else(|| RuntimeSolveError::solve_ir("invalid event refresh clock"))?;
            if self.periodic_clock_active(owner, t, "event refresh")? {
                self.refresh_slots_with_plan(
                    plan,
                    RefreshSlotArgs {
                        t,
                        solver_y: &mut *solver_y,
                        params,
                        tol,
                        max_iters,
                        certify_coordinates: true,
                    },
                )?;
            }
        }
        Ok(())
    }

    pub fn refresh_algebraic_and_output_slots(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_slots_with_plan(
            &self.algebraic_refresh,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: false,
            },
        )
    }

    fn refresh_slots_with_plan(
        &self,
        plan: &RefreshPlan,
        mut args: RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        if plan.rows.is_empty() && plan.simultaneous_plan.is_empty() {
            return Ok(());
        }
        self.validate_refresh_inputs(args.solver_y, args.params)?;
        let mut incoming = self.refresh_snapshot_scratch.borrow_mut();
        copy_runtime_values_into(
            &mut incoming,
            args.solver_y,
            "algebraic projection snapshot",
        )?;
        // A dependency-complete causal schedule already proves the value
        // solution.  Executing the staged projection schedule as well would
        // replay every exact singleton after the complete causal seed sweep.
        // Apart from being redundant, that doubles the dominant continuous
        // callback work for fully explicit models.
        if plan.causal_solution_certified {
            let result = self.refresh_causal_seed_rows(plan, &mut args);
            if result.is_err() {
                args.solver_y.copy_from_slice(&incoming);
            }
            return result;
        }
        if self.value_stage_schedule_is_certified(plan) {
            let result = self.refresh_slots_with_stages(plan, &mut args, &incoming);
            if result.is_err() {
                args.solver_y.copy_from_slice(&incoming);
            }
            return result;
        }
        let mut causal_seed_failed = false;
        if !plan.causal_seed_rows.is_empty() {
            match self.refresh_causal_seed_rows(plan, &mut args) {
                Ok(()) => {}
                Err(error) => {
                    restore_after_causal_seed_error(error, args.solver_y, &incoming)?;
                    causal_seed_failed = true;
                }
            }
        }
        let result = self.project_refresh_slots(plan, &mut args, causal_seed_failed);
        if result.is_err() {
            args.solver_y.copy_from_slice(&incoming);
        }
        result
    }

    fn refresh_causal_seed_rows(
        &self,
        plan: &RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_parameter_static_seed_rows(
            &plan.static_causal_seed_rows,
            args.t,
            args.solver_y,
            args.params,
        )?;
        self.refresh_slots_once(
            &plan.dynamic_causal_seed_rows,
            args.t,
            args.solver_y,
            args.params,
        )
    }

    fn refresh_parameter_static_seed_rows(
        &self,
        rows: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if rows.is_empty() {
            return Ok(());
        }
        self.prepare_static_refresh_cache(params, solver_y.len());
        self.refresh_prepared_static_rows(rows, t, solver_y, params)
    }

    fn prepare_static_refresh_cache(&self, params: &[f64], solver_len: usize) {
        let mut cache = self.static_refresh_cache.borrow_mut();
        let params_match = cache.valid
            && cache.params.len() == self.static_refresh_parameter_indices.len()
            && cache
                .params
                .iter()
                .zip(self.static_refresh_parameter_indices.iter().copied())
                .all(|(lhs, index)| {
                    params
                        .get(index)
                        .is_some_and(|rhs| lhs.to_bits() == rhs.to_bits())
                });
        if !params_match {
            cache.valid = true;
            cache.params.clear();
            cache.params.extend(
                self.static_refresh_parameter_indices
                    .iter()
                    .filter_map(|&index| params.get(index).copied()),
            );
            cache.values.clear();
            cache.values.resize(solver_len, None);
        }
    }

    fn refresh_prepared_static_rows(
        &self,
        rows: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        let fully_cached = {
            let cache = self.static_refresh_cache.borrow();
            rows.iter().all(|row| {
                cache
                    .values
                    .get(row.target_index)
                    .is_some_and(Option::is_some)
            })
        };
        if fully_cached {
            let cache = self.static_refresh_cache.borrow();
            for row in rows {
                solver_y[row.target_index] = cached_static_refresh_value(&cache, row.target_index)?;
            }
            return Ok(());
        }

        self.refresh_slots_once(rows, t, solver_y, params)?;
        let mut cache = self.static_refresh_cache.borrow_mut();
        for row in rows {
            cache.values[row.target_index] = Some(solver_y[row.target_index]);
        }
        Ok(())
    }

    fn project_refresh_slots(
        &self,
        plan: &RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        use_complete_plan: bool,
    ) -> Result<(), RuntimeSolveError> {
        let projection_plan = if use_complete_plan {
            &plan.simultaneous_plan
        } else {
            &plan.value_projection_plan
        };
        let projection_model = RefreshProjectionModel {
            runtime: self,
            plan: projection_plan,
            block_indices: &plan.simultaneous_block_indices,
            plan_validated: false,
            jacobian_v: ProjectionJacobian::SolverY {
                block: &self.implicit_projection_jacobian_v,
                scalar: &self.implicit_projection_scalar_jacobian_v,
            },
        };
        let projection_args = crate::AlgebraicProjectionArgs {
            parameters: args.params,
            time: args.t,
            state_count: self.state_count,
            tolerance: args.tol,
        };
        if args.certify_coordinates {
            project_algebraics_with_plan_certified(
                &projection_model,
                projection_plan,
                args.solver_y,
                projection_args,
                args.max_iters,
            )
        } else {
            project_algebraics_with_plan(
                &projection_model,
                projection_plan,
                args.solver_y,
                projection_args,
                args.max_iters,
            )
        }
    }

    /// Project accepted state values onto lower-order constraints retained by
    /// structural index reduction.
    pub fn project_state_manifold(
        &self,
        solver_y: &mut [f64],
        params: &[f64],
        t: f64,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let projection_model = RuntimeManifoldProjection { runtime: self };
        crate::project_state_manifold(
            &projection_model,
            solver_y,
            params,
            t,
            self.state_count,
            tol,
        )
    }

    /// Whether checked Solve IR retained any lower-order state constraints.
    ///
    /// An empty projection artifact is a construction-time certificate that
    /// projecting continuous states cannot change them. FMI hosts use this to
    /// avoid reconstructing observation algebraics merely to discover that
    /// there is no manifold system to evaluate.
    pub fn requires_state_manifold_projection(&self) -> bool {
        !self
            .model
            .problem
            .continuous
            .manifold_projection_plan
            .is_empty()
    }

    fn validate_refresh_inputs(
        &self,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        // Refresh-plan construction already proves one implicit output for
        // every algebraic coordinate. Explicit states are owned by derivative
        // rows and therefore need no placeholder implicit rows.
        solve_eval::validate_input_requirements(
            self.implicit_scalar_rhs.requirements(),
            solver_y,
            params,
            None,
        )?;
        Ok(())
    }

    fn eval_refresh_row(
        &self,
        row: &AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index;
        let value = self.eval_refresh_row_value(row, t, solver_y, params)?;
        // Catch non-finite results here (where the variable is known) and raise
        // a spanned diagnostic; otherwise a NaN slips through the iteration (the
        // `delta > max_delta` check is false for NaN) and only surfaces later as
        // an opaque "step size too small".
        if !value.is_finite() {
            return Err(self.non_finite_value_error(index, value));
        }
        Ok(value)
    }

    /// Solver slot name for diagnostics.
    fn solver_name(&self, index: usize) -> &str {
        self.model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(index)
            .map_or("<unnamed>", String::as_str)
    }

    /// Build a spanned non-finite-value error, resolving the solver slot's name
    /// and source span (from `variable_meta`) so the failure is traceable.
    fn non_finite_value_error(&self, index: usize, value: f64) -> RuntimeSolveError {
        let name = self
            .model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(index)
            .cloned()
            .unwrap_or_else(|| format!("y[{index}]"));
        let span = self.solver_source_span(index);
        let kind = if value.is_nan() { "NaN" } else { "inf" };
        RuntimeSolveError::NonFiniteValue { name, kind, span }
    }

    fn solver_source_span(&self, index: usize) -> Option<rumoca_core::Span> {
        let name = self
            .model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(index)?;
        self.model
            .variable_meta
            .iter()
            .find(|meta| &meta.name == name)
            .map(|meta| meta.source_span)
    }

    fn eval_refresh_row_value(
        &self,
        row: &AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index;
        // The assignment fast path is only valid when this plan entry updates
        // the row's own implicit target; for a cross-paired row (a coupled
        // block solved a residual row for one of its other unknowns) the
        // assignment value belongs to a different variable.
        if row.assignment_target == Some(index)
            && let Some(value) = self
                .implicit_scalar_rhs
                .eval_target_assignment_output_unchecked_with_context(
                    row.row_idx,
                    row.output_offset,
                    index,
                    solver_y,
                    params,
                    t,
                    self.row_eval_context(),
                )?
        {
            return Ok(value);
        }
        let residual = self.refresh_row_residual(row, t, solver_y, params)?;
        self.solve_refresh_residual_row(row, residual, t, solver_y, params)
    }

    /// Evaluate one scalar view of the canonical implicit residual system.
    fn refresh_row_residual(
        &self,
        row: &AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        self.implicit_scalar_rhs
            .eval_row_output_unchecked_with_context(
                row.row_idx,
                row.output_offset,
                solver_y,
                params,
                t,
                self.row_eval_context(),
            )
            .map_err(Into::into)
    }

    fn solve_refresh_residual_row(
        &self,
        row: &AlgebraicRefreshRow,
        residual: f64,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index;
        let current = solver_y[index];
        let mut probe_y = self.refresh_probe_scratch.borrow_mut();
        probe_y.clear();
        reserve_runtime_vec_capacity(&mut probe_y, solver_y.len(), "refresh residual probe")?;
        probe_y.extend_from_slice(solver_y);
        probe_y[index] = current + 1.0;
        let probe_residual = self.refresh_row_residual(row, t, &probe_y, params)?;
        let slope = probe_residual - residual;
        if slope.is_finite() && slope.abs() > 1.0e-12 {
            return Ok(current - residual / slope);
        }
        // A residual that does not respond to the paired variable means the
        // refresh plan paired this row with a variable it cannot determine.
        // Nudging the value by the residual (the old fallback) converges to a
        // wrong but stable solution; fail loudly instead.
        Err(RuntimeSolveError::RefreshTargetUnassignable {
            row: row.row_idx,
            target: self.solver_name(index).to_string(),
            span: self.solver_source_span(index),
        })
    }

    fn refresh_slots_once(
        &self,
        plan: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if self.try_mixed_native_assignment_refresh(plan, t, solver_y, params)? {
            self.validate_refresh_values(plan, solver_y)?;
            return Ok(());
        }
        if self.try_native_assignment_refresh(plan, t, solver_y, params)? {
            self.validate_refresh_values(plan, solver_y)?;
            return Ok(());
        }
        if self.can_batch_assignment_refresh(plan) {
            self.implicit_scalar_rhs
                .apply_target_assignment_rows_unchecked_with_context(
                    plan,
                    solver_y,
                    params,
                    t,
                    self.row_eval_context(),
                )
                .map_err(RuntimeSolveError::from)?;
            self.validate_refresh_values(plan, solver_y)?;
            return Ok(());
        }
        let mut row_outputs = Vec::new();
        let mut row_pos = 0usize;
        while row_pos < plan.len() {
            if let Some(next_pos) =
                self.try_refresh_tensor_output_segment(plan, row_pos, t, solver_y, params)?
            {
                row_pos = next_pos;
                continue;
            }
            if let Some(next_pos) = self.try_refresh_shapeless_output_segment(
                plan,
                row_pos,
                t,
                solver_y,
                params,
                &mut row_outputs,
            )? {
                row_pos = next_pos;
                continue;
            }
            let refresh_row = &plan[row_pos];
            let index = refresh_row.target_index;
            let value = self.eval_refresh_row(refresh_row, t, solver_y, params)?;
            solver_y[index] = value;
            row_pos += 1;
        }
        Ok(())
    }

    /// Execute a causal assignment plan without eagerly evaluating inactive
    /// branches. Straight-line runs retain the low-overhead native assignment
    /// schedule, while a lazy row is compiled from the active trace learned by
    /// the reference evaluator and protected by its appended branch guards.
    fn try_mixed_native_assignment_refresh(
        &self,
        plan: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        let key = (plan.as_ptr() as usize, plan.len());
        let cached = self.mixed_refresh_segments.borrow().get(&key).cloned();
        let segments = match cached {
            Some(Some(segments)) => segments,
            Some(None) => return Ok(false),
            None => {
                if self.execution_backend.is_none() || !self.can_batch_assignment_refresh(plan) {
                    self.mixed_refresh_segments.borrow_mut().insert(key, None);
                    return Ok(false);
                }
                let mut segments = Vec::new();
                let mut position = 0usize;
                let mut has_lazy = false;
                while position < plan.len() {
                    let start = position;
                    let row_idx = plan[position].row_idx;
                    let lazy = self.implicit_scalar_rhs.has_lazy_row_plan(row_idx);
                    position += 1;
                    if lazy {
                        has_lazy = true;
                        while position < plan.len() && plan[position].row_idx == row_idx {
                            position += 1;
                        }
                    } else {
                        while position < plan.len()
                            && !self
                                .implicit_scalar_rhs
                                .has_lazy_row_plan(plan[position].row_idx)
                        {
                            position += 1;
                        }
                    }
                    segments.push((start, position, lazy));
                }
                if !has_lazy {
                    self.mixed_refresh_segments.borrow_mut().insert(key, None);
                    return Ok(false);
                }
                let segments: Rc<[(usize, usize, bool)]> = segments.into();
                self.mixed_refresh_segments
                    .borrow_mut()
                    .insert(key, Some(segments.clone()));
                segments
            }
        };

        for &(start, end, lazy) in segments.iter() {
            let segment = &plan[start..end];
            if lazy {
                self.apply_guarded_native_refresh_group(segment, t, solver_y, params)?;
            } else if !self.try_native_assignment_refresh(segment, t, solver_y, params)? {
                self.implicit_scalar_rhs
                    .apply_target_assignment_rows_unchecked_with_context(
                        segment,
                        solver_y,
                        params,
                        t,
                        self.row_eval_context(),
                    )
                    .map_err(RuntimeSolveError::from)?;
            }
        }
        Ok(true)
    }

    fn apply_guarded_native_refresh_group(
        &self,
        group: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        let Some(first) = group.first() else {
            return Ok(());
        };
        let row_idx = first.row_idx;
        let key = (group.as_ptr() as usize, group.len());

        {
            let mut variants = self.compiled_refresh_specializations.borrow_mut();
            if let Some(variants) = variants.get_mut(&key) {
                for variant_index in (0..variants.len()).rev() {
                    let variant = &variants[variant_index];
                    let total_outputs = variant
                        .output_count
                        .checked_add(variant.guard_expectations.len())
                        .ok_or_else(|| {
                            RuntimeSolveError::solve_ir(
                                "compiled refresh specialization output count overflow",
                            )
                        })?;
                    let mut scratch = self.compiled_output_scratch.borrow_mut();
                    scratch.resize(total_outputs, 0.0);
                    let valid = variant
                        .expression
                        .call(
                            solver_y,
                            params,
                            t,
                            self.model.external_tables.as_slice(),
                            &mut scratch,
                        )
                        .is_ok()
                        && scratch[variant.output_count..]
                            .iter()
                            .zip(&variant.guard_expectations)
                            .all(|(actual, expected)| (*actual != 0.0) == *expected);
                    if !valid {
                        continue;
                    }
                    validate_runtime_output_len(
                        "compiled guarded refresh group",
                        group.len(),
                        variant.output_count,
                    )?;
                    for (row, value) in group.iter().zip(&scratch[..variant.output_count]) {
                        solver_y[row.target_index] = *value;
                    }
                    drop(scratch);
                    if variant_index + 1 != variants.len() {
                        let variant = variants.remove(variant_index);
                        variants.push(variant);
                    }
                    return Ok(());
                }
            }
        }

        let output_targets = group
            .iter()
            .map(|row| (row.output_offset, row.target_index))
            .collect::<Vec<_>>();
        let Some(program) = self
            .implicit_scalar_rhs
            .exact_target_assignment_group_program(row_idx, &output_targets)
        else {
            self.implicit_scalar_rhs
                .apply_target_assignment_rows_unchecked_with_context(
                    group,
                    solver_y,
                    params,
                    t,
                    self.row_eval_context(),
                )
                .map_err(RuntimeSolveError::from)?;
            return Ok(());
        };
        let Some(span) = self.implicit_scalar_rhs.block().program_span(row_idx) else {
            return Err(RuntimeSolveError::solve_ir(
                "guarded refresh group is missing its source span",
            ));
        };
        let block = solve::ScalarProgramBlock::with_output_indices(
            vec![program],
            vec![span],
            (0..group.len()).collect(),
        )
        .map_err(|error| RuntimeSolveError::solve_ir(error.to_string()))?;
        let prepared = PreparedScalarProgramBlock::new(block).map_err(RuntimeSolveError::from)?;
        let mut values = Vec::new();
        prepared.eval_row_outputs_unchecked_with_context(
            0,
            solver_y,
            params,
            t,
            self.row_eval_context(),
            &mut values,
        )?;
        validate_runtime_output_len("guarded refresh group", group.len(), values.len())?;

        self.compile_guarded_refresh_specialization(key, &prepared);
        for (row, value) in group.iter().zip(values) {
            solver_y[row.target_index] = value;
        }
        Ok(())
    }

    fn compile_guarded_refresh_specialization(
        &self,
        key: (usize, usize),
        prepared: &PreparedScalarProgramBlock,
    ) {
        let Some(backend) = self.execution_backend.as_ref() else {
            return;
        };
        let Some((program, output_count, guard_expectations)) = cached_row_program(prepared, 0)
        else {
            return;
        };
        let Some(total_outputs) = output_count.checked_add(guard_expectations.len()) else {
            return;
        };
        let Some(span) = prepared.block().program_span(0) else {
            return;
        };
        let Ok(block) = solve::ScalarProgramBlock::with_output_indices(
            vec![program],
            vec![span],
            (0..total_outputs).collect(),
        ) else {
            return;
        };
        let Ok(expression) = backend.compile_expression(&block) else {
            return;
        };
        tracing::debug!(
            target: "rumoca_solver::native_execution",
            ops = block.programs()[0].len(),
            outputs = output_count,
            guards = guard_expectations.len(),
            "compiled guarded refresh specialization"
        );
        self.compiled_refresh_specializations
            .borrow_mut()
            .entry(key)
            .or_default()
            .push(CompiledDiscreteSpecialization {
                expression,
                output_count,
                guard_expectations,
            });
    }

    fn try_native_assignment_refresh(
        &self,
        plan: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        let Some(backend) = self.execution_backend.as_ref() else {
            return Ok(false);
        };
        let key = (plan.as_ptr() as usize, plan.len());
        let cached = self
            .compiled_assignment_schedules
            .borrow()
            .get(&key)
            .cloned();
        let compiled = match cached {
            Some(Some(compiled)) => compiled,
            Some(None) => return Ok(false),
            None => {
                let mut programs = Vec::new();
                let mut targets = Vec::with_capacity(plan.len());
                let mut debug_groups = Vec::new();
                let mut position = 0usize;
                let mut complete = true;
                while position < plan.len() {
                    let row_idx = plan[position].row_idx;
                    let mut end = position + 1;
                    while end < plan.len() && plan[end].row_idx == row_idx {
                        end += 1;
                    }
                    let group_output_targets = plan[position..end]
                        .iter()
                        .map(|row| (row.output_offset, row.target_index))
                        .collect::<Vec<_>>();
                    if let Some(program) = self
                        .implicit_scalar_rhs
                        .exact_target_assignment_group_program(row_idx, &group_output_targets)
                    {
                        programs.push(program);
                        targets.extend(group_output_targets.iter().map(|&(_, target)| target));
                        debug_groups.push((
                            row_idx,
                            group_output_targets
                                .iter()
                                .map(|&(_, target)| target)
                                .collect::<Vec<_>>(),
                        ));
                        position = end;
                        continue;
                    }
                    let row = &plan[position];
                    let Some(program) = self
                        .implicit_scalar_rhs
                        .exact_target_assignment_output_program(
                            row.row_idx,
                            row.output_offset,
                            row.target_index,
                        )
                    else {
                        complete = false;
                        break;
                    };
                    programs.push(program);
                    targets.push(row.target_index);
                    debug_groups.push((row.row_idx, vec![row.target_index]));
                    position += 1;
                }
                if !complete {
                    self.compiled_assignment_schedules
                        .borrow_mut()
                        .insert(key, None);
                    return Ok(false);
                }
                if std::env::var_os("RUMOCA_PROFILE_IR").is_some() {
                    let target_names = targets
                        .iter()
                        .take(32)
                        .map(|&target| self.solver_name(target))
                        .collect::<Vec<_>>();
                    eprintln!(
                        "rumoca-assignment-schedule rows={} targets={} names={target_names:?}",
                        programs.len(),
                        targets.len(),
                    );
                    for (program, (row, group_targets)) in programs.iter().zip(debug_groups.iter())
                    {
                        if program.len() < 100
                            && !program.iter().any(|operation| {
                                matches!(
                                    operation,
                                    rumoca_ir_solve::LinearOp::FunctionFold { .. }
                                        | rumoca_ir_solve::LinearOp::StoreOutputFunctionFold { .. }
                                )
                            })
                        {
                            continue;
                        }
                        let names = group_targets
                            .iter()
                            .map(|&target| self.solver_name(target))
                            .collect::<Vec<_>>();
                        let mut kinds = std::collections::BTreeMap::new();
                        for operation in program {
                            *kinds.entry(operation.kind_name()).or_insert(0usize) += 1;
                        }
                        eprintln!(
                            "rumoca-assignment-program row={row} ops={} outputs={} names={names:?} kinds={kinds:?}",
                            program.len(),
                            group_targets.len(),
                        );
                    }
                }
                let compiled = match backend.compile_assignment_schedule(&programs, &targets) {
                    Ok(compiled) => Some(compiled),
                    Err(error) => {
                        tracing::debug!(
                            target: "rumoca_solver::native_execution",
                            programs = programs.len(),
                            targets = targets.len(),
                            %error,
                            "failed to compile assignment schedule"
                        );
                        None
                    }
                };
                self.compiled_assignment_schedules
                    .borrow_mut()
                    .insert(key, compiled.clone());
                if let Some(compiled) = compiled {
                    compiled
                } else {
                    self.compiled_assignment_schedules
                        .borrow_mut()
                        .insert(key, None);
                    return Ok(false);
                }
            }
        };
        if compiled
            .call(solver_y, params, t, self.model.external_tables.as_slice())
            .is_ok()
        {
            return Ok(true);
        }
        self.compiled_assignment_schedules
            .borrow_mut()
            .insert(key, None);
        Ok(false)
    }

    fn validate_refresh_values(
        &self,
        plan: &[AlgebraicRefreshRow],
        solver_y: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        for row in plan {
            let value = solver_y[row.target_index];
            if !value.is_finite() {
                return Err(self.non_finite_value_error(row.target_index, value));
            }
        }
        Ok(())
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
        if self.delay_runtime.event_root_count() > 0 {
            self.refresh_algebraic_and_output_slots(t, &mut solver_y, params, tol, max_iters)?;
        }
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
        if self.delay_runtime.event_root_count() > 0 {
            self.refresh_algebraic_and_output_slots(t, solver_y, params, tol, max_iters)?;
        }
        if model_root_count > 0 {
            let model_out = &mut out[..model_root_count];
            let Some(plan) = &self.root_condition_plan else {
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

    #[allow(clippy::too_many_arguments)]
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
        let remainder = self.root_refresh_after_derivative.as_ref().ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "root refresh derivative-settled remainder disappeared".to_string(),
            )
        })?;
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
        validate_finite_runtime_output("root search output", out)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn eval_root_conditions_after_derivative_settle_into(
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
        let remainder = self.root_refresh_after_derivative.as_ref().ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "root refresh derivative-settled remainder disappeared".to_string(),
            )
        })?;
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
        validate_runtime_output_len("root condition output", root_count, out.len())?;
        let model_root_count = self.model.problem.events.root_conditions.len();
        self.eval_root_conditions_from_refreshed_solver_y(
            t,
            solver_y,
            params,
            &mut out[..model_root_count],
        )?;
        validate_finite_runtime_output("root condition output", out)
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
                RootConditionPlanEntry::StaticParameter => 0.0,
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
                | RootConditionPlanEntry::StaticParameter
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
        row_indices: &[usize],
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if row_indices.is_empty() {
            return Ok(());
        }
        self.eval_single_output_rows_with_native(
            &self.root_condition_rows,
            &self.compiled_root_rows,
            &self.failed_root_rows,
            row_indices,
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
    if std::env::var_os("RUMOCA_PROFILE_NATIVE").is_some() {
        eprintln!("[native-profile] program={program}: {reason}");
    }
    tracing::debug!(
        target: "rumoca_solver::native_execution",
        program,
        reason,
        "could not compile discrete specialization"
    );
}

fn build_visible_name_index(model: &solve::SolveModel) -> HashMap<String, usize> {
    model
        .visible_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect()
}

fn build_runtime_refresh_plans(
    model: &solve::SolveModel,
    implicit: &PreparedScalarProgramBlock,
    derivative: &solve::ScalarProgramBlock,
    structured_discrete: &solve::ScalarProgramBlock,
) -> Result<
    (
        RefreshPlan,
        RefreshPlan,
        RefreshPlan,
        RefreshPlan,
        Vec<RefreshPlan>,
    ),
    EvalSolveError,
> {
    let algebraic = build_algebraic_refresh_plan(model, implicit)?;
    let derivative = build_derivative_refresh_plan(model, derivative, implicit, &algebraic)?;
    let root = build_root_refresh_plan(model, implicit, &algebraic)?;
    let clock_count = model.problem.clocks.periodic_event_schedules.len();
    let mut unowned_discrete = Vec::new();
    let mut clock_discrete = vec![Vec::new(); clock_count];
    for (output, clock_owner) in model
        .problem
        .discrete
        .clock_owners
        .iter()
        .copied()
        .enumerate()
    {
        match clock_owner {
            Some(owner) => clock_discrete[owner.index()].push(output),
            None => unowned_discrete.push(output),
        }
    }
    let mut unowned_guarded = Vec::new();
    let mut clock_guarded = vec![Vec::new(); clock_count];
    for program in &model.problem.discrete.guarded_assignments {
        let consumer = (program.program(), program.span());
        match program.clock_owner() {
            Some(owner) => clock_guarded[owner.index()].push(consumer),
            None => unowned_guarded.push(consumer),
        }
    }
    let mut unowned_structured = Vec::new();
    let mut clock_structured = vec![Vec::new(); clock_count];
    for (update_index, update) in model.problem.discrete.structured_updates.iter().enumerate() {
        let outputs = model
            .problem
            .discrete
            .structured_assignments(update_index)?;
        let destination = update.clock_owner.map_or(&mut unowned_structured, |owner| {
            &mut clock_structured[owner.index()]
        });
        destination.extend(outputs.into_iter().map(|(_, source)| source));
    }
    let mut unowned_actions = Vec::new();
    let mut clock_actions = vec![Vec::new(); clock_count];
    for (output, action) in model.problem.events.actions.iter().enumerate() {
        match action.clock_owner {
            Some(owner) => clock_actions[owner.index()].push(output),
            None => unowned_actions.push(output),
        }
    }
    let event = build_scalar_dependency_refresh_plan_with_outputs_and_programs(
        model,
        implicit,
        &algebraic,
        &[
            &model.problem.discrete.runtime_assignment_rhs,
            &model.problem.discrete.post_commit_assignment_rhs,
            &model.problem.events.root_conditions,
        ],
        &[
            (&model.problem.discrete.rhs, &unowned_discrete),
            (structured_discrete, &unowned_structured),
            (&model.problem.events.action_conditions, &unowned_actions),
        ],
        &unowned_guarded,
    )?;
    let mut clock_events = Vec::with_capacity(clock_count);
    for clock in 0..clock_count {
        clock_events.push(
            build_scalar_dependency_refresh_plan_with_outputs_and_programs(
                model,
                implicit,
                &algebraic,
                &[],
                &[
                    (&model.problem.discrete.rhs, &clock_discrete[clock]),
                    (structured_discrete, &clock_structured[clock]),
                    (
                        &model.problem.events.action_conditions,
                        &clock_actions[clock],
                    ),
                ],
                &clock_guarded[clock],
            )?,
        );
    }
    trace_refresh_plan(model, "algebraic", &algebraic);
    trace_refresh_plan(model, "derivative", &derivative);
    trace_refresh_plan(model, "root", &root);
    trace_refresh_plan(model, "event", &event);
    for (clock, plan) in clock_events.iter().enumerate() {
        trace_refresh_plan(model, &format!("clock-event-{clock}"), plan);
    }
    Ok((algebraic, derivative, root, event, clock_events))
}

fn fill_inactive_root_output(out: &mut [f64]) -> Result<(), RuntimeSolveError> {
    if let Some(first) = out.first_mut() {
        *first = 1.0;
    }
    Ok(())
}

fn validate_runtime_output_len(
    context: &str,
    expected: usize,
    actual: usize,
) -> Result<(), RuntimeSolveError> {
    if actual == expected {
        return Ok(());
    }
    Err(RuntimeSolveError::solve_ir(format!(
        "{context} expected {expected} values, got {actual}"
    )))
}

fn validate_finite_runtime_output(context: &str, values: &[f64]) -> Result<(), RuntimeSolveError> {
    if let Some((index, value)) = values
        .iter()
        .copied()
        .enumerate()
        .find(|(_, value)| !value.is_finite())
    {
        return Err(RuntimeSolveError::solve_ir(format!(
            "{context} produced non-finite value {value} at index {index}"
        )));
    }
    Ok(())
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

fn visible_value_index_error(
    name: &str,
    index: usize,
    len: usize,
    context: &'static str,
) -> RuntimeSolveError {
    RuntimeSolveError::solve_ir(format!(
        "{context} for visible name `{name}` reference index {index}, but only {len} values are available"
    ))
}

#[cfg(test)]
mod tests;
