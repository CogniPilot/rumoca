//! Runtime orchestration for projection, events, and visible outputs.

mod execution_backend;

pub use execution_backend::{
    CompiledSolveAssignmentSchedule, CompiledSolveEventObservation, CompiledSolveEventTransaction,
    CompiledSolveProjectionJacobian, CompiledSolveTargetValues, SolveExecutionBackend,
};
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
use crate::runtime::solve_events::{event_action_params, visible_values_with_context};
use crate::runtime::solve_ops::write_clock_activation_params;
use crate::{
    EventActionOutcome, RuntimeEventStop, RuntimeSolveError, SolveStopSchedule,
    push_visible_values, relation_memory_value_from_root,
};
use rumoca_eval_solve::refresh_plan::trace_refresh_plan;
use rumoca_eval_solve::{
    EvalSolveError, PreparedComputeBlock, PreparedEvaluationBlock, PreparedEventTransactionProgram,
    PreparedGuardedAssignmentProgram, PreparedScalarProgramBlock, RowEvalContext,
    to_scalar_program_block,
};

/// The algebraic projection an event walk runs between its steps, reporting
/// whether it changed the coordinate.
pub(super) type ProjectAlgebraics<'a> =
    dyn FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError> + 'a;

mod block_residual_split;
pub use block_residual_split::{
    BlockResidualSplitCounts, block_residual_split_counts, reset_block_residual_split_counts,
};
mod chart_sharing;
mod compiled_expressions;
mod compiled_inventory;
mod compiled_lifecycle;
mod compiled_messages;
mod compiled_targets;
mod construction;
mod execution_owners;
mod selected_programs;
use execution_owners::{
    DirectionalProgram, EvaluationDirectional, EvaluationPrimal, PrimalProgram, ProgramAnalysis,
};
#[cfg(test)]
pub(crate) mod test_kernels;
use chart_sharing::BlockReuse;
pub use compiled_expressions::{CompiledSolveExpression, CompiledSolveJacobianExpression};
use compiled_inventory::{CompiledExpressions, PreparedExpressions, require_prepared};
use compiled_lifecycle::CompiledLifecycle;
mod coupled_event;
mod discrete_rows;
mod event_transactions;
pub use event_transactions::EventTransactionExecution;
use event_transactions::PreparedEventTransactionCoverage;
mod event_update;
mod execution_inventory;
use execution_inventory::RuntimeExecutionInventory;
pub use execution_inventory::ScalarExecutionView;
mod guarded_assignments;
mod initial_continuation;
mod initial_event;
mod initial_projection;
mod initial_relation_memory;
mod interpreted_schedules;
mod manifold_execution;
mod model_owner;
mod scalar_dispatch;
pub use model_owner::SolveRuntimeModel;
mod native_projection_assignments;
mod native_specialization;
use native_specialization::{RowEvalPoint, SpecializedRows};
mod plans;
mod precompiled;
pub use precompiled::{PrecompiledSolveBackend, PrecompiledSolveBackendBuilder};
mod reference_reverse;
mod refresh_batch;
mod refresh_execution;
mod refresh_projection;
mod relation_memory;
pub use reference_reverse::ReferenceReverseExecution;
mod seed_linearization;
mod sensitivity;
mod singular_mode;
mod support;
mod tangent_evaluators;
mod warnings;
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
    copy_grouped_expression_values, direct_time_root_search_default, direct_time_root_value,
    direct_visible_value, root_condition_plan, total_root_condition_count, visible_value_plan,
};
use refresh_execution::static_refresh_parameter_indices;
use refresh_projection::*;
use seed_linearization::SeedProjectionCache;
#[cfg(test)]
use support::compiled_expression;
use support::{
    admitted_compiled_error, admitted_output_call, build_visible_name_index, compiled_jacobian,
    copy_runtime_values, copy_runtime_values_into, fill_inactive_root_output, optional_compiled,
    required_compiled, reserve_runtime_index_map_capacity, reserve_runtime_vec_capacity,
    resize_runtime_values, validate_finite_runtime_output, validate_runtime_output_len,
    visible_value_index_error, zero_runtime_values,
};
use tangent_evaluators::{colored_tangent_evaluators, torn_tangent_evaluators};

#[derive(Clone)]
struct CompiledDiscreteSpecialization {
    source_span: rumoca_core::Span,
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
            EvalSolveError::CompiledExecution { message, span } => Self::CompiledExecution {
                context: "evaluator pure call",
                message,
                source_spans: span.into_iter().collect(),
            },
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
            fault @ (EvalSolveError::InvalidTensorIndex { .. }
            | EvalSolveError::TensorIndexOutOfBounds { .. }
            | EvalSolveError::NativeSourceIndexFault { .. }
            | EvalSolveError::SourceOperationFault { .. }) => Self::SourceFault {
                message: fault.to_string(),
                span: fault.source_span(),
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

/// Execution prepared for one immutable model and its derived layout counts.
/// Callers cannot substitute another source after preparation.
///
/// ```
/// use rumoca_solver::{SolveRuntime, SolveRuntimeModel};
/// fn inspect(runtime: &SolveRuntime) {
///     let _: &SolveRuntimeModel = runtime.model();
///     let _: usize = runtime.state_count();
///     let _: usize = runtime.solver_count();
/// }
/// ```
///
/// ```compile_fail
/// use rumoca_solver::SolveRuntime;
/// fn replace_source(runtime: &mut SolveRuntime, other: &SolveRuntime) {
///     runtime.model = other.model.clone();
/// }
/// ```
///
/// ```compile_fail
/// use rumoca_solver::SolveRuntime;
/// fn corrupt_state_count(runtime: &mut SolveRuntime) {
///     runtime.state_count = usize::MAX;
/// }
/// ```
///
/// ```compile_fail
/// use rumoca_solver::SolveRuntime;
/// fn corrupt_solver_count(runtime: &mut SolveRuntime) {
///     runtime.solver_count = 0;
/// }
/// ```
#[derive(Clone)]
pub struct SolveRuntime {
    model: SolveRuntimeModel,
    state_count: usize,
    solver_count: usize,
    runtime_state: solve_eval::SimulationRuntimeState,
    delay_runtime: DelayRuntime,
    root_condition_count: usize,
    /// Warning-level assertion sites reported so far (MLS §8.3.7).
    warning_log: RefCell<warnings::WarningLog>,
    clock_activation_cache: RefCell<ClockActivationCache>,
    execution: RuntimeExecutionInventory,
}

#[derive(Clone)]
struct ScalarExecutionInventory {
    coupled_event_jvps: coupled_event::CoupledEventJvps,
    implicit_projection_scalar_jacobian_v: DirectionalProgram,
    implicit_scalar_rhs: PrimalProgram,
    /// Tangent plan of each algebraic projection block's tearing, aligned
    /// with the block list; `None` for an untorn block or one without a plan.
    torn_tangents: Rc<[Option<rumoca_eval_solve::TornTangentEvaluator>]>,
    /// Colored tangent plan of each algebraic projection block, aligned with
    /// the block list and its structural artifacts.
    colored_tangents: Rc<[Option<rumoca_eval_solve::ColoredTangentEvaluator>]>,
    /// Block residual splits aligned with the projection plan's blocks.
    block_splits: Rc<[Option<Rc<block_residual_split::BlockSplits>>]>,
    /// Invariant values of the block projection call in progress.
    active_split: block_residual_split::ActiveSplitSlot,
    /// Output buffer of a single-row split evaluation.
    split_row_scratch: std::cell::RefCell<Vec<f64>>,
    /// The entries of the most recent batched residual evaluation.
    residual_row_batch: std::cell::RefCell<block_residual_split::ResidualRowBatch>,
    refresh_program_rows: FxHashMap<solve::RefreshScalarProgramSource, usize>,
    manifold: manifold_execution::PreparedManifoldProjection,
    initial_residual_jacobian_v: EvaluationDirectional,
    initial_scalar_residual: PrimalProgram,
    expressions: PreparedExpressions,
    compiled_algebraic_jacobians: Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>>,
    /// Forward-mode AD Jacobian-vector product of `derivative_rhs`
    /// (`d(der)/d(y)·v`), lowered to `LinearOp`s with `LoadSeed`. Applied — with a
    /// seed completed by `seed_refresh_derivative_dependencies` — to form
    /// the exact state Jacobian for the state-only BDF path.
    derivative_jacobian_v: EvaluationDirectional,
    /// Primal state-derivative scalar program `der = f(solver_y, p, t)`. Reversed
    /// by [`Self::reverse_state_derivative_vjp`] to form the reverse-mode VJP
    /// `(∂der/∂[solver_y|p])ᵀ·λ` (Track A scalar reverse core).
    derivative_scalar: EvaluationPrimal,
    /// Per-row forward-mode AD Jacobian-vector product of `implicit_rhs`
    /// (`d(residual_row)/d[y|p]·v`). Used to propagate state and parameter seeds
    /// through the algebraic projection row by row.
    implicit_jacobian_v: DirectionalProgram,
    continuous_structural: solve::ContinuousStructuralArtifacts,
    initialization_structural: solve::InitializationStructuralArtifacts,
    algebraic_newton_caches: Vec<RefCell<crate::runtime::projection::SparseNewtonCache>>,
    seed_projection_cache: RefCell<SeedProjectionCache>,
    algebraic_refresh: solve::RefreshPlan,
    /// Per initialization residual row, the positions of the algebraic
    /// refresh blocks it reads directly or through other blocks; `None` when
    /// a block Jacobian must carry its direction through every block
    /// ([`initial_projection::settled_read_cones`]).
    settled_read_cones: Option<initial_projection::InitialReadCones>,
    derivative_refresh: solve::RefreshPlan,
    root_refresh: solve::RefreshPlan,
    event_refresh: solve::RefreshPlan,
    root_refresh_after_derivative: Option<solve::RefreshRemainderRelation>,
    root_refresh_after_algebraic: Option<solve::RefreshRemainderRelation>,
    algebraic_refresh_after_derivative: Option<solve::RefreshRemainderRelation>,
    clock_event_refresh_after_event: Vec<solve::RefreshRemainderRelation>,
    /// Certified coverage for the initialization homotopy continuation; the
    /// single source of truth shared by the sweep driver and the acceptance
    /// check in [`InitialContinuationCoverage::certify`].
    initial_continuation: Option<InitialContinuationCoverage>,
    /// The observation programs of the root-condition and event-action blocks,
    /// checked once when the runtime is built.
    observation_authority: solve_eval::EventObservationAuthority,
    root_condition_rows: PreparedScalarProgramBlock,
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
    initialization_update_rhs: PreparedScalarProgramBlock,
    dynamic_time_event_rhs: PreparedScalarProgramBlock,
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
    torn_sweep_cache: TornSweepCache,
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
    event_transaction_input_scratch: RefCell<Vec<f64>>,
    event_transaction_output_scratch: RefCell<Vec<Vec<f64>>>,
    compiled_root_rows: RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_root_rows: RefCell<BTreeSet<usize>>,
    compiled_visible_rows: RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_visible_rows: RefCell<BTreeSet<usize>>,
    compiled_event_action_rows: RefCell<FxHashMap<usize, Vec<CompiledDiscreteSpecialization>>>,
    failed_event_action_rows: RefCell<BTreeSet<usize>>,
    compiled_assignment_schedules: interpreted_schedules::CompiledSchedules,
    interpreted_assignment_schedules: interpreted_schedules::InterpretedSchedules,
    native_projection_assignments: native_projection_assignments::NativeProjectionAssignments,
    compiled_output_scratch: RefCell<Vec<f64>>,
}

/// Opaque mutable state shared by the evaluators behind one ME component.
#[derive(Clone)]
pub(crate) struct SolveRuntimeSnapshot {
    evaluator: solve_eval::SimulationRuntimeStateSnapshot,
    delay: DelayRuntimeSnapshot,
}

impl SolveRuntime {
    /// The immutable source owner whose execution was prepared by construction.
    pub fn model(&self) -> &SolveRuntimeModel {
        &self.model
    }

    /// Number of continuous states in the prepared source layout.
    pub fn state_count(&self) -> usize {
        self.state_count
    }

    /// Number of solver values in the prepared source layout.
    pub fn solver_count(&self) -> usize {
        self.solver_count
    }

    pub fn new(model: &solve::SolveModel) -> Result<Self, EvalSolveError> {
        Self::new_with_execution_backend(model, None)
    }

    #[cfg(test)]
    pub(crate) fn new_fixture(model: &solve::SolveModel) -> Result<Self, EvalSolveError> {
        let mut model = model.clone();
        model.problem.continuous.refresh_owners =
            solve_eval::refresh_plan::build_continuous_refresh_owners(&mut model.problem)?;
        Self::new(&model)
    }

    #[cfg(test)]
    pub(crate) fn new_fixture_with_backend(
        model: &solve::SolveModel,
        backend: Rc<dyn SolveExecutionBackend>,
    ) -> Result<Self, EvalSolveError> {
        let mut model = model.clone();
        model.problem.continuous.refresh_owners =
            solve_eval::refresh_plan::build_continuous_refresh_owners(&mut model.problem)?;
        Self::new_with_execution_backend(&model, Some(backend))
    }

    pub fn new_with_execution_backend(
        model: &solve::SolveModel,
        execution_backend: Option<Rc<dyn SolveExecutionBackend>>,
    ) -> Result<Self, EvalSolveError> {
        Self::construct(SolveRuntimeModel::copied(model), execution_backend, None)
    }

    pub(crate) fn new_with_model_owner(
        model: SolveRuntimeModel,
        execution_backend: Option<Rc<dyn SolveExecutionBackend>>,
    ) -> Result<Self, EvalSolveError> {
        Self::construct(model, execution_backend, None)
    }

    /// The runtime of an alternate reduced chart over `model`, sharing this
    /// primary runtime's prepared and compiled programs wherever `model`
    /// carries the same ones (see [`chart_sharing`]).
    pub(crate) fn new_alternate(&self, model: &solve::SolveModel) -> Result<Self, EvalSolveError> {
        Self::construct(
            SolveRuntimeModel::copied(model),
            self.scalar_evaluation()?.scalar.execution_backend.clone(),
            Some(self),
        )
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

    pub fn has_delay_channels(&self) -> bool {
        !self.delay_runtime.is_empty()
    }

    pub fn reset_delay_history(&self) {
        self.delay_runtime.reset();
    }

    /// Record the accepted delay sources as the left limit of an event at
    /// `time`, the accepted coordinate Event Mode is entered at.
    pub fn hold_delay_history_at_event_entry(&self, time: f64) {
        self.delay_runtime.hold_accepted_history_to(time);
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

    pub fn root_condition_count(&self) -> usize {
        self.root_condition_count
    }
}

impl ScalarExecutionView<'_> {
    /// The checked observation context of the root-condition and event-action
    /// blocks, over the authority derived at construction.
    pub(super) fn event_observations(&self) -> solve_eval::CheckedEventObservationContext<'_> {
        solve_eval::CheckedEventObservationContext::new(
            &self.scalar.observation_authority,
            &self.model.pure_calls,
            &self.model.problem.events.actions,
        )
    }

    fn eval_discrete_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        if self.eval_lifecycle_program_outputs(
            &self.scalar.discrete_rhs,
            program,
            RowEvalPoint { y, p, t },
            out,
        )? {
            self.report_nonfinite_discrete_program_outputs(program, t, out);
            return Ok(());
        }
        let mut compiled_rows = self.scalar.compiled_discrete_rows.borrow_mut();
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

        if !had_compiled && !self.scalar.discrete_rhs.has_lazy_row_plan(program) {
            self.scalar
                .failed_discrete_rows
                .borrow_mut()
                .remove(&program);
            self.compile_discrete_specialization(program);
            if self
                .scalar
                .compiled_discrete_rows
                .borrow()
                .contains_key(&program)
            {
                return self.eval_discrete_program_outputs(program, y, p, t, out);
            }
        }

        self.scalar
            .discrete_rhs
            .eval_row_outputs_unchecked_with_context(
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
        let mut scratch = self.scalar.compiled_output_scratch.borrow_mut();
        scratch.resize(total_outputs, 0.0);
        compiled
            .expression
            .call(y, p, t, self.model.external_tables.as_slice(), &mut scratch)
            .map_err(|error| {
                admitted_compiled_error("discrete specialization", &[compiled.source_span], error)
            })?;
        let guards_match = scratch[compiled.output_count..]
            .iter()
            .zip(&compiled.guard_expectations)
            .all(|(actual, expected)| (*actual != 0.0) == *expected);
        if !guards_match {
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
            let row = (0..self.model.problem.discrete.update_targets.len()).find(|&row| {
                self.scalar.discrete_rhs.row_output_position(row) == Some((program, offset))
            });
            let target =
                row.and_then(|row| self.model.problem.discrete.update_targets.get(row).copied());
            let name =
                target.and_then(|target| self.model.problem.layout.scalar_name_for_slot(target));
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
        let Some((program, _)) = self
            .scalar
            .implicit_scalar_rhs
            .source()
            .row_output_position(row)
        else {
            return;
        };
        let Some(reads) = self
            .scalar
            .implicit_scalar_rhs
            .source()
            .nonfinite_y_reads(program)
        else {
            return;
        };
        let inputs = reads.collect::<BTreeSet<_>>();
        eprintln!(
            "[nan-trace] implicit residual row {row} ({}) @ t={t} = {}",
            self.row_target_name(row),
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

    /// The reduced tear Jacobian of a torn block from its tangent plan (see
    /// [`KernelRequest::TornJacobian`](crate::runtime::projection::KernelRequest)):
    /// `None` when the block has no plan, and a singular answer when a causal
    /// coefficient vanishes at this point. `index` is the block's position in
    /// the algebraic projection plan, the same index its tangent plan was
    /// issued under, so the lookup is direct.
    pub(crate) fn torn_tangent_jacobian(
        &self,
        index: usize,
        tearing: &solve::BlockTearing,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<Option<rumoca_eval_solve::TornTangentJacobian>>, RuntimeSolveError> {
        debug_assert!(
            self.model
                .problem
                .continuous
                .algebraic_projection_plan
                .blocks
                .scalar_blocks()
                .get(index)
                .is_some_and(|block| block.tearing.as_ref() == Some(tearing)),
            "a torn block index names the plan block that owns its tearing"
        );
        let evaluator = self
            .scalar
            .torn_tangents
            .get(index)
            .and_then(Option::as_ref);
        let Some(evaluator) = evaluator else {
            return Ok(None);
        };
        let owner = &self.scalar.implicit_projection_scalar_jacobian_v;
        let mut call =
            |program: usize, seed: &[f64], out: &mut Vec<f64>| -> Result<bool, RuntimeSolveError> {
                if !owner.is_compiled() {
                    return Ok(false);
                }
                owner.eval_program_outputs(
                    program,
                    solve_eval::JacobianEvalInputs { y, p, t, seed },
                    self.row_eval_context(),
                    out,
                )?;
                Ok(true)
            };
        evaluator
            .eval_through(
                rumoca_eval_solve::TangentPoint {
                    y,
                    p,
                    t,
                    context: self.row_eval_context(),
                },
                &mut call,
            )
            .map(Some)
    }

    /// Refresh every delayed value: from its source while the initialization
    /// problem settles, from the committed history afterwards; `true` when one
    /// moved beyond `tol`, the same change test every other settle step uses.
    fn refresh_initial_delay_identity(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        if !self.has_delay_channels() {
            return Ok(false);
        }
        let before = params.to_vec();
        self.refresh_delay_values(time, solver_y, params)?;
        Ok(crate::runtime_values_changed(&before, params, tol))
    }

    pub fn derivative_settled_coordinate_can_refresh_roots(&self) -> bool {
        self.scalar.root_refresh_after_derivative.is_some()
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

    pub(crate) fn full_solver_y_with_guess_certified(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.refresh_algebraic_and_output_slots_certified(t, guess, params, tol, max_iters)
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
        let mut scratch = self.scalar.derivative_scratch.borrow_mut();
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
        let mut solver_y = self.scalar.root_scratch.borrow_mut();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        self.refresh_slots_with_plan(
            &self.scalar.root_refresh,
            RefreshSlotArgs {
                t,
                solver_y: &mut solver_y,
                params,
                tol,
                max_iters,
                certify_coordinates: true,
            },
        )?;
        self.eval_root_conditions_from_refreshed_solver_y(
            t,
            &solver_y,
            params,
            &mut out[..model_root_count],
        )?;
        self.delay_runtime.evaluate_event_roots(
            t,
            &solver_y,
            params,
            self.row_eval_context(),
            &mut out[model_root_count..],
        )?;
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
        let mut solver_y = self.scalar.root_scratch.borrow_mut();
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
                .scalar
                .root_condition_plan
                .as_ref()
                .is_none_or(|plan| !plan.search_rows.is_empty());
        if model_roots_need_refresh || self.delay_runtime.event_root_count() > 0 {
            self.refresh_slots_with_plan(
                &self.scalar.root_refresh,
                RefreshSlotArgs {
                    t,
                    solver_y,
                    params,
                    tol,
                    max_iters,
                    certify_coordinates: true,
                },
            )?;
        }
        if model_root_count > 0 {
            let model_out = &mut out[..model_root_count];
            let Some(plan) = &self.scalar.root_condition_plan else {
                self.eval_root_conditions_from_refreshed_solver_y(t, solver_y, params, model_out)?;
                self.delay_runtime.evaluate_event_roots(
                    t,
                    solver_y,
                    params,
                    self.row_eval_context(),
                    &mut out[model_root_count..],
                )?;
                return validate_finite_runtime_output("root search output", out);
            };
            self.validate_root_plan_output_len(plan, model_out)?;
            if plan.search_rows.is_empty() {
                self.write_planned_root_search_defaults(plan, params, t, model_out)?;
            } else {
                self.write_planned_root_search_conditions(plan, solver_y, params, t, model_out)?;
            }
        }
        self.delay_runtime.evaluate_event_roots(
            t,
            solver_y,
            params,
            self.row_eval_context(),
            &mut out[model_root_count..],
        )?;
        validate_finite_runtime_output("root search output", out)
    }

    fn eval_root_conditions_from_refreshed_solver_y(
        &self,
        t: f64,
        y: &[f64],
        p: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if let Some(plan) = &self.scalar.root_condition_plan {
            return self.write_planned_root_conditions(plan, y, p, t, out);
        }
        if let Some(compiled) = self.scalar.compiled_root_conditions() {
            compiled.call(y, p, t, self.model.external_tables.as_slice(), out)?;
            return validate_finite_runtime_output("root condition output", out);
        }
        let observations = self.event_observations();
        let context = RowEvalContext {
            event_observations: Some(&observations),
            ..self.row_eval_context()
        };
        self.scalar
            .root_condition_rows
            .eval_with_context(y, p, t, context, out)
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
                RootConditionPlanEntry::ContinuousStatic | RootConditionPlanEntry::Dynamic => 1.0,
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
            SpecializedRows {
                block: &self.scalar.root_condition_rows,
                cache: &self.scalar.compiled_root_rows,
                failed: &self.scalar.failed_root_rows,
            },
            output_indices,
            RowEvalPoint { y, p: params, t },
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

impl SolveRuntime {
    pub fn row_eval_context(&self) -> RowEvalContext<'_> {
        RowEvalContext {
            external_tables: Some(self.model.external_tables.as_slice()),
            pure_calls: Some(&self.model.pure_calls),
            pure_call_execution: self
                .scalar_backend()
                .and_then(|backend| backend.pure_call_execution()),
            runtime_state: Some(&self.runtime_state),
            message_program_execution: self.message_program_execution(),
            ..Default::default()
        }
    }
    pub fn initialize_delay_history(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .initialize(time, solver_y, params, self.row_eval_context())
    }
    pub fn refresh_delay_values(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.delay_runtime
            .refresh(time, solver_y, params, self.row_eval_context())
    }
    pub fn commit_delay_history(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .commit(time, solver_y, params, self.row_eval_context())
    }
}
