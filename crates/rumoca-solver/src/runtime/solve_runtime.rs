//! Runtime orchestration for projection, events, and visible outputs.

use indexmap::IndexMap;
use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;
use rustc_hash::FxHashMap;
use std::{cell::RefCell, collections::BTreeSet, ops::Deref, rc::Rc, sync::Arc};

use crate::runtime::delay::{DelayRuntime, DelayRuntimeSnapshot};
use crate::runtime::pre_params::{
    advance_event_iteration_pre_params, event_iteration_plan_settled, seed_event_entry_pre_params,
};
use crate::runtime::projection::{
    ImplicitProjectionModel, ManifoldProjectionModel, project_algebraic_seed_with_plan,
    project_algebraics_with_plan, project_algebraics_with_plan_certified,
};
use crate::runtime::solve_events::{
    RuntimeEventStopRequest, current_dynamic_time_event_stop, event_action_params,
    next_runtime_event_stop, visible_values_with_context,
};
use crate::runtime::solve_ops::write_clock_activation_params;
use crate::{
    EventActionOutcome, NativeExecutionOwner, RuntimeEventStop, RuntimeSolveError,
    SolveStopSchedule, push_visible_values, relation_memory_value_from_root,
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
mod linearization;
pub use linearization::{AlgebraicLinearization, AlgebraicSettle};
use linearization::{StateDerivativeScratch, validate_derivative_output_len};
mod selected_rows;
use selected_rows::{RowEvalPoint, SelectedRows};
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
    copy_grouped_expression_values, direct_time_root_search_default, direct_time_root_value,
    direct_visible_value, prepare_manifold_projection_programs, root_condition_plan,
    total_root_condition_count, visible_value_plan,
};
use refresh_projection::*;
use support::{
    build_visible_name_index, copy_runtime_values, copy_runtime_values_into,
    fill_inactive_root_output, reserve_runtime_index_map_capacity, reserve_runtime_vec_capacity,
    resize_runtime_values, validate_finite_runtime_output, validate_runtime_output_len,
    visible_value_index_error, zero_runtime_values,
};

/// Backend-neutral callable produced from one checked Solve-IR expression block.
pub trait CompiledSolveExpression {
    fn call(&self, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) -> Result<(), String>;
}

/// Backend-neutral callable for a checked forward-mode Solve-IR expression.
pub trait CompiledSolveJacobianExpression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), String>;
}

/// Backend-neutral callable for a causally ordered set of exact assignments.
/// Program outputs write directly to the flattened solver-Y target list, so
/// subsequent programs observe earlier assignments while multiple outputs of
/// one source program commit together.
pub trait CompiledSolveAssignmentSchedule {
    fn call(&self, y: &mut [f64], p: &[f64], t: f64) -> Result<(), String>;
}

/// Backend-neutral callable for one checked aggregate event transaction.
pub trait CompiledSolveEventTransaction {
    fn call(&self, input: &[f64], output: &mut [f64]) -> Result<(), String>;
}

/// Execution adapter injected by a concrete simulation backend.
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
        execution: &solve::ExactRefreshAssignmentExecution<'_>,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String>;

    fn compile_event_transaction(
        &self,
        program: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String>;
}

#[cfg(test)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum InterpreterExecutionOwner {
    ContinuousRefreshRows,
    RefreshProjectionRows,
    TornSweeps,
    ManifoldResidual,
    ManifoldJacobian,
    InitialProjectionRows,
    DerivativeSensitivity,
    ImplicitSensitivity,
    DiscreteScalarRows,
    StructuredDiscreteRows,
    GuardedAssignments,
    ClockPartitionIntermediates,
    EventActionConditions,
    DynamicTimeEvents,
    InitializationUpdates,
    RuntimeAssignments,
    PostCommitAssignments,
    VisibleValues,
    DelayExpressions,
    PreparationConstantRoots,
}

#[cfg(test)]
impl InterpreterExecutionOwner {
    const fn catalog_ordinal(self) -> usize {
        match self {
            Self::ContinuousRefreshRows => 0,
            Self::RefreshProjectionRows => 1,
            Self::TornSweeps => 2,
            Self::ManifoldResidual => 3,
            Self::ManifoldJacobian => 4,
            Self::InitialProjectionRows => 5,
            Self::DerivativeSensitivity => 6,
            Self::ImplicitSensitivity => 7,
            Self::DiscreteScalarRows => 8,
            Self::StructuredDiscreteRows => 9,
            Self::GuardedAssignments => 10,
            Self::ClockPartitionIntermediates => 11,
            Self::EventActionConditions => 12,
            Self::DynamicTimeEvents => 13,
            Self::InitializationUpdates => 14,
            Self::RuntimeAssignments => 15,
            Self::PostCommitAssignments => 16,
            Self::VisibleValues => 17,
            Self::DelayExpressions => 18,
            Self::PreparationConstantRoots => 19,
        }
    }
}

fn native_execution_owner_catalog_ordinal(owner: &NativeExecutionOwner) -> usize {
    match owner {
        NativeExecutionOwner::ImplicitResidual => 0,
        NativeExecutionOwner::ImplicitProjectionJacobian => 1,
        NativeExecutionOwner::ImplicitFullJacobian => 2,
        NativeExecutionOwner::InitialResidual => 3,
        NativeExecutionOwner::InitialResidualJacobian => 4,
        NativeExecutionOwner::DerivativeRhs => 5,
        NativeExecutionOwner::RootConditions => 6,
        NativeExecutionOwner::EventTransaction { index: _ } => 7,
        NativeExecutionOwner::ExactAssignment { sequence: _ } => 8,
        NativeExecutionOwner::PureCallTable => 9,
    }
}

#[cfg(test)]
const ALL_INTERPRETER_EXECUTION_OWNERS: [InterpreterExecutionOwner; 20] = [
    InterpreterExecutionOwner::ContinuousRefreshRows,
    InterpreterExecutionOwner::RefreshProjectionRows,
    InterpreterExecutionOwner::TornSweeps,
    InterpreterExecutionOwner::ManifoldResidual,
    InterpreterExecutionOwner::ManifoldJacobian,
    InterpreterExecutionOwner::InitialProjectionRows,
    InterpreterExecutionOwner::DerivativeSensitivity,
    InterpreterExecutionOwner::ImplicitSensitivity,
    InterpreterExecutionOwner::DiscreteScalarRows,
    InterpreterExecutionOwner::StructuredDiscreteRows,
    InterpreterExecutionOwner::GuardedAssignments,
    InterpreterExecutionOwner::ClockPartitionIntermediates,
    InterpreterExecutionOwner::EventActionConditions,
    InterpreterExecutionOwner::DynamicTimeEvents,
    InterpreterExecutionOwner::InitializationUpdates,
    InterpreterExecutionOwner::RuntimeAssignments,
    InterpreterExecutionOwner::PostCommitAssignments,
    InterpreterExecutionOwner::VisibleValues,
    InterpreterExecutionOwner::DelayExpressions,
    InterpreterExecutionOwner::PreparationConstantRoots,
];

trait InterpreterPermit: Clone {
    fn row_eval_context<'runtime>(
        &self,
        runtime: &'runtime SolveRuntime,
    ) -> RowEvalContext<'runtime>;

    fn seeded_row_eval_context<'runtime>(
        &self,
        runtime: &'runtime SolveRuntime,
        seed: &'runtime [f64],
    ) -> RowEvalContext<'runtime> {
        let mut context = self.row_eval_context(runtime);
        context.seed = Some(seed);
        context
    }
}

macro_rules! interpreter_permit {
    ($name:ident) => {
        #[derive(Clone, Copy)]
        struct $name {
            _brand: PermitBrand,
        }
        impl InterpreterPermit for $name {
            fn row_eval_context<'runtime>(
                &self,
                runtime: &'runtime SolveRuntime,
            ) -> RowEvalContext<'runtime> {
                RuntimeRowEvalContextPermit::for_runtime(runtime)
            }
        }
    };
}

/// Private construction brand: only `InterpreterExecutionPlan::selected`
/// issues interpreter permits.  Runtime sibling modules may consume a permit,
/// but cannot mint one to reach a raw evaluator context.
#[derive(Clone, Copy)]
struct PermitBrand;

interpreter_permit!(ContinuousRefreshRowsPermit);
interpreter_permit!(RefreshProjectionRowsPermit);
interpreter_permit!(TornSweepsPermit);
interpreter_permit!(ManifoldResidualPermit);
interpreter_permit!(ManifoldJacobianPermit);
interpreter_permit!(InitialProjectionRowsPermit);
interpreter_permit!(DerivativeSensitivityPermit);
interpreter_permit!(ImplicitSensitivityPermit);
interpreter_permit!(DiscreteScalarRowsPermit);
interpreter_permit!(StructuredDiscreteRowsPermit);
interpreter_permit!(GuardedAssignmentsPermit);
interpreter_permit!(ClockPartitionIntermediatesPermit);
interpreter_permit!(EventActionConditionsPermit);
interpreter_permit!(InitializationUpdatesPermit);
interpreter_permit!(RuntimeAssignmentsPermit);
interpreter_permit!(PostCommitAssignmentsPermit);
interpreter_permit!(VisibleValuesPermit);
interpreter_permit!(DelayExpressionsPermit);
interpreter_permit!(ImplicitResidualPermit);
interpreter_permit!(ImplicitProjectionJacobianPermit);
interpreter_permit!(ImplicitFullJacobianPermit);
interpreter_permit!(InitialResidualPermit);
interpreter_permit!(InitialResidualJacobianPermit);
interpreter_permit!(DerivativeRhsPermit);
interpreter_permit!(RootConditionsPermit);
interpreter_permit!(EventTransactionPermit);
interpreter_permit!(ExactAssignmentPermit);
interpreter_permit!(PreparationConstantRootsPermit);

/// The sole owner of a runtime-bound interpreter context. Individual
/// operation permits may only borrow this complete context or construct their
/// own explicitly documented special case below.
struct RuntimeRowEvalContextPermit;

impl RuntimeRowEvalContextPermit {
    fn for_runtime(runtime: &SolveRuntime) -> RowEvalContext<'_> {
        RowEvalContext {
            pure_calls: Some(runtime.model.pure_calls()),
            runtime_state: Some(&runtime.runtime_state),
            ..Default::default()
        }
    }
}

impl PreparationConstantRootsPermit {
    fn row_eval_context_for_model<'model>(
        &self,
        model: &'model solve::SolveModel,
    ) -> RowEvalContext<'model> {
        RowEvalContext {
            pure_calls: Some(model.pure_calls()),
            ..Default::default()
        }
    }
}

#[derive(Clone, Copy)]
pub(super) struct DynamicTimeEventsPermit {
    _brand: PermitBrand,
}

impl InterpreterPermit for DynamicTimeEventsPermit {
    fn row_eval_context<'runtime>(
        &self,
        runtime: &'runtime SolveRuntime,
    ) -> RowEvalContext<'runtime> {
        RuntimeRowEvalContextPermit::for_runtime(runtime)
    }
}

impl DynamicTimeEventsPermit {
    pub(super) fn row_eval_context_for_model<'model>(
        &self,
        model: &'model solve::SolveModel,
        runtime_state: &'model solve_eval::SimulationRuntimeState,
    ) -> RowEvalContext<'model> {
        RowEvalContext {
            pure_calls: Some(model.pure_calls()),
            runtime_state: Some(runtime_state),
            ..Default::default()
        }
    }
}

#[derive(Clone)]
enum ExecutionArm<T, P> {
    Interpreter(P),
    Native(T),
}

type ExactAssignmentExecutionArm =
    ExecutionArm<Rc<dyn CompiledSolveAssignmentSchedule>, ExactAssignmentPermit>;
type ExactAssignmentExecutionCatalog =
    FxHashMap<solve::RefreshSequenceId, ExactAssignmentExecutionArm>;

/// Preparation-issued interpreter owners which cannot yet be specialized
/// without first executing interpreter semantics.  Keeping these as named
/// arms makes their heterogeneous selection explicit instead of treating the
/// lack of a compiled object as an execution decision.
#[derive(Clone)]
struct InterpreterExecutionPlan {
    implicit_residual: ImplicitResidualPermit,
    implicit_projection_jacobian: ImplicitProjectionJacobianPermit,
    implicit_full_jacobian: ImplicitFullJacobianPermit,
    initial_residual: InitialResidualPermit,
    initial_residual_jacobian: InitialResidualJacobianPermit,
    derivative_rhs: DerivativeRhsPermit,
    root_conditions: RootConditionsPermit,
    event_transactions: EventTransactionPermit,
    exact_assignments: ExactAssignmentPermit,
    continuous_refresh_rows: ContinuousRefreshRowsPermit,
    refresh_projection_rows: RefreshProjectionRowsPermit,
    torn_sweeps: TornSweepsPermit,
    manifold_residual: ManifoldResidualPermit,
    manifold_jacobian: ManifoldJacobianPermit,
    initial_projection_rows: InitialProjectionRowsPermit,
    derivative_sensitivity: DerivativeSensitivityPermit,
    implicit_sensitivity: ImplicitSensitivityPermit,
    discrete_scalar_rows: DiscreteScalarRowsPermit,
    structured_discrete_rows: StructuredDiscreteRowsPermit,
    guarded_assignments: GuardedAssignmentsPermit,
    clock_partition_intermediates: ClockPartitionIntermediatesPermit,
    event_action_conditions: EventActionConditionsPermit,
    dynamic_time_events: DynamicTimeEventsPermit,
    initialization_updates: InitializationUpdatesPermit,
    runtime_assignments: RuntimeAssignmentsPermit,
    post_commit_assignments: PostCommitAssignmentsPermit,
    visible_values: VisibleValuesPermit,
    delay_expressions: DelayExpressionsPermit,
    preparation_constant_roots: PreparationConstantRootsPermit,
}

impl InterpreterExecutionPlan {
    fn selected() -> Self {
        Self {
            implicit_residual: ImplicitResidualPermit {
                _brand: PermitBrand,
            },
            implicit_projection_jacobian: ImplicitProjectionJacobianPermit {
                _brand: PermitBrand,
            },
            implicit_full_jacobian: ImplicitFullJacobianPermit {
                _brand: PermitBrand,
            },
            initial_residual: InitialResidualPermit {
                _brand: PermitBrand,
            },
            initial_residual_jacobian: InitialResidualJacobianPermit {
                _brand: PermitBrand,
            },
            derivative_rhs: DerivativeRhsPermit {
                _brand: PermitBrand,
            },
            root_conditions: RootConditionsPermit {
                _brand: PermitBrand,
            },
            event_transactions: EventTransactionPermit {
                _brand: PermitBrand,
            },
            exact_assignments: ExactAssignmentPermit {
                _brand: PermitBrand,
            },
            continuous_refresh_rows: ContinuousRefreshRowsPermit {
                _brand: PermitBrand,
            },
            refresh_projection_rows: RefreshProjectionRowsPermit {
                _brand: PermitBrand,
            },
            torn_sweeps: TornSweepsPermit {
                _brand: PermitBrand,
            },
            manifold_residual: ManifoldResidualPermit {
                _brand: PermitBrand,
            },
            manifold_jacobian: ManifoldJacobianPermit {
                _brand: PermitBrand,
            },
            initial_projection_rows: InitialProjectionRowsPermit {
                _brand: PermitBrand,
            },
            derivative_sensitivity: DerivativeSensitivityPermit {
                _brand: PermitBrand,
            },
            implicit_sensitivity: ImplicitSensitivityPermit {
                _brand: PermitBrand,
            },
            discrete_scalar_rows: DiscreteScalarRowsPermit {
                _brand: PermitBrand,
            },
            structured_discrete_rows: StructuredDiscreteRowsPermit {
                _brand: PermitBrand,
            },
            guarded_assignments: GuardedAssignmentsPermit {
                _brand: PermitBrand,
            },
            clock_partition_intermediates: ClockPartitionIntermediatesPermit {
                _brand: PermitBrand,
            },
            event_action_conditions: EventActionConditionsPermit {
                _brand: PermitBrand,
            },
            dynamic_time_events: DynamicTimeEventsPermit {
                _brand: PermitBrand,
            },
            initialization_updates: InitializationUpdatesPermit {
                _brand: PermitBrand,
            },
            runtime_assignments: RuntimeAssignmentsPermit {
                _brand: PermitBrand,
            },
            post_commit_assignments: PostCommitAssignmentsPermit {
                _brand: PermitBrand,
            },
            visible_values: VisibleValuesPermit {
                _brand: PermitBrand,
            },
            delay_expressions: DelayExpressionsPermit {
                _brand: PermitBrand,
            },
            preparation_constant_roots: PreparationConstantRootsPermit {
                _brand: PermitBrand,
            },
        }
    }

    #[cfg(test)]
    fn dynamic_time_events_for_test() -> DynamicTimeEventsPermit {
        Self::selected().dynamic_time_events
    }

    #[cfg(test)]
    fn owner_inventory(&self) -> [InterpreterExecutionOwner; 20] {
        [
            InterpreterExecutionOwner::ContinuousRefreshRows,
            InterpreterExecutionOwner::RefreshProjectionRows,
            InterpreterExecutionOwner::TornSweeps,
            InterpreterExecutionOwner::ManifoldResidual,
            InterpreterExecutionOwner::ManifoldJacobian,
            InterpreterExecutionOwner::InitialProjectionRows,
            InterpreterExecutionOwner::DerivativeSensitivity,
            InterpreterExecutionOwner::ImplicitSensitivity,
            InterpreterExecutionOwner::DiscreteScalarRows,
            InterpreterExecutionOwner::StructuredDiscreteRows,
            InterpreterExecutionOwner::GuardedAssignments,
            InterpreterExecutionOwner::ClockPartitionIntermediates,
            InterpreterExecutionOwner::EventActionConditions,
            InterpreterExecutionOwner::DynamicTimeEvents,
            InterpreterExecutionOwner::InitializationUpdates,
            InterpreterExecutionOwner::RuntimeAssignments,
            InterpreterExecutionOwner::PostCommitAssignments,
            InterpreterExecutionOwner::VisibleValues,
            InterpreterExecutionOwner::DelayExpressions,
            InterpreterExecutionOwner::PreparationConstantRoots,
        ]
    }
}

#[cfg(test)]
pub(super) fn dynamic_time_events_permit_for_test() -> DynamicTimeEventsPermit {
    InterpreterExecutionPlan::dynamic_time_events_for_test()
}

#[derive(Clone)]
struct RuntimeExecutionPlan {
    implicit_rhs: ExecutionArm<Rc<dyn CompiledSolveExpression>, ImplicitResidualPermit>,
    implicit_projection_jacobian:
        ExecutionArm<Rc<dyn CompiledSolveJacobianExpression>, ImplicitProjectionJacobianPermit>,
    implicit_full_jacobian:
        ExecutionArm<Rc<dyn CompiledSolveJacobianExpression>, ImplicitFullJacobianPermit>,
    initial_residual: ExecutionArm<Rc<dyn CompiledSolveExpression>, InitialResidualPermit>,
    initial_residual_jacobian:
        ExecutionArm<Rc<dyn CompiledSolveJacobianExpression>, InitialResidualJacobianPermit>,
    derivative_rhs: ExecutionArm<Rc<dyn CompiledSolveExpression>, DerivativeRhsPermit>,
    root_conditions: RootConditionExecution,
    event_transactions:
        Vec<ExecutionArm<Rc<dyn CompiledSolveEventTransaction>, EventTransactionPermit>>,
    exact_assignments: ExactAssignmentExecutionCatalog,
    interpreter: InterpreterExecutionPlan,
}

#[derive(Clone)]
enum RootConditionExecution {
    Planned {
        plan: RootConditionPlan,
        interpreter: RootConditionsPermit,
    },
    Direct(ExecutionArm<Rc<dyn CompiledSolveExpression>, RootConditionsPermit>),
}

#[derive(Clone)]
struct PreparedRefreshPlan {
    plan: solve::IssuedRefreshPlan,
    program_rows: Box<[PreparedRefreshProgramRow]>,
    execution: PreparedRefreshExecution,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PreparedRefreshProgramRow(usize);

impl PreparedRefreshProgramRow {
    fn checked(
        source: solve::RefreshScalarProgramSource,
        program_row: usize,
        projected_source: Option<solve::RefreshScalarProgramSource>,
    ) -> Result<Self, RuntimeSolveError> {
        if projected_source != Some(source) {
            return Err(RuntimeSolveError::solve_ir(
                "continuous refresh row cites a foreign final scalar projection",
            ));
        }
        Ok(Self(program_row))
    }

    const fn index(self) -> usize {
        self.0
    }
}

#[derive(Debug)]
struct PreparedRefreshProgramCatalog<'a> {
    projected_sources: &'a [Option<solve::RefreshScalarProgramSource>],
    rows: FxHashMap<solve::RefreshScalarProgramSource, usize>,
}

impl<'a> PreparedRefreshProgramCatalog<'a> {
    fn construct(
        projected_sources: &'a [Option<solve::RefreshScalarProgramSource>],
    ) -> Result<Self, RuntimeSolveError> {
        let mut rows = FxHashMap::default();
        for (program_row, source) in projected_sources.iter().copied().enumerate() {
            let Some(source) = source else {
                continue;
            };
            if rows.insert(source, program_row).is_some() {
                return Err(RuntimeSolveError::solve_ir(
                    "final scalar projection repeats a continuous refresh source identity",
                ));
            }
        }
        Ok(Self {
            projected_sources,
            rows,
        })
    }

    fn bind(
        &self,
        source: solve::RefreshScalarProgramSource,
    ) -> Result<PreparedRefreshProgramRow, RuntimeSolveError> {
        let program_row = self.rows.get(&source).copied().ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "construction-issued refresh row has no final scalar projection",
            )
        })?;
        PreparedRefreshProgramRow::checked(
            source,
            program_row,
            self.projected_sources.get(program_row).copied().flatten(),
        )
    }
}

#[derive(Clone, Copy)]
struct PreparedRefreshRow<'a> {
    row: &'a solve::AlgebraicRefreshRow,
    program_row: PreparedRefreshProgramRow,
}

impl<'a> PreparedRefreshRow<'a> {
    const fn row(self) -> &'a solve::AlgebraicRefreshRow {
        self.row
    }

    const fn program_row(self) -> usize {
        self.program_row.index()
    }
}

impl Deref for PreparedRefreshRow<'_> {
    type Target = solve::AlgebraicRefreshRow;

    fn deref(&self) -> &Self::Target {
        self.row
    }
}

#[derive(Clone, Copy)]
struct PreparedRefreshRows<'a> {
    rows: &'a [solve::AlgebraicRefreshRow],
    program_rows: &'a [PreparedRefreshProgramRow],
    indices: &'a [u32],
}

impl<'a> PreparedRefreshRows<'a> {
    const fn len(self) -> usize {
        self.indices.len()
    }

    const fn is_empty(self) -> bool {
        self.indices.is_empty()
    }

    fn get(self, position: usize) -> Option<PreparedRefreshRow<'a>> {
        let index = usize::try_from(*self.indices.get(position)?).ok()?;
        Some(PreparedRefreshRow {
            row: self.rows.get(index)?,
            program_row: *self.program_rows.get(index)?,
        })
    }

    fn iter(self) -> impl ExactSizeIterator<Item = PreparedRefreshRow<'a>> {
        self.indices.iter().map(|index| {
            let index =
                usize::try_from(*index).expect("checked prepared refresh row index fits usize");
            PreparedRefreshRow {
                row: &self.rows[index],
                program_row: self.program_rows[index],
            }
        })
    }
}

impl PreparedRefreshPlan {
    fn selected_rows<'a>(
        &'a self,
        selection: &'a solve::RefreshRowSelection,
    ) -> PreparedRefreshRows<'a> {
        PreparedRefreshRows {
            rows: self.plan.rows(),
            program_rows: &self.program_rows,
            indices: selection.indices(),
        }
    }

    fn static_causal_rows(&self) -> PreparedRefreshRows<'_> {
        self.selected_rows(self.plan.static_causal_seed_rows())
    }

    fn dynamic_causal_rows(&self) -> PreparedRefreshRows<'_> {
        self.selected_rows(self.plan.dynamic_causal_seed_rows())
    }
}

impl Deref for PreparedRefreshPlan {
    type Target = solve::IssuedRefreshPlan;

    fn deref(&self) -> &Self::Target {
        &self.plan
    }
}

#[derive(Clone)]
enum PreparedRefreshExecution {
    CertifiedCausal,
    CertifiedStages(Box<[PreparedRefreshStage]>),
    FullProjection,
}

#[derive(Clone)]
enum PreparedRefreshStage {
    ExactAssignments {
        static_sequence: solve::RefreshSequenceId,
        dynamic_sequence: solve::RefreshSequenceId,
        static_rows: solve::RefreshRowSelection,
        dynamic_rows: solve::RefreshRowSelection,
    },
    ProjectionBlock {
        block_index: usize,
        plan: solve::AlgebraicProjectionPlan,
    },
}

#[derive(Clone, Copy)]
enum ExecutionRequest<'a> {
    Interpreter,
    Native(&'a dyn SolveExecutionBackend),
}

fn expression_arm<P: InterpreterPermit>(
    request: ExecutionRequest<'_>,
    owner: NativeExecutionOwner,
    interpreter: P,
    block: &solve::ScalarProgramBlock,
) -> Result<ExecutionArm<Rc<dyn CompiledSolveExpression>, P>, RuntimeSolveError> {
    let _owner_ordinal = native_execution_owner_catalog_ordinal(&owner);
    match request {
        ExecutionRequest::Interpreter => Ok(ExecutionArm::Interpreter(interpreter)),
        ExecutionRequest::Native(backend) => backend
            .compile_expression(block)
            .map(ExecutionArm::Native)
            .map_err(|reason| RuntimeSolveError::native_compile(owner, reason)),
    }
}

fn jacobian_arm<P: InterpreterPermit>(
    request: ExecutionRequest<'_>,
    owner: NativeExecutionOwner,
    interpreter: P,
    block: &solve::ScalarProgramBlock,
) -> Result<ExecutionArm<Rc<dyn CompiledSolveJacobianExpression>, P>, RuntimeSolveError> {
    let _owner_ordinal = native_execution_owner_catalog_ordinal(&owner);
    match request {
        ExecutionRequest::Interpreter => Ok(ExecutionArm::Interpreter(interpreter)),
        ExecutionRequest::Native(backend) => backend
            .compile_jacobian_expression(block)
            .map(ExecutionArm::Native)
            .map_err(|reason| RuntimeSolveError::native_compile(owner, reason)),
    }
}

fn exact_assignment_arms(
    request: ExecutionRequest<'_>,
    model: &solve::SolveModel,
    plans: &[&PreparedRefreshPlan],
    interpreter: ExactAssignmentPermit,
) -> Result<ExactAssignmentExecutionCatalog, RuntimeSolveError> {
    let sequences = exact_assignment_sequences(plans);
    let mut arms = FxHashMap::default();
    for sequence in sequences {
        let execution = model.exact_refresh_assignment_execution(sequence);
        let arm = match (request, execution.as_ref()) {
            (ExecutionRequest::Native(backend), Some(execution)) => backend
                .compile_assignment_schedule(execution)
                .map(ExecutionArm::Native)
                .map_err(|reason| {
                    RuntimeSolveError::native_compile(
                        NativeExecutionOwner::ExactAssignment { sequence },
                        reason,
                    )
                })?,
            (ExecutionRequest::Native(_), None) => {
                // A sequence without a construction-issued exact assignment
                // schedule is selected as interpreter-owned at preparation.
                ExecutionArm::Interpreter(interpreter)
            }
            (ExecutionRequest::Interpreter, Some(_)) => ExecutionArm::Interpreter(interpreter),
            (ExecutionRequest::Interpreter, None) => ExecutionArm::Interpreter(interpreter),
        };
        arms.insert(sequence, arm);
    }
    Ok(arms)
}

fn exact_assignment_sequences(
    plans: &[&PreparedRefreshPlan],
) -> BTreeSet<solve::RefreshSequenceId> {
    let mut sequences = BTreeSet::new();
    for prepared in plans {
        collect_exact_assignment_sequences(prepared, &mut sequences);
    }
    sequences
}

fn collect_exact_assignment_sequences(
    prepared: &PreparedRefreshPlan,
    sequences: &mut BTreeSet<solve::RefreshSequenceId>,
) {
    let plan = &prepared.plan;
    match &prepared.execution {
        PreparedRefreshExecution::CertifiedCausal => {
            insert_refresh_sequence(
                !plan.static_causal_rows().is_empty(),
                plan.static_causal_sequence(),
                sequences,
            );
            insert_refresh_sequence(
                !plan.dynamic_causal_rows().is_empty(),
                plan.dynamic_causal_sequence(),
                sequences,
            );
        }
        PreparedRefreshExecution::CertifiedStages(stages) => {
            collect_staged_exact_assignment_sequences(prepared, stages, sequences);
        }
        PreparedRefreshExecution::FullProjection => {}
    }
}

fn collect_staged_exact_assignment_sequences(
    plan: &PreparedRefreshPlan,
    stages: &[PreparedRefreshStage],
    sequences: &mut BTreeSet<solve::RefreshSequenceId>,
) {
    for stage in stages {
        let PreparedRefreshStage::ExactAssignments {
            static_sequence,
            dynamic_sequence,
            static_rows,
            dynamic_rows,
        } = stage
        else {
            continue;
        };
        insert_refresh_sequence(
            !plan.selected_rows(static_rows).is_empty(),
            *static_sequence,
            sequences,
        );
        insert_refresh_sequence(
            !plan.selected_rows(dynamic_rows).is_empty(),
            *dynamic_sequence,
            sequences,
        );
    }
}

fn insert_refresh_sequence(
    has_rows: bool,
    sequence: solve::RefreshSequenceId,
    sequences: &mut BTreeSet<solve::RefreshSequenceId>,
) {
    if has_rows {
        sequences.insert(sequence);
    }
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

impl From<solve_eval::ScalarizeError> for RuntimeSolveError {
    fn from(value: solve_eval::ScalarizeError) -> Self {
        Self::from(EvalSolveError::from(value))
    }
}

fn set_initial_event_flag(model: &solve::SolveModel, p: &mut [f64], value: bool) {
    let Some(index) = model.problem().solve_layout().initial_event_parameter_index else {
        return;
    };
    if let Some(slot) = p.get_mut(index) {
        *slot = f64::from(value);
    }
}

enum SolveRuntimeModelOwner {
    Standalone(Arc<solve::SolveModel>),
    Fmi(solve::fmi::FmiRuntimeView),
}

impl SolveRuntimeModelOwner {
    fn model(&self) -> &solve::SolveModel {
        match self {
            Self::Standalone(model) => model.as_ref(),
            Self::Fmi(runtime) => runtime.model(),
        }
    }
}

impl std::ops::Deref for SolveRuntimeModelOwner {
    type Target = solve::SolveModel;

    fn deref(&self) -> &Self::Target {
        self.model()
    }
}

impl AsRef<solve::SolveModel> for SolveRuntimeModelOwner {
    fn as_ref(&self) -> &solve::SolveModel {
        self.model()
    }
}

pub struct SolveRuntime {
    model: SolveRuntimeModelOwner,
    implicit_rhs: PreparedComputeBlock,
    implicit_projection_jacobian_v: PreparedComputeBlock,
    implicit_projection_scalar_jacobian_v: PreparedScalarProgramBlock,
    implicit_scalar_rhs: PreparedScalarProgramBlock,
    manifold_residual: PreparedComputeBlock,
    manifold_jacobian_v: PreparedComputeBlock,
    initial_residual: PreparedComputeBlock,
    initial_residual_jacobian_v: PreparedComputeBlock,
    initial_scalar_residual: PreparedScalarProgramBlock,
    derivative_rhs: PreparedComputeBlock,
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
    algebraic_refresh: PreparedRefreshPlan,
    derivative_refresh: PreparedRefreshPlan,
    root_refresh: PreparedRefreshPlan,
    event_refresh: PreparedRefreshPlan,
    root_refresh_after_derivative: PreparedRefreshPlan,
    clock_event_refresh_after_event: Vec<PreparedRefreshPlan>,
    /// Certified coverage for the initialization homotopy continuation; the
    /// single source of truth shared by the sweep driver and the acceptance
    /// check in [`InitialContinuationCoverage::certify`].
    initial_continuation: Option<InitialContinuationCoverage>,
    root_condition_rows: PreparedScalarProgramBlock,
    event_action_conditions: PreparedScalarProgramBlock,
    event_action_active_row_indices: RefCell<Vec<usize>>,
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
    pub(crate) output_names: Vec<String>,
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
    runtime_state: solve_eval::SimulationRuntimeState,
    delay_runtime: DelayRuntime,
    root_condition_count: usize,
    derivative_scratch: RefCell<StateDerivativeScratch>,
    root_scratch: RefCell<Vec<f64>>,
    /// Reusable register tape / adjoint buffers for the reverse-mode VJP sweep,
    /// kept across calls so a hot reverse loop stays allocation-free.
    reverse_scratch: RefCell<solve_eval::reverse::ReverseScratch>,
    clock_partition_intermediate_scratch: RefCell<Vec<f64>>,
    event_transaction_input_scratch: RefCell<Vec<f64>>,
    event_transaction_output_scratch: RefCell<Vec<Vec<f64>>>,
    execution_plan: RuntimeExecutionPlan,
    native_assignment_scratch: RefCell<Vec<f64>>,
    clock_activation_cache: RefCell<ClockActivationCache>,
}

/// Opaque mutable state shared by the evaluators behind one ME component.
#[derive(Clone)]
pub(crate) struct SolveRuntimeSnapshot {
    evaluator: solve_eval::SimulationRuntimeStateSnapshot,
    delay: DelayRuntimeSnapshot,
}

mod runtime_impl;

#[cfg(test)]
mod tests;
