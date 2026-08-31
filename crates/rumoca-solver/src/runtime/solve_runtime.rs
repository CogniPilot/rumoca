//! Runtime orchestration for projection, events, and visible outputs.

use indexmap::IndexMap;
use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;
use rustc_hash::FxHashMap;
use std::{cell::RefCell, collections::BTreeSet, ops::Deref, rc::Rc};

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
use refresh_execution::static_refresh_parameter_indices;
use refresh_projection::*;
use support::{
    build_visible_name_index, copy_runtime_values, copy_runtime_values_into,
    fill_inactive_root_output, reserve_runtime_index_map_capacity, reserve_runtime_vec_capacity,
    resize_runtime_values, validate_finite_runtime_output, validate_runtime_output_len,
    visible_value_index_error, zero_runtime_values,
};

/// Backend-neutral callable produced from one checked Solve-IR expression block.
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
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
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
            external_tables: Some(runtime.model.external_tables.as_slice()),
            pure_calls: Some(&runtime.model.pure_calls),
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
            external_tables: Some(model.external_tables.as_slice()),
            pure_calls: Some(&model.pure_calls),
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
            external_tables: Some(model.external_tables.as_slice()),
            pure_calls: Some(&model.pure_calls),
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
    exact_assignments: FxHashMap<
        solve::RefreshSequenceId,
        ExecutionArm<Rc<dyn CompiledSolveAssignmentSchedule>, ExactAssignmentPermit>,
    >,
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
    plan: solve::RefreshPlan,
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
            rows: &self.plan.rows,
            program_rows: &self.program_rows,
            indices: selection.indices(),
        }
    }

    fn static_causal_rows(&self) -> PreparedRefreshRows<'_> {
        self.selected_rows(&self.plan.static_causal_seed_rows)
    }

    fn dynamic_causal_rows(&self) -> PreparedRefreshRows<'_> {
        self.selected_rows(&self.plan.dynamic_causal_seed_rows)
    }
}

impl Deref for PreparedRefreshPlan {
    type Target = solve::RefreshPlan;

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
) -> Result<
    FxHashMap<
        solve::RefreshSequenceId,
        ExecutionArm<Rc<dyn CompiledSolveAssignmentSchedule>, ExactAssignmentPermit>,
    >,
    RuntimeSolveError,
> {
    let owners = &model.problem.continuous().refresh_owners;
    let mut sequences = BTreeSet::new();
    for prepared in plans {
        let plan = &prepared.plan;
        match &prepared.execution {
            PreparedRefreshExecution::CertifiedCausal => {
                if !plan.static_causal_rows().is_empty() {
                    sequences.insert(plan.static_causal_sequence);
                }
                if !plan.dynamic_causal_rows().is_empty() {
                    sequences.insert(plan.dynamic_causal_sequence);
                }
            }
            PreparedRefreshExecution::CertifiedStages(stages) => {
                for stage in stages {
                    match stage {
                        PreparedRefreshStage::ExactAssignments {
                            static_sequence,
                            dynamic_sequence,
                            static_rows,
                            dynamic_rows,
                        } => {
                            if !plan.selected_rows(static_rows).is_empty() {
                                sequences.insert(*static_sequence);
                            }
                            if !plan.selected_rows(dynamic_rows).is_empty() {
                                sequences.insert(*dynamic_sequence);
                            }
                        }
                        PreparedRefreshStage::ProjectionBlock { .. } => {}
                    }
                }
            }
            PreparedRefreshExecution::FullProjection => {}
        }
    }
    let mut arms = FxHashMap::default();
    for sequence in sequences {
        let arm = match (request, owners.exact_assignment_schedule(sequence)) {
            (ExecutionRequest::Native(backend), Some(schedule)) => backend
                .compile_assignment_schedule(
                    &model.problem.continuous().implicit_rhs,
                    owners,
                    schedule,
                )
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
    let Some(index) = model.problem.solve_layout().initial_event_parameter_index else {
        return;
    };
    if let Some(slot) = p.get_mut(index) {
        *slot = f64::from(value);
    }
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
    root_refresh_after_derivative: Option<PreparedRefreshPlan>,
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

impl SolveRuntime {
    pub fn new(model: &solve::SolveModel) -> Result<Self, RuntimeSolveError> {
        Self::prepare(model, ExecutionRequest::Interpreter)
    }

    pub fn new_native(
        model: &solve::SolveModel,
        execution_backend: &dyn SolveExecutionBackend,
    ) -> Result<Self, RuntimeSolveError> {
        Self::prepare(model, ExecutionRequest::Native(execution_backend))
    }

    // SPEC_0021: Exception - construction-issued owner binding stays contiguous for auditability.
    #[allow(clippy::too_many_lines)]
    fn prepare(
        model: &solve::SolveModel,
        execution_request: ExecutionRequest<'_>,
    ) -> Result<Self, RuntimeSolveError> {
        let interpreter_execution = InterpreterExecutionPlan::selected();
        let continuous_structural = model.artifacts.continuous.structural.clone();
        let initialization_structural = model.artifacts.initialization.structural.clone();
        let algebraic_newton_caches = (0..continuous_structural.algebraic_projection().len())
            .map(|_| RefCell::new(crate::runtime::projection::SparseNewtonCache::default()))
            .collect();
        let implicit_scalar_projection =
            solve_eval::to_scalar_program_projection(&model.problem.continuous().implicit_rhs)?;
        let refresh_program_sources = implicit_scalar_projection.sources().to_vec();
        let refresh_program_catalog =
            PreparedRefreshProgramCatalog::construct(&refresh_program_sources)?;
        let implicit_scalar_programs = implicit_scalar_projection.into_block();
        let implicit_rhs_execution = expression_arm(
            execution_request,
            NativeExecutionOwner::ImplicitResidual,
            interpreter_execution.implicit_residual,
            &implicit_scalar_programs,
        )?;
        let implicit_projection_scalar_jacobian =
            to_scalar_program_block(&model.artifacts.continuous.implicit_jacobian_v)?;
        let implicit_projection_jacobian_execution = jacobian_arm(
            execution_request,
            NativeExecutionOwner::ImplicitProjectionJacobian,
            interpreter_execution.implicit_projection_jacobian,
            &implicit_projection_scalar_jacobian,
        )?;
        let implicit_full_jacobian_v = model
            .artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .clone();
        let implicit_full_jacobian_execution = jacobian_arm(
            execution_request,
            NativeExecutionOwner::ImplicitFullJacobian,
            interpreter_execution.implicit_full_jacobian,
            &implicit_full_jacobian_v,
        )?;
        let implicit_scalar_rhs = PreparedScalarProgramBlock::new(implicit_scalar_programs)?;
        let (manifold_residual, manifold_jacobian_v) = prepare_manifold_projection_programs(model)?;
        let derivative_scalar_rhs =
            to_scalar_program_block(&model.problem.continuous().derivative_rhs)?;
        // Scalarization is an evaluator-boundary view of the compact
        // structured map.  Build it once and share it between dependency
        // planning and the prepared runtime adapter.
        let structured_discrete_scalar_rhs =
            to_scalar_program_block(&model.problem.discrete().structured_rhs)?;
        let guarded_assignment_programs = model
            .problem
            .discrete()
            .guarded_assignments
            .iter()
            .map(PreparedGuardedAssignmentProgram::new)
            .collect::<Result<Vec<_>, _>>()?;
        let event_transaction_programs = model
            .problem
            .discrete()
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
            .discrete()
            .observation_refresh
            .iter()
            .enumerate()
            .filter_map(|(row, selected)| selected.then_some(row))
            .collect::<Box<[_]>>();
        let event_transaction_execution = event_transaction_programs
            .iter()
            .enumerate()
            .map(|(index, prepared)| match execution_request {
                ExecutionRequest::Interpreter => Ok(ExecutionArm::Interpreter(
                    interpreter_execution.event_transactions,
                )),
                ExecutionRequest::Native(backend) => backend
                    .compile_event_transaction(prepared.program())
                    .map(ExecutionArm::Native)
                    .map_err(|reason| {
                        RuntimeSolveError::native_compile(
                            NativeExecutionOwner::EventTransaction { index },
                            reason,
                        )
                    }),
            })
            .collect::<Result<Vec<_>, RuntimeSolveError>>()?;
        let derivative_rhs_execution = expression_arm(
            execution_request,
            NativeExecutionOwner::DerivativeRhs,
            interpreter_execution.derivative_rhs,
            &derivative_scalar_rhs,
        )?;
        let refresh_owners = &model.problem.continuous().refresh_owners;
        let algebraic_refresh = refresh_owners.algebraic().clone();
        let derivative_refresh = refresh_owners.derivative().clone();
        let root_refresh = refresh_owners.root().clone();
        let event_refresh = refresh_owners.event().clone();
        let clock_event_refresh = refresh_owners.clock_events().to_vec();
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
            &refresh_program_catalog,
        )?;
        let root_refresh_after_derivative = refresh_owners
            .root_after_derivative()
            .map(solve::RefreshRemainderRelation::remainder)
            .cloned();
        let clock_event_refresh_after_event = refresh_owners
            .clock_events_after_event()
            .iter()
            .map(solve::RefreshRemainderRelation::remainder)
            .cloned()
            .collect::<Vec<_>>();
        trace_reverse_projection_coverage(model, &implicit_scalar_rhs);
        let visible_value_plan = visible_value_plan(model);
        let root_conditions_execution = match root_condition_plan(
            model,
            &root_refresh,
            interpreter_execution.preparation_constant_roots,
        )? {
            Some(plan) => RootConditionExecution::Planned {
                plan,
                interpreter: interpreter_execution.root_conditions,
            },
            None => RootConditionExecution::Direct(expression_arm(
                execution_request,
                NativeExecutionOwner::RootConditions,
                interpreter_execution.root_conditions,
                &model.problem.events().root_conditions,
            )?),
        };
        let (initial_scalar_residual, initial_continuation) =
            InitialContinuationCoverage::certify_runtime_blocks(
                model,
                &implicit_scalar_rhs,
                &algebraic_refresh,
            )?;
        let initial_residual_execution = expression_arm(
            execution_request,
            NativeExecutionOwner::InitialResidual,
            interpreter_execution.initial_residual,
            &initial_scalar_residual,
        )?;
        let initial_scalar_jacobian =
            to_scalar_program_block(&model.artifacts.initialization.residual_jacobian_v)?;
        let initial_residual_jacobian_execution = jacobian_arm(
            execution_request,
            NativeExecutionOwner::InitialResidualJacobian,
            interpreter_execution.initial_residual_jacobian,
            &initial_scalar_jacobian,
        )?;
        let delay_runtime = DelayRuntime::new(&model.problem.events().delays)?;
        let root_condition_count =
            total_root_condition_count(model, delay_runtime.event_root_count())?;
        let structured_discrete_rows =
            PreparedStructuredDiscreteRows::new(model, structured_discrete_scalar_rhs)?;
        let clock_partition_structured_rows = discrete_rows::clock_partition_structured_rows(
            model.problem.discrete().structured_updates.len(),
            structured_discrete_rows.rows(),
        );
        let clock_partition_clocks =
            discrete_rows::clock_partition_clocks(&model.problem.discrete());
        let algebraic_refresh = prepare_refresh_plan(
            algebraic_refresh,
            &continuous_structural,
            &refresh_program_catalog,
        )?;
        let derivative_refresh = prepare_refresh_plan(
            derivative_refresh,
            &continuous_structural,
            &refresh_program_catalog,
        )?;
        let root_refresh = prepare_refresh_plan(
            root_refresh,
            &continuous_structural,
            &refresh_program_catalog,
        )?;
        let event_refresh = prepare_refresh_plan(
            event_refresh,
            &continuous_structural,
            &refresh_program_catalog,
        )?;
        let root_refresh_after_derivative = root_refresh_after_derivative
            .map(|plan| {
                prepare_refresh_plan(plan, &continuous_structural, &refresh_program_catalog)
            })
            .transpose()?;
        let clock_event_refresh_after_event = clock_event_refresh_after_event
            .into_iter()
            .map(|plan| {
                prepare_refresh_plan(plan, &continuous_structural, &refresh_program_catalog)
            })
            .collect::<Result<Vec<_>, RuntimeSolveError>>()?;
        drop(refresh_program_catalog);
        drop(refresh_program_sources);
        let mut executable_refreshes = vec![
            &algebraic_refresh,
            &derivative_refresh,
            &root_refresh,
            &event_refresh,
        ];
        executable_refreshes.extend(root_refresh_after_derivative.iter());
        executable_refreshes.extend(clock_event_refresh_after_event.iter());
        let exact_assignments = exact_assignment_arms(
            execution_request,
            model,
            &executable_refreshes,
            interpreter_execution.exact_assignments,
        )?;
        let execution_plan = RuntimeExecutionPlan {
            implicit_rhs: implicit_rhs_execution,
            implicit_projection_jacobian: implicit_projection_jacobian_execution,
            implicit_full_jacobian: implicit_full_jacobian_execution,
            initial_residual: initial_residual_execution,
            initial_residual_jacobian: initial_residual_jacobian_execution,
            derivative_rhs: derivative_rhs_execution,
            root_conditions: root_conditions_execution,
            event_transactions: event_transaction_execution,
            exact_assignments,
            interpreter: interpreter_execution,
        };
        Ok(Self {
            model: model.clone(),
            state_count: model.state_scalar_count(),
            solver_count: model.solver_scalar_count(),
            implicit_rhs: PreparedComputeBlock::new_with_label(
                &model.problem.continuous().implicit_rhs,
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
                &model.problem.initialization().residual,
                "runtime_initial_residual",
            )?,
            initial_residual_jacobian_v: PreparedComputeBlock::new_with_label(
                &model.artifacts.initialization.residual_jacobian_v,
                "runtime_initial_residual_jacobian_v",
            )?,
            initial_scalar_residual: PreparedScalarProgramBlock::new(initial_scalar_residual)?,
            derivative_rhs: PreparedComputeBlock::new_with_label(
                &model.problem.continuous().derivative_rhs,
                "runtime_derivative_rhs",
            )?,
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
                model.problem.events().root_conditions.clone(),
            )?,
            event_action_conditions: PreparedScalarProgramBlock::new(
                model.problem.events().action_conditions.clone(),
            )?,
            event_action_active_row_indices: RefCell::new(Vec::new()),
            discrete_rhs: PreparedScalarProgramBlock::new(model.problem.discrete().rhs.clone())?,
            observation_refresh_scalar_rows,
            observation_refresh_p_scratch: RefCell::new(Vec::new()),
            observation_refresh_values_scratch: RefCell::new(Vec::new()),
            clock_partition_intermediates: PreparedScalarProgramBlock::new(
                model
                    .problem
                    .discrete()
                    .clock_partition_intermediates
                    .clone(),
            )?,
            clock_partition_clocks,
            clock_partition_structured_rows,
            clock_partition_work_y: RefCell::new(Vec::new()),
            clock_partition_work_p: RefCell::new(Vec::new()),
            guarded_assignment_programs,
            event_transaction_programs,
            event_transaction_coverage,
            runtime_assignment_rhs: PreparedScalarProgramBlock::new(
                model.problem.discrete().runtime_assignment_rhs.clone(),
            )?,
            post_commit_assignment_rhs: PreparedScalarProgramBlock::new(
                model.problem.discrete().post_commit_assignment_rhs.clone(),
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
            torn_sweep_cache: TornSweepCache::default(),
            runtime_state: solve_eval::SimulationRuntimeState::new(),
            delay_runtime,
            root_condition_count,
            derivative_scratch: RefCell::new(StateDerivativeScratch::default()),
            root_scratch: RefCell::new(Vec::new()),
            reverse_scratch: RefCell::new(solve_eval::reverse::ReverseScratch::default()),
            clock_partition_intermediate_scratch: RefCell::new(Vec::new()),
            event_transaction_input_scratch: RefCell::new(Vec::new()),
            event_transaction_output_scratch: RefCell::new(event_transaction_output_scratch),
            execution_plan,
            native_assignment_scratch: RefCell::new(Vec::new()),
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
            .discrete()
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
        let schedules = &self.model.problem.clocks().periodic_event_schedules;
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
        self.discrete_rhs.eval_row_outputs_unchecked_with_context(
            program,
            y,
            p,
            t,
            self.execution_plan
                .interpreter
                .discrete_scalar_rows
                .row_eval_context(self),
            out,
        )?;
        self.report_nonfinite_discrete_program_outputs(program, t, out);
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
            let row = (0..self.model.problem.discrete().update_targets.len())
                .find(|&row| self.discrete_rhs.row_output_position(row) == Some((program, offset)));
            let target = row.and_then(|row| {
                self.model
                    .problem
                    .discrete()
                    .update_targets
                    .get(row)
                    .copied()
            });
            let name = target.and_then(|target| {
                self.model
                    .problem
                    .layout()
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

    #[cfg(test)]
    fn root_condition_plan_for_test(&self) -> Option<&RootConditionPlan> {
        match &self.execution_plan.root_conditions {
            RootConditionExecution::Planned { plan, .. } => Some(plan),
            RootConditionExecution::Direct(_) => None,
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
            .initialize(
                time,
                solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
            )
            .map_err(Into::into)
    }

    pub fn refresh_delay_values(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.delay_runtime
            .refresh(
                time,
                solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
            )
            .map_err(Into::into)
    }

    pub fn commit_delay_history(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .commit(
                time,
                solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
            )
            .map_err(Into::into)
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
            AlgebraicSettle { tol, max_iters },
            &mut derivative,
        )?;
        Ok(derivative)
    }

    /// The public runtime API mirrors the solver callback inputs: it never
    /// hides the caller's scratch or output buffers behind an allocation. The
    /// convergence controls travel as the same `AlgebraicSettle` the
    /// linearization entry points already carry.
    pub fn eval_state_derivatives_with_guess_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        settle: AlgebraicSettle,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.refresh_derivative_dependencies(t, guess, params, settle.tol, settle.max_iters)?;
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
        let model_root_count = self.model.problem.events().root_conditions.len();
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
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
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

    /// The public runtime API mirrors the solver callback inputs: it never
    /// hides the certified warm start or the output buffer behind an
    /// allocation. The convergence controls travel as the same
    /// `AlgebraicSettle` the linearization entry points already carry.
    pub fn eval_root_search_conditions_with_guess_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        settle: AlgebraicSettle,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.eval_root_search_conditions_at_solver_y(
            t,
            params,
            settle.tol,
            settle.max_iters,
            out,
            guess,
        )
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
        let model_root_count = self.model.problem.events().root_conditions.len();
        let model_roots_need_refresh = model_root_count > 0
            && match &self.execution_plan.root_conditions {
                RootConditionExecution::Planned { plan, .. } => !plan.search_rows.is_empty(),
                RootConditionExecution::Direct(_) => true,
            };
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
            match &self.execution_plan.root_conditions {
                RootConditionExecution::Planned { plan, interpreter } => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    if plan.search_rows.is_empty() {
                        self.write_planned_root_search_defaults(plan, params, t, model_out)?;
                    } else {
                        self.write_planned_root_search_conditions(
                            plan,
                            interpreter,
                            solver_y,
                            params,
                            t,
                            model_out,
                        )?;
                    }
                }
                RootConditionExecution::Direct(_) => self
                    .eval_root_conditions_from_refreshed_solver_y(t, solver_y, params, model_out)?,
            }
        }
        self.delay_runtime
            .evaluate_event_roots(
                t,
                solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
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
        let remainder = self.root_refresh_after_derivative.as_ref().ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "root refresh derivative-settled remainder disappeared".to_string(),
            )
        })?;
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
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root search output", root_count, out.len())?;
        let model_root_count = self.model.problem.events().root_conditions.len();
        if model_root_count > 0 {
            let model_out = &mut out[..model_root_count];
            match &self.execution_plan.root_conditions {
                RootConditionExecution::Planned { plan, .. } if plan.search_rows.is_empty() => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    self.write_planned_root_search_defaults(plan, params, t, model_out)?;
                }
                RootConditionExecution::Planned { plan, interpreter } => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    self.write_planned_root_search_conditions(
                        plan,
                        interpreter,
                        solver_y,
                        params,
                        t,
                        model_out,
                    )?;
                }
                RootConditionExecution::Direct(_) => self
                    .eval_root_conditions_from_refreshed_solver_y(t, solver_y, params, model_out)?,
            }
        }
        self.delay_runtime
            .evaluate_event_roots(
                t,
                solver_y,
                params,
                self.execution_plan
                    .interpreter
                    .delay_expressions
                    .row_eval_context(self),
                &mut out[model_root_count..],
            )
            .map_err(RuntimeSolveError::from)?;
        validate_finite_runtime_output("root search output", out)
    }

    fn eval_root_conditions_from_refreshed_solver_y(
        &self,
        t: f64,
        y: &[f64],
        p: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match &self.execution_plan.root_conditions {
            RootConditionExecution::Planned { plan, interpreter } => {
                self.write_planned_root_conditions(plan, interpreter, y, p, t, out)?
            }
            RootConditionExecution::Direct(ExecutionArm::Native(compiled)) => compiled
                .call(y, p, t, self.model.external_tables.as_slice(), out)
                .map_err(|reason| {
                    RuntimeSolveError::native_call(NativeExecutionOwner::RootConditions, reason)
                })?,
            RootConditionExecution::Direct(ExecutionArm::Interpreter(selected_arm)) => self
                .root_condition_rows
                .eval_with_context(y, p, t, selected_arm.row_eval_context(self), out)?,
        }
        validate_finite_runtime_output("root condition output", out)
    }

    fn write_planned_root_conditions(
        &self,
        plan: &RootConditionPlan,
        selected_arm: &RootConditionsPermit,
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
        self.eval_planned_root_rows(selected_arm, &plan.evaluated_rows, y, params, t, out)
    }

    fn write_planned_root_search_conditions(
        &self,
        plan: &RootConditionPlan,
        selected_arm: &RootConditionsPermit,
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.write_planned_root_search_defaults(plan, params, t, out)?;
        self.eval_planned_root_rows(selected_arm, &plan.search_rows, y, params, t, out)
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
        selected_arm: &RootConditionsPermit,
        output_indices: &[usize],
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if output_indices.is_empty() {
            return Ok(());
        }
        self.eval_selected_outputs(
            selected_arm,
            SelectedRows {
                block: &self.root_condition_rows,
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

#[cfg(test)]
mod tests;
