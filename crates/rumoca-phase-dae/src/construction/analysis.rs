mod clocks;
mod comprehensions;
mod delays;
mod derivatives;
mod derived_parameters;
mod discrete_values;
mod equation_partitions;
mod event_conditions;
mod expression_events;
mod expression_semi_linear;
mod expression_validation;
mod function_array_assemblies;
mod function_bodies;
mod function_conditionals;
mod function_definitions;
mod function_externals;
mod function_impurity;
mod function_loops;
mod function_ranges;
mod function_record_assemblies;
mod function_reductions;
mod function_returns;
pub(in crate::construction) mod function_statement_products;
mod function_value_types;
mod history_operators;
mod initial_algorithms;
mod loop_compaction;
mod model_algorithm_calls;
mod model_algorithm_statements;
mod model_algorithms;
mod model_expression_owners;
mod model_roles;
mod multi_output_equations;
pub(in crate::construction) mod record_array_fields;
mod record_equations;
mod reference_identity;
mod sample_aliases;
mod source_balance;
mod structured_families;
mod unexecuted_branches;
mod when_chains;

use super::*;
use crate::balance::BalanceDetail;
use clocks::SampledTarget;
#[cfg(test)]
pub(super) use clocks::expression_preorder_ordinal;
use clocks::{ClockAnalysis, ClockDomainAnalysis, analyze_clocks};
pub(super) use clocks::{
    ClockOwnerId, ClockPlan, ClockTransferPlans, ClockedValuePlan, WhenOccurrenceId,
    is_inferred_clock_condition, is_whole_clock_coordinate,
};
use comprehensions::analyze_comprehensions;
pub(super) use comprehensions::{
    ComprehensionKey, ComprehensionPlans, specialized_comprehension_plan,
};
pub(super) use delays::DelayPlan;
use delays::analyze_delays;
pub(in crate::construction) use derivatives::DerivativePlans;
use derivatives::{
    DerivativeCandidates, StateTargets, admit_derivative_roles, analyze_derivatives,
};
pub(super) use derived_parameters::DerivedParameterPlan;
use derived_parameters::{DerivedParameterAnalysis, analyze_derived_parameters};
pub(super) use discrete_values::DiscreteValueTopologyPlan;
use discrete_values::analyze_discrete_value_topology;
pub(super) use equation_partitions::{
    AggregateDiscreteConnections, DiscreteValueAssignmentPlan, EquationPartition, ModelEquationRow,
    ModelEquationSequence, discrete_value_assignment, structured_discrete_assignments,
    structured_discrete_element_assignments,
};
use equation_partitions::{
    aggregate_discrete_connections, defined_discrete_targets, discrete_connection_ranks,
};
use event_conditions::{
    evaluate_sample_schedule, validate_algorithm_condition, validate_condition_expression,
    validate_when_activation_condition, validate_when_condition_expression,
};
pub(super) use expression_events::{
    AlgorithmConditionProduct, AlgorithmRelationOwner, DynamicTimeEventOperand,
    ExpressionEventPlan, ExpressionEventPlans,
};
use expression_events::{analyze_expression_event_ownership, issue_algorithm_condition};
use expression_semi_linear::analyze_semi_linear_rules;
pub(super) use expression_semi_linear::{SemiLinearRowFilter, SemiLinearRules};
use expression_validation::{
    PreContext, validate_expression, validate_expression_in_context_with_literals,
    validate_expression_scoped_with_record_array_fields, validate_model_algorithm_range,
    validate_model_expression_with_record_array_fields, validate_specialized_expression,
    validate_specialized_subscripts, validate_subscripts_scoped, validate_when_expression,
    when_body_context,
};
use function_array_assemblies::coalesce_function_array_assemblies;
use function_bodies::{
    plan_function_statements, resolve_function_definitions,
    validate_function_expression_with_roles, validate_function_statements,
    validate_function_subscripts, validate_functions,
};
pub(super) use function_conditionals::selected_conditional_statements;
use function_conditionals::{plan_function_conditional, resolve_function_conditional};
use function_definitions::FunctionDefinitions;
pub(super) use function_definitions::FunctionValueSeed;
use function_externals::validate_external_function;
pub(super) use function_externals::{ExternalArgumentPlan, ExternalFunctionPlan};
use function_impurity::validate_impure_call_contexts;
pub(super) use function_loops::flattened_function_loop_source;
use function_loops::{subscript_is_binder, validate_function_loop};
pub(super) use function_ranges::assigned_function_targets;
use function_ranges::static_shape_integer_expression;
use function_ranges::{
    immutable_integer_defaults, static_function_range, validate_function_range_expression,
};
use function_record_assemblies::{
    function_value_def_id, plan_staged_record_assemblies, record_constructor,
    require_constructor_field, resolved_constructor_fields, resolved_record_value,
    validate_record_output_assembly,
};
use function_reductions::validate_integer_reduction;
use function_returns::{
    certify_nonleading_return_branches, nonreturn_path, normalize_function_returns,
    validate_guarded_function_return,
};
pub(super) use function_statement_products::{
    FunctionArrayAssemblyPlan, FunctionIntegerReduction, FunctionPlan, FunctionStatementProduct,
    FunctionStatementSequence,
};
use function_statement_products::{
    function_statement_product_error, issue_function_statement_sequence,
};
pub(super) use function_value_types::record_field_projections;
use function_value_types::validate_function_value_type;
pub(super) use history_operators::HistoryOperatorPlans;
use history_operators::analyze_history_operators;
pub(super) use initial_algorithms::InitialDiscreteValue;
use initial_algorithms::{
    InitialAlgorithmAnalysis, analyze_initial_algorithms, claim_initial_discrete_equations,
    reject_unsupported_initial_algorithm_statements,
};
use loop_compaction::compact_function_loops;
use model_algorithm_calls::ModelEventCallAnalysis;
pub(super) use model_algorithm_calls::{ModelEventFunctionCallPlan, ModelEventFunctionOutputPlan};
use model_algorithm_statements::validate_model_algorithm;
use model_algorithms::analyze_event_algorithms;
pub(super) use model_algorithms::{
    EventAssignmentRoute, EventBlockPlan, EventElseProduct, EventLoweringProduct,
    EventStatementPlan, ModelAlgorithmPlan, ModelAlgorithmSequence, ModelEventTensorLoopPlan,
};
pub(super) use model_algorithms::{
    algorithm_targets, event_targets, is_event_condition, model_algorithm_targets,
    when_chain_targets,
};
use model_expression_owners::ModelExpressionOwnerVisitor;
use model_roles::{
    ModelRoles, analyze_model_roles, apply_clocked_partition_roles, is_predefined_clock_variable,
};
pub(in crate::construction) use multi_output_equations::MultiOutputEquationPlan;
use multi_output_equations::analyze_multi_output_equations;
pub(super) use record_array_fields::{RecordArrayFieldPlan, RecordArrayFieldPlans};
use record_array_fields::{
    analyze_record_array_fields, reference_declarations,
    validate_record_array_field_runtime_coordinates,
};
use record_equations::{
    analyze_record_equations, reject_initial_record_equations, reject_record_family_rows,
    reject_structured_record_equations,
};
use sample_aliases::analyze_sample_aliases;
use source_balance::{SourceBalanceInput, source_balance};
use structured_families::{
    StructuredEquationOwners, analyze_structured_equation_owners, validate_structured_templates,
};
use unexecuted_branches::{check_function_assignment_shapes, check_unexecuted_branches};
use when_chains::validate_when_chains;

pub(super) struct Analysis<'flat> {
    pub(super) constants: EvalContext,
    pub(super) delay_plans: HashMap<Span, DelayPlan>,
    /// Exact analysis certificates for every admitted MLS `der(...)` occurrence.
    pub(super) derivatives: DerivativePlans,
    /// Exact analysis certificates for MLS §3.7.5 `edge`/`change` occurrences.
    pub(super) history_operators: HistoryOperatorPlans,
    pub(super) roles: HashMap<VarName, PlannedRole>,
    pub(super) balance: BalanceDetail,
    pub(super) structured_equation_owners: StructuredEquationOwners<'flat>,
    /// Scalar initial-equation rows represented by typed initial discrete-value
    /// definitions rather than numeric initialization residuals.
    pub(super) initial_discrete_equation_rows: HashSet<usize>,
    pub(super) sample_lattices: Vec<(Span, PeriodicClockSchedule)>,
    pub(super) expression_events: ExpressionEventPlans,
    /// Exact scalar Boolean aliases of MLS §3.7.5 `sample(start, interval)`.
    ///
    /// Conditions consume this proof so a source `when sample_alias` retains
    /// the typed periodic clock instead of buffering a held B.1c coordinate.
    pub(super) sample_alias_schedules: HashMap<VarName, PeriodicClockSchedule>,
    pub(super) clock_plans: HashMap<InstanceId, ClockPlan>,
    pub(super) clock_equation_rows: HashSet<usize>,
    pub(super) clocked_equation_owners: HashMap<usize, ClockPlan>,
    pub(super) clocked_value_owners: HashMap<InstanceId, ClockedValuePlan>,
    /// Owning clock of every `when Clock()` branch, keyed by the branch span.
    pub(super) clocked_when_owners: HashMap<WhenOccurrenceId, ClockPlan>,
    /// Owning clock of every runtime coordinate in a clocked partition.
    pub(super) clocked_coordinate_owners: HashMap<InstanceId, ClockPlan>,
    pub(super) clock_transfer_plans: ClockTransferPlans,
    /// `fixed = false` parameters an initial algorithm determines (MLS §8.6).
    pub(super) initial_parameters: HashMap<VarName, Expression>,
    /// Discrete coordinates whose initialization-instant value an initial
    /// algorithm determines (MLS §8.6).
    pub(super) initial_discrete_values: HashMap<VarName, InitialDiscreteValue>,
    /// Assertions an initial algorithm owns, with enclosing guards folded in.
    pub(super) initial_algorithm_assertions: Vec<flat::AssertEquation>,
    pub(super) function_plans: HashMap<FunctionSpecializationKey, FunctionPlan>,
    pub(super) function_shapes: FunctionShapeAnalysis,
    pub(super) comprehension_plans: ComprehensionPlans,
    pub(super) record_array_fields: Arc<RecordArrayFieldPlans>,
    pub(super) derived_parameters: HashMap<VarName, DerivedParameterPlan>,
    pub(super) derived_parameter_families: HashSet<usize>,
    pub(super) derived_parameter_rows: HashSet<usize>,
    pub(super) record_equations: HashMap<usize, RecordEquationPlan>,
    /// Continuous MLS §12.4.3 tuple equations lowered by result ordinal.
    pub(super) multi_output_equations: HashMap<usize, MultiOutputEquationPlan>,
    /// Initial MLS §12.4.3 tuple equations lowered by result ordinal.
    pub(super) initial_multi_output_equations: HashMap<usize, MultiOutputEquationPlan>,
    pub(super) discrete_value_topology: DiscreteValueTopologyPlan,
    pub(super) assigned_discrete_targets: HashSet<VarName>,
    /// MLS §3.7.4.5 Rule 1 / Rule 2 replacement residuals, keyed by the model
    /// equation row they replace. Empty until
    /// [`Analysis::with_semi_linear_rules`] proves them.
    pub(super) semi_linear_rules: SemiLinearRules,
}

pub(super) struct AnalyzedModel<'flat> {
    pub(super) analysis: Analysis<'flat>,
    pub(super) model_algorithms: ModelAlgorithmSequence<'flat>,
    /// The sole source-ordered role assignment for ordinary model equations.
    pub(super) model_equations: ModelEquationSequence<'flat>,
}

struct SourceBalanceAnalysis {
    detail: BalanceDetail,
    assigned_discrete_targets: HashSet<VarName>,
}

enum FunctionStatementPlan {
    Assignment(FunctionAssignmentPlan),
    /// An MLS §8.3.7 assertion whose condition this exact value-proven
    /// specialization establishes as `true`.
    ///
    /// The plan is the proof that construction may erase the flow action. An
    /// unsettled assertion is never represented by this variant: it needs a
    /// call-scoped runtime owner.
    ProvenAssertion,
    /// A default-level MLS §8.3.7 assertion owned by the top-level function
    /// statement sequence and evaluated once for every call.
    RuntimeAssertion,
    /// One compiler-owned immutable Boolean that captures a return predicate
    /// at its source statement before later mutable values can change.
    GeneratedBooleanAssignment {
        target: VarName,
        value: Expression,
        span: Span,
    },
    For {
        domain: StructuredIndexDomain,
        binder_spans: Vec<Span>,
        lowering: FunctionLoopLowering,
        statements: Vec<FunctionStatementPlan>,
        source_depth: usize,
    },
    If {
        branches: Vec<Vec<FunctionStatementPlan>>,
        fallback: Option<Vec<FunctionStatementPlan>>,
        targets: Vec<FunctionConditionalTarget>,
    },
    /// An MLS §11.5 conditional whose executed branch this specialization
    /// proves, planned as the unconditional statement sequence it denotes.
    ///
    /// `selected` names the condition branch that holds, or `None` for the else
    /// part; `statements` is the plan of exactly those statements.
    ProvenBranch {
        selected: Option<usize>,
        statements: Vec<FunctionStatementPlan>,
    },
    /// MLS §11.2.1.1 assignment from a call with multiple results.
    ///
    /// One entry per *receiving slot* written at the call site, in source
    /// order, so entry `i` names the target of result ordinal `i`. `None` is an
    /// omitted receiver — the `(out1, , out3)` spelling — whose result the DAE
    /// never reads and therefore never lowers.
    MultiOutputCall {
        outputs: Vec<Option<FunctionAssignmentPlan>>,
    },
    /// A pure multi-result call whose receiving list defines every field of
    /// one record-valued function output or local.
    RecordMultiOutputAssembly(FunctionRecordCallAssemblyPlan),
    ArrayAssembly(AnalyzedFunctionArrayAssemblyPlan),
    ArrayAssemblyMember,
    RecordAssembly(FunctionRecordAssemblyPlan),
    RecordAssemblyMember,
    /// One field of a record result assembled at its source position and
    /// stored independently until every field has a checked value.
    RecordFieldAssembly(FunctionRecordFieldAssemblyPlan),
    RecordFieldAssemblyMember,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) struct FunctionConditionalTarget {
    pub(super) name: VarName,
    pub(super) target_def_id: rumoca_core::DefId,
    pub(super) record_field: Option<FunctionRecordFieldIdentity>,
}

pub(super) struct FunctionAssignmentPlan {
    target: VarName,
    target_def_id: rumoca_core::DefId,
    record_field: Option<FunctionRecordFieldIdentity>,
    record_root_name: VarName,
    record_field_name: Option<VarName>,
    subscripts: Box<[Subscript]>,
    /// Aggregate seed this element write starts from, proven dead by the
    /// definedness certificate that every declared element is written.
    seed: Option<FunctionValueSeed>,
}

impl FunctionAssignmentPlan {
    pub(super) fn target(&self) -> &VarName {
        &self.target
    }

    pub(super) fn target_def_id(&self) -> rumoca_core::DefId {
        self.target_def_id
    }

    pub(super) fn subscripts(&self) -> &[Subscript] {
        &self.subscripts
    }

    pub(super) fn record_field(&self) -> Option<FunctionRecordFieldIdentity> {
        self.record_field
    }

    pub(super) fn resolved_record_field(
        &self,
    ) -> Option<(&VarName, FunctionRecordFieldIdentity, &VarName)> {
        Some((
            &self.record_root_name,
            self.record_field?,
            self.record_field_name.as_ref()?,
        ))
    }

    pub(super) fn is_whole(&self) -> bool {
        self.subscripts.is_empty()
    }

    pub(super) fn seed(&self) -> Option<&FunctionValueSeed> {
        self.seed.as_ref()
    }
}

/// Advance the exact staged-field reaching definitions across one checked plan.
///
/// Registered staging locals are storage only. This source-point state is the
/// capability proving which of them contains the current semantic field value.
fn advance_function_record_staging(
    plan: &FunctionStatementPlan,
    available: &mut HashSet<FunctionRecordFieldIdentity>,
) {
    match plan {
        FunctionStatementPlan::Assignment(assignment) => {
            advance_assignment_record_staging(assignment, available);
        }
        FunctionStatementPlan::MultiOutputCall { outputs } => {
            for assignment in outputs.iter().flatten() {
                advance_assignment_record_staging(assignment, available);
            }
        }
        FunctionStatementPlan::RecordMultiOutputAssembly(assembly) => {
            available.retain(|identity| identity.target != assembly.target_def_id);
        }
        FunctionStatementPlan::RecordAssembly(assembly) => {
            available.retain(|identity| identity.target != assembly.target_def_id);
        }
        FunctionStatementPlan::RecordFieldAssembly(assembly) => {
            let identity = FunctionRecordFieldIdentity {
                target: assembly.target_def_id,
                field: assembly.field.def_id,
            };
            if assembly.finalize_fields.is_some() {
                available.retain(|field| field.target != identity.target);
            } else {
                available.insert(identity);
            }
        }
        FunctionStatementPlan::If {
            branches, fallback, ..
        } => {
            let mut paths = branches
                .iter()
                .map(|branch| record_staging_after(branch, available))
                .collect::<Vec<_>>();
            paths.push(match fallback {
                Some(fallback) => record_staging_after(fallback, available),
                None => available.clone(),
            });
            intersect_record_staging_paths(available, &paths);
        }
        FunctionStatementPlan::ProvenBranch { statements, .. } => {
            for statement in statements {
                advance_function_record_staging(statement, available);
            }
        }
        FunctionStatementPlan::For { statements, .. } => {
            let body = record_staging_after(statements, available);
            available.retain(|identity| body.contains(identity));
        }
        FunctionStatementPlan::ProvenAssertion
        | FunctionStatementPlan::RuntimeAssertion
        | FunctionStatementPlan::GeneratedBooleanAssignment { .. }
        | FunctionStatementPlan::ArrayAssembly(_)
        | FunctionStatementPlan::ArrayAssemblyMember
        | FunctionStatementPlan::RecordAssemblyMember
        | FunctionStatementPlan::RecordFieldAssemblyMember => {}
    }
}

fn advance_assignment_record_staging(
    assignment: &FunctionAssignmentPlan,
    available: &mut HashSet<FunctionRecordFieldIdentity>,
) {
    match assignment.record_field() {
        Some(identity) if assignment.is_whole() => {
            available.insert(identity);
        }
        Some(_) => {}
        None => {
            available.retain(|identity| identity.target != assignment.target_def_id());
        }
    }
}

fn record_staging_after(
    plans: &[FunctionStatementPlan],
    incoming: &HashSet<FunctionRecordFieldIdentity>,
) -> HashSet<FunctionRecordFieldIdentity> {
    let mut available = incoming.clone();
    for plan in plans {
        advance_function_record_staging(plan, &mut available);
    }
    available
}

fn intersect_record_staging_paths(
    available: &mut HashSet<FunctionRecordFieldIdentity>,
    paths: &[HashSet<FunctionRecordFieldIdentity>],
) {
    let Some(first) = paths.first() else {
        available.clear();
        return;
    };
    *available = first
        .iter()
        .copied()
        .filter(|identity| paths[1..].iter().all(|path| path.contains(identity)))
        .collect();
}

struct AnalyzedFunctionArrayAssemblyPlan {
    target: VarName,
    target_def_id: rumoca_core::DefId,
    direct_members: Vec<AnalyzedFunctionArrayDirectMember>,
    extent: usize,
    suffix_index: Option<rumoca_core::ForIndex>,
    loop_plan: Option<Box<FunctionStatementPlan>>,
    seed: Option<FunctionValueSeed>,
}

struct AnalyzedFunctionArrayDirectMember {
    subscripts: Box<[Subscript]>,
    one_based_index: i64,
}

pub(super) struct FunctionRecordAssemblyPlan {
    pub(super) target: VarName,
    pub(super) target_def_id: rumoca_core::DefId,
    pub(super) statement_count: usize,
    pub(super) fields: Vec<FunctionRecordFieldAssembly>,
    pub(super) seed: Option<FunctionValueSeed>,
}

pub(super) struct FunctionRecordFieldAssemblyPlan {
    pub(super) target: VarName,
    pub(super) target_def_id: rumoca_core::DefId,
    pub(super) statement_count: usize,
    pub(super) field: FunctionRecordFieldAssembly,
    /// Earlier field definitions this field's expressions may read directly.
    pub(super) available_fields: Vec<ResolvedFunctionRecordField>,
    /// Constructor-order fields when this field completes the record.
    pub(super) finalize_fields: Option<Vec<ResolvedFunctionRecordField>>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(super) struct FunctionRecordFieldIdentity {
    pub(super) target: rumoca_core::DefId,
    pub(super) field: rumoca_core::DefId,
}

#[derive(Clone, Debug)]
pub(super) struct ResolvedFunctionRecordField {
    pub(super) name: VarName,
    pub(super) def_id: rumoca_core::DefId,
}

pub(super) fn function_record_field_name(target: &VarName, field: &VarName) -> VarName {
    VarName::new(format!("{target}.{field}"))
}

pub(super) struct FunctionRecordFieldAssembly {
    pub(super) name: VarName,
    pub(super) def_id: rumoca_core::DefId,
    pub(super) scalar_type: Option<dae::ScalarType>,
    pub(super) dimensions: Vec<u32>,
    pub(super) scalars: Vec<FunctionRecordScalarSource>,
    pub(super) aggregate_statement: Option<usize>,
}

pub(super) struct FunctionRecordCallAssemblyPlan {
    pub(super) target: VarName,
    pub(super) target_def_id: rumoca_core::DefId,
    pub(super) fields: Vec<FunctionRecordCallField>,
}

pub(super) struct FunctionRecordCallField {
    pub(super) name: VarName,
    pub(super) def_id: rumoca_core::DefId,
    pub(super) result_ordinal: usize,
}

#[derive(Clone)]
pub(super) struct FunctionRecordScalarSource {
    pub(super) statement_offset: usize,
    pub(super) value_coordinates: Vec<u32>,
}

pub(super) enum FunctionLoopLowering {
    Fold {
        targets: Vec<VarName>,
        iteration_locals: Vec<VarName>,
    },
    TotalArrayDefinition,
}

#[derive(Clone, Copy)]
struct FunctionValidationContext<'scope> {
    function: &'scope rumoca_core::Function,
    flat: &'scope flat::Model,
    roles: &'scope HashMap<VarName, PlannedRole>,
    static_integers: &'scope HashMap<VarName, i64>,
    shapes: &'scope ShapeEnvironment,
    shape_analysis: &'scope FunctionShapeAnalysis,
    generated_booleans: &'scope [function_returns::GeneratedBooleanDefinition],
    /// Record-field coordinates already constructed by an enclosing staged
    /// assembly at this exact source position.
    staged_record_fields: &'scope HashSet<FunctionRecordFieldIdentity>,
    /// Whether this source sequence maps directly to the call-scoped action
    /// sequence rather than a loop or runtime-conditional value owner.
    call_scoped_actions: bool,
}

/// Name the statement form, so a report says which owner is missing.
pub(super) fn statement_kind(statement: &rumoca_core::Statement) -> &'static str {
    match statement {
        rumoca_core::Statement::Empty { .. } => "empty",
        rumoca_core::Statement::Assignment { .. } => "assignment",
        rumoca_core::Statement::Return { .. } => "return",
        rumoca_core::Statement::Break { .. } => "break",
        rumoca_core::Statement::For { .. } => "for",
        rumoca_core::Statement::While { .. } => "while",
        rumoca_core::Statement::If { .. } => "if",
        rumoca_core::Statement::When { .. } => "when",
        rumoca_core::Statement::FunctionCall { .. } => "function-call",
        rumoca_core::Statement::Reinit { .. } => "reinit",
        rumoca_core::Statement::Assert { .. } => "assert",
    }
}

pub(super) fn required_statement_span(
    statement: &rumoca_core::Statement,
    owner: impl Into<String>,
) -> Result<Span, ToDaeError> {
    let kind = statement_kind(statement);
    statement
        .source_span()
        .ok_or_else(|| ToDaeError::MissingProvenance {
            owner: format!("{} ({kind} statement occurrence)", owner.into()),
        })
}

#[derive(Clone, Copy, Debug)]
pub(super) enum PlannedRole {
    /// An MLS §9.1.3 member that has no connection and no binding. It is
    /// retained in Flat IR for source identity, but has no runtime coordinate.
    UnusedExpandable,
    Parameter,
    Constant,
    Input,
    State,
    Algebraic,
    Output,
    DiscreteReal,
    DiscreteValue,
    Clock,
    EnumerationLiteral,
    Aggregate,
}

/// The planned roles that own a coordinate in the DAE variable arena.
///
/// An MLS §9.1.3 unused expandable member, a clock, an enumeration literal and
/// a record aggregate are all planned roles with no variable of their own: the
/// identity pass filters them before anything is reserved. Naming the outcome
/// of that filter is what lets the reservation and definition mappings be
/// exhaustive matches rather than matches with an arm nothing can reach.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum RuntimeVariableRole {
    Parameter,
    Constant,
    Input,
    State,
    Algebraic,
    Output,
    DiscreteReal,
    DiscreteValue,
}

impl PlannedRole {
    /// The runtime coordinate this role owns, or `None` when it owns none.
    pub(super) const fn runtime(self) -> Option<RuntimeVariableRole> {
        match self {
            Self::UnusedExpandable | Self::Clock | Self::EnumerationLiteral | Self::Aggregate => {
                None
            }
            Self::Parameter => Some(RuntimeVariableRole::Parameter),
            Self::Constant => Some(RuntimeVariableRole::Constant),
            Self::Input => Some(RuntimeVariableRole::Input),
            Self::State => Some(RuntimeVariableRole::State),
            Self::Algebraic => Some(RuntimeVariableRole::Algebraic),
            Self::Output => Some(RuntimeVariableRole::Output),
            Self::DiscreteReal => Some(RuntimeVariableRole::DiscreteReal),
            Self::DiscreteValue => Some(RuntimeVariableRole::DiscreteValue),
        }
    }
}

/// The sole checked interpretation of one Flat whole-record equation.
///
/// Analysis authenticates occurrence, nominal/effective layout, leaf type,
/// role, and shape exactly once before issuing this private plan. Balance,
/// topology, and lowering consume its derived facts; they must not reopen Flat
/// to repeat or repair those checks.
pub(super) struct RecordEquationPlan {
    pub(super) fields: Vec<RecordEquationFieldPlan>,
    pub(super) aggregate: Option<RecordEquationAggregateSide>,
}

#[derive(Clone, Copy)]
pub(super) enum RecordEquationAggregateSide {
    Left,
    Right,
}

pub(super) enum RecordEquationFieldPlan {
    ContinuousRealResidual {
        target: RecordEquationCoordinate,
        value: RecordEquationFieldValue,
    },
    DiscreteRealResidual {
        target: RecordEquationCoordinate,
        value: RecordEquationFieldValue,
    },
    DiscreteValueDefinition {
        target: RecordEquationCoordinate,
        value: RecordEquationFieldValue,
        dependencies: HashSet<VarName>,
    },
}

impl RecordEquationFieldPlan {
    pub(super) fn target(&self) -> &RecordEquationCoordinate {
        match self {
            Self::ContinuousRealResidual { target, .. }
            | Self::DiscreteRealResidual { target, .. }
            | Self::DiscreteValueDefinition { target, .. } => target,
        }
    }

    pub(super) fn value(&self) -> &RecordEquationFieldValue {
        match self {
            Self::ContinuousRealResidual { value, .. }
            | Self::DiscreteRealResidual { value, .. }
            | Self::DiscreteValueDefinition { value, .. } => value,
        }
    }
}

pub(super) enum RecordEquationFieldValue {
    AggregateProjection(Box<[usize]>),
    Coordinate(RecordEquationCoordinate),
}

#[derive(Clone)]
pub(super) struct RecordEquationCoordinate {
    /// Diagnostic/display identity only; semantic transition uses
    /// `instance_id`.
    name: VarName,
    instance_id: rumoca_core::InstanceId,
    /// Derived once from the checked Flat leaf at plan construction.
    scalar_count: usize,
    /// Derived once from the checked Appendix-B role at plan construction.
    discrete_unknown: bool,
}

impl RecordEquationCoordinate {
    pub(super) fn name(&self) -> &VarName {
        &self.name
    }

    pub(super) fn instance_id(&self) -> rumoca_core::InstanceId {
        self.instance_id
    }

    pub(super) fn scalar_count(&self) -> usize {
        self.scalar_count
    }

    pub(super) fn is_discrete_unknown(&self) -> bool {
        self.discrete_unknown
    }
}

pub(super) fn analyze<'flat>(flat: &'flat flat::Model) -> Result<AnalyzedModel<'flat>, ToDaeError> {
    validate_source_model(flat)?;
    let structured_equation_owners = analyze_structured_equation_owners(flat)?;
    // DAE-C02 is an input-shape and occurrence-identity contract, so issue its
    // complete occurrence certificates before any unrelated semantic analysis
    // can choose a later diagnostic.
    let (states, derivative_candidates) = analyze_derivatives(&structured_equation_owners)?;
    // Fold the parameter fixed point before shape analysis: MLS §12.2 array
    // dimensions can depend on the settled parameter values from MLS §4.5.
    let constants = constant_context(flat)?;
    let function_shapes = FunctionShapeAnalysis::analyze(flat, &constants)?;
    // Preserve the established first-error contract: function-body defects
    // refuse as soon as their shape environment closes, before unrelated
    // model ownership analyses can select a different diagnostic.
    let function_plans = validate_functions(flat, &function_shapes)?;
    let record_array_fields = Arc::clone(function_shapes.record_array_fields());
    let expression_support = analyze_expression_support(&structured_equation_owners, &constants)?;
    let clocks = analyze_clocks(flat, &constants)?;
    let ModelRoles {
        derivatives,
        variables: roles,
        expressions: expression_roles,
    } = analyze_model_roles(flat, &clocks.sampled_targets, states, derivative_candidates)?;
    let record_equations = analyze_record_equation_sets(flat, &roles)?;
    analyze_from_foundation(
        flat,
        AnalysisFoundation {
            constants,
            function_shapes,
            function_plans,
            record_array_fields,
            expression_support,
            clocks,
            derivatives,
            roles,
            expression_roles,
            record_equations,
            structured_equation_owners,
        },
    )
}

struct AnalysisFoundation<'flat> {
    constants: EvalContext,
    function_shapes: FunctionShapeAnalysis,
    function_plans: HashMap<FunctionSpecializationKey, FunctionPlan>,
    record_array_fields: Arc<RecordArrayFieldPlans>,
    expression_support: ExpressionSupportPlans,
    clocks: ClockAnalysis,
    derivatives: DerivativePlans,
    roles: HashMap<VarName, PlannedRole>,
    expression_roles: HashMap<VarName, PlannedRole>,
    record_equations: RecordEquationSets,
    structured_equation_owners: StructuredEquationOwners<'flat>,
}

struct AnalysisCompletion<'flat> {
    derived_parameters: DerivedParameterAnalysis,
    clock_domains: ClockDomainAnalysis,
    history_operators: HistoryOperatorPlans,
    multi_output_equations: MultiOutputEquationSets,
    sample_lattices: Vec<(Span, PeriodicClockSchedule)>,
    model_algorithms: ModelAlgorithmSequence<'flat>,
    model_equations: ModelEquationSequence<'flat>,
    discrete_value_topology: DiscreteValueTopologyPlan,
    initial_algorithms: InitialAlgorithmAnalysis,
    initial_discrete_equation_rows: HashSet<usize>,
    balance: SourceBalanceAnalysis,
    expression_events: ExpressionEventPlans,
    sample_alias_schedules: HashMap<VarName, PeriodicClockSchedule>,
}

struct InitialOwnershipAndBalance {
    algorithms: InitialAlgorithmAnalysis,
    equation_rows: HashSet<usize>,
    balance: SourceBalanceAnalysis,
}

fn analyze_from_foundation<'flat>(
    flat: &'flat flat::Model,
    mut foundation: AnalysisFoundation<'flat>,
) -> Result<AnalyzedModel<'flat>, ToDaeError> {
    validate_runtime_coordinates(flat, &foundation.roles, &foundation.record_array_fields)?;
    let derived_parameters = analyze_derived_parameters(flat, &foundation.roles)?;
    apply_derived_parameter_roles(
        &derived_parameters.plans,
        &mut foundation.roles,
        &mut foundation.expression_roles,
    );
    let clock_domains = analyze_clocked_partitions(
        flat,
        &foundation.clocks,
        &foundation.constants,
        &mut foundation.roles,
        &mut foundation.expression_roles,
    )?;
    let history_operators =
        analyze_history_operators(&foundation.structured_equation_owners, &foundation.roles)?;
    let multi_output_equations = analyze_multi_output_equation_sets(
        flat,
        &foundation.expression_roles,
        &foundation.function_shapes,
    )?;
    validate_expressions_and_structured_rows(ExpressionValidationInput {
        flat,
        structured_equation_owners: &foundation.structured_equation_owners,
        roles: &foundation.roles,
        expression_roles: &foundation.expression_roles,
        record_array_fields: &foundation.record_array_fields,
        values: foundation.function_shapes.model_values(),
        multi_output_equations: &multi_output_equations.continuous,
        initial_multi_output_equations: &multi_output_equations.initialization,
    })?;
    let continuous_family_rows = foundation
        .structured_equation_owners
        .continuous()
        .structured_row_indices()
        .collect::<HashSet<_>>();
    reject_record_family_rows(
        &foundation.record_equations.continuous,
        &continuous_family_rows,
        &flat.equations,
    )?;
    let (discrete_connection_ranks, aggregate_discrete_connections) = analyze_discrete_connections(
        flat,
        &foundation.roles,
        &foundation.record_equations.continuous,
    )?;
    let model_equations = ModelEquationSequence::issue(
        flat,
        &foundation.roles,
        &discrete_connection_ranks,
        aggregate_discrete_connections,
    )?;
    let discrete_value_topology = analyze_discrete_value_topology(
        &model_equations,
        &foundation.roles,
        &foundation.record_equations.continuous,
    )?;
    let (expression_events, sample_alias_schedules) = analyze_expression_event_ownership(
        &model_equations,
        &foundation.roles,
        &foundation.constants,
    )?;
    let (mut sample_lattices, model_algorithms) = analyze_event_algorithms(
        flat,
        &foundation.roles,
        &foundation.expression_roles,
        &foundation.constants,
        &foundation.function_shapes,
        &sample_alias_schedules,
    )?;
    let initial = analyze_initial_ownership_and_balance(
        flat,
        &foundation,
        &derived_parameters,
        &multi_output_equations,
        &model_equations,
        &mut sample_lattices,
    )?;
    finish_analysis(
        foundation,
        AnalysisCompletion {
            derived_parameters,
            clock_domains,
            history_operators,
            multi_output_equations,
            sample_lattices,
            model_algorithms,
            model_equations,
            discrete_value_topology,
            initial_algorithms: initial.algorithms,
            initial_discrete_equation_rows: initial.equation_rows,
            balance: initial.balance,
            expression_events,
            sample_alias_schedules,
        },
    )
}

fn analyze_initial_ownership_and_balance(
    flat: &flat::Model,
    foundation: &AnalysisFoundation<'_>,
    derived_parameters: &DerivedParameterAnalysis,
    multi_output_equations: &MultiOutputEquationSets,
    model_equations: &ModelEquationSequence<'_>,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<InitialOwnershipAndBalance, ToDaeError> {
    let (algorithms, equation_rows) = analyze_initial_owners(
        flat,
        &foundation.roles,
        &foundation.constants,
        sample_lattices,
    )?;
    let balance = analyze_source_balance(SourceBalanceAnalysisInput {
        equations: model_equations,
        roles: &foundation.roles,
        clock_equation_rows: &foundation.clocks.equation_rows,
        derived_parameter_rows: &derived_parameters.rows,
        record_equations: &foundation.record_equations.continuous,
        multi_output_equations: &multi_output_equations.continuous,
    })?;
    Ok(InitialOwnershipAndBalance {
        algorithms,
        equation_rows,
        balance,
    })
}

fn finish_analysis<'flat>(
    foundation: AnalysisFoundation<'flat>,
    completion: AnalysisCompletion<'flat>,
) -> Result<AnalyzedModel<'flat>, ToDaeError> {
    let model_algorithms = completion.model_algorithms;
    let model_equations = completion.model_equations;
    let analysis = Analysis {
        constants: foundation.constants,
        delay_plans: foundation.expression_support.delays,
        derivatives: foundation.derivatives,
        history_operators: completion.history_operators,
        roles: foundation.roles,
        balance: completion.balance.detail,
        structured_equation_owners: foundation.structured_equation_owners,
        initial_discrete_equation_rows: completion.initial_discrete_equation_rows,
        sample_lattices: completion.sample_lattices,
        expression_events: completion.expression_events,
        sample_alias_schedules: completion.sample_alias_schedules,
        clock_plans: foundation.clocks.plans,
        clock_equation_rows: foundation.clocks.equation_rows,
        clocked_equation_owners: completion.clock_domains.equation_owners,
        clocked_value_owners: completion.clock_domains.value_owners,
        clocked_when_owners: completion.clock_domains.when_owners,
        clocked_coordinate_owners: completion.clock_domains.coordinate_owners,
        clock_transfer_plans: completion.clock_domains.transfers,
        initial_parameters: completion.initial_algorithms.parameters,
        initial_discrete_values: completion.initial_algorithms.discrete_values,
        initial_algorithm_assertions: completion.initial_algorithms.assertions,
        function_plans: foundation.function_plans,
        function_shapes: foundation.function_shapes,
        comprehension_plans: foundation.expression_support.comprehensions,
        record_array_fields: foundation.record_array_fields,
        derived_parameters: completion.derived_parameters.plans,
        derived_parameter_families: completion.derived_parameters.families,
        derived_parameter_rows: completion.derived_parameters.rows,
        record_equations: foundation.record_equations.continuous,
        multi_output_equations: completion.multi_output_equations.continuous,
        initial_multi_output_equations: completion.multi_output_equations.initialization,
        discrete_value_topology: completion.discrete_value_topology,
        assigned_discrete_targets: completion.balance.assigned_discrete_targets,
        semi_linear_rules: SemiLinearRules::default(),
    };
    Ok(AnalyzedModel {
        analysis,
        model_algorithms,
        model_equations,
    })
}

struct ExpressionValidationInput<'a> {
    flat: &'a flat::Model,
    structured_equation_owners: &'a StructuredEquationOwners<'a>,
    roles: &'a HashMap<VarName, PlannedRole>,
    expression_roles: &'a HashMap<VarName, PlannedRole>,
    record_array_fields: &'a RecordArrayFieldPlans,
    values: &'a ShapeEnvironment,
    multi_output_equations: &'a HashMap<usize, MultiOutputEquationPlan>,
    initial_multi_output_equations: &'a HashMap<usize, MultiOutputEquationPlan>,
}

struct ExpressionSupportPlans {
    comprehensions: ComprehensionPlans,
    delays: HashMap<Span, DelayPlan>,
}

fn analyze_expression_support(
    owners: &StructuredEquationOwners<'_>,
    constants: &EvalContext,
) -> Result<ExpressionSupportPlans, ToDaeError> {
    let flat = owners.model();
    Ok(ExpressionSupportPlans {
        comprehensions: analyze_comprehensions(all_model_expressions(flat), constants)?,
        delays: analyze_delays(owners, constants)?,
    })
}

struct RecordEquationSets {
    continuous: HashMap<usize, RecordEquationPlan>,
}

fn analyze_record_equation_sets(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<RecordEquationSets, ToDaeError> {
    reject_initial_record_equations(flat)?;
    reject_structured_record_equations(flat)?;
    Ok(RecordEquationSets {
        continuous: analyze_record_equations(flat, &flat.equations, roles)?,
    })
}

fn validate_expressions_and_structured_rows(
    input: ExpressionValidationInput<'_>,
) -> Result<(), ToDaeError> {
    validate_model_expressions(
        input.flat,
        input.expression_roles,
        input.record_array_fields,
        input.values,
        input.multi_output_equations,
        input.initial_multi_output_equations,
    )?;
    validate_structured_templates(
        input.structured_equation_owners,
        input.roles,
        input.expression_roles,
        input.record_array_fields,
        input.values,
    )
}

fn analyze_multi_output_equation_sets(
    flat: &flat::Model,
    expression_roles: &HashMap<VarName, PlannedRole>,
    function_shapes: &FunctionShapeAnalysis,
) -> Result<MultiOutputEquationSets, ToDaeError> {
    let continuous = analyze_multi_output_equations(
        flat,
        &flat.equations,
        expression_roles,
        function_shapes,
        false,
    )?;
    let initialization = analyze_multi_output_equations(
        flat,
        &flat.initial_equations,
        expression_roles,
        function_shapes,
        true,
    )?;
    Ok(MultiOutputEquationSets {
        continuous,
        initialization,
    })
}

struct MultiOutputEquationSets {
    continuous: HashMap<usize, MultiOutputEquationPlan>,
    initialization: HashMap<usize, MultiOutputEquationPlan>,
}

fn validate_runtime_coordinates(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    record_array_fields: &RecordArrayFieldPlans,
) -> Result<(), ToDaeError> {
    validate_record_array_field_runtime_coordinates(flat, record_array_fields, roles)
}

/// Prove initial-algorithm ownership and claim initial discrete equation rows
/// as one construction transaction.
///
/// The equation claim mutates the algorithm-owned discrete staging plan, so
/// returning either half before both succeed would expose a partial ownership
/// proof to the rest of analysis.
fn analyze_initial_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(InitialAlgorithmAnalysis, HashSet<usize>), ToDaeError> {
    let mut algorithms = analyze_initial_algorithm_owners(flat, roles, constants, sample_lattices)?;
    let rows = claim_initial_discrete_equations(flat, roles, &mut algorithms.discrete_values)?;
    Ok((algorithms, rows))
}

fn validate_source_model(flat: &flat::Model) -> Result<(), ToDaeError> {
    validate_flat_shape(flat)?;
    // Prove the initial-algorithm grammar before another analysis reports a
    // consequence of its missing owner, such as an `assert` read as a callee.
    reject_unsupported_initial_algorithm_statements(flat)?;
    validate_impure_call_contexts(flat)
}

fn analyze_discrete_connections(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    record_equations: &HashMap<usize, RecordEquationPlan>,
) -> Result<(HashMap<VarName, usize>, AggregateDiscreteConnections), ToDaeError> {
    let ranks = discrete_connection_ranks(flat, roles, record_equations)?;
    let aggregates = aggregate_discrete_connections(flat, roles, &ranks)?;
    Ok((ranks, aggregates))
}

impl AnalyzedModel<'_> {
    /// Prove the MLS §3.7.4.5 Rule 1 / Rule 2 replacements over the model
    /// equation rows, completing the plan [`analyze`] leaves empty.
    ///
    /// The rules read every other owner's row claims, so they can only be
    /// proven once the rest of the analysis exists. Construction is the caller;
    /// `balance_detail` deliberately is not, because the source balance the
    /// rules preserve is counted on the untransformed rows.
    pub(super) fn with_semi_linear_rules(mut self) -> Self {
        let mut claimed = self
            .analysis
            .structured_equation_owners
            .continuous()
            .structured_row_indices()
            .collect::<HashSet<_>>();
        claimed.extend(&self.analysis.clock_equation_rows);
        claimed.extend(&self.analysis.derived_parameter_rows);
        self.analysis.semi_linear_rules = analyze_semi_linear_rules(
            &self.model_equations,
            &SemiLinearRowFilter {
                excluded: &claimed,
                records: &self.analysis.record_equations,
                clocked: &self.analysis.clocked_equation_owners,
            },
        );
        self
    }
}

/// Proves the clocked partitions and corrects the role plan they contradict.
///
/// The proven partition owners are what turn a continuous role plan into the
/// clocked discrete-time role MLS §16.5.1 requires, and the corrected roles are
/// in turn what let `assign_value_owners` prove clock ownership for those
/// coordinates. The replay is exact rather than iterative: partition membership
/// reads roles only through `is_clock_runtime_role`, which both the old and the
/// new role satisfy, so the second pass proves the same partitions and only
/// widens the ownership relation over them.
fn analyze_clocked_partitions(
    flat: &flat::Model,
    clocks: &ClockAnalysis,
    constants: &EvalContext,
    roles: &mut HashMap<VarName, PlannedRole>,
    expression_roles: &mut HashMap<VarName, PlannedRole>,
) -> Result<ClockDomainAnalysis, ToDaeError> {
    let domains = clocks::analyze_clock_domains(
        flat,
        roles,
        &clocks.plans,
        &clocks.equation_rows,
        &clocks.sampled_targets,
        constants,
    )?;
    if !apply_clocked_partition_roles(flat, &domains.coordinate_owners, roles, expression_roles)? {
        return Ok(domains);
    }
    clocks::analyze_clock_domains(
        flat,
        roles,
        &clocks.plans,
        &clocks.equation_rows,
        &clocks.sampled_targets,
        constants,
    )
}

fn apply_derived_parameter_roles(
    plans: &HashMap<VarName, DerivedParameterPlan>,
    roles: &mut HashMap<VarName, PlannedRole>,
    expression_roles: &mut HashMap<VarName, PlannedRole>,
) {
    for name in plans.keys() {
        roles.insert(name.clone(), PlannedRole::Parameter);
        expression_roles.insert(name.clone(), PlannedRole::Parameter);
    }
}

fn validate_model_expressions(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    record_array_fields: &RecordArrayFieldPlans,
    model_values: &ShapeEnvironment,
    multi_output_equations: &HashMap<usize, MultiOutputEquationPlan>,
    initial_multi_output_equations: &HashMap<usize, MultiOutputEquationPlan>,
) -> Result<(), ToDaeError> {
    for variable in flat.variables.values() {
        for expression in variable_attribute_expressions(variable) {
            if let Some(span) = empty_array_bound_to_declaration(variable, expression) {
                // The owning declaration proves the element type and extent, so
                // the literal carries no operand that needs validating.
                require_span(span, "empty array attribute")?;
            } else {
                validate_model_expression_with_record_array_fields(
                    expression,
                    roles,
                    record_array_fields,
                    model_values,
                )?;
            }
            validate_known_function_calls(expression, flat)?;
        }
    }
    for (row, equation) in flat.equations.iter().enumerate() {
        if multi_output_equations.contains_key(&row) {
            continue;
        }
        let expression = &equation.residual;
        validate_model_expression_with_record_array_fields(
            expression,
            roles,
            record_array_fields,
            model_values,
        )?;
        validate_known_function_calls(expression, flat)?;
    }
    for (row, equation) in flat.initial_equations.iter().enumerate() {
        if initial_multi_output_equations.contains_key(&row) {
            continue;
        }
        let expression = &equation.residual;
        validate_model_expression_with_record_array_fields(
            expression,
            roles,
            record_array_fields,
            model_values,
        )?;
        validate_known_function_calls(expression, flat)?;
    }
    Ok(())
}

/// MLS §10.4: an empty array literal has no element from which to derive a
/// type, so its element type and trailing extents come from the declaration it
/// is bound to. A variable attribute may be an empty array exactly when its
/// own declaration proves a zero outer extent.
pub(super) fn empty_array_bound_to_declaration(
    variable: &flat::Variable,
    expression: &Expression,
) -> Option<Span> {
    let Expression::Array { elements, span, .. } = expression else {
        return None;
    };
    (elements.is_empty() && variable.dims.first() == Some(&0)).then_some(*span)
}

fn validate_flat_shape(flat: &flat::Model) -> Result<(), ToDaeError> {
    if !flat.predefined_types.is_complete() {
        return Err(ToDaeError::MissingSemanticIdentity {
            identity: "predefined scalar canonical TypeIds".to_string(),
        });
    }
    flat.validate().map_err(|error| {
        let detail = format!("{error:?}");
        match error.span() {
            Some(span) => ToDaeError::unsupported_flat("Flat shape contract", detail, span),
            None => ToDaeError::internal(format!(
                "Flat shape contract failed without a source owner: {detail}"
            )),
        }
    })
}

struct SourceBalanceAnalysisInput<'scope> {
    equations: &'scope ModelEquationSequence<'scope>,
    roles: &'scope HashMap<VarName, PlannedRole>,
    clock_equation_rows: &'scope HashSet<usize>,
    derived_parameter_rows: &'scope HashSet<usize>,
    record_equations: &'scope HashMap<usize, RecordEquationPlan>,
    multi_output_equations: &'scope HashMap<usize, MultiOutputEquationPlan>,
}

fn analyze_source_balance(
    input: SourceBalanceAnalysisInput<'_>,
) -> Result<SourceBalanceAnalysis, ToDaeError> {
    let SourceBalanceAnalysisInput {
        equations,
        roles,
        clock_equation_rows,
        derived_parameter_rows,
        record_equations,
        multi_output_equations,
    } = input;
    let assigned_discrete_targets = defined_discrete_targets(equations, roles, record_equations);
    let mut non_runtime_rows = clock_equation_rows.clone();
    non_runtime_rows.extend(derived_parameter_rows);
    let detail = source_balance(SourceBalanceInput {
        equations,
        roles,
        assigned_targets: &assigned_discrete_targets,
        excluded_equation_rows: &non_runtime_rows,
        record_equations,
        multi_output_equations,
    })?;
    Ok(SourceBalanceAnalysis {
        detail,
        assigned_discrete_targets,
    })
}

/// Replay the initial algorithms, then validate every assertion the model owns
/// — the ones an equation section declares and the ones a replayed section
/// produced — against one condition grammar.
fn analyze_initial_algorithm_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<InitialAlgorithmAnalysis, ToDaeError> {
    let initial_algorithms = analyze_initial_algorithms(flat, roles, constants)?;
    validate_assertions(
        flat.assert_equations
            .iter()
            .chain(&flat.initial_assert_equations)
            .chain(&initial_algorithms.assertions),
        roles,
        constants,
        sample_lattices,
    )?;
    Ok(initial_algorithms)
}

fn validate_assertions<'flat>(
    assertions: impl IntoIterator<Item = &'flat flat::AssertEquation>,
    roles: &HashMap<VarName, PlannedRole>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    for assertion in assertions {
        require_span(assertion.span, "assert equation")?;
        validate_condition_expression(&assertion.condition, roles, constants, sample_lattices)?;
        validate_expression(&assertion.message, roles)?;
        if let Some(level) = &assertion.level {
            validate_expression(level, roles)?;
        }
    }
    Ok(())
}

pub(super) fn analyze_record_array_field_plans(
    flat: &flat::Model,
) -> Result<RecordArrayFieldPlans, ToDaeError> {
    analyze_record_array_fields(
        flat,
        all_model_expressions(flat)
            .chain(structured_template_expressions(&flat.structured_equations))
            .chain(structured_template_expressions(
                &flat.initial_structured_equations,
            ))
            .chain(
                flat.functions
                    .values()
                    .flat_map(function_shapes::function_expressions),
            ),
    )
}

fn structured_template_expressions(
    families: &[flat::StructuredEquationFamily],
) -> impl Iterator<Item = &Expression> {
    families
        .iter()
        .filter_map(|family| family.template.as_ref())
        .flat_map(|template| &template.body)
}

/// Fold every `constant`/`parameter` binding the Flat model settles at
/// translation time.
///
/// The fixed point is keyed on the exact occurrence identity of each
/// declaration — `validate_flat_shape` has already proven every Flat variable
/// carries a distinct allocated [`InstanceId`], so "has this declaration been
/// settled" is a question about the identity and never about a rendered name
/// two occurrences could share.
///
/// A binding that does not fold is only skipped when the evaluator says *why*
/// in typed terms: MLS §4.4 permits a parameter value to be established during
/// initialization instead, and those failures carry a
/// `RuntimeDependentReason`. Any other failure proves the model or the
/// evaluator wrong and is reported at the binding.
fn constant_context(flat: &flat::Model) -> Result<EvalContext, ToDaeError> {
    let mut settled = HashMap::new();
    for _ in 0..flat.variables.len() {
        let context = resolved_constant_context(flat, &settled)?;
        let mut progress = false;
        let mut newly_settled = Vec::new();
        for (name, variable) in &flat.variables {
            let identity = variable_occurrence_identity(name, variable)?;
            if settled.contains_key(&identity)
                || !matches!(
                    variable.variability,
                    Variability::Constant(_) | Variability::Parameter(_)
                )
                || matches!(variable.variability, Variability::Parameter(_))
                    && variable.fixed == Some(false)
            {
                continue;
            }
            let Some(binding) = &variable.binding else {
                continue;
            };
            match eval_expr(binding, &context) {
                Ok(value) => {
                    newly_settled.push((identity, value));
                    progress = true;
                }
                Err(error) if error.runtime_dependent_reason().is_some() => {}
                Err(error) => {
                    return Err(ToDaeError::unsupported_flat(
                        "parameter binding",
                        format!("`{name}` cannot be evaluated: {error}"),
                        error.span().unwrap_or(variable.source_span),
                    ));
                }
            }
        }
        settled.extend(newly_settled);
        if !progress {
            break;
        }
    }
    let mut context = resolved_constant_context(flat, &settled)?;
    register_deferred_parameters(flat, &mut context)?;
    Ok(context)
}

fn resolved_constant_context(
    flat: &flat::Model,
    settled: &HashMap<rumoca_eval_flat::constant::ResolvedOccurrenceKey, EvalValue>,
) -> Result<EvalContext, ToDaeError> {
    let mut values = Vec::with_capacity(settled.len());
    let mut shapes = Vec::with_capacity(flat.variables.len());
    for variable in flat.variables.values() {
        let Some(reference) = variable.component_ref.as_ref() else {
            return Err(ToDaeError::unsupported_flat(
                "parameter identity inventory",
                format!(
                    "Flat variable `{}` has no structured component reference",
                    variable.name
                ),
                variable.source_span,
            ));
        };
        let identity = rumoca_eval_flat::constant::ResolvedOccurrenceKey {
            instance_id: variable.instance_id,
            root_def_id: reference.root_def_id(),
        };
        shapes.push(rumoca_eval_flat::constant::ResolvedShapeBinding {
            identity,
            dimensions: variable.dims.clone(),
        });
        if let Some(value) = settled.get(&identity) {
            values.push(rumoca_eval_flat::constant::ResolvedValueBinding {
                identity,
                value: value.clone(),
            });
        }
    }
    let inventory =
        rumoca_eval_flat::constant::ResolvedIdentityInventory::try_from_bindings(values, shapes)
            .map_err(|error| ToDaeError::internal(error.to_string()))?;
    let mut context = EvalContext::resolved(
        flat.variables.len(),
        flat.functions.len() * 2,
        inventory,
        rumoca_eval_flat::constant::ResolvedEnumCatalog::empty(),
    );
    let mut issued_functions = HashMap::new();
    for function in flat.functions.values() {
        if let Some(instance_id) = function.instance_id
            && issued_functions
                .get(&instance_id)
                .is_some_and(|existing| *existing == function)
        {
            continue;
        }
        context
            .try_add_function(function.clone())
            .map_err(|error| {
                ToDaeError::unsupported_flat(
                    "constant function catalog",
                    error.to_string(),
                    function.span,
                )
            })?;
        if let Some(instance_id) = function.instance_id {
            issued_functions.insert(instance_id, function);
        }
    }
    for (name, variable) in &flat.variables {
        context.add_array_dimensions(name.to_string(), variable.dims.clone());
    }
    Ok(context)
}

fn variable_occurrence_identity(
    name: &VarName,
    variable: &flat::Variable,
) -> Result<rumoca_eval_flat::constant::ResolvedOccurrenceKey, ToDaeError> {
    let reference = variable.component_ref.as_ref().ok_or_else(|| {
        ToDaeError::unsupported_flat(
            "parameter identity inventory",
            format!("Flat variable `{name}` has no structured component reference"),
            variable.source_span,
        )
    })?;
    Ok(rumoca_eval_flat::constant::ResolvedOccurrenceKey {
        instance_id: variable.instance_id,
        root_def_id: reference.root_def_id(),
    })
}

/// Name every `fixed = false` parameter the initialization system settles, and
/// what settles it.
///
/// MLS §4.4 lets a parameter declaration defer its value and MLS §8.6 gives the
/// initial section the job of establishing it. Such a parameter is absent from
/// the fold above for a *reason*, not by accident: it has no binding to fold.
/// A later consumer that asks the fold for its value — a `sample(start,
/// interval)` phase, a `delay` bound, a `Clock` argument — otherwise learns
/// only that the name has no value and reports it as unknown, which reads as a
/// name-resolution defect and hides the construct that was actually hit.
///
/// The distinction recorded here is the one that matters to those consumers:
/// a determining expression that reads `time` is settled by the *simulation
/// start instant*, because MLS §8.6 evaluates the initial section at the
/// initialization instant. That instant is chosen when the model is simulated,
/// not when it is translated, so no translation-time value exists for it at
/// all — as opposed to a value that merely waits for the initialization system
/// to run.
///
/// The two tiers are ordered, not symmetric: [`DeferredParameterSource::
/// StartInstant`] is a *proof* about the shape it recognizes, while
/// [`DeferredParameterSource::InitializationSystem`] is the conservative floor
/// every other deferred parameter falls to and asserts nothing about the start
/// instant either way. See [`deferred_parameter_source`] for exactly what is
/// proven and which start-instant-dependent shapes land in the weaker tier.
///
/// # Known remaining
///
/// A parameter that *has* a binding reading a deferred parameter is not
/// registered here — the guard below skips binding-holders, and the fold at
/// [`constant_context`] discards its failure through the
/// `runtime_dependent_reason` arm — so such a parameter is still reported as an
/// unknown name. This is real MSL, not a hypothetical:
/// `Modelica.Blocks.Math.ContinuousMean` (`Blocks/Math.mo:2349-2352`) declares
/// `parameter Real t_0(fixed = false)` with `initial equation t_0 = time` and
/// then `parameter Real actualStartTime = max(t_0, startTime)`, so
/// `actualStartTime` is start-instant-dependent through its binding. Closing it
/// needs one of: propagating the deferred source through the binding fold, or
/// registering a failed-fold binding over deferred free names as deferred
/// itself. Both belong with the start-relative-schedule work that would let
/// these sample starts construct at all, rather than with this diagnostic.
fn register_deferred_parameters(
    flat: &flat::Model,
    context: &mut EvalContext,
) -> Result<(), ToDaeError> {
    for (name, variable) in &flat.variables {
        let identity = variable_occurrence_identity(name, variable)?;
        if !matches!(variable.variability, Variability::Parameter(_))
            || variable.fixed != Some(false)
            || variable.binding.is_some()
            || context.occurrence_value(identity).is_some()
        {
            continue;
        }
        context.add_deferred_parameter(name.to_string(), deferred_parameter_source(flat, name));
    }
    Ok(())
}

/// What settles `target`: the start instant, or the initialization system.
///
/// This proves exactly one shape and claims nothing beyond it. `StartInstant`
/// is returned when some initial equation's residual is a *top-level*
/// subtraction with `target` itself as one direct operand, and the other
/// operand *syntactically* reads `time`. That is the shape flat lowering gives
/// `t0 = time`, and it is the shape both MSL sample-start sites are written in.
///
/// Everything else falls to `InitializationSystem`, which is therefore a floor
/// — "this parameter has no translation-time value" — and **not** a claim that
/// the value is independent of the start instant. Reachable shapes that do
/// depend on the start instant and still land in the weaker tier:
///
/// - indirectly, through another deferred parameter (`a = time; t0 = a`);
/// - through an initial *algorithm* target (`t0 := time`), which is owned by
///   [`initial_algorithms`] and never appears in `flat.initial_equations`;
/// - through a residual that is not a top-level subtraction on `target`
///   (`t0*t0 = time + 1.0`).
///
/// Each is refused either way, and the weaker label is true of all of them, so
/// the floor costs precision in the message and never correctness. Widening the
/// rule is deliberately left to the start-relative-schedule work, where a
/// start-instant dependency has to be *represented* rather than only named.
fn deferred_parameter_source(
    flat: &flat::Model,
    target: &VarName,
) -> rumoca_eval_flat::constant::DeferredParameterSource {
    use rumoca_eval_flat::constant::DeferredParameterSource;
    let determined_by_time = flat
        .initial_equations
        .iter()
        .filter_map(|equation| determining_value(&equation.residual, target))
        .any(reads_time);
    if determined_by_time {
        DeferredParameterSource::StartInstant
    } else {
        DeferredParameterSource::InitializationSystem
    }
}

/// The side of a `target - value` residual that is not `target`.
///
/// Flat lowering writes `t0 = time` as the residual `t0 - time`, so this
/// recognizes a residual that is a top-level subtraction with `target` as one
/// *direct* operand, and returns the other operand. It is a syntactic match on
/// that one shape, not a solve: a residual that merely constrains `target`
/// (`t0*t0 - (time + 1.0)`) yields `None`, and its caller then labels the
/// parameter by the weaker tier rather than inspecting it further.
fn determining_value<'a>(residual: &'a Expression, target: &VarName) -> Option<&'a Expression> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = residual
    else {
        return None;
    };
    if names_variable(lhs, target) {
        return Some(rhs);
    }
    if names_variable(rhs, target) {
        return Some(lhs);
    }
    None
}

fn names_variable(expression: &Expression, target: &VarName) -> bool {
    matches!(
        expression,
        Expression::VarRef { name, subscripts, .. }
            if subscripts.is_empty() && name.var_name() == target
    )
}

/// True when the expression reads `time`, whose value MLS §8.6 fixes at the
/// initialization instant.
fn reads_time(expression: &Expression) -> bool {
    if let Expression::VarRef { name, .. } = expression
        && name.as_str() == "time"
    {
        return true;
    }
    expression_children(expression).into_iter().any(reads_time)
}

fn validate_known_function_calls(
    expression: &Expression,
    flat: &flat::Model,
) -> Result<(), ToDaeError> {
    if let Expression::FunctionCall {
        name,
        args,
        is_constructor,
        call_kind,
        span,
    } = expression
    {
        match rumoca_core::classify_named_function_arg_marker(
            name,
            args,
            *is_constructor,
            *call_kind,
        ) {
            rumoca_core::NamedFunctionArgMarker::Valid { .. } => {
                return Err(ToDaeError::unsupported_flat(
                    "named function argument",
                    "generated named-argument wrappers must be eliminated before DAE construction",
                    *span,
                ));
            }
            rumoca_core::NamedFunctionArgMarker::Malformed => {
                return Err(ToDaeError::unsupported_flat(
                    "named function argument",
                    "a generated named argument must be a constructor invocation with one value and a nonempty name",
                    *span,
                ));
            }
            rumoca_core::NamedFunctionArgMarker::NotMarker => {
                validate_regular_function_call(flat, name, args, *span)?;
            }
        }
    }
    for child in expression_children(expression) {
        validate_known_function_calls(child, flat)?;
    }
    Ok(())
}

fn validate_regular_function_call(
    flat: &flat::Model,
    name: &rumoca_core::Reference,
    args: &[Expression],
    span: Span,
) -> Result<(), ToDaeError> {
    if enumeration_conversion(flat, name, args, span)?.is_some() {
        return Ok(());
    }
    let function = flat
        .functions
        .get(name.var_name())
        .ok_or_else(|| ToDaeError::unresolved_reference(name.as_str(), span))?;
    if args.len() == function.inputs.len() {
        return Ok(());
    }
    Err(ToDaeError::unsupported_flat(
        "function call arity",
        format!(
            "`{}` expects {} inputs but receives {}",
            function.name,
            function.inputs.len(),
            args.len()
        ),
        span,
    ))
}

/// Which sample of a record's fields a structured assignment reads.
///
/// Carried as a type rather than a flag beside the pairs so a caller cannot
/// lower `target := pre(source)` by reading the CURRENT field values, which
/// would be a silently wrong answer rather than a refused one.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum StructuredSource {
    /// `target := source`.
    Current(StructuredLeaf),
    /// `target := pre(source)`. Modelica defines `pre` of a record
    /// component-wise, so every field reads its own previous sample and the
    /// whole-record form means exactly the field-wise form.
    Previous(StructuredLeaf),
}

impl StructuredSource {
    pub(super) fn name(&self) -> &VarName {
        match self {
            Self::Current(leaf) | Self::Previous(leaf) => &leaf.name,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct StructuredLeaf {
    pub(super) name: VarName,
    pub(super) identity: rumoca_eval_flat::constant::ResolvedOccurrenceKey,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct StructuredAssignmentPlan {
    pub(super) pairs: Vec<(StructuredLeaf, StructuredSource)>,
}

pub(super) fn structured_assignment_plan(
    flat: &flat::Model,
    target: &rumoca_core::ComponentReference,
    value: &Expression,
) -> Option<StructuredAssignmentPlan> {
    // See through `pre`: the record form is the field-wise form, so the
    // pairing below is identical and only the sample each field reads differs.
    let (value, wrap): (&Expression, fn(StructuredLeaf) -> StructuredSource) = match value {
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args,
            ..
        } if args.len() == 1 => (&args[0], StructuredSource::Previous),
        value => (value, StructuredSource::Current),
    };
    let Expression::VarRef {
        name, subscripts, ..
    } = value
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let source = name.component_ref()?;
    let target_instance = flat.record_instances.get(&target.to_var_name())?;
    let source_instance = flat.record_instances.get(name.var_name())?;
    if name.instance_id()? != source_instance.instance_id {
        return None;
    }
    let target_prefix = target
        .parts()
        .iter()
        .map(|part| part.def_id)
        .collect::<Vec<_>>();
    let source_prefix = source
        .parts()
        .iter()
        .map(|part| part.def_id)
        .collect::<Vec<_>>();
    let target_leaves = structured_leaves(flat, target_instance.instance_id, &target_prefix)?;
    let source_leaves = structured_leaves(flat, source_instance.instance_id, &source_prefix)?;
    if target_leaves.is_empty() || target_leaves.len() != source_leaves.len() {
        return None;
    }
    let mut pairs = Vec::with_capacity(target_leaves.len());
    for (suffix, target_leaf) in target_leaves {
        let source_leaf = source_leaves.get(&suffix)?.clone();
        pairs.push((target_leaf, wrap(source_leaf)));
    }
    pairs.sort_by(|(lhs, _), (rhs, _)| lhs.name.as_str().cmp(rhs.name.as_str()));
    Some(StructuredAssignmentPlan { pairs })
}

fn structured_leaves(
    flat: &flat::Model,
    owner: rumoca_core::InstanceId,
    prefix: &[rumoca_core::DefId],
) -> Option<HashMap<Vec<rumoca_core::DefId>, StructuredLeaf>> {
    let mut leaves = HashMap::new();
    for (name, variable) in &flat.variables {
        if !occurrence_descends_from(flat, variable.instance_id, owner)? {
            continue;
        }
        let reference = variable.component_ref.as_ref()?;
        let declarations = reference_declarations(reference);
        if !declarations.starts_with(prefix) || declarations.len() == prefix.len() {
            continue;
        }
        let suffix = declarations[prefix.len()..].to_vec();
        let leaf = StructuredLeaf {
            name: name.clone(),
            identity: rumoca_eval_flat::constant::ResolvedOccurrenceKey {
                instance_id: variable.instance_id,
                root_def_id: reference.root_def_id(),
            },
        };
        if leaves.insert(suffix, leaf).is_some() {
            return None;
        }
    }
    Some(leaves)
}

fn occurrence_descends_from(
    flat: &flat::Model,
    descendant: rumoca_core::InstanceId,
    ancestor: rumoca_core::InstanceId,
) -> Option<bool> {
    let mut cursor = Some(descendant);
    while let Some(instance) = cursor {
        if instance == ancestor {
            return Some(true);
        }
        cursor = flat.instance_relations.get(&instance)?.owner;
    }
    Some(false)
}

pub(super) fn effective_function_scalar_type(
    flat: &flat::Model,
    value: &rumoca_core::FunctionParam,
) -> Option<dae::ScalarType> {
    let canonical = value.effective_type.canonical_type();
    if canonical == flat.predefined_types.real {
        Some(dae::ScalarType::Real)
    } else if canonical == flat.predefined_types.integer {
        Some(dae::ScalarType::Integer)
    } else if canonical == flat.predefined_types.boolean {
        Some(dae::ScalarType::Boolean)
    } else if canonical == flat.predefined_types.string {
        Some(dae::ScalarType::String)
    } else if flat.enumeration_type_roots.contains(&canonical) {
        Some(dae::ScalarType::Enumeration)
    } else {
        None
    }
}

pub(super) fn effective_variable_scalar_type(
    flat: &flat::Model,
    variable: &flat::Variable,
) -> Option<dae::ScalarType> {
    let canonical = flat
        .effective_types
        .get(&variable.type_id)?
        .canonical_type();
    if canonical == flat.predefined_types.real {
        Some(dae::ScalarType::Real)
    } else if canonical == flat.predefined_types.integer {
        Some(dae::ScalarType::Integer)
    } else if canonical == flat.predefined_types.boolean {
        Some(dae::ScalarType::Boolean)
    } else if canonical == flat.predefined_types.string {
        Some(dae::ScalarType::String)
    } else if flat.enumeration_types.contains(&variable.type_id) {
        Some(dae::ScalarType::Enumeration)
    } else {
        None
    }
}
