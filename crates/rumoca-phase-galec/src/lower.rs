//! Checked DAE → [`AlgorithmCodePackage`] lowering.
//!
//! This projection reads the immutable branded DAE directly. Periodic clock
//! guards become the implicit `DoStep` tick; guarded assignments are ordered
//! by their current-tick dependencies; `pre(x)` becomes a protected
//! `'previous(x)'` state committed after all assignments.

mod assigned_primitives;
mod call_ownership;
mod causal_outputs;
mod causal_substitution;
mod clock_schedule;
mod clocked_assignments;
mod shared_call_memo;
use shared_call_memo::MaterializedFunctionCallKey;
mod conditionals;
mod dependent_folding;
mod expression_array_update;
mod expression_core;
mod expression_function_folds;
mod expression_functions;
mod expression_helpers;
mod expression_projection;
mod expression_record_fields;
mod guard_binding;
mod local_integer_bounds;
mod pre_references;
mod start;
mod structural_locals;
mod user_functions;
mod whole_array_move;

use std::collections::{HashMap, HashSet};
use std::fmt;

use rumoca_core::Span;
use rumoca_eval_dae::NumericEvaluator;
use rumoca_ir_dae as dae;
use rumoca_ir_galec::ast as gast;

use crate::admissibility::{AdmittedClock, check_view as check_admissibility_view};
use crate::diagnostic::GalecTargetError;
use crate::input::{GalecInput, GalecOptions};
use rumoca_ir_galec::package::{AlgorithmCodeArithmeticProfile, AlgorithmCodePackageMetadata};
use rumoca_ir_galec::{AlgorithmCodePackageIssuer, OriginBoundAlgorithmCodePackage};

use assigned_primitives::{
    AssignedPrimitiveSnapshot, AssignedPrimitives, ConditionalActivationKey,
    ConditionalActivationKind, SelectionPointId, SelectionPointNamespace,
};
use call_ownership::{
    CallExecutionSource, CommittedCallActionLedger, CommittedFunctionCallActionLedger,
    CrossGroupCallRetention, EmissionRegion, ExpectedRootCallAction, FunctionCallAction,
    FunctionCallReuse, MaterializedRootCall, PreparedCallActions, RetainedCallActivation,
    RetainedCallResults, RootCallAction, RootCallReuse,
};
use clock_schedule::lower_clock_schedule;
use expression_core::{conditional_activation_operands, first_function_assertion};
use expression_helpers::*;
use local_integer_bounds::{ConditionalIntegerBounds, LocalIntegerBounds, LoopIntegerBounds};
use pre_references::referenced_pre_variables;
use start::{StartShape, StartValues};

#[cfg(test)]
fn positive_zero_arithmetic() -> AlgorithmCodeArithmeticProfile {
    AlgorithmCodeArithmeticProfile::construct(
        rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64,
        rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VariableClass {
    Input,
    Output,
    Local,
    TunableParameter,
    DependentParameter,
    Constant,
    State,
}

#[derive(Clone)]
struct ClassifiedVariable<'dae> {
    id: dae::VariableId<'dae>,
    variable: dae::VariableView<'dae>,
    class: VariableClass,
    scalar_type: gast::ScalarType,
    name: gast::Name,
}

/// Semantic owner of every temporary minted by one expression lowerer.
///
/// Separate lowerers may publish locals into the same GALEC method scope. A
/// free-form string cannot prove those namespaces disjoint, so the lowerer
/// accepts only this closed owner vocabulary. Owners that can occur more than
/// once retain their branded DAE identity instead of accepting a caller-made
/// ordinal or spelling.
#[derive(Clone, Copy)]
enum TemporaryNamespace<'dae> {
    Value,
    Causal,
    Clocked(dae::ClockId<'dae>),
    Dependent(dae::VariableId<'dae>),
}

impl fmt::Display for TemporaryNamespace<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value => formatter.write_str("value"),
            Self::Causal => formatter.write_str("causal"),
            Self::Clocked(clock) => write!(formatter, "clocked{}", clock.index()),
            Self::Dependent(variable) => write!(formatter, "dependent{}", variable.index()),
        }
    }
}

/// One checked classification product. Dependent-parameter ordering is derived
/// from exact DAE variable identities and cannot be supplied by a codegen caller.
struct ClassifiedVariables<'dae> {
    variables: Vec<ClassifiedVariable<'dae>>,
    dependent_parameter_order: Vec<u32>,
}

impl<'dae> ClassifiedVariables<'dae> {
    fn iter(&self) -> impl Iterator<Item = &ClassifiedVariable<'dae>> {
        self.variables.iter()
    }

    fn as_slice(&self) -> &[ClassifiedVariable<'dae>] {
        &self.variables
    }
}

/// The block-wide facts every DoStep/Startup lowering step reads.
///
/// These five travel together through the whole projection: the checked DAE,
/// its causal definitions, the classified block variables keyed by identity,
/// the name each `pre(x)` state got, and the value-affecting arithmetic profile.
/// They are carried as one value because every lowering step needs the same
/// construction-issued semantic context.
#[derive(Clone, Copy)]
struct BlockLowering<'a, 'dae> {
    view: dae::DaeView<'dae>,
    definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
    by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &'a HashMap<u32, gast::Name>,
    arithmetic: AlgorithmCodeArithmeticProfile,
}

struct ParameterDependencyProof {
    dependent: HashSet<u32>,
    order: Vec<u32>,
}

struct ProjectionParts {
    nominals: Vec<Option<f64>>,
    interface_inputs: Vec<gast::InterfaceVariable>,
    interface_outputs: Vec<gast::InterfaceVariable>,
    interface_parameters: Vec<gast::InterfaceVariable>,
    protected: Vec<gast::ProtectedEntity>,
    startup: Vec<gast::Spanned<gast::Statement>>,
    recalibrate: Vec<gast::Spanned<gast::Statement>>,
    /// Locals of the dependent-parameter statements, which Startup and
    /// Recalibrate both run and therefore both declare.
    startup_locals: Vec<gast::VariableDeclaration>,
    /// User functions reached from dependent-parameter bindings.
    startup_called_user_functions: HashSet<u32>,
    do_step_locals: Vec<gast::VariableDeclaration>,
    /// Generation-time value of each dependent parameter, by DAE variable
    /// index, which is what a folded binding is emitted as.
    dependent_starts: HashMap<u32, gast::Expression>,
    /// Dependent parameters emitted as folded values rather than as calls.
    constant_folded: Vec<rumoca_ir_galec::package::ConstantFoldedParameter>,
    /// The first empty block-storage extent, retained until every reachable
    /// executable expression has passed through its central constructor.
    ///
    /// GALEC cannot declare the empty storage, but a FirstProduct contraction
    /// over that storage has the more precise, occurrence-local rejection
    /// required by the selected arithmetic profile. No partially built block
    /// escapes when this diagnostic remains after executable lowering.
    zero_extent_rejection: Option<GalecTargetError>,
}

/// The only local route from emitted GALEC statements to package closure.
///
/// The whole-`DoStep` call ledger is retained beside the block until the block
/// is consumed by the package constructor. This prevents validation from
/// becoming a discarded side check whose statements could be packaged through
/// an independent path.
struct CommittedBlockEmission {
    block: gast::Block,
    call_ledger: CommittedCallActionLedger,
    function_call_ledgers: Vec<CommittedFunctionCallActionLedger>,
}

struct BlockCompletionRequest<'refs, 'dae, 'origin> {
    issuer: AlgorithmCodePackageIssuer<'origin>,
    view: dae::DaeView<'dae>,
    definitions: &'refs rumoca_phase_structural::CausalDefinitions<'dae>,
    arithmetic: AlgorithmCodeArithmeticProfile,
    block_name: gast::Name,
    period_ref: String,
    zero_extent_rejection: Option<GalecTargetError>,
    call_ledger: CommittedCallActionLedger,
}

impl CommittedBlockEmission {
    fn new(
        block: gast::Block,
        call_ledger: CommittedCallActionLedger,
        function_call_ledgers: Vec<CommittedFunctionCallActionLedger>,
    ) -> Self {
        Self {
            block,
            call_ledger,
            function_call_ledgers,
        }
    }

    fn construct_package<'origin>(
        self,
        issuer: AlgorithmCodePackageIssuer<'origin>,
        metadata: AlgorithmCodePackageMetadata,
    ) -> Result<OriginBoundAlgorithmCodePackage<'origin>, rumoca_ir_galec::package::PackageError>
    {
        self.call_ledger
            .close_package(issuer, self.block, metadata, self.function_call_ledgers)
    }
}

impl ProjectionParts {
    fn protected_declarations(&mut self) -> ProtectedDeclarations<'_> {
        ProtectedDeclarations {
            nominals: &mut self.nominals,
            protected: &mut self.protected,
            startup: &mut self.startup,
        }
    }
}

/// The three parallel outputs one protected declaration contributes to a
/// GALEC block.
///
/// A protected entity, the Startup assignment that seeds it, and its nominal
/// are appended together for every state a lowering pass declares, so the
/// three vectors stay index-aligned only when they are extended as a unit.
struct ProtectedDeclarations<'a> {
    nominals: &'a mut Vec<Option<f64>>,
    protected: &'a mut Vec<gast::ProtectedEntity>,
    startup: &'a mut Vec<gast::Spanned<gast::Statement>>,
}

/// Lower one checked DAE into validated eFMI Algorithm Code.
pub fn lower_to_algorithm_code<'inv>(
    brand: rumoca_core::TargetInvocationBrand<'inv>,
    input: &GalecInput<'_>,
    options: &GalecOptions,
) -> Result<rumoca_ir_galec::TracedAlgorithmCodeProduct<'inv>, Vec<GalecTargetError>> {
    let projected = rumoca_ir_galec::TracedAlgorithmCodeProduct::project_from_origin(
        brand,
        input.dae.source_map(),
        input.model_name,
        |issuer| {
            input.dae.inspect(|view| {
                let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
                let clock = check_admissibility_view(view, &definitions)?;
                lower_view(issuer, input, options, view, &definitions, clock)
            })
        },
    );
    match projected {
        Ok(product) => Ok(product),
        Err(rumoca_ir_galec::AlgorithmCodeOriginProjectionError::Projection(errors)) => Err(errors),
        Err(rumoca_ir_galec::AlgorithmCodeOriginProjectionError::Origin(error)) => {
            Err(vec![GalecTargetError::LoweringInternal {
                detail: format!("failed to close Algorithm Code trace origin: {error}"),
            }])
        }
    }
}

fn lower_view<'dae, 'origin>(
    issuer: AlgorithmCodePackageIssuer<'origin>,
    input: &GalecInput<'_>,
    options: &GalecOptions,
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    clock: AdmittedClock,
) -> Result<OriginBoundAlgorithmCodePackage<'origin>, Vec<GalecTargetError>> {
    let clock_id = admitted_clock_id(view, &clock).map_err(single)?;
    validate_clocks(&clock, view).map_err(single)?;
    let classified = classify_variables(view, definitions)?;
    let by_id = classified
        .iter()
        .map(|variable| (variable.id.index(), variable.clone()))
        .collect::<HashMap<_, _>>();
    let referenced_pre = referenced_pre_variables(view)?;
    let pre_names = build_pre_names(&referenced_pre, &by_id)?;

    let lowering = BlockLowering {
        view,
        definitions,
        by_id: &by_id,
        pre_names: &pre_names,
        arithmetic: options.arithmetic_profile,
    };
    let causal_assignments =
        causal_outputs::CausalAssignmentsPlan::construct(definitions, classified.as_slice())
            .map_err(single)?;
    let mut parts = build_variable_parts(lowering, &classified, &referenced_pre)?;
    let zero_extent_rejection = parts.zero_extent_rejection.take();

    let period_ref = append_clock_period(
        &clock,
        view.clock(clock_id)
            .expect("admitted checked clock resolves")
            .provenance()
            .span(),
        classified.as_slice(),
        &pre_names,
        &mut parts.protected_declarations(),
    )?;

    let mut retained_calls = RetainedCallResults::default();
    let clocked = lower_clock_schedule(
        lowering,
        &clock,
        classified.as_slice(),
        &mut parts.protected_declarations(),
        &mut retained_calls,
    )
    .map_err(single)?;
    let causal = causal_outputs::prepare_causal_assignments(
        lowering,
        &causal_assignments,
        &mut retained_calls,
    )
    .map_err(single)?;
    let call_ledger = CommittedCallActionLedger::construct(
        view,
        clocked.call_actions.into_iter().chain(causal.call_actions),
    )
    .map_err(single)?;
    parts.do_step_locals.extend(clocked.locals);
    parts.do_step_locals.extend(causal.locals);
    let mut do_step = clocked.statements;
    do_step.extend(causal.statements);
    let mut called_user_functions = clocked.called_user_functions;
    called_user_functions.extend(parts.startup_called_user_functions.iter().copied());
    called_user_functions.extend(causal.called_user_functions);
    append_pre_commits(&referenced_pre, &by_id, &pre_names, &mut do_step)?;
    let block_name = crate::mangle::galec_variable_name(
        options.block_name.as_deref().unwrap_or(input.model_name),
    )
    .map_err(single)?;
    complete_lowered_view(
        BlockCompletionRequest {
            issuer,
            view,
            definitions,
            arithmetic: lowering.arithmetic,
            block_name,
            period_ref,
            zero_extent_rejection,
            call_ledger,
        },
        parts,
        do_step,
        called_user_functions,
    )
}

fn complete_lowered_view<'dae, 'origin>(
    request: BlockCompletionRequest<'_, 'dae, 'origin>,
    parts: ProjectionParts,
    do_step: Vec<gast::Spanned<gast::Statement>>,
    called_user_functions: HashSet<u32>,
) -> Result<OriginBoundAlgorithmCodePackage<'origin>, Vec<GalecTargetError>> {
    let mut block = gast::Block::new(request.block_name);
    block.interface = parts
        .interface_inputs
        .into_iter()
        .chain(parts.interface_outputs)
        .chain(parts.interface_parameters)
        .collect();
    block.protected = parts.protected;
    // Startup and Recalibrate run the same dependent-parameter statements, so
    // they declare the same locals.
    block.startup.locals = parts.startup_locals.clone();
    block.startup.statements = parts.startup;
    block.recalibrate.locals = parts.startup_locals;
    block.recalibrate.statements = parts.recalibrate;
    let protected_functions = user_functions::lower_reachable_committed(
        request.view,
        request.definitions,
        called_user_functions,
        request.arithmetic,
    )
    .map_err(single)?;
    block.protected_functions = protected_functions.functions;
    if let Some(error) = request.zero_extent_rejection {
        return Err(single(error));
    }
    block.do_step.locals = parts.do_step_locals;
    block.do_step.statements = do_step;
    CommittedBlockEmission::new(block, request.call_ledger, protected_functions.call_ledgers)
        .construct_package(
            request.issuer,
            AlgorithmCodePackageMetadata::new(
                parts.nominals,
                request.period_ref,
                parts.constant_folded,
                request.arithmetic,
            ),
        )
        .map_err(|error| {
            vec![GalecTargetError::LoweringInternal {
                detail: format!("lowering produced an invalid Algorithm Code package: {error}"),
            }]
        })
}

fn build_variable_parts<'dae>(
    lowering: BlockLowering<'_, 'dae>,
    classified: &ClassifiedVariables<'dae>,
    referenced_pre: &[dae::VariableId<'dae>],
) -> Result<ProjectionParts, Vec<GalecTargetError>> {
    let BlockLowering {
        view,
        by_id,
        pre_names,
        arithmetic,
        ..
    } = lowering;
    let mut evaluator = NumericEvaluator::with_real_matrix_multiply_semantics(
        view,
        arithmetic.real_matrix_multiply(),
    );
    let mut parts = ProjectionParts {
        nominals: Vec::new(),
        interface_inputs: Vec::new(),
        interface_outputs: Vec::new(),
        interface_parameters: Vec::new(),
        protected: Vec::new(),
        startup: Vec::new(),
        recalibrate: Vec::new(),
        startup_locals: Vec::new(),
        startup_called_user_functions: HashSet::new(),
        do_step_locals: Vec::new(),
        dependent_starts: HashMap::new(),
        constant_folded: Vec::new(),
        zero_extent_rejection: None,
    };
    for variable in classified.as_slice() {
        if let Some(error) = zero_extent_rejection(variable) {
            parts.zero_extent_rejection.get_or_insert(error);
            continue;
        }
        append_variable(view, variable, &mut evaluator, &mut parts)?;
    }
    dependent_folding::append_dependent_parameters(
        lowering,
        &classified.dependent_parameter_order,
        &mut parts,
    )
    .map_err(single)?;
    append_previous_states(
        view,
        referenced_pre,
        by_id,
        pre_names,
        &mut evaluator,
        &mut parts.protected_declarations(),
    )?;
    Ok(parts)
}

fn zero_extent_rejection(classified: &ClassifiedVariable<'_>) -> Option<GalecTargetError> {
    classified
        .variable
        .value_type()
        .dimensions()
        .iter()
        .position(|extent| *extent == 0)
        .map(|index| GalecTargetError::NonPositiveDimension {
            variable: classified.variable.name().to_string(),
            dimension: index + 1,
            size: 0,
            span: classified.variable.declaration().span(),
        })
}

fn append_variable<'dae>(
    view: dae::DaeView<'dae>,
    classified: &ClassifiedVariable<'dae>,
    evaluator: &mut NumericEvaluator<'dae>,
    parts: &mut ProjectionParts,
) -> Result<(), Vec<GalecTargetError>> {
    if classified.class == VariableClass::Local {
        parts
            .do_step_locals
            .push(declaration(classified, gast::RangeAttributes::default()));
        return Ok(());
    }
    let projected = build_projected_variable(view, classified, evaluator)?;
    let start = projected.start;
    let declaration = declaration(classified, projected.range);
    match classified.class {
        VariableClass::Input => parts.interface_inputs.push(gast::InterfaceVariable {
            kind: gast::InterfaceKind::Input,
            decl: declaration,
            start: Some(start),
        }),
        VariableClass::Output => {
            parts.interface_outputs.push(gast::InterfaceVariable {
                kind: gast::InterfaceKind::Output,
                decl: declaration,
                start: Some(start.clone()),
            });
            parts.startup.push(initial_assignment(classified, start));
        }
        VariableClass::TunableParameter => {
            parts.interface_parameters.push(gast::InterfaceVariable {
                kind: gast::InterfaceKind::TunableParameter,
                decl: declaration,
                start: Some(start.clone()),
            });
            parts.startup.push(initial_assignment(classified, start));
        }
        VariableClass::DependentParameter => {
            parts
                .dependent_starts
                .insert(classified.id.index(), start.clone());
            parts.protected.push(gast::ProtectedEntity {
                kind: gast::ProtectedKind::DependentParameter,
                decl: declaration,
                start: Some(start),
            });
        }
        VariableClass::Local => unreachable!("causal locals return before state construction"),
        VariableClass::Constant | VariableClass::State => {
            parts.protected.push(gast::ProtectedEntity {
                kind: if classified.class == VariableClass::Constant {
                    gast::ProtectedKind::Constant
                } else {
                    gast::ProtectedKind::State
                },
                decl: declaration,
                start: Some(start.clone()),
            });
            parts.startup.push(initial_assignment(classified, start));
        }
    }
    parts.nominals.push(projected.nominal);
    Ok(())
}

fn admitted_clock_id<'dae>(
    view: dae::DaeView<'dae>,
    clock: &AdmittedClock,
) -> Result<dae::ClockId<'dae>, GalecTargetError> {
    clock
        .domains
        .iter()
        .find(|domain| domain.divisor == 1)
        .and_then(|domain| usize::try_from(domain.clock_index).ok())
        .and_then(|index| view.clock_id(index))
        .ok_or(GalecTargetError::NoPeriodicClock)
}

fn validate_clocks(clock: &AdmittedClock, view: dae::DaeView<'_>) -> Result<(), GalecTargetError> {
    for domain in &clock.domains {
        let id = usize::try_from(domain.clock_index)
            .ok()
            .and_then(|index| view.clock_id(index))
            .expect("admitted clock index resolves");
        let entry = view.clock(id).expect("admitted clock resolves");
        let dae::ClockOperation::Periodic(schedule) = entry.operation() else {
            unreachable!("admissibility retained only periodic clocks")
        };
        let span = entry.provenance().span();
        if !schedule.period_seconds().is_finite() || schedule.period_seconds() <= 0.0 {
            return Err(GalecTargetError::InvalidClockPeriod {
                period_seconds: schedule.period_seconds(),
                span,
            });
        }
        if schedule.phase_seconds() != 0.0 {
            return Err(unsupported(
                "clock-phase",
                format!(
                    "clock phase offset {} s cannot be represented by the eFMI Beta-1 Clock",
                    schedule.phase_seconds()
                ),
                span,
            ));
        }
    }
    Ok(())
}

fn classify_variables<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
) -> Result<ClassifiedVariables<'dae>, Vec<GalecTargetError>> {
    let parameter_dependencies =
        ParameterDependencyProof::derive(view).map_err(|error| vec![error])?;
    let mut variables = Vec::new();
    let mut errors = Vec::new();
    for (id, variable) in view.variables() {
        let causally_defined = definitions.definition_for_variable(id).is_some()
            || definitions.fully_defines_variable(id);
        let classified =
            if causally_defined && variable.causality() != dae::VariableCausality::Output {
                classify_causal_local(id, variable)
            } else {
                classify_variable(id, variable, &parameter_dependencies)
            };
        match classified {
            Ok(classified) => variables.push(classified),
            Err(error) => errors.push(error),
        }
    }
    if errors.is_empty() {
        Ok(ClassifiedVariables {
            variables,
            dependent_parameter_order: parameter_dependencies.order,
        })
    } else {
        Err(errors)
    }
}

fn classify_causal_local<'dae>(
    id: dae::VariableId<'dae>,
    variable: dae::VariableView<'dae>,
) -> Result<ClassifiedVariable<'dae>, GalecTargetError> {
    let span = variable.declaration().span();
    if !matches!(
        variable.role(),
        dae::VariableRole::Algebraic | dae::VariableRole::Output
    ) || variable.causality() != dae::VariableCausality::Local
    {
        return Err(GalecTargetError::UnclassifiableVariable {
            variable: variable.name().to_string(),
            causality: causality_name(variable.causality()),
            partition: role_name(variable.role()),
            origin: origin_name(variable.origin()),
            span,
        });
    }
    Ok(ClassifiedVariable {
        id,
        variable,
        class: VariableClass::Local,
        scalar_type: scalar_type(
            variable.value_type().scalar_type(),
            variable.name().as_str(),
            span,
        )?,
        name: with_span(
            crate::mangle::galec_variable_name(variable.name().as_str())?,
            span,
        ),
    })
}

fn classify_variable<'dae>(
    id: dae::VariableId<'dae>,
    variable: dae::VariableView<'dae>,
    parameter_dependencies: &ParameterDependencyProof,
) -> Result<ClassifiedVariable<'dae>, GalecTargetError> {
    let span = variable.declaration().span();
    let class = match (variable.causality(), variable.role()) {
        (dae::VariableCausality::Input, _) => VariableClass::Input,
        (dae::VariableCausality::Output, _) => VariableClass::Output,
        (_, dae::VariableRole::Constant) => VariableClass::Constant,
        (_, dae::VariableRole::Parameter)
            if parameter_dependencies.dependent.contains(&id.index()) =>
        {
            VariableClass::DependentParameter
        }
        (_, dae::VariableRole::Parameter) if variable.is_tunable() => {
            VariableClass::TunableParameter
        }
        (_, dae::VariableRole::Parameter) => VariableClass::Constant,
        (
            dae::VariableCausality::Local,
            dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue,
        ) => VariableClass::State,
        _ => {
            return Err(GalecTargetError::UnclassifiableVariable {
                variable: variable.name().to_string(),
                causality: causality_name(variable.causality()),
                partition: role_name(variable.role()),
                origin: origin_name(variable.origin()),
                span,
            });
        }
    };
    let scalar_type = scalar_type(
        variable.value_type().scalar_type(),
        variable.name().as_str(),
        span,
    )?;
    let name = with_span(
        crate::mangle::galec_variable_name(variable.name().as_str())?,
        span,
    );
    Ok(ClassifiedVariable {
        id,
        variable,
        class,
        scalar_type,
        name,
    })
}

impl ParameterDependencyProof {
    fn derive(view: dae::DaeView<'_>) -> Result<Self, GalecTargetError> {
        let parameter_ids: HashSet<u32> = view
            .variables()
            .filter(|(_, variable)| variable.role() == dae::VariableRole::Parameter)
            .map(|(id, _)| id.index())
            .collect();
        let (dependent, dependencies) = Self::collect_dependencies(view, &parameter_ids);
        let order = Self::topological_order(view, &dependent, &dependencies)?;
        Ok(Self { dependent, order })
    }

    /// Map each dependent parameter to the parameters its definition reads.
    ///
    /// A parameter is dependent when it is a calculated parameter or when its
    /// binding reads another parameter; those are the two forms that must be
    /// recomputed in Recalibrate rather than frozen at Startup.
    fn collect_dependencies<'dae>(
        view: dae::DaeView<'dae>,
        parameter_ids: &HashSet<u32>,
    ) -> (HashSet<u32>, HashMap<u32, HashSet<u32>>) {
        let mut dependencies: HashMap<u32, HashSet<u32>> = HashMap::new();
        let mut dependent = HashSet::new();
        for (id, variable) in view.variables() {
            if variable.role() != dae::VariableRole::Parameter {
                continue;
            }
            let calculated = variable.causality() == dae::VariableCausality::CalculatedParameter;
            let definition = if calculated {
                variable.binding().or(variable.start())
            } else {
                variable.binding()
            };
            let direct = definition.map_or_else(HashSet::new, |definition| {
                Self::direct_parameter_references(view, definition, parameter_ids)
            });
            if calculated || !direct.is_empty() {
                dependent.insert(id.index());
                dependencies.insert(id.index(), direct);
            }
        }
        (dependent, dependencies)
    }

    /// Collect the parameters that `definition` reads directly.
    fn direct_parameter_references<'dae>(
        view: dae::DaeView<'dae>,
        definition: dae::ExprId<'dae>,
        parameter_ids: &HashSet<u32>,
    ) -> HashSet<u32> {
        let mut direct = HashSet::new();
        dae::for_each_expression(view, definition, |_, expression| {
            if let Some(reference) = expression.variable_coordinate()
                && parameter_ids.contains(&reference.index())
            {
                direct.insert(reference.index());
            }
        });
        direct
    }

    /// Order dependent parameters so each definition follows everything it
    /// reads, rejecting a cycle by naming one parameter on it.
    fn topological_order(
        view: dae::DaeView<'_>,
        dependent: &HashSet<u32>,
        dependencies: &HashMap<u32, HashSet<u32>>,
    ) -> Result<Vec<u32>, GalecTargetError> {
        let mut order = Vec::with_capacity(dependent.len());
        let mut emitted = HashSet::new();
        while order.len() < dependent.len() {
            let before = order.len();
            Self::emit_ready_pass(view, dependent, dependencies, &mut emitted, &mut order);
            if order.len() == before {
                return Err(GalecTargetError::StartDependencyCycle {
                    through: Self::cycle_witness(view, dependent, &emitted),
                });
            }
        }
        Ok(order)
    }

    /// Append every dependent parameter whose reads are already ordered.
    fn emit_ready_pass(
        view: dae::DaeView<'_>,
        dependent: &HashSet<u32>,
        dependencies: &HashMap<u32, HashSet<u32>>,
        emitted: &mut HashSet<u32>,
        order: &mut Vec<u32>,
    ) {
        for (id, _) in view.variables() {
            let raw = id.index();
            if dependent.contains(&raw)
                && !emitted.contains(&raw)
                && Self::is_ready(raw, dependent, dependencies, emitted)
            {
                emitted.insert(raw);
                order.push(raw);
            }
        }
    }

    /// True when every dependent parameter `raw` reads has already been ordered.
    fn is_ready(
        raw: u32,
        dependent: &HashSet<u32>,
        dependencies: &HashMap<u32, HashSet<u32>>,
        emitted: &HashSet<u32>,
    ) -> bool {
        let Some(direct) = dependencies.get(&raw) else {
            return true;
        };
        direct
            .iter()
            .all(|dependency| !dependent.contains(dependency) || emitted.contains(dependency))
    }

    /// Name one still-unordered dependent parameter, for the cycle diagnostic.
    fn cycle_witness(
        view: dae::DaeView<'_>,
        dependent: &HashSet<u32>,
        emitted: &HashSet<u32>,
    ) -> String {
        view.variables()
            .find(|(id, _)| dependent.contains(&id.index()) && !emitted.contains(&id.index()))
            .map_or_else(
                || "<unknown>".to_owned(),
                |(_, variable)| variable.name().to_string(),
            )
    }
}

fn scalar_type(
    scalar: dae::ScalarType,
    name: &str,
    span: Span,
) -> Result<gast::ScalarType, GalecTargetError> {
    match scalar {
        dae::ScalarType::Real => Ok(gast::ScalarType::Real),
        dae::ScalarType::Integer => Ok(gast::ScalarType::Integer),
        dae::ScalarType::Boolean => Ok(gast::ScalarType::Boolean),
        // MLS §4.9.5: an enumeration value IS its 1-based ordinal, and the DAE
        // has already rewritten every enumeration literal to that ordinal
        // (`lower/expression_helpers.rs`, `DaeLiteral::Enumeration`). So the
        // GALEC representation is simply `Integer`, which makes comparison,
        // assignment, `pre()` and array subscripting work through the existing
        // integer paths.
        //
        // LIMITATION: this is an *unbounded* Integer. `dae::ScalarType::
        // Enumeration` is a unit variant and `dae::ValueType` carries no
        // enumeration identity or literal count, so the cardinality that would
        // give this declaration `min = 1` / `max = <last ordinal>` is not
        // available here. Nothing therefore constrains the ordinal to its
        // enumeration's range, and the implicit `limit self` boundary
        // saturation has no bounds to clamp against. Closing that gap needs the
        // cardinality plumbed onto the DAE value type; it is tracked separately.
        dae::ScalarType::Enumeration => Ok(gast::ScalarType::Integer),
        dae::ScalarType::String => Err(unsupported(
            "string-variable",
            format!("String variable `{name}` has no GALEC scalar type"),
            span,
        )),
        dae::ScalarType::Record => Err(unsupported(
            "record-value",
            format!("record value `{name}` requires a checked field projection"),
            span,
        )),
    }
}

struct ProjectedVariable {
    start: gast::Expression,
    range: gast::RangeAttributes,
    nominal: Option<f64>,
}

fn build_projected_variable<'dae>(
    view: dae::DaeView<'dae>,
    classified: &ClassifiedVariable<'dae>,
    evaluator: &mut NumericEvaluator<'dae>,
) -> Result<ProjectedVariable, Vec<GalecTargetError>> {
    let shape = StartShape::checked(classified.variable).map_err(single)?;
    let values = initial_values(view, classified, evaluator).map_err(single)?;
    let (start, range, nominal) = match classified.scalar_type {
        gast::ScalarType::Real => (
            shape.real(values).map_err(single)?,
            gast::RangeAttributes {
                min: optional_real(evaluator, classified.variable.minimum())
                    .map_err(single)?
                    .map(gast::Expression::Real),
                max: optional_real(evaluator, classified.variable.maximum())
                    .map_err(single)?
                    .map(gast::Expression::Real),
            },
            optional_real(evaluator, classified.variable.nominal()).map_err(single)?,
        ),
        gast::ScalarType::Integer => (
            shape.integer(values).map_err(single)?,
            gast::RangeAttributes {
                min: optional_integer(view, evaluator, classified.variable.minimum())
                    .map_err(single)?
                    .map(gast::Expression::Integer),
                max: optional_integer(view, evaluator, classified.variable.maximum())
                    .map_err(single)?
                    .map(gast::Expression::Integer),
            },
            None,
        ),
        gast::ScalarType::Boolean => {
            if classified.variable.minimum().is_some()
                || classified.variable.maximum().is_some()
                || classified.variable.nominal().is_some()
            {
                return Err(vec![GalecTargetError::AttributeTypeMismatch {
                    variable: classified.variable.name().to_string(),
                    attribute: "numeric bound",
                    expected: "Boolean",
                    found: "numeric",
                    span: Some(classified.variable.declaration().span()),
                }]);
            }
            (
                shape.boolean(values).map_err(single)?,
                gast::RangeAttributes::default(),
                None,
            )
        }
    };
    Ok(ProjectedVariable {
        start,
        range,
        nominal,
    })
}

fn initial_values<'dae>(
    view: dae::DaeView<'dae>,
    classified: &ClassifiedVariable<'dae>,
    evaluator: &mut NumericEvaluator<'dae>,
) -> Result<StartValues, GalecTargetError> {
    let expression = match classified.variable.role() {
        dae::VariableRole::Parameter | dae::VariableRole::Constant => classified
            .variable
            .binding()
            .or(classified.variable.start()),
        _ => classified
            .variable
            .start()
            .or(classified.variable.binding()),
    };
    let Some(expression) = expression else {
        return Ok(StartValues::shaped(
            vec![default_scalar(classified.scalar_type); classified.variable.scalar_count()],
            classified.variable.declaration().span(),
        ));
    };
    let values = evaluator.expression(expression).map_err(|error| {
        GalecTargetError::AttributeNotEvaluable {
            variable: classified.variable.name().to_string(),
            attribute: "start",
            reason: error.to_string(),
            span: Some(error.span()),
        }
    })?;
    let expression =
        view.expression(expression)
            .ok_or_else(|| GalecTargetError::AttributeNotEvaluable {
                variable: classified.variable.name().to_string(),
                attribute: "start",
                reason: "checked initial expression identity does not resolve".to_owned(),
                span: Some(classified.variable.declaration().span()),
            })?;
    StartValues::evaluated(
        values,
        expression.value_type().is_scalar(),
        classified.variable.name(),
        expression.provenance().span(),
    )
}

const fn default_scalar(scalar: gast::ScalarType) -> f64 {
    match scalar {
        gast::ScalarType::Real | gast::ScalarType::Integer | gast::ScalarType::Boolean => 0.0,
    }
}

fn declaration(
    classified: &ClassifiedVariable<'_>,
    range: gast::RangeAttributes,
) -> gast::VariableDeclaration {
    gast::VariableDeclaration {
        ty: gast::TypeRef::Primitive(classified.scalar_type),
        name: classified.name.clone(),
        dimensions: classified
            .variable
            .value_type()
            .dimensions()
            .iter()
            .map(|extent| gast::Dimension::Expr(gast::Expression::Integer(i64::from(*extent))))
            .collect(),
        range,
        span: classified.variable.declaration().span(),
    }
}

fn initial_assignment(
    classified: &ClassifiedVariable<'_>,
    value: gast::Expression,
) -> gast::Spanned<gast::Statement> {
    gast::Spanned::new(
        gast::Statement::Assignment {
            target: state_reference(
                classified.name.clone(),
                classified.variable.declaration().span(),
            ),
            value,
        },
        classified
            .variable
            .start()
            .map_or(classified.variable.declaration().span(), |_| {
                classified.variable.declaration().span()
            }),
    )
}

fn build_pre_names<'dae>(
    referenced: &[dae::VariableId<'dae>],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
) -> Result<HashMap<u32, gast::Name>, Vec<GalecTargetError>> {
    referenced
        .iter()
        .map(|id| {
            let base = by_id.get(&id.index()).ok_or_else(|| {
                vec![GalecTargetError::UnknownVariableReference {
                    name: format!("#{}", id.index()),
                    span: None,
                }]
            })?;
            crate::mangle::pre_state_name(base.variable.name().as_str())
                .map(|name| {
                    (
                        id.index(),
                        with_span(name, base.variable.declaration().span()),
                    )
                })
                .map_err(single)
        })
        .collect()
}

fn append_previous_states<'dae>(
    view: dae::DaeView<'dae>,
    referenced: &[dae::VariableId<'dae>],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    evaluator: &mut NumericEvaluator<'dae>,
    declarations: &mut ProtectedDeclarations<'_>,
) -> Result<(), Vec<GalecTargetError>> {
    for id in referenced {
        let base = by_id
            .get(&id.index())
            .expect("pre variable was resolved while collecting names");
        let name = pre_names
            .get(&id.index())
            .expect("pre variable name was constructed")
            .clone();
        let mut previous = base.clone();
        previous.name = name;
        previous.class = VariableClass::State;
        let projected = build_projected_variable(view, &previous, evaluator)?;
        let start = projected.start;
        let mut decl = declaration(&previous, projected.range);
        decl.name = previous.name.clone();
        declarations.protected.push(gast::ProtectedEntity {
            kind: gast::ProtectedKind::State,
            decl,
            start: Some(start.clone()),
        });
        declarations
            .startup
            .push(initial_assignment(&previous, start));
        declarations.nominals.push(projected.nominal);
    }
    Ok(())
}

fn append_clock_period(
    clock: &AdmittedClock,
    span: Span,
    classified: &[ClassifiedVariable<'_>],
    pre_names: &HashMap<u32, gast::Name>,
    declarations: &mut ProtectedDeclarations<'_>,
) -> Result<String, Vec<GalecTargetError>> {
    let mut candidate = "samplePeriod".to_owned();
    let mut suffix = 0usize;
    while classified
        .iter()
        .any(|variable| crate::mangle::name_lexeme(&variable.name) == candidate)
        || pre_names
            .values()
            .any(|name| crate::mangle::name_lexeme(name) == candidate)
        || rumoca_ir_galec::builtins::is_reserved_name(&candidate)
    {
        suffix += 1;
        candidate = format!("clockSamplePeriod{suffix}");
    }
    let name = with_span(
        crate::mangle::galec_variable_name(&candidate).map_err(single)?,
        span,
    );
    declarations.nominals.push(None);
    let declaration = gast::VariableDeclaration {
        ty: gast::TypeRef::Primitive(gast::ScalarType::Real),
        name: name.clone(),
        dimensions: Vec::new(),
        range: gast::RangeAttributes::default(),
        span,
    };
    declarations.protected.push(gast::ProtectedEntity {
        kind: gast::ProtectedKind::Constant,
        decl: declaration,
        start: Some(gast::Expression::Real(clock.period_seconds)),
    });
    declarations.startup.push(gast::Spanned::new(
        gast::Statement::Assignment {
            target: state_reference(name.clone(), span),
            value: gast::Expression::Real(clock.period_seconds),
        },
        span,
    ));
    Ok(name.lexeme().to_owned())
}

fn append_pre_commits<'dae>(
    referenced: &[dae::VariableId<'dae>],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    do_step: &mut Vec<gast::Spanned<gast::Statement>>,
) -> Result<(), Vec<GalecTargetError>> {
    for id in referenced {
        let base = by_id.get(&id.index()).ok_or_else(|| {
            vec![GalecTargetError::UnknownVariableReference {
                name: format!("#{}", id.index()),
                span: None,
            }]
        })?;
        let span = base.variable.declaration().span();
        do_step.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: state_reference(
                    pre_names
                        .get(&id.index())
                        .expect("pre name constructed")
                        .clone(),
                    span,
                ),
                value: gast::Expression::Ref(state_reference(base.name.clone(), span)),
            },
            span,
        ));
    }
    Ok(())
}

/// The clock domain one event guard is lowered against.
///
/// A guard is admitted only when every clock it names is the clock whose
/// `DoStep` runs the action, so the whole guard tree is walked against one
/// expected clock, one checked view that resolves its conditions, one lowerer
/// that records the locals the guard emits, and one span its diagnostics
/// carry.
struct ActionGuardContext<'a, 'b, 'dae> {
    view: dae::DaeView<'dae>,
    expected: dae::ClockId<'dae>,
    lowerer: &'a mut ExpressionLowerer<'b, 'dae>,
    span: Span,
}

fn lower_action_guard<'dae>(
    context: &mut ActionGuardContext<'_, '_, 'dae>,
    guard: dae::ConditionId<'dae>,
) -> Result<Option<gast::Expression>, GalecTargetError> {
    let view = context.view;
    let span = context.span;
    match view
        .condition(guard)
        .expect("checked event guard resolves")
        .operation()
    {
        dae::ConditionOperation::Initial => Err(unsupported(
            "initial-event-guard",
            "initial() event actions are outside a periodic DoStep clock".to_owned(),
            span,
        )),
        dae::ConditionOperation::Clock(found) if found == context.expected => Ok(None),
        dae::ConditionOperation::Clock(_) => Err(unsupported(
            "multiple-clock-event-guard",
            "event action combines distinct clock domains".to_owned(),
            span,
        )),
        dae::ConditionOperation::Relation(relation) => {
            let relation = view
                .relation(relation)
                .expect("checked relation identity resolves");
            let expression = context.lowerer.lower(relation.expression())?;
            require_boolean(&expression, span)?;
            Ok(Some(expression.expression))
        }
        dae::ConditionOperation::Discrete(expression) => {
            let expression = context.lowerer.lower(expression)?;
            require_boolean(&expression, span)?;
            Ok(Some(expression.expression))
        }
        dae::ConditionOperation::Not(condition) => {
            let condition = lower_action_guard(context, condition)?;
            condition
                .map(|condition| Some(gast::Expression::Not(Box::new(condition))))
                .ok_or_else(|| {
                    unsupported(
                        "negated-clock-event-guard",
                        "an admitted clock cannot be negated inside its own DoStep".to_owned(),
                        span,
                    )
                })
        }
        dae::ConditionOperation::And(lhs, rhs) => {
            combine_action_guards(context, lhs, rhs, gast::BinaryOp::And)
        }
        dae::ConditionOperation::Or(lhs, rhs) => {
            combine_action_guards(context, lhs, rhs, gast::BinaryOp::Or)
        }
        // A vector activation is `edge(b1) or … or edge(bn)`, and GALEC has no
        // per-element activation buffer to build those edges from. Lowering the
        // disjunction of *levels* here would fire the action whenever any
        // element is merely true, so it is refused rather than approximated.
        dae::ConditionOperation::AnyRise(_, _) => Err(unsupported(
            "vector-activation-event-guard",
            "a vector `when {…}` activation has no admitted per-element edge in a DoStep clock"
                .to_owned(),
            span,
        )),
        // An unguarded algorithm section and a section-level `assert` run
        // whenever the section runs, so their guard adds nothing to the clock.
        dae::ConditionOperation::Always => Ok(None),
    }
}

fn combine_action_guards<'dae>(
    context: &mut ActionGuardContext<'_, '_, 'dae>,
    lhs: dae::ConditionId<'dae>,
    rhs: dae::ConditionId<'dae>,
    operator: gast::BinaryOp,
) -> Result<Option<gast::Expression>, GalecTargetError> {
    let lhs = lower_action_guard(context, lhs)?;
    let rhs = lower_action_guard(context, rhs)?;
    Ok(match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => Some(gast::Expression::binary(operator, lhs, rhs)),
        (Some(expression), None) | (None, Some(expression)) if operator == gast::BinaryOp::And => {
            Some(expression)
        }
        (None, _) | (_, None) => None,
    })
}

/// A speculative pass over a loop body clones the whole lowerer, so the trial
/// leaves no temporary, cache entry or emitted statement behind.
#[derive(Clone)]
struct ExpressionLowerer<'a, 'dae> {
    view: dae::DaeView<'dae>,
    by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &'a HashMap<u32, gast::Name>,
    definitions: &'a rumoca_phase_structural::CausalDefinitions<'dae>,
    call_frames: Vec<CallFrame<'dae>>,
    function_fold_values: Vec<(dae::FunctionFoldId<'dae>, Vec<Vec<TypedExpression>>)>,
    function_fold_output_cache: HashMap<FunctionFoldOutputKey, TypedExpression>,
    scalar_projection_cache: HashMap<ScalarProjectionKey, TypedExpression>,
    function_fold_projection_cache: HashMap<u32, bool>,
    comprehension_frames: Vec<ComprehensionFrame>,
    /// Construction-issued identities for the exact finite/runtime points
    /// whose binders have been projected by this lowerer.
    ///
    /// The interner compares the checked owner, the complete enclosing point
    /// path, and the structured GALEC binder expressions. Cache keys carry
    /// only the resulting private id, so a call/effect cache cannot represent
    /// reuse across two different fold or comprehension points.
    iteration_points: Vec<IterationPointIdentity>,
    loop_index_bounds: Vec<LoopIndexBound>,
    conditional_activation_path: Vec<ConditionalActivationKey>,
    /// Exact projection decisions interned by checked owner and structured
    /// coordinate. The carried id is additionally namespaced by the semantic
    /// owner of this lowerer, so activation facts from two clock domains or a
    /// clock and the causal region cannot collide in the committed ledger.
    selection_points: Vec<SelectionPointIdentity>,
    /// Sequential statement epoch. Equal predicate expressions correlate only
    /// within one atomic statement/group; after a store, the same coordinate
    /// expression may read a different runtime value.
    selection_epoch: u32,
    materialize_function_values: bool,
    inline_causal_locals: bool,
    /// Value-affecting arithmetic selected before this semantic phase begins.
    /// Every contraction reads this field; no contraction constructor owns a
    /// fallback relation.
    arithmetic: AlgorithmCodeArithmeticProfile,
    conditional_depth: usize,
    materialized_function_values: HashMap<MaterializedFunctionValueKey, gast::Name>,
    materialized_function_calls: HashMap<MaterializedFunctionCallKey, Vec<gast::Name>>,
    imported_materialized_call_regions: HashMap<MaterializedFunctionCallKey, EmissionRegion>,
    retained_call_dependencies: HashSet<EmissionRegion>,
    call_argument_read_captures: Vec<HashSet<u32>>,
    materialized_shared_record_fields: HashMap<SharedRecordFieldKey, gast::Expression>,
    /// Index-list entries of an array-update target already bound to a local.
    ///
    /// Keyed by the entry expression so every coordinate of the same update,
    /// and every update reading the array it produces, shares one evaluation.
    array_update_index_locals: HashMap<ArrayUpdateIndexKey, gast::Name>,
    /// Proven ranges of scalar Integer function locals, keyed by lexeme.
    ///
    /// The map is the ranges that hold at the current point of the function
    /// body. A straight-line assignment replaces the entry for its target; a
    /// conditional replaces the entry for each local it writes with the union
    /// over its reaching definitions ([`ConditionalIntegerBounds`]); a loop
    /// drops the entries of the locals its body may write. So an entry always
    /// bounds every value a reader can observe, and a local read before its
    /// assignment simply has no entry and is refused.
    local_integer_bounds: LocalIntegerBounds,
    /// Primitive expression values already committed to a function local
    /// ([`AssignedPrimitives`]).
    assigned_primitive_expressions: AssignedPrimitives,
    /// Pure aggregate definitions whose checked local storage is unnecessary.
    structural_function_locals: structural_locals::StructuralFunctionLocals<'dae>,
    called_user_functions: HashSet<u32>,
    /// Construction-issued call owners evaluated by the current root lowering.
    ///
    /// Only calls reached outside an entered function body are recorded. A
    /// call inside an inlined function is owned by that enclosing source call;
    /// treating the function-body identity as a model-level invocation would
    /// conflate distinct calls of the same function.
    evaluated_root_call_actions: Vec<RootCallAction>,
    /// Source-call capabilities reached by the real root lowering. Kept
    /// separately from emitted actions so deleting or dropping an action
    /// cannot make an incomplete ledger appear complete.
    expected_root_call_actions: HashSet<ExpectedRootCallAction>,
    /// Dominating evaluation capabilities consumed by reached memo hits.
    reused_root_call_actions: Vec<RootCallReuse>,
    /// Exact emitted actions that justify each materialized memo fact.
    materialized_call_sources: HashMap<MaterializedFunctionCallKey, HashSet<CallExecutionSource>>,
    /// Complete call paths committed while emitting one protected function.
    ///
    /// This is separate from model-root ownership: an inner expression in a
    /// substituted body belongs to the enclosing model call during `DoStep`,
    /// but it is an independently checked action when that protected function
    /// body itself is emitted.
    evaluated_function_call_actions: Vec<FunctionCallAction>,
    expected_function_call_actions: HashSet<FunctionCallAction>,
    reused_function_call_actions: Vec<FunctionCallReuse>,
    function_scope: Option<dae::FunctionId<'dae>>,
    temporary_locals: Vec<gast::VariableDeclaration>,
    temporary_counter: usize,
    temporary_namespace: TemporaryNamespace<'dae>,
    capture_assertions: bool,
    seen_assertion_calls: HashSet<FunctionAssertionCallKey>,
    pending_prefix_statements: Vec<gast::Spanned<gast::Statement>>,
    /// Lazily-built index from a classified block variable's GALEC name to its
    /// declared shape. `by_id` is keyed by variable identity, but a `gast`
    /// state reference carries only the name — this index is built once, on
    /// the first shape query, instead of scanning `by_id` per query.
    state_shapes_by_name: Option<HashMap<String, (Vec<u32>, gast::ScalarType)>>,
}

#[derive(Clone)]
struct CallFrame<'dae> {
    call: dae::ExprId<'dae>,
    owner: dae::ExprId<'dae>,
    function: dae::FunctionId<'dae>,
    arguments: Vec<dae::ExprId<'dae>>,
    /// Actual arguments evaluated exactly once, before entering the function.
    ///
    /// Keeping the evaluated values in the frame makes eager Modelica call
    /// semantics structural: an unused formal cannot erase an effectful actual,
    /// and multiple formal reads cannot re-evaluate the actual.
    prepared_arguments: Vec<PreparedInlineArgument>,
    indices: Vec<Option<i64>>,
}

#[derive(Clone)]
enum PreparedInlineArgument {
    Primitive(PreparedInlineValue),
    Record(Vec<PreparedInlineValue>),
}

#[derive(Clone)]
struct PreparedInlineValue {
    expression: gast::Expression,
    dimensions: Vec<u32>,
    scalar_type: gast::ScalarType,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionAssertionCallKey {
    path: Vec<FunctionAssertionCallSite>,
    iteration_path: Vec<IterationPointId>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionAssertionCallSite {
    owner: u32,
    function: u32,
    arguments: Vec<u32>,
    indices: Vec<Option<i64>>,
    span: Span,
}

#[derive(Clone)]
struct ComprehensionFrame {
    domain: u32,
    binders: Vec<gast::Expression>,
    point: IterationPointId,
}

/// Private identity issued only by [`ExpressionLowerer::enter_iteration_point`].
///
/// Callers cannot manufacture an ordinal and thereby claim two iteration
/// points are equal: the id is interned from the checked iteration owner, its
/// parent path, and the exact binder projection.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct IterationPointId(u32);

#[derive(Clone, Copy, PartialEq, Eq)]
enum IterationOwner {
    Comprehension { expression: u32 },
    FunctionFold { function: u32, fold: u32 },
}

#[derive(Clone, PartialEq)]
struct IterationPointIdentity {
    id: IterationPointId,
    parent: Vec<IterationPointId>,
    owner: IterationOwner,
    binders: Vec<gast::Expression>,
}

#[derive(Clone)]
struct LoopIndexBound {
    name: gast::Name,
    minimum: i64,
    maximum: i64,
}

#[derive(Clone, PartialEq)]
struct SelectionPointIdentity {
    id: SelectionPointId,
    kind: ConditionalActivationKind,
    expression: Option<u32>,
    operands: Vec<u32>,
    epoch: u32,
    iteration_path: Vec<IterationPointId>,
    coordinates: Vec<gast::Expression>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct MaterializedFunctionValueKey {
    call_path: Vec<MaterializedCallKey>,
    iteration_path: Vec<IterationPointId>,
    function: u32,
    definition: u32,
    indices: Vec<i64>,
    fields: Vec<u32>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct MaterializedCallKey {
    owner: u32,
    function: u32,
    arguments: Vec<u32>,
    indices: Vec<Option<i64>>,
    iteration_path: Vec<IterationPointId>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct SharedRecordFieldKey {
    iteration_path: Vec<IterationPointId>,
    expression: u32,
    field: usize,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct ArrayUpdateIndexKey {
    iteration_path: Vec<IterationPointId>,
    expression: u32,
}

#[derive(Clone)]
struct ConditionalMaterializationSnapshot {
    function_values: HashMap<MaterializedFunctionValueKey, gast::Name>,
    function_calls: HashMap<MaterializedFunctionCallKey, Vec<gast::Name>>,
    call_sources: HashMap<MaterializedFunctionCallKey, HashSet<CallExecutionSource>>,
    fold_outputs: HashMap<FunctionFoldOutputKey, TypedExpression>,
    seen_assertions: HashSet<FunctionAssertionCallKey>,
    assigned_primitive_expressions: AssignedPrimitiveSnapshot,
}

struct MaterializedConditional<'a, 'dae> {
    operands: dae::ExpressionOperands<'dae>,
    indices: &'a [gast::Expression],
    scalar_type: gast::ScalarType,
    target: &'a gast::Name,
    activation_operands: Vec<u32>,
    selection: SelectionPointId,
    span: Span,
}

#[derive(Clone)]
struct TypedExpression {
    expression: gast::Expression,
    scalar_type: gast::ScalarType,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionFoldOutputKey {
    call_path: Vec<MaterializedCallKey>,
    iteration_path: Vec<IterationPointId>,
    fold: u32,
    carried: u32,
    scalar: u32,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct ScalarProjectionKey {
    call_path: Vec<MaterializedCallKey>,
    iteration_path: Vec<IterationPointId>,
    expression: u32,
    indices: Vec<i64>,
}

#[cfg(test)]
mod tests;
