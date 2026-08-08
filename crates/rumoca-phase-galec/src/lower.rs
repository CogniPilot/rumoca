//! Checked DAE → [`AlgorithmCodePackage`] lowering.
//!
//! This projection reads the immutable branded DAE directly. Periodic clock
//! guards become the implicit `DoStep` tick; guarded assignments are ordered
//! by their current-tick dependencies; `pre(x)` becomes a protected
//! `'previous(x)'` state committed after all assignments.

mod causal_outputs;
mod clock_schedule;
mod clocked_assignments;
mod expression_functions;
mod expression_helpers;
mod expression_projection;
mod pre_references;
mod start;
mod user_functions;

use std::collections::{HashMap, HashSet};

use rumoca_core::Span;
use rumoca_eval_dae::NumericEvaluator;
use rumoca_ir_dae as dae;
use rumoca_ir_galec::ast as gast;

use crate::admissibility::{AdmittedClock, check_admissibility};
use crate::diagnostic::GalecTargetError;
use crate::input::{GalecInput, GalecOptions};
use rumoca_ir_galec::package::AlgorithmCodePackage;

use clock_schedule::lower_clock_schedule;
use expression_helpers::*;
use pre_references::referenced_pre_variables;
use start::{StartShape, StartValues};

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

struct ProjectionParts {
    nominals: Vec<Option<f64>>,
    interface_inputs: Vec<gast::InterfaceVariable>,
    interface_outputs: Vec<gast::InterfaceVariable>,
    interface_parameters: Vec<gast::InterfaceVariable>,
    protected: Vec<gast::ProtectedEntity>,
    startup: Vec<gast::Spanned<gast::Statement>>,
    recalibrate: Vec<gast::Spanned<gast::Statement>>,
    do_step_locals: Vec<gast::VariableDeclaration>,
}

/// Lower one checked DAE into validated eFMI Algorithm Code.
pub fn lower_to_algorithm_code(
    input: &GalecInput<'_>,
    options: &GalecOptions,
) -> Result<AlgorithmCodePackage, Vec<GalecTargetError>> {
    let clock = check_admissibility(input)?;
    input
        .dae
        .inspect(|view| lower_view(input, options, view, clock))
}

fn lower_view<'dae>(
    input: &GalecInput<'_>,
    options: &GalecOptions,
    view: dae::DaeView<'dae>,
    clock: AdmittedClock,
) -> Result<AlgorithmCodePackage, Vec<GalecTargetError>> {
    let clock_id = admitted_clock_id(view, &clock).map_err(single)?;
    validate_clocks(&clock, view).map_err(single)?;
    let classified = classify_variables(view)?;
    let by_id = classified
        .iter()
        .map(|variable| (variable.id.index(), variable.clone()))
        .collect::<HashMap<_, _>>();
    let referenced_pre = referenced_pre_variables(view)?;
    let pre_names = build_pre_names(&referenced_pre, &by_id)?;

    let mut parts = build_variable_parts(view, &classified, &by_id, &referenced_pre, &pre_names)?;

    let period_ref = append_clock_period(
        &clock,
        view.clock(clock_id)
            .expect("admitted checked clock resolves")
            .provenance()
            .span(),
        &classified,
        &pre_names,
        &mut parts.nominals,
        &mut parts.protected,
        &mut parts.startup,
    )?;

    let clocked = lower_clock_schedule(
        view,
        &clock,
        &classified,
        &by_id,
        &pre_names,
        &mut parts.nominals,
        &mut parts.protected,
        &mut parts.startup,
    )
    .map_err(single)?;
    parts.do_step_locals.extend(clocked.locals);
    let mut do_step = clocked.statements;
    let mut called_user_functions = clocked.called_user_functions;
    called_user_functions.extend(
        causal_outputs::append_causal_assignments(
            view,
            &classified,
            &by_id,
            &pre_names,
            &mut parts.do_step_locals,
            &mut do_step,
        )
        .map_err(single)?,
    );
    append_pre_commits(&referenced_pre, &by_id, &pre_names, &mut do_step)?;
    let block_name = crate::mangle::galec_variable_name(
        options.block_name.as_deref().unwrap_or(input.model_name),
    )
    .map_err(single)?;
    let mut block = gast::Block::new(block_name);
    block.interface = parts
        .interface_inputs
        .into_iter()
        .chain(parts.interface_outputs)
        .chain(parts.interface_parameters)
        .collect();
    block.protected = parts.protected;
    block.startup.statements = parts.startup;
    block.recalibrate.statements = parts.recalibrate;
    block.protected_functions =
        user_functions::lower_reachable(view, called_user_functions).map_err(single)?;
    block.do_step.locals = parts.do_step_locals;
    block.do_step.statements = do_step;
    AlgorithmCodePackage::construct(block, parts.nominals, &period_ref).map_err(|error| {
        vec![GalecTargetError::LoweringInternal {
            detail: format!("lowering produced an invalid Algorithm Code package: {error}"),
        }]
    })
}

fn build_variable_parts<'dae>(
    view: dae::DaeView<'dae>,
    classified: &[ClassifiedVariable<'dae>],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    referenced_pre: &[dae::VariableId<'dae>],
    pre_names: &HashMap<u32, gast::Name>,
) -> Result<ProjectionParts, Vec<GalecTargetError>> {
    let mut evaluator = NumericEvaluator::new(view);
    let mut parts = ProjectionParts {
        nominals: Vec::new(),
        interface_inputs: Vec::new(),
        interface_outputs: Vec::new(),
        interface_parameters: Vec::new(),
        protected: Vec::new(),
        startup: Vec::new(),
        recalibrate: Vec::new(),
        do_step_locals: Vec::new(),
    };
    for variable in classified {
        append_variable(view, variable, by_id, pre_names, &mut evaluator, &mut parts)?;
    }
    append_previous_states(
        view,
        referenced_pre,
        by_id,
        pre_names,
        &mut evaluator,
        &mut parts.nominals,
        &mut parts.protected,
        &mut parts.startup,
    )?;
    Ok(parts)
}

fn append_variable<'dae>(
    view: dae::DaeView<'dae>,
    classified: &ClassifiedVariable<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
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
            parts.protected.push(gast::ProtectedEntity {
                kind: gast::ProtectedKind::DependentParameter,
                decl: declaration,
                start: Some(start),
            });
            let assignment =
                dependent_assignment(view, classified, by_id, pre_names).map_err(single)?;
            parts.startup.push(assignment.clone());
            parts.recalibrate.push(assignment);
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
) -> Result<Vec<ClassifiedVariable<'dae>>, Vec<GalecTargetError>> {
    let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
    let mut variables = Vec::new();
    let mut errors = Vec::new();
    for (id, variable) in view.variables() {
        let causally_defined = definitions.definition_for_variable(id).is_some()
            || definitions.fully_defines_variable(id);
        let classified =
            if causally_defined && variable.causality() != dae::VariableCausality::Output {
                classify_causal_local(id, variable)
            } else {
                classify_variable(id, variable)
            };
        match classified {
            Ok(classified) => variables.push(classified),
            Err(error) => errors.push(error),
        }
    }
    if errors.is_empty() {
        Ok(variables)
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
) -> Result<ClassifiedVariable<'dae>, GalecTargetError> {
    let span = variable.declaration().span();
    let class = match (variable.causality(), variable.role()) {
        (dae::VariableCausality::Input, _) => VariableClass::Input,
        (dae::VariableCausality::Output, _) => VariableClass::Output,
        (_, dae::VariableRole::Constant) => VariableClass::Constant,
        (dae::VariableCausality::CalculatedParameter, dae::VariableRole::Parameter) => {
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

fn scalar_type(
    scalar: dae::ScalarType,
    name: &str,
    span: Span,
) -> Result<gast::ScalarType, GalecTargetError> {
    match scalar {
        dae::ScalarType::Real => Ok(gast::ScalarType::Real),
        dae::ScalarType::Integer => Ok(gast::ScalarType::Integer),
        dae::ScalarType::Boolean => Ok(gast::ScalarType::Boolean),
        dae::ScalarType::Enumeration => Err(unsupported(
            "enumeration-variable",
            format!("enumeration variable `{name}` has no owner-aware GALEC scalar type"),
            span,
        )),
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

fn dependent_assignment<'dae>(
    view: dae::DaeView<'dae>,
    classified: &ClassifiedVariable<'dae>,
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
) -> Result<gast::Spanned<gast::Statement>, GalecTargetError> {
    let expression = classified
        .variable
        .binding()
        .or(classified.variable.start())
        .ok_or_else(|| GalecTargetError::AttributeNotEvaluable {
            variable: classified.variable.name().to_string(),
            attribute: "binding",
            reason: "dependent parameter has no defining expression".to_owned(),
            span: Some(classified.variable.declaration().span()),
        })?;
    let mut lowerer = ExpressionLowerer::new(view, by_id, pre_names);
    let value = lowerer.lower(expression)?;
    let value = coerce(
        value,
        classified.scalar_type,
        expression_span(view, expression),
    )?;
    Ok(gast::Spanned::new(
        gast::Statement::Assignment {
            target: state_reference(
                classified.name.clone(),
                classified.variable.declaration().span(),
            ),
            value,
        },
        expression_span(view, expression),
    ))
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

#[allow(clippy::too_many_arguments)]
fn append_previous_states<'dae>(
    view: dae::DaeView<'dae>,
    referenced: &[dae::VariableId<'dae>],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &HashMap<u32, gast::Name>,
    evaluator: &mut NumericEvaluator<'dae>,
    nominals: &mut Vec<Option<f64>>,
    protected: &mut Vec<gast::ProtectedEntity>,
    startup: &mut Vec<gast::Spanned<gast::Statement>>,
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
        protected.push(gast::ProtectedEntity {
            kind: gast::ProtectedKind::State,
            decl,
            start: Some(start.clone()),
        });
        startup.push(initial_assignment(&previous, start));
        nominals.push(projected.nominal);
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn append_clock_period(
    clock: &AdmittedClock,
    span: Span,
    classified: &[ClassifiedVariable<'_>],
    pre_names: &HashMap<u32, gast::Name>,
    nominals: &mut Vec<Option<f64>>,
    protected: &mut Vec<gast::ProtectedEntity>,
    startup: &mut Vec<gast::Spanned<gast::Statement>>,
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
    nominals.push(None);
    let declaration = gast::VariableDeclaration {
        ty: gast::TypeRef::Primitive(gast::ScalarType::Real),
        name: name.clone(),
        dimensions: Vec::new(),
        range: gast::RangeAttributes::default(),
        span,
    };
    protected.push(gast::ProtectedEntity {
        kind: gast::ProtectedKind::Constant,
        decl: declaration,
        start: Some(gast::Expression::Real(clock.period_seconds)),
    });
    startup.push(gast::Spanned::new(
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

fn lower_action_guard<'dae>(
    view: dae::DaeView<'dae>,
    guard: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    span: Span,
) -> Result<Option<gast::Expression>, GalecTargetError> {
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
        dae::ConditionOperation::Clock(found) if found == expected => Ok(None),
        dae::ConditionOperation::Clock(_) => Err(unsupported(
            "multiple-clock-event-guard",
            "event action combines distinct clock domains".to_owned(),
            span,
        )),
        dae::ConditionOperation::Relation(relation) => {
            let relation = view
                .relation(relation)
                .expect("checked relation identity resolves");
            let expression = lowerer.lower(relation.expression())?;
            require_boolean(&expression, span)?;
            Ok(Some(expression.expression))
        }
        dae::ConditionOperation::Discrete(expression) => {
            let expression = lowerer.lower(expression)?;
            require_boolean(&expression, span)?;
            Ok(Some(expression.expression))
        }
        dae::ConditionOperation::Not(condition) => {
            let condition = lower_action_guard(view, condition, expected, lowerer, span)?;
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
            combine_action_guards(view, lhs, rhs, expected, lowerer, gast::BinaryOp::And, span)
        }
        dae::ConditionOperation::Or(lhs, rhs) => {
            combine_action_guards(view, lhs, rhs, expected, lowerer, gast::BinaryOp::Or, span)
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

#[allow(clippy::too_many_arguments)]
fn combine_action_guards<'dae>(
    view: dae::DaeView<'dae>,
    lhs: dae::ConditionId<'dae>,
    rhs: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    operator: gast::BinaryOp,
    span: Span,
) -> Result<Option<gast::Expression>, GalecTargetError> {
    let lhs = lower_action_guard(view, lhs, expected, lowerer, span)?;
    let rhs = lower_action_guard(view, rhs, expected, lowerer, span)?;
    Ok(match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => Some(gast::Expression::binary(operator, lhs, rhs)),
        (Some(expression), None) | (None, Some(expression)) if operator == gast::BinaryOp::And => {
            Some(expression)
        }
        (None, _) | (_, None) => None,
    })
}

struct ExpressionLowerer<'a, 'dae> {
    view: dae::DaeView<'dae>,
    by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
    pre_names: &'a HashMap<u32, gast::Name>,
    definitions: rumoca_phase_structural::CausalDefinitions<'dae>,
    call_frames: Vec<CallFrame<'dae>>,
    function_fold_values: Vec<(dae::FunctionFoldId<'dae>, Vec<Vec<TypedExpression>>)>,
    function_fold_output_cache: HashMap<FunctionFoldOutputKey, TypedExpression>,
    scalar_projection_cache: HashMap<ScalarProjectionKey, TypedExpression>,
    function_fold_projection_cache: HashMap<u32, bool>,
    comprehension_frames: Vec<ComprehensionFrame>,
    loop_index_bounds: Vec<LoopIndexBound>,
    materialize_function_values: bool,
    inline_causal_locals: bool,
    conditional_depth: usize,
    materialized_function_values: HashMap<MaterializedFunctionValueKey, gast::Name>,
    materialized_function_calls: HashMap<MaterializedFunctionCallKey, Vec<gast::Name>>,
    called_user_functions: HashSet<u32>,
    function_scope: Option<dae::FunctionId<'dae>>,
    temporary_locals: Vec<gast::VariableDeclaration>,
    temporary_counter: usize,
    temporary_namespace: String,
    capture_assertions: bool,
    seen_assertion_calls: HashSet<FunctionAssertionCallKey>,
    pending_prefix_statements: Vec<gast::Spanned<gast::Statement>>,
}

struct CallFrame<'dae> {
    call: dae::ExprId<'dae>,
    function: dae::FunctionId<'dae>,
    arguments: Vec<dae::ExprId<'dae>>,
    indices: Vec<Option<i64>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionAssertionCallKey {
    path: Vec<FunctionAssertionCallSite>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionAssertionCallSite {
    function: u32,
    arguments: Vec<u32>,
    indices: Vec<Option<i64>>,
    span: Span,
}

struct ComprehensionFrame {
    domain: u32,
    binders: Vec<gast::Expression>,
}

struct LoopIndexBound {
    name: gast::Name,
    minimum: i64,
    maximum: i64,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct MaterializedFunctionValueKey {
    call_path: Vec<MaterializedCallKey>,
    function: u32,
    definition: u32,
    indices: Vec<i64>,
    fields: Vec<u32>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct MaterializedCallKey {
    function: u32,
    arguments: Vec<u32>,
    indices: Vec<Option<i64>>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct MaterializedFunctionCallKey {
    call_path: Vec<MaterializedCallKey>,
    function: u32,
    arguments: Vec<u32>,
}

#[derive(Clone)]
struct TypedExpression {
    expression: gast::Expression,
    scalar_type: gast::ScalarType,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct FunctionFoldOutputKey {
    call_path: Vec<MaterializedCallKey>,
    fold: u32,
    carried: u32,
    scalar: u32,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct ScalarProjectionKey {
    call_path: Vec<MaterializedCallKey>,
    expression: u32,
    indices: Vec<i64>,
}

fn expression_depth(expression: &gast::Expression) -> usize {
    match expression {
        gast::Expression::Bool(_)
        | gast::Expression::Integer(_)
        | gast::Expression::Real(_)
        | gast::Expression::Ref(_)
        | gast::Expression::Neg(_) => 1,
        gast::Expression::Size { dimension, .. }
        | gast::Expression::Paren(dimension)
        | gast::Expression::Not(dimension) => 1 + expression_depth(dimension),
        gast::Expression::Call(call) => {
            1 + call
                .arguments
                .iter()
                .map(expression_depth)
                .max()
                .unwrap_or(0)
        }
        gast::Expression::If(value) => {
            let branch_depth = value
                .branches
                .iter()
                .flat_map(|(condition, result)| [condition, result])
                .map(expression_depth)
                .max()
                .unwrap_or(0);
            1 + branch_depth.max(expression_depth(&value.else_value))
        }
        gast::Expression::Array(elements) => {
            1 + elements.iter().map(expression_depth).max().unwrap_or(0)
        }
        gast::Expression::Binary { lhs, rhs, .. } => {
            1 + expression_depth(lhs).max(expression_depth(rhs))
        }
    }
}

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    fn new(
        view: dae::DaeView<'dae>,
        by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
        pre_names: &'a HashMap<u32, gast::Name>,
    ) -> Self {
        Self {
            view,
            by_id,
            pre_names,
            definitions: rumoca_phase_structural::CausalDefinitions::derive(view),
            call_frames: Vec::new(),
            function_fold_values: Vec::new(),
            function_fold_output_cache: HashMap::new(),
            scalar_projection_cache: HashMap::new(),
            function_fold_projection_cache: HashMap::new(),
            comprehension_frames: Vec::new(),
            loop_index_bounds: Vec::new(),
            materialize_function_values: false,
            inline_causal_locals: false,
            conditional_depth: 0,
            materialized_function_values: HashMap::new(),
            materialized_function_calls: HashMap::new(),
            called_user_functions: HashSet::new(),
            function_scope: None,
            temporary_locals: Vec::new(),
            temporary_counter: 0,
            temporary_namespace: "value".to_owned(),
            capture_assertions: false,
            seen_assertion_calls: HashSet::new(),
            pending_prefix_statements: Vec::new(),
        }
    }

    fn with_assertions(
        view: dae::DaeView<'dae>,
        by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
        pre_names: &'a HashMap<u32, gast::Name>,
    ) -> Self {
        Self {
            capture_assertions: true,
            ..Self::new(view, by_id, pre_names)
        }
    }

    fn with_do_step_effects(
        view: dae::DaeView<'dae>,
        by_id: &'a HashMap<u32, ClassifiedVariable<'dae>>,
        pre_names: &'a HashMap<u32, gast::Name>,
    ) -> Self {
        Self {
            capture_assertions: true,
            materialize_function_values: true,
            ..Self::new(view, by_id, pre_names)
        }
    }

    /// Finish one emitted statement group and sever every temporary cache
    /// whose initializer belongs to that group.
    ///
    /// A later scheduler may reorder statement groups. Keeping a cached local
    /// across this boundary would let the later group read a temporary whose
    /// defining assignment moved after the read.
    fn take_prefix_statements(&mut self) -> Vec<gast::Spanned<gast::Statement>> {
        self.finish_statement_group();
        self.drain_prefix_statements()
    }

    fn drain_prefix_statements(&mut self) -> Vec<gast::Spanned<gast::Statement>> {
        std::mem::take(&mut self.pending_prefix_statements)
    }

    fn finish_statement_group(&mut self) {
        self.materialized_function_values.clear();
        self.materialized_function_calls.clear();
        self.function_fold_output_cache.clear();
        self.scalar_projection_cache.clear();
        self.seen_assertion_calls.clear();
    }

    fn take_temporary_locals(&mut self) -> Vec<gast::VariableDeclaration> {
        std::mem::take(&mut self.temporary_locals)
    }

    fn take_called_user_functions(&mut self) -> HashSet<u32> {
        std::mem::take(&mut self.called_user_functions)
    }

    fn with_temporary_namespace(mut self, namespace: impl Into<String>) -> Self {
        self.temporary_namespace = namespace.into();
        self
    }

    fn with_causal_inlining(mut self) -> Self {
        self.inline_causal_locals = true;
        self
    }

    fn lower(&mut self, id: dae::ExprId<'dae>) -> Result<TypedExpression, GalecTargetError> {
        self.lower_at(id, &[])
    }

    fn lower_element(
        &mut self,
        id: dae::ExprId<'dae>,
        indices: &[u32],
    ) -> Result<TypedExpression, GalecTargetError> {
        let indices = indices
            .iter()
            .map(|index| gast::Expression::Integer(i64::from(*index)))
            .collect::<Vec<_>>();
        self.lower_at(id, &indices)
    }

    fn lower_at(
        &mut self,
        id: dae::ExprId<'dae>,
        indices: &[gast::Expression],
    ) -> Result<TypedExpression, GalecTargetError> {
        let cache_key = self.scalar_projection_key(id, indices);
        if let Some(key) = &cache_key
            && let Some(value) = self.scalar_projection_cache.get(key)
        {
            return Ok(value.clone());
        }
        let node = self
            .view
            .expression(id)
            .expect("checked expression identity resolves");
        if node.value_type().dimensions().len() != indices.len() {
            return Err(unsupported(
                "array-projection",
                format!(
                    "expression rank {} cannot be projected with {} indices",
                    node.value_type().dimensions().len(),
                    indices.len()
                ),
                node.provenance().span(),
            ));
        }
        let scalar_type = scalar_type(
            node.value_type().scalar_type(),
            "<expression>",
            node.provenance().span(),
        )?;
        let value = self.lower_operation(id, node, indices, scalar_type)?;
        if let Some(key) = cache_key {
            self.scalar_projection_cache.insert(key, value.clone());
        }
        Ok(value)
    }

    fn scalar_projection_key(
        &self,
        expression: dae::ExprId<'dae>,
        indices: &[gast::Expression],
    ) -> Option<ScalarProjectionKey> {
        if !self.materialize_function_values
            || self.conditional_depth != 0
            || !self.comprehension_frames.is_empty()
            || !self.loop_index_bounds.is_empty()
            || !self.function_fold_values.is_empty()
        {
            return None;
        }
        Some(ScalarProjectionKey {
            call_path: self
                .call_frames
                .iter()
                .map(|frame| MaterializedCallKey {
                    function: frame.function.index(),
                    arguments: frame
                        .arguments
                        .iter()
                        .map(|argument| argument.index())
                        .collect(),
                    indices: frame.indices.clone(),
                })
                .collect(),
            expression: expression.index(),
            indices: indices
                .iter()
                .map(constant_integer)
                .collect::<Option<Vec<_>>>()?,
        })
    }

    fn lower_operation(
        &mut self,
        id: dae::ExprId<'dae>,
        node: dae::ExpressionView<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
    ) -> Result<TypedExpression, GalecTargetError> {
        let expression = match node.operation() {
            dae::ExpressionOperation::Literal(literal) => {
                lower_literal(literal, node.provenance().span())?
            }
            dae::ExpressionOperation::Coordinate(coordinate) => {
                return self.coordinate_at(coordinate, indices, node.provenance().span());
            }
            dae::ExpressionOperation::ClockTransfer { .. } => {
                return Err(unsupported(
                    "clock-transfer",
                    "cross-clock value transfer is not representable in scalar GALEC".to_owned(),
                    node.provenance().span(),
                ));
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                self.lower_unary_at(operator, operand, indices, node.provenance().span())?
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                return self.lower_binary_at(
                    operator,
                    lhs,
                    rhs,
                    indices,
                    scalar_type,
                    node.provenance().span(),
                );
            }
            dae::ExpressionOperation::Conditional(operands) => {
                self.lower_conditional_at(operands, indices, scalar_type, node.provenance().span())?
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                if matches!(builtin, dae::PureBuiltin::Sum | dae::PureBuiltin::Product) {
                    return self.lower_reduction(builtin, arguments, scalar_type);
                }
                if !indices.is_empty() {
                    return self.lower_elementwise_builtin(
                        builtin,
                        arguments,
                        indices,
                        scalar_type,
                        node.provenance().span(),
                    );
                }
                lower_builtin(self, builtin, arguments, node.provenance().span())?
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                return self.lower_function_value(definition, indices, scalar_type);
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                return self.lower_index_at(base, subscripts, indices, node.provenance().span());
            }
            dae::ExpressionOperation::Array(elements) => {
                return self.lower_array_at(elements, indices, node.provenance().span());
            }
            _ => return self.lower_aggregate_operation(id, node, indices, scalar_type),
        };
        self.bound_expression(
            TypedExpression {
                expression,
                scalar_type,
            },
            node.provenance().span(),
        )
    }

    fn bound_expression(
        &mut self,
        value: TypedExpression,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        const MAX_INLINE_DEPTH: usize = 16;
        if !self.materialize_function_values
            || expression_depth(&value.expression) <= MAX_INLINE_DEPTH
        {
            return Ok(value);
        }
        let name = gast::Name::ident(format!(
            "rumoca_{}_expr_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(value.scalar_type),
            name: name.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        });
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(name.clone()),
                value: value.expression,
            },
            span,
        ));
        Ok(TypedExpression {
            expression: gast::Expression::Ref(gast::Reference::local(name)),
            scalar_type: value.scalar_type,
        })
    }

    fn lower_function_value(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
    ) -> Result<TypedExpression, GalecTargetError> {
        if self.function_scope == Some(definition.id().function()) {
            let value = self
                .view
                .function(definition.id().function())
                .expect("checked function identity resolves")
                .values()
                .find(|value| value.id() == definition.target())
                .expect("checked function definition target resolves");
            return Ok(TypedExpression {
                expression: gast::Expression::Ref(gast::Reference::Local(gast::RefPart {
                    name: user_functions::value_name(value)?,
                    subscripts: indices.to_vec(),
                    span: definition.provenance().span(),
                })),
                scalar_type,
            });
        }
        let Some(key) = self.function_value_key(definition, indices, Vec::new()) else {
            return self.lower_at(definition.rhs(), indices);
        };
        if let Some(name) = self.materialized_function_values.get(&key) {
            return Ok(TypedExpression {
                expression: gast::Expression::Ref(gast::Reference::local(name.clone())),
                scalar_type,
            });
        }

        let value = self.lower_at(definition.rhs(), indices)?;
        self.store_materialized_function_value(
            key,
            value,
            scalar_type,
            definition.provenance().span(),
        )
    }

    fn function_value_key(
        &self,
        definition: dae::FunctionDefinitionView<'dae>,
        indices: &[gast::Expression],
        fields: Vec<u32>,
    ) -> Option<MaterializedFunctionValueKey> {
        if !self.materialize_function_values
            || self.conditional_depth != 0
            || self.call_frames.is_empty()
        {
            return None;
        }
        Some(MaterializedFunctionValueKey {
            call_path: self
                .call_frames
                .iter()
                .map(|frame| MaterializedCallKey {
                    function: frame.function.index(),
                    arguments: frame
                        .arguments
                        .iter()
                        .map(|argument| argument.index())
                        .collect(),
                    indices: frame.indices.clone(),
                })
                .collect(),
            function: definition.id().function().index(),
            definition: definition.id().ordinal(),
            indices: indices
                .iter()
                .map(|index| match index {
                    gast::Expression::Integer(value) => Some(*value),
                    _ => None,
                })
                .collect::<Option<Vec<_>>>()?,
            fields,
        })
    }

    fn store_materialized_function_value(
        &mut self,
        key: MaterializedFunctionValueKey,
        value: TypedExpression,
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let name = gast::Name::ident(format!(
            "rumoca_{}_value_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(scalar_type),
            name: name.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        });
        self.pending_prefix_statements.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(name.clone()),
                value: coerce(value, scalar_type, span)?,
            },
            span,
        ));
        self.materialized_function_values.insert(key, name.clone());
        Ok(TypedExpression {
            expression: gast::Expression::Ref(gast::Reference::local(name)),
            scalar_type,
        })
    }

    fn lower_aggregate_operation(
        &mut self,
        id: dae::ExprId<'dae>,
        node: dae::ExpressionView<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
    ) -> Result<TypedExpression, GalecTargetError> {
        match node.operation() {
            dae::ExpressionOperation::Range(range) => lower_range_at(
                range.start().value(),
                range.effective_step(),
                range.stop().value(),
                indices,
                scalar_type,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => self.lower_call_at(
                id,
                function,
                output,
                arguments,
                indices,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => self.lower_array_update_at(
                base,
                value,
                subscripts,
                indices,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.lower_comprehension_at(domain, body, indices, node.provenance().span())
            }
            dae::ExpressionOperation::Field { base, field } => self.lower_record_field_at(
                base,
                field as usize,
                indices,
                scalar_type,
                node.provenance().span(),
            ),
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. } => self
                .lower_function_fold_parameter_at(fold, carried, indices, node.provenance().span()),
            dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } => {
                self.lower_function_fold_output_at(fold, carried, indices, node.provenance().span())
            }
            dae::ExpressionOperation::Record(_)
            | dae::ExpressionOperation::StringConversion { .. } => Err(unsupported(
                "expression-form",
                format!(
                    "checked expression form {:?} is outside the scalar GALEC projection",
                    node.kind()
                ),
                node.provenance().span(),
            )),
            _ => unreachable!("ordinary scalar operation was lowered before aggregate dispatch"),
        }
    }

    fn lower_conditional_at(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        if self.materialize_function_values {
            self.conditional_depth += 1;
            let result = self.lower_materialized_conditional(operands, indices, scalar_type, span);
            self.conditional_depth -= 1;
            return result;
        }
        self.lower_conditional_branches(operands, indices, scalar_type, span)
    }

    fn lower_materialized_conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let name = gast::Name::ident(format!(
            "rumoca_{}_conditional_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(scalar_type),
            name: name.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        });
        let statements = self.lower_materialized_conditional_branch(
            operands,
            indices,
            scalar_type,
            &name,
            0,
            span,
        )?;
        self.pending_prefix_statements.extend(statements);
        Ok(gast::Expression::Ref(gast::Reference::local(name)))
    }

    fn lower_materialized_conditional_branch(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        target: &gast::Name,
        ordinal: usize,
        span: Span,
    ) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
        if ordinal + 1 == operands.len() {
            let start = self.pending_prefix_statements.len();
            let value = self.lower_at(
                operands.get(ordinal).expect("checked conditional fallback"),
                indices,
            )?;
            let mut body = self.pending_prefix_statements.split_off(start);
            body.push(gast::Spanned::new(
                gast::Statement::Assignment {
                    target: gast::Reference::local(target.clone()),
                    value: coerce(value, scalar_type, span)?,
                },
                span,
            ));
            return Ok(body);
        }

        let condition_start = self.pending_prefix_statements.len();
        let condition = self.lower(
            operands
                .get(ordinal)
                .expect("checked conditional branch condition"),
        )?;
        require_boolean(&condition, span)?;
        let mut statements = self.pending_prefix_statements.split_off(condition_start);

        let value_start = self.pending_prefix_statements.len();
        let value = self.lower_at(
            operands
                .get(ordinal + 1)
                .expect("checked conditional branch value"),
            indices,
        )?;
        let mut body = self.pending_prefix_statements.split_off(value_start);
        body.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(target.clone()),
                value: coerce(value, scalar_type, span)?,
            },
            span,
        ));
        let else_body = self.lower_materialized_conditional_branch(
            operands,
            indices,
            scalar_type,
            target,
            ordinal + 2,
            span,
        )?;
        statements.push(gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: vec![gast::IfBranch {
                    condition: gast::Condition::Expression(condition.expression),
                    body,
                    span,
                }],
                else_body: Some(else_body),
            }),
            span,
        ));
        Ok(statements)
    }

    fn lower_conditional_branches(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let mut branches = Vec::new();
        for ordinal in (0..operands.len() - 1).step_by(2) {
            let condition =
                self.lower(operands.get(ordinal).expect("checked condition operand"))?;
            require_boolean(&condition, span)?;
            let value = self.lower_at(
                operands.get(ordinal + 1).expect("checked value operand"),
                indices,
            )?;
            branches.push((condition.expression, coerce(value, scalar_type, span)?));
        }
        let fallback = self.lower_at(
            operands
                .get(operands.len() - 1)
                .expect("checked conditional fallback"),
            indices,
        )?;
        Ok(gast::Expression::If(gast::IfExpression {
            branches,
            else_value: Box::new(coerce(fallback, scalar_type, span)?),
        }))
    }
}

fn first_function_assertion(statements: dae::FunctionStatements<'_>) -> Option<Span> {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::Assertion { provenance, .. } => {
                return Some(provenance.span());
            }
            dae::FunctionStatementView::For { statements, .. } => {
                if let Some(span) = first_function_assertion(statements) {
                    return Some(span);
                }
            }
            dae::FunctionStatementView::Assignment { .. } => {}
        }
    }
    None
}

#[cfg(test)]
mod tests;
