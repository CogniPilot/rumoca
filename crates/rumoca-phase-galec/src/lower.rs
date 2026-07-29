//! Checked DAE → [`AlgorithmCodePackage`] lowering.
//!
//! This projection reads the immutable branded DAE directly. Periodic clock
//! guards become the implicit `DoStep` tick; guarded assignments are ordered
//! by their current-tick dependencies; `pre(x)` becomes a protected
//! `'previous(x)'` state committed after all assignments.

use std::collections::{HashMap, HashSet};

use rumoca_core::Span;
use rumoca_eval_dae::NumericEvaluator;
use rumoca_ir_dae as dae;
use rumoca_ir_galec::ast as gast;

use crate::admissibility::{AdmittedClock, check_admissibility};
use crate::diagnostic::GalecTargetError;
use crate::input::{GalecInput, GalecOptions};
use rumoca_ir_galec::package::AlgorithmCodePackage;

mod clocked_assignments;
mod expression_helpers;
mod start;
use clocked_assignments::lower_clocked_assignments;
use expression_helpers::*;
use start::{boolean_start, integer_start, real_start};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VariableClass {
    Input,
    Output,
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
    let clock_id = admitted_clock_id(view).map_err(single)?;
    validate_clock(clock, view, clock_id).map_err(single)?;
    let classified = classify_variables(view)?;
    let by_id = classified
        .iter()
        .map(|variable| (variable.id.index(), variable.clone()))
        .collect::<HashMap<_, _>>();
    let referenced_pre = referenced_pre_variables(view)?;
    let pre_names = build_pre_names(&referenced_pre, &by_id)?;

    let mut parts = build_variable_parts(view, &classified, &by_id, &referenced_pre, &pre_names)?;

    let period_ref = append_clock_period(
        clock,
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

    let mut do_step =
        lower_clocked_assignments(view, clock_id, &by_id, &pre_names).map_err(single)?;
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
    block.do_step.statements = do_step;

    AlgorithmCodePackage::construct(
        block,
        parts.nominals,
        &period_ref,
    )
    .map_err(|error| {
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
) -> Result<dae::ClockId<'dae>, GalecTargetError> {
    (0..view.clock_count())
        .filter_map(|index| view.clock_id(index))
        .find(|id| {
            matches!(
                view.clock(*id).map(dae::ClockView::operation),
                Some(dae::ClockOperation::Periodic(_))
            )
        })
        .ok_or(GalecTargetError::ClockCountNotOne { count: 0 })
}

fn validate_clock<'dae>(
    clock: AdmittedClock,
    view: dae::DaeView<'dae>,
    clock_id: dae::ClockId<'dae>,
) -> Result<(), GalecTargetError> {
    let span = view
        .clock(clock_id)
        .expect("admitted clock resolves")
        .provenance()
        .span();
    if !clock.period_seconds.is_finite() || clock.period_seconds <= 0.0 {
        return Err(GalecTargetError::InvalidClockPeriod {
            period_seconds: clock.period_seconds,
            span,
        });
    }
    if clock.phase_seconds != 0.0 {
        return Err(unsupported(
            "clock-phase",
            format!(
                "clock phase offset {} s cannot be represented by the eFMI Beta-1 Clock",
                clock.phase_seconds
            ),
            span,
        ));
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
        if definitions.definition_for_variable(id).is_some() {
            continue;
        }
        match classify_variable(id, variable) {
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
    let scalar_type = scalar_type(variable.value_type().scalar_type(), variable.name(), span)?;
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
    name: &rumoca_core::VarName,
    span: Span,
) -> Result<gast::ScalarType, GalecTargetError> {
    match scalar {
        dae::ScalarType::Real => Ok(gast::ScalarType::Real),
        dae::ScalarType::Integer => Ok(gast::ScalarType::Integer),
        dae::ScalarType::Boolean => Ok(gast::ScalarType::Boolean),
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
    let dimensions = classified
        .variable
        .value_type()
        .dimensions()
        .iter()
        .map(|extent| u64::from(*extent))
        .collect::<Vec<_>>();
    let values = initial_values(classified, evaluator).map_err(single)?;
    let (start, range, nominal) = match classified.scalar_type {
        gast::ScalarType::Real => (
            real_start(&dimensions, values).map_err(single)?,
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
            integer_start(
                &dimensions,
                values,
                classified.variable.declaration().span(),
            )
            .map_err(single)?,
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
                boolean_start(
                    &dimensions,
                    values,
                    classified.variable.declaration().span(),
                )
                .map_err(single)?,
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
    classified: &ClassifiedVariable<'dae>,
    evaluator: &mut NumericEvaluator<'dae>,
) -> Result<Vec<f64>, GalecTargetError> {
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
        return Ok(vec![
            default_scalar(classified.scalar_type);
            classified.variable.scalar_count()
        ]);
    };
    evaluator
        .expression(expression)
        .map_err(|error| GalecTargetError::AttributeNotEvaluable {
            variable: classified.variable.name().to_string(),
            attribute: "start",
            reason: error.to_string(),
            span: Some(error.span()),
        })
}

const fn default_scalar(scalar: gast::ScalarType) -> f64 {
    match scalar {
        gast::ScalarType::Real | gast::ScalarType::Integer | gast::ScalarType::Boolean => 0.0,
    }
}

fn exact_integer(value: f64, span: Span) -> Result<i64, GalecTargetError> {
    if value.is_finite()
        && value.fract() == 0.0
        && value >= i64::MIN as f64
        && value <= i64::MAX as f64
    {
        Ok(value as i64)
    } else {
        Err(GalecTargetError::AttributeTypeMismatch {
            variable: "<checked Integer>".to_owned(),
            attribute: "value",
            expected: "Integer",
            found: "non-integral or out-of-range Real",
            span: Some(span),
        })
    }
}

fn optional_real<'dae>(
    evaluator: &mut NumericEvaluator<'dae>,
    expression: Option<dae::ExprId<'dae>>,
) -> Result<Option<f64>, GalecTargetError> {
    expression
        .map(|expression| scalar_numeric(evaluator, expression))
        .transpose()
}

fn optional_integer<'dae>(
    view: dae::DaeView<'dae>,
    evaluator: &mut NumericEvaluator<'dae>,
    expression: Option<dae::ExprId<'dae>>,
) -> Result<Option<i64>, GalecTargetError> {
    expression
        .map(|expression| {
            let span = expression_span(view, expression);
            exact_integer(scalar_numeric(evaluator, expression)?, span)
        })
        .transpose()
}

fn scalar_numeric<'dae>(
    evaluator: &mut NumericEvaluator<'dae>,
    expression: dae::ExprId<'dae>,
) -> Result<f64, GalecTargetError> {
    let values = evaluator.expression(expression).map_err(|error| {
        GalecTargetError::AttributeNotEvaluable {
            variable: "<checked variable>".to_owned(),
            attribute: "bound",
            reason: error.to_string(),
            span: Some(error.span()),
        }
    })?;
    match values.as_slice() {
        [value] => Ok(*value),
        _ => Err(GalecTargetError::AttributeNotEvaluable {
            variable: "<checked variable>".to_owned(),
            attribute: "bound",
            reason: "attribute is not scalar".to_owned(),
            span: None,
        }),
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

fn referenced_pre_variables<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<Vec<dae::VariableId<'dae>>, Vec<GalecTargetError>> {
    let mut ids = Vec::new();
    let mut seen = HashSet::new();
    for index in 0..view.event_action_count() {
        let action = view
            .event_action(
                view.event_action_id(index)
                    .expect("dense checked action identity"),
            )
            .expect("checked action resolves");
        collect_condition_pre(view, action.trigger(), &mut seen, &mut ids)?;
        collect_condition_pre(view, action.guard(), &mut seen, &mut ids)?;
        let value = match action.operation() {
            dae::EventActionOperation::AssignDiscreteReal { value, .. }
            | dae::EventActionOperation::Reinitialize { value, .. } => Some(value),
            dae::EventActionOperation::Assert { message, level } => {
                collect_pre(view, message, &mut seen, &mut ids)?;
                level
            }
            dae::EventActionOperation::Terminate { message } => Some(message),
        };
        if let Some(value) = value {
            collect_pre(view, value, &mut seen, &mut ids)?;
        }
    }
    for index in 0..view.discrete_value_owner_count() {
        let owner = view
            .discrete_value_owner(
                view.discrete_value_owner_id(index)
                    .expect("dense checked B.1c owner identity"),
            )
            .expect("checked B.1c owner resolves");
        for branch in owner.branches().iter() {
            if let dae::DiscreteBranchActivation::When { trigger, guard } = branch.activation() {
                collect_condition_pre(view, trigger, &mut seen, &mut ids)?;
                collect_condition_pre(view, guard, &mut seen, &mut ids)?;
            }
            for (value, _) in branch.values().iter() {
                collect_pre(view, value, &mut seen, &mut ids)?;
            }
        }
    }
    Ok(ids)
}

fn collect_condition_pre<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    seen_variables: &mut HashSet<u32>,
    ids: &mut Vec<dae::VariableId<'dae>>,
) -> Result<(), Vec<GalecTargetError>> {
    let mut pending = vec![root];
    let mut seen_conditions = HashSet::new();
    while let Some(condition) = pending.pop() {
        if !seen_conditions.insert(condition.index()) {
            continue;
        }
        match view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation()
        {
            dae::ConditionOperation::Initial | dae::ConditionOperation::Clock(_) => {}
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked relation identity resolves")
                    .expression();
                collect_pre(view, expression, seen_variables, ids)?;
            }
            dae::ConditionOperation::Discrete(expression) => {
                collect_pre(view, expression, seen_variables, ids)?;
            }
            dae::ConditionOperation::Not(inner) => pending.push(inner),
            dae::ConditionOperation::And(lhs, rhs) | dae::ConditionOperation::Or(lhs, rhs) => {
                pending.extend([lhs, rhs]);
            }
        }
    }
    Ok(())
}

fn collect_pre<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    seen: &mut HashSet<u32>,
    ids: &mut Vec<dae::VariableId<'dae>>,
) -> Result<(), Vec<GalecTargetError>> {
    dae::for_each_expression(view, expression, |id, node| {
        let variable = match node.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::PreDiscreteReal(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::PreDiscreteValue(id)) => {
                Some(dae::VariableId::from(id))
            }
            _ => None,
        };
        if let Some(variable) = variable
            && seen.insert(variable.index())
        {
            ids.push(variable);
        }
        let _ = id;
    });
    Ok(())
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
    clock: AdmittedClock,
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
}

struct CallFrame<'dae> {
    call: dae::ExprId<'dae>,
    function: dae::FunctionId<'dae>,
    arguments: Vec<dae::ExprId<'dae>>,
}

struct TypedExpression {
    expression: gast::Expression,
    scalar_type: gast::ScalarType,
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
        }
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
            &rumoca_core::VarName::new("<expression>"),
            node.provenance().span(),
        )?;
        self.lower_operation(id, node, indices, scalar_type)
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
                return self.lower_at(definition.rhs(), indices);
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                return self.lower_index_at(base, subscripts, indices, node.provenance().span());
            }
            dae::ExpressionOperation::Array(elements) => {
                return self.lower_array_at(elements, indices, node.provenance().span());
            }
            dae::ExpressionOperation::Range { start, step, stop } => {
                return lower_range_at(
                    start,
                    step,
                    stop,
                    indices,
                    scalar_type,
                    node.provenance().span(),
                );
            }
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
            } => {
                return self.lower_call_at(
                    id,
                    function,
                    output,
                    arguments,
                    indices,
                    node.provenance().span(),
                );
            }
            dae::ExpressionOperation::ArrayUpdate { .. }
            | dae::ExpressionOperation::Comprehension { .. }
            | dae::ExpressionOperation::Record(_)
            | dae::ExpressionOperation::Field { .. }
            | dae::ExpressionOperation::FunctionFoldParameter { .. }
            | dae::ExpressionOperation::FunctionFoldOutput { .. } => {
                return Err(unsupported(
                    "expression-form",
                    format!(
                        "checked expression form {:?} is outside the scalar GALEC projection",
                        node.kind()
                    ),
                    node.provenance().span(),
                ));
            }
        };
        Ok(TypedExpression {
            expression,
            scalar_type,
        })
    }

    fn lower_unary_at(
        &mut self,
        operator: dae::UnaryOperator,
        operand: dae::ExprId<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let operand = self.lower_at(operand, indices)?;
        match operator {
            dae::UnaryOperator::Plus => Ok(operand.expression),
            dae::UnaryOperator::Negate => match operand.scalar_type {
                gast::ScalarType::Real => Ok(gast::Expression::negated_real(operand.expression)),
                gast::ScalarType::Integer => {
                    Ok(gast::Expression::negated_integer(operand.expression))
                }
                gast::ScalarType::Boolean => Err(type_mismatch("numeric", "Boolean", span)),
            },
            dae::UnaryOperator::Not => Ok(gast::Expression::Not(Box::new(operand.expression))),
        }
    }

    fn lower_binary_at(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let lhs_type = self.view.expression(lhs).expect("checked lhs").value_type();
        let rhs_type = self.view.expression(rhs).expect("checked rhs").value_type();
        if operator == dae::BinaryOperator::Multiply
            && indices.is_empty()
            && matches!(
                (lhs_type.dimensions(), rhs_type.dimensions()),
                ([lhs_extent], [rhs_extent]) if lhs_extent == rhs_extent
            )
        {
            let extent = lhs_type.dimensions()[0];
            let mut terms = Vec::with_capacity(extent as usize);
            for index in 1..=extent {
                let index = [gast::Expression::Integer(i64::from(index))];
                let lhs = self.lower_at(lhs, &index)?;
                let rhs = self.lower_at(rhs, &index)?;
                terms.push(lower_binary(operator, lhs, rhs, scalar_type, span)?);
            }
            let expression = terms
                .into_iter()
                .reduce(|lhs, rhs| gast::Expression::binary(gast::BinaryOp::Add, lhs, rhs))
                .ok_or_else(|| {
                    unsupported(
                        "zero-dot-product",
                        "zero-length dot product requires an explicit additive identity".to_owned(),
                        span,
                    )
                })?;
            return Ok(TypedExpression {
                expression,
                scalar_type,
            });
        }
        let lhs_indices = operand_projection(lhs_type.dimensions(), indices, span)?;
        let rhs_indices = operand_projection(rhs_type.dimensions(), indices, span)?;
        let lhs = self.lower_at(lhs, &lhs_indices)?;
        let rhs = self.lower_at(rhs, &rhs_indices)?;
        let expression = lower_binary(operator, lhs, rhs, scalar_type, span)?;
        Ok(TypedExpression {
            expression,
            scalar_type,
        })
    }

    fn lower_call_at(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if self.call_frames.iter().any(|frame| frame.call == call) {
            return Err(unsupported(
                "recursive-function",
                "recursive checked function cannot be inlined into GALEC".to_owned(),
                span,
            ));
        }
        let function_view = self
            .view
            .function(function)
            .expect("checked function identity resolves");
        let result = function_view
            .result_values()
            .rhs(output as usize)
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!("checked function output {output} is missing"),
            })?;
        self.call_frames.push(CallFrame {
            call,
            function,
            arguments: arguments.iter().collect(),
        });
        let lowered = self.lower_at(result, indices);
        self.call_frames.pop();
        lowered
    }

    fn lower_index_at(
        &mut self,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        projection: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let mut projected = projection.iter();
        let mut base_indices = Vec::with_capacity(
            self.view
                .expression(base)
                .expect("checked indexed base")
                .value_type()
                .dimensions()
                .len(),
        );
        for subscript in subscripts.iter() {
            match subscript {
                dae::SubscriptView::Index { expression, .. } => {
                    base_indices.push(self.lower(expression)?.expression);
                }
                dae::SubscriptView::Whole { .. } => {
                    base_indices.push(next_projected_index(&mut projected, "whole", span)?);
                }
                dae::SubscriptView::Slice { expression, .. } => {
                    let selected = next_projected_index(&mut projected, "slice", span)?;
                    base_indices.push(self.lower_slice_index(expression, selected, span)?);
                }
            }
        }
        base_indices.extend(projected.cloned());
        self.lower_at(base, &base_indices)
    }

    fn lower_slice_index(
        &mut self,
        expression: dae::ExprId<'dae>,
        projected: gast::Expression,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let ordinal = match projected {
            gast::Expression::Integer(value) if value >= 1 => value,
            _ => {
                return Err(unsupported(
                    "dynamic-slice-projection",
                    "GALEC slice projection requires a constructor-proven literal ordinal"
                        .to_owned(),
                    span,
                ));
            }
        };
        let node = self
            .view
            .expression(expression)
            .expect("checked slice expression");
        if let dae::ExpressionOperation::Range { start, step, .. } = node.operation() {
            return start
                .checked_add((ordinal - 1).saturating_mul(step))
                .map(gast::Expression::Integer)
                .ok_or_else(|| {
                    unsupported(
                        "slice-overflow",
                        "slice index arithmetic overflowed".to_owned(),
                        span,
                    )
                });
        }
        self.lower_at(expression, &[gast::Expression::Integer(ordinal)])
            .map(|value| value.expression)
    }

    fn lower_array_at(
        &mut self,
        elements: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let (first, rest) = indices.split_first().ok_or_else(|| {
            unsupported(
                "array-projection",
                "array constructor requires an element index".to_owned(),
                span,
            )
        })?;
        let ordinal = match first {
            gast::Expression::Integer(value) => usize::try_from(*value)
                .ok()
                .and_then(|value| value.checked_sub(1)),
            _ => None,
        }
        .ok_or_else(|| {
            unsupported(
                "dynamic-array-constructor-index",
                "array constructor projection requires a positive literal index".to_owned(),
                span,
            )
        })?;
        let element = elements.get(ordinal).ok_or_else(|| {
            unsupported(
                "array-constructor-index",
                "array constructor projection is outside its checked extent".to_owned(),
                span,
            )
        })?;
        self.lower_at(element, rest)
    }

    fn lower_elementwise_builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if builtin == dae::PureBuiltin::Linspace {
            return self.lower_linspace_element(arguments, indices, span);
        }
        if builtin == dae::PureBuiltin::Cross {
            return self.lower_cross_element(arguments, indices, scalar_type, span);
        }
        if builtin == dae::PureBuiltin::Zeros || builtin == dae::PureBuiltin::Ones {
            return Ok(TypedExpression {
                expression: gast::Expression::Real(if builtin == dae::PureBuiltin::Ones {
                    1.0
                } else {
                    0.0
                }),
                scalar_type,
            });
        }
        if builtin == dae::PureBuiltin::Fill {
            return self.lower_at(arguments.get(0).expect("checked fill value argument"), &[]);
        }
        if matches!(
            builtin,
            dae::PureBuiltin::Smooth | dae::PureBuiltin::NoEvent | dae::PureBuiltin::Homotopy
        ) {
            let ordinal = usize::from(builtin == dae::PureBuiltin::Smooth);
            return self.lower_at(
                arguments
                    .get(ordinal)
                    .expect("checked transparent builtin argument"),
                indices,
            );
        }
        let mut lowered = Vec::with_capacity(arguments.len());
        for argument in arguments.iter() {
            let dimensions = self
                .view
                .expression(argument)
                .expect("checked builtin argument")
                .value_type()
                .dimensions();
            let projection = operand_projection(dimensions, indices, span)?;
            lowered.push(self.lower_at(argument, &projection)?);
        }
        let expression = lower_builtin_arguments(builtin, lowered, span)?;
        Ok(TypedExpression {
            expression,
            scalar_type,
        })
    }

    fn lower_linspace_element(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let [index] = indices else {
            unreachable!("checked linspace result has rank one")
        };
        let start = coerce(
            self.lower_at(arguments.get(0).expect("checked linspace start"), &[])?,
            gast::ScalarType::Real,
            span,
        )?;
        let stop = coerce(
            self.lower_at(arguments.get(1).expect("checked linspace stop"), &[])?,
            gast::ScalarType::Real,
            span,
        )?;
        let count = coerce(
            self.lower_at(arguments.get(2).expect("checked linspace extent"), &[])?,
            gast::ScalarType::Real,
            span,
        )?;
        let index = coerce(
            TypedExpression {
                expression: index.clone(),
                scalar_type: gast::ScalarType::Integer,
            },
            gast::ScalarType::Real,
            span,
        )?;
        let one = gast::Expression::Real(1.0);
        let offset = gast::Expression::binary(gast::BinaryOp::Sub, index, one.clone());
        let width = gast::Expression::binary(gast::BinaryOp::Sub, count, one);
        let delta = gast::Expression::binary(gast::BinaryOp::Sub, stop, start.clone());
        let scaled = gast::Expression::binary(gast::BinaryOp::Mul, delta, offset);
        Ok(TypedExpression {
            expression: gast::Expression::binary(
                gast::BinaryOp::Add,
                start,
                gast::Expression::binary(gast::BinaryOp::Div, scaled, width),
            ),
            scalar_type: gast::ScalarType::Real,
        })
    }

    fn lower_cross_element(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        let [index] = indices else {
            unreachable!("checked cross result has rank one")
        };
        let mut component = |ordinal: usize| {
            let (first, second) = [(1, 2), (2, 0), (0, 1)][ordinal];
            let first = [gast::Expression::Integer(i64::from(first + 1))];
            let second = [gast::Expression::Integer(i64::from(second + 1))];
            let lhs_first = self.lower_at(arguments.get(0).expect("checked cross lhs"), &first)?;
            let rhs_second =
                self.lower_at(arguments.get(1).expect("checked cross rhs"), &second)?;
            let positive = lower_binary(
                dae::BinaryOperator::Multiply,
                lhs_first,
                rhs_second,
                scalar_type,
                span,
            )?;
            let lhs_second =
                self.lower_at(arguments.get(0).expect("checked cross lhs"), &second)?;
            let rhs_first = self.lower_at(arguments.get(1).expect("checked cross rhs"), &first)?;
            let negative = lower_binary(
                dae::BinaryOperator::Multiply,
                lhs_second,
                rhs_first,
                scalar_type,
                span,
            )?;
            Ok::<_, GalecTargetError>(gast::Expression::binary(
                gast::BinaryOp::Sub,
                positive,
                negative,
            ))
        };
        let expression = if let gast::Expression::Integer(index) = index {
            component(usize::try_from(*index - 1).expect("checked cross index is 1..=3"))?
        } else {
            let first = component(0)?;
            let second = component(1)?;
            let third = component(2)?;
            gast::Expression::If(gast::IfExpression {
                branches: vec![
                    (
                        gast::Expression::binary(
                            gast::BinaryOp::Eq,
                            index.clone(),
                            gast::Expression::Integer(1),
                        ),
                        first,
                    ),
                    (
                        gast::Expression::binary(
                            gast::BinaryOp::Eq,
                            index.clone(),
                            gast::Expression::Integer(2),
                        ),
                        second,
                    ),
                ],
                else_value: Box::new(third),
            })
        };
        Ok(TypedExpression {
            expression,
            scalar_type,
        })
    }

    fn coordinate_at(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<TypedExpression, GalecTargetError> {
        if let dae::CoordinateView::Algebraic(variable) = coordinate
            && let Some(definition) = self.definitions.definition(variable)
        {
            return self.lower_at(definition, indices);
        }
        if let dae::CoordinateView::FunctionParameter(parameter) = coordinate {
            let argument = self
                .call_frames
                .iter()
                .rev()
                .find(|frame| frame.function == parameter.function())
                .and_then(|frame| frame.arguments.get(parameter.ordinal() as usize))
                .copied()
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: "function parameter used without its checked call frame".to_owned(),
                })?;
            return self.lower_at(argument, indices);
        }
        let (variable, previous) = coordinate_variable(coordinate, span)?;
        let classified = self.by_id.get(&variable.index()).ok_or_else(|| {
            GalecTargetError::UnknownVariableReference {
                name: format!("#{}", variable.index()),
                span: Some(span),
            }
        })?;
        let name = if previous {
            self.pre_names
                .get(&variable.index())
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: format!(
                        "pre-coordinate for `{}` was not collected",
                        classified.variable.name()
                    ),
                })?
                .clone()
        } else {
            classified.name.clone()
        };
        let expression = if indices
            .iter()
            .all(|index| matches!(index, gast::Expression::Integer(_)))
        {
            gast::Expression::Ref(state_reference_with_subscripts(
                name,
                indices.to_vec(),
                span,
            ))
        } else {
            self.lower_dynamic_reference(classified, name, indices, span)?
        };
        Ok(TypedExpression {
            expression,
            scalar_type: classified.scalar_type,
        })
    }

    fn lower_dynamic_reference(
        &self,
        classified: &ClassifiedVariable<'dae>,
        name: gast::Name,
        indices: &[gast::Expression],
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let dimensions = classified.variable.value_type().dimensions();
        if dimensions.len() != indices.len() {
            return Err(unsupported(
                "dynamic-array-index",
                "dynamic reference does not have one index per checked dimension".to_owned(),
                span,
            ));
        }
        for (index, extent) in indices.iter().zip(dimensions) {
            if !matches!(index, gast::Expression::Integer(_)) {
                self.prove_dynamic_index(index, *extent, span)?;
            }
        }
        let candidates = row_major_indices(dimensions)
            .into_iter()
            .filter(|candidate| {
                indices.iter().zip(candidate).all(|(index, candidate)| {
                    !matches!(index, gast::Expression::Integer(found) if *found != i64::from(*candidate))
                })
            })
            .collect::<Vec<_>>();
        let (fallback, branches) = candidates.split_last().ok_or_else(|| {
            unsupported(
                "dynamic-array-index",
                "checked dynamic index domain is empty".to_owned(),
                span,
            )
        })?;
        let branches = branches
            .iter()
            .map(|candidate| {
                let condition = indices
                    .iter()
                    .zip(candidate)
                    .filter(|(index, _)| !matches!(index, gast::Expression::Integer(_)))
                    .map(|(index, candidate)| {
                        gast::Expression::binary(
                            gast::BinaryOp::Eq,
                            index.clone(),
                            gast::Expression::Integer(i64::from(*candidate)),
                        )
                    })
                    .reduce(|lhs, rhs| gast::Expression::binary(gast::BinaryOp::And, lhs, rhs))
                    .expect("dynamic reference has at least one dynamic index");
                (
                    condition,
                    gast::Expression::Ref(state_reference_indexed(name.clone(), candidate, span)),
                )
            })
            .collect();
        Ok(gast::Expression::If(gast::IfExpression {
            branches,
            else_value: Box::new(gast::Expression::Ref(state_reference_indexed(
                name, fallback, span,
            ))),
        }))
    }

    fn prove_dynamic_index(
        &self,
        index: &gast::Expression,
        extent: u32,
        span: Span,
    ) -> Result<(), GalecTargetError> {
        let gast::Expression::Ref(gast::Reference::State(parts)) = index else {
            return Err(unsupported(
                "dynamic-array-index",
                "dynamic index needs a directly bounded checked coordinate".to_owned(),
                span,
            ));
        };
        let [part] = parts.as_slice() else {
            return Err(unsupported(
                "dynamic-array-index",
                "dynamic index needs one directly bounded checked coordinate".to_owned(),
                span,
            ));
        };
        let variable = self
            .by_id
            .values()
            .find(|variable| variable.name.lexeme() == part.name.lexeme())
            .ok_or_else(|| {
                unsupported(
                    "dynamic-array-index",
                    "dynamic index coordinate is not in the checked variable environment"
                        .to_owned(),
                    span,
                )
            })?;
        let minimum = variable
            .variable
            .minimum()
            .and_then(|value| literal_integer(self.view, value));
        let maximum = variable
            .variable
            .maximum()
            .and_then(|value| literal_integer(self.view, value));
        if minimum.is_some_and(|minimum| minimum >= 1)
            && maximum.is_some_and(|maximum| maximum <= i64::from(extent))
        {
            Ok(())
        } else {
            Err(unsupported(
                "dynamic-array-index",
                format!(
                    "index `{}` lacks proven bounds within 1:{extent}",
                    variable.variable.name()
                ),
                span,
            ))
        }
    }

    fn lower_conditional_at(
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

const fn causality_name(causality: dae::VariableCausality) -> &'static str {
    match causality {
        dae::VariableCausality::Input => "input",
        dae::VariableCausality::Output => "output",
        dae::VariableCausality::Parameter => "parameter",
        dae::VariableCausality::CalculatedParameter => "calculatedParameter",
        dae::VariableCausality::Independent => "independent",
        dae::VariableCausality::Local => "local",
    }
}

const fn role_name(role: dae::VariableRole) -> &'static str {
    match role {
        dae::VariableRole::Parameter => "parameter",
        dae::VariableRole::Constant => "constant",
        dae::VariableRole::Input => "input",
        dae::VariableRole::State => "state",
        dae::VariableRole::Algebraic => "algebraic",
        dae::VariableRole::Output => "output",
        dae::VariableRole::DiscreteReal => "discrete Real",
        dae::VariableRole::DiscreteValue => "discrete value",
    }
}

const fn origin_name(origin: dae::VariableOrigin) -> &'static str {
    match origin {
        dae::VariableOrigin::Source => "source",
        dae::VariableOrigin::Generated => "generated",
    }
}

const fn event_name(operation: dae::EventActionOperation<'_>) -> &'static str {
    match operation {
        dae::EventActionOperation::Assert { .. } => "assert",
        dae::EventActionOperation::Terminate { .. } => "terminate",
        dae::EventActionOperation::Reinitialize { .. } => "reinitialize",
        dae::EventActionOperation::AssignDiscreteReal { .. } => "assign discrete Real",
    }
}
