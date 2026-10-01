use super::*;

/// Lower one record equation as its field equations.
///
/// Each field equation joins the system its target's coordinate selects
/// (MLS 3.7 §8.3.1 expands an equality of records member-wise): continuous
/// fields are residuals of the continuous (or initialization) system, a
/// discrete Real field is a discrete Real residual, and the discrete-valued
/// fields are assignments of the equation's one planned B.1c owner. Every
/// discrete field reads its own lowering of the right side, as a discrete
/// receiver of a multi-result equation does. `discrete` is `None` for an
/// initialization equation, whose fields are all initialization residuals.
pub(super) fn lower_record_equation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    (equation, plan): (&flat::Equation, &RecordEquationPlan),
    owner: dae::DaeProvenance,
    mut discrete: Option<MultiOutputDiscreteOwners<'_, 'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    let Expression::Binary {
        op: OpBinary::Sub,
        rhs,
        ..
    } = &equation.residual
    else {
        unreachable!("record equation certificate has a subtraction residual")
    };
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::RecordEquationProjection, equation.span)?;
    let model_equation = discrete.is_some();
    let is_discrete = |field: &RecordEquationFieldPlan| {
        model_equation
            && matches!(
                coordinates[&field.target],
                Coordinate::DiscreteValue(_) | Coordinate::DiscreteReal(_)
            )
    };
    let shared = plan
        .fields
        .iter()
        .any(|field| {
            !is_discrete(field)
                && matches!(
                    field.value,
                    RecordEquationFieldValue::AggregateProjection(_)
                )
        })
        .then(|| lower_expression(construction, coordinates, functions, rhs, None))
        .transpose()?;
    let discrete_owner = record_discrete_owner(coordinates, plan, discrete.as_mut(), owner)?;
    for field in &plan.fields {
        let aggregate = if is_discrete(field) {
            match field.value {
                RecordEquationFieldValue::AggregateProjection(_) => Some(lower_expression(
                    construction,
                    coordinates,
                    functions,
                    rhs,
                    None,
                )?),
                RecordEquationFieldValue::Coordinate(_) => None,
            }
        } else {
            shared
        };
        let value = record_field_value(construction, coordinates, field, aggregate, generated)?;
        match (
            coordinates[&field.target],
            discrete.as_mut(),
            discrete_owner,
        ) {
            (Coordinate::DiscreteValue(target), Some(discrete), Some(handle)) => {
                discrete.discrete_values.always(
                    handle,
                    target,
                    value,
                    owner,
                    dae::DaeProvenance::source(equation.span)?,
                )?;
            }
            (coordinate, discrete, _) => define_field_residual(
                construction,
                (coordinate, value),
                (owner, generated),
                discrete.is_some(),
            )?,
        }
    }
    Ok(())
}

/// The residual `target - value` of one field equation, in the system the
/// target's coordinate selects: initialization for an initial equation, the
/// discrete Real system for a discrete Real target, and otherwise the
/// continuous system.
fn define_field_residual<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    (coordinate, value): (Coordinate<'dae>, dae::ExprId<'dae>),
    (owner, generated): (dae::DaeProvenance, dae::DaeProvenance),
    model_equation: bool,
) -> Result<(), dae::DaeConstructionError> {
    let lhs = construction
        .expressions(|expressions| expressions.at(generated).coordinate(coordinate.current()))?;
    let residual = construction.expressions(|expressions| {
        expressions
            .at(generated)
            .binary(dae::BinaryOperator::Subtract, lhs, value)
    })?;
    match coordinate {
        _ if !model_equation => {
            construction.initialization(|system| system.value_equation(owner, residual))?;
        }
        Coordinate::DiscreteReal(_) => {
            construction.discrete(|system| {
                system.real_equation(owner, |equation| equation.residual(residual))
            })?;
        }
        _ => {
            construction.continuous(|system| system.value_equation(owner, residual))?;
        }
    }
    Ok(())
}

/// The planned owner of the equation's discrete-valued fields, if it has any.
fn record_discrete_owner<'dae>(
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    plan: &RecordEquationPlan,
    discrete: Option<&mut MultiOutputDiscreteOwners<'_, 'dae>>,
    owner: dae::DaeProvenance,
) -> Result<Option<DiscreteValueOwnerHandle>, dae::DaeConstructionError> {
    let targets = plan
        .fields
        .iter()
        .filter(|field| matches!(coordinates[&field.target], Coordinate::DiscreteValue(_)))
        .map(|field| field.target.clone())
        .collect::<Vec<_>>();
    let Some(discrete) = discrete.filter(|_| !targets.is_empty()) else {
        return Ok(None);
    };
    discrete
        .discrete_values
        .owner(owner, targets, coordinates, discrete.topology)
}

/// The value one field equation equates its target with.
fn record_field_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    field: &RecordEquationFieldPlan,
    aggregate: Option<dae::ExprId<'dae>>,
    generated: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    match &field.value {
        RecordEquationFieldValue::AggregateProjection(projection) => lower_record_projection(
            construction,
            aggregate.expect("an aggregate record field reads one lowered aggregate value"),
            projection,
            generated,
        ),
        RecordEquationFieldValue::Coordinate(source) => construction.expressions(|expressions| {
            expressions
                .at(generated)
                .coordinate(coordinates[source].current())
        }),
    }
}

fn lower_record_projection<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    mut value: dae::ExprId<'dae>,
    projection: &[usize],
    generated: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    for ordinal in projection {
        value = construction
            .expressions(|expressions| expressions.at(generated).field(value, *ordinal))?;
    }
    Ok(value)
}
