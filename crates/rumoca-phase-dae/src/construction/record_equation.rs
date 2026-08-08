use super::*;

pub(super) fn lower_record_equation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    equation: &flat::Equation,
    plan: &RecordEquationPlan,
    owner: dae::DaeProvenance,
    initialization: bool,
) -> Result<(), dae::DaeConstructionError> {
    let Expression::Binary {
        op: OpBinary::Sub,
        rhs,
        ..
    } = &equation.residual
    else {
        unreachable!("record equation certificate has a subtraction residual")
    };
    let aggregate = plan
        .fields
        .iter()
        .any(|field| {
            matches!(
                field.value,
                RecordEquationFieldValue::AggregateProjection(_)
            )
        })
        .then(|| lower_expression(construction, coordinates, functions, rhs, None))
        .transpose()?;
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::RecordEquationProjection, equation.span)?;
    for field in &plan.fields {
        let lhs = construction.expressions(|expressions| {
            expressions
                .at(generated)
                .coordinate(coordinates[&field.target].current())
        })?;
        let rhs = match &field.value {
            RecordEquationFieldValue::AggregateProjection(projection) => lower_record_projection(
                construction,
                aggregate.expect("an aggregate record equation lowers one aggregate value"),
                projection,
                generated,
            )?,
            RecordEquationFieldValue::Coordinate(source) => {
                construction.expressions(|expressions| {
                    expressions
                        .at(generated)
                        .coordinate(coordinates[source].current())
                })?
            }
        };
        let residual = construction.expressions(|expressions| {
            expressions
                .at(generated)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        if initialization {
            construction.initialization(|system| system.value_equation(owner, residual))?;
        } else {
            construction.continuous(|system| system.value_equation(owner, residual))?;
        }
    }
    Ok(())
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
