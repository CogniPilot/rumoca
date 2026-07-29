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
    let record = lower_expression(construction, coordinates, functions, rhs, None)?;
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::RecordEquationProjection, equation.span)?;
    for field in &plan.fields {
        let lhs = construction.expressions(|expressions| {
            expressions
                .at(generated)
                .coordinate(coordinates[&field.coordinate].current())
        })?;
        let rhs = construction
            .expressions(|expressions| expressions.at(generated).field(record, field.ordinal))?;
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
