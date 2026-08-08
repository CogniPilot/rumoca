use super::*;

pub(super) fn lower_multi_output_equation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    equation: &flat::Equation,
    plan: &MultiOutputEquationPlan,
    owner: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        unreachable!("a multi-output equation plan owns a subtraction residual")
    };
    let Expression::Tuple { elements, .. } = lhs.as_ref() else {
        unreachable!("a multi-output equation plan owns a receiving tuple")
    };
    let Expression::FunctionCall {
        name,
        args,
        is_constructor: false,
        span,
    } = rhs.as_ref()
    else {
        unreachable!("a multi-output equation plan owns a function call")
    };
    let provenance = dae::DaeProvenance::source(*span)?;
    let symbols = LoweringSymbols {
        coordinates,
        functions,
        shapes: functions.shapes.model_values(),
        function_body: None,
        values: None,
        owner_clock: None,
    };
    let call = lower_call_operands(
        construction,
        symbols,
        &HashMap::new(),
        name,
        args,
        provenance,
    )?;
    for (ordinal, target) in plan.outputs.iter().enumerate() {
        if target.is_none() {
            continue;
        }
        let lhs = lower_expression(
            construction,
            coordinates,
            functions,
            &elements[ordinal],
            None,
        )?;
        let rhs = call.result(construction, ordinal, provenance)?;
        let residual = generated_residual(construction, owner, lhs, rhs)?;
        construction.continuous(|system| system.value_equation(owner, residual))?;
    }
    Ok(())
}
