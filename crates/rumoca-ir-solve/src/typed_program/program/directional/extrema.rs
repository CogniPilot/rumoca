//! Ordered extrema tangents over the checked aggregate's row-major domain.

use super::*;

pub(super) fn reduction_tangent<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    operand: ProgramRegister<'program>,
    tangent: ProgramRegister<'program>,
    minimum: bool,
    value_type: &SolveValueType,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let domain = pointwise::pointwise_domain(value_type, builder.arithmetic, provenance)?;
    let indices = vec![0; value_type.dimensions().len()];
    let first = builder.project_element(operand, indices.clone(), provenance)?;
    let first_tangent = builder.project_element(tangent, indices, provenance)?;
    let first_pass = builder.constant(SolveValue::boolean(true), provenance)?;
    // The primal Reduce starts at the actual first cell. Skip its comparison
    // on the first pass, preserving NaN and signed-zero behavior without an
    // invented extremum identity (SOLVE-C56).
    let values = builder.fold(
        domain,
        &[first, first_tangent, first_pass],
        &[operand, tangent],
        provenance,
        |builder, carried, captures, binders, outputs| {
            reduce_step(
                builder, carried, captures, binders, outputs, minimum, provenance,
            )
        },
    )?;
    let [_, tangent, _] = values.as_slice() else {
        return Err(SolveProgramConstructionError::InvalidFold { provenance });
    };
    Ok(*tangent)
}

fn reduce_step<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    carried: &[ProgramSlot<'program>],
    captures: &[ProgramSlot<'program>],
    binders: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
    minimum: bool,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let coordinates = binders
        .iter()
        .map(|binder| builder.load(*binder, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    let lhs = builder.load(carried[0], provenance)?;
    let lhs_tangent = builder.load(carried[1], provenance)?;
    let first_pass = builder.load(carried[2], provenance)?;
    let aggregate = builder.load(captures[0], provenance)?;
    let rhs = builder.project_element_dynamic(aggregate, &coordinates, provenance)?;
    let aggregate_tangent = builder.load(captures[1], provenance)?;
    let rhs_tangent =
        builder.project_element_dynamic(aggregate_tangent, &coordinates, provenance)?;
    let scalar_type = builder.register_type(lhs, provenance)?.clone();
    let values = builder.conditional(
        first_pass,
        &[lhs, rhs, lhs_tangent, rhs_tangent],
        vec![scalar_type.clone(), scalar_type],
        provenance,
        |builder, captures, outputs| {
            let lhs = builder.load(captures[0], provenance)?;
            let tangent = builder.load(captures[2], provenance)?;
            builder.store(outputs[0], lhs, provenance)?;
            builder.store(outputs[1], tangent, provenance)
        },
        |builder, captures, outputs| reduce_pair(builder, captures, outputs, minimum, provenance),
    )?;
    let [primal, tangent] = values.as_slice() else {
        return Err(SolveProgramConstructionError::InvalidRegion { provenance });
    };
    builder.store(outputs[0], *primal, provenance)?;
    builder.store(outputs[1], *tangent, provenance)?;
    let next_pass = builder.constant(SolveValue::boolean(false), provenance)?;
    builder.store(outputs[2], next_pass, provenance)
}

fn reduce_pair<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    captures: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
    minimum: bool,
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let lhs = builder.load(captures[0], provenance)?;
    let rhs = builder.load(captures[1], provenance)?;
    let lhs_tangent = builder.load(captures[2], provenance)?;
    let rhs_tangent = builder.load(captures[3], provenance)?;
    let operator = if minimum {
        SolveBinaryOperator::Min
    } else {
        SolveBinaryOperator::Max
    };
    let primal = builder.binary(operator, lhs, rhs, provenance)?;
    let tangent = partials::extremum_tangent(
        builder,
        [lhs, rhs],
        [lhs_tangent, rhs_tangent],
        minimum,
        provenance,
    )?;
    builder.store(outputs[0], primal, provenance)?;
    builder.store(outputs[1], tangent, provenance)
}
