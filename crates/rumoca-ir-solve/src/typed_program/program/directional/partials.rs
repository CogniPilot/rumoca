//! Scalar partial guards shared by scalar and pointwise tensor tangents.

use super::*;

pub(super) fn logarithm_tangent<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    operand: ProgramRegister<'program>,
    tangent: ProgramRegister<'program>,
    base_ten: bool,
    zero: ProgramRegister<'program>,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let denominator = if base_ten {
        let ln10 = builder.constant(
            SolveValue::real(builder.arithmetic, std::f64::consts::LN_10),
            provenance,
        )?;
        builder.binary(SolveBinaryOperator::Multiply, operand, ln10, provenance)?
    } else {
        operand
    };
    let one = builder.constant(SolveValue::real(builder.arithmetic, 1.0), provenance)?;
    let partial = builder.binary(SolveBinaryOperator::Divide, one, denominator, provenance)?;
    scale_by_finite_partial(builder, tangent, partial, zero, provenance)
}

pub(super) fn quotient_tangent<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    [lhs, rhs]: [ProgramRegister<'program>; 2],
    [lhs_tangent, rhs_tangent]: [ProgramRegister<'program>; 2],
    zero: ProgramRegister<'program>,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let one = builder.constant(SolveValue::real(builder.arithmetic, 1.0), provenance)?;
    let lhs_partial = builder.binary(SolveBinaryOperator::Divide, one, rhs, provenance)?;
    let square = builder.binary(SolveBinaryOperator::Multiply, rhs, rhs, provenance)?;
    let negated = builder.unary(SolveUnaryOperator::Negate, lhs, provenance)?;
    let rhs_partial = builder.binary(SolveBinaryOperator::Divide, negated, square, provenance)?;
    sum_of_scaled_partials(
        builder,
        [(lhs_tangent, lhs_partial), (rhs_tangent, rhs_partial)],
        zero,
        provenance,
    )
}

pub(super) fn sum_of_scaled_partials<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    [(lhs_tangent, lhs_partial), (rhs_tangent, rhs_partial)]: [(ProgramRegister<'program>, ProgramRegister<'program>);
        2],
    zero: ProgramRegister<'program>,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let lhs_term = scale_by_finite_partial(builder, lhs_tangent, lhs_partial, zero, provenance)?;
    let rhs_term = scale_by_finite_partial(builder, rhs_tangent, rhs_partial, zero, provenance)?;
    builder.binary(SolveBinaryOperator::Add, lhs_term, rhs_term, provenance)
}

pub(super) fn scale_by_finite_partial<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    tangent: ProgramRegister<'program>,
    partial: ProgramRegister<'program>,
    zero: ProgramRegister<'program>,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let partial = finite_or_zero(builder, partial, zero, provenance)?;
    builder.binary(SolveBinaryOperator::Multiply, tangent, partial, provenance)
}

/// `value - value` is zero for finite values and NaN for every non-finite one.
pub(super) fn finite_or_zero<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    value: ProgramRegister<'program>,
    zero: ProgramRegister<'program>,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let difference = builder.binary(SolveBinaryOperator::Subtract, value, value, provenance)?;
    let finite = builder.compare(SolveCompareOperator::Equal, difference, zero, provenance)?;
    builder.select(finite, value, zero, provenance)
}
