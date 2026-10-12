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

/// The scalar extremum kink rule: retain the left tangent at equality.
pub(super) fn extremum_tangent<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    [lhs, rhs]: [ProgramRegister<'program>; 2],
    [lhs_tangent, rhs_tangent]: [ProgramRegister<'program>; 2],
    minimum: bool,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let comparison = if minimum {
        SolveCompareOperator::LessEqual
    } else {
        SolveCompareOperator::GreaterEqual
    };
    let condition = builder.compare(comparison, lhs, rhs, provenance)?;
    builder.select(condition, lhs_tangent, rhs_tangent, provenance)
}

/// The `pow` kink rule of `rumoca_eval_solve::reverse`: the base partial
/// `r·l^(r-1)` when finite, the exponent partial `l^r·ln(l)` only for
/// `l > 0` and when finite, each a function of the primal operands alone.
pub(super) fn power_tangent<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    [lhs, rhs]: [Directional<ProgramRegister<'program>>; 2],
    primal: ProgramRegister<'program>,
    [lhs_tangent, rhs_tangent]: [ProgramRegister<'program>; 2],
    zero: ProgramRegister<'program>,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let base = match lhs.tangent {
        None => None,
        Some(_) => {
            let one = builder.constant(SolveValue::real(builder.arithmetic, 1.0), provenance)?;
            let exponent =
                builder.binary(SolveBinaryOperator::Subtract, rhs.primal, one, provenance)?;
            let power =
                builder.binary(SolveBinaryOperator::Power, lhs.primal, exponent, provenance)?;
            let partial =
                builder.binary(SolveBinaryOperator::Multiply, rhs.primal, power, provenance)?;
            Some(scale_by_finite_partial(
                builder,
                lhs_tangent,
                partial,
                zero,
                provenance,
            )?)
        }
    };
    let exponent = match rhs.tangent {
        None => None,
        Some(_) => {
            let log = builder.unary(SolveUnaryOperator::Log, lhs.primal, provenance)?;
            let partial = builder.binary(SolveBinaryOperator::Multiply, primal, log, provenance)?;
            let finite = finite_or_zero(builder, partial, zero, provenance)?;
            let lhs_positive =
                builder.compare(SolveCompareOperator::Greater, lhs.primal, zero, provenance)?;
            let partial = builder.select(lhs_positive, finite, zero, provenance)?;
            Some(builder.binary(
                SolveBinaryOperator::Multiply,
                rhs_tangent,
                partial,
                provenance,
            )?)
        }
    };
    match (base, exponent) {
        (Some(base), Some(exponent)) => {
            builder.binary(SolveBinaryOperator::Add, base, exponent, provenance)
        }
        (Some(term), None) | (None, Some(term)) => Ok(term),
        (None, None) => Ok(zero),
    }
}
