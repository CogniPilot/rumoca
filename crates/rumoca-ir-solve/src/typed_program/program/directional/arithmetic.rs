//! The single scalar directional arithmetic relation, also used by compact pointwise lifting.

use super::*;

pub(super) struct DirectionalArithmetic<'builder, 'program> {
    pub(super) builder: &'builder mut TypedProgramBuilder<'program>,
}

impl<'program> DirectionalArithmetic<'_, 'program> {
    pub(super) fn zero(
        &mut self,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let scalar = self
            .builder
            .constant(SolveValue::real(self.builder.arithmetic, 0.0), provenance)?;
        if value_type.dimensions().is_empty() {
            Ok(scalar)
        } else {
            self.builder
                .fill(scalar, value_type.dimensions().to_vec(), provenance)
        }
    }

    fn constant_like(
        &mut self,
        value_type: &SolveValueType,
        value: f64,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let scalar = self
            .builder
            .constant(SolveValue::real(self.builder.arithmetic, value), provenance)?;
        if value_type.dimensions().is_empty() {
            Ok(scalar)
        } else {
            self.builder
                .fill(scalar, value_type.dimensions().to_vec(), provenance)
        }
    }

    pub(super) fn tangent_or_zero(
        &mut self,
        value: Directional<ProgramRegister<'program>>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        match value.tangent {
            Some(tangent) => Ok(tangent),
            None => self.zero(value_type, provenance),
        }
    }

    pub(super) fn derive_unary(
        &mut self,
        operator: SolveUnaryOperator,
        operand: Directional<ProgramRegister<'program>>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let primal = self.builder.unary(operator, operand.primal, provenance)?;
        self.derive_unary_from_primal(operator, operand, primal, value_type, provenance)
    }

    // SPEC_0021: Exception - exhaustive tangent relation over every checked
    // unary operator; keeping primal and tangent clauses adjacent makes the
    // construction proof reviewable as one total match.
    // SPEC_0021: Exception - cohesive exhaustive flow stays contiguous so ordering remains auditable.
    #[allow(clippy::too_many_lines)]
    pub(super) fn derive_unary_from_primal(
        &mut self,
        operator: SolveUnaryOperator,
        operand: Directional<ProgramRegister<'program>>,
        primal: ProgramRegister<'program>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
        if !is_real(value_type) {
            return Ok(Directional {
                primal,
                tangent: None,
            });
        }
        let tangent = self.tangent_or_zero(operand, value_type, provenance)?;
        let zero = self.zero(value_type, provenance)?;
        let derivative = match operator {
            SolveUnaryOperator::Negate => {
                self.builder
                    .unary(SolveUnaryOperator::Negate, tangent, provenance)?
            }
            SolveUnaryOperator::Not => zero,
            SolveUnaryOperator::Abs => {
                let negative =
                    self.builder
                        .unary(SolveUnaryOperator::Negate, tangent, provenance)?;
                let condition = self.builder.compare(
                    SolveCompareOperator::GreaterEqual,
                    operand.primal,
                    zero,
                    provenance,
                )?;
                self.builder
                    .select(condition, tangent, negative, provenance)?
            }
            SolveUnaryOperator::Sign
            | SolveUnaryOperator::Floor
            | SolveUnaryOperator::Ceiling
            | SolveUnaryOperator::Truncate => zero,
            SolveUnaryOperator::Sin => {
                let factor =
                    self.builder
                        .unary(SolveUnaryOperator::Cos, operand.primal, provenance)?;
                self.builder
                    .binary(SolveBinaryOperator::Multiply, tangent, factor, provenance)?
            }
            SolveUnaryOperator::Cos => {
                let factor =
                    self.builder
                        .unary(SolveUnaryOperator::Sin, operand.primal, provenance)?;
                let factor = self
                    .builder
                    .unary(SolveUnaryOperator::Negate, factor, provenance)?;
                self.builder
                    .binary(SolveBinaryOperator::Multiply, tangent, factor, provenance)?
            }
            SolveUnaryOperator::Tan => {
                let factor =
                    self.builder
                        .unary(SolveUnaryOperator::Cos, operand.primal, provenance)?;
                let denominator = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    factor,
                    factor,
                    provenance,
                )?;
                self.builder.binary(
                    SolveBinaryOperator::Divide,
                    tangent,
                    denominator,
                    provenance,
                )?
            }
            SolveUnaryOperator::Asin | SolveUnaryOperator::Acos => {
                let one = self.constant_like(value_type, 1.0, provenance)?;
                let square = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    operand.primal,
                    operand.primal,
                    provenance,
                )?;
                let denominator =
                    self.builder
                        .binary(SolveBinaryOperator::Subtract, one, square, provenance)?;
                let denominator =
                    self.builder
                        .unary(SolveUnaryOperator::Sqrt, denominator, provenance)?;
                let reciprocal = self.builder.binary(
                    SolveBinaryOperator::Divide,
                    one,
                    denominator,
                    provenance,
                )?;
                let partial = if operator == SolveUnaryOperator::Acos {
                    self.builder
                        .unary(SolveUnaryOperator::Negate, reciprocal, provenance)?
                } else {
                    reciprocal
                };
                self.scale_by_finite_partial(tangent, partial, zero, provenance)?
            }
            SolveUnaryOperator::Atan => {
                let one = self.constant_like(value_type, 1.0, provenance)?;
                let square = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    operand.primal,
                    operand.primal,
                    provenance,
                )?;
                let denominator =
                    self.builder
                        .binary(SolveBinaryOperator::Add, one, square, provenance)?;
                self.builder.binary(
                    SolveBinaryOperator::Divide,
                    tangent,
                    denominator,
                    provenance,
                )?
            }
            SolveUnaryOperator::Sinh | SolveUnaryOperator::Cosh => {
                let derivative_operator = if operator == SolveUnaryOperator::Sinh {
                    SolveUnaryOperator::Cosh
                } else {
                    SolveUnaryOperator::Sinh
                };
                let factor = self
                    .builder
                    .unary(derivative_operator, operand.primal, provenance)?;
                self.builder
                    .binary(SolveBinaryOperator::Multiply, tangent, factor, provenance)?
            }
            SolveUnaryOperator::Tanh => {
                let factor =
                    self.builder
                        .unary(SolveUnaryOperator::Cosh, operand.primal, provenance)?;
                let denominator = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    factor,
                    factor,
                    provenance,
                )?;
                self.builder.binary(
                    SolveBinaryOperator::Divide,
                    tangent,
                    denominator,
                    provenance,
                )?
            }
            SolveUnaryOperator::Exp => {
                self.builder
                    .binary(SolveBinaryOperator::Multiply, tangent, primal, provenance)?
            }
            SolveUnaryOperator::Log | SolveUnaryOperator::Log10 => {
                let denominator = if operator == SolveUnaryOperator::Log10 {
                    let ln10 =
                        self.constant_like(value_type, std::f64::consts::LN_10, provenance)?;
                    self.builder.binary(
                        SolveBinaryOperator::Multiply,
                        operand.primal,
                        ln10,
                        provenance,
                    )?
                } else {
                    operand.primal
                };
                let one = self.constant_like(value_type, 1.0, provenance)?;
                let partial = self.builder.binary(
                    SolveBinaryOperator::Divide,
                    one,
                    denominator,
                    provenance,
                )?;
                self.scale_by_finite_partial(tangent, partial, zero, provenance)?
            }
            SolveUnaryOperator::Sqrt => {
                let half = self.constant_like(value_type, 0.5, provenance)?;
                let partial =
                    self.builder
                        .binary(SolveBinaryOperator::Divide, half, primal, provenance)?;
                self.scale_by_finite_partial(tangent, partial, zero, provenance)?
            }
        };
        Ok(Directional {
            primal,
            tangent: Some(derivative),
        })
    }

    // SPEC_0021: Exception - exhaustive tangent relation over every checked
    // binary operator, including the scalar AD singular-value guards.
    // SPEC_0021: Exception - cohesive exhaustive flow stays contiguous so ordering remains auditable.
    #[allow(clippy::too_many_lines)]
    pub(super) fn derive_binary(
        &mut self,
        operator: SolveBinaryOperator,
        lhs: Directional<ProgramRegister<'program>>,
        rhs: Directional<ProgramRegister<'program>>,
        value_type: &SolveValueType,
        provenance: Span,
    ) -> Result<Directional<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let direct_primal = self
            .builder
            .binary(operator, lhs.primal, rhs.primal, provenance)?;
        if !is_real(value_type) {
            return Ok(Directional {
                primal: direct_primal,
                tangent: None,
            });
        }
        let zero = self.zero(value_type, provenance)?;
        let primal = if operator == SolveBinaryOperator::Divide {
            let denominator_is_zero =
                self.builder
                    .compare(SolveCompareOperator::Equal, rhs.primal, zero, provenance)?;
            let numerator_is_zero =
                self.builder
                    .compare(SolveCompareOperator::Equal, lhs.primal, zero, provenance)?;
            let zero_over_zero =
                self.builder
                    .select(numerator_is_zero, zero, direct_primal, provenance)?;
            self.builder.select(
                denominator_is_zero,
                zero_over_zero,
                direct_primal,
                provenance,
            )?
        } else {
            direct_primal
        };
        let lhs_tangent = self.tangent_or_zero(lhs, value_type, provenance)?;
        let rhs_tangent = self.tangent_or_zero(rhs, value_type, provenance)?;
        let tangent = match operator {
            SolveBinaryOperator::Add | SolveBinaryOperator::Subtract => {
                self.builder
                    .binary(operator, lhs_tangent, rhs_tangent, provenance)?
            }
            SolveBinaryOperator::Multiply => {
                let first = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    lhs_tangent,
                    rhs.primal,
                    provenance,
                )?;
                let second = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    lhs.primal,
                    rhs_tangent,
                    provenance,
                )?;
                self.builder
                    .binary(SolveBinaryOperator::Add, first, second, provenance)?
            }
            SolveBinaryOperator::Divide => self.derive_quotient(
                lhs,
                rhs,
                [lhs_tangent, rhs_tangent],
                (value_type, zero),
                provenance,
            )?,
            SolveBinaryOperator::Power => self.derive_power(
                lhs,
                rhs,
                primal,
                [lhs_tangent, rhs_tangent],
                (value_type, zero),
                provenance,
            )?,
            SolveBinaryOperator::Atan2 => {
                self.derive_atan2(lhs, rhs, [lhs_tangent, rhs_tangent], zero, provenance)?
            }
            SolveBinaryOperator::Min | SolveBinaryOperator::Max => {
                let comparison = if operator == SolveBinaryOperator::Max {
                    SolveCompareOperator::GreaterEqual
                } else {
                    SolveCompareOperator::LessEqual
                };
                let condition = self
                    .builder
                    .compare(comparison, lhs.primal, rhs.primal, provenance)?;
                self.builder
                    .select(condition, lhs_tangent, rhs_tangent, provenance)?
            }
            // A truncated Integer quotient is piecewise constant, like a Boolean.
            SolveBinaryOperator::IntegerQuotient
            | SolveBinaryOperator::And
            | SolveBinaryOperator::Or
            | SolveBinaryOperator::IntegerModulo
            | SolveBinaryOperator::IntegerRemainder => self.zero(value_type, provenance)?,
        };
        Ok(Directional {
            primal,
            tangent: Some(tangent),
        })
    }

    /// `l / r` under the division kink rule of `rumoca_eval_solve::reverse`:
    /// `1 / r` and `-l / r²`, each when finite, else zero.
    fn derive_quotient(
        &mut self,
        lhs: Directional<ProgramRegister<'program>>,
        rhs: Directional<ProgramRegister<'program>>,
        [lhs_tangent, rhs_tangent]: [ProgramRegister<'program>; 2],
        (value_type, zero): (&SolveValueType, ProgramRegister<'program>),
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let one = self.constant_like(value_type, 1.0, provenance)?;
        let lhs_partial =
            self.builder
                .binary(SolveBinaryOperator::Divide, one, rhs.primal, provenance)?;
        let square = self.builder.binary(
            SolveBinaryOperator::Multiply,
            rhs.primal,
            rhs.primal,
            provenance,
        )?;
        let negated = self
            .builder
            .unary(SolveUnaryOperator::Negate, lhs.primal, provenance)?;
        let rhs_partial =
            self.builder
                .binary(SolveBinaryOperator::Divide, negated, square, provenance)?;
        self.sum_of_scaled_partials(
            [(lhs_tangent, lhs_partial), (rhs_tangent, rhs_partial)],
            zero,
            provenance,
        )
    }

    /// `atan2(l, r)` under the kink rule of `rumoca_eval_solve::reverse`:
    /// `r / (l² + r²)` and `-l / (l² + r²)`, each when finite, else zero, so
    /// the origin contributes nothing.
    fn derive_atan2(
        &mut self,
        lhs: Directional<ProgramRegister<'program>>,
        rhs: Directional<ProgramRegister<'program>>,
        [lhs_tangent, rhs_tangent]: [ProgramRegister<'program>; 2],
        zero: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let lhs_square = self.builder.binary(
            SolveBinaryOperator::Multiply,
            lhs.primal,
            lhs.primal,
            provenance,
        )?;
        let rhs_square = self.builder.binary(
            SolveBinaryOperator::Multiply,
            rhs.primal,
            rhs.primal,
            provenance,
        )?;
        let denominator =
            self.builder
                .binary(SolveBinaryOperator::Add, lhs_square, rhs_square, provenance)?;
        let lhs_partial = self.builder.binary(
            SolveBinaryOperator::Divide,
            rhs.primal,
            denominator,
            provenance,
        )?;
        let negated = self
            .builder
            .unary(SolveUnaryOperator::Negate, lhs.primal, provenance)?;
        let rhs_partial = self.builder.binary(
            SolveBinaryOperator::Divide,
            negated,
            denominator,
            provenance,
        )?;
        self.sum_of_scaled_partials(
            [(lhs_tangent, lhs_partial), (rhs_tangent, rhs_partial)],
            zero,
            provenance,
        )
    }

    /// `du_l · ∂l + du_r · ∂r`, each partial zeroed where it is not finite.
    fn sum_of_scaled_partials(
        &mut self,
        [(lhs_tangent, lhs_partial), (rhs_tangent, rhs_partial)]: [(ProgramRegister<'program>, ProgramRegister<'program>);
            2],
        zero: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let lhs_term = self.scale_by_finite_partial(lhs_tangent, lhs_partial, zero, provenance)?;
        let rhs_term = self.scale_by_finite_partial(rhs_tangent, rhs_partial, zero, provenance)?;
        self.builder
            .binary(SolveBinaryOperator::Add, lhs_term, rhs_term, provenance)
    }

    /// The `pow` kink rule of `rumoca_eval_solve::reverse`: the base partial
    /// `r·l^(r-1)` when finite, the exponent partial `l^r·ln(l)` only for
    /// `l > 0` and when finite, each a function of the primal operands alone.
    pub(super) fn derive_power(
        &mut self,
        lhs: Directional<ProgramRegister<'program>>,
        rhs: Directional<ProgramRegister<'program>>,
        primal: ProgramRegister<'program>,
        [lhs_tangent, rhs_tangent]: [ProgramRegister<'program>; 2],
        (value_type, zero): (&SolveValueType, ProgramRegister<'program>),
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let base = match lhs.tangent {
            None => None,
            Some(_) => {
                let one = self.constant_like(value_type, 1.0, provenance)?;
                let exponent = self.builder.binary(
                    SolveBinaryOperator::Subtract,
                    rhs.primal,
                    one,
                    provenance,
                )?;
                let power = self.builder.binary(
                    SolveBinaryOperator::Power,
                    lhs.primal,
                    exponent,
                    provenance,
                )?;
                let partial = self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    rhs.primal,
                    power,
                    provenance,
                )?;
                Some(self.scale_by_finite_partial(lhs_tangent, partial, zero, provenance)?)
            }
        };
        let exponent = match rhs.tangent {
            None => None,
            Some(_) => {
                let log = self
                    .builder
                    .unary(SolveUnaryOperator::Log, lhs.primal, provenance)?;
                let partial =
                    self.builder
                        .binary(SolveBinaryOperator::Multiply, primal, log, provenance)?;
                let finite = self.finite_or_zero(partial, zero, provenance)?;
                let lhs_positive = self.builder.compare(
                    SolveCompareOperator::Greater,
                    lhs.primal,
                    zero,
                    provenance,
                )?;
                let partial = self
                    .builder
                    .select(lhs_positive, finite, zero, provenance)?;
                Some(self.builder.binary(
                    SolveBinaryOperator::Multiply,
                    rhs_tangent,
                    partial,
                    provenance,
                )?)
            }
        };
        match (base, exponent) {
            (Some(base), Some(exponent)) => {
                self.builder
                    .binary(SolveBinaryOperator::Add, base, exponent, provenance)
            }
            (Some(term), None) | (None, Some(term)) => Ok(term),
            (None, None) => Ok(zero),
        }
    }

    /// `tangent · partial` when the local partial is finite, and zero where it
    /// does not exist (the `rumoca_eval_solve::reverse` kink rules).
    fn scale_by_finite_partial(
        &mut self,
        tangent: ProgramRegister<'program>,
        partial: ProgramRegister<'program>,
        zero: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let partial = self.finite_or_zero(partial, zero, provenance)?;
        self.builder
            .binary(SolveBinaryOperator::Multiply, tangent, partial, provenance)
    }

    /// `value` when finite, else zero: `value - value` is `0` exactly for every
    /// finite value and NaN for an infinite or NaN one.
    fn finite_or_zero(
        &mut self,
        value: ProgramRegister<'program>,
        zero: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let difference =
            self.builder
                .binary(SolveBinaryOperator::Subtract, value, value, provenance)?;
        let finite =
            self.builder
                .compare(SolveCompareOperator::Equal, difference, zero, provenance)?;
        self.builder.select(finite, value, zero, provenance)
    }
}
