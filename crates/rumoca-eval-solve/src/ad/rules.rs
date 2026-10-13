use super::*;

impl<'a> AdBuilder<'a> {
    pub(super) fn unary_dual(
        &mut self,
        op: UnaryOp,
        x: DualReg,
    ) -> Result<DualReg, SolveDerivationError> {
        let zero = self.zero_reg()?;
        let out = match op {
            UnaryOp::Neg => {
                let re = self.emit_unary(UnaryOp::Neg, x.re)?;
                let du = self.emit_unary(UnaryOp::Neg, x.du)?;
                DualReg { re, du }
            }
            UnaryOp::Not => {
                let re = self.emit_unary(UnaryOp::Not, x.re)?;
                DualReg { re, du: zero }
            }
            UnaryOp::Abs => {
                let re = self.emit_unary(UnaryOp::Abs, x.re)?;
                let neg_du = self.emit_unary(UnaryOp::Neg, x.du)?;
                let cond = self.emit_compare(CompareOp::Ge, x.re, zero)?;
                let du = self.emit_select(cond, x.du, neg_du)?;
                DualReg { re, du }
            }
            UnaryOp::Sign | UnaryOp::Floor | UnaryOp::Ceil | UnaryOp::Trunc => {
                let re = self.emit_unary(op, x.re)?;
                DualReg { re, du: zero }
            }
            UnaryOp::Sin => self.unary_mul_chain(UnaryOp::Sin, UnaryOp::Cos, x)?,
            UnaryOp::Cos => {
                let re = self.emit_unary(UnaryOp::Cos, x.re)?;
                let sinx = self.emit_unary(UnaryOp::Sin, x.re)?;
                let neg_sinx = self.emit_unary(UnaryOp::Neg, sinx)?;
                let du = self.emit_binary(BinaryOp::Mul, x.du, neg_sinx)?;
                DualReg { re, du }
            }
            UnaryOp::Tan => {
                let re = self.emit_unary(UnaryOp::Tan, x.re)?;
                let cosx = self.emit_unary(UnaryOp::Cos, x.re)?;
                let cos_sq = self.emit_binary(BinaryOp::Mul, cosx, cosx)?;
                let du = self.emit_binary(BinaryOp::Div, x.du, cos_sq)?;
                DualReg { re, du }
            }
            UnaryOp::Asin => self.lower_asin_or_acos(x, false)?,
            UnaryOp::Acos => self.lower_asin_or_acos(x, true)?,
            UnaryOp::Atan => {
                let re = self.emit_unary(UnaryOp::Atan, x.re)?;
                let one = self.one_reg()?;
                let x_sq = self.emit_binary(BinaryOp::Mul, x.re, x.re)?;
                let denom = self.emit_binary(BinaryOp::Add, one, x_sq)?;
                let du = self.emit_binary(BinaryOp::Div, x.du, denom)?;
                DualReg { re, du }
            }
            UnaryOp::Sinh => self.unary_mul_chain(UnaryOp::Sinh, UnaryOp::Cosh, x)?,
            UnaryOp::Cosh => self.unary_mul_chain(UnaryOp::Cosh, UnaryOp::Sinh, x)?,
            UnaryOp::Tanh => {
                let re = self.emit_unary(UnaryOp::Tanh, x.re)?;
                let cosh = self.emit_unary(UnaryOp::Cosh, x.re)?;
                let cosh_sq = self.emit_binary(BinaryOp::Mul, cosh, cosh)?;
                let du = self.emit_binary(BinaryOp::Div, x.du, cosh_sq)?;
                DualReg { re, du }
            }
            UnaryOp::Exp => {
                let re = self.emit_unary(UnaryOp::Exp, x.re)?;
                let du = self.emit_binary(BinaryOp::Mul, x.du, re)?;
                DualReg { re, du }
            }
            UnaryOp::Log => self.lower_log_like(x, false)?,
            UnaryOp::Log10 => self.lower_log_like(x, true)?,
            UnaryOp::Sqrt => self.lower_sqrt(x)?,
        };
        Ok(out)
    }

    pub(super) fn unary_mul_chain(
        &mut self,
        re_op: UnaryOp,
        deriv_op: UnaryOp,
        x: DualReg,
    ) -> Result<DualReg, SolveDerivationError> {
        let re = self.emit_unary(re_op, x.re)?;
        let deriv_term = self.emit_unary(deriv_op, x.re)?;
        let du = self.emit_binary(BinaryOp::Mul, x.du, deriv_term)?;
        Ok(DualReg { re, du })
    }

    pub(super) fn lower_asin_or_acos(
        &mut self,
        x: DualReg,
        is_acos: bool,
    ) -> Result<DualReg, SolveDerivationError> {
        let re = self.emit_unary(
            if is_acos {
                UnaryOp::Acos
            } else {
                UnaryOp::Asin
            },
            x.re,
        )?;
        let one = self.one_reg()?;
        let x_sq = self.emit_binary(BinaryOp::Mul, x.re, x.re)?;
        let denom_sq = self.emit_binary(BinaryOp::Sub, one, x_sq)?;
        let denom = self.emit_unary(UnaryOp::Sqrt, denom_sq)?;
        let reciprocal = self.emit_binary(BinaryOp::Div, one, denom)?;
        let partial = if is_acos {
            self.emit_unary(UnaryOp::Neg, reciprocal)?
        } else {
            reciprocal
        };
        let du = self.scale_by_finite_partial(x.du, partial)?;
        Ok(DualReg { re, du })
    }

    pub(super) fn lower_log_like(
        &mut self,
        x: DualReg,
        is_log10: bool,
    ) -> Result<DualReg, SolveDerivationError> {
        let op = if is_log10 {
            UnaryOp::Log10
        } else {
            UnaryOp::Log
        };
        let re = self.emit_unary(op, x.re)?;
        let denom = if is_log10 {
            let ln10 = self.ln10_reg()?;
            self.emit_binary(BinaryOp::Mul, x.re, ln10)?
        } else {
            x.re
        };
        let one = self.one_reg()?;
        let partial = self.emit_binary(BinaryOp::Div, one, denom)?;
        let du = self.scale_by_finite_partial(x.du, partial)?;
        Ok(DualReg { re, du })
    }

    pub(super) fn lower_sqrt(&mut self, x: DualReg) -> Result<DualReg, SolveDerivationError> {
        let re = self.emit_unary(UnaryOp::Sqrt, x.re)?;
        let half = self.half_reg()?;
        let partial = self.emit_binary(BinaryOp::Div, half, re)?;
        let du = self.scale_by_finite_partial(x.du, partial)?;
        Ok(DualReg { re, du })
    }

    /// `tangent · partial` when the local partial is finite, and zero where it
    /// does not exist (the `crate::reverse` kink rules). `p - p` is
    /// `0` exactly for every finite `p` and NaN for an infinite or NaN one.
    pub(super) fn scale_by_finite_partial(
        &mut self,
        tangent: Reg,
        partial: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        let guarded = self.finite_or_zero(partial)?;
        self.emit_binary(BinaryOp::Mul, tangent, guarded)
    }

    pub(super) fn finite_or_zero(&mut self, value: Reg) -> Result<Reg, SolveDerivationError> {
        let zero = self.zero_reg()?;
        let difference = self.emit_binary(BinaryOp::Sub, value, value)?;
        let finite = self.emit_compare(CompareOp::Eq, difference, zero)?;
        self.emit_select(finite, value, zero)
    }

    pub(super) fn binary_dual(
        &mut self,
        op: BinaryOp,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, SolveDerivationError> {
        if op != BinaryOp::Div && self.tangent_is_zero(lhs) && self.tangent_is_zero(rhs) {
            let re = self.emit_binary(op, lhs.re, rhs.re)?;
            return Ok(DualReg { re, du: lhs.du });
        }
        let out = match op {
            BinaryOp::Add => self.binary_add(lhs, rhs)?,
            BinaryOp::Sub => self.binary_sub(lhs, rhs)?,
            BinaryOp::Mul => self.binary_mul(lhs, rhs)?,
            BinaryOp::Div => self.binary_div(lhs, rhs)?,
            BinaryOp::Pow => self.binary_pow(lhs, rhs)?,
            BinaryOp::And | BinaryOp::Or => self.binary_bool(op, lhs, rhs)?,
            BinaryOp::Atan2 => self.binary_atan2(lhs, rhs)?,
            BinaryOp::Min => self.binary_minmax(lhs, rhs, false)?,
            BinaryOp::Max => self.binary_minmax(lhs, rhs, true)?,
        };
        Ok(out)
    }

    pub(super) fn binary_add(
        &mut self,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, SolveDerivationError> {
        let re = self.emit_binary(BinaryOp::Add, lhs.re, rhs.re)?;
        let du = self.emit_binary(BinaryOp::Add, lhs.du, rhs.du)?;
        Ok(DualReg { re, du })
    }

    pub(super) fn binary_sub(
        &mut self,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, SolveDerivationError> {
        let re = self.emit_binary(BinaryOp::Sub, lhs.re, rhs.re)?;
        let du = self.emit_binary(BinaryOp::Sub, lhs.du, rhs.du)?;
        Ok(DualReg { re, du })
    }

    pub(super) fn binary_mul(
        &mut self,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, SolveDerivationError> {
        let re = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.re)?;
        let term1 = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re)?;
        let term2 = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du)?;
        let du = self.emit_binary(BinaryOp::Add, term1, term2)?;
        Ok(DualReg { re, du })
    }

    pub(super) fn binary_div(
        &mut self,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, SolveDerivationError> {
        let zero = self.zero_reg()?;
        let denom_zero = self.emit_compare(CompareOp::Eq, rhs.re, zero)?;
        let numer_zero = self.emit_compare(CompareOp::Eq, lhs.re, zero)?;

        let safe_re = self.emit_binary(BinaryOp::Div, lhs.re, rhs.re)?;
        let denom_zero_re = self.emit_select(numer_zero, zero, safe_re)?;
        let re = self.emit_select(denom_zero, denom_zero_re, safe_re)?;

        if self.tangent_is_zero(lhs) && self.tangent_is_zero(rhs) {
            return Ok(DualReg { re, du: zero });
        }

        // Each local partial when finite (the `crate::reverse`
        // kink rules): a zero, subnormal, or huge denominator zeroes the
        // partial that does not exist.
        let lhs_term = if self.tangent_is_zero(lhs) {
            None
        } else {
            let one = self.one_reg()?;
            let partial = self.emit_binary(BinaryOp::Div, one, rhs.re)?;
            Some(self.scale_by_finite_partial(lhs.du, partial)?)
        };
        let rhs_term = if self.tangent_is_zero(rhs) {
            None
        } else {
            let negated = self.emit_unary(UnaryOp::Neg, lhs.re)?;
            let rhs_sq = self.emit_binary(BinaryOp::Mul, rhs.re, rhs.re)?;
            let partial = self.emit_binary(BinaryOp::Div, negated, rhs_sq)?;
            Some(self.scale_by_finite_partial(rhs.du, partial)?)
        };
        let du = self.sum_terms(lhs_term, rhs_term)?;
        Ok(DualReg { re, du })
    }

    pub(super) fn binary_pow(
        &mut self,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, SolveDerivationError> {
        let re = self.emit_binary(BinaryOp::Pow, lhs.re, rhs.re)?;
        let du = self.lower_pow_du(lhs, rhs, re)?;
        Ok(DualReg { re, du })
    }

    /// The `pow` kink rule of `crate::reverse`: the base partial
    /// `r·l^(r-1)` when finite, the exponent partial `l^r·ln(l)` only for
    /// `l > 0` and when finite. Each partial depends on the primal operands
    /// alone, so a seeded exponent never changes the base term.
    pub(super) fn lower_pow_du(
        &mut self,
        lhs: DualReg,
        rhs: DualReg,
        re: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        let base_term = if self.tangent_is_zero(lhs) {
            None
        } else {
            let one = self.one_reg()?;
            let rhs_minus_one = self.emit_binary(BinaryOp::Sub, rhs.re, one)?;
            let x_pow_n_minus_1 = self.emit_binary(BinaryOp::Pow, lhs.re, rhs_minus_one)?;
            let partial = self.emit_binary(BinaryOp::Mul, rhs.re, x_pow_n_minus_1)?;
            Some(self.scale_by_finite_partial(lhs.du, partial)?)
        };
        let exponent_term = if self.tangent_is_zero(rhs) {
            None
        } else {
            let zero = self.zero_reg()?;
            let ln_x = self.emit_unary(UnaryOp::Log, lhs.re)?;
            let partial = self.emit_binary(BinaryOp::Mul, re, ln_x)?;
            let finite = self.finite_or_zero(partial)?;
            let lhs_positive = self.emit_compare(CompareOp::Gt, lhs.re, zero)?;
            let guarded = self.emit_select(lhs_positive, finite, zero)?;
            Some(self.emit_binary(BinaryOp::Mul, rhs.du, guarded)?)
        };
        self.sum_terms(base_term, exponent_term)
    }

    /// The sum of the operand terms of a binary tangent, omitting a term whose
    /// operand carries no tangent.
    pub(super) fn sum_terms(
        &mut self,
        lhs: Option<Reg>,
        rhs: Option<Reg>,
    ) -> Result<Reg, SolveDerivationError> {
        match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => self.emit_binary(BinaryOp::Add, lhs, rhs),
            (Some(term), None) | (None, Some(term)) => Ok(term),
            (None, None) => self.zero_reg(),
        }
    }

    pub(super) fn binary_bool(
        &mut self,
        op: BinaryOp,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, SolveDerivationError> {
        let re = self.emit_binary(op, lhs.re, rhs.re)?;
        let du = self.zero_reg()?;
        Ok(DualReg { re, du })
    }

    pub(super) fn binary_atan2(
        &mut self,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, SolveDerivationError> {
        let re = self.emit_binary(BinaryOp::Atan2, lhs.re, rhs.re)?;
        let lhs_sq = self.emit_binary(BinaryOp::Mul, lhs.re, lhs.re)?;
        let rhs_sq = self.emit_binary(BinaryOp::Mul, rhs.re, rhs.re)?;
        let denom = self.emit_binary(BinaryOp::Add, lhs_sq, rhs_sq)?;
        // Each local partial when finite (the `crate::reverse`
        // kink rules), so the origin, where both are `0 / 0`, contributes
        // nothing.
        let lhs_term = if self.tangent_is_zero(lhs) {
            None
        } else {
            let partial = self.emit_binary(BinaryOp::Div, rhs.re, denom)?;
            Some(self.scale_by_finite_partial(lhs.du, partial)?)
        };
        let rhs_term = if self.tangent_is_zero(rhs) {
            None
        } else {
            let negated = self.emit_unary(UnaryOp::Neg, lhs.re)?;
            let partial = self.emit_binary(BinaryOp::Div, negated, denom)?;
            Some(self.scale_by_finite_partial(rhs.du, partial)?)
        };
        let du = self.sum_terms(lhs_term, rhs_term)?;
        Ok(DualReg { re, du })
    }

    pub(super) fn binary_minmax(
        &mut self,
        lhs: DualReg,
        rhs: DualReg,
        is_max: bool,
    ) -> Result<DualReg, SolveDerivationError> {
        let cmp = if is_max { CompareOp::Ge } else { CompareOp::Le };
        let cond = self.emit_compare(cmp, lhs.re, rhs.re)?;
        let re = self.emit_select(cond, lhs.re, rhs.re)?;
        let du = self.emit_select(cond, lhs.du, rhs.du)?;
        Ok(DualReg { re, du })
    }
}
