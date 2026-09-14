//! Retain primal evaluation while omitting structurally inactive tangents.

use super::*;

impl AdBuilder {
    pub(super) fn tangent_is_zero(&self, value: DualReg) -> bool {
        self.cached_zero == Some(value.du)
    }

    pub(super) fn range_tangent_is_zero(
        &self,
        start: Reg,
        count: usize,
    ) -> Result<bool, LowerError> {
        for offset in 0..count {
            let register = checked_ad_reg_offset(start, offset, self.span, "AD inactive range")?;
            if !self.tangent_is_zero(self.lookup(register)?) {
                return Ok(false);
            }
        }
        Ok(true)
    }

    pub(super) fn pack_primal_range(
        &mut self,
        start: Reg,
        count: usize,
    ) -> Result<Reg, LowerError> {
        if let Some((primal, _)) = self.planar_dual_range(start, count)? {
            return Ok(primal);
        }
        let mut registers = ad_vec_with_capacity(count, "AD primal range", self.span)?;
        for offset in 0..count {
            let register = checked_ad_reg_offset(start, offset, self.span, "AD primal range")?;
            registers.push(self.lookup(register)?.re);
        }
        self.pack_registers(&registers)
    }

    pub(super) fn emit_zero_tangent_result(
        &mut self,
        source: Reg,
        count: usize,
        operation: impl FnOnce(Reg) -> LinearOp,
    ) -> Result<(), LowerError> {
        let start = self.emit_result_operation(count, operation)?;
        let du = self.zero_reg()?;
        for offset in 0..count {
            let source = checked_ad_reg_offset(source, offset, self.span, "AD inactive source")?;
            let re = checked_ad_reg_offset(start, offset, self.span, "AD inactive result")?;
            self.bind(source, DualReg { re, du })?;
        }
        Ok(())
    }

    pub(super) fn emit_result_operation(
        &mut self,
        count: usize,
        operation: impl FnOnce(Reg) -> LinearOp,
    ) -> Result<Reg, LowerError> {
        let start = self.next_reg;
        for _ in 0..count {
            self.alloc_reg()?;
        }
        self.ops.push(operation(start));
        Ok(start)
    }

    fn pack_tangent_range(&mut self, start: Reg, count: usize) -> Result<Reg, LowerError> {
        if let Some((_, tangent)) = self.planar_dual_range(start, count)? {
            return Ok(tangent);
        }
        let mut registers = ad_vec_with_capacity(count, "AD tangent range", self.span)?;
        for offset in 0..count {
            let source = checked_ad_reg_offset(start, offset, self.span, "AD tangent source")?;
            registers.push(self.lookup(source)?.du);
        }
        self.pack_registers(&registers)
    }

    pub(super) fn lower_inactive_bilinear_factors(
        &mut self,
        source: Reg,
        operands: [(Reg, usize); 2],
        count: usize,
        operation: impl Fn(Reg, Reg, Reg) -> LinearOp,
    ) -> Result<bool, LowerError> {
        let [(lhs, lhs_count), (rhs, rhs_count)] = operands;
        let lhs_zero = self.range_tangent_is_zero(lhs, lhs_count)?;
        let rhs_zero = self.range_tangent_is_zero(rhs, rhs_count)?;
        if !lhs_zero && !rhs_zero {
            return Ok(false);
        }
        let lhs_re = self.pack_primal_range(lhs, lhs_count)?;
        let rhs_re = self.pack_primal_range(rhs, rhs_count)?;
        if lhs_zero && rhs_zero {
            self.emit_zero_tangent_result(source, count, |dst| operation(dst, lhs_re, rhs_re))?;
            return Ok(true);
        }
        let re_start = self.emit_result_operation(count, |dst| operation(dst, lhs_re, rhs_re))?;
        let lhs_du = if lhs_zero {
            lhs_re
        } else {
            self.pack_tangent_range(lhs, lhs_count)?
        };
        let rhs_du = if rhs_zero {
            rhs_re
        } else {
            self.pack_tangent_range(rhs, rhs_count)?
        };
        let du_start = self.emit_result_operation(count, |dst| operation(dst, lhs_du, rhs_du))?;
        for offset in 0..count {
            let source = checked_ad_reg_offset(source, offset, self.span, "bilinear AD source")?;
            let re = checked_ad_reg_offset(re_start, offset, self.span, "bilinear AD primal")?;
            let du = checked_ad_reg_offset(du_start, offset, self.span, "bilinear AD tangent")?;
            self.bind(source, DualReg { re, du })?;
        }
        Ok(true)
    }

    pub(super) fn lower_zero_tangent_call(
        &mut self,
        dst_start: Reg,
        input_starts: &[Reg],
        site: &rumoca_ir_solve::SolvePureCallSite,
    ) -> Result<bool, LowerError> {
        for (&start, value_type) in input_starts.iter().zip(site.inputs()) {
            if !self.range_tangent_is_zero(start, value_type.scalar_count() as usize)? {
                return Ok(false);
            }
        }
        let mut inputs = Vec::with_capacity(input_starts.len());
        for (&start, value_type) in input_starts.iter().zip(site.inputs()) {
            inputs.push(self.pack_primal_range(start, value_type.scalar_count() as usize)?);
        }
        let count = site
            .output_scalar_count()
            .ok_or_else(|| unsupported("inactive pure-call output width overflows"))?;
        self.emit_zero_tangent_result(dst_start, count, |dst_start| LinearOp::PureCall {
            dst_start,
            input_starts: inputs.into_boxed_slice(),
            site: site.clone(),
        })?;
        Ok(true)
    }
}
