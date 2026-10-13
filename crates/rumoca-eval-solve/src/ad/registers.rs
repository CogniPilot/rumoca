use super::*;

impl<'a> AdBuilder<'a> {
    pub(super) fn bind(&mut self, src: Reg, dual: DualReg) -> Result<(), SolveDerivationError> {
        if !self.map.bind(src, dual) {
            return Err(unsupported("duplicate destination register in primal row"));
        }
        Ok(())
    }

    pub(super) fn lookup(&self, reg: Reg) -> Result<DualReg, SolveDerivationError> {
        self.map
            .get(reg)
            .ok_or_else(|| unsupported("missing source register in primal row"))
    }

    pub(super) fn alloc_reg(&mut self) -> Result<Reg, SolveDerivationError> {
        let reg = self.next_reg;
        self.next_reg = self.next_reg.checked_add(1).ok_or_else(|| {
            ad_optional_contract_violation(
                format!("AD register allocation overflow after r{reg}"),
                self.span,
            )
        })?;
        Ok(reg)
    }

    pub(super) fn emit_const(&mut self, value: f64) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::Const { dst, value });
        Ok(dst)
    }

    pub(super) fn pack_registers(&mut self, regs: &[Reg]) -> Result<Reg, SolveDerivationError> {
        if let Some(&start) = regs.first()
            && regs.iter().copied().enumerate().all(|(offset, register)| {
                u32::try_from(offset)
                    .ok()
                    .and_then(|offset| start.checked_add(offset))
                    == Some(register)
            })
        {
            return Ok(start);
        }
        if let Some(start) = self.pack_compact_registers(regs)? {
            return Ok(start);
        }
        let start = self.next_reg;
        for &src in regs {
            let dst = self.alloc_reg()?;
            self.ops.push(LinearOp::Move { dst, src });
        }
        Ok(start)
    }

    pub(super) fn pack_dual_register_range(
        &mut self,
        primal_start: Reg,
        count: usize,
    ) -> Result<Reg, SolveDerivationError> {
        if count == 0 {
            return Ok(self.next_reg);
        }
        if let Some(start) = self.interleaved_dual_range(primal_start, count)? {
            return Ok(start);
        }
        let capacity = checked_ad_product(count, 2, self.span, "dual register range")?;
        let mut registers = ad_vec_with_capacity(capacity, "dual register range", self.span)?;
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(primal_start, offset, self.span, "dual register range")?;
            let value = self.lookup(primal)?;
            registers.extend([value.re, value.du]);
        }
        self.pack_registers(&registers)
    }

    pub(super) fn emit_load_time(&mut self) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadTime { dst });
        Ok(dst)
    }

    pub(super) fn emit_load_y(&mut self, index: usize) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadY { dst, index });
        Ok(dst)
    }

    pub(super) fn emit_load_p(&mut self, index: usize) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadP { dst, index });
        Ok(dst)
    }

    pub(super) fn emit_load_seed(&mut self, index: usize) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadSeed { dst, index });
        Ok(dst)
    }

    pub(super) fn emit_load_indexed_p(
        &mut self,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedP {
            dst,
            base,
            count,
            index,
        });
        Ok(dst)
    }

    pub(super) fn emit_load_indexed_seed(
        &mut self,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedSeed {
            dst,
            base,
            count,
            index,
        });
        Ok(dst)
    }

    pub(super) fn p_seed_index(&self, index: usize) -> Result<usize, SolveDerivationError> {
        match self.seed_mode {
            SeedMode::SolverYOnly | SeedMode::SolverYSubset { .. } => Ok(index),
            SeedMode::SolverYAndP { p_seed_offset } => {
                p_seed_offset.checked_add(index).ok_or_else(|| {
                    ad_optional_contract_violation(
                        format!(
                        "parameter seed index offset {p_seed_offset} plus index {index} overflows"
                    ),
                        self.span,
                    )
                })
            }
        }
    }

    pub(super) fn emit_table_bounds(
        &mut self,
        table_id: Reg,
        max: bool,
    ) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::TableBounds { dst, table_id, max });
        Ok(dst)
    }

    pub(super) fn emit_table_lookup(
        &mut self,
        table_id: Reg,
        column: Reg,
        input: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        });
        Ok(dst)
    }

    pub(super) fn emit_table_lookup_slope(
        &mut self,
        table_id: Reg,
        column: Reg,
        input: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        });
        Ok(dst)
    }

    pub(super) fn emit_table_next_event(
        &mut self,
        table_id: Reg,
        time: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        });
        Ok(dst)
    }

    pub(super) fn emit_unary(
        &mut self,
        op: UnaryOp,
        arg: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        if let Some(&value) = self.unary_values.get(&(op, arg)) {
            return Ok(value);
        }
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::Unary { dst, op, arg });
        self.unary_values.insert((op, arg), dst);
        Ok(dst)
    }

    pub(super) fn emit_binary(
        &mut self,
        op: BinaryOp,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::Binary { dst, op, lhs, rhs });
        Ok(dst)
    }

    pub(super) fn emit_compare(
        &mut self,
        op: CompareOp,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::Compare { dst, op, lhs, rhs });
        Ok(dst)
    }

    pub(super) fn emit_select(
        &mut self,
        cond: Reg,
        if_true: Reg,
        if_false: Reg,
    ) -> Result<Reg, SolveDerivationError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        });
        Ok(dst)
    }

    pub(super) fn zero_reg(&mut self) -> Result<Reg, SolveDerivationError> {
        if let Some(reg) = self.cached_zero {
            return Ok(reg);
        }
        let reg = self.emit_const(0.0)?;
        self.cached_zero = Some(reg);
        Ok(reg)
    }

    pub(super) fn one_reg(&mut self) -> Result<Reg, SolveDerivationError> {
        if let Some(reg) = self.cached_one {
            return Ok(reg);
        }
        let reg = self.emit_const(1.0)?;
        self.cached_one = Some(reg);
        Ok(reg)
    }

    pub(super) fn ln10_reg(&mut self) -> Result<Reg, SolveDerivationError> {
        if let Some(reg) = self.cached_ln10 {
            return Ok(reg);
        }
        let reg = self.emit_const(std::f64::consts::LN_10)?;
        self.cached_ln10 = Some(reg);
        Ok(reg)
    }

    pub(super) fn half_reg(&mut self) -> Result<Reg, SolveDerivationError> {
        if let Some(reg) = self.cached_half {
            return Ok(reg);
        }
        let reg = self.emit_const(0.5)?;
        self.cached_half = Some(reg);
        Ok(reg)
    }
}
