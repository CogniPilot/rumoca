use rumoca_ir_solve::{BinaryOp, CompareOp, LinearOp, RandomGenerator, Reg, ScalarSlot, UnaryOp};

use super::cse::{SlotLoadKey, canonical_binary_key};
use super::{LowerBuilder, LowerError};

impl LowerBuilder<'_> {
    pub(super) fn try_alloc_reg(&mut self, span: rumoca_core::Span) -> Result<Reg, LowerError> {
        self.ensure_reg_capacity(1, span)?;
        let reg = self.next_reg;
        self.next_reg += 1;
        Ok(reg)
    }

    pub(super) fn ensure_reg_capacity(
        &self,
        count: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let count = Reg::try_from(count).map_err(|_| allocation_error(self.next_reg, span))?;
        self.next_reg
            .checked_add(count)
            .ok_or_else(|| allocation_error(self.next_reg, span))?;
        Ok(())
    }

    pub(super) fn alloc_call_site(&mut self) -> Result<u64, LowerError> {
        let local = self.next_call_site;
        if local > u64::from(u32::MAX) {
            return Err(call_site_error(format!(
                "call-site local index {local} exceeds 32-bit namespace"
            )));
        }
        self.next_call_site = self
            .next_call_site
            .checked_add(1)
            .ok_or_else(|| call_site_error("call-site counter overflow".to_string()))?;
        let namespace = self
            .call_site_namespace
            .checked_mul(1u64 << 32)
            .ok_or_else(|| call_site_error("call-site namespace overflow".to_string()))?;
        namespace
            .checked_add(local)
            .ok_or_else(|| call_site_error("call-site id overflow".to_string()))
    }

    pub(super) fn emit_const_at(
        &mut self,
        value: f64,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        if !self.dedup_access_ops {
            let dst = self.try_alloc_reg(span)?;
            self.ops.push(LinearOp::Const { dst, value });
            return Ok(dst);
        }
        let key = SlotLoadKey::Constant(value.to_bits());
        if let Some(reg) = self.cse.slots.get(&key).copied() {
            return Ok(reg);
        }
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::Const { dst, value });
        self.cse.slots.insert(key, dst);
        Ok(dst)
    }

    pub(super) fn emit_load_time_at(&mut self, span: rumoca_core::Span) -> Result<Reg, LowerError> {
        if !self.dedup_access_ops {
            let dst = self.try_alloc_reg(span)?;
            self.ops.push(LinearOp::LoadTime { dst });
            return Ok(dst);
        }
        if let Some(reg) = self.cse.slots.get(&SlotLoadKey::Time).copied() {
            return Ok(reg);
        }
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::LoadTime { dst });
        self.cse.slots.insert(SlotLoadKey::Time, dst);
        Ok(dst)
    }

    pub(super) fn emit_unary_at(
        &mut self,
        op: UnaryOp,
        arg: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        if let Some(reg) = self.cse.unary.get(&(op, arg)).copied() {
            return Ok(reg);
        }
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::Unary { dst, op, arg });
        self.cse.unary.insert((op, arg), dst);
        Ok(dst)
    }

    pub(super) fn emit_binary_at(
        &mut self,
        op: BinaryOp,
        lhs: Reg,
        rhs: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let key = canonical_binary_key(op, lhs, rhs);
        if let Some(reg) = self.cse.binary.get(&key).copied() {
            return Ok(reg);
        }
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::Binary { dst, op, lhs, rhs });
        self.cse.binary.insert(key, dst);
        Ok(dst)
    }

    pub(super) fn emit_compare_at(
        &mut self,
        op: CompareOp,
        lhs: Reg,
        rhs: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let key = (op, lhs, rhs);
        if let Some(reg) = self.cse.compare.get(&key).copied() {
            return Ok(reg);
        }
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::Compare { dst, op, lhs, rhs });
        self.cse.compare.insert(key, dst);
        Ok(dst)
    }

    pub(super) fn emit_select_at(
        &mut self,
        cond: Reg,
        if_true: Reg,
        if_false: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let key = (cond, if_true, if_false);
        if let Some(reg) = self.cse.select.get(&key).copied() {
            return Ok(reg);
        }
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        });
        self.cse.select.insert(key, dst);
        Ok(dst)
    }

    /// Emit a runtime-indexed parameter load over the contiguous slot run
    /// `base..base+count`, addressed by the 0-based flat-offset register
    /// `index`. Not CSE'd: the offset register is runtime-variable.
    pub(super) fn emit_load_indexed_p(
        &mut self,
        base: usize,
        count: usize,
        index: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::LoadIndexedP {
            dst,
            base,
            count,
            index,
        });
        Ok(dst)
    }

    pub(super) fn emit_slot_load(
        &mut self,
        slot: ScalarSlot,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let key = match slot {
            ScalarSlot::Time => SlotLoadKey::Time,
            ScalarSlot::Y { index, .. } => SlotLoadKey::Y(index),
            ScalarSlot::P { index, .. } => SlotLoadKey::P(index),
            ScalarSlot::Constant(value) => SlotLoadKey::Constant(value.to_bits()),
        };
        if !self.dedup_access_ops {
            let dst = self.try_alloc_reg(span)?;
            match slot {
                ScalarSlot::Time => self.ops.push(LinearOp::LoadTime { dst }),
                ScalarSlot::Y { index, .. } => self.ops.push(LinearOp::LoadY { dst, index }),
                ScalarSlot::P { index, .. } => {
                    self.ops.push(LinearOp::LoadP { dst, index });
                    self.param_slot_regs.insert(dst, index);
                }
                ScalarSlot::Constant(value) => self.ops.push(LinearOp::Const { dst, value }),
            }
            return Ok(dst);
        }
        if let Some(reg) = self.cse.slots.get(&key).copied() {
            if let ScalarSlot::P { index, .. } = slot {
                self.param_slot_regs.insert(reg, index);
            }
            return Ok(reg);
        }
        let dst = self.try_alloc_reg(span)?;
        match slot {
            ScalarSlot::Time => self.ops.push(LinearOp::LoadTime { dst }),
            ScalarSlot::Y { index, .. } => self.ops.push(LinearOp::LoadY { dst, index }),
            ScalarSlot::P { index, .. } => {
                self.ops.push(LinearOp::LoadP { dst, index });
                self.param_slot_regs.insert(dst, index);
            }
            ScalarSlot::Constant(value) => self.ops.push(LinearOp::Const { dst, value }),
        }
        self.cse.slots.insert(key, dst);
        Ok(dst)
    }

    pub(super) fn emit_table_bounds(
        &mut self,
        table_id: Reg,
        max: bool,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::TableBounds { dst, table_id, max });
        Ok(dst)
    }

    pub(super) fn emit_table_lookup(
        &mut self,
        table_id: Reg,
        column: Reg,
        input: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::TableLookup {
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
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        });
        Ok(dst)
    }

    pub(super) fn emit_random_initial_state(
        &mut self,
        generator: RandomGenerator,
        local_seed: Reg,
        global_seed: Reg,
        state_len: usize,
        state_index: usize,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::RandomInitialState {
            dst,
            generator,
            local_seed,
            global_seed,
            state_len,
            state_index,
        });
        Ok(dst)
    }

    pub(super) fn emit_random_result(
        &mut self,
        generator: RandomGenerator,
        state_values: &[Reg],
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        self.ensure_reg_capacity(checked_allocation_count(state_values.len(), 1, span)?, span)?;
        let state_start = self.try_pack_registers(state_values, span)?;
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::RandomResult {
            dst,
            generator,
            state_start,
            state_len: state_values.len(),
        });
        Ok(dst)
    }

    pub(super) fn emit_random_state(
        &mut self,
        generator: RandomGenerator,
        state_values: &[Reg],
        state_index: usize,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        self.ensure_reg_capacity(checked_allocation_count(state_values.len(), 1, span)?, span)?;
        let state_start = self.try_pack_registers(state_values, span)?;
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::RandomState {
            dst,
            generator,
            state_start,
            state_len: state_values.len(),
            state_index,
        });
        Ok(dst)
    }

    pub(super) fn emit_impure_random_init(
        &mut self,
        seed: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let dst = self.try_alloc_reg(span)?;
        self.ops.push(LinearOp::ImpureRandomInit { dst, seed });
        Ok(dst)
    }

    pub(super) fn emit_impure_random(
        &mut self,
        id: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let dst = self.try_alloc_reg(span)?;
        let call_site = self.alloc_call_site()?;
        self.ops.push(LinearOp::ImpureRandom { dst, id, call_site });
        Ok(dst)
    }

    pub(super) fn emit_impure_random_integer(
        &mut self,
        id: Reg,
        imin: Reg,
        imax: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let dst = self.try_alloc_reg(span)?;
        let call_site = self.alloc_call_site()?;
        self.ops.push(LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            call_site,
        });
        Ok(dst)
    }

    pub(super) fn try_pack_registers(
        &mut self,
        regs: &[Reg],
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        self.ensure_reg_capacity(regs.len(), span)?;
        let start = self.next_reg;
        for &src in regs {
            let dst = self.next_reg;
            self.next_reg += 1;
            self.ops.push(LinearOp::Move { dst, src });
        }
        Ok(start)
    }
}

fn allocation_error(reg: Reg, span: rumoca_core::Span) -> LowerError {
    LowerError::contract_violation(
        format!("Solve register allocation overflow after r{reg}"),
        span,
    )
}

fn checked_allocation_count(
    base: usize,
    extra: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    base.checked_add(extra)
        .ok_or_else(|| allocation_error(Reg::MAX, span))
}

fn call_site_error(reason: String) -> LowerError {
    LowerError::UnspannedContractViolation { reason }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unspanned_emit_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    #[test]
    fn allocation_error_does_not_fabricate_dummy_span() {
        let err = allocation_error(Reg::MAX, unspanned_emit_test_span());

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("Solve register allocation overflow after")
        );
    }

    #[test]
    fn call_site_error_is_unspanned() {
        let err = call_site_error("call-site counter overflow".to_string());

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: call-site counter overflow"
        );
    }
}
