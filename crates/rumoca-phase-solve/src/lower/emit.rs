use rumoca_ir_solve::{
    BinaryOp, CompareOp, ExternalFunctionKind, LinearOp, RandomGenerator, Reg, ScalarSlot, UnaryOp,
};

use super::cse::{SlotLoadKey, canonical_binary_key};
use super::{LowerBuilder, LowerError};

impl LowerBuilder<'_> {
    pub(super) fn alloc_reg(&mut self) -> Reg {
        let reg = self.next_reg;
        self.next_reg = self.next_reg.saturating_add(1);
        reg
    }

    pub(super) fn alloc_call_site(&mut self) -> u64 {
        let local = self.next_call_site;
        self.next_call_site = self.next_call_site.saturating_add(1);
        (self.call_site_namespace << 32) | local
    }

    pub(super) fn emit_const(&mut self, value: f64) -> Reg {
        let key = SlotLoadKey::Constant(value.to_bits());
        if let Some(reg) = self.cse.slots.get(&key).copied() {
            return reg;
        }
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Const { dst, value });
        self.cse.slots.insert(key, dst);
        dst
    }

    pub(super) fn emit_load_time(&mut self) -> Reg {
        if let Some(reg) = self.cse.slots.get(&SlotLoadKey::Time).copied() {
            return reg;
        }
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::LoadTime { dst });
        self.cse.slots.insert(SlotLoadKey::Time, dst);
        dst
    }

    pub(super) fn emit_unary(&mut self, op: UnaryOp, arg: Reg) -> Reg {
        if let Some(reg) = self.cse.unary.get(&(op, arg)).copied() {
            return reg;
        }
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Unary { dst, op, arg });
        self.cse.unary.insert((op, arg), dst);
        dst
    }

    pub(super) fn emit_binary(&mut self, op: BinaryOp, lhs: Reg, rhs: Reg) -> Reg {
        let key = canonical_binary_key(op, lhs, rhs);
        if let Some(reg) = self.cse.binary.get(&key).copied() {
            return reg;
        }
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Binary { dst, op, lhs, rhs });
        self.cse.binary.insert(key, dst);
        dst
    }

    pub(super) fn emit_compare(&mut self, op: CompareOp, lhs: Reg, rhs: Reg) -> Reg {
        let key = (op, lhs, rhs);
        if let Some(reg) = self.cse.compare.get(&key).copied() {
            return reg;
        }
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Compare { dst, op, lhs, rhs });
        self.cse.compare.insert(key, dst);
        dst
    }

    pub(super) fn emit_select(&mut self, cond: Reg, if_true: Reg, if_false: Reg) -> Reg {
        let key = (cond, if_true, if_false);
        if let Some(reg) = self.cse.select.get(&key).copied() {
            return reg;
        }
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        });
        self.cse.select.insert(key, dst);
        dst
    }

    pub(super) fn emit_slot_load(&mut self, slot: ScalarSlot) -> Result<Reg, LowerError> {
        let key = match slot {
            ScalarSlot::Time => SlotLoadKey::Time,
            ScalarSlot::Y { index, .. } => SlotLoadKey::Y(index),
            ScalarSlot::P { index, .. } => SlotLoadKey::P(index),
            ScalarSlot::Constant(value) => SlotLoadKey::Constant(value.to_bits()),
        };
        if let Some(reg) = self.cse.slots.get(&key).copied() {
            return Ok(reg);
        }
        let dst = self.alloc_reg();
        match slot {
            ScalarSlot::Time => self.ops.push(LinearOp::LoadTime { dst }),
            ScalarSlot::Y { index, .. } => self.ops.push(LinearOp::LoadY { dst, index }),
            ScalarSlot::P { index, .. } => self.ops.push(LinearOp::LoadP { dst, index }),
            ScalarSlot::Constant(value) => self.ops.push(LinearOp::Const { dst, value }),
        }
        self.cse.slots.insert(key, dst);
        Ok(dst)
    }

    pub(super) fn emit_table_bounds(&mut self, table_id: Reg, max: bool) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableBounds { dst, table_id, max });
        dst
    }

    pub(super) fn emit_table_lookup(&mut self, table_id: Reg, column: Reg, input: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        });
        dst
    }

    pub(super) fn emit_table_next_event(&mut self, table_id: Reg, time: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        });
        dst
    }

    pub(super) fn emit_external_call(
        &mut self,
        function: ExternalFunctionKind,
        args: &[Reg],
    ) -> Result<Reg, LowerError> {
        self.emit_external_call_output(function, args, 0)
    }

    pub(super) fn emit_external_call_output(
        &mut self,
        function: ExternalFunctionKind,
        args: &[Reg],
        output_index: usize,
    ) -> Result<Reg, LowerError> {
        if args.len() > 8 {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "external function {function:?} has {} scalar arguments; max supported is 8",
                    args.len()
                ),
            });
        }
        let mut packed = [0; 8];
        for (idx, arg) in args.iter().copied().enumerate() {
            packed[idx] = arg;
        }
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::ExternalCall {
            dst,
            function,
            args: packed,
            arg_count: args.len(),
            output_index,
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
    ) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::RandomInitialState {
            dst,
            generator,
            local_seed,
            global_seed,
            state_len,
            state_index,
        });
        dst
    }

    pub(super) fn emit_random_result(
        &mut self,
        generator: RandomGenerator,
        state_values: &[Reg],
    ) -> Reg {
        let state_start = self.pack_registers(state_values);
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::RandomResult {
            dst,
            generator,
            state_start,
            state_len: state_values.len(),
        });
        dst
    }

    pub(super) fn emit_random_state(
        &mut self,
        generator: RandomGenerator,
        state_values: &[Reg],
        state_index: usize,
    ) -> Reg {
        let state_start = self.pack_registers(state_values);
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::RandomState {
            dst,
            generator,
            state_start,
            state_len: state_values.len(),
            state_index,
        });
        dst
    }

    pub(super) fn emit_impure_random_init(&mut self, seed: Reg) -> Reg {
        let dst = self.alloc_reg();
        self.ops.push(LinearOp::ImpureRandomInit { dst, seed });
        dst
    }

    pub(super) fn emit_impure_random(&mut self, id: Reg) -> Reg {
        let dst = self.alloc_reg();
        let call_site = self.alloc_call_site();
        self.ops.push(LinearOp::ImpureRandom { dst, id, call_site });
        dst
    }

    pub(super) fn emit_impure_random_integer(&mut self, id: Reg, imin: Reg, imax: Reg) -> Reg {
        let dst = self.alloc_reg();
        let call_site = self.alloc_call_site();
        self.ops.push(LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            call_site,
        });
        dst
    }

    pub(super) fn pack_registers(&mut self, regs: &[Reg]) -> Reg {
        let start = self.next_reg;
        for &src in regs {
            let dst = self.alloc_reg();
            self.ops.push(LinearOp::Move { dst, src });
        }
        start
    }
}
