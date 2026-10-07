//! Scalar-register ABI conversion surrounds the unchanged checked typed helpers.
use super::*;
impl BodyEmitter<'_> {
    pub(in crate::emit) fn emit_native_pure_call(
        &mut self,
        op: LinearOp,
        prefix: Option<&[LinearOp]>,
    ) -> Result<(), String> {
        let memo = self
            .calls
            .and_then(|plan| prefix.and_then(|prefix| super::memo::find(&plan.memos, prefix, &op)));
        let LinearOp::PureCall {
            dst_start,
            input_starts,
            site,
        } = op
        else {
            return Err("expected a primal issued call".into());
        };
        let plan = self
            .calls
            .ok_or("native pure call requires its model table")?;
        let call = plan.call(&site)?;
        if let Some(memo) = memo {
            self.call_address(memo.flag);
            self.push(Instruction::I32Load(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            self.push(Instruction::I32Eqz);
            self.push(Instruction::If(BlockType::Empty));
        }
        let mut offset = plan.input;
        let mut flat = 0u32;
        for (&start, value) in input_starts.iter().zip(site.inputs()) {
            self.pack_input(start, offset, value, flat)?;
            flat = flat
                .checked_add(value.scalar_count())
                .ok_or("native typed input overflow")?;
            offset = offset
                .checked_add(
                    value
                        .scalar_count()
                        .checked_mul(8)
                        .ok_or("native typed input overflow")?,
                )
                .ok_or("native typed input overflow")?;
        }
        for offset in [plan.input, plan.output, plan.scratch] {
            self.call_address(offset);
        }
        self.push(Instruction::Call(call.function));
        self.push(Instruction::LocalTee(plan.status));
        self.push(Instruction::If(BlockType::Empty));
        self.push(Instruction::LocalGet(plan.status));
        self.push(Instruction::Return);
        self.push(Instruction::End);
        if let Some(memo) = memo {
            self.call_address(memo.tuple);
            self.call_address(plan.output);
            self.push(Instruction::I32Const(memo.bytes as i32));
            self.push(Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
            self.call_address(memo.flag);
            self.push(Instruction::I32Const(1));
            self.push(Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
            self.push(Instruction::End);
            self.call_address(plan.output);
            self.call_address(memo.tuple);
            self.push(Instruction::I32Const(memo.bytes as i32));
            self.push(Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
        }
        let mut register = dst_start;
        let mut offset = plan.output;
        let mut flat = 0u32;
        for output in site.outputs() {
            let value = output.value_type();
            self.unpack_output(register, offset, value, flat)?;
            flat = flat
                .checked_add(value.scalar_count())
                .ok_or("native typed output overflow")?;
            register = register
                .checked_add(value.scalar_count())
                .ok_or("native typed output overflow")?;
            offset = offset
                .checked_add(
                    value
                        .scalar_count()
                        .checked_mul(8)
                        .ok_or("native typed output overflow")?,
                )
                .ok_or("native typed output overflow")?;
        }
        Ok(())
    }

    pub(super) fn call_address(&mut self, offset: u32) {
        self.push(Instruction::LocalGet(SEED_PTR_PARAM));
        self.push(Instruction::I32Const(offset as i32));
        self.push(Instruction::I32Add);
    }

    fn call_cell_address(&mut self, offset: u32, counter: u32) {
        self.call_address(offset);
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
    }

    /// Pack one call input value. An Integer cell the stage binds to a typed
    /// input lane (SOLVE-C69) copies that `i64` exactly; every other cell is
    /// converted from its Real register.
    fn pack_input(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
        flat: u32,
    ) -> Result<(), String> {
        let counter = self.arena.ok_or("missing native register arena")?.counter;
        let count = value.scalar_count();
        let lanes = (0..count)
            .map(|cell| self.argument_lane(flat + cell))
            .collect::<Vec<_>>();
        if lanes.iter().all(Option::is_none) {
            return self.cells(counter, count as usize, |this| {
                this.pack_cell(start, offset, value, counter)
            });
        }
        for (cell, lane) in (0..count).zip(lanes) {
            match lane {
                Some(lane) => {
                    let cell = cell
                        .checked_mul(8)
                        .and_then(|bytes| offset.checked_add(bytes))
                        .ok_or("native typed input overflow")?;
                    let base = self
                        .calls
                        .ok_or("missing native call layout")?
                        .typed_lanes_cell;
                    self.call_address(cell);
                    self.call_address(base);
                    self.push(Instruction::I32Load(MemArg {
                        offset: 0,
                        align: 2,
                        memory_index: 0,
                    }));
                    self.push(Instruction::I64Load(MemArg {
                        offset: lane as u64,
                        align: 3,
                        memory_index: 0,
                    }));
                    self.push(Instruction::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                None => {
                    self.push(Instruction::I32Const(cell as i32));
                    self.push(Instruction::LocalSet(counter));
                    self.pack_cell(start, offset, value, counter)?;
                }
            }
        }
        Ok(())
    }

    /// The typed input lane the issued stage binds an Integer argument cell
    /// of the top-level call being emitted to.
    fn argument_lane(&self, cell: u32) -> Option<usize> {
        self.region_path.is_empty().then_some(())?;
        self.native_stage?
            .integer_argument_lane(self.operation_ordinal, cell)
    }

    /// Whether the Real view of an Integer result cell of the call being
    /// emitted is read, so it must be exact. Only a top-level call of an
    /// issued stage carries the proof that a view is unread.
    fn result_checked(&self, cell: u32) -> bool {
        !self.region_path.is_empty()
            || self
                .native_stage
                .is_none_or(|stage| stage.integer_result_checked(self.operation_ordinal, cell))
    }

    fn pack_cell(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
        counter: u32,
    ) -> Result<(), String> {
        self.call_cell_address(offset, counter);
        self.push_arena_address(start, Some(counter), 1)?;
        self.push(Instruction::F64Load(arena::memarg()));
        match value.element_type() {
            solve::SolveScalarType::Real {
                format: solve::SolveRealFormat::Binary64,
                ..
            } => {
                self.push(Instruction::F64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            solve::SolveScalarType::Boolean => {
                self.push(Instruction::F64Const(0.0.into()));
                self.push(Instruction::F64Ne);
                self.push(Instruction::I64ExtendI32U);
                self.push(Instruction::I64Store(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            solve::SolveScalarType::Integer(domain) => self.pack_integer(domain),
            _ => return Err("native scalar call requires Binary64".into()),
        }
        Ok(())
    }

    fn pack_integer(&mut self, domain: solve::SolveIntegerDomain) {
        let integer = self.calls.expect("checked call layout").integer;
        self.push(Instruction::LocalTee(LOCAL_BASE));
        self.push(Instruction::I64TruncSatF64S);
        self.push(Instruction::LocalTee(integer));
        self.push(Instruction::F64ConvertI64S);
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::F64Ne);
        self.return_status_if(2);
        for (limit, compare) in [
            (domain.minimum(), Instruction::I64LtS),
            (domain.maximum(), Instruction::I64GtS),
        ] {
            self.push(Instruction::LocalGet(integer));
            self.push(Instruction::I64Const(limit));
            self.push(compare);
            self.return_status_if(2);
        }
        self.push(Instruction::LocalGet(integer));
        self.push(Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
    }

    fn unpack_output(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
        flat: u32,
    ) -> Result<(), String> {
        let counter = self.arena.ok_or("missing native register arena")?.counter;
        let count = value.scalar_count();
        let checked = (0..count)
            .map(|cell| self.result_checked(flat + cell))
            .collect::<Vec<_>>();
        if checked.windows(2).all(|pair| pair[0] == pair[1]) {
            let checked = checked.first().copied().unwrap_or(true);
            return self.cells(counter, count as usize, |this| {
                this.unpack_cell(start, offset, value, counter, checked)
            });
        }
        for (cell, checked) in (0..count).zip(checked) {
            self.push(Instruction::I32Const(cell as i32));
            self.push(Instruction::LocalSet(counter));
            self.unpack_cell(start, offset, value, counter, checked)?;
        }
        Ok(())
    }

    fn unpack_cell(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
        counter: u32,
        checked: bool,
    ) -> Result<(), String> {
        self.push_arena_address(start, Some(counter), 1)?;
        self.call_cell_address(offset, counter);
        match value.element_type() {
            solve::SolveScalarType::Real {
                format: solve::SolveRealFormat::Binary64,
                ..
            } => {
                self.push(Instruction::F64Load(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            solve::SolveScalarType::Integer(_) => {
                self.push(Instruction::I64Load(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                // A read Real view of an Integer result must be exact (SOLVE-C69).
                if checked {
                    let integer = self.calls.ok_or("missing native call layout")?.integer;
                    self.push(Instruction::LocalTee(integer));
                    self.require_exact_binary64();
                    self.push(Instruction::LocalGet(integer));
                }
                self.push(Instruction::F64ConvertI64S);
            }
            solve::SolveScalarType::Boolean => {
                self.push(Instruction::I64Load(MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
                self.push(Instruction::F64ConvertI64U);
            }
            _ => return Err("native scalar call requires Binary64".into()),
        }
        self.push(Instruction::F64Store(arena::memarg()));
        Ok(())
    }
}

impl BodyEmitter<'_> {
    /// Write one typed input lane's Real view into its slot of the private P
    /// copy (SOLVE-C69):
    /// an Integer by IntegerToReal, checked to be exact when the view has an
    /// unbound reader; a Boolean byte must be 0 or 1. Both refuse with
    /// status 2 instead of rounding or reinterpreting.
    pub(in crate::emit) fn write_input_view(
        &mut self,
        input: &solve::NativeInputLane,
    ) -> Result<(), String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        let view = u32::try_from(input.p_index())
            .ok()
            .and_then(|n| n.checked_mul(8))
            .ok_or("native input view overflow")?;
        let lane = MemArg {
            offset: input.lane_offset() as u64,
            align: 0,
            memory_index: 0,
        };
        match input.lane() {
            solve::NativeOutputLane::Integer => {
                self.push(Instruction::LocalGet(OUT_PTR_PARAM));
                self.push(Instruction::I64Load(MemArg { align: 3, ..lane }));
                self.push(Instruction::LocalSet(plan.integer));
                if input.checked() {
                    self.push(Instruction::LocalGet(plan.integer));
                    self.require_exact_binary64();
                }
                self.push_p_address(view);
                self.push(Instruction::LocalGet(plan.integer));
                self.push(Instruction::F64ConvertI64S);
            }
            solve::NativeOutputLane::Boolean => {
                self.push(Instruction::LocalGet(OUT_PTR_PARAM));
                self.push(Instruction::I32Load8U(lane));
                self.push(Instruction::LocalTee(plan.status));
                self.push(Instruction::I32Const(1));
                self.push(Instruction::I32GtU);
                self.return_status_if(2);
                self.push_p_address(view);
                self.push(Instruction::LocalGet(plan.status));
                self.push(Instruction::F64ConvertI32U);
            }
            solve::NativeOutputLane::Real => {
                return Err("a Real input has no typed input lane".into());
            }
        }
        self.push(Instruction::F64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        Ok(())
    }

    fn push_p_address(&mut self, offset: u32) {
        self.push(Instruction::LocalGet(P_PTR_PARAM));
        self.push(Instruction::I32Const(offset as i32));
        self.push(Instruction::I32Add);
    }

    /// Consume the i64 on the stack; return status 2 unless its magnitude is
    /// at most 2^53, so its Binary64 conversion is exact.
    fn require_exact_binary64(&mut self) {
        self.push(Instruction::I64Const(1 << 53));
        self.push(Instruction::I64Add);
        self.push(Instruction::I64Const(1 << 54));
        self.push(Instruction::I64GtU);
        self.return_status_if(2);
    }
}
