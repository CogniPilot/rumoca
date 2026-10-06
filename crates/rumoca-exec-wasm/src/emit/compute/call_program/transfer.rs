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
        for (&start, value) in input_starts.iter().zip(site.inputs()) {
            self.pack_input(start, offset, value)?;
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
        for output in site.outputs() {
            let value = output.value_type();
            self.unpack_output(register, offset, value)?;
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

    fn pack_input(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
    ) -> Result<(), String> {
        let counter = self.arena.ok_or("missing native register arena")?.counter;
        self.cells(counter, value.scalar_count() as usize, |this| {
            this.pack_cell(start, offset, value, counter)
        })
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
    ) -> Result<(), String> {
        let counter = self.arena.ok_or("missing native register arena")?.counter;
        self.cells(counter, value.scalar_count() as usize, |this| {
            this.unpack_cell(start, offset, value, counter)
        })
    }

    fn unpack_cell(
        &mut self,
        start: Reg,
        offset: u32,
        value: &solve::SolveValueType,
        counter: u32,
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
