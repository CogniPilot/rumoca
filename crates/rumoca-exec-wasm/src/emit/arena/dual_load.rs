//! Preserve the source's interleaved primal and seed lanes.
use super::*;

impl BodyEmitter<'_> {
    pub(super) fn arena_dual_load(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            count,
            seed_start,
            ..
        } = operation
        else {
            return Err("expected two-lane TensorLoad".into());
        };
        let pointer = match input {
            rumoca_ir_solve::TensorInputKind::Y => Y_PTR_PARAM,
            rumoca_ir_solve::TensorInputKind::P => P_PTR_PARAM,
        };
        let counter = self.arena_plan()?.counter;
        self.loop_start(counter, count)?;
        self.push_arena_address(dst_start, Some(counter), 2)?;
        self.push(Instruction::LocalGet(pointer));
        self.push(Instruction::I32Const(layouts::cell_bytes(input_start)?));
        self.push(Instruction::I32Add);
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
        self.push(Instruction::F64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        self.push(Instruction::F64Store(memarg()));
        self.push_arena_address(dst_start + 1, Some(counter), 2)?;
        if let Some(start) = seed_start {
            self.seed_array_address(start, counter)?;
            self.push(Instruction::F64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
        } else {
            self.push(Instruction::F64Const(0.0.into()));
        }
        self.push(Instruction::F64Store(memarg()));
        self.loop_end(counter);
        Ok(())
    }
}
