//! Row output accounting around the actual chosen bulk or scalar path.

use super::*;

impl RowLowerCtx<'_, '_> {
    pub(super) fn lower_function_fold_on_stack(
        &mut self,
        carried_slot: StackSlot,
        captures: &[cranelift_codegen::ir::Value],
        captures_ptr: Option<cranelift_codegen::ir::Value>,
        program: &rumoca_ir_solve::FunctionFoldProgram,
    ) -> Result<(), CompileError> {
        if !crate::expansion::enabled() {
            return self.lower_function_fold_on_stack_unrecorded(
                carried_slot,
                captures,
                captures_ptr,
                program,
            );
        }
        let token = crate::expansion::open(
            crate::ExpansionSource::FunctionFold(Box::new(program.clone())),
            "FunctionFoldBody",
            None,
            program.carried_count as u64,
            (
                std::ptr::from_ref(&*self.fb.func) as usize,
                self.fb.func.dfg.num_insts(),
            ),
        );
        let result = self.lower_function_fold_on_stack_unrecorded(
            carried_slot,
            captures,
            captures_ptr,
            program,
        );
        crate::expansion::finish(token, self.fb.func.dfg.num_insts(), result.is_ok());
        result
    }

    pub(super) fn lower_recorded_output(
        &mut self,
        op: &LinearOp,
        out_ptr: cranelift_codegen::ir::Value,
        out_idx: i32,
    ) -> Result<i32, CompileError> {
        if !crate::expansion::enabled() {
            return self.lower_single_output(op, out_ptr, out_idx);
        }
        let count = match op {
            LinearOp::StoreOutputRange { count, .. } => *count,
            LinearOp::StoreOutput { .. } => 1,
            _ => return self.lower_single_output(op, out_ptr, out_idx),
        };
        let token = crate::expansion::open(
            crate::ExpansionSource::Row(op.clone()),
            op.kind_name(),
            None,
            count as u64,
            (
                std::ptr::from_ref(&*self.fb.func) as usize,
                self.fb.func.dfg.num_insts(),
            ),
        );
        crate::expansion::path("row-output-store-scope");
        let result = self.lower_single_output(op, out_ptr, out_idx);
        crate::expansion::finish(token, self.fb.func.dfg.num_insts(), result.is_ok());
        result
    }

    fn lower_single_output(
        &mut self,
        op: &LinearOp,
        out_ptr: cranelift_codegen::ir::Value,
        mut out_idx: i32,
    ) -> Result<i32, CompileError> {
        if let LinearOp::StoreOutputRange {
            start,
            count,
            stride: 1,
        } = *op
            && let Some(next) = self.copy_taped_output_range(start, count, out_ptr, out_idx)?
        {
            crate::expansion::path("row-tape-range");
            return Ok(next);
        }
        let values = match op.clone() {
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                crate::expansion::path("row-scalar-range");
                crate::expansion::moved(
                    count as u64,
                    "row-scalar-output",
                    std::ptr::from_ref(&*self.fb.func) as usize,
                );
                self.lower_output_range(start, count, stride)?
            }
            operation => self.lower_op(operation)?.into_iter().collect(),
        };
        for value in values {
            self.fb.ins().store(self.flags, value, out_ptr, out_idx * 8);
            out_idx += 1;
        }
        Ok(out_idx)
    }

    pub(super) fn store_assignment_outputs(
        &mut self,
        row: &[LinearOp],
        outputs: Vec<cranelift_codegen::ir::Value>,
        targets: &[usize],
    ) -> Result<(), CompileError> {
        let token = crate::expansion::enabled()
            .then(|| {
                crate::expansion::open(
                    crate::ExpansionSource::AssignmentOutputs(row.to_vec()),
                    "AssignmentOutputs",
                    None,
                    targets.len() as u64,
                    (
                        std::ptr::from_ref(&*self.fb.func) as usize,
                        self.fb.func.dfg.num_insts(),
                    ),
                )
            })
            .flatten();
        crate::expansion::moved(
            targets.len() as u64,
            "assignment-output-stores",
            std::ptr::from_ref(&*self.fb.func) as usize,
        );
        let result = self.store_assignment_outputs_unrecorded(outputs, targets);
        crate::expansion::finish(token, self.fb.func.dfg.num_insts(), result.is_ok());
        result
    }
}
