use super::*;

impl RowLowerCtx<'_, '_> {
    pub(super) fn lower_op(
        &mut self,
        op: LinearOp,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        if !crate::expansion::enabled() {
            return self.lower_op_with_constants(op);
        }
        let token = crate::expansion::row(
            &op,
            (
                std::ptr::from_ref(&*self.fb.func) as usize,
                self.fb.func.dfg.num_insts(),
            ),
        );
        let result = self.lower_op_with_constants(op);
        crate::expansion::finish(token, self.fb.func.dfg.num_insts(), result.is_ok());
        result
    }

    fn lower_op_with_constants(
        &mut self,
        op: LinearOp,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let constant = self.operation_constant(&op);
        let destination = op
            .dst_register()
            .map(|start| {
                checked_reg_offset(start, op.dst_register_count() - 1, "constant destination")
                    .map(|last| start..=last)
            })
            .transpose()?;
        let output = self.lower_op_native(op)?;
        if let Some(destination) = destination {
            if destination.start() == destination.end() {
                self.known_constants.remove(destination.start());
            } else {
                self.known_constants
                    .retain(|register, _| !destination.contains(register));
            }
            if let Some(value) = constant {
                self.known_constants.insert(*destination.start(), value);
            }
        }
        Ok(output)
    }

    fn operation_constant(&self, op: &LinearOp) -> Option<f64> {
        match op {
            LinearOp::Const { value, .. } => Some(*value),
            LinearOp::LoadFoldIndex { dimension, .. } => {
                self.fold_index_constants?.get(*dimension).copied()
            }
            LinearOp::Move { src, .. } => self.known_constants.get(src).copied(),
            LinearOp::Unary { op, arg, .. } => {
                fold_unary_constant(*op, *self.known_constants.get(arg)?)
            }
            LinearOp::Binary { op, lhs, rhs, .. } => fold_binary_constant(
                *op,
                *self.known_constants.get(lhs)?,
                *self.known_constants.get(rhs)?,
            ),
            _ => None,
        }
    }
}
