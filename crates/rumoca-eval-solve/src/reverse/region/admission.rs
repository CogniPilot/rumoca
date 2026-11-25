//! Check every reachable region body, including inactive alternatives. Shared
//! programs are immutable and live for the complete admission walk.
use super::*;
use std::{collections::HashSet, sync::Arc};

#[derive(Default)]
pub(in crate::reverse) struct Admission {
    folds: HashSet<*const FunctionFoldProgram>,
    conditionals: HashSet<*const FunctionConditionalProgram>,
}

impl Admission {
    pub(in crate::reverse) fn row(
        &mut self,
        row: &[LinearOp],
        row_idx: usize,
    ) -> Result<(), EvalSolveError> {
        for (instruction, op) in row.iter().enumerate() {
            self.check(op).map_err(|e| {
                unsupported(&format!("{e} at row {row_idx}, instruction {instruction}"))
            })?;
        }
        Ok(())
    }

    pub(in crate::reverse) fn check(&mut self, op: &LinearOp) -> Result<(), EvalSolveError> {
        use LinearOp::*;
        match op {
            FunctionFold { program, .. } | GuardedFunctionFold { program, .. } => {
                self.fold(program)
            }
            FunctionConditional { program, .. } => self.conditional(program),
            scalar_ops!() | StoreOutput { .. } | StoreOutputRange { .. } => Ok(()),
            tensor_ops!() => {
                if tensor::supported(op) {
                    tensor::refuse_overlap(op)
                } else {
                    Err(unsupported_reverse_op(op))
                }
            }
            PureCall {
                dst_start,
                input_starts,
                site,
            } => {
                if call::admitted(*dst_start, input_starts, site) {
                    Ok(())
                } else {
                    Err(unsupported_reverse_op(op))
                }
            }
            region_local_ops!() | outside_first_order!() => Err(unsupported_reverse_op(op)),
        }
    }

    fn fold(&mut self, program: &Arc<FunctionFoldProgram>) -> Result<(), EvalSolveError> {
        if self.folds.insert(Arc::as_ptr(program)) {
            self.body("fold update", &program.update)?;
            if let Some(test) = &program.continuation {
                self.body("fold continuation", &test.ops)?;
            }
        }
        Ok(())
    }

    fn conditional(
        &mut self,
        program: &Arc<FunctionConditionalProgram>,
    ) -> Result<(), EvalSolveError> {
        if self.conditionals.insert(Arc::as_ptr(program)) {
            for arm in &program.arms {
                self.body("conditional predicate", &arm.condition)?;
                self.body("conditional result", &arm.result)?;
            }
            self.body("conditional fallback", &program.fallback)?;
        }
        Ok(())
    }

    fn body(&mut self, label: &str, row: &[LinearOp]) -> Result<(), EvalSolveError> {
        use LinearOp::*;
        for (i, op) in row.iter().enumerate() {
            let result = match op {
                LoadIndexedFoldCarried { .. }
                | LoadIndexedFoldCapture { .. }
                | LoadFoldCarried { .. }
                | LoadFoldCapture { .. }
                | LoadFoldIndex { .. }
                | LoadFunctionConditionalCapture { .. }
                | LoadFunctionConditionalCaptureRange { .. }
                | StoreOutputFoldTensorUpdate { lanes: 1, .. } => Ok(()),
                StoreOutputFunctionFold { program, .. } => self.fold(program),
                scalar_ops!()
                | tensor_ops!()
                | region_ops!()
                | outside_first_order!()
                | StoreOutputFoldTensorUpdate { .. }
                | PureCall { .. }
                | StoreOutput { .. }
                | StoreOutputRange { .. } => self.check(op),
            };
            result.map_err(|e| unsupported(&format!("{e} in {label} at instruction {i}")))?;
        }
        Ok(())
    }
}
