use std::{collections::BTreeMap, sync::Arc};

use super::*;

#[derive(Clone, Default)]
pub(super) struct PreparedTensorAffineAssignments {
    // The containing prepared row retains the immutable checked source owner.
    // A store's exact prefix distinguishes repeated output-register uses.
    entries: RefCell<BTreeMap<(u32, usize, usize), Arc<PreparedTensorAffine>>>,
}

pub(super) struct PreparedTensorAffine {
    program: ScalarProgramBlock,
    registers: usize,
    result: u32,
    coefficient: u32,
}

impl PreparedTensorAffineAssignments {
    #[cfg(test)]
    pub(super) fn materialized_count(&self) -> usize {
        self.entries.borrow().len()
    }

    pub(super) fn selected(
        &self,
        row: &[LinearOp],
        shape: &TargetAssignmentShape,
        span: Option<rumoca_core::Span>,
    ) -> Result<Arc<PreparedTensorAffine>, EvalSolveError> {
        let TargetAssignmentShape::TensorAffine {
            target_y_index,
            projection,
            ..
        } = shape
        else {
            return Err(invalid_prepared_row(
                "selected assignment is not tensor-affine",
            ));
        };
        let key = (
            projection.output_register(),
            *target_y_index,
            shape.expr_eval_len(),
        );
        if let Some(prepared) = self.entries.borrow().get(&key) {
            return Ok(Arc::clone(prepared));
        }
        let prepared = Arc::new(PreparedTensorAffine::new(row, shape, span)?);
        self.entries.borrow_mut().insert(key, Arc::clone(&prepared));
        Ok(prepared)
    }
}

impl PreparedTensorAffine {
    fn new(
        row: &[LinearOp],
        shape: &TargetAssignmentShape,
        span: Option<rumoca_core::Span>,
    ) -> Result<Self, EvalSolveError> {
        let invalid =
            || invalid_prepared_row_with_span("invalid issued tensor-affine projection", span);
        let mut operations = row
            .get(..shape.expr_eval_len())
            .ok_or_else(invalid)?
            .iter()
            .filter(|op| {
                !matches!(
                    op,
                    LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }
                )
            })
            .cloned()
            .collect::<Vec<_>>();
        let (result, coefficient) =
            rumoca_ir_solve::materialize_target_assignment(shape, &mut operations)
                .ok_or_else(invalid)?;
        let coefficient = coefficient.ok_or_else(invalid)?;
        operations.push(LinearOp::StoreOutput { src: result });
        let program = ScalarProgramBlock::with_program_spans(
            vec![operations],
            vec![span.ok_or_else(invalid)?],
        )
        .map_err(|_| invalid())?;
        let registers = program.program_register_count(0).ok_or_else(invalid)?;
        Ok(Self {
            program,
            registers,
            result,
            coefficient,
        })
    }

    pub(super) fn eval(
        &self,
        request: TargetAssignmentScratchRequest<'_>,
        span: Option<rumoca_core::Span>,
    ) -> Result<f64, EvalSolveError> {
        eval_prevalidated_discard_output_program(
            PreparedRowEval::new(
                &self.program.programs()[0],
                self.registers,
                request.y,
                request.p,
                request.t,
                request.context,
            )
            .with_source_span(span),
            true,
            &mut *request.scratch,
        )
        .map_err(|error| error.with_source_span(span))?;
        let coefficient = request.scratch.regs[self.coefficient as usize];
        if coefficient == 0.0 || !coefficient.is_finite() {
            return Err(EvalSolveError::SingularTargetAssignment {
                row: request.row_idx,
                target_y_index: request.shape.target_y_index(),
                coefficient,
                span,
            });
        }
        Ok(request.scratch.regs[self.result as usize])
    }
}
