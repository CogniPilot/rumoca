//! Private-output executable view issued by the existing prepared target owner.

use super::*;

struct TargetValuePrefix<'a> {
    operations: &'a [LinearOp],
    len: usize,
    span: rumoca_core::Span,
}

/// A checked canonical target query, distinct from a Y-committing refresh schedule.
#[derive(Clone)]
pub struct PreparedTargetValuePlan {
    program: ScalarProgramBlock,
    row: usize,
    output: usize,
    target: usize,
    canonical_prefix_len: usize,
    private_result_offset: usize,
}

impl PreparedTargetValuePlan {
    pub fn program(&self) -> &ScalarProgramBlock {
        &self.program
    }
    pub const fn row(&self) -> usize {
        self.row
    }
    pub const fn output(&self) -> usize {
        self.output
    }
    pub const fn target(&self) -> usize {
        self.target
    }
    pub const fn canonical_prefix_len(&self) -> usize {
        self.canonical_prefix_len
    }
    pub const fn private_result_offset(&self) -> usize {
        self.private_result_offset
    }
}

impl PreparedScalarProgramBlock {
    /// Initial portable profile covers only checked exact Direct/Zero isolators.
    /// Other shapes keep their canonical evaluator until their error ABI is proved.
    pub fn portable_target_value_plan(
        &self,
        row_idx: usize,
        output_offset: usize,
        target_y_index: usize,
    ) -> Result<Option<PreparedTargetValuePlan>, EvalSolveError> {
        let Some(TargetValuePrefix {
            operations: prefix,
            len: prefix_len,
            span,
        }) = self.direct_target_value_prefix(row_idx, output_offset, target_y_index)?
        else {
            return Ok(None);
        };
        // Retain every original prefix operation, including discarded output stores.
        // Typed calls retain every original tuple result and their issued table owner.
        let mut operations = prefix.to_vec();
        let private_result_offset = self.append_target_value_family(
            row_idx,
            output_offset,
            target_y_index,
            prefix_len,
            &mut operations,
        )?;
        let program = ScalarProgramBlock::with_program_spans(vec![operations], vec![span])
            .map_err(|error| invalid_prepared_row_with_span(error.to_string(), Some(span)))?;
        Ok(Some(PreparedTargetValuePlan {
            program,
            row: row_idx,
            output: output_offset,
            target: target_y_index,
            canonical_prefix_len: prefix.len(),
            private_result_offset,
        }))
    }
    fn direct_target_value_prefix(
        &self,
        row_idx: usize,
        output_offset: usize,
        target_y_index: usize,
    ) -> Result<Option<TargetValuePrefix<'_>>, EvalSolveError> {
        if !self.certifies_exact_target_assignment_output(row_idx, output_offset, target_y_index) {
            return Ok(None);
        }
        let Some(shape) = self.assignment_shape_for_output(row_idx, output_offset, target_y_index)
        else {
            return Ok(None);
        };
        if !shape.is_direct() {
            return Ok(None);
        }
        let span = self.block.program_span(row_idx);
        let Some(source) = self.block.programs().get(row_idx) else {
            return Ok(None);
        };
        let Some(prefix) = source.get(..shape.expr_eval_len()) else {
            return Err(invalid_prepared_row_with_span(
                "issued target-value prefix is out of bounds",
                span,
            ));
        };
        let Some(span) = span else {
            return Ok(None);
        };
        Ok(Some(TargetValuePrefix {
            operations: prefix,
            len: shape.expr_eval_len(),
            span,
        }))
    }

    fn append_target_value_family(
        &self,
        row: usize,
        output: usize,
        target: usize,
        prefix_len: usize,
        operations: &mut Vec<LinearOp>,
    ) -> Result<usize, EvalSolveError> {
        let mut selected = None;
        for (candidate_output, shape) in self.row_assignment_shapes[row].iter() {
            if shape.expr_eval_len() != prefix_len || !shape.is_direct() {
                continue;
            }
            let offset = ScalarProgramBlock::program_output_count(operations);
            let result = AssignmentProgramBuilder::new(operations)
                .and_then(|mut builder| builder.materialize(shape))
                .ok_or_else(|| {
                    invalid_prepared_row("issued direct target family cannot materialize")
                })?;
            operations.push(LinearOp::StoreOutput { src: result });
            if *candidate_output == output && shape.target_y_index() == target {
                selected = Some(offset);
            }
        }
        selected
            .ok_or_else(|| invalid_prepared_row("issued target family lacks its checked selection"))
    }
}
