//! Private-output executable view issued by the existing prepared target owner.

#[cfg(test)]
mod tests;

use super::*;
use std::collections::BTreeMap;
use std::sync::Arc;

pub(super) type TargetValueFamilies = BTreeMap<(usize, usize), TargetValueFamily>;

pub(super) struct TargetValueFamily {
    program: ScalarProgramBlock,
    selections: BTreeMap<(usize, usize), usize>,
    coefficient_outputs: Arc<[(usize, usize, usize)]>,
}

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
    coefficient_outputs: Arc<[(usize, usize, usize)]>,
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
    /// Private value offset, target, and coefficient output for each guarded isolator.
    pub fn coefficient_outputs(&self) -> &[(usize, usize, usize)] {
        &self.coefficient_outputs
    }
}

impl PreparedScalarProgramBlock {
    /// Materialize the existing checked exact isolator family without committing Y.
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
        }) = self.target_value_prefix(row_idx, output_offset, target_y_index)?
        else {
            return Ok(None);
        };
        let mut families = self.target_value_families.borrow_mut();
        let key = (row_idx, prefix_len);
        let family = match families.entry(key) {
            std::collections::btree_map::Entry::Occupied(entry) => entry.into_mut(),
            std::collections::btree_map::Entry::Vacant(entry) => {
                // Retain every original prefix operation, including discarded output stores.
                // Typed calls retain all tuple results and their issued table owner.
                let mut operations = prefix.to_vec();
                let mut coefficient_outputs = Vec::new();
                let selections = self.append_target_value_family(
                    row_idx,
                    prefix_len,
                    &mut operations,
                    &mut coefficient_outputs,
                )?;
                let program = ScalarProgramBlock::with_program_spans(vec![operations], vec![span])
                    .map_err(|error| {
                        invalid_prepared_row_with_span(error.to_string(), Some(span))
                    })?;
                entry.insert(TargetValueFamily {
                    program,
                    selections,
                    coefficient_outputs: coefficient_outputs.into(),
                })
            }
        };
        let private_result_offset = family
            .selections
            .get(&(output_offset, target_y_index))
            .copied()
            .ok_or_else(|| {
                invalid_prepared_row("issued target family lacks its checked selection")
            })?;
        Ok(Some(PreparedTargetValuePlan {
            program: family.program.clone(),
            row: row_idx,
            output: output_offset,
            target: target_y_index,
            canonical_prefix_len: prefix.len(),
            private_result_offset,
            coefficient_outputs: Arc::clone(&family.coefficient_outputs),
        }))
    }
    fn target_value_prefix(
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
        prefix_len: usize,
        operations: &mut Vec<LinearOp>,
        coefficient_outputs: &mut Vec<(usize, usize, usize)>,
    ) -> Result<BTreeMap<(usize, usize), usize>, EvalSolveError> {
        let mut selections = BTreeMap::new();
        let mut issued = std::collections::BTreeSet::new();
        let mut output_offset = ScalarProgramBlock::program_output_count(operations);
        let mut builder = rumoca_ir_solve::ExactAssignmentProgramBuilder::new(operations)
            .ok_or_else(|| invalid_prepared_row("issued target prefix cannot materialize"))?;
        let coefficient_output_error =
            || invalid_prepared_row("issued coefficient output cannot materialize");
        for (candidate_output, shape) in self.row_assignment_shapes[row].iter() {
            // The canonical evaluator selects the first shape for this pair.
            if !issued.insert((*candidate_output, shape.target_y_index()))
                || shape.expr_eval_len() != prefix_len
            {
                continue;
            }
            let offset = output_offset;
            let (result, coefficient) = builder.materialize_guarded(shape).ok_or_else(|| {
                invalid_prepared_row("issued exact target family cannot materialize")
            })?;
            builder
                .push(LinearOp::StoreOutput { src: result })
                .ok_or_else(|| invalid_prepared_row("issued target output cannot materialize"))?;
            output_offset += 1;
            if let Some(coefficient) = coefficient {
                let coefficient_offset = output_offset;
                builder
                    .push(LinearOp::StoreOutput { src: coefficient })
                    .ok_or_else(coefficient_output_error)?;
                output_offset += 1;
                coefficient_outputs.push((offset, shape.target_y_index(), coefficient_offset));
            }
            selections.insert((*candidate_output, shape.target_y_index()), offset);
        }
        Ok(selections)
    }
}
