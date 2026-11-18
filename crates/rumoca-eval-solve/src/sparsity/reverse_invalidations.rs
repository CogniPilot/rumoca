//! Ordered projection invalidation without a dense inverse dependency relation.

#[cfg(test)]
mod tests;

use rumoca_ir_solve::{AlgebraicProjectionPlan, StructuralPattern, StructuralPatternView};

use super::{EvalSolveError, sparsity_error};

#[cfg(test)]
pub(super) fn derive_algebraic_reverse_invalidations(
    source: Option<&StructuralPattern>,
    plan: &AlgebraicProjectionPlan,
) -> Result<Vec<bool>, EvalSolveError> {
    derive_algebraic_reverse_invalidations_with_execution(source, plan, None)
}

pub(super) fn derive_algebraic_reverse_invalidations_with_execution(
    source: Option<&StructuralPattern>,
    plan: &AlgebraicProjectionPlan,
    primal: Option<&rumoca_ir_solve::ScalarProgramBlock>,
) -> Result<Vec<bool>, EvalSolveError> {
    let Some(source) = source else {
        return Ok(Vec::new());
    };
    if plan.blocks.is_empty() {
        return Ok(Vec::new());
    }
    let obligations = execution_rows(primal, source)?;
    let mut earlier = PriorRowDependencies::new(source);
    let mut invalidations = Vec::with_capacity(plan.blocks.len());
    for block in plan.blocks.scalar_blocks() {
        // Every target is checked, even after the first intersecting target.
        // This block's rows become earlier only after all its targets are read.
        let invalidates = block.y_indices.iter().try_fold(false, |found, &column| {
            earlier
                .invalidates(column)
                .map(|affected| found || affected)
        })?;
        invalidations.push(invalidates);
        for &row in &block.rows {
            earlier.include(row)?;
            if let Some(reads) = obligations.get(row).and_then(Option::as_ref) {
                earlier.mark_affected(reads);
            }
        }
    }
    Ok(invalidations)
}

struct PriorRowDependencies<'source> {
    source: &'source StructuralPattern,
    included_rows: Vec<bool>,
    affected_columns: Vec<bool>,
    all_columns_affected: bool,
}

impl<'source> PriorRowDependencies<'source> {
    fn new(source: &'source StructuralPattern) -> Self {
        Self {
            source,
            included_rows: vec![false; source.rows() as usize],
            affected_columns: vec![false; source.columns() as usize],
            all_columns_affected: false,
        }
    }

    fn invalidates(&self, column: usize) -> Result<bool, EvalSolveError> {
        let Some(&affected) = self.affected_columns.get(column) else {
            return Err(sparsity_error(
                format!(
                    "projection invalidation column {column} is outside 0..{}",
                    self.source.columns()
                ),
                Some(self.source.provenance().span()),
            ));
        };
        Ok(self.all_columns_affected || affected)
    }

    /// Columns an earlier row's execution obligations read: a later block
    /// that changes one invalidates it, without a Jacobian edge.
    fn mark_affected(&mut self, reads: &rumoca_ir_solve::IndexIntervals) {
        for column in reads.iter() {
            self.affected_columns[column] = true;
        }
    }

    fn include(&mut self, row: usize) -> Result<(), EvalSolveError> {
        let row_count = self.included_rows.len();
        let Some(included) = self.included_rows.get_mut(row) else {
            return Err(sparsity_error(
                format!("projection invalidation row {row} is outside 0..{row_count}"),
                Some(self.source.provenance().span()),
            ));
        };
        if *included {
            return Ok(());
        }
        *included = true;
        if matches!(self.source.view(), StructuralPatternView::Full) {
            self.all_columns_affected = true;
        } else {
            // Read the existing certified relation; never derive or omit edges
            // in this consumer. Each included row is visited at most once.
            self.source.visit_row_columns(row, &mut |column| {
                self.affected_columns[column] = true;
            });
        }
        Ok(())
    }
}

/// Whole program obligations accompany every output that evaluates that owner.
/// They do not enter the value/Jacobian relation used for numerical projection.
fn execution_rows(
    block: Option<&rumoca_ir_solve::ScalarProgramBlock>,
    source: &StructuralPattern,
) -> Result<Vec<Option<std::sync::Arc<rumoca_ir_solve::IndexIntervals>>>, EvalSolveError> {
    let Some(block) = block else {
        return Ok(Vec::new());
    };
    if block.output_count() != source.rows() as usize {
        return Err(sparsity_error(
            "execution invalidation output extent differs from its Jacobian",
            Some(source.provenance().span()),
        ));
    }
    let mut rows = vec![None; block.output_count()];
    let mut ordinal = 0;
    for (program_index, program) in block.programs().iter().enumerate() {
        let span = block.program_span(program_index);
        let reads =
            StructuralPattern::derive_execution_obligation_y_dependency_ranges(program, span)
                .map_err(|error| super::from_pattern_error(error, span))?;
        if reads
            .iter()
            .any(|column| column >= source.columns() as usize)
        {
            return Err(sparsity_error(
                "execution invalidation read is outside solver columns",
                span,
            ));
        }
        for _ in 0..rumoca_ir_solve::ScalarProgramBlock::program_output_count(program) {
            let row = *block.output_indices().get(ordinal).ok_or_else(|| {
                sparsity_error("execution invalidation output lacks identity", span)
            })?;
            let slot = rows.get_mut(row).ok_or_else(|| {
                sparsity_error("execution invalidation output is out of bounds", span)
            })?;
            if slot.is_some() {
                return Err(sparsity_error(
                    "execution invalidation output is duplicated",
                    span,
                ));
            }
            *slot = Some(reads.clone());
            ordinal += 1;
        }
    }
    if ordinal != block.output_indices().len() {
        return Err(sparsity_error(
            "execution invalidation output inventory differs",
            Some(source.provenance().span()),
        ));
    }
    Ok(rows)
}
