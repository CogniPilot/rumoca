//! Ordered projection invalidation without a dense inverse dependency relation.

#[cfg(test)]
mod tests;

use rumoca_ir_solve::{AlgebraicProjectionPlan, StructuralPattern, StructuralPatternView};

use super::{EvalSolveError, sparsity_error};

pub(super) fn derive_algebraic_reverse_invalidations(
    source: Option<&StructuralPattern>,
    plan: &AlgebraicProjectionPlan,
) -> Result<Vec<bool>, EvalSolveError> {
    let Some(source) = source else {
        return Ok(Vec::new());
    };
    if plan.blocks.is_empty() {
        return Ok(Vec::new());
    }
    let mut earlier = PriorRowDependencies::new(source);
    let mut invalidations = Vec::with_capacity(plan.blocks.len());
    for block in &plan.blocks {
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
