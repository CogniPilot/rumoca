//! Local matrix coordinates of a construction-proven causal partition.

#[cfg(test)]
mod tests;

use super::{AlgebraicProjectionBlock, StructuralPattern};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AffineEliminationLayout {
    pattern: StructuralPattern,
    reduced_pattern: StructuralPattern,
    row_columns: Box<[Box<[usize]>]>,
    zero_guards: Box<[(usize, usize)]>,
    causal: Box<[(usize, usize)]>,
    residuals: Box<[usize]>,
    tears: Box<[usize]>,
}

impl AffineEliminationLayout {
    pub(super) fn derive(
        block: &AlgebraicProjectionBlock,
        pattern: &StructuralPattern,
    ) -> Option<Self> {
        let tearing = block.tearing.as_ref()?;
        Self::derive_for_tearing(block, pattern, tearing)
    }

    pub(super) fn derive_for_tearing(
        block: &AlgebraicProjectionBlock,
        pattern: &StructuralPattern,
        tearing: &super::BlockTearing,
    ) -> Option<Self> {
        let n = block.rows.len();
        if n != block.y_indices.len()
            || n != pattern.rows() as usize
            || n != pattern.columns() as usize
            || tearing.causal_steps.is_empty()
            || tearing.tear_y_indices.is_empty()
            || tearing.tear_y_indices.len() != tearing.residual_rows.len()
        {
            return None;
        }
        let partition = super::tearing_pair::LocalTearingPartition::derive(block, tearing)?;
        let super::tearing_pair::LocalTearingPartition {
            residuals,
            tears,
            causal,
        } = partition;
        let mut known_columns = vec![false; n];
        for &column in &tears {
            known_columns[column] = true;
        }
        let mut zero_guards = Vec::new();
        for &(row, column) in &causal {
            known_columns[column] = true;
            if !pattern.contains(row as u32, column as u32) {
                return None;
            }
            append_zero_guards(pattern, row, &known_columns, &mut zero_guards);
        }
        let row_columns = (0..n)
            .map(|row| {
                let mut columns = Vec::new();
                pattern.visit_row_columns(row, |column| columns.push(column));
                columns.into_boxed_slice()
            })
            .collect();
        Some(Self {
            pattern: pattern.clone(),
            reduced_pattern: StructuralPattern::full(
                tears.len(),
                tears.len(),
                pattern.provenance(),
            )
            .ok()?,
            row_columns,
            zero_guards: zero_guards.into_boxed_slice(),
            causal,
            residuals,
            tears,
        })
    }

    pub const fn pattern(&self) -> &StructuralPattern {
        &self.pattern
    }

    pub const fn reduced_pattern(&self) -> &StructuralPattern {
        &self.reduced_pattern
    }
    pub fn row_columns(&self, row: usize) -> &[usize] {
        &self.row_columns[row]
    }

    /// Coefficients that must be exactly zero before this order is triangular.
    pub fn zero_guards(&self) -> &[(usize, usize)] {
        &self.zero_guards
    }

    pub fn causal(&self) -> &[(usize, usize)] {
        &self.causal
    }
    pub fn residuals(&self) -> &[usize] {
        &self.residuals
    }
    pub fn tears(&self) -> &[usize] {
        &self.tears
    }
}

fn append_zero_guards(
    pattern: &StructuralPattern,
    row: usize,
    known_columns: &[bool],
    guards: &mut Vec<(usize, usize)>,
) {
    pattern.visit_row_columns(row, |dependency| {
        if !known_columns[dependency] {
            guards.push((row, dependency));
        }
    });
}
