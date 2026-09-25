//! Local matrix coordinates of a construction-proven causal partition.

#[cfg(test)]
mod tests;

use std::collections::BTreeMap;

use super::{AlgebraicProjectionBlock, StructuralPattern};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AffineEliminationLayout {
    pattern: StructuralPattern,
    reduced_pattern: StructuralPattern,
    row_columns: Box<[Box<[usize]>]>,
    zero_guards: Box<[(usize, usize)]>,
    guard_steps: Box<[(usize, usize)]>,
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
        let rows = local_positions(&block.rows)?;
        let columns = local_positions(&block.y_indices)?;
        let residuals = tearing
            .residual_rows
            .iter()
            .map(|row| rows.get(row).copied())
            .collect::<Option<Box<[_]>>>()?;
        let tears = tearing
            .tear_y_indices
            .iter()
            .map(|column| columns.get(column).copied())
            .collect::<Option<Box<[_]>>>()?;
        let causal = tearing
            .causal_steps
            .iter()
            .map(|step| Some((*rows.get(&step.row)?, *columns.get(&step.y_index)?)))
            .collect::<Option<Box<[_]>>>()?;
        let mut seen_rows = vec![false; n];
        let mut known_columns = vec![false; n];
        let mut zero_guards = Vec::new();
        for (&row, &column) in residuals.iter().zip(&tears) {
            claim(&mut seen_rows, row)?;
            claim(&mut known_columns, column)?;
        }
        for &(row, column) in &causal {
            claim(&mut seen_rows, row)?;
            claim(&mut known_columns, column)?;
            if !pattern.contains(row as u32, column as u32) {
                return None;
            }
            append_zero_guards(pattern, row, &known_columns, &mut zero_guards);
        }
        if !seen_rows.iter().chain(&known_columns).all(|seen| *seen) {
            return None;
        }
        let guard_steps = guard_steps(&causal, &zero_guards, n)?;
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
            guard_steps,
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

    /// Coefficients that must be exactly zero before this order is triangular
    /// without promoting the step that solves their column.
    pub fn zero_guards(&self) -> &[(usize, usize)] {
        &self.zero_guards
    }

    /// For each zero guard, in the same order, the causal position of the
    /// step whose row holds it and the later causal position that solves its
    /// column. A nonzero guard promotes that solving step to a tear unless the
    /// holding step was itself promoted; guards are ordered by holding
    /// position, so every promotion of a holding step is decided first.
    pub fn guard_steps(&self) -> &[(usize, usize)] {
        &self.guard_steps
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

fn local_positions(indices: &[usize]) -> Option<BTreeMap<usize, usize>> {
    let mut positions = BTreeMap::new();
    for (local, &source) in indices.iter().enumerate() {
        if positions.insert(source, local).is_some() {
            return None;
        }
    }
    Some(positions)
}

fn claim(seen: &mut [bool], index: usize) -> Option<()> {
    let previous = std::mem::replace(seen.get_mut(index)?, true);
    (!previous).then_some(())
}

fn guard_steps(
    causal: &[(usize, usize)],
    guards: &[(usize, usize)],
    n: usize,
) -> Option<Box<[(usize, usize)]>> {
    let mut row_position = vec![None; n];
    let mut column_position = vec![None; n];
    for (position, &(row, column)) in causal.iter().enumerate() {
        row_position[row] = Some(position);
        column_position[column] = Some(position);
    }
    guards
        .iter()
        .map(|&(row, column)| {
            // Every zero guard is recorded from within the causal loop for the
            // row of the causal step being placed (see `append_zero_guards`),
            // so that row always owns a causal position; the lookup is
            // unreachable-None by construction. The solving column, by
            // contrast, may still be a tear, which owns no causal step and
            // declines the layout through its own lookup.
            debug_assert!(
                row_position[row].is_some(),
                "zero guard on row {row} without a causal step"
            );
            let holder = row_position[row]?;
            let solver = column_position[column]?;
            (solver > holder).then_some((holder, solver))
        })
        .collect()
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
