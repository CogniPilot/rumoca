//! Whole-owner differential orders without splitting canonical tensors.

use std::ops::Range;

use rumoca_ir_dae as dae;

use super::{DifferentialStructure, SignatureEntry, contract, offsets};
use crate::StructuralError;
use crate::incidence::projection::visit_owner_rows;

#[cfg(test)]
mod tests;

/// Source-bound offset refinement, conditional on the same numerical regularity
/// as its scalar analysis. This product does not authorize a state basis.
#[derive(Debug)]
pub struct TensorDifferentialOffsets<'analysis, 'dae> {
    source: &'analysis DifferentialStructure<'dae>,
    equations: Vec<u32>,
    variables: Vec<u32>,
}

impl<'analysis, 'dae> TensorDifferentialOffsets<'analysis, 'dae> {
    pub fn source(&self) -> &'analysis DifferentialStructure<'dae> {
        self.source
    }

    /// Canonical continuous scalar-view orders, constant within each owner.
    pub fn equation_orders(&self) -> &[u32] {
        &self.equations
    }

    /// Orders aligned with source coordinates, constant within each declaration.
    pub fn variable_orders(&self) -> &[u32] {
        &self.variables
    }
}

pub(super) fn analyze<'analysis, 'dae>(
    source: &'analysis DifferentialStructure<'dae>,
    view: dae::DaeView<'dae>,
) -> Result<Option<TensorDifferentialOffsets<'analysis, 'dae>>, StructuralError> {
    let mut row_groups = Vec::new();
    let mut spans = Vec::new();
    let mut end = 0;
    for owner in view.continuous_owners() {
        let start = end;
        visit_owner_rows(view, owner, |row| {
            spans.push(row.provenance.span());
            end += 1;
            Ok(())
        })?;
        row_groups.push(start..end);
    }
    let mut column_groups = Vec::new();
    let mut start = 0;
    while start < source.variables.len() {
        let variable = source.variables[start].variable;
        let end = source.variables[start..]
            .iter()
            .take_while(|coordinate| coordinate.variable == variable)
            .count()
            + start;
        column_groups.push(start..end);
        start = end;
    }
    let groups = OwnerGroups {
        rows: &row_groups,
        columns: &column_groups,
    };
    let Some((equations, variables)) = refine(
        &source.rows,
        &source.matching,
        (&source.equation_orders, &source.variable_orders),
        groups,
    )
    .map_err(|reason| contract(&spans, reason))?
    else {
        return Ok(None);
    };
    if offsets::certify(&source.rows, &source.matching, &equations, &variables)
        != Some(source.formal_dimension)
        || !uniform(&equations, &row_groups)
        || !uniform(&variables, &column_groups)
    {
        return Err(contract(
            &spans,
            "tensor differential offset certificate is invalid",
        ));
    }
    Ok(Some(TensorDifferentialOffsets {
        source,
        equations,
        variables,
    }))
}

#[derive(Clone, Copy)]
struct OwnerGroups<'a> {
    rows: &'a [Range<usize>],
    columns: &'a [Range<usize>],
}

type OffsetVectors = (Vec<u32>, Vec<u32>);

fn refine(
    rows: &[Vec<SignatureEntry>],
    matching: &[usize],
    initial: (&[u32], &[u32]),
    groups: OwnerGroups<'_>,
) -> Result<Option<OffsetVectors>, &'static str> {
    let (mut equations, mut variables) = (initial.0.to_vec(), initial.1.to_vec());
    if rows.len() != matching.len()
        || rows.len() != equations.len()
        || rows.len() != variables.len()
        || !partitions(groups.rows, equations.len())
        || !partitions(groups.columns, variables.len())
    {
        return Err("tensor differential offset groups do not cover their source views");
    }
    // The contracted difference graph has one node per row/column owner.
    // One sweep relaxes every edge. If the final sweep still raises an order,
    // a positive cycle prevents any finite uniform solution.
    let nodes = groups
        .rows
        .len()
        .checked_add(groups.columns.len())
        .ok_or("tensor differential owner count overflow")?;
    for _ in 0..=nodes {
        let mut changed = equalize(&mut equations, groups.rows);
        for (row, entries) in rows.iter().enumerate() {
            for entry in entries {
                let value = equations[row]
                    .checked_add(entry.order)
                    .ok_or("tensor differential order overflow")?;
                let target = variables
                    .get_mut(entry.column)
                    .ok_or("tensor differential signature column is out of range")?;
                changed |= raise(target, value);
            }
        }
        changed |= equalize(&mut variables, groups.columns);
        for (row, &column) in matching.iter().enumerate() {
            let entry = rows[row]
                .iter()
                .find(|entry| entry.column == column)
                .ok_or("tensor differential matching claims an absent edge")?;
            let value = variables[column]
                .checked_sub(entry.order)
                .ok_or("negative tensor differential equation order")?;
            changed |= raise(&mut equations[row], value);
        }
        if !changed {
            return Ok(Some((equations, variables)));
        }
    }
    Ok(None)
}

fn raise(target: &mut u32, value: u32) -> bool {
    if value <= *target {
        return false;
    }
    *target = value;
    true
}

fn equalize(values: &mut [u32], groups: &[Range<usize>]) -> bool {
    let mut changed = false;
    for group in groups {
        let maximum = values[group.clone()].iter().copied().max().unwrap_or(0);
        for value in &mut values[group.clone()] {
            changed |= raise(value, maximum);
        }
    }
    changed
}

fn partitions(groups: &[Range<usize>], count: usize) -> bool {
    let mut end = 0;
    for group in groups {
        if group.start != end || group.end < group.start || group.end > count {
            return false;
        }
        end = group.end;
    }
    end == count
}

fn uniform(values: &[u32], groups: &[Range<usize>]) -> bool {
    groups.iter().all(|group| {
        let Some(first) = values.get(group.start) else {
            return group.is_empty();
        };
        values[group.clone()].iter().all(|value| value == first)
    })
}
