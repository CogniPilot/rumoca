//! Whole-owner differential orders without splitting canonical tensors.

use std::collections::BTreeSet;
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
    preferred: Vec<PreferredAdmission>,
}

/// One `StateSelect.prefer` declaration the refinement gave a formal successor,
/// with the equation owners whose order that admission raised, each named by
/// its first canonical scalar row.
#[derive(Debug, Clone)]
pub struct PreferredAdmission {
    pub variable: u32,
    /// `(first canonical row, order before this admission)` per raised owner.
    pub raised_owners: Vec<(usize, u32)>,
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

    /// The admitted `StateSelect.prefer` declarations, in declaration order.
    pub fn preferred(&self) -> &[PreferredAdmission] {
        &self.preferred
    }
}

pub(super) fn analyze<'analysis, 'dae>(
    source: &'analysis DifferentialStructure<'dae>,
    view: dae::DaeView<'dae>,
    withheld: &BTreeSet<u32>,
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
    let column_groups = declaration_column_groups(source);
    let groups = OwnerGroups {
        rows: &row_groups,
        columns: &column_groups,
    };
    let requests = |column: usize, request: rumoca_core::StateSelect| {
        view.variable(source.variables[column].variable)
            .expect("source differential coordinate")
            .continuous_state_select()
            == Some(request)
    };
    let variable_orders = source
        .variable_orders
        .iter()
        .enumerate()
        .map(|(column, &order)| {
            if requests(column, rumoca_core::StateSelect::Always) {
                order.max(1)
            } else {
                order
            }
        })
        .collect::<Vec<_>>();
    let Some((mut equations, mut variables)) = refine(
        &source.rows,
        &source.matching,
        (&source.equation_orders, &variable_orders),
        groups,
        source.invariant_columns(),
    )
    .map_err(|reason| contract(&spans, reason))?
    else {
        return Ok(None);
    };
    let mut preferred = admit_preferred(
        source,
        groups,
        (&mut equations, &mut variables),
        |group: &Range<usize>| {
            requests(group.start, rumoca_core::StateSelect::Prefer)
                && !withheld.contains(&source.variables[group.start].variable.index())
        },
    )
    .map_err(|reason| contract(&spans, reason))?;
    let undifferentiated = |group: &Range<usize>| {
        let variable = view
            .variable(source.variables[group.start].variable)
            .expect("source differential coordinate");
        variable.role() != dae::VariableRole::State
            && matches!(
                variable.continuous_state_select(),
                Some(rumoca_core::StateSelect::Always | rumoca_core::StateSelect::Prefer)
            )
            && !withheld.contains(&source.variables[group.start].variable.index())
    };
    preferred.extend(
        deepen_requested(
            source,
            groups,
            (&mut equations, &mut variables),
            (undifferentiated, |column: usize| {
                view.variable(source.variables[column].variable)
                    .is_some_and(|variable| variable.role() == dae::VariableRole::State)
            }),
        )
        .map_err(|reason| contract(&spans, reason))?,
    );
    if offsets::certify(
        &source.rows,
        &source.matching,
        &equations,
        &variables,
        source.invariant_columns(),
    ) != Some(source.formal_dimension)
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
        preferred,
    }))
}

/// MLS 3.7 §4.9.7.1 makes `prefer` a request, not an obligation. Each requested
/// declaration without a formal successor receives one exactly when the monotone
/// closure admits it on top of every earlier admission, in declaration order; a
/// positive cycle leaves it without one. The closure preserves matched
/// equalities, so the formal dimension is unchanged, and it never lowers an
/// order, so every earlier admission keeps its successor.
fn admit_preferred(
    source: &DifferentialStructure<'_>,
    groups: OwnerGroups<'_>,
    (equations, variables): (&mut Vec<u32>, &mut Vec<u32>),
    requested: impl Fn(&Range<usize>) -> bool,
) -> Result<Vec<PreferredAdmission>, &'static str> {
    let mut admitted = Vec::new();
    for group in groups.columns {
        if variables[group.start] > 0
            || source.invariant_columns()[group.start]
            || !requested(group)
        {
            continue;
        }
        let mut raised = variables.clone();
        raised[group.clone()].fill(1);
        let Some((next_equations, next_variables)) = refine(
            &source.rows,
            &source.matching,
            (equations, &raised),
            groups,
            source.invariant_columns(),
        )?
        else {
            continue;
        };
        let raised_owners = groups
            .rows
            .iter()
            .filter(|rows| !rows.is_empty() && next_equations[rows.start] > equations[rows.start])
            .map(|rows| (rows.start, equations[rows.start]))
            .collect();
        admitted.push(PreferredAdmission {
            variable: source.variables[group.start].variable.index(),
            raised_owners,
        });
        (*equations, *variables) = (next_equations, next_variables);
    }
    Ok(admitted)
}

/// Deepest equation order the shared differentiation profile constructs.
pub(crate) const FORMAL_ORDER_PROFILE: u32 = 2;

/// Place each undifferentiated `StateSelect.always` or `prefer` declaration that
/// holds a formal successor at the deepest derivative level its own equations
/// admit: its order rises while the closure raises no differentiated source
/// declaration and no equation beyond [`FORMAL_ORDER_PROFILE`]. An algebraic tied
/// to position-level coordinates (a joint's `x = prismatic.s`) then competes at
/// their stage,
/// where MLS 3.7 §4.9.7.1 asks for it as a state; one tied to rates stays at the
/// rate stage. Each raise is recorded like an admission, so a refused
/// prolongation withholds it.
fn deepen_requested(
    source: &DifferentialStructure<'_>,
    groups: OwnerGroups<'_>,
    (equations, variables): (&mut Vec<u32>, &mut Vec<u32>),
    (requested, differentiated): (impl Fn(&Range<usize>) -> bool, impl Fn(usize) -> bool),
) -> Result<Vec<PreferredAdmission>, &'static str> {
    let mut deepened = Vec::new();
    for group in groups.columns {
        if variables[group.start] == 0 || !requested(group) {
            continue;
        }
        loop {
            let mut raised = variables.clone();
            let order = variables[group.start] + 1;
            raised[group.clone()].fill(order);
            let Some((next_equations, next_variables)) = refine(
                &source.rows,
                &source.matching,
                (equations, &raised),
                groups,
                source.invariant_columns(),
            )?
            else {
                break;
            };
            let states_unchanged = next_variables
                .iter()
                .zip(raised.iter())
                .enumerate()
                .all(|(column, (next, current))| next == current || !differentiated(column));
            if !states_unchanged
                || next_equations
                    .iter()
                    .any(|&order| order > FORMAL_ORDER_PROFILE)
            {
                break;
            }
            deepened.push(PreferredAdmission {
                variable: source.variables[group.start].variable.index(),
                raised_owners: groups
                    .rows
                    .iter()
                    .filter(|rows| {
                        !rows.is_empty() && next_equations[rows.start] > equations[rows.start]
                    })
                    .map(|rows| (rows.start, equations[rows.start]))
                    .collect(),
            });
            (*equations, *variables) = (next_equations, next_variables);
        }
    }
    Ok(deepened)
}

/// The column ranges of each source declaration, in canonical order.
fn declaration_column_groups(source: &DifferentialStructure<'_>) -> Vec<Range<usize>> {
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
    column_groups
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
    invariant_columns: &[bool],
) -> Result<Option<OffsetVectors>, &'static str> {
    let (mut equations, mut variables) = (initial.0.to_vec(), initial.1.to_vec());
    if rows.len() != matching.len()
        || rows.len() != equations.len()
        || rows.len() != variables.len()
        || rows.len() != invariant_columns.len()
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
        changed |= relax_variable_orders(&equations, &mut variables, rows, invariant_columns)?;
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

/// Refine offsets for an assignment that carries no invariant columns.
#[cfg(test)]
fn refine_scalar(
    rows: &[Vec<SignatureEntry>],
    matching: &[usize],
    initial: (&[u32], &[u32]),
    groups: OwnerGroups<'_>,
) -> Result<Option<OffsetVectors>, &'static str> {
    refine(rows, matching, initial, groups, &vec![false; rows.len()])
}

/// Raise each variable order to the largest a row demands, reporting whether any
/// order changed. A parameter-constant column stays at order zero: its
/// derivatives are the zero constant, so a differentiated row never raises it.
fn relax_variable_orders(
    equations: &[u32],
    variables: &mut [u32],
    rows: &[Vec<SignatureEntry>],
    invariant_columns: &[bool],
) -> Result<bool, &'static str> {
    let mut changed = false;
    for (row, entries) in rows.iter().enumerate() {
        for entry in entries {
            if invariant_columns
                .get(entry.column)
                .copied()
                .unwrap_or(false)
            {
                continue;
            }
            let value = equations[row]
                .checked_add(entry.order)
                .ok_or("tensor differential order overflow")?;
            let target = variables
                .get_mut(entry.column)
                .ok_or("tensor differential signature column is out of range")?;
            changed |= raise(target, value);
        }
    }
    Ok(changed)
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
