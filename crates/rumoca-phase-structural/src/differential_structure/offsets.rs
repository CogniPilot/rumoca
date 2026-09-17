//! Least offsets and a separate primal/dual certificate check.

use super::SignatureEntry;

/// The pure structural least offsets, with no external per-column obligation.
/// Exercises the difference-constraint solver directly; the production analysis
/// uses [`least_offsets_with_lower_bounds`] to carry the state-derivative bound.
#[cfg(test)]
pub(super) fn least_offsets(
    rows: &[Vec<SignatureEntry>],
    matching: &[usize],
) -> Result<(Vec<u32>, Vec<u32>), &'static str> {
    least_offsets_with_lower_bounds(rows, matching, &vec![0_u32; rows.len()])
}

/// The least offsets of [`least_offsets`], refined by a per-column lower bound on
/// the variable offset.
///
/// A designated differential state owns an integration slot, so its first
/// derivative must exist in the prolonged system even when that derivative never
/// appears in a continuous equation (its only occurrence is an initial-equation
/// constraint on the derivative's value). Seeding that column's offset at one
/// forces the equation defining the state to be differentiated, supplying the
/// derivative through the ordinary prolongation. The bound is non-binding for a
/// state whose derivative already appears continuously: that column reaches at
/// least one from the signature alone, so the refined offsets equal the pure
/// least offsets for every well-posed index-one and holonomic system.
pub(super) fn least_offsets_with_lower_bounds(
    rows: &[Vec<SignatureEntry>],
    matching: &[usize],
    variable_lower_bounds: &[u32],
) -> Result<(Vec<u32>, Vec<u32>), &'static str> {
    if matching.len() != rows.len() {
        return Err("differential matching has the wrong row count");
    }
    if variable_lower_bounds.len() != rows.len() {
        return Err("differential lower bounds have the wrong column count");
    }
    let mut equations = vec![0_u32; rows.len()];
    let mut variables = vec![0_u32; rows.len()];
    // Each iteration relaxes a matched-row difference-constraint graph. An
    // optimal assignment has no positive cycle; a longest simple path uses
    // fewer than n edges. The final iteration verifies the fixed point.
    for _ in 0..=rows.len() {
        variables.copy_from_slice(variable_lower_bounds);
        for (row, entries) in rows.iter().enumerate() {
            for entry in entries {
                let value = equations[row]
                    .checked_add(entry.order)
                    .ok_or("differential order overflow")?;
                let target = variables
                    .get_mut(entry.column)
                    .ok_or("signature column is out of range")?;
                *target = (*target).max(value);
            }
        }
        let mut changed = false;
        for (row, &column) in matching.iter().enumerate() {
            let entry = rows[row]
                .iter()
                .find(|entry| entry.column == column)
                .ok_or("differential matching claims an absent edge")?;
            let order = variables[column]
                .checked_sub(entry.order)
                .ok_or("negative differential equation order")?;
            if order < equations[row] {
                return Err("differential offset iteration is not monotone");
            }
            changed |= order != equations[row];
            equations[row] = order;
        }
        if !changed {
            return Ok((equations, variables));
        }
    }
    Err("differential offsets exceed the simple-path bound")
}

pub(super) fn certify(
    rows: &[Vec<SignatureEntry>],
    matching: &[usize],
    equations: &[u32],
    variables: &[u32],
) -> Option<usize> {
    let count = rows.len();
    if [matching.len(), equations.len(), variables.len()]
        .iter()
        .any(|&len| len != count)
    {
        return None;
    }
    let mut seen = vec![false; count];
    let mut objective = 0_usize;
    for (row, entries) in rows.iter().enumerate() {
        let column = matching[row];
        if std::mem::replace(seen.get_mut(column)?, true) {
            return None;
        }
        let matched = entries.iter().find(|entry| entry.column == column)?;
        if variables[column].checked_sub(equations[row])? != matched.order {
            return None;
        }
        objective = objective.checked_add(usize::try_from(matched.order).ok()?)?;
        for entry in entries {
            if variables.get(entry.column)?.checked_sub(equations[row])? < entry.order {
                return None;
            }
        }
    }
    let sum = |values: &[u32]| {
        values.iter().try_fold(0_usize, |total, &value| {
            total.checked_add(usize::try_from(value).ok()?)
        })
    };
    (sum(variables)?.checked_sub(sum(equations)?)? == objective).then_some(objective)
}
