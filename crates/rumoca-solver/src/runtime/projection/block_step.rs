//! Saving and restoring a block's coordinates around a trial step, and the
//! step-size rules of its backtracking line search.

use super::scaled_tolerance;

/// The values of `indices` in `y`; an index outside `y` holds NaN and is never
/// restored.
pub(super) fn block_values(y: &[f64], indices: &[usize]) -> Vec<f64> {
    indices
        .iter()
        .map(|&index| y.get(index).copied().unwrap_or(f64::NAN))
        .collect()
}

/// Restore the values [`block_values`] saved.
pub(super) fn restore_block_values(y: &mut [f64], indices: &[usize], values: &[f64]) {
    for (&index, &value) in indices.iter().zip(values) {
        if let Some(slot) = y.get_mut(index) {
            *slot = value;
        }
    }
}

/// Whether `y` differs from the values [`block_values`] saved.
pub(super) fn block_values_changed(y: &[f64], indices: &[usize], values: &[f64]) -> bool {
    for (&index, &value) in indices.iter().zip(values) {
        if y.get(index) != Some(&value) {
            return true;
        }
    }
    false
}

pub(super) fn algebraic_step_at_resolution(current: f64, candidate: f64) -> bool {
    candidate == current || candidate == current.next_up() || candidate == current.next_down()
}

/// Return the next halved step while at least one correction remains larger
/// than its variable's declared solver accuracy.
///
/// Once every correction is within its scaled tolerance, every later halving is
/// smaller still. The block cannot make a solution-significant state update on
/// this direction, so the caller returns it unsettled to the existing retry or
/// typed failure path instead of searching down to floating-point underflow.
pub(super) fn next_scaled_backtrack(
    current_alpha: f64,
    delta: &[f64],
    variable_scales: &[f64],
    tolerance: f64,
) -> Option<f64> {
    let next_alpha = current_alpha * 0.5;
    if !(next_alpha > 0.0 && next_alpha < current_alpha) || delta.len() != variable_scales.len() {
        return None;
    }
    delta
        .iter()
        .copied()
        .zip(variable_scales.iter().copied())
        .any(|(correction, scale)| {
            (next_alpha * correction).abs() > scaled_tolerance(tolerance, scale)
        })
        .then_some(next_alpha)
}
