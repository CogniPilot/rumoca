//! Checked affine restrictions on the coordinate relation's finite box.
use super::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub(super) struct Guard {
    pub(super) form: AffineForm,
    pub(super) lower: i64,
    pub(super) upper: i64,
}

impl Coordinates {
    pub(in super::super) fn restricted(mut self, form: AffineForm, lower: i64, upper: i64) -> Self {
        let guard = Guard { form, lower, upper };
        let constant = self.output_rank.checked_add(self.free_dimensions.len())
            == Some(guard.form.coeffs.len())
            && guard.form.coeffs.iter().all(|&c| c == 0);
        if !(self.guards.contains(&guard)
            || constant && (lower..=upper).contains(&guard.form.constant))
        {
            let mut guards = self.guards.into_vec();
            guards.push(guard);
            self.guards = guards.into();
        }
        self
    }

    pub(in super::super) fn is_empty(&self) -> bool {
        self.guards.iter().any(|guard| {
            guard.lower > guard.upper
                || (guard.form.coeffs.iter().all(|&c| c == 0)
                    && !(guard.lower..=guard.upper).contains(&guard.form.constant))
        })
    }

    pub(super) fn check_guard_domain(&self, dimensions: &[u32]) -> Option<()> {
        for guard in &self.guards {
            if guard.form.coeffs.len() != dimensions.len() {
                return None;
            }
            super::checked_prefix_bounds(&guard.form, dimensions)?;
        }
        Some(())
    }

    pub(super) fn guards_match(&self, point: &[i64]) -> Option<bool> {
        for guard in &self.guards {
            if guard.form.coeffs.len() != point.len() {
                return None;
            }
            let value = guard
                .form
                .coeffs
                .iter()
                .zip(point)
                .try_fold(guard.form.constant, |sum, (c, x)| {
                    sum.checked_add(c.checked_mul(*x)?)
                })?;
            if !(guard.lower..=guard.upper).contains(&value) {
                return Some(false);
            }
        }
        Some(true)
    }
}

#[cfg(test)]
mod tests;
