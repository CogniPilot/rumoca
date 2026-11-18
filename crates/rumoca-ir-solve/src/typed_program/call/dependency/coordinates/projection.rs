//! Prune guarded finite boxes without enumerating an entire tensor per output.
use super::*;
use std::ops::Range;

impl Coordinates {
    pub(super) fn guarded_input_elements(
        &self,
        output_dimensions: &[u32],
        output_element: usize,
        input_dimensions: &[u32],
    ) -> Option<Vec<usize>> {
        let mut point = coordinates(output_dimensions, output_element)?;
        if self.free_dimensions.is_empty() {
            return if self.guards_match(&point)? {
                Some(vec![flatten_subscripts(
                    &self.subscripts,
                    &point,
                    input_dimensions,
                )?])
            } else {
                Some(Vec::new())
            };
        }
        let first = self.axis_range(&point, 0)?;
        if first.is_empty() {
            return Some(Vec::new());
        }
        let mut ranges = vec![first];
        let mut elements = Vec::new();
        // Explicit stack: tensor rank does not become Rust recursion depth.
        while let Some(range) = ranges.last_mut() {
            let value = range.next();
            let axis = ranges.len() - 1;
            let Some(value) = value else {
                ranges.pop();
                continue;
            };
            point.truncate(self.output_rank + axis);
            point.push(value);
            if axis + 1 != self.free_dimensions.len() {
                ranges.push(self.axis_range(&point, axis + 1)?);
                continue;
            }
            if self.guards_match(&point)? {
                elements.push(flatten_subscripts(
                    &self.subscripts,
                    &point,
                    input_dimensions,
                )?);
            }
        }
        Some(elements)
    }

    fn axis_range(&self, point: &[i64], axis: usize) -> Option<Range<i64>> {
        let extent = i64::from(*self.free_dimensions.get(axis)?);
        if extent == 0 {
            return Some(0..0);
        }
        let mut low = 0i128;
        let mut high = i128::from(extent - 1);
        for guard in &self.guards {
            let bounds = match small_guard_axis_bounds(guard, point, axis, self) {
                Some(bounds) => bounds.map(|(low, high)| (i128::from(low), i128::from(high))),
                None => {
                    guard_axis_bounds(guard, point, axis, self.output_rank, &self.free_dimensions)?
                }
            };
            let Some((minimum, maximum)) = bounds else {
                continue;
            };
            low = low.max(minimum);
            high = high.min(maximum);
            if low > high {
                return Some(0..0);
            }
        }
        Some(i64::try_from(low).ok()?..i64::try_from(high.checked_add(1)?).ok()?)
    }
}

/// [`guard_axis_bounds`] in 64-bit arithmetic, or `None` when that is not
/// proved exact: each product takes a 32-bit coefficient and a 32-bit
/// coordinate or extent, so it cannot overflow, and every sum is checked.
/// Whatever it answers the 128-bit rule answers too; targets that emulate
/// 128-bit arithmetic in software otherwise spend the projection there.
fn small_guard_axis_bounds(
    guard: &Guard,
    point: &[i64],
    axis: usize,
    relation: &Coordinates,
) -> Option<Option<(i64, i64)>> {
    let (output_rank, free) = (relation.output_rank, &relation.free_dimensions);
    if guard.form.coeffs.len() != output_rank.checked_add(free.len())?
        || point.len() != output_rank.checked_add(axis)?
    {
        return None;
    }
    let base = guard
        .form
        .coeffs
        .iter()
        .zip(point)
        .try_fold(guard.form.constant, |sum, (c, x)| {
            sum.checked_add(small_product(*c, *x)?)
        })?;
    let mut minimum = 0i64;
    let mut maximum = 0i64;
    for (offset, &extent) in free.iter().enumerate().skip(axis + 1) {
        if extent == 0 {
            return Some(Some((1, 0)));
        }
        let value = small_product(
            guard.form.coeffs[output_rank + offset],
            i64::from(extent - 1),
        )?;
        minimum = minimum.checked_add(value.min(0))?;
        maximum = maximum.checked_add(value.max(0))?;
    }
    let coefficient = guard.form.coeffs[output_rank + axis];
    if coefficient == 0 {
        return Some(
            (base.checked_add(maximum)? < guard.lower || base.checked_add(minimum)? > guard.upper)
                .then_some((1, 0)),
        );
    }
    let (low, high, divisor) = if coefficient > 0 {
        (
            guard.lower.checked_sub(base)?.checked_sub(maximum)?,
            guard.upper.checked_sub(base)?.checked_sub(minimum)?,
            coefficient,
        )
    } else {
        (
            base.checked_add(minimum)?.checked_sub(guard.upper)?,
            base.checked_add(maximum)?.checked_sub(guard.lower)?,
            coefficient.checked_neg()?,
        )
    };
    let floor = low.checked_div_euclid(divisor)?;
    let ceil = floor.checked_add(i64::from(low.checked_rem_euclid(divisor)? != 0))?;
    Some(Some((ceil, high.checked_div_euclid(divisor)?)))
}

/// `coefficient * value` when both are small enough that it cannot overflow:
/// a 32-bit signed factor times a 32-bit unsigned one stays inside 64 bits,
/// so the wrapping product is the exact one and needs no widened check.
fn small_product(coefficient: i64, value: i64) -> Option<i64> {
    (i32::try_from(coefficient).is_ok() && u32::try_from(value).is_ok())
        .then(|| coefficient.wrapping_mul(value))
}

fn guard_axis_bounds(
    guard: &Guard,
    point: &[i64],
    axis: usize,
    output_rank: usize,
    free: &[u32],
) -> Option<Option<(i128, i128)>> {
    if guard.form.coeffs.len() != output_rank.checked_add(free.len())?
        || point.len() != output_rank.checked_add(axis)?
    {
        return None;
    }
    let base = guard
        .form
        .coeffs
        .iter()
        .zip(point)
        .try_fold(i128::from(guard.form.constant), |sum, (c, x)| {
            sum.checked_add(i128::from(*c).checked_mul(i128::from(*x))?)
        })?;
    let mut minimum = 0i128;
    let mut maximum = 0i128;
    for (offset, &extent) in free.iter().enumerate().skip(axis + 1) {
        if extent == 0 {
            return Some(Some((1, 0)));
        }
        let value = i128::from(guard.form.coeffs[output_rank + offset])
            .checked_mul(i128::from(extent - 1))?;
        minimum = minimum.checked_add(value.min(0))?;
        maximum = maximum.checked_add(value.max(0))?;
    }
    let coefficient = i128::from(guard.form.coeffs[output_rank + axis]);
    let lower = i128::from(guard.lower);
    let upper = i128::from(guard.upper);
    if coefficient == 0 {
        return Some(
            (base.checked_add(maximum)? < lower || base.checked_add(minimum)? > upper)
                .then_some((1, 0)),
        );
    }
    let (low, high, divisor) = if coefficient > 0 {
        (
            lower.checked_sub(base)?.checked_sub(maximum)?,
            upper.checked_sub(base)?.checked_sub(minimum)?,
            coefficient,
        )
    } else {
        (
            base.checked_add(minimum)?.checked_sub(upper)?,
            base.checked_add(maximum)?.checked_sub(lower)?,
            coefficient.checked_neg()?,
        )
    };
    let floor = low.checked_div_euclid(divisor)?;
    let ceil = floor.checked_add(i128::from(low.checked_rem_euclid(divisor)? != 0))?;
    Some(Some((ceil, high.checked_div_euclid(divisor)?)))
}

#[cfg(test)]
mod tests;
