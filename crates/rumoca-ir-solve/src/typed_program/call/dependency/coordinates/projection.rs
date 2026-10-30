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
        let mut ranges = vec![self.axis_range(&point, 0)?];
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
            let bounds =
                guard_axis_bounds(guard, point, axis, self.output_rank, &self.free_dimensions)?;
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
