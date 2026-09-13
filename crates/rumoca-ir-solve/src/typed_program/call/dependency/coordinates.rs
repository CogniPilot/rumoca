//! Compact affine relations between output coordinates and input coordinates.

use rumoca_core::AffineForm;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub(super) struct Coordinates {
    output_rank: usize,
    free_dimensions: Box<[u32]>,
    subscripts: Box<[AffineForm]>,
}

impl Coordinates {
    pub(super) fn identity(rank: usize) -> Self {
        Self::access(
            rank,
            &[],
            (0..rank)
                .map(|axis| AffineForm::unit_binder(axis, rank))
                .collect(),
        )
    }

    pub(super) fn access(
        output_rank: usize,
        free_dimensions: &[u32],
        subscripts: Vec<AffineForm>,
    ) -> Self {
        Self {
            output_rank,
            free_dimensions: free_dimensions.into(),
            subscripts: subscripts.into(),
        }
    }

    /// Substitute an operand access into a dependency on that operand.
    pub(super) fn compose(&self, access: &Self) -> Option<Self> {
        if self.output_rank != access.subscripts.len() {
            return None;
        }
        let free_dimensions: Box<[_]> = access
            .free_dimensions
            .iter()
            .chain(&self.free_dimensions)
            .copied()
            .collect();
        let count = access.output_rank.checked_add(free_dimensions.len())?;
        let subscripts = self
            .subscripts
            .iter()
            .map(|source| {
                let mut result = AffineForm::constant(source.constant, count);
                for (coefficient, substitution) in source
                    .coeffs
                    .iter()
                    .take(self.output_rank)
                    .zip(&access.subscripts)
                {
                    let mut extended = substitution.clone();
                    extended.coeffs.resize(count, 0);
                    result = result.checked_add(&extended.checked_scale(*coefficient)?)?;
                }
                let free_start = access.output_rank + access.free_dimensions.len();
                for (index, coefficient) in source.coeffs.iter().skip(self.output_rank).enumerate()
                {
                    result.coeffs[free_start + index] = *coefficient;
                }
                Some(result)
            })
            .collect::<Option<Vec<_>>>()?;
        Some(Self::access(access.output_rank, &free_dimensions, subscripts).without_unused_axes())
    }

    fn without_unused_axes(mut self) -> Self {
        for axis in (0..self.free_dimensions.len()).rev() {
            let coefficient = self.output_rank + axis;
            if self
                .subscripts
                .iter()
                .all(|subscript| subscript.coeffs[coefficient] == 0)
            {
                self = self.without_free_axis(axis);
            }
        }
        self
    }

    fn without_free_axis(mut self, axis: usize) -> Self {
        let mut dimensions = self.free_dimensions.into_vec();
        dimensions.remove(axis);
        self.free_dimensions = dimensions.into();
        for subscript in &mut self.subscripts {
            subscript.coeffs.remove(self.output_rank + axis);
        }
        self
    }

    /// Enumerated only by the scalar dependency view, never while issuing a call.
    pub(super) fn input_elements(
        &self,
        output_dimensions: &[u32],
        output_element: usize,
        input_dimensions: &[u32],
    ) -> Option<Vec<usize>> {
        if output_dimensions.len() != self.output_rank
            || input_dimensions.len() != self.subscripts.len()
        {
            return None;
        }
        let output = coordinates(output_dimensions, output_element)?;
        let free_count = self
            .free_dimensions
            .iter()
            .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))?;
        (0..free_count)
            .map(|index| {
                let mut point = output.clone();
                point.extend(coordinates(&self.free_dimensions, index)?);
                flatten_subscripts(&self.subscripts, &point, input_dimensions)
            })
            .collect()
    }
}

fn coordinates(dimensions: &[u32], mut element: usize) -> Option<Vec<i64>> {
    let mut point = vec![0; dimensions.len()];
    for (coordinate, &extent) in point.iter_mut().zip(dimensions).rev() {
        if extent == 0 {
            return None;
        }
        *coordinate = i64::try_from(element % extent as usize).ok()?;
        element /= extent as usize;
    }
    (element == 0).then_some(point)
}

fn flatten_subscripts(
    subscripts: &[AffineForm],
    point: &[i64],
    dimensions: &[u32],
) -> Option<usize> {
    subscripts
        .iter()
        .zip(dimensions)
        .try_fold(0usize, |flat, (form, &extent)| {
            if form.coeffs.len() != point.len() {
                return None;
            }
            let coordinate = form
                .coeffs
                .iter()
                .zip(point)
                .try_fold(form.constant, |sum, (coefficient, value)| {
                    sum.checked_add(coefficient.checked_mul(*value)?)
                })?;
            let coordinate = usize::try_from(coordinate).ok()?;
            if coordinate >= extent as usize {
                return None;
            }
            flat.checked_mul(extent as usize)?.checked_add(coordinate)
        })
}
