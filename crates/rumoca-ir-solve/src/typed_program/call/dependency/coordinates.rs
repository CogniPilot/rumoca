//! Compact affine relations between output coordinates and input coordinates.

use rumoca_core::AffineForm;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub(super) struct Coordinates {
    output_rank: usize,
    free_dimensions: Box<[u32]>,
    subscripts: Box<[AffineForm]>,
}

impl Coordinates {
    pub(super) fn is_identity(&self, dimensions: &[u32]) -> bool {
        self.free_dimensions.is_empty() && *self == Self::identity(dimensions.len())
    }

    /// Proves every existing scalar mapping check over its complete box.
    pub(super) fn complete_domain(
        &self,
        output_dimensions: &[u32],
        input_dimensions: &[u32],
    ) -> Option<()> {
        if output_dimensions.len() != self.output_rank
            || input_dimensions.len() != self.subscripts.len()
        {
            return None;
        }
        let free_count = self
            .free_dimensions
            .iter()
            .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))?;
        if free_count == 0 {
            return Some(());
        }
        let dimensions: Vec<_> = output_dimensions
            .iter()
            .chain(&self.free_dimensions)
            .copied()
            .collect();
        for (form, &extent) in self.subscripts.iter().zip(input_dimensions) {
            if form.coeffs.len() != dimensions.len() {
                return None;
            }
            let (minimum, maximum) = checked_prefix_bounds(form, &dimensions)?;
            if minimum < 0 || maximum >= i64::from(extent) {
                return None;
            }
        }
        // flatten_subscripts uses checked row-major arithmetic in this order.
        input_dimensions
            .iter()
            .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))?;
        Some(())
    }

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

fn checked_prefix_bounds(form: &AffineForm, dimensions: &[u32]) -> Option<(i64, i64)> {
    let mut minimum = form.constant;
    let mut maximum = form.constant;
    for (&coefficient, &extent) in form.coeffs.iter().zip(dimensions) {
        let last = i64::from(extent.checked_sub(1)?);
        let product = coefficient.checked_mul(last)?;
        minimum = minimum.checked_add(product.min(0))?;
        maximum = maximum.checked_add(product.max(0))?;
    }
    Some((minimum, maximum))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn complete_identity_domain_is_compact_and_matches_scalar_mapping() {
        let coordinates = Coordinates::identity(1);
        assert_eq!(
            coordinates.complete_domain(&[4_194_304], &[4_194_304]),
            Some(())
        );
        assert!(coordinates.is_identity(&[4_194_304]));
        for element in [0, 2_097_152, 4_194_303] {
            assert_eq!(
                coordinates.input_elements(&[4_194_304], element, &[4_194_304]),
                Some(vec![element])
            );
        }
    }

    #[test]
    fn certificate_checks_hidden_last_coordinate_and_each_overflow_prefix() {
        let shifted = Coordinates::access(
            1,
            &[],
            vec![AffineForm {
                constant: 1,
                coeffs: vec![1],
            }],
        );
        assert_eq!(shifted.input_elements(&[4], 0, &[4]), Some(vec![1]));
        assert_eq!(shifted.input_elements(&[4], 3, &[4]), None);
        assert_eq!(shifted.complete_domain(&[4], &[4]), None);
        let multiply = Coordinates::access(
            1,
            &[],
            vec![AffineForm {
                constant: 0,
                coeffs: vec![i64::MAX],
            }],
        );
        assert_eq!(multiply.complete_domain(&[3], &[u32::MAX]), None);
        let prefix = Coordinates::access(
            2,
            &[],
            vec![AffineForm {
                constant: i64::MAX,
                coeffs: vec![1, -1],
            }],
        );
        assert_eq!(prefix.complete_domain(&[2, 2], &[u32::MAX]), None);
    }

    #[test]
    fn complete_free_box_retains_existing_scalar_order_and_empty_free_policy() {
        let coordinates = Coordinates::access(
            1,
            &[3],
            vec![AffineForm {
                constant: 0,
                coeffs: vec![3, 1],
            }],
        );
        assert_eq!(coordinates.complete_domain(&[2], &[6]), Some(()));
        assert_eq!(
            coordinates.input_elements(&[2], 1, &[6]),
            Some(vec![3, 4, 5])
        );
        let empty = Coordinates::access(
            1,
            &[0],
            vec![AffineForm {
                constant: -99,
                coeffs: vec![],
            }],
        );
        assert_eq!(empty.complete_domain(&[2], &[6]), Some(()));
        assert_eq!(empty.input_elements(&[2], 1, &[6]), Some(vec![]));
    }
}
