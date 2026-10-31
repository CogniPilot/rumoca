//! Compact affine relations between output coordinates and input coordinates.

use rumoca_core::AffineForm;
use serde::{Deserialize, Serialize};

mod guards;
mod projection;
use guards::Guard;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub(super) struct Coordinates {
    output_rank: usize,
    free_dimensions: Box<[u32]>,
    subscripts: Box<[AffineForm]>,
    guards: Box<[Guard]>,
}

/// Free axes plus guards a may-depend relation may carry. Composing an
/// access appends both, so a chain of static element updates or nested
/// region captures grows every relation it passes through; past this bound
/// the dependency is stated over the whole input instead, which only adds
/// dependencies. Exact value projections decline instead of widening.
pub(super) const RELATION_TERM_BUDGET: usize = 32;

/// Affine coefficients one composed relation may hold: every subscript and
/// guard is a form over the output axes plus the free axes, so this bounds
/// a relation's size relative to the ranks involved as well as its terms.
pub(super) const RELATION_COEFFICIENT_BUDGET: usize = 4096;

impl Coordinates {
    /// The terms composition accumulates: free axes and guards.
    pub(super) fn terms(&self) -> usize {
        self.free_dimensions.len() + self.guards.len()
    }

    /// Whether `self.compose(access)` could exceed either budget, decided
    /// from both operands before anything is allocated. Composition keeps
    /// `self`'s subscripts, collects both guard lists, and forms them over
    /// `access`'s output axes plus both operands' free axes.
    pub(super) fn composition_exceeds_budget(&self, access: &Self) -> bool {
        let terms = self.terms() + access.terms();
        let forms = self.subscripts.len() + self.guards.len() + access.guards.len();
        let width = access.output_rank + access.free_dimensions.len() + self.free_dimensions.len();
        terms > RELATION_TERM_BUDGET || forms.saturating_mul(width) > RELATION_COEFFICIENT_BUDGET
    }

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
        self.check_guard_domain(&dimensions)?;
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
            guards: Box::default(),
        }
    }

    /// Substitute an operand access into a dependency on that operand.
    pub(super) fn compose(&self, access: &Self) -> Option<Self> {
        if self.output_rank != access.subscripts.len()
            || !self.valid_arity()
            || !access.valid_arity()
        {
            return None;
        }
        let free_dimensions: Box<[_]> = access
            .free_dimensions
            .iter()
            .chain(&self.free_dimensions)
            .copied()
            .collect();
        let count = access.output_rank.checked_add(free_dimensions.len())?;
        let substitute = |source: &AffineForm| {
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
            for (index, coefficient) in source.coeffs.iter().skip(self.output_rank).enumerate() {
                *result.coeffs.get_mut(free_start + index)? = *coefficient;
            }
            Some(result)
        };
        let subscripts = self
            .subscripts
            .iter()
            .map(&substitute)
            .collect::<Option<Vec<_>>>()?;
        let mut result = Self::access(access.output_rank, &free_dimensions, subscripts);
        for guard in &access.guards {
            let mut form = guard.form.clone();
            form.coeffs.resize(count, 0);
            result = result.restricted(form, guard.lower, guard.upper);
        }
        for guard in &self.guards {
            result = result.restricted(substitute(&guard.form)?, guard.lower, guard.upper);
        }
        Some(result.without_unused_axes())
    }

    fn valid_arity(&self) -> bool {
        let Some(count) = self.output_rank.checked_add(self.free_dimensions.len()) else {
            return false;
        };
        self.subscripts
            .iter()
            .all(|form| form.coeffs.len() == count)
            && self
                .guards
                .iter()
                .all(|guard| guard.form.coeffs.len() == count)
    }

    /// Drop free axes that no subscript reads. An axis named only by its own
    /// one-axis guards is existential: it decides whether the relation holds,
    /// not which input coordinate it reaches, so it is projected out exactly
    /// (or the relation is empty). Composed call paths otherwise each keep a
    /// private copy of such axes and equal relations never deduplicate.
    fn without_unused_axes(mut self) -> Self {
        for axis in (0..self.free_dimensions.len()).rev() {
            let coefficient = self.output_rank + axis;
            if self
                .subscripts
                .iter()
                .any(|subscript| subscript.coeffs[coefficient] != 0)
            {
                continue;
            }
            let mentions = |guard: &Guard| guard.form.coeffs[coefficient] != 0;
            if !self.guards.iter().any(mentions) {
                self = self.without_free_axis(axis);
                continue;
            }
            let Some(feasible) = self.existential_axis_feasible(axis) else {
                continue;
            };
            if !feasible {
                return self.empty();
            }
            let mut guards = self.guards.into_vec();
            guards.retain(|guard| !mentions(guard));
            self.guards = guards.into();
            self = self.without_free_axis(axis);
        }
        self
    }

    /// `None` when a guard on `axis` also reads another axis, so projection
    /// would not be exact; otherwise whether some value in the axis box
    /// satisfies every one-axis guard on it.
    fn existential_axis_feasible(&self, axis: usize) -> Option<bool> {
        let coefficient = self.output_rank + axis;
        let mut low = 0i128;
        let mut high = i128::from(*self.free_dimensions.get(axis)?) - 1;
        for guard in &self.guards {
            let c = i128::from(guard.form.coeffs[coefficient]);
            if c == 0 {
                continue;
            }
            if guard
                .form
                .coeffs
                .iter()
                .enumerate()
                .any(|(index, &other)| index != coefficient && other != 0)
            {
                return None;
            }
            let k = i128::from(guard.form.constant);
            let (lower, upper) = (i128::from(guard.lower), i128::from(guard.upper));
            // c * x + k in [lower, upper], solved for integer x.
            let (from, to, divisor) = if c > 0 {
                (lower - k, upper - k, c)
            } else {
                (k - upper, k - lower, -c)
            };
            low = low.max(-(-from).div_euclid(divisor));
            high = high.min(to.div_euclid(divisor));
        }
        Some(low <= high)
    }

    fn empty(self) -> Self {
        let rank = self.output_rank;
        Self {
            output_rank: rank,
            free_dimensions: Box::default(),
            subscripts: self
                .subscripts
                .iter()
                .map(|_| AffineForm::constant(0, rank))
                .collect(),
            guards: Box::new([Guard {
                form: AffineForm::constant(0, rank),
                lower: 1,
                upper: 0,
            }]),
        }
    }

    fn without_free_axis(mut self, axis: usize) -> Self {
        let mut dimensions = self.free_dimensions.into_vec();
        dimensions.remove(axis);
        self.free_dimensions = dimensions.into();
        for subscript in &mut self.subscripts {
            subscript.coeffs.remove(self.output_rank + axis);
        }
        for guard in &mut self.guards {
            guard.form.coeffs.remove(self.output_rank + axis);
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
        if !self.guards.is_empty() {
            return self.guarded_input_elements(
                output_dimensions,
                output_element,
                input_dimensions,
            );
        }
        let output = coordinates(output_dimensions, output_element)?;
        let free_count = self
            .free_dimensions
            .iter()
            .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))?;
        let mut elements = Vec::new();
        for index in 0..free_count {
            let mut point = output.clone();
            point.extend(coordinates(&self.free_dimensions, index)?);
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

/// A relation of `output_rank` output axes and `free` free axes (extent 2)
/// onto `input_rank` input axes, carrying `guards` distinct guards, so that
/// `terms()` is `free + guards`. `seed` is every subscript's constant, which
/// keeps relations built with different seeds distinct.
#[cfg(test)]
impl Coordinates {
    pub(super) fn with_terms(
        seed: i64,
        output_rank: usize,
        input_rank: usize,
        free: usize,
        guards: usize,
    ) -> Self {
        let width = output_rank + free;
        assert!(
            width > 0 || guards == 0,
            "a guard needs an axis to stay a guard"
        );
        let mut coordinates = Self::access(
            output_rank,
            &vec![2; free],
            (0..input_rank)
                .map(|_| AffineForm::constant(seed, width))
                .collect(),
        );
        for guard in 0..guards {
            let mut form = AffineForm::constant(i64::try_from(guard).unwrap(), width);
            form.coeffs[0] = 1;
            coordinates = coordinates.restricted(form, i64::MIN, i64::MAX);
        }
        assert_eq!(coordinates.terms(), free + guards);
        coordinates
    }

    /// Affine coefficients the relation holds.
    fn coefficients(&self) -> usize {
        (self.subscripts.len() + self.guards.len())
            * (self.output_rank + self.free_dimensions.len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_term_budget_admits_thirty_two_terms_and_refuses_thirty_three() {
        let access = Coordinates::with_terms(0, 1, 1, 0, 0);
        for (guards, exceeds) in [(31, false), (32, false), (33, true)] {
            let relation = Coordinates::with_terms(0, 1, 1, 0, guards);
            assert_eq!(
                relation.composition_exceeds_budget(&access),
                exceeds,
                "{guards} terms"
            );
        }
        // Free axes count as terms, and both operands' terms add up.
        let relation = Coordinates::with_terms(0, 1, 1, 16, 0);
        for (guards, exceeds) in [(16, false), (17, true)] {
            let access = Coordinates::with_terms(0, 1, 1, 0, guards);
            assert_eq!(
                relation.composition_exceeds_budget(&access),
                exceeds,
                "16 free + {guards} guards"
            );
        }
    }

    #[test]
    fn the_coefficient_budget_admits_4096_and_refuses_4097() {
        // No terms: forms (the relation's subscripts) times width (the
        // access's output rank) alone decide.
        for (forms, width, exceeds) in [(63, 65, false), (64, 64, false), (17, 241, true)] {
            let relation = Coordinates::with_terms(0, 1, forms, 0, 0);
            let access = Coordinates::with_terms(0, width, 1, 0, 0);
            assert_eq!(
                relation.composition_exceeds_budget(&access),
                exceeds,
                "{forms} forms x width {width} = {}",
                forms * width
            );
        }
    }

    #[test]
    fn an_admitted_composition_stays_within_the_bound_it_was_admitted_under() {
        // The predicate is decided before composing; the composed relation
        // must not hold more terms or coefficients than it predicted.
        let relation = Coordinates::with_terms(3, 2, 3, 4, 5);
        let access = Coordinates::with_terms(7, 6, 2, 7, 8);
        assert!(!relation.composition_exceeds_budget(&access));
        let composed = relation.compose(&access).expect("the ranks agree");
        assert!(composed.terms() <= relation.terms() + access.terms());
        let forms = relation.subscripts.len() + relation.guards.len() + access.guards.len();
        let width =
            access.output_rank + access.free_dimensions.len() + relation.free_dimensions.len();
        assert!(composed.coefficients() <= forms * width);
    }

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

    fn form(constant: i64, coeffs: &[i64]) -> AffineForm {
        AffineForm {
            constant,
            coeffs: coeffs.to_vec(),
        }
    }

    /// The kite trace shape: an input coordinate reached under guards on
    /// free axes that no subscript reads.
    fn existential(guard_constant: i64) -> Coordinates {
        Coordinates::access(1, &[19, 3, 3], vec![form(3, &[0, 0, 0, 1])])
            .restricted(form(guard_constant, &[0, 0, 1, 0]), 0, 0)
            .restricted(form(0, &[0, 1, 0, 0]), 0, 5)
    }

    #[test]
    fn existential_axes_project_out_with_identical_scalar_mapping() {
        for guard_constant in [0, -1, -2] {
            let raw = existential(guard_constant);
            let projected = raw.clone().without_unused_axes();
            assert_eq!(
                projected,
                Coordinates::access(1, &[3], vec![form(3, &[0, 1])])
            );
            // Dependencies are sets: the raw form repeats each input once per
            // value of the existential axis, which is the multiplicity the
            // projection removes.
            for element in 0..4 {
                let mut expected = raw.input_elements(&[4], element, &[8]).unwrap();
                expected.sort_unstable();
                expected.dedup();
                assert_eq!(
                    projected.input_elements(&[4], element, &[8]),
                    Some(expected)
                );
            }
        }
        assert_eq!(
            existential(0).without_unused_axes(),
            existential(-1).without_unused_axes()
        );
    }

    #[test]
    fn infeasible_existential_axis_empties_the_relation() {
        let raw = existential(-3);
        assert_eq!(raw.input_elements(&[4], 0, &[8]), Some(vec![]));
        assert!(raw.without_unused_axes().is_empty());
        let negative = Coordinates::access(1, &[3], vec![form(2, &[0, 0])]).restricted(
            form(1, &[0, -2]),
            -4,
            -2,
        );
        assert_eq!(
            negative.clone().without_unused_axes(),
            Coordinates::access(1, &[], vec![form(2, &[0])])
        );
        let none =
            Coordinates::access(1, &[3], vec![form(2, &[0, 0])]).restricted(form(0, &[0, 2]), 1, 1);
        assert!(none.without_unused_axes().is_empty());
    }

    #[test]
    fn coupled_existential_axis_is_retained() {
        let coupled = Coordinates::access(1, &[3], vec![form(2, &[0, 0])]).restricted(
            form(0, &[1, -1]),
            0,
            0,
        );
        assert_eq!(coupled.clone().without_unused_axes(), coupled);
        assert_eq!(coupled.input_elements(&[4], 3, &[8]), Some(vec![]));
        assert_eq!(coupled.input_elements(&[4], 1, &[8]), Some(vec![2]));
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
