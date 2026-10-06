//! Exact sets of array indices and binder values as unions of boxes of
//! arithmetic progressions, and the images of affine subscripts over them.
//!
//! MLS 3.6 §10.5 selects array elements by index vectors, and a compact
//! function loop writes `y[e(i)]` once per point of its domain (§11.2.2). The
//! elements such a loop defines, or a slice reads, are the image of a range
//! under the subscripts: for affine subscripts, one progression per
//! dimension. Definedness proofs and dependency projections therefore work on
//! these images and on exact point solutions directly, never on one index
//! tuple or one domain point at a time, so their cost does not grow with the
//! extent of an array or of a loop domain.
//!
//! A binder region is an [`IndexBox`] with one coordinate per binder. Set
//! operations are exact or return `None`; [`image`] and [`solve_point`]
//! answer [`SetAnswer::Exact`] or a declared [`SetAnswer::Superset`] for
//! forms no box describes exactly, which every caller handles explicitly.

use super::AffineForm;

/// The integers `first + k * step` for `0 <= k < count`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Progression {
    first: i64,
    step: i64,
    count: i64,
}

/// A bound on intermediate splitting work; beyond it a proof is refused.
const SPLIT_LIMIT: usize = 4096;
/// The largest stride ratio a set difference splits into residue classes.
const RESIDUE_LIMIT: i64 = 64;

impl Progression {
    pub const EMPTY: Self = Self {
        first: 0,
        step: 1,
        count: 0,
    };

    pub fn single(value: i64) -> Self {
        Self {
            first: value,
            step: 1,
            count: 1,
        }
    }

    /// `lower:step:upper` (MLS §10.4.1) for a positive or negative step; the
    /// same set is kept with an ascending step.
    pub fn range(lower: i64, step: i64, upper: i64) -> Option<Self> {
        if step == 0 {
            return None;
        }
        let magnitude = step.checked_abs()?;
        let (first, last) = if step > 0 {
            if upper < lower {
                return Some(Self::EMPTY);
            }
            (
                lower,
                lower
                    .checked_add((upper.checked_sub(lower)? / magnitude).checked_mul(magnitude)?)?,
            )
        } else {
            if upper > lower {
                return Some(Self::EMPTY);
            }
            let last = lower;
            let first = lower
                .checked_sub((lower.checked_sub(upper)? / magnitude).checked_mul(magnitude)?)?;
            (first, last)
        };
        let count = (last - first) / magnitude + 1;
        Some(Self {
            first,
            step: magnitude,
            count,
        })
    }

    pub fn is_empty(self) -> bool {
        self.count == 0
    }

    /// The number of elements.
    pub fn len(self) -> i64 {
        self.count
    }

    pub fn step(self) -> i64 {
        self.step
    }

    pub fn first(self) -> Option<i64> {
        (!self.is_empty()).then_some(self.first)
    }

    pub fn last(self) -> Option<i64> {
        (!self.is_empty()).then(|| self.first + (self.count - 1) * self.step)
    }

    /// `a * v + b` for every element `v`; the image of an affine map.
    pub fn affine_image(self, a: i64, b: i64) -> Option<Self> {
        if self.is_empty() {
            return Some(Self::EMPTY);
        }
        if a == 0 || self.count == 1 {
            return Some(Self::single(a.checked_mul(self.first)?.checked_add(b)?));
        }
        let first = a.checked_mul(self.first)?.checked_add(b)?;
        let last = a.checked_mul(self.last()?)?.checked_add(b)?;
        let step = a.checked_mul(self.step)?.checked_abs()?;
        Some(Self {
            first: first.min(last),
            step,
            count: self.count,
        })
    }

    /// The elements of both progressions (exact, by the Chinese remainder
    /// theorem on their residues).
    pub fn intersect(self, other: Self) -> Option<Self> {
        let (Some(lhs_last), Some(rhs_last)) = (self.last(), other.last()) else {
            return Some(Self::EMPTY);
        };
        let lower = self.first.max(other.first);
        let upper = lhs_last.min(rhs_last);
        if lower > upper {
            return Some(Self::EMPTY);
        }
        let (gcd, x, _) = extended_gcd(self.step, other.step);
        let difference = other.first.checked_sub(self.first)?;
        if difference % gcd != 0 {
            return Some(Self::EMPTY);
        }
        let step = (self.step / gcd).checked_mul(other.step)?;
        // first ≡ self.first (mod self.step) and ≡ other.first (mod other.step)
        let multiplier =
            i128::from(difference / gcd) * i128::from(x) % i128::from(other.step / gcd);
        let anchor = i128::from(self.first) + multiplier * i128::from(self.step);
        let step_wide = i128::from(step);
        let offset = (anchor - i128::from(lower)).rem_euclid(step_wide);
        let first = i64::try_from(i128::from(lower) + offset).ok()?;
        if first > upper {
            return Some(Self::EMPTY);
        }
        Some(Self {
            first,
            step,
            count: (upper - first) / step + 1,
        })
    }

    /// The elements of `self` not in `other`, as disjoint progressions.
    fn difference(self, other: Self) -> Option<Vec<Self>> {
        let common = self.intersect(other)?;
        let (Some(common_first), Some(common_last)) = (common.first(), common.last()) else {
            return Some(vec![self]);
        };
        let mut parts = Vec::new();
        // Elements of `self` before and after the common run.
        let before = (common_first - self.first) / self.step;
        if before > 0 {
            parts.push(Self {
                first: self.first,
                step: self.step,
                count: before,
            });
        }
        let after_first = common_last + self.step;
        let after = self
            .last()?
            .checked_sub(after_first)
            .map_or(0, |span| if span < 0 { 0 } else { span / self.step + 1 });
        if after > 0 {
            parts.push(Self {
                first: after_first,
                step: self.step,
                count: after,
            });
        }
        // Inside the run, the common progression keeps every `ratio`-th
        // element of `self`; the other residue classes remain.
        let ratio = common.step / self.step;
        if ratio > RESIDUE_LIMIT {
            return None;
        }
        for residue in 1..ratio {
            let first = common_first + residue * self.step;
            if first > common_last {
                break;
            }
            parts.push(Self {
                first,
                step: common.step,
                count: (common_last - first) / common.step + 1,
            });
        }
        Some(parts)
    }
}

fn extended_gcd(a: i64, b: i64) -> (i64, i64, i64) {
    if b == 0 {
        return (a, 1, 0);
    }
    let (gcd, x, y) = extended_gcd(b, a % b);
    (gcd, y, x - (a / b) * y)
}

/// The product of one progression per dimension.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexBox(pub Vec<Progression>);

impl IndexBox {
    /// Every index of an array with these extents.
    pub fn whole(extents: &[i64]) -> Option<Self> {
        extents
            .iter()
            .map(|extent| Progression::range(1, 1, *extent))
            .collect::<Option<Vec<_>>>()
            .map(Self)
    }

    pub fn is_empty(&self) -> bool {
        self.0.iter().any(|axis| axis.is_empty())
    }

    /// Whether every index lies within these extents.
    pub fn within(&self, extents: &[i64]) -> bool {
        self.0.len() == extents.len()
            && self.0.iter().zip(extents).all(|(axis, extent)| {
                axis.is_empty()
                    || (axis.first >= 1 && axis.last().is_some_and(|last| last <= *extent))
            })
    }

    pub fn intersect(&self, other: &Self) -> Option<Self> {
        if self.0.len() != other.0.len() {
            return None;
        }
        self.0
            .iter()
            .zip(&other.0)
            .map(|(lhs, rhs)| lhs.intersect(*rhs))
            .collect::<Option<Vec<_>>>()
            .map(Self)
    }

    /// `self` without `other`, as disjoint boxes.
    fn difference(&self, other: &Self) -> Option<Vec<Self>> {
        if self.0.len() != other.0.len() {
            return None;
        }
        let mut remaining = self.clone();
        let mut parts = Vec::new();
        for axis in 0..self.0.len() {
            for part in remaining.0[axis].difference(other.0[axis])? {
                let mut piece = remaining.clone();
                piece.0[axis] = part;
                parts.push(piece);
            }
            remaining.0[axis] = remaining.0[axis].intersect(other.0[axis])?;
            if remaining.0[axis].is_empty() {
                break;
            }
        }
        Some(parts)
    }
}

/// A finite union of index boxes.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct IndexUnion(Vec<IndexBox>);

impl IndexUnion {
    pub fn of(boxes: impl IntoIterator<Item = IndexBox>) -> Self {
        Self(boxes.into_iter().filter(|part| !part.is_empty()).collect())
    }

    pub fn extend(&mut self, other: Self) {
        for part in other.0 {
            if !self.contains_box(&part) {
                self.0.push(part);
            }
        }
    }

    /// Whether every index of `query` is in the set. An undecidable split is
    /// reported as not contained.
    pub fn contains_box(&self, query: &IndexBox) -> bool {
        let mut pending = vec![query.clone()];
        for part in &self.0 {
            let Some(next) = subtract_all(pending, part) else {
                return false;
            };
            pending = next;
            if pending.is_empty() {
                return true;
            }
        }
        pending.iter().all(IndexBox::is_empty)
    }

    pub fn within(&self, extents: &[i64]) -> bool {
        self.0.iter().all(|part| part.within(extents))
    }

    pub fn boxes(&self) -> &[IndexBox] {
        &self.0
    }

    /// The indices of `self` not in `other`; `None` when a difference cannot
    /// be represented.
    pub fn difference(&self, other: &Self) -> Option<Self> {
        let mut pending = self.0.clone();
        for part in &other.0 {
            pending = subtract_all(pending, part)?;
        }
        Some(Self(pending))
    }

    pub fn contains_set(&self, query: &Self) -> bool {
        query.0.iter().all(|part| self.contains_box(part))
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// The indices in both sets; `None` when an endpoint overflows.
    pub fn meet(&self, other: &Self) -> Option<Self> {
        let parts = self
            .0
            .iter()
            .flat_map(|lhs| other.0.iter().map(move |rhs| lhs.intersect(rhs)))
            .collect::<Option<Vec<_>>>()?;
        Some(Self::of(parts))
    }
}

/// Every piece of `pending` without `part`; `None` when a difference cannot be
/// represented or the split grows past its bound.
fn subtract_all(pending: Vec<IndexBox>, part: &IndexBox) -> Option<Vec<IndexBox>> {
    let mut next = Vec::new();
    for piece in pending {
        next.extend(
            piece
                .difference(part)?
                .into_iter()
                .filter(|rest| !rest.is_empty()),
        );
        if next.len() > SPLIT_LIMIT {
            return None;
        }
    }
    Some(next)
}

impl IndexUnion {
    /// The set moved by `delta` along `coordinate`.
    pub fn translated(&self, coordinate: usize, delta: i64) -> Option<Self> {
        let mut moved = Vec::with_capacity(self.0.len());
        for part in &self.0 {
            let mut part = part.clone();
            part.0[coordinate] = part.0.get(coordinate)?.affine_image(1, delta)?;
            moved.push(part);
        }
        Some(Self::of(moved))
    }
}

/// One subscript over a binder region: `form(p) + o` for every offset `o`.
///
/// A scalar affine subscript has one offset (`0`); a range subscript
/// `i:i + 1` has the offsets `0:1`; a binder-free subscript has a constant
/// form and its selected indices as offsets.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IndexAxis {
    pub form: AffineForm,
    pub offsets: Vec<Progression>,
}

/// A set an affine query produced: exactly, or a declared superset when the
/// query's form has no exact box description.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SetAnswer {
    Exact(IndexUnion),
    Superset(IndexUnion),
}

impl SetAnswer {
    pub fn exact(self) -> Option<IndexUnion> {
        match self {
            Self::Exact(set) => Some(set),
            Self::Superset(_) => None,
        }
    }

    /// The set, exact or not; for a caller that needs only containment in a
    /// superset (a read whose every possible element is defined).
    pub fn bound(self) -> IndexUnion {
        match self {
            Self::Exact(set) | Self::Superset(set) => set,
        }
    }
}

/// The largest number of offsets an image or solution splits into.
const OFFSET_LIMIT: usize = 64;

/// The elements `axes` name at some binder value of `region`.
///
/// Exact when each axis reads at most one binder and no binder indexes two
/// axes; otherwise each axis is bounded by its interval hull and the answer is
/// a superset. `None` when an endpoint overflows.
pub fn image(axes: &[IndexAxis], region: &IndexBox) -> Option<SetAnswer> {
    let mut exact = !binder_repeats(axes);
    let mut per_axis = Vec::with_capacity(axes.len());
    for axis in axes {
        let (values, axis_exact) = axis_image(axis, region)?;
        exact &= axis_exact;
        per_axis.push(values);
    }
    let boxes = product(&per_axis);
    let set = IndexUnion::of(boxes);
    Some(if exact {
        SetAnswer::Exact(set)
    } else {
        SetAnswer::Superset(set)
    })
}

/// The binder values `p` of `region` at which `index` is one of the
/// elements `axes(p)` names: the iterations of a loop that write one element.
///
/// Exact when each axis reads at most one binder; an axis over several binders
/// leaves its binders unconstrained (a superset).
pub fn solve_point(axes: &[IndexAxis], region: &IndexBox, index: &[i64]) -> Option<SetAnswer> {
    if axes.len() != index.len()
        || axes
            .iter()
            .any(|axis| axis.form.coeffs.len() > region.0.len())
    {
        return None;
    }
    let mut allowed = region
        .0
        .iter()
        .map(|values| vec![*values])
        .collect::<Vec<_>>();
    let mut exact = true;
    for (axis, value) in axes.iter().zip(index) {
        let binders = axis_binders(axis);
        match binders.as_slice() {
            [] => {
                let target = value.checked_sub(axis.form.constant)?;
                if !axis
                    .offsets
                    .iter()
                    .any(|offsets| contains(*offsets, target))
                {
                    return Some(SetAnswer::Exact(IndexUnion::default()));
                }
            }
            [binder] => {
                let scale = axis.form.coeffs[*binder];
                let target = value.checked_sub(axis.form.constant)?;
                let values = offsets_of(&axis.offsets)?
                    .into_iter()
                    .filter_map(|offset| target.checked_sub(offset))
                    .filter(|numerator| numerator % scale == 0)
                    .map(|numerator| numerator / scale)
                    .collect::<Vec<_>>();
                let reached = values
                    .into_iter()
                    .filter(|candidate| {
                        allowed[*binder]
                            .iter()
                            .any(|values| contains(*values, *candidate))
                    })
                    .map(Progression::single)
                    .collect::<Vec<_>>();
                if reached.is_empty() {
                    return Some(SetAnswer::Exact(IndexUnion::default()));
                }
                allowed[*binder] = reached;
            }
            _ => exact = false,
        }
    }
    let set = IndexUnion::of(product(&allowed));
    Some(if exact {
        SetAnswer::Exact(set)
    } else {
        SetAnswer::Superset(set)
    })
}

fn contains(values: Progression, candidate: i64) -> bool {
    values
        .intersect(Progression::single(candidate))
        .is_some_and(|hit| !hit.is_empty())
}

fn axis_binders(axis: &IndexAxis) -> Vec<usize> {
    axis.form
        .coeffs
        .iter()
        .enumerate()
        .filter(|(_, scale)| **scale != 0)
        .map(|(binder, _)| binder)
        .collect()
}

fn binder_repeats(axes: &[IndexAxis]) -> bool {
    let mut seen = Vec::new();
    axes.iter().flat_map(axis_binders).any(|binder| {
        if seen.len() <= binder {
            seen.resize(binder + 1, false);
        }
        std::mem::replace(&mut seen[binder], true)
    })
}

/// Every offset value, when there are at most `OFFSET_LIMIT`.
fn offsets_of(offsets: &[Progression]) -> Option<Vec<i64>> {
    let mut values = Vec::new();
    for part in offsets {
        let (Some(first), Some(last)) = (part.first(), part.last()) else {
            continue;
        };
        let mut value = first;
        while value <= last {
            values.push(value);
            if values.len() > OFFSET_LIMIT {
                return None;
            }
            value = value.checked_add(part.step())?;
        }
    }
    Some(values)
}

/// One axis's elements over `region`, and whether they are exact.
fn axis_image(axis: &IndexAxis, region: &IndexBox) -> Option<(Vec<Progression>, bool)> {
    let binders = axis_binders(axis);
    let constant = axis.form.constant;
    match binders.as_slice() {
        [] => Some((
            axis.offsets
                .iter()
                .map(|offsets| offsets.affine_image(1, constant))
                .collect::<Option<Vec<_>>>()?,
            true,
        )),
        [binder] => {
            let values = *region.0.get(*binder)?;
            let base = values.affine_image(axis.form.coeffs[*binder], constant)?;
            let mut parts = Vec::new();
            for offsets in &axis.offsets {
                parts.extend(shifted_images(base, *offsets)?);
            }
            Some((parts, true))
        }
        _ => {
            // Terms whose images tile like the digits of a mixed radix (a row
            // index times its row length plus a column) form one progression.
            if let Some(base) = tiled_sum(axis, &binders, region)? {
                let base = base.affine_image(1, constant)?;
                let mut parts = Vec::new();
                for offsets in &axis.offsets {
                    parts.extend(shifted_images(base, *offsets)?);
                }
                return Some((parts, true));
            }
            // Interval hull of the sum: a superset of the exact image.
            let mut lower = constant;
            let mut upper = constant;
            for binder in binders {
                let values = *region.0.get(binder)?;
                let (Some(first), Some(last)) = (values.first(), values.last()) else {
                    return Some((vec![Progression::EMPTY], true));
                };
                let scale = axis.form.coeffs[binder];
                let (a, b) = (scale.checked_mul(first)?, scale.checked_mul(last)?);
                lower = lower.checked_add(a.min(b))?;
                upper = upper.checked_add(a.max(b))?;
            }
            let mut parts = Vec::new();
            for offsets in &axis.offsets {
                let (Some(first), Some(last)) = (offsets.first(), offsets.last()) else {
                    continue;
                };
                parts.push(Progression::range(
                    lower.checked_add(first)?,
                    1,
                    upper.checked_add(last)?,
                )?);
            }
            Some((parts, false))
        }
    }
}

/// `{b + o : b in base, o in offsets}` as progressions.
fn shifted_images(base: Progression, offsets: Progression) -> Option<Vec<Progression>> {
    let (Some(base_first), Some(base_last)) = (base.first(), base.last()) else {
        return Some(vec![Progression::EMPTY]);
    };
    let (Some(first), Some(last)) = (offsets.first(), offsets.last()) else {
        return Some(vec![Progression::EMPTY]);
    };
    if first == last {
        return Some(vec![base.affine_image(1, first)?]);
    }
    let count = (last - first) / offsets.step() + 1;
    // Offset blocks that exactly tile the gaps between base elements form one
    // progression.
    if base_first == base_last || offsets.step().checked_mul(count)? == base.step() {
        return Progression::range(
            base_first.checked_add(first)?,
            offsets.step(),
            base_last.checked_add(last)?,
        )
        .map(|tiled| vec![tiled]);
    }
    offsets_of(&[offsets])?
        .into_iter()
        .map(|offset| base.affine_image(1, offset))
        .collect()
}

/// The product boxes of one union of progressions per axis.
fn product(per_axis: &[Vec<Progression>]) -> Vec<IndexBox> {
    let mut boxes = vec![Vec::new()];
    for parts in per_axis {
        let mut extended = Vec::with_capacity(boxes.len() * parts.len());
        for prefix in &boxes {
            for part in parts {
                let mut next: Vec<Progression> = prefix.clone();
                next.push(*part);
                extended.push(next);
            }
        }
        boxes = extended;
    }
    boxes.into_iter().map(IndexBox).collect()
}

#[cfg(test)]
mod tests;

/// The exact set `{sum of scale_b * v_b}` over the region when the terms
/// tile: sorted by step, each term's step equals the span of the terms
/// before it. `Ok(None)` when they do not tile; `None` on overflow.
fn tiled_sum(
    axis: &IndexAxis,
    binders: &[usize],
    region: &IndexBox,
) -> Option<Option<Progression>> {
    let mut terms = binders
        .iter()
        .map(|binder| {
            region
                .0
                .get(*binder)?
                .affine_image(axis.form.coeffs[*binder], 0)
        })
        .collect::<Option<Vec<_>>>()?;
    if terms.iter().any(|term| term.is_empty()) {
        return Some(Some(Progression::EMPTY));
    }
    terms.sort_by_key(|term| {
        if term.len() == 1 {
            i64::MAX
        } else {
            term.step()
        }
    });
    let mut sum = terms[0];
    for term in &terms[1..] {
        let shift = term.first()?;
        if term.len() == 1 {
            sum = sum.affine_image(1, shift)?;
        } else if sum.len() == 1 {
            sum = term.affine_image(1, sum.first()?)?;
        } else if term.step() == sum.step().checked_mul(sum.len())? {
            sum = Progression::range(
                sum.first()?.checked_add(shift)?,
                sum.step(),
                sum.last()?.checked_add(term.last()?)?,
            )?;
        } else {
            return Some(None);
        }
    }
    Some(Some(sum))
}
