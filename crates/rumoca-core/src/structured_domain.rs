use serde::{Deserialize, Serialize};

use crate::Expression;

/// Domain-local semantic identity of one structured iteration binder.
///
/// The integer is an ordinal inside exactly one [`StructuredIndexDomain`]. It
/// is deliberately not a `DefId`: a source loop token and a resolved
/// declaration occupy different namespaces. Flat wire replay binds this
/// identity to the token's exact spelling and span in the family template.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct StructuredIndexBinderId(u32);

impl StructuredIndexBinderId {
    pub const fn new(index: u32) -> Self {
        Self(index)
    }

    pub const fn index(self) -> u32 {
        self.0
    }

    pub fn from_ordinal(ordinal: usize) -> Option<Self> {
        u32::try_from(ordinal).ok().map(Self)
    }
}

impl std::fmt::Display for StructuredIndexBinderId {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(formatter)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructuredIndexBinder {
    pub id: StructuredIndexBinderId,
    pub display_name: String,
    pub lower: i64,
    pub upper: i64,
    pub step: i64,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct StructuredIndexDomain {
    pub binders: Vec<StructuredIndexBinder>,
}

/// An index expression written as `constant + Σ coeffs[b] · binder_b` over a
/// family's binder list (positional: `coeffs[b]` is the `b`-th binder's stride).
///
/// This is the compact, materialization-free description of how one array
/// subscript varies across a regular elementwise `for` family.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct AffineForm {
    pub constant: i64,
    pub coeffs: Vec<i64>,
}

impl AffineForm {
    pub fn constant(value: i64, binder_count: usize) -> Self {
        Self {
            constant: value,
            coeffs: vec![0; binder_count],
        }
    }

    pub fn unit_binder(index: usize, binder_count: usize) -> Self {
        let mut coeffs = vec![0; binder_count];
        coeffs[index] = 1;
        Self {
            constant: 0,
            coeffs,
        }
    }

    /// True when the form has no binder dependence (a plain integer).
    pub fn is_binder_free(&self) -> bool {
        self.coeffs.iter().all(|c| *c == 0)
    }

    pub fn checked_add(&self, other: &Self) -> Option<Self> {
        if self.coeffs.len() != other.coeffs.len() {
            return None;
        }
        Some(Self {
            constant: self.constant.checked_add(other.constant)?,
            coeffs: self
                .coeffs
                .iter()
                .zip(&other.coeffs)
                .map(|(a, b)| a.checked_add(*b))
                .collect::<Option<Vec<_>>>()?,
        })
    }

    pub fn checked_neg(&self) -> Option<Self> {
        Some(Self {
            constant: self.constant.checked_neg()?,
            coeffs: self
                .coeffs
                .iter()
                .map(|coefficient| coefficient.checked_neg())
                .collect::<Option<Vec<_>>>()?,
        })
    }

    pub fn checked_scale(&self, factor: i64) -> Option<Self> {
        Some(Self {
            constant: self.constant.checked_mul(factor)?,
            coeffs: self
                .coeffs
                .iter()
                .map(|coefficient| coefficient.checked_mul(factor))
                .collect::<Option<Vec<_>>>()?,
        })
    }
}

/// One subscripted array access within a regular family body, with each
/// subscript dimension resolved to an [`AffineForm`] over the family binders.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArrayAccess {
    /// Dotted source name of the accessed variable (e.g. `u`, `body.x`).
    pub var: String,
    /// Per-dimension affine index, in subscript order.
    pub subscripts: Vec<AffineForm>,
}

impl ArrayAccess {
    /// Per-binder stride of this access's flat (scalar) element index.
    ///
    /// Given the accessed array's row-major memory strides (`memory_strides[k]`
    /// is the element-index step for a unit increment of subscript dimension
    /// `k`), the flat index is
    /// `Σ_k memory_strides[k] · (constant_k + Σ_b coeffs_k[b] · binder_b)`, so a
    /// unit increment of binder `b` moves the flat index by
    /// `Σ_k memory_strides[k] · coeffs_k[b]`. The returned vector holds that step
    /// for each binder `b` in `0..binder_count`. The affine *constants* (stencil
    /// offsets such as the `+1` in `u[i+1, j]`) do not affect the stride -- they
    /// shift only the base element index, which the Solve-IR carries in the base
    /// row's operations.
    ///
    /// `memory_strides` must have one entry per subscript dimension, i.e.
    /// `memory_strides.len() == self.subscripts.len()`.
    pub fn binder_index_strides(
        &self,
        memory_strides: &[usize],
        binder_count: usize,
    ) -> Option<Vec<i64>> {
        if self.subscripts.len() != memory_strides.len()
            || self
                .subscripts
                .iter()
                .any(|subscript| subscript.coeffs.len() != binder_count)
        {
            return None;
        }
        (0..binder_count)
            .map(|binder| {
                self.subscripts
                    .iter()
                    .zip(memory_strides)
                    .try_fold(0i128, |stride, (subscript, memory_stride)| {
                        stride.checked_add(
                            i128::try_from(*memory_stride)
                                .ok()?
                                .checked_mul(i128::from(subscript.coeffs[binder]))?,
                        )
                    })
                    .and_then(|stride| i64::try_from(stride).ok())
            })
            .collect()
    }
}

/// Row-major (last dimension contiguous) element-index strides for an array of
/// the given dimensions: `strides[k]` is the flat element-index step for a unit
/// increment of dimension `k`. For `[NX, NY]` this is `[NY, 1]`; for `[a, b, c]`
/// it is `[b*c, c, 1]`. An empty `dims` yields an empty stride vector.
///
/// These are the `memory_strides` consumed by [`ArrayAccess::binder_index_strides`].
pub fn row_major_strides(dims: &[usize]) -> Option<Vec<usize>> {
    let mut strides = vec![1usize; dims.len()];
    for k in (0..dims.len().saturating_sub(1)).rev() {
        strides[k] = strides[k + 1].checked_mul(dims[k + 1])?;
    }
    Some(strides)
}

/// Convert one zero-based row-major scalar ordinal to zero-based coordinates.
///
/// Returns `None` for an out-of-range ordinal, a zero extent, or scalar-count
/// overflow. A scalar shape (`[]`) contains exactly ordinal zero.
pub fn row_major_coordinates(extents: &[u32], ordinal: usize) -> Option<Vec<u32>> {
    let scalar_count = extents
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))?;
    if ordinal >= scalar_count {
        return None;
    }
    let mut remainder = ordinal;
    let mut coordinates = vec![0; extents.len()];
    for (axis, extent) in extents.iter().enumerate().rev() {
        if *extent == 0 {
            return None;
        }
        coordinates[axis] = u32::try_from(remainder % *extent as usize).ok()?;
        remainder /= *extent as usize;
    }
    Some(coordinates)
}

/// Convert zero-based coordinates to one zero-based row-major scalar ordinal.
///
/// Returns `None` for rank mismatch, an out-of-range coordinate, or address
/// overflow. A scalar shape (`[]`) maps its only coordinate tuple to zero.
pub fn flatten_coordinates(extents: &[u32], coordinates: &[u32]) -> Option<usize> {
    if extents.len() != coordinates.len() {
        return None;
    }
    extents
        .iter()
        .zip(coordinates)
        .try_fold(0usize, |flat, (extent, coordinate)| {
            if coordinate >= extent {
                return None;
            }
            flat.checked_mul(*extent as usize)?
                .checked_add(*coordinate as usize)
        })
}

/// Checked multiplication for tensor extents and row-major storage lengths.
pub const fn checked_product(lhs: usize, rhs: usize) -> Option<usize> {
    lhs.checked_mul(rhs)
}

/// A regular elementwise `for` family: its (possibly nested) loop binders and
/// the affine array accesses appearing in its uniform body.
///
/// This is the compact, materialization-free description of the family that the
/// Solve-IR lowering needs to build an `AffineStencil`: one base row plus, per
/// access, the per-binder strides recorded here.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct RegularForFamily {
    /// Loop binder names, outermost first (a nested `for i ... for j` is `[i, j]`).
    pub binders: Vec<String>,
    /// Every subscripted access in the body (loads and the output).
    pub accesses: Vec<ArrayAccess>,
}

/// The canonical "writable as an array comprehension" form of a structured family:
/// its loop body residual(s) written once with symbolic binder indices, e.g.
/// `{ body(i, j) for i in 1:NX, j in 1:NY }`. This is the source-of-truth shape of a
/// family — flatten captures it before expanding the loop, so downstream phases read
/// the template directly instead of reconstructing it from materialized corner cells.
///
/// [`RegularForFamily`] is a *derived* affine index over this template, present only
/// when every array access is affine in the binders. A family whose body uses
/// `sqrt`/`abs`/`if` (e.g. an immersed-boundary mask) is still a comprehension and
/// stays compact here even though it has no affine descriptor.
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct ComprehensionTemplate {
    /// One residual `Expression` per template equation, in the family's equation
    /// order. Binder indices appear as variable references whose names match the
    /// family `domain`'s binder `display_name`s; binder-dependent array subscripts
    /// are kept symbolic (e.g. `u[i - 1, j]`).
    pub body: Vec<Expression>,
    /// How a domain point derives its scalar body from `body`.
    ///
    /// Source `for` equations substitute the domain binders into an already
    /// scalar body. Whole-array, slice, and connection equations instead project
    /// one row-major element from an aggregate residual. Keeping this distinction
    /// explicit prevents later phases from guessing based on rendered names or
    /// whether a binder happens to occur in the expression.
    #[serde(
        default,
        skip_serializing_if = "ComprehensionScalarView::is_binder_substitution"
    )]
    pub scalar_view: ComprehensionScalarView,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum ComprehensionScalarView {
    /// Substitute the family binder values into the symbolic scalar body.
    #[default]
    BinderSubstitution,
    /// Select the row-major domain element from the aggregate body.
    RowMajorProjection,
    /// Substitute the leading binders and project the remaining row-major axes.
    BinderPrefixProjection { binder_count: u32 },
}

impl ComprehensionScalarView {
    pub(crate) fn is_binder_substitution(&self) -> bool {
        matches!(self, Self::BinderSubstitution)
    }

    pub fn body_scalar(self, point: usize, extents: &[u32]) -> Option<usize> {
        match self {
            Self::BinderSubstitution => Some(0),
            Self::RowMajorProjection => Some(point),
            Self::BinderPrefixProjection { binder_count } => {
                let suffix = extents.get(usize::try_from(binder_count).ok()?..)?;
                let count = suffix.iter().try_fold(1usize, |count, extent| {
                    count.checked_mul(usize::try_from(*extent).ok()?)
                })?;
                (count > 0).then_some(point % count)
            }
        }
    }
}

impl StructuredIndexDomain {
    /// Prove this domain once and mint the witness that answers extent,
    /// stride, ordinal, and coordinate questions without rediscovering the
    /// proof.
    ///
    /// Minting is the single failure boundary: it rejects a zero-step binder
    /// and a scalar count or stride product that leaves `usize`. Every
    /// accessor on [`ValidStructuredIndexDomain`] is total afterwards.
    pub fn validated(&self) -> Result<ValidStructuredIndexDomain<'_>, StructuredIndexDomainError> {
        ValidStructuredIndexDomain::mint(self)
    }

    pub fn validate(&self) -> Result<usize, StructuredIndexDomainError> {
        Ok(self.validated()?.scalar_count())
    }

    pub fn scalar_count(&self) -> Result<usize, StructuredIndexDomainError> {
        self.validate()
    }

    /// Number of values along each binder, in declaration order.
    ///
    /// This validates on every call. Callers that read more than one derived
    /// fact, or read one inside a loop, should mint the witness with
    /// [`StructuredIndexDomain::validated`] instead.
    pub fn extents(&self) -> Result<Vec<usize>, StructuredIndexDomainError> {
        Ok(self.validated()?.extents().to_vec())
    }

    /// Enumerate binder tuples in deterministic scalar-view order.
    ///
    /// The domain is unvalidated here, so minting happens first and the
    /// iterator itself is total.
    pub fn index_tuple_iter(
        &self,
    ) -> Result<impl ExactSizeIterator<Item = Vec<i64>> + '_, StructuredIndexDomainError> {
        Ok(self.validated()?.into_index_tuple_iter())
    }

    /// Materialize every binder tuple.
    ///
    /// Unlike the iterator this reserves the whole tuple list up front, so it
    /// still reports a host allocation limit as a typed error.
    pub fn index_tuples(&self) -> Result<Vec<Vec<i64>>, StructuredIndexDomainError> {
        let valid = self.validated()?;
        let mut tuples = Vec::new();
        reserve_tuple_capacity(&mut tuples, valid.scalar_count())?;
        tuples.extend(valid.index_tuple_iter());
        Ok(tuples)
    }
}

/// A [`StructuredIndexDomain`] whose iteration facts were proved at the
/// validation gate.
///
/// `StructuredIndexDomain` is a plain description: any caller can write down
/// binders whose step is zero or whose extents multiply past `usize`. This
/// witness is the only way to read the derived facts, and it exists only if
/// [`StructuredIndexDomain::validated`] proved them. Holding one is therefore
/// the proof that
///
/// - every binder has a non-zero step and a `usize` value count,
/// - the row-major ordinal strides fit `usize`, and
/// - the domain's scalar count fits `usize`.
///
/// Coordinates are computed on demand from the ordinal, so no accessor ever
/// materializes a coordinate list the caller did not ask for.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValidStructuredIndexDomain<'domain> {
    domain: &'domain StructuredIndexDomain,
    extents: Vec<usize>,
    strides: Vec<usize>,
    scalar_count: usize,
}

impl<'domain> ValidStructuredIndexDomain<'domain> {
    fn mint(domain: &'domain StructuredIndexDomain) -> Result<Self, StructuredIndexDomainError> {
        let extents = domain
            .binders
            .iter()
            .map(StructuredIndexBinder::value_count)
            .collect::<Result<Vec<_>, _>>()?;
        let strides = ordinal_strides_for_extents(&extents)?;
        let mut scalar_count = 1usize;
        for extent in &extents {
            scalar_count = scalar_count
                .checked_mul(*extent)
                .ok_or(StructuredIndexDomainError::ScalarCountOverflow)?;
        }
        Ok(Self {
            domain,
            extents,
            strides,
            scalar_count,
        })
    }

    pub fn domain(&self) -> &'domain StructuredIndexDomain {
        self.domain
    }

    pub fn binders(&self) -> &'domain [StructuredIndexBinder] {
        &self.domain.binders
    }

    /// Number of binders, which is the length of every index tuple.
    pub fn rank(&self) -> usize {
        self.extents.len()
    }

    pub fn scalar_count(&self) -> usize {
        self.scalar_count
    }

    /// Number of values along each binder, in declaration order.
    pub fn extents(&self) -> &[usize] {
        &self.extents
    }

    /// Row-major ordinal strides, with the innermost binder varying fastest.
    pub fn ordinal_strides(&self) -> &[usize] {
        &self.strides
    }

    /// Binder coordinates of a row-major ordinal, without materializing any
    /// preceding tuple. Ordinals outside the domain have no coordinates.
    pub fn index_tuple_at(&self, ordinal: usize) -> Option<Vec<i64>> {
        (ordinal < self.scalar_count).then(|| self.coordinates(ordinal))
    }

    /// Row-major ordinal of binder coordinates. Coordinates outside the domain
    /// have no ordinal.
    pub fn ordinal_of(&self, index_tuple: &[i64]) -> Option<usize> {
        if index_tuple.len() != self.rank() {
            return None;
        }
        let mut ordinal = 0usize;
        for (((binder, value), extent), stride) in self
            .domain
            .binders
            .iter()
            .zip(index_tuple)
            .zip(&self.extents)
            .zip(&self.strides)
        {
            let position = binder_position(binder, *extent, *value)?;
            // `position < extent` and `extent * stride` divides the proved
            // scalar count, so the running ordinal stays below it.
            ordinal += position * stride;
        }
        Some(ordinal)
    }

    /// Ordinals needed to inspect the base point and one neighbor along every
    /// non-singleton binder. This is O(rank), independent of cardinality.
    pub fn corner_ordinals(&self) -> Vec<usize> {
        if self.scalar_count == 0 {
            return Vec::new();
        }
        let mut ordinals = Vec::with_capacity(1 + self.rank());
        ordinals.push(0);
        ordinals.extend(
            self.extents
                .iter()
                .zip(&self.strides)
                .filter_map(|(extent, stride)| (*extent > 1).then_some(*stride)),
        );
        ordinals
    }

    /// Lazily enumerate binder tuples in deterministic scalar-view order.
    pub fn index_tuple_iter(&self) -> impl ExactSizeIterator<Item = Vec<i64>> + '_ {
        (0..self.scalar_count).map(|ordinal| self.coordinates(ordinal))
    }

    /// The same enumeration, carrying the witness so the iterator outlives the
    /// borrow that minted it.
    pub fn into_index_tuple_iter(self) -> impl ExactSizeIterator<Item = Vec<i64>> + 'domain {
        (0..self.scalar_count).map(move |ordinal| self.coordinates(ordinal))
    }

    /// Coordinates of an ordinal the caller already placed inside the domain.
    fn coordinates(&self, ordinal: usize) -> Vec<i64> {
        let mut tuple = Vec::with_capacity(self.rank());
        tuple.extend(
            self.domain
                .binders
                .iter()
                .zip(&self.extents)
                .zip(&self.strides)
                .map(|((binder, extent), stride)| {
                    binder_coordinate(binder, *extent, *stride, ordinal)
                }),
        );
        tuple
    }
}

/// Value of one binder at the position `ordinal` selects along its axis.
///
/// The caller places `ordinal` inside the domain, so every extent here is
/// non-zero and every stride divides the scalar count. The position is below
/// the binder's value count, which puts the value between the binder's `lower`
/// and its last stepped value; both are `i64`, so the `i128` arithmetic
/// narrows exactly.
fn binder_coordinate(
    binder: &StructuredIndexBinder,
    extent: usize,
    stride: usize,
    ordinal: usize,
) -> i64 {
    let position = (ordinal / stride) % extent;
    (i128::from(binder.lower) + i128::from(binder.step) * position as i128) as i64
}

/// Position of `value` along one binder's axis, or `None` when the value is
/// not one of the binder's `extent` stepped values.
fn binder_position(binder: &StructuredIndexBinder, extent: usize, value: i64) -> Option<usize> {
    if extent == 0 {
        return None;
    }
    let distance = i128::from(value) - i128::from(binder.lower);
    let step = i128::from(binder.step);
    if distance % step != 0 {
        return None;
    }
    let position = usize::try_from(distance / step).ok()?;
    (position < extent).then_some(position)
}

fn ordinal_strides_for_extents(
    extents: &[usize],
) -> Result<Vec<usize>, StructuredIndexDomainError> {
    let mut strides = vec![1usize; extents.len()];
    for index in (0..extents.len().saturating_sub(1)).rev() {
        strides[index] = strides[index + 1]
            .checked_mul(extents[index + 1])
            .ok_or(StructuredIndexDomainError::ScalarCountOverflow)?;
    }
    Ok(strides)
}

fn reserve_tuple_capacity(
    tuples: &mut Vec<Vec<i64>>,
    capacity: usize,
) -> Result<(), StructuredIndexDomainError> {
    tuples
        .try_reserve_exact(capacity)
        .map_err(|_| StructuredIndexDomainError::IndexTupleCapacityOverflow)
}

impl StructuredIndexBinder {
    fn value_count(&self) -> Result<usize, StructuredIndexDomainError> {
        if self.step == 0 {
            return Err(StructuredIndexDomainError::ZeroStep {
                binder_id: self.id,
                display_name: self.display_name.clone(),
            });
        }
        let count = if self.step > 0 {
            self.positive_value_count()
        } else {
            self.negative_value_count()
        };
        usize::try_from(count).map_err(|_| StructuredIndexDomainError::ScalarCountOverflow)
    }

    fn positive_value_count(&self) -> u128 {
        if self.lower > self.upper {
            return 0;
        }
        let distance = (self.upper as i128 - self.lower as i128) as u128;
        let step = self.step as u128;
        distance / step + 1
    }

    fn negative_value_count(&self) -> u128 {
        if self.lower < self.upper {
            return 0;
        }
        let distance = (self.lower as i128 - self.upper as i128) as u128;
        let step = -(self.step as i128);
        distance / step as u128 + 1
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum StructuredIndexDomainError {
    ZeroStep {
        binder_id: StructuredIndexBinderId,
        display_name: String,
    },
    ScalarCountOverflow,
    IndexTupleCapacityOverflow,
}

impl std::fmt::Display for StructuredIndexDomainError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ZeroStep {
                binder_id,
                display_name,
            } => write!(
                f,
                "index binder `{display_name}` ({binder_id}) has zero step"
            ),
            Self::ScalarCountOverflow => {
                write!(f, "structured domain scalar count overflows usize")
            }
            Self::IndexTupleCapacityOverflow => {
                write!(
                    f,
                    "structured domain index tuple capacity exceeds host memory limits"
                )
            }
        }
    }
}

impl std::error::Error for StructuredIndexDomainError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn structured_index_domain_error_displays_zero_step() {
        let error = StructuredIndexDomainError::ZeroStep {
            binder_id: StructuredIndexBinderId::new(3),
            display_name: "i".to_string(),
        };

        assert_eq!(error.to_string(), "index binder `i` (3) has zero step");
    }

    #[test]
    fn structured_index_domain_error_displays_scalar_count_overflow() {
        assert_eq!(
            StructuredIndexDomainError::ScalarCountOverflow.to_string(),
            "structured domain scalar count overflows usize"
        );
    }

    #[test]
    fn index_tuples_enumerates_cartesian_domain() {
        let domain = StructuredIndexDomain {
            binders: vec![
                StructuredIndexBinder {
                    id: StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 2,
                    step: 1,
                },
                StructuredIndexBinder {
                    id: StructuredIndexBinderId::new(1),
                    display_name: "j".to_string(),
                    lower: 3,
                    upper: 4,
                    step: 1,
                },
            ],
        };

        assert_eq!(
            domain.index_tuples(),
            Ok(vec![vec![1, 3], vec![1, 4], vec![2, 3], vec![2, 4]])
        );
    }

    #[test]
    fn index_tuples_rejects_zero_step() {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: StructuredIndexBinderId::new(7),
                display_name: "k".to_string(),
                lower: 1,
                upper: 3,
                step: 0,
            }],
        };

        assert_eq!(
            domain.index_tuples(),
            Err(StructuredIndexDomainError::ZeroStep {
                binder_id: StructuredIndexBinderId::new(7),
                display_name: "k".to_string()
            })
        );
    }

    #[test]
    fn index_tuples_rejects_scalar_count_overflow() {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: i64::MIN,
                upper: i64::MAX,
                step: 1,
            }],
        };

        assert_eq!(
            domain.index_tuples(),
            Err(StructuredIndexDomainError::ScalarCountOverflow)
        );
    }

    #[test]
    fn ordinal_coordinates_support_reverse_and_non_unit_ranges() {
        let domain = StructuredIndexDomain {
            binders: vec![
                StructuredIndexBinder {
                    id: StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower: 5,
                    upper: 1,
                    step: -2,
                },
                StructuredIndexBinder {
                    id: StructuredIndexBinderId::new(1),
                    display_name: "j".to_string(),
                    lower: 2,
                    upper: 8,
                    step: 3,
                },
            ],
        };

        let valid = domain.validated().expect("compact reverse domain is valid");

        assert_eq!(valid.extents(), [3, 3]);
        assert_eq!(valid.ordinal_strides(), [3, 1]);
        assert_eq!(valid.index_tuple_at(5), Some(vec![3, 8]));
        assert_eq!(valid.ordinal_of(&[3, 8]), Some(5));
        assert_eq!(valid.ordinal_of(&[4, 8]), None);
        assert_eq!(valid.corner_ordinals(), vec![0, 3, 1]);
    }

    #[test]
    fn empty_domain_has_no_tuples_or_corners() {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 3,
                upper: 1,
                step: 1,
            }],
        };

        let valid = domain.validated().expect("empty domain is valid");

        assert_eq!(valid.scalar_count(), 0);
        assert_eq!(valid.index_tuple_at(0), None);
        assert_eq!(valid.ordinal_of(&[3]), None);
        assert_eq!(valid.corner_ordinals(), Vec::<usize>::new());
        assert_eq!(valid.index_tuple_iter().next(), None);
    }

    #[test]
    fn distant_ordinal_is_computed_without_materializing_prefix() {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 1_000_000,
                step: 1,
            }],
        };

        let valid = domain.validated().expect("million-point domain is valid");

        assert_eq!(valid.index_tuple_at(999_999), Some(vec![1_000_000]));
        assert_eq!(valid.ordinal_of(&[1_000_000]), Some(999_999));
    }

    #[test]
    fn affine_form_arithmetic_declines_overflow_and_rank_mismatch() {
        let maximum = AffineForm {
            constant: i64::MAX,
            coeffs: vec![i64::MAX],
        };
        let one = AffineForm {
            constant: 1,
            coeffs: vec![1],
        };
        let minimum = AffineForm {
            constant: i64::MIN,
            coeffs: vec![i64::MIN],
        };

        assert_eq!(maximum.checked_add(&one), None);
        assert_eq!(minimum.checked_neg(), None);
        assert_eq!(maximum.checked_scale(2), None);
        assert_eq!(
            one.checked_add(&AffineForm {
                constant: 1,
                coeffs: vec![1, 0],
            }),
            None
        );
    }

    fn access(var: &str, subscripts: Vec<AffineForm>) -> ArrayAccess {
        ArrayAccess {
            var: var.to_string(),
            subscripts,
        }
    }

    #[test]
    fn row_major_strides_are_inner_dimension_contiguous() {
        assert_eq!(row_major_strides(&[]), Some(Vec::<usize>::new()));
        assert_eq!(row_major_strides(&[4]), Some(vec![1]));
        assert_eq!(row_major_strides(&[3, 4]), Some(vec![4, 1]));
        assert_eq!(row_major_strides(&[2, 3, 4]), Some(vec![12, 4, 1]));
        assert_eq!(row_major_strides(&[2, usize::MAX, 2]), None);
    }

    #[test]
    fn row_major_coordinates_and_flattening_are_exact_inverses() {
        let extents = [2, 3, 4];
        for ordinal in 0..24 {
            let coordinates = row_major_coordinates(&extents, ordinal).unwrap();
            assert_eq!(flatten_coordinates(&extents, &coordinates), Some(ordinal));
        }
        assert_eq!(row_major_coordinates(&[], 0), Some(Vec::new()));
        assert_eq!(flatten_coordinates(&[], &[]), Some(0));
        assert_eq!(row_major_coordinates(&[2, 0, 3], 0), None);
        assert_eq!(row_major_coordinates(&[2, 3], 6), None);
        assert_eq!(flatten_coordinates(&[2, 3], &[2, 0]), None);
        assert_eq!(flatten_coordinates(&[2, 3], &[1]), None);
    }

    #[test]
    fn row_major_strides_compose_with_binder_index_strides() {
        // u[NX, NY] with NX=5, NY=4: a unit i-step moves the index by NY=4,
        // a unit j-step by 1 -- exactly what binder_index_strides recovers.
        let memory_strides = row_major_strides(&[5, 4]).unwrap();
        let u_ij = access(
            "u",
            vec![
                AffineForm {
                    constant: 0,
                    coeffs: vec![1, 0],
                },
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                },
            ],
        );
        assert_eq!(
            u_ij.binder_index_strides(&memory_strides, 2),
            Some(vec![4, 1])
        );
    }

    #[test]
    fn binder_index_strides_for_2d_stencil_offsets() {
        // A 2-D field `u[NX, NY]` is row-major, so a unit step of binder i (the
        // outer subscript) moves the flat index by NY and a unit step of binder j
        // by 1. The +1 offset in u[i+1, j] must not change the stride.
        let memory_strides = [4, 1]; // NY = 4
        let u_ij = access(
            "u",
            vec![
                AffineForm {
                    constant: 0,
                    coeffs: vec![1, 0],
                },
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                },
            ],
        );
        let u_ip1_j = access(
            "u",
            vec![
                AffineForm {
                    constant: 1,
                    coeffs: vec![1, 0],
                },
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                },
            ],
        );
        assert_eq!(
            u_ij.binder_index_strides(&memory_strides, 2),
            Some(vec![4, 1])
        );
        assert_eq!(
            u_ip1_j.binder_index_strides(&memory_strides, 2),
            Some(vec![4, 1])
        );
    }

    #[test]
    fn binder_index_strides_account_for_scaled_subscripts() {
        // A `2*i` outer subscript doubles the per-i stride.
        let memory_strides = [4, 1];
        let scaled = access(
            "u",
            vec![
                AffineForm {
                    constant: 0,
                    coeffs: vec![2, 0],
                },
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                },
            ],
        );
        assert_eq!(
            scaled.binder_index_strides(&memory_strides, 2),
            Some(vec![8, 1])
        );
    }

    #[test]
    fn binder_index_strides_sum_across_subscript_dimensions() {
        // A coupled access u[i + j, j]: binder j appears in BOTH subscript
        // dimensions, so its stride sums both contributions (4*1 + 1*1 = 5),
        // while binder i appears only in the outer dimension (4).
        let memory_strides = [4, 1];
        let coupled = access(
            "u",
            vec![
                AffineForm {
                    constant: 0,
                    coeffs: vec![1, 1],
                }, // i + j
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                }, // j
            ],
        );
        assert_eq!(
            coupled.binder_index_strides(&memory_strides, 2),
            Some(vec![4, 5])
        );
    }

    #[test]
    fn binder_index_strides_zero_for_binder_free_access() {
        // A boundary access at a fixed index (e.g. u[NX, j]) has no i-stride.
        let memory_strides = [4, 1];
        let boundary = access(
            "u",
            vec![
                AffineForm {
                    constant: 6,
                    coeffs: vec![0, 0],
                },
                AffineForm {
                    constant: 0,
                    coeffs: vec![0, 1],
                },
            ],
        );
        assert_eq!(
            boundary.binder_index_strides(&memory_strides, 2),
            Some(vec![0, 1])
        );
    }

    #[test]
    fn binder_index_strides_decline_invalid_shape_and_overflow() {
        let wrong_rank = access(
            "u",
            vec![AffineForm {
                constant: 0,
                coeffs: vec![1, 0],
            }],
        );
        assert_eq!(wrong_rank.binder_index_strides(&[4, 1], 2), None);

        let overflowing = access(
            "u",
            vec![AffineForm {
                constant: 0,
                coeffs: vec![i64::MAX],
            }],
        );
        assert_eq!(overflowing.binder_index_strides(&[usize::MAX], 1), None);
    }
}
