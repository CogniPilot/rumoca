use std::fmt;
use std::ops::Range;

use indexmap::IndexSet;
use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve::AffineStencilIndexStrideTerm;

/// Compact may-read relation over solver-Y coordinates.
///
/// Scalar coordinates remain explicit, contiguous tensor loads remain ranges,
/// and structured Map/AffineStencil loads remain affine images of their checked
/// domains. The relation is materialized only against the already scalar solver
/// layout consumed by the current refresh-plan boundary.
#[derive(Debug, Default)]
pub(super) struct CompactYDependencySet {
    explicit: IndexSet<usize>,
    ranges: Vec<Range<usize>>,
    affine_images: Vec<AffineYDependency>,
}

impl CompactYDependencySet {
    pub(super) fn from_explicit(explicit: IndexSet<usize>) -> Self {
        Self {
            explicit,
            ..Self::default()
        }
    }

    pub(super) fn insert(&mut self, index: usize) -> Result<(), CompactYDependencyError> {
        if self.explicit.contains(&index) {
            return Ok(());
        }
        self.explicit
            .try_reserve(1)
            .map_err(|_| CompactYDependencyError::Allocation)?;
        self.explicit.insert(index);
        Ok(())
    }

    pub(super) fn extend_explicit(
        &mut self,
        indices: impl IntoIterator<Item = usize>,
    ) -> Result<(), CompactYDependencyError> {
        for index in indices {
            self.insert(index)?;
        }
        Ok(())
    }

    pub(super) fn insert_range(
        &mut self,
        range: Range<usize>,
    ) -> Result<(), CompactYDependencyError> {
        if !range.is_empty() {
            self.ranges
                .try_reserve(1)
                .map_err(|_| CompactYDependencyError::Allocation)?;
            self.ranges.push(range);
        }
        Ok(())
    }

    pub(super) fn extend(&mut self, mut source: Self) -> Result<(), CompactYDependencyError> {
        self.explicit
            .try_reserve(source.explicit.len())
            .map_err(|_| CompactYDependencyError::Allocation)?;
        self.ranges
            .try_reserve(source.ranges.len())
            .map_err(|_| CompactYDependencyError::Allocation)?;
        self.affine_images
            .try_reserve(source.affine_images.len())
            .map_err(|_| CompactYDependencyError::Allocation)?;
        self.explicit.extend(source.explicit);
        self.ranges.append(&mut source.ranges);
        self.affine_images.append(&mut source.affine_images);
        Ok(())
    }

    pub(super) fn insert_affine<'a>(
        &mut self,
        base: usize,
        domain: &StructuredIndexDomain,
        terms: impl IntoIterator<Item = &'a AffineStencilIndexStrideTerm>,
    ) -> Result<(), CompactYDependencyError> {
        let extents = domain
            .extents()
            .map_err(|error| CompactYDependencyError::InvalidDomain(format!("{error:?}")))?;
        if extents.contains(&0) {
            return Ok(());
        }
        let mut strides = vec![0i128; extents.len()];
        for term in terms {
            let Some(stride) = strides.get_mut(term.dimension) else {
                return Err(CompactYDependencyError::InvalidDimension {
                    dimension: term.dimension,
                    rank: extents.len(),
                });
            };
            *stride = stride
                .checked_add(term.stride as i128)
                .ok_or(CompactYDependencyError::IndexOverflow)?;
        }
        if strides.iter().all(|stride| *stride == 0) {
            self.insert(base)?;
            return Ok(());
        }
        self.affine_images
            .try_reserve(1)
            .map_err(|_| CompactYDependencyError::Allocation)?;
        self.affine_images
            .push(AffineYDependency::checked(base, extents, strides)?);
        Ok(())
    }

    pub(super) fn may_contain(&self, index: usize) -> bool {
        self.explicit.contains(&index)
            || self.ranges.iter().any(|range| range.contains(&index))
            || self
                .affine_images
                .iter()
                .any(|image| image.may_contain(index))
    }

    /// Select only already-issued scalar producer identities intersecting this
    /// compact relation. This does not enumerate the tensor domain or build a
    /// scalar dependency graph; it projects onto the current scalar refresh
    /// adapter's canonical target catalog.
    pub(super) fn into_seed_stack(
        self,
        candidates: impl IntoIterator<Item = usize>,
    ) -> Result<Vec<usize>, CompactYDependencyError> {
        let mut stack = Vec::new();
        let mut seen = IndexSet::new();
        for index in candidates {
            if self.may_contain(index) && !seen.contains(&index) {
                seen.try_reserve(1)
                    .map_err(|_| CompactYDependencyError::Allocation)?;
                stack
                    .try_reserve(1)
                    .map_err(|_| CompactYDependencyError::Allocation)?;
                seen.insert(index);
                stack.push(index);
            }
        }
        Ok(stack)
    }
}

#[derive(Debug)]
struct AffineYDependency {
    base: i128,
    extents: Box<[usize]>,
    strides: Box<[i128]>,
    minimum: i128,
    maximum: i128,
    stride_gcd: u128,
}

impl AffineYDependency {
    fn checked(
        base: usize,
        extents: Vec<usize>,
        strides: Vec<i128>,
    ) -> Result<Self, CompactYDependencyError> {
        if extents.len() != strides.len() {
            return Err(CompactYDependencyError::RankMismatch);
        }
        let base = i128::try_from(base).map_err(|_| CompactYDependencyError::IndexOverflow)?;
        let mut minimum = base;
        let mut maximum = base;
        let mut stride_gcd = 0u128;
        for (extent, stride) in extents.iter().copied().zip(strides.iter().copied()) {
            let last = i128::try_from(extent.saturating_sub(1))
                .map_err(|_| CompactYDependencyError::IndexOverflow)?;
            let offset = last
                .checked_mul(stride)
                .ok_or(CompactYDependencyError::IndexOverflow)?;
            if offset < 0 {
                minimum = minimum
                    .checked_add(offset)
                    .ok_or(CompactYDependencyError::IndexOverflow)?;
            } else {
                maximum = maximum
                    .checked_add(offset)
                    .ok_or(CompactYDependencyError::IndexOverflow)?;
            }
            stride_gcd = gcd(stride_gcd, stride.unsigned_abs());
        }
        if minimum < 0 || usize::try_from(maximum).is_err() {
            return Err(CompactYDependencyError::IndexOverflow);
        }
        Ok(Self {
            base,
            extents: extents.into_boxed_slice(),
            strides: strides.into_boxed_slice(),
            minimum,
            maximum,
            stride_gcd,
        })
    }

    fn may_contain(&self, index: usize) -> bool {
        let Ok(index) = i128::try_from(index) else {
            return false;
        };
        if index < self.minimum || index > self.maximum {
            return false;
        }
        if self.strides.len() == 1 {
            return self.one_dimensional_contains(index);
        }
        let delta = index - self.base;
        self.stride_gcd == 0 || delta.unsigned_abs().is_multiple_of(self.stride_gcd)
    }

    fn one_dimensional_contains(&self, index: i128) -> bool {
        let stride = self.strides[0];
        if stride == 0 {
            return index == self.base;
        }
        let delta = index - self.base;
        if delta % stride != 0 {
            return false;
        }
        let ordinal = delta / stride;
        ordinal >= 0 && usize::try_from(ordinal).is_ok_and(|ordinal| ordinal < self.extents[0])
    }
}

fn gcd(mut lhs: u128, mut rhs: u128) -> u128 {
    while rhs != 0 {
        let remainder = lhs % rhs;
        lhs = rhs;
        rhs = remainder;
    }
    lhs
}

#[derive(Debug)]
pub(super) enum CompactYDependencyError {
    Allocation,
    IndexOverflow,
    InvalidDimension { dimension: usize, rank: usize },
    InvalidDomain(String),
    RankMismatch,
}

impl fmt::Display for CompactYDependencyError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Allocation => formatter.write_str("compact Y dependency storage exceeds memory"),
            Self::IndexOverflow => formatter.write_str("compact Y dependency index overflows"),
            Self::InvalidDimension { dimension, rank } => write!(
                formatter,
                "compact Y dependency dimension {dimension} is outside rank {rank}"
            ),
            Self::InvalidDomain(error) => {
                write!(formatter, "compact Y dependency domain is invalid: {error}")
            }
            Self::RankMismatch => {
                formatter.write_str("compact Y dependency extent/stride ranks disagree")
            }
        }
    }
}
