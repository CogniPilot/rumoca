//! Compact periodic blocks; a bounding span never owns its gaps.

use super::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct Coverage {
    pub(super) span: Range<usize>,
    pub(super) count: usize,
    pub(super) stride: usize,
    pub(super) width: usize,
}

impl Coverage {
    pub(super) fn dense(span: Range<usize>) -> Self {
        Self {
            count: span.len(),
            span,
            stride: 1,
            width: 1,
        }
    }

    pub(super) fn overlaps(&self, other: &Self) -> Checked<bool> {
        let low = self.span.start.max(other.span.start) as i128;
        let high = self.span.end.min(other.span.end) as i128;
        if low >= high {
            return Ok(false);
        }
        if self.stride == 1 {
            return Ok(other.touches(low as usize..high as usize));
        }
        if other.stride == 1 {
            return Ok(self.touches(low as usize..high as usize));
        }
        if self.width > 1 || other.width > 1 {
            return Ok(self.block_overlap(other));
        }
        self.progression_overlap(other, low, high)
    }

    fn touches(&self, range: Range<usize>) -> bool {
        let low = range.start.max(self.span.start);
        let high = range.end.min(self.span.end);
        if low >= high {
            return false;
        }
        let offset = (low - self.span.start) % self.stride;
        offset < self.width
            || low
                .checked_add(self.stride - offset)
                .is_some_and(|next| next < high)
    }

    fn block_overlap(&self, other: &Self) -> bool {
        let (blocks, queried) = if self.count / self.width <= other.count / other.width {
            (self, other)
        } else {
            (other, self)
        };
        (0..blocks.count / blocks.width).any(|ordinal| {
            let start = blocks.span.start + ordinal * blocks.stride;
            queried.touches(start..start + blocks.width)
        })
    }

    fn progression_overlap(&self, other: &Self, low: i128, high: i128) -> Checked<bool> {
        let a = self.stride as i128;
        let b = other.stride as i128;
        let (gcd, inverse) = extended_gcd(a, b);
        let difference = other.span.start as i128 - self.span.start as i128;
        if difference % gcd != 0 {
            return Ok(false);
        }
        let modulus = b / gcd;
        let ordinal = (difference / gcd)
            .checked_mul(inverse)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native target intersection overflows",
            ))?
            .rem_euclid(modulus);
        let first = (self.span.start as i128)
            .checked_add(
                a.checked_mul(ordinal)
                    .ok_or(NativeRefreshAssignmentRefusal(
                        "native target intersection overflows",
                    ))?,
            )
            .ok_or(NativeRefreshAssignmentRefusal(
                "native target intersection overflows",
            ))?;
        let period = a
            .checked_mul(modulus)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native target intersection overflows",
            ))?;
        let next = first
            .checked_add((low - first).div_euclid(period).checked_mul(period).ok_or(
                NativeRefreshAssignmentRefusal("native target intersection overflows"),
            )?)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native target intersection overflows",
            ))?;
        Ok(if next < low {
            next.checked_add(period).is_some_and(|n| n < high)
        } else {
            next < high
        })
    }
}

fn extended_gcd(mut a: i128, mut b: i128) -> (i128, i128) {
    let (mut x, mut next_x) = (1, 0);
    while b != 0 {
        let quotient = a / b;
        (a, b) = (b, a % b);
        (x, next_x) = (next_x, x - quotient * next_x);
    }
    (a, x)
}

pub(super) fn complete<'a>(
    sets: impl Iterator<Item = &'a Coverage>,
    count: usize,
) -> Checked<bool> {
    let sets = sets.collect::<Vec<_>>();
    let spans = super::span_index::SpanIndex::new(sets.iter().map(|set| set.span.clone()));
    let mut cardinality = 0usize;
    for (index, set) in sets.iter().enumerate() {
        if set.count == 0
            || set.stride == 0
            || set.width == 0
            || set.width > set.stride
            || set.count % set.width != 0
            || set.span.end > count
        {
            return Ok(false);
        }
        cardinality = cardinality
            .checked_add(set.count)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native target cardinality overflows",
            ))?;
        let mut candidates = Vec::new();
        spans.visit(&set.span, &mut |previous| {
            if previous < index {
                candidates.push(previous);
            }
        });
        candidates.sort_unstable();
        for previous in candidates {
            if set.overlaps(sets[previous])? {
                return Ok(false);
            }
        }
    }
    Ok(cardinality == count)
}

/// Derive exact periodic blocks for affine axes with at most one gap, otherwise retain the
/// conservative bounding read span. This never enlarges a target's ownership.
pub(super) fn reads(
    span: Range<usize>,
    terms: &[crate::AffineStencilIndexStrideTerm],
    extents: &[usize],
) -> Checked<Coverage> {
    let mut axes = vec![0i128; extents.len()];
    for term in terms {
        axes[term.dimension] = axes[term.dimension]
            .checked_add(term.stride as i128)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native read stride overflows",
            ))?;
    }
    let mut axes = axes
        .into_iter()
        .zip(extents)
        .filter(|(stride, extent)| *stride != 0 && **extent > 1)
        .map(|(stride, extent)| (stride.abs(), *extent))
        .collect::<Vec<_>>();
    axes.sort_unstable();
    let (mut width, mut blocks, mut stride, mut gap) = (1usize, 1usize, 1i128, false);
    for (axis_stride, extent) in axes {
        let expected = if gap {
            stride.checked_mul(blocks as i128)
        } else {
            Some(width as i128)
        }
        .ok_or(NativeRefreshAssignmentRefusal(
            "native read stride overflows",
        ))?;
        if axis_stride == expected {
            let count = if gap { &mut blocks } else { &mut width };
            *count = count
                .checked_mul(extent)
                .ok_or(NativeRefreshAssignmentRefusal(
                    "native read cardinality overflows",
                ))?;
        } else if !gap && axis_stride > width as i128 {
            (stride, blocks, gap) = (axis_stride, extent, true);
        } else {
            return Ok(Coverage::dense(span));
        }
    }
    if !gap {
        return Ok(Coverage::dense(span));
    }
    let stride = usize::try_from(stride)
        .map_err(|_| NativeRefreshAssignmentRefusal("native read stride overflows"))?;
    let count = width
        .checked_mul(blocks)
        .ok_or(NativeRefreshAssignmentRefusal(
            "native read cardinality overflows",
        ))?;
    Ok(Coverage {
        span,
        count,
        stride,
        width,
    })
}
