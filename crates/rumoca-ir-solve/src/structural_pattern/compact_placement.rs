//! Compact, one-to-one placement geometry; no domain-point expansion.
use super::{
    AffineColumnMap, PatternDerivation, PatternProvenance, PatternRepresentation,
    StructuralPattern, StructuralPatternError, checked_dimension, dependency_error,
    stacked::{PatternSegment, SegmentRows},
};
use crate::CompactJacobianPlacement;
use rumoca_core::{IndexBinderProvenance, Span, StructuredIndexBinder, StructuredIndexDomain};

impl CompactJacobianPlacement {
    pub(crate) fn derive_placement_relation(
        &self,
        span: Span,
    ) -> Result<StructuralPattern, StructuralPatternError> {
        let bad = || {
            dependency_error(
                "compact placement geometry is not representable",
                Some(span),
            )
        };
        let states = checked_dimension(self.states)?;
        let height = self
            .extents
            .iter()
            .try_fold(1usize, |n, &e| n.checked_mul(e as usize))
            .ok_or_else(bad)?;
        let mut start = i128::from(self.row.base);
        let mut strides = Vec::with_capacity(self.extents.len());
        for (&stride, &extent) in self.row.strides.iter().zip(&self.extents) {
            if stride < 0 {
                start += i128::from(stride) * i128::from(extent - 1);
            }
            strides.push(u32::try_from(stride.unsigned_abs()).map_err(|_| bad())?);
        }
        let rows = SegmentRows::Strided {
            start: u32::try_from(start).map_err(|_| bad())?,
            extents: self.extents.clone().into_boxed_slice(),
            strides: strides.into_boxed_slice(),
        };
        rows.validate(height, Some(span))?;
        let domain = StructuredIndexDomain {
            binders: self
                .extents
                .iter()
                .enumerate()
                .map(|(axis, &extent)| {
                    Ok(StructuredIndexBinder {
                        provenance: IndexBinderProvenance::Generated,
                        id: axis,
                        display_name: format!("ordinal_{axis}"),
                        lower: 0,
                        upper: i64::from(extent) - 1,
                        step: 1,
                    })
                })
                .collect::<Result<Vec<_>, StructuralPatternError>>()?,
        };
        let columns = self
            .columns
            .iter()
            .map(|column| {
                normalize_column(
                    column.base,
                    &column.strides,
                    &self.row.strides,
                    &self.extents,
                    span,
                )
            })
            .collect::<Result<Vec<_>, StructuralPatternError>>()?;
        let provenance = PatternProvenance::derived(PatternDerivation::AffineDomain, span)?;
        let pattern = StructuralPattern::checked_affine(
            height,
            self.states,
            domain,
            0,
            columns.into_boxed_slice(),
            provenance,
        )?;
        // The placement constructor already proved global bounds. The nested
        // row proof above establishes injectivity without a states-sized bitmap.
        Ok(StructuralPattern::from_checked_parts(
            states,
            states,
            PatternRepresentation::Stacked {
                segments: vec![PatternSegment { rows, pattern }].into_boxed_slice(),
            },
            provenance,
        ))
    }
}

fn normalize_column(
    base: i64,
    column: &[i64],
    row: &[i64],
    extents: &[u32],
    span: Span,
) -> Result<AffineColumnMap, StructuralPatternError> {
    let bad = || dependency_error("compact column normalization overflows", Some(span));
    let mut base = i128::from(base);
    let mut strides = Vec::with_capacity(extents.len());
    for ((&stride, &row_stride), &extent) in column.iter().zip(row).zip(extents) {
        let normalized = if row_stride < 0 {
            base += i128::from(stride) * i128::from(extent - 1);
            -i128::from(stride)
        } else {
            i128::from(stride)
        };
        strides.push(isize::try_from(normalized).map_err(|_| bad())?);
    }
    Ok(AffineColumnMap {
        start: u32::try_from(base).map_err(|_| bad())?,
        strides: strides.into_boxed_slice(),
    })
}
