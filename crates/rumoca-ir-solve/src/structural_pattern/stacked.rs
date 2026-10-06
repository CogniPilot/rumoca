//! Stacked structural patterns: the exact relation of a compute block whose
//! nodes own disjoint sets of output rows (SPEC_0039 canonical pattern
//! `Stacked`).
//!
//! Each node of a checked compute-block JVP writes its own output rows and
//! reads no other node's outputs, so the block's relation is the disjoint
//! union of the per-node relations, each placed on its node's rows. An affine
//! tensor node keeps its compact `Affine` relation over its domain ordinals
//! and places them through its affine output map, so the stack stays
//! proportional to the node count and the scalar rows, never to a tensor
//! domain.

use rumoca_core::{Span, StructuredIndexDomain};
use serde::{Deserialize, Serialize};

use super::{
    PatternDerivation, PatternProvenance, PatternRepresentation, StructuralPattern,
    StructuralPatternError, StructuralPatternWire, checked_dimension, dependency_error,
    fill_scalar_jvp_rows,
};
use crate::{ComputeBlock, ComputeNode, TensorOutputMap};

/// Where the local rows of one segment sit among the stacked rows.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
pub(super) enum SegmentRows {
    /// Local row `k` is row `start + k`.
    Contiguous { start: u32 },
    /// Local row `k` is the `k`-th point of a row-major domain with
    /// `extents`, placed at `start + sum(strides[d] * position[d])`. The
    /// strides, ordered by size, nest (each at least the span of the smaller
    /// ones), so every row has at most one point.
    Strided {
        start: u32,
        extents: Box<[u32]>,
        strides: Box<[u32]>,
    },
    /// Local row `k` is `rows[k]`; the rows strictly increase.
    Listed { rows: Box<[u32]> },
}

impl SegmentRows {
    /// The stacked row of local row `local`.
    fn row_of(&self, local: usize) -> usize {
        match self {
            Self::Contiguous { start } => *start as usize + local,
            Self::Strided {
                start,
                extents,
                strides,
            } => {
                let mut remainder = local;
                let mut row = *start as usize;
                for (extent, stride) in extents.iter().zip(strides.iter()).rev() {
                    row += (remainder % *extent as usize) * *stride as usize;
                    remainder /= *extent as usize;
                }
                row
            }
            Self::Listed { rows } => rows[local] as usize,
        }
    }

    /// The local row placed at stacked row `row`, if this segment owns it.
    fn local_of(&self, row: usize, height: usize) -> Option<usize> {
        match self {
            Self::Contiguous { start } => {
                let local = row.checked_sub(*start as usize)?;
                (local < height).then_some(local)
            }
            Self::Strided {
                start,
                extents,
                strides,
            } => strided_local(row.checked_sub(*start as usize)?, extents, strides),
            Self::Listed { rows } => rows.binary_search(&u32::try_from(row).ok()?).ok(),
        }
    }

    /// Check the map's own shape against the segment height.
    fn validate(&self, height: usize, span: Option<Span>) -> Result<(), StructuralPatternError> {
        let consistent = match self {
            Self::Contiguous { .. } => true,
            Self::Strided {
                extents, strides, ..
            } => {
                extents.len() == strides.len()
                    && extents
                        .iter()
                        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
                        == Some(height)
                    && strides_nest(extents, strides)
            }
            Self::Listed { rows } => {
                rows.len() == height && rows.windows(2).all(|pair| pair[0] < pair[1])
            }
        };
        if consistent {
            Ok(())
        } else {
            Err(dependency_error(
                "a stacked segment's row map does not place its rows one to one",
                span,
            ))
        }
    }
}

/// Dimensions ordered by stride, skipping extent-1 dimensions.
fn nested_order(extents: &[u32], strides: &[u32]) -> Vec<usize> {
    let mut order = (0..extents.len())
        .filter(|&dimension| extents[dimension] > 1)
        .collect::<Vec<_>>();
    order.sort_by_key(|&dimension| strides[dimension]);
    order
}

/// Whether every stride is at least the span the smaller strides cover, so
/// the placement is one to one and invertible digit by digit.
fn strides_nest(extents: &[u32], strides: &[u32]) -> bool {
    let mut span = 1u64;
    for dimension in nested_order(extents, strides) {
        let stride = u64::from(strides[dimension]);
        if stride < span {
            return false;
        }
        span += stride * u64::from(extents[dimension] - 1);
    }
    true
}

/// The row-major ordinal of the point placed at `offset`, if any.
fn strided_local(offset: usize, extents: &[u32], strides: &[u32]) -> Option<usize> {
    let mut remainder = offset;
    let mut positions = vec![0usize; extents.len()];
    for dimension in nested_order(extents, strides).into_iter().rev() {
        let position = remainder / strides[dimension] as usize;
        if position >= extents[dimension] as usize {
            return None;
        }
        positions[dimension] = position;
        remainder -= position * strides[dimension] as usize;
    }
    if remainder != 0 {
        return None;
    }
    Some(
        positions
            .iter()
            .zip(extents.iter())
            .fold(0usize, |ordinal, (position, extent)| {
                ordinal * *extent as usize + position
            }),
    )
}

/// One segment of a stacked pattern: `pattern` relates its local rows, which
/// `rows` places among the stacked rows, to every column.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub(super) struct PatternSegment {
    rows: SegmentRows,
    pattern: StructuralPattern,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct PatternSegmentWire {
    rows: SegmentRows,
    pattern: StructuralPatternWire,
}

impl PatternSegmentWire {
    pub(super) fn replay(self) -> Result<PatternSegment, StructuralPatternError> {
        Ok(PatternSegment {
            rows: self.rows,
            pattern: self.pattern.replay()?,
        })
    }
}

impl PatternSegment {
    fn height(&self) -> usize {
        self.pattern.rows as usize
    }

    /// Check that the segment is flat, covers `columns`, and places its rows one
    /// to one onto rows no earlier segment claimed in `owned`.
    fn validate(
        &self,
        columns: u32,
        owned: &mut [bool],
        span: Option<Span>,
    ) -> Result<(), StructuralPatternError> {
        if self.pattern.columns != columns {
            return Err(dependency_error(
                "a stacked segment does not cover the pattern's columns",
                span,
            ));
        }
        if matches!(
            self.pattern.representation,
            PatternRepresentation::Stacked { .. }
        ) {
            return Err(dependency_error(
                "a stacked segment is itself stacked",
                span,
            ));
        }
        self.rows.validate(self.height(), span)?;
        let rows = owned.len();
        for local in 0..self.height() {
            let row = self.rows.row_of(local);
            let slot = owned.get_mut(row).filter(|slot| !**slot).ok_or_else(|| {
                dependency_error(
                    format!("stacked row {row} is outside 0..{rows} or owned twice"),
                    span,
                )
            })?;
            *slot = true;
        }
        Ok(())
    }

    fn local_of(&self, row: usize) -> Option<usize> {
        self.rows.local_of(row, self.height())
    }
}

/// The segment owning `row`, with the row's local index inside it.
fn segment_for_row(segments: &[PatternSegment], row: usize) -> Option<(&PatternSegment, usize)> {
    segments
        .iter()
        .find_map(|segment| segment.local_of(row).map(|local| (segment, local)))
}

pub(super) fn contains(segments: &[PatternSegment], row: u32, column: u32) -> bool {
    segment_for_row(segments, row as usize)
        .is_some_and(|(segment, local)| segment.pattern.contains(local as u32, column))
}

pub(super) fn nonzero_upper_bound(segments: &[PatternSegment]) -> Option<usize> {
    segments.iter().try_fold(0usize, |total, segment| {
        total.checked_add(segment.pattern.nonzero_upper_bound()?)
    })
}

pub(super) fn visit_row_columns(
    segments: &[PatternSegment],
    row: usize,
    visitor: &mut dyn FnMut(usize),
) {
    if let Some((segment, local)) = segment_for_row(segments, row) {
        segment.pattern.visit_row_columns(local, visitor);
    }
}

pub(super) fn append_column_rows(columns: &mut [Vec<usize>], segments: &[PatternSegment]) {
    for segment in segments {
        for (column, rows) in segment.pattern.column_rows().into_iter().enumerate() {
            columns[column].extend(rows.into_iter().map(|local| segment.rows.row_of(local)));
        }
    }
    for rows in columns {
        rows.sort_unstable();
    }
}

impl StructuralPattern {
    /// Rebuild a stacked pattern, checking that every segment is flat, covers
    /// the pattern's columns, places its rows one to one inside `rows`, and
    /// shares no row with another segment.
    pub(super) fn checked_stacked(
        rows: usize,
        columns: usize,
        segments: Vec<PatternSegment>,
        provenance: PatternProvenance,
    ) -> Result<Self, StructuralPatternError> {
        let checked_rows = checked_dimension(rows)?;
        let checked_columns = checked_dimension(columns)?;
        let span = Some(provenance.span());
        let mut owned = vec![false; rows];
        for segment in &segments {
            segment.validate(checked_columns, &mut owned, span)?;
        }
        Ok(Self {
            rows: checked_rows,
            columns: checked_columns,
            representation: PatternRepresentation::Stacked {
                segments: segments.into_boxed_slice(),
            },
            provenance,
        })
    }

    /// Derive the exact structural relation of a checked compute-block JVP
    /// whose nodes own disjoint output rows, without materializing a tensor
    /// domain.
    ///
    /// SPEC_0039 / SOLVE-C17: every segment is issued by this module's own
    /// derivations over the node that owns it: an affine tensor node by
    /// [`StructuralPattern::derive_from_affine_jvp`] over its compact domain,
    /// placed through its output map, a scalar-program node by the
    /// register-dependency walk, and a matrix product or linear solve
    /// conservatively as `Full` over its rows.
    pub fn derive_from_compute_jvp(
        block: &ComputeBlock,
        rows: usize,
        columns: usize,
        owner_span: Span,
    ) -> Result<Self, StructuralPatternError> {
        let mut segments = Vec::with_capacity(block.nodes.len());
        let mut cursor = 0usize;
        for (node_index, node) in block.nodes.iter().enumerate() {
            let (segment, next) = node_segment(node, node_index, cursor, columns, owner_span)?;
            cursor = next;
            segments.extend(segment);
        }
        if cursor != rows {
            return Err(dependency_error(
                format!("compute-block Jacobian produces {cursor} rows, expected {rows}"),
                Some(owner_span),
            ));
        }
        if let [segment] = segments.as_slice()
            && segment.rows == (SegmentRows::Contiguous { start: 0 })
            && segment.height() == rows
        {
            return Ok(segment.pattern.clone());
        }
        let provenance =
            PatternProvenance::derived(PatternDerivation::DependencyPropagation, owner_span)?;
        Self::checked_stacked(rows, columns, segments, provenance)
    }
}

/// The segment one node owns (none for a node without outputs) and the output
/// cursor after it, following [`ComputeBlock::output_count`].
fn node_segment(
    node: &ComputeNode,
    node_index: usize,
    cursor: usize,
    columns: usize,
    owner_span: Span,
) -> Result<(Option<PatternSegment>, usize), StructuralPatternError> {
    match node {
        ComputeNode::ScalarPrograms(block) => {
            let shape_error = |error: crate::SolveProblemShapeContractError| {
                dependency_error(format!("{error:?}"), Some(owner_span))
            };
            let context = "compute-block Jacobian sparsity";
            let outputs = block
                .compute_block_output_indices(context, node_index, cursor)
                .map_err(shape_error)?;
            let next = block
                .advance_compute_block_output_cursor(context, node_index, cursor)
                .map_err(shape_error)?;
            Ok((scalar_segment(block, &outputs, columns, owner_span)?, next))
        }
        ComputeNode::Map {
            domain,
            output_map,
            base_ops,
            load_strides,
            span,
            ..
        }
        | ComputeNode::AffineStencil {
            domain,
            output_map,
            base_ops,
            load_strides,
            span,
            ..
        } => {
            let owner = source_span(*span, owner_span);
            let end = output_map.output_count(domain).map_err(|error| {
                dependency_error(format!("invalid affine output map: {error:?}"), Some(owner))
            })?;
            let next = cursor.max(end);
            let points = domain.scalar_count().map_err(|error| {
                dependency_error(format!("invalid affine domain: {error}"), Some(owner))
            })?;
            if points == 0 {
                return Ok((None, next));
            }
            let local_map = TensorOutputMap::dense_contiguous(0, domain).map_err(|error| {
                dependency_error(format!("invalid affine domain: {error:?}"), Some(owner))
            })?;
            let pattern = StructuralPattern::derive_from_affine_jvp(
                domain,
                &local_map,
                base_ops,
                load_strides,
                points,
                columns,
                owner,
            )?;
            let rows = affine_rows(domain, output_map, owner)?;
            Ok((Some(PatternSegment { rows, pattern }), next))
        }
        ComputeNode::MatMul { m, n, span, .. } => dense_node_segment(
            cursor,
            m.checked_mul(*n),
            columns,
            source_span(*span, owner_span),
        ),
        ComputeNode::LinSolve { n, span, .. } => {
            dense_node_segment(cursor, Some(*n), columns, source_span(*span, owner_span))
        }
    }
}

/// A scalar-program node's rows, listed in ascending order, with the
/// register-dependency relation of each.
fn scalar_segment(
    block: &crate::ScalarProgramBlock,
    outputs: &[usize],
    columns: usize,
    owner_span: Span,
) -> Result<Option<PatternSegment>, StructuralPatternError> {
    if outputs.is_empty() {
        return Ok(None);
    }
    let mut rows = outputs
        .iter()
        .map(|&row| checked_dimension(row))
        .collect::<Result<Vec<_>, _>>()?;
    rows.sort_unstable();
    rows.dedup();
    let local = outputs
        .iter()
        .map(|&row| rows.binary_search(&(row as u32)).unwrap_or(usize::MAX))
        .collect::<Vec<_>>();
    let mut slots = vec![None; rows.len()];
    fill_scalar_jvp_rows(block, &local, columns, owner_span, &mut slots)?;
    let dependencies = slots
        .into_iter()
        .map(Option::unwrap_or_default)
        .collect::<Vec<_>>();
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, owner_span)?;
    let pattern = StructuralPattern::from_checked_row_dependencies(
        rows.len(),
        columns,
        &dependencies,
        provenance,
    )?;
    let rows = match rows.first() {
        Some(&start) if rows.last().map(|&last| last - start + 1) == Some(rows.len() as u32) => {
            SegmentRows::Contiguous { start }
        }
        _ => SegmentRows::Listed {
            rows: rows.into_boxed_slice(),
        },
    };
    Ok(Some(PatternSegment { rows, pattern }))
}

/// Where an affine node's row-major domain points land: contiguous for a
/// dense map, strided for a nested positive map, listed otherwise.
fn affine_rows(
    domain: &StructuredIndexDomain,
    output_map: &TensorOutputMap,
    span: Span,
) -> Result<SegmentRows, StructuralPatternError> {
    let invalid = |error: String| dependency_error(error, Some(span));
    let start = checked_dimension(output_map.start)?;
    if TensorOutputMap::dense_contiguous(output_map.start, domain).as_ref() == Ok(output_map) {
        return Ok(SegmentRows::Contiguous { start });
    }
    let extents = domain
        .extents()
        .map_err(|error| invalid(format!("invalid affine domain: {error}")))?
        .into_iter()
        .map(checked_dimension)
        .collect::<Result<Box<[_]>, _>>()?;
    let mut strides = vec![0isize; extents.len()];
    for term in &output_map.strides {
        let stride = strides.get_mut(term.dimension).ok_or_else(|| {
            invalid(format!(
                "output stride dimension {} is outside the domain",
                term.dimension
            ))
        })?;
        *stride = stride
            .checked_add(term.stride)
            .ok_or_else(|| invalid("output stride overflows".to_string()))?;
    }
    if let Ok(strides) = strides
        .iter()
        .map(|&stride| u32::try_from(stride))
        .collect::<Result<Box<[_]>, _>>()
        && strides_nest(&extents, &strides)
    {
        return Ok(SegmentRows::Strided {
            start,
            extents,
            strides,
        });
    }
    // A map whose placement is not a nested lattice (a reversed or
    // interleaved axis) lists its rows in point order.
    let rows = output_map
        .output_indices(domain)
        .map_err(|error| invalid(format!("invalid affine output map: {error:?}")))?
        .into_iter()
        .map(checked_dimension)
        .collect::<Result<Vec<_>, _>>()?;
    if rows.windows(2).all(|pair| pair[0] < pair[1]) {
        return Ok(SegmentRows::Listed {
            rows: rows.into_boxed_slice(),
        });
    }
    Err(invalid(
        "affine output map does not place its points in increasing rows".to_string(),
    ))
}

/// `span` when it is source-backed, else the block owner's.
fn source_span(span: Span, owner_span: Span) -> Span {
    if span.is_dummy() { owner_span } else { span }
}

/// A matrix product or linear solve: every output row may read every column
/// (SPEC_0039 conservative `Full`), over the rows it appends at `cursor`.
fn dense_node_segment(
    cursor: usize,
    count: Option<usize>,
    columns: usize,
    span: Span,
) -> Result<(Option<PatternSegment>, usize), StructuralPatternError> {
    let overflow = || dependency_error("dense node output count overflows", Some(span));
    let count = count.ok_or_else(overflow)?;
    let next = cursor.checked_add(count).ok_or_else(overflow)?;
    if count == 0 {
        return Ok((None, next));
    }
    let provenance = PatternProvenance::derived(PatternDerivation::ConservativeFull, span)?;
    let segment = PatternSegment {
        rows: SegmentRows::Contiguous {
            start: checked_dimension(cursor)?,
        },
        pattern: StructuralPattern::full(count, columns, provenance)?,
    };
    Ok((Some(segment), next))
}
