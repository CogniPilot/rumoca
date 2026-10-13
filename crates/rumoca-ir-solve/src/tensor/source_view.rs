#[cfg(test)]
mod tests;

use super::*;
use std::ops::{ControlFlow, Range};

/// Borrowed local shape and placement facts for one actual compute aggregate.
/// Enclosing layout, table, and root coverage remain the root constructor's obligations.
#[derive(Clone, Copy, Debug)]
pub struct ComputeSourceView<'source> {
    source: &'source ComputeBlock,
    context: &'static str,
    output_count: usize,
}

/// Exact output placement, with affine holes retained by the canonical map.
#[derive(Clone, Debug)]
pub enum ComputeOutputImage<'source> {
    Contiguous(Range<usize>),
    Explicit(&'source [usize]),
    ProgramRanges(&'source [ScalarProgramOutputPlacement]),
    Affine {
        domain: &'source StructuredIndexDomain,
        output_map: &'source TensorOutputMap,
        kind: &'static str,
        span: Span,
    },
}

/// A placement query keeps the actual node and its position in the borrowed source.
#[derive(Clone, Debug)]
pub struct ComputeOutputPlacement<'source> {
    node_index: usize,
    node: &'source ComputeNode,
    image: ComputeOutputImage<'source>,
}

impl ComputeBlock {
    pub fn checked_source_view(
        &self,
        context: &'static str,
    ) -> Result<ComputeSourceView<'_>, SolveProblemShapeContractError> {
        self.validate_shape_contract(context)?;
        let output_count = self.output_count(context)?;
        Ok(ComputeSourceView {
            source: self,
            context,
            output_count,
        })
    }
}

impl<'source> ComputeSourceView<'source> {
    pub fn source(self) -> &'source ComputeBlock {
        self.source
    }

    pub fn output_count(self) -> usize {
        self.output_count
    }

    /// Visits one compact placement per node in source order, without sorting or deduplication.
    pub fn visit_output_placements<B>(
        self,
        mut visitor: impl FnMut(ComputeOutputPlacement<'source>) -> ControlFlow<B>,
    ) -> Result<ControlFlow<B>, SolveProblemShapeContractError> {
        let mut cursor = 0;
        for (node_index, node) in self.source.nodes.iter().enumerate() {
            let (image, next) = output_placement(self.context, node_index, node, cursor)?;
            let placement = ComputeOutputPlacement {
                node_index,
                node,
                image,
            };
            if let ControlFlow::Break(value) = visitor(placement) {
                return Ok(ControlFlow::Break(value));
            }
            cursor = next;
        }
        Ok(ControlFlow::Continue(()))
    }
}

impl<'source> ComputeOutputPlacement<'source> {
    pub fn node_index(&self) -> usize {
        self.node_index
    }

    pub fn node(&self) -> &'source ComputeNode {
        self.node
    }

    pub fn image(&self) -> &ComputeOutputImage<'source> {
        &self.image
    }

    pub(super) fn append_indices(
        &self,
        outputs: &mut BTreeSet<usize>,
        context: &'static str,
    ) -> Result<(), SolveProblemShapeContractError> {
        match &self.image {
            ComputeOutputImage::Contiguous(range) => outputs.extend(range.clone()),
            ComputeOutputImage::Explicit(indices) => outputs.extend(indices.iter().copied()),
            ComputeOutputImage::ProgramRanges(entries) => {
                for entry in *entries {
                    entry.extend_materialized_indices(outputs);
                }
            }
            ComputeOutputImage::Affine {
                domain,
                output_map,
                kind,
                span,
            } => {
                append_tensor_output_indices(
                    outputs,
                    output_map,
                    domain,
                    TensorOutputContext {
                        context,
                        node_index: self.node_index,
                        kind,
                        span: *span,
                    },
                )?;
            }
        }
        Ok(())
    }
}

pub(crate) fn advance_compute_output_cursor(
    context: &'static str,
    node_index: usize,
    node: &ComputeNode,
    cursor: usize,
) -> Result<usize, SolveProblemShapeContractError> {
    output_placement(context, node_index, node, cursor).map(|(_, next)| next)
}

fn output_placement<'source>(
    context: &'static str,
    node_index: usize,
    node: &'source ComputeNode,
    cursor: usize,
) -> Result<(ComputeOutputImage<'source>, usize), SolveProblemShapeContractError> {
    let (image, next) = match node {
        ComputeNode::ScalarPrograms(block) => {
            let next = block.advance_compute_block_output_cursor(context, node_index, cursor)?;
            let image = match block.output_placements() {
                ScalarOutputPlacementView::Dense { .. } => {
                    ComputeOutputImage::Contiguous(cursor..next)
                }
                ScalarOutputPlacementView::Explicit(indices) => {
                    ComputeOutputImage::Explicit(indices)
                }
                ScalarOutputPlacementView::ProgramRanges(entries) => {
                    ComputeOutputImage::ProgramRanges(entries)
                }
            };
            (image, next)
        }
        ComputeNode::Map {
            domain,
            output_map,
            span,
            ..
        } => affine_placement(
            context, node_index, cursor, domain, output_map, "Map", *span,
        )?,
        ComputeNode::AffineStencil {
            domain,
            output_map,
            span,
            ..
        } => affine_placement(
            context,
            node_index,
            cursor,
            domain,
            output_map,
            "AffineStencil",
            *span,
        )?,
        ComputeNode::MatMul { m, n, span, .. } => {
            let count = m
                .checked_mul(*n)
                .ok_or_else(|| output_index_overflow(context, node_index, Some(*span)))?;
            contiguous_placement(context, node_index, cursor, count, *span)?
        }
        ComputeNode::LinSolve { n, span, .. } => {
            contiguous_placement(context, node_index, cursor, *n, *span)?
        }
    };
    Ok((image, next))
}

fn contiguous_placement(
    context: &'static str,
    node_index: usize,
    cursor: usize,
    count: usize,
    span: Span,
) -> Result<(ComputeOutputImage<'static>, usize), SolveProblemShapeContractError> {
    let next = cursor
        .checked_add(count)
        .ok_or_else(|| output_index_overflow(context, node_index, Some(span)))?;
    Ok((ComputeOutputImage::Contiguous(cursor..next), next))
}
