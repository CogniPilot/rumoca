//! Local invocation geometry, not execution/parallelism or root admission.

use super::{ComputeNode, ComputeOutputImage, ComputeOutputPlacement, ComputeSourceView};
use rumoca_core::Span;
use std::ops::ControlFlow;

#[cfg(test)]
mod tests;

/// Immutable one-dimensional dispatch limits. Production issuance is reserved
/// for the canonical Solve IR target description; that issuer is not added here.
/// This subproduct chooses no representation, fallback, ordering or ABI.
#[derive(Debug)]
pub struct DispatchBounds {
    group_size: usize,
    max_groups: usize,
}

#[cfg(test)]
impl DispatchBounds {
    pub(crate) fn construct(
        group_size: usize,
        max_group_size: usize,
        max_groups: usize,
    ) -> Result<Self, NativeDispatchError> {
        if group_size == 0 || group_size > max_group_size || max_groups == 0 {
            return Err(NativeDispatchError::InvalidBounds);
        }
        Ok(Self {
            group_size,
            max_groups,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeDispatchFamily {
    Map,
    AffineStencil,
}

/// Counts issued for an original compact native domain, without enumerating it.
#[derive(Debug)]
pub struct NativeDispatchEntry {
    node_index: usize,
    family: NativeDispatchFamily,
    invocations: usize,
    groups: usize,
    group_size: usize,
    span: Span,
}

impl NativeDispatchEntry {
    pub fn node_index(&self) -> usize {
        self.node_index
    }
    pub fn family(&self) -> NativeDispatchFamily {
        self.family
    }
    pub fn invocations(&self) -> usize {
        self.invocations
    }
    pub fn groups(&self) -> usize {
        self.groups
    }
    pub fn group_size(&self) -> usize {
        self.group_size
    }
    pub fn span(&self) -> Span {
        self.span
    }
}

/// Bound to the actual local source and immutable target resource facts.
/// It is not a proof of global root coverage, independent iterations, fault
/// scheduling or an executable target entry. Those capabilities stay required.
pub struct NativeDispatchGeometry<'source, 'target> {
    source: ComputeSourceView<'source>,
    bounds: &'target DispatchBounds,
    entries: Box<[NativeDispatchEntry]>,
}

impl NativeDispatchGeometry<'_, '_> {
    /// Read only against the actual expected source and target resource owner.
    /// Equal contents of another object do not grant this product's binding.
    pub fn checked_entries(
        &self,
        source: ComputeSourceView<'_>,
        bounds: &DispatchBounds,
    ) -> Result<&[NativeDispatchEntry], NativeDispatchError> {
        if !std::ptr::eq(self.source.source(), source.source()) {
            return Err(NativeDispatchError::ForeignSource {
                issued_at: self.entries.first().map(NativeDispatchEntry::span),
            });
        }
        if !std::ptr::eq(self.bounds, bounds) {
            return Err(NativeDispatchError::ForeignBounds);
        }
        Ok(&self.entries)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum NativeDispatchError {
    #[error("dispatch source placement refused: {0}")]
    Source(#[source] Box<super::SolveProblemShapeContractError>),
    #[error("dispatch geometry belongs to another checked compute source")]
    ForeignSource { issued_at: Option<Span> },
    #[error("dispatch geometry belongs to another target resource owner")]
    ForeignBounds,
    #[error("invalid one-dimensional target dispatch resource facts")]
    InvalidBounds,
    #[error("native domain geometry is not issued for compute node {node_index}")]
    UnsupportedNode {
        node_index: usize,
        span: Option<Span>,
    },
    #[error("compute node {node_index} requires {groups} groups, target limit {limit}")]
    GroupLimit {
        node_index: usize,
        span: Span,
        groups: usize,
        limit: usize,
    },
    #[error("dispatch geometry allocation failed: {0}")]
    Allocation(std::collections::TryReserveError),
}

impl<'source> ComputeSourceView<'source> {
    /// Issue local affine-domain counts only. Unsupported nodes refuse the
    /// complete product; no scalar fallback or empty execution is invented.
    pub fn native_dispatch_geometry<'target>(
        self,
        bounds: &'target DispatchBounds,
    ) -> Result<NativeDispatchGeometry<'source, 'target>, NativeDispatchError> {
        let mut entries = reserve_entries(self.source().nodes.len())?;
        let flow = self
            .visit_output_placements(|placement| match native_entry(&placement, bounds) {
                Ok(entry) => {
                    entries.push(entry);
                    ControlFlow::Continue(())
                }
                Err(error) => ControlFlow::Break(error),
            })
            .map_err(|error| NativeDispatchError::Source(Box::new(error)))?;
        if let ControlFlow::Break(error) = flow {
            return Err(error);
        }
        Ok(NativeDispatchGeometry {
            source: self,
            bounds,
            entries: entries.into_boxed_slice(),
        })
    }
}

fn native_entry(
    placement: &ComputeOutputPlacement<'_>,
    bounds: &DispatchBounds,
) -> Result<NativeDispatchEntry, NativeDispatchError> {
    let node_index = placement.node_index();
    let (invocations, family, span) = match (placement.node(), placement.image()) {
        (ComputeNode::Map { span, .. }, ComputeOutputImage::Affine { invocations, .. }) => {
            (*invocations, NativeDispatchFamily::Map, *span)
        }
        (
            ComputeNode::AffineStencil { span, .. },
            ComputeOutputImage::Affine { invocations, .. },
        ) => (*invocations, NativeDispatchFamily::AffineStencil, *span),
        (ComputeNode::ScalarPrograms(programs), _) => {
            return Err(NativeDispatchError::UnsupportedNode {
                node_index,
                span: programs.first_source_span(),
            });
        }
        (
            ComputeNode::Map { span, .. }
            | ComputeNode::AffineStencil { span, .. }
            | ComputeNode::MatMul { span, .. }
            | ComputeNode::LinSolve { span, .. },
            _,
        ) => {
            return Err(NativeDispatchError::UnsupportedNode {
                node_index,
                span: Some(*span),
            });
        }
    };
    // Avoid an overflowing invocations + group_size - 1 intermediate.
    let groups = invocations.div_ceil(bounds.group_size);
    if groups > bounds.max_groups {
        return Err(NativeDispatchError::GroupLimit {
            node_index,
            span,
            groups,
            limit: bounds.max_groups,
        });
    }
    Ok(NativeDispatchEntry {
        node_index,
        family,
        invocations,
        groups,
        group_size: bounds.group_size,
        span,
    })
}

fn reserve_entries(nodes: usize) -> Result<Vec<NativeDispatchEntry>, NativeDispatchError> {
    let mut entries = Vec::new();
    entries
        .try_reserve_exact(nodes)
        .map_err(NativeDispatchError::Allocation)?;
    Ok(entries)
}
