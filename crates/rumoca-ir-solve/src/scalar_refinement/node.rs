use super::{
    ForeignScalarRefinementSource, ScalarProgramCollector, ScalarProgramProjection, ScalarizeError,
    bound::check_source, finish_projection,
};
use crate::{ComputeSourceView, tensor::advance_compute_output_cursor};

#[cfg(test)]
mod tests;

/// Exact scalar semantics of one original node with its global output placement.
/// This is not a target representation selection or resource admission.
pub struct SourceNodeScalarRefinement<'source> {
    source: ComputeSourceView<'source>,
    node_index: usize,
    projection: ScalarProgramProjection,
}

#[derive(Debug, thiserror::Error)]
pub enum SourceNodeRefinementError {
    #[error("compute node {node_index} is absent from the checked source")]
    UnknownNode { node_index: usize },
    #[error("scalar refinement was issued for node {issued}, expected {expected}")]
    ForeignNode { issued: usize, expected: usize },
    #[error(transparent)]
    ForeignSource(#[from] ForeignScalarRefinementSource),
    #[error(transparent)]
    Scalar(#[from] ScalarizeError),
}

impl<'source> ComputeSourceView<'source> {
    /// Refine only the named original node using the canonical scalar collector.
    /// Prefix cursor facts stay compact; preceding nodes are never scalarized.
    pub fn scalar_node_refinement(
        self,
        node_index: usize,
    ) -> Result<SourceNodeScalarRefinement<'source>, SourceNodeRefinementError> {
        let node = self
            .source()
            .nodes
            .get(node_index)
            .ok_or(SourceNodeRefinementError::UnknownNode { node_index })?;
        let cursor = self.source().nodes[..node_index]
            .iter()
            .enumerate()
            .try_fold(0, |cursor, (index, prefix)| {
                advance_compute_output_cursor("scalar node refinement", index, prefix, cursor)
            })
            .map_err(ScalarizeError::from)?;
        let mut collector = ScalarProgramCollector {
            next_output: cursor,
            ..ScalarProgramCollector::default()
        };
        collector.append_compute_node(node_index, node)?;
        Ok(SourceNodeScalarRefinement {
            source: self,
            node_index,
            projection: finish_projection(collector)?,
        })
    }
}

impl SourceNodeScalarRefinement<'_> {
    /// The consumer must name both the expected source and original node.
    pub fn checked_projection(
        &self,
        source: ComputeSourceView<'_>,
        node_index: usize,
    ) -> Result<&ScalarProgramProjection, SourceNodeRefinementError> {
        check_source(self.source, source)?;
        if node_index != self.node_index {
            return Err(SourceNodeRefinementError::ForeignNode {
                issued: self.node_index,
                expected: node_index,
            });
        }
        Ok(&self.projection)
    }
}
