use super::{ScalarProgramProjection, ScalarizeError, project_checked_source};
use crate::ComputeSourceView;

/// Scalar semantic projection of one immutable checked source occurrence.
/// This does not choose a target representation or authorize scalar expansion
/// for an executable target. Those decisions require its issued target plan.
pub struct SourceScalarRefinement<'source> {
    source: ComputeSourceView<'source>,
    projection: ScalarProgramProjection,
}

#[derive(Debug, thiserror::Error)]
#[error("scalar refinement belongs to another checked compute source")]
pub struct ForeignScalarRefinementSource;

impl<'source> ComputeSourceView<'source> {
    /// Build the existing scalar semantics once, retaining actual source identity.
    pub fn scalar_refinement(self) -> Result<SourceScalarRefinement<'source>, ScalarizeError> {
        Ok(SourceScalarRefinement {
            source: self,
            projection: project_checked_source(self.source())?,
        })
    }
}

impl SourceScalarRefinement<'_> {
    pub fn checked_projection(
        &self,
        source: ComputeSourceView<'_>,
    ) -> Result<&ScalarProgramProjection, ForeignScalarRefinementSource> {
        check_source(self.source, source)?;
        Ok(&self.projection)
    }

    pub(super) fn into_projection(self) -> ScalarProgramProjection {
        self.projection
    }
}

pub(super) fn check_source(
    issued: ComputeSourceView<'_>,
    expected: ComputeSourceView<'_>,
) -> Result<(), ForeignScalarRefinementSource> {
    if !std::ptr::eq(issued.source(), expected.source()) {
        return Err(ForeignScalarRefinementSource);
    }
    Ok(())
}
