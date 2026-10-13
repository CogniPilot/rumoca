//! A dependency walk bound to one immutable issued-storage predicate.

use super::{
    ProjectionError, ProjectionRequest, ScalarCoordinateProjectionCache, dae, project_coordinates,
};

/// One dependency walk bound to its immutable issued-storage predicate.
/// Its private cache cannot be reused under another executable storage context.
pub struct PrunedScalarCoordinateProjection<'query, 'dae> {
    view: dae::DaeView<'dae>,
    omitted: &'query dyn Fn(dae::ExprId<'dae>) -> bool,
    cache: ScalarCoordinateProjectionCache<'dae>,
}

impl<'query, 'dae> PrunedScalarCoordinateProjection<'query, 'dae> {
    /// Bind one finalized DAE and one storage-leaf query for the complete walk.
    #[must_use]
    pub fn new(
        view: dae::DaeView<'dae>,
        omitted: &'query dyn Fn(dae::ExprId<'dae>) -> bool,
    ) -> Self {
        Self {
            view,
            omitted,
            cache: ScalarCoordinateProjectionCache::default(),
        }
    }

    /// Visit a scalar using the cache owned by this exact query.
    pub fn project(
        &mut self,
        root: dae::ExprId<'dae>,
        scalar_index: usize,
        domain_point: Option<(dae::DomainId<'dae>, &[i64])>,
        mut visit: impl FnMut(dae::CoordinateView<'dae>, usize),
    ) -> Result<(), ProjectionError> {
        project_coordinates(
            self.view,
            root,
            scalar_index,
            domain_point,
            &mut self.cache,
            ProjectionRequest {
                relevant: None,
                omitted: Some(self.omitted),
            },
            &mut visit,
        )
    }
}
