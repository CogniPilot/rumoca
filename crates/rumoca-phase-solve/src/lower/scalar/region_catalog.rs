//! The capture catalog of a conditional region that contains another region.
//!
//! A region lists the symbolic points and fold tuples of the enclosing folds
//! as capture sources and loads only those it reads. A region nested in that
//! region sees them through it: each scalar of the outer catalog is offered to
//! the inner region as `ParentCatalog { ordinal }`, the position of the scalar
//! in the outer catalog's fixed order (symbolic points, then fold tuples, each
//! in listing order). The outer region resolves the ordinal to its own source
//! only when the inner region reads it, so nesting never loads a scalar
//! nobody reads.

use super::*;

impl<'dae> RegionVisiblePoints<'dae> {
    /// This catalog as the catalog of a region nested in the region owning it.
    pub(super) fn forwarded(&self) -> Self {
        let mut ordinal = 0usize;
        let mut next = || {
            let source = FunctionConditionalCaptureSource::ParentCatalog { ordinal };
            ordinal += 1;
            source
        };
        Self {
            symbolic: self
                .symbolic
                .iter()
                .map(|(domain, sources)| (*domain, sources.iter().map(|_| next()).collect()))
                .collect(),
            folds: self
                .folds
                .iter()
                .map(|(fold, tuple)| {
                    let tuple = tuple
                        .iter()
                        .map(|carried| carried.iter().map(|_| next()).collect())
                        .collect();
                    (*fold, tuple)
                })
                .collect(),
        }
    }

    /// The source at `ordinal` in the order `forwarded` numbers them.
    fn source_at(&self, ordinal: usize) -> Option<FunctionConditionalCaptureSource<'dae>> {
        self.symbolic
            .iter()
            .flat_map(|(_, sources)| sources.iter())
            .chain(
                self.folds
                    .iter()
                    .flat_map(|(_, tuple)| tuple.iter().flatten()),
            )
            .nth(ordinal)
            .copied()
    }
}

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    /// The source of this region's own catalog that an inner region's
    /// `ParentCatalog { ordinal }` names.
    pub(super) fn region_catalog_source(
        &self,
        ordinal: usize,
        span: Span,
    ) -> Result<FunctionConditionalCaptureSource<'dae>, LowerError> {
        self.deferred_function_conditional_captures
            .as_ref()
            .and_then(|captures| captures.visible.source_at(ordinal))
            .ok_or_else(|| {
                LowerError::contract(
                    "nested function-conditional region names a catalog scalar its owner lacks",
                    span,
                )
            })
    }
}
