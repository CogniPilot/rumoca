//! Placement geometry only; the body/direction join supplies semantic authority.
use super::CompactJacobianPlacement;
use crate::{StructuralPattern, StructuralPatternError};
use rumoca_core::Span;
use std::marker::PhantomData;
#[cfg(test)]
mod tests;

/// A branded receipt for one issued placement's compact coordinate relation.
/// It is not a derivative certificate: the subsequent checked body/output join
/// must bind the actual directions before a product can expose this relation.
/// A clone of the placement retains its identity; reconstruction by shape does
/// not. Receipt fields and construction are private, and receipts are not wire
/// values. A future replay must issue a fresh owner and recheck its body join.
/// ```compile_fail,E0451
/// use rumoca_ir_solve::{CompactJacobianPlacement, CompactJacobianPlacementReceipt, StructuralPattern};
/// fn forge<'a>(placement: &'a CompactJacobianPlacement, relation: StructuralPattern)
///     -> CompactJacobianPlacementReceipt<'a> {
///     CompactJacobianPlacementReceipt { placement, relation, brand: Default::default() }
/// }
/// ```
pub struct CompactJacobianPlacementReceipt<'owner> {
    placement: &'owner CompactJacobianPlacement,
    relation: StructuralPattern,
    brand: PhantomData<fn(&'owner mut ()) -> &'owner mut ()>,
}

impl CompactJacobianPlacement {
    /// Use the receipt inside its fresh region; it cannot escape the callback.
    /// Unsupported non-injective/nonnested row geometry refuses here without
    /// changing the existing placement or its dense materialization contract.
    /// ```compile_fail
    /// use rumoca_ir_solve::{CompactJacobianPlacement, CompactJacobianPlacementReceipt};
    /// use rumoca_core::Span;
    /// fn escape(p: &CompactJacobianPlacement, span: Span) -> CompactJacobianPlacementReceipt<'_> {
    ///     p.with_dependency_receipt(span, |receipt| receipt).unwrap()
    /// }
    /// ```
    pub fn with_dependency_receipt<R>(
        &self,
        span: Span,
        use_receipt: impl for<'owner> FnOnce(CompactJacobianPlacementReceipt<'owner>) -> R,
    ) -> Result<R, StructuralPatternError> {
        let relation = self.derive_placement_relation(span)?;
        Ok(use_receipt(CompactJacobianPlacementReceipt {
            placement: self,
            relation,
            brand: PhantomData,
        }))
    }
}

impl CompactJacobianPlacementReceipt<'_> {
    pub fn state_count(&self) -> u32 {
        self.relation().rows()
    }

    pub fn belongs_to(&self, placement: &CompactJacobianPlacement) -> bool {
        std::sync::Arc::ptr_eq(&self.placement.identity, &placement.identity)
    }

    // Semantic access stays IR-local until the checked body/output join. Keeping
    // this here also makes the staged receipt retain its actual relation.
    pub(crate) fn relation(&self) -> &StructuralPattern {
        &self.relation
    }
}
