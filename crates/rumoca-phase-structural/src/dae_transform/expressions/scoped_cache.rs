//! Reuse proved expressions within one reconstruction and exact substitution.

use std::collections::HashMap;

use rumoca_ir_dae as dae;

use super::super::differentiation::Derivative;
use super::ExpressionRebuilder;

#[derive(PartialEq, Eq, Hash)]
pub(in crate::dae_transform) struct ScopedReconstructionKey<'source> {
    expression: dae::ExprId<'source>,
    calls: Vec<dae::ExprId<'source>>,
    order: u8,
    state_only: bool,
    substitute_demoted: bool,
    provenance: dae::DaeProvenance,
}

/// Lookup only: entries never determine reconstruction order or escape the
/// finalized source/target construction that owns their branded identities.
#[derive(Default)]
pub(in crate::dae_transform) struct ScopedReconstructionCache<'source, 'target> {
    pub(in crate::dae_transform) derivatives:
        HashMap<ScopedReconstructionKey<'source>, Derivative<'target>>,
    pub(in crate::dae_transform) materialized:
        HashMap<ScopedReconstructionKey<'source>, dae::ExprId<'target>>,
    pub(in crate::dae_transform) instantiated:
        HashMap<ScopedReconstructionKey<'source>, dae::ExprId<'target>>,
    pub(in crate::dae_transform) coefficients:
        HashMap<(ScopedReconstructionKey<'source>, Option<u32>, bool), dae::ExprId<'target>>,
}

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(in crate::dae_transform) fn scoped_reconstruction_key(
        &self,
        expression: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> ScopedReconstructionKey<'source> {
        ScopedReconstructionKey {
            expression,
            calls: self.function_context.call_path().collect(),
            order,
            state_only: self.state_only_derivative,
            substitute_demoted: self.substitute_demoted_value,
            provenance,
        }
    }
}
