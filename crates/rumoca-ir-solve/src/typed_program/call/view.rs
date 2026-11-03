use super::recursion::PendingOwner;
use super::{
    BoundSolvePureCallBody, SolvePureCallInterface, SolvePureCallOwner, SolvePureCallOwnerId,
};

/// A borrowed prefix of already-issued owners, optionally followed by the
/// reserved members of one recursive group under construction (SOLVE-C62).
/// Lookup preserves their exact identities and selects the primal or
/// directional contract without copying. Reserved members have no
/// directional form.
#[derive(Clone, Copy, Default)]
pub(in crate::typed_program) struct SolvePureCallTableView<'owner> {
    owners: &'owner [SolvePureCallOwner],
    pending: &'owner [PendingOwner],
    directional: bool,
    bound: Option<&'owner [BoundSolvePureCallBody<'owner>]>,
    bound_original: &'owner [SolvePureCallOwnerId],
    bound_directional: &'owner [Option<SolvePureCallOwnerId>],
}

impl<'owner> SolvePureCallTableView<'owner> {
    pub(in crate::typed_program) const fn primal(owners: &'owner [SolvePureCallOwner]) -> Self {
        Self {
            owners,
            pending: &[],
            directional: false,
            bound: None,
            bound_original: &[],
            bound_directional: &[],
        }
    }

    pub(in crate::typed_program) const fn directional(
        owners: &'owner [SolvePureCallOwner],
    ) -> Self {
        Self {
            owners,
            pending: &[],
            directional: true,
            bound: None,
            bound_original: &[],
            bound_directional: &[],
        }
    }

    /// The primal prefix followed by the reserved members of one group.
    pub(in crate::typed_program) const fn with_pending(
        owners: &'owner [SolvePureCallOwner],
        pending: &'owner [PendingOwner],
    ) -> Self {
        Self {
            owners,
            pending,
            directional: false,
            bound: None,
            bound_original: &[],
            bound_directional: &[],
        }
    }

    pub(in crate::typed_program) fn bound(
        bindings: &'owner [BoundSolvePureCallBody<'owner>],
        original: &'owner [SolvePureCallOwnerId],
        directional: &'owner [Option<SolvePureCallOwnerId>],
    ) -> Self {
        Self {
            owners: &[],
            pending: &[],
            directional: false,
            bound: Some(bindings),
            bound_original: original,
            bound_directional: directional,
        }
    }

    pub(in crate::typed_program) fn bound_indices(
        self,
    ) -> Option<(
        &'owner [SolvePureCallOwnerId],
        &'owner [Option<SolvePureCallOwnerId>],
    )> {
        self.bound
            .map(|_| (self.bound_original, self.bound_directional))
    }

    pub(in crate::typed_program) fn bound_namespace(
        self,
    ) -> Option<&'owner [BoundSolvePureCallBody<'owner>]> {
        self.bound
    }

    pub(in crate::typed_program) fn shares_bound_namespace(
        self,
        bindings: &[BoundSolvePureCallBody<'_>],
    ) -> bool {
        self.bound.is_some_and(|bound| {
            std::ptr::eq(bound.as_ptr(), bindings.as_ptr()) && bound.len() == bindings.len()
        })
    }

    pub(in crate::typed_program) const fn as_primal(self) -> Self {
        Self {
            directional: false,
            ..self
        }
    }

    pub(in crate::typed_program) fn get(
        self,
        index: usize,
    ) -> Option<SolvePureCallInterface<'owner>> {
        if let Some(bindings) = self.bound {
            let id = SolvePureCallOwnerId::from_index(u32::try_from(index).ok()?);
            return bindings.get(index).map(|binding| binding.interface(id));
        }
        let Some(owner) = self.owners.get(index) else {
            if self.directional {
                return None;
            }
            return self
                .pending
                .get(index - self.owners.len())
                .filter(|pending| pending.id().index() as usize == index)
                .map(PendingOwner::interface);
        };
        if owner.id.index() as usize != index {
            return None;
        }
        if self.directional {
            Some(owner.directional.as_ref()?.interface(owner.id))
        } else {
            Some(owner.interface())
        }
    }
}
