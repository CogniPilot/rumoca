use super::recursion::PendingOwner;
use super::{SolvePureCallInterface, SolvePureCallOwner};

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
}

impl<'owner> SolvePureCallTableView<'owner> {
    pub(in crate::typed_program) const fn primal(owners: &'owner [SolvePureCallOwner]) -> Self {
        Self {
            owners,
            pending: &[],
            directional: false,
        }
    }

    pub(in crate::typed_program) const fn directional(
        owners: &'owner [SolvePureCallOwner],
    ) -> Self {
        Self {
            owners,
            pending: &[],
            directional: true,
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
        }
    }

    pub(in crate::typed_program) fn get(
        self,
        index: usize,
    ) -> Option<SolvePureCallInterface<'owner>> {
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
