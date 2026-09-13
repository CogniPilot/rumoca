use super::{SolvePureCallInterface, SolvePureCallOwner};

/// A borrowed prefix of already-issued owners. Lookup preserves their exact
/// identities and selects the primal or directional contract without copying.
#[derive(Clone, Copy, Default)]
pub(in crate::typed_program) struct SolvePureCallTableView<'owner> {
    owners: &'owner [SolvePureCallOwner],
    directional: bool,
}

impl<'owner> SolvePureCallTableView<'owner> {
    pub(in crate::typed_program) const fn primal(owners: &'owner [SolvePureCallOwner]) -> Self {
        Self {
            owners,
            directional: false,
        }
    }

    pub(in crate::typed_program) const fn directional(
        owners: &'owner [SolvePureCallOwner],
    ) -> Self {
        Self {
            owners,
            directional: true,
        }
    }

    pub(in crate::typed_program) fn get(
        self,
        index: usize,
    ) -> Option<SolvePureCallInterface<'owner>> {
        let owner = self
            .owners
            .get(index)
            .filter(|owner| owner.id.index() as usize == index)?;
        if self.directional {
            Some(owner.directional.as_ref()?.interface(owner.id))
        } else {
            Some(owner.interface())
        }
    }
}
