//! Read-only projections from the linear compact-loop construction capability.

use super::*;

impl<'dae> FunctionLoop<'dae> {
    pub const fn body(&self) -> &FunctionBody<'dae> {
        &self.body
    }

    pub const fn fold(&self) -> FunctionFoldId<'dae> {
        self.fold
    }

    pub const fn domain(&self) -> DomainId<'dae> {
        self.domain
    }
}

impl<'dae> Functions<'_, 'dae> {
    /// Read membership in the same issued fold's exit update inventory.
    pub fn loop_carries(
        &self,
        loop_body: &FunctionLoop<'dae>,
        target: FunctionValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<bool, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        check_function_value_owner(loop_body.body.function, target, provenance)?;
        function_value_entry(self.storage, target, provenance)?;
        let raw = function_fold_raw(self.storage, loop_body.fold, provenance)?;
        Ok(self.storage.function_folds[raw as usize]
            .targets
            .contains(&target.ordinal()))
    }
}
