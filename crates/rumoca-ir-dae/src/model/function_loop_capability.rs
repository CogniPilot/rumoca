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
        match self.body.domain {
            Some(domain) => domain,
            None => unreachable!(),
        }
    }
}
