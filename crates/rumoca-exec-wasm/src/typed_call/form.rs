//! Borrow the canonical owner's issued execution form without rebuilding IR.
use super::TypedCallCompileError;
use rumoca_ir_solve as solve;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum Form {
    Primal,
    Directional,
}

impl Form {
    pub(super) fn body(
        self,
        owner: &solve::SolvePureCallOwner,
    ) -> Result<&solve::TypedProgram, TypedCallCompileError> {
        match self {
            Self::Primal => Ok(owner.body()),
            Self::Directional => owner
                .directional()
                .map(|form| form.body())
                .ok_or(TypedCallCompileError::SiteMismatch),
        }
    }

    pub(super) fn outputs(
        self,
        owner: &solve::SolvePureCallOwner,
    ) -> Option<&[solve::SolvePureCallOutput]> {
        match self {
            Self::Primal => Some(owner.outputs()),
            Self::Directional => owner.directional().map(|form| form.outputs()),
        }
    }
}
