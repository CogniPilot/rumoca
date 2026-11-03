//! Exact source-issued helper bodies retain their own child-call namespace.

use super::*;

#[cfg(test)]
mod tests;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum BodyForm {
    Original,
    Directional,
}

#[derive(Clone, Copy, Debug)]
enum BoundBody<'source> {
    Original(&'source SolvePureCallOwner),
    Directional(&'source SolvePureCallDirectionalOwner),
}

/// One immutable helper body and its exact interface/source namespace.
///
/// This is a borrowed construction input, not a numerical graph or a target
/// call plan. Neither a raw body nor an interface-compatible table can be
/// substituted. Resolving a child retains the body's issued namespace.
#[derive(Clone, Copy, Debug)]
pub struct BoundSolvePureCallBody<'source> {
    table: &'source SolvePureCallTable,
    owner: &'source SolvePureCallOwner,
    body: BoundBody<'source>,
}

impl SolvePureCallTable {
    /// Bind the exact original body, preserving its raw value semantics.
    pub fn bind_original_body(
        &self,
        owner: SolvePureCallOwnerId,
        provenance: Span,
    ) -> Result<BoundSolvePureCallBody<'_>, SolveProgramConstructionError> {
        self.bind_body(owner, BodyForm::Original, provenance)
    }

    /// Bind the already-issued canonical directional body. Its auxiliary
    /// primal channel is not authority to replace the original body's values.
    pub fn bind_directional_body(
        &self,
        owner: SolvePureCallOwnerId,
        provenance: Span,
    ) -> Result<BoundSolvePureCallBody<'_>, SolveProgramConstructionError> {
        self.bind_body(owner, BodyForm::Directional, provenance)
    }

    fn bind_body(
        &self,
        owner: SolvePureCallOwnerId,
        form: BodyForm,
        provenance: Span,
    ) -> Result<BoundSolvePureCallBody<'_>, SolveProgramConstructionError> {
        let missing = || SolveProgramConstructionError::UnknownCallOwner { provenance };
        let owner = self.owner(owner).ok_or_else(missing)?;
        let body = match form {
            BodyForm::Original => BoundBody::Original(owner),
            BodyForm::Directional => {
                BoundBody::Directional(owner.directional.as_ref().ok_or_else(missing)?)
            }
        };
        Ok(BoundSolvePureCallBody {
            table: self,
            owner,
            body,
        })
    }
}

impl<'source> BoundSolvePureCallBody<'source> {
    pub fn body(self) -> &'source TypedProgram {
        match self.body {
            BoundBody::Original(owner) => &owner.body,
            BoundBody::Directional(owner) => &owner.body,
        }
    }

    pub fn inputs(self) -> &'source [SolveValueType] {
        match self.body {
            BoundBody::Original(owner) => &owner.inputs,
            BoundBody::Directional(owner) => &owner.inputs,
        }
    }

    pub fn outputs(self) -> &'source [SolvePureCallOutput] {
        match self.body {
            BoundBody::Original(owner) => &owner.outputs,
            BoundBody::Directional(owner) => &owner.outputs,
        }
    }

    /// Resolve a call selector in this body's own helper namespace. The
    /// consumer does not select a primal/directional mode for the child.
    /// This lookup alone does not prove a call occurs in the source body.
    pub fn resolve_child(
        self,
        owner: SolvePureCallOwnerId,
        provenance: Span,
    ) -> Result<Self, SolveProgramConstructionError> {
        self.table.bind_body(owner, self.form(), provenance)
    }

    /// Value-equal replay or a different body form has a different binding.
    pub fn shares_binding(self, other: Self) -> bool {
        self.table.shares_table_owner(other.table)
            && self.owner.id == other.owner.id
            && self.form() == other.form()
    }

    fn form(self) -> BodyForm {
        match self.body {
            BoundBody::Original(_) => BodyForm::Original,
            BoundBody::Directional(_) => BodyForm::Directional,
        }
    }
}
