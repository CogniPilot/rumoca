//! One target helper per reachable, construction-issued pure-call owner.
#[cfg(test)]
mod tests;
use super::{TypedCallCompileError, layout::FramePlan};
use crate::emit::MathImport;
use rumoca_ir_solve as solve;
use std::collections::{BTreeMap, BTreeSet};

pub(super) struct LinkedOwners {
    pub plans: Vec<Option<FramePlan>>,
    pub functions: Vec<Option<u32>>,
    pub math_imports: Vec<MathImport>,
    pub math_indices: BTreeMap<MathImport, u32>,
}

impl LinkedOwners {
    pub(super) fn construct(
        table: &solve::SolvePureCallTable,
        root: solve::SolvePureCallOwnerId,
    ) -> Result<Self, TypedCallCompileError> {
        Self::construct_roots(table, &[root])
    }

    pub(super) fn construct_roots(
        table: &solve::SolvePureCallTable,
        roots: &[solve::SolvePureCallOwnerId],
    ) -> Result<Self, TypedCallCompileError> {
        let mut reachable = vec![false; table.owners().len()];
        let mut pending = roots.to_vec();
        let mut math_imports = BTreeSet::new();
        while let Some(id) = pending.pop() {
            let owner = table.owner(id).ok_or(TypedCallCompileError::SiteMismatch)?;
            // SOLVE-C62 groups need depth-carrying frames and source-bound
            // stack failures. The native typed-call ABI does not execute them.
            if table.recursive_group(id).is_some() {
                return Err(TypedCallCompileError::UnsupportedRecursiveGroup {
                    owner: id,
                    provenance: owner.provenance(),
                });
            }
            if reachable[id.index() as usize] {
                continue;
            }
            reachable[id.index() as usize] = true;
            check_outputs(owner)?;
            super::math::collect(owner.body(), &mut math_imports);
            collect_calls(owner.body(), &mut pending);
        }
        let math_imports = math_imports.into_iter().collect::<Vec<_>>();
        let math_indices = math_imports
            .iter()
            .enumerate()
            .map(|(i, import)| (*import, i as u32))
            .collect();
        let mut next =
            u32::try_from(math_imports.len()).map_err(|_| TypedCallCompileError::SizeLimit)?;
        let mut linked = Self {
            plans: Vec::with_capacity(reachable.len()),
            functions: Vec::with_capacity(reachable.len()),
            math_imports,
            math_indices,
        };
        // Admission above excludes SOLVE-C62 groups. The remaining checked
        // calls reference only previously issued owners (SOLVE-C52).
        for (owner, reachable) in table.owners().iter().zip(reachable) {
            let plan = reachable
                .then(|| FramePlan::construct(owner.body(), &linked.plans))
                .transpose()?;
            linked.plans.push(plan);
            linked.functions.push(reachable.then_some(next));
            next = next
                .checked_add(u32::from(reachable))
                .ok_or(TypedCallCompileError::SizeLimit)?;
        }
        Ok(linked)
    }

    pub(super) fn plan(
        &self,
        id: solve::SolvePureCallOwnerId,
    ) -> Result<&FramePlan, TypedCallCompileError> {
        self.plans
            .get(id.index() as usize)
            .and_then(Option::as_ref)
            .ok_or(TypedCallCompileError::SiteMismatch)
    }
}

fn check_outputs(owner: &solve::SolvePureCallOwner) -> Result<(), TypedCallCompileError> {
    for (output, value) in owner.outputs().iter().enumerate() {
        if value.kind() == solve::SolvePureCallOutputKind::AssertionPredicate {
            return Err(TypedCallCompileError::UnsupportedOutputKind {
                owner: owner.id(),
                output,
                kind: value.kind(),
                provenance: owner.provenance(),
            });
        }
    }
    Ok(())
}

fn collect_calls(program: &solve::TypedProgram, pending: &mut Vec<solve::SolvePureCallOwnerId>) {
    for operation in program.operations() {
        match operation.operation() {
            solve::SolveOperation::Call { owner, .. } => pending.push(*owner),
            solve::SolveOperation::Conditional {
                if_true, if_false, ..
            } => {
                collect_calls(if_true.body(), pending);
                collect_calls(if_false.body(), pending);
            }
            solve::SolveOperation::Map { body, .. } => collect_calls(body.body(), pending),
            solve::SolveOperation::Fold { transition, .. } => {
                collect_calls(transition.body(), pending);
            }
            _ => {}
        }
    }
}
