//! Reuse the checked typed emitter inside a complete native assignment module.
#[cfg(test)]
mod tests;
use super::{TypedCallCompileError, TypedCallFault, emit, link::LinkedOwners};
use crate::emit::MathImport;
use rumoca_ir_solve as solve;
use std::collections::BTreeMap;
use wasm_encoder::Function;

pub(crate) struct ProgramHelpers {
    linked: LinkedOwners,
    relocated: bool,
}

#[derive(Clone, Copy)]
pub(crate) struct CallLayout {
    pub input: u32,
    pub output: u32,
    pub scratch: u32,
    /// Scratch the callee would need without sharing between sequential operations.
    pub unshared: u32,
    pub function: u32,
}

impl ProgramHelpers {
    pub(crate) fn new(
        table: &solve::SolvePureCallTable,
        sites: &[solve::SolvePureCallSite],
    ) -> Result<Self, TypedCallCompileError> {
        if sites.iter().any(|site| !table.matches_site(site)) {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        if table.arithmetic().real_format() != solve::SolveRealFormat::Binary64 {
            return Err(TypedCallCompileError::UnsupportedArithmetic);
        }
        let roots = sites.iter().map(|site| site.owner()).collect::<Vec<_>>();
        Ok(Self {
            linked: LinkedOwners::construct_roots(table, &roots)?,
            relocated: false,
        })
    }

    pub(crate) fn math_imports(&self) -> &[MathImport] {
        &self.linked.math_imports
    }

    /// Relocation owns one checked, transactional catalog assignment.
    pub(crate) fn relocate(&mut self, imports: &[MathImport]) -> Result<(), TypedCallCompileError> {
        if self.relocated {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        let count = u32::try_from(imports.len()).map_err(|_| TypedCallCompileError::SizeLimit)?;
        let offset = count
            .checked_sub(self.linked.math_imports.len() as u32)
            .ok_or(TypedCallCompileError::SizeLimit)?;
        let indices = imports
            .iter()
            .enumerate()
            .map(|(index, import)| (*import, index as u32))
            .collect::<BTreeMap<_, _>>();
        if indices.len() != imports.len()
            || self
                .linked
                .math_imports
                .iter()
                .any(|import| !indices.contains_key(import))
        {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        let functions = self
            .linked
            .functions
            .iter()
            .map(|function| {
                function
                    .map(|index| {
                        index
                            .checked_add(offset)
                            .ok_or(TypedCallCompileError::SizeLimit)
                    })
                    .transpose()
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.linked.functions = functions;
        self.linked.math_indices = indices;
        self.relocated = true;
        Ok(())
    }

    pub(crate) fn layout(
        &self,
        site: &solve::SolvePureCallSite,
    ) -> Result<CallLayout, TypedCallCompileError> {
        let plan = self.linked.plan(site.owner())?;
        Ok(CallLayout {
            input: plan.input_bytes,
            output: plan.output_bytes,
            scratch: plan.scratch_bytes,
            unshared: plan.unshared_bytes,
            function: self.linked.functions[site.owner().index() as usize]
                .ok_or(TypedCallCompileError::SiteMismatch)?,
        })
    }

    pub(crate) fn bodies(
        &self,
        table: &solve::SolvePureCallTable,
    ) -> Result<(Vec<Function>, Vec<TypedCallFault>), TypedCallCompileError> {
        let mut bodies = Vec::new();
        let mut faults = Vec::new();
        for owner in table.owners() {
            if self.linked.functions[owner.id().index() as usize].is_none() {
                continue;
            }
            // Statuses 1 and 2 belong to the whole-program ABI and input conversion.
            let (body, mut owner_faults) =
                emit::compile_owner(owner, &self.linked, faults.len() + 2)?;
            bodies.push(body);
            faults.append(&mut owner_faults);
        }
        Ok((bodies, faults))
    }
}

impl ProgramHelpers {
    /// The frame layout of every owner a call site reaches, once per owner.
    pub(crate) fn owner_scratch(
        &self,
        table: &solve::SolvePureCallTable,
    ) -> Vec<crate::ScratchOwner> {
        table
            .owners()
            .iter()
            .zip(&self.linked.plans)
            .filter_map(|(owner, plan)| {
                plan.as_ref().map(|plan| crate::ScratchOwner {
                    owner: owner.id().index() as usize,
                    provenance: owner.provenance(),
                    frame: plan.report(owner.body()),
                })
            })
            .collect()
    }
}
