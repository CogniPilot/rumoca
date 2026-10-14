//! Reuse the checked typed emitter inside a complete native assignment module.
#[cfg(test)]
mod tests;
use super::{
    TypedCallCompileError, TypedCallFault, emit,
    link::{AssertionMode, LinkedOwners},
};
use crate::emit::MathImport;
use rumoca_ir_solve as solve;
use std::collections::BTreeMap;
use wasm_encoder::Function;

pub(crate) use super::layout::Demand;

pub(crate) struct ProgramHelpers {
    linked: LinkedOwners,
    directional: Option<LinkedOwners>,
    relocated: bool,
}

#[derive(Clone)]
pub(crate) struct CallLayout {
    pub input: u32,
    pub output: u32,
    pub scratch: u32,
    /// Scratch the callee would need without sharing between sequential operations.
    pub unshared: u64,
    pub function: u32,
    pub abi: std::rc::Rc<CallAbi>,
}

/// Where a call's tuple cells, result cells and buffers are, as the entry glue
/// that calls a root owner fills and reads them.
#[derive(Debug)]
pub(crate) struct CallAbi {
    /// The input cells, in input order.
    pub inputs: Vec<InputAbi>,
    /// The outputs, in output order.
    pub outputs: Vec<OutputAbi>,
    /// The outputs are left in the head of the callee's scratch; otherwise the
    /// callee publishes them to its output buffer.
    pub outputs_in_scratch: bool,
}

/// One input cell of the callee's input tuple.
#[derive(Debug, Clone, Copy)]
pub(crate) struct InputAbi {
    pub offset: u32,
    /// Bytes of the input value.
    pub bytes: u32,
    /// The cell holds the address of the value instead of the value.
    pub by_reference: bool,
}

/// One output of the callee, in the head of its scratch.
#[derive(Debug, Clone, Copy)]
pub(crate) struct OutputAbi {
    /// Scratch offset of the value, or of the 8-byte cell holding its address.
    pub offset: u32,
    /// Bytes of the output value.
    pub bytes: u32,
    /// For an output returned by reference: the input tuple offset of the cell
    /// the caller stores the address of the buffer to build it in.
    pub buffer_cell: Option<u32>,
}

impl ProgramHelpers {
    pub(crate) fn new(
        table: &solve::SolvePureCallTable,
        sites: &[solve::SolvePureCallSite],
    ) -> Result<Self, TypedCallCompileError> {
        Self::with_demands(table, sites, |_| Demand::all(), AssertionMode::Value)
    }

    /// A complete native entry executes its original assigned-read guards;
    /// unlike a numeric value row, it has no scheduled assertion observer.
    pub(crate) fn native_with_demands(
        table: &solve::SolvePureCallTable,
        sites: &[solve::SolvePureCallSite],
        demand: impl Fn(&solve::SolvePureCallSite) -> Demand,
    ) -> Result<Self, TypedCallCompileError> {
        Self::with_demands(table, sites, demand, AssertionMode::Standalone)
    }

    pub(crate) fn event(
        source: &solve::RetainedEventProgram,
        sites: &[solve::SolvePureCallSite],
    ) -> Result<Self, TypedCallCompileError> {
        if source.table().arithmetic().real_format() != solve::SolveRealFormat::Binary64 {
            return Err(TypedCallCompileError::UnsupportedArithmetic);
        }
        if sites.iter().any(|site| !source.table().matches_site(site)) {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        let roots = sites
            .iter()
            .map(solve::SolvePureCallSite::owner)
            .collect::<Vec<_>>();
        Ok(Self {
            linked: LinkedOwners::construct_event(source, &roots)?,
            directional: None,
            relocated: false,
        })
    }

    pub(crate) fn observations(
        &self,
        site: &solve::SolvePureCallSite,
    ) -> Result<Option<super::observations::ObservationPlan>, TypedCallCompileError> {
        let unit = self.linked.root_unit(site.owner());
        Ok(self.linked.unit_plan(unit)?.observations.clone())
    }

    /// Link the program whose root `site` is built for the outputs
    /// `demand(site)` of it that some stage reads (SOLVE-C95).
    fn with_demands(
        table: &solve::SolvePureCallTable,
        sites: &[solve::SolvePureCallSite],
        demand: impl Fn(&solve::SolvePureCallSite) -> Demand,
        assertion_mode: AssertionMode,
    ) -> Result<Self, TypedCallCompileError> {
        if sites.iter().any(|site| !table.matches_site(site)) {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        if table.arithmetic().real_format() != solve::SolveRealFormat::Binary64 {
            return Err(TypedCallCompileError::UnsupportedArithmetic);
        }
        let roots = sites
            .iter()
            .map(|site| (site.owner(), demand(site)))
            .collect::<Vec<_>>();
        Ok(Self {
            linked: LinkedOwners::construct_program(table, &roots, assertion_mode)?,
            directional: None,
            relocated: false,
        })
    }

    pub(crate) fn with_directional(
        table: &solve::SolvePureCallTable,
        sites: &[solve::SolvePureCallSite],
        directional: &[solve::SolvePureCallDirectionalSite],
    ) -> Result<Self, TypedCallCompileError> {
        if directional
            .iter()
            .any(|site| !table.matches_directional_site(site))
        {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        let mut helpers = Self::new(table, sites)?;
        if !directional.is_empty() {
            let roots = directional
                .iter()
                .map(solve::SolvePureCallDirectionalSite::owner)
                .collect::<Vec<_>>();
            helpers.directional = Some(LinkedOwners::construct_directional_program(table, &roots)?);
        }
        Ok(helpers)
    }

    fn links(&self) -> impl Iterator<Item = &LinkedOwners> {
        std::iter::once(&self.linked).chain(self.directional.iter())
    }

    pub(crate) fn math_imports(&self) -> Vec<MathImport> {
        self.links()
            .flat_map(|linked| linked.math_imports.iter().copied())
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect()
    }

    /// Stage both forms before assigning any relocated function identity.
    pub(crate) fn relocate(&mut self, imports: &[MathImport]) -> Result<(), TypedCallCompileError> {
        if self.relocated {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        let primal = relocated_functions(&self.linked, imports, 0)?;
        let count =
            u32::try_from(self.linked.order.len()).map_err(|_| TypedCallCompileError::SizeLimit)?;
        let directional = self
            .directional
            .as_ref()
            .map(|linked| relocated_functions(linked, imports, count))
            .transpose()?;
        let indices = imports
            .iter()
            .enumerate()
            .map(|(index, import)| (*import, index as u32))
            .collect::<BTreeMap<_, _>>();
        self.linked.functions = primal;
        self.linked.math_indices = indices.clone();
        if let (Some(linked), Some(functions)) = (&mut self.directional, directional) {
            linked.functions = functions;
            linked.math_indices = indices;
        }
        self.relocated = true;
        Ok(())
    }

    pub(crate) fn layout(
        &self,
        site: &solve::SolvePureCallSite,
    ) -> Result<CallLayout, TypedCallCompileError> {
        layout_of(&self.linked, site.owner())
    }

    pub(crate) fn directional_layout(
        &self,
        site: &solve::SolvePureCallDirectionalSite,
    ) -> Result<CallLayout, TypedCallCompileError> {
        layout_of(
            self.directional
                .as_ref()
                .ok_or(TypedCallCompileError::SiteMismatch)?,
            site.owner(),
        )
    }

    pub(crate) fn bodies(
        &self,
        table: &solve::SolvePureCallTable,
        budget: usize,
    ) -> Result<(Vec<Function>, Vec<TypedCallFault>), TypedCallCompileError> {
        let mut bodies = Vec::new();
        let mut faults = Vec::new();
        let mut over_budget = OverBudget::default();
        let mut parts: Vec<Function> = Vec::new();
        let facts = emit::CallProgramFacts::established_by_call_program_layout();
        let first_free = self
            .links()
            .map(LinkedOwners::first_free_function)
            .max()
            .ok_or(TypedCallCompileError::SiteMismatch)?;
        for (linked, unit) in self
            .links()
            .flat_map(|linked| linked.order.iter().map(move |unit| (linked, *unit)))
        {
            let owner = &table.owners()[linked.unit_owner[unit] as usize];
            // Statuses 1 and 2 belong to the whole-program ABI and input conversion.
            let outlining = emit::Outlining {
                budget,
                first_part: u32::try_from(parts.len())
                    .ok()
                    .and_then(|n| first_free.checked_add(n))
                    .ok_or(TypedCallCompileError::SizeLimit)?,
            };
            match emit::compile_helper((owner, unit), linked, faults.len() + 2, facts, outlining) {
                Ok(mut helper) => {
                    bodies.push(helper.entry);
                    parts.append(&mut helper.parts);
                    faults.append(&mut helper.faults);
                }
                Err(error @ TypedCallCompileError::FunctionBudgetExceeded { .. }) => {
                    over_budget.record(error);
                }
                Err(error) => return Err(error),
            }
        }
        over_budget.into_result()?;
        bodies.append(&mut parts);
        Ok((bodies, faults))
    }
}

impl ProgramHelpers {
    /// The frame layout of every owner a call site reaches, once per owner.
    pub(crate) fn owner_scratch(
        &self,
        table: &solve::SolvePureCallTable,
    ) -> Vec<crate::ScratchOwner> {
        self.links()
            .flat_map(|linked| linked.order.iter().map(move |unit| (linked, *unit)))
            .filter_map(|(linked, unit)| {
                let plan = linked.plans[unit].as_ref()?;
                let owner = &table.owners()[linked.unit_owner[unit] as usize];
                Some(crate::ScratchOwner {
                    owner: owner.id().index() as usize,
                    provenance: owner.provenance(),
                    frame: plan.report(linked.form.body(owner).ok()?),
                })
            })
            .collect()
    }
}

fn layout_of(
    linked: &LinkedOwners,
    owner: solve::SolvePureCallOwnerId,
) -> Result<CallLayout, TypedCallCompileError> {
    let unit = linked.root_unit(owner);
    let plan = linked.unit_plan(unit)?;
    let outputs = plan
        .output_slots
        .iter()
        .map(|slot| {
            let range = plan.slots[*slot];
            match plan.ref_outputs.get(slot) {
                Some(reference) => OutputAbi {
                    offset: reference.result_cell,
                    bytes: range.bytes,
                    buffer_cell: Some(reference.dest_cell),
                },
                None => OutputAbi {
                    offset: range.offset,
                    bytes: range.bytes,
                    buffer_cell: None,
                },
            }
        })
        .collect();
    let inputs = plan
        .input_cells
        .iter()
        .map(|cell| InputAbi {
            offset: cell.offset,
            bytes: cell.bytes,
            by_reference: cell.by_reference,
        })
        .collect();
    Ok(CallLayout {
        abi: std::rc::Rc::new(CallAbi {
            inputs,
            outputs,
            outputs_in_scratch: plan.outputs_in_scratch,
        }),
        input: plan.input_bytes,
        output: plan.output_bytes,
        scratch: plan.scratch_bytes,
        unshared: plan.unshared_bytes,
        function: linked.functions[unit].ok_or(TypedCallCompileError::SiteMismatch)?,
    })
}

fn relocated_functions(
    linked: &LinkedOwners,
    imports: &[MathImport],
    previous: u32,
) -> Result<Vec<Option<u32>>, TypedCallCompileError> {
    let count = u32::try_from(imports.len()).map_err(|_| TypedCallCompileError::SizeLimit)?;
    let offset = count
        .checked_sub(linked.math_imports.len() as u32)
        .and_then(|offset| offset.checked_add(previous))
        .ok_or(TypedCallCompileError::SizeLimit)?;
    let indices = imports
        .iter()
        .enumerate()
        .map(|(index, import)| (*import, index as u32))
        .collect::<BTreeMap<_, _>>();
    if indices.len() != imports.len()
        || linked
            .math_imports
            .iter()
            .any(|import| !indices.contains_key(import))
    {
        return Err(TypedCallCompileError::SiteMismatch);
    }
    linked
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
        .collect()
}

/// Every helper that cannot fit the byte budget, reported by its largest
/// member so one compile names the owner to fix.
#[derive(Default)]
struct OverBudget {
    largest: Option<TypedCallCompileError>,
    count: usize,
}

impl OverBudget {
    fn record(&mut self, error: TypedCallCompileError) {
        self.count += 1;
        let larger = match (&self.largest, &error) {
            (
                Some(TypedCallCompileError::FunctionBudgetExceeded { bytes: seen, .. }),
                TypedCallCompileError::FunctionBudgetExceeded { bytes, .. },
            ) => bytes > seen,
            _ => true,
        };
        if larger {
            self.largest = Some(error);
        }
    }

    fn into_result(self) -> Result<(), TypedCallCompileError> {
        match self.largest {
            Some(TypedCallCompileError::FunctionBudgetExceeded {
                owner,
                provenance,
                bytes,
                operations,
                budget,
                ..
            }) => Err(TypedCallCompileError::FunctionBudgetExceeded {
                owner,
                provenance,
                bytes,
                operations,
                budget,
                over_budget_owners: self.count,
            }),
            _ => Ok(()),
        }
    }
}
