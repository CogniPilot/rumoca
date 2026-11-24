//! Binding execution projected from the same admitted initialization owner.

use crate::{ScalarProgramBlock, ScalarSlot};
use serde::Serialize;
use std::collections::BTreeSet;

/// A whole source program and the rows its two lifecycle consumers commit.
/// Fields are private: a backend cannot issue its own store classification.
#[derive(Debug, Serialize)]
pub struct FmiBindingProgram {
    program_index: usize,
    initialization_only: bool,
    parameter_rows: Vec<usize>,
    state_start_rows: Vec<usize>,
    quotient_seed_rows: Vec<usize>,
}

impl FmiBindingProgram {
    pub fn program_index(&self) -> usize {
        self.program_index
    }

    /// Rows committed after initialization. Other results of the same whole
    /// program still execute, with their values confined to private scratch.
    pub fn parameter_rows(&self) -> &[usize] {
        &self.parameter_rows
    }
}

#[derive(Debug)]
pub(super) struct BindingExecution {
    source: ScalarProgramBlock,
    programs: Vec<FmiBindingProgram>,
}

impl BindingExecution {
    /// Called only after parameter dependency and storage-role admission.
    /// Rebuild row/lifecycle selectors from this exact immutable program owner;
    /// neither operation spelling nor template scans confer commit authority.
    pub(super) fn checked(
        source: &ScalarProgramBlock,
        targets: &[ScalarSlot],
        order: &[usize],
    ) -> Result<Self, &'static str> {
        if source.output_count() != targets.len() {
            return Err("binding execution output/target count differs");
        }
        let selected = order.iter().copied().collect::<BTreeSet<_>>();
        if selected.len() != order.len()
            || selected != (0..source.programs().len()).collect::<BTreeSet<_>>()
        {
            return Err("binding execution order is not the exact program inventory");
        }
        let mut rows = BTreeSet::new();
        let programs = order
            .iter()
            .map(|&index| program(source, targets, index, &mut rows))
            .collect::<Result<Vec<_>, _>>()?;
        if rows != (0..targets.len()).collect::<BTreeSet<_>>() {
            return Err("binding execution rows do not cover the exact target inventory");
        }
        Ok(Self {
            source: source.clone(),
            programs,
        })
    }

    pub(super) fn classify_quotient_seeds(mut self, problem: &crate::SolveProblem) -> Self {
        for program in &mut self.programs {
            if super::parameter_updates::is_quotient_seed(problem, program.program_index) {
                program.quotient_seed_rows = std::mem::take(&mut program.parameter_rows);
                program.initialization_only = true;
            }
        }
        self
    }

    pub(super) fn source(&self) -> &ScalarProgramBlock {
        &self.source
    }

    pub(super) fn programs(&self) -> &[FmiBindingProgram] {
        &self.programs
    }
}

/// The parameter bindings of a Solve target that runs the shared C kernel
/// without an FMI component (a per-lane CUDA initialization): the same
/// admission, dependency order, and lifecycle rows as the FMI C profile
/// ([`super::FmiCCodegenView::parameter_binding_execution`]), derived from the
/// Solve model alone. None of them reads FMI metadata.
///
/// Unlike the FMI C profile under parameter-determined events, no discrete
/// equation settles with the bindings: the runtime's
/// `apply_initialization_updates` applies the bindings alone, and the
/// target's initial event iteration settles the discrete rows afterwards.
#[derive(Debug)]
pub struct SolveBindingExecution {
    execution: BindingExecution,
    levels: usize,
}

impl SolveBindingExecution {
    /// Admit the model to the C kernel as the generated C component does,
    /// without the checks of the component's own variables
    /// (`c_codegen::admit_kernel`), and issue the execution rows of its
    /// parameter bindings.
    pub fn checked(model: &crate::SolveModel) -> Result<Self, &'static str> {
        let (order, levels) =
            super::c_codegen::admit_kernel(model, None).map_err(|error| error.0)?;
        let init = &model.problem.initialization;
        let execution =
            BindingExecution::checked(init.update_rhs(), init.update_targets(), &order)?
                .classify_quotient_seeds(&model.problem);
        Ok(Self { execution, levels })
    }

    /// Whole programs in dependency order, with constructor-issued commit rows.
    pub fn programs(&self) -> &[FmiBindingProgram] {
        self.execution.programs()
    }

    /// The number of dependency levels of the bindings: the repeated sweeps
    /// the runtime needs to settle them (`parameter_updates::validate`).
    #[must_use]
    pub const fn levels(&self) -> usize {
        self.levels
    }
}

/// The keys the binding block of the FMI C refresh (`refresh_values`) reads
/// from an FMI C view, `binding_execution` and `discrete_equations` (empty,
/// see above), and the dependency levels as `binding_levels`.
impl Serialize for SolveBindingExecution {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeMap;
        let mut entries = serializer.serialize_map(Some(3))?;
        entries.serialize_entry("binding_execution", self.programs())?;
        entries.serialize_entry("discrete_equations", &[] as &[usize])?;
        entries.serialize_entry("binding_levels", &self.levels)?;
        entries.end()
    }
}

fn program(
    source: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    index: usize,
    seen: &mut BTreeSet<usize>,
) -> Result<FmiBindingProgram, &'static str> {
    let ordinals = source
        .program_output_ordinals(index)
        .ok_or("binding execution program has no output range")?;
    let mut parameter_rows = Vec::new();
    let mut state_start_rows = Vec::new();
    for ordinal in ordinals {
        let row = source
            .output_index_at(ordinal)
            .ok_or("binding execution output has no row identity")?;
        if !seen.insert(row) {
            return Err("binding execution target row is written twice");
        }
        match targets.get(row) {
            Some(ScalarSlot::P { .. }) => parameter_rows.push(row),
            Some(ScalarSlot::Y { .. }) => state_start_rows.push(row),
            _ => return Err("binding execution target is not admitted parameter/state storage"),
        }
    }
    if parameter_rows.is_empty() && state_start_rows.is_empty() {
        return Err("binding execution has a program without an owned result");
    }
    Ok(FmiBindingProgram {
        program_index: index,
        initialization_only: parameter_rows.is_empty(),
        parameter_rows,
        state_start_rows,
        quotient_seed_rows: Vec::new(),
    })
}

#[cfg(test)]
mod tests;
