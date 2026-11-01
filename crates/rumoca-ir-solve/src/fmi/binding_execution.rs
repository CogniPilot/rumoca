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

    pub(super) fn source(&self) -> &ScalarProgramBlock {
        &self.source
    }

    pub(super) fn programs(&self) -> &[FmiBindingProgram] {
        &self.programs
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
    })
}

#[cfg(test)]
mod tests;
