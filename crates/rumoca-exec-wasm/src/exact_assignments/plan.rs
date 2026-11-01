//! Final emission plan from existing construction-issued exact owners.
use rumoca_ir_solve::{
    ComputeBlock, ContinuousRefreshOwners, ExactRefreshAssignmentSchedule, ScalarProgramBlock,
    VarLayout,
};

pub(crate) struct Program {
    pub block: ScalarProgramBlock,
    pub targets: Vec<usize>,
    /// Declared directional coordinate count of a private entry; none is primal.
    pub seed: Option<usize>,
}

pub(crate) struct Plan {
    pub programs: Vec<Program>,
    pub maximum_tuple: usize,
}

impl Plan {
    pub(crate) fn private(block: &ScalarProgramBlock) -> Result<Self, String> {
        Self::private_entries(&[(block, None)])
    }

    /// One private entry per source; each keeps its own complete checked tuple.
    pub(crate) fn private_entries(
        entries: &[(&ScalarProgramBlock, Option<usize>)],
    ) -> Result<Self, String> {
        let mut programs = Vec::new();
        programs
            .try_reserve_exact(entries.len())
            .map_err(|_| "WASM private entry plan allocation failed")?;
        let mut maximum_tuple = 0;
        for &(block, seed) in entries {
            if block.programs().len() != 1 {
                return Err("private entry requires one complete checked program".into());
            }
            let count = block.stored_output_count();
            if count == 0 {
                return Err("private entry has no checked output".into());
            }
            maximum_tuple = maximum_tuple.max(count);
            programs.push(Program {
                block: block.clone(),
                targets: Vec::new(),
                seed,
            });
        }
        if programs.is_empty() {
            return Err("private module requires at least one entry".into());
        }
        Ok(Self {
            programs,
            maximum_tuple,
        })
    }

    pub(crate) fn new(
        source: &ComputeBlock,
        owners: &ContinuousRefreshOwners,
        schedule: &ExactRefreshAssignmentSchedule,
        layout: &VarLayout,
    ) -> Result<Self, String> {
        layout
            .validate_shape_contract()
            .map_err(|error| error.to_string())?;
        source
            .validate_shape_contract("WASM exact assignment source")
            .map_err(|error| error.to_string())?;
        let original = owners
            .exact_assignment_schedule(schedule.sequence_id())
            .ok_or("WASM exact assignment sequence is not issued by these owners")?;
        if original.program_ids() != schedule.program_ids() {
            return Err(
                "WASM exact assignment sequence differs from its issued program order".into(),
            );
        }
        let mut programs = Vec::new();
        programs
            .try_reserve_exact(schedule.program_ids().len())
            .map_err(|_| "WASM exact assignment plan allocation failed")?;
        let mut maximum_tuple = 0;
        for &id in schedule.program_ids() {
            let issued = owners
                .exact_assignment_program(id)
                .ok_or("WASM exact assignment program is missing")?;
            let block = issued
                .final_scalar_program(source)
                .map_err(|error| error.to_string())?;
            let [operations] = block.programs() else {
                return Err(
                    "WASM exact assignment projection requires one complete source program".into(),
                );
            };
            let count = ScalarProgramBlock::program_output_count(operations);
            if count == 0 || count != issued.target_indices().len() {
                return Err(
                    "WASM exact assignment tuple differs from its issued target list".into(),
                );
            }
            if issued
                .target_indices()
                .iter()
                .any(|&target| target >= layout.y_scalars())
            {
                return Err("WASM exact assignment target exceeds its issued Y layout".into());
            }
            // Never reject repeated targets belonging to distinct ordered programs.
            // Their new values become inputs of subsequent source programs.
            let mut targets = Vec::new();
            targets
                .try_reserve_exact(count)
                .map_err(|_| "WASM exact assignment target allocation failed")?;
            targets.extend_from_slice(issued.target_indices());
            maximum_tuple = maximum_tuple.max(count);
            programs.push(Program {
                block,
                targets,
                seed: None,
            });
        }
        Ok(Self {
            programs,
            maximum_tuple,
        })
    }
}
