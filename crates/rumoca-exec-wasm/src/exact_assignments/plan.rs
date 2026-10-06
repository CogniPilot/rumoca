//! Final emission plan from existing construction-issued exact owners.
use rumoca_ir_solve::{
    ComputeBlock, ContinuousRefreshOwners, ExactRefreshAssignmentSchedule, ScalarProgramBlock,
    VarLayout,
};

pub(crate) struct Program {
    pub block: ScalarProgramBlock,
    pub targets: Vec<usize>,
}

pub(crate) struct Plan {
    pub programs: Vec<Program>,
    pub maximum_tuple: usize,
}

impl Plan {
    pub(crate) fn private(block: &ScalarProgramBlock) -> Result<Self, String> {
        if block.programs().len() != 1 {
            return Err("private entry requires one complete checked program".into());
        }
        let maximum_tuple = block.stored_output_count();
        if maximum_tuple == 0 {
            return Err("private entry has no checked output".into());
        }
        Ok(Self {
            programs: vec![Program {
                block: block.clone(),
                targets: Vec::new(),
            }],
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
            programs.push(Program { block, targets });
        }
        Ok(Self {
            programs,
            maximum_tuple,
        })
    }
}
