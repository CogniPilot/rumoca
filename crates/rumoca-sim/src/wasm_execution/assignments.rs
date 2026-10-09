//! Complete issued refresh schedules keep tuple publication and fault order.

use rumoca_exec_wasm::CompiledExactAssignmentWasm;
use rumoca_ir_solve::VarLayout;
use std::cell::RefCell;

pub(super) struct WasmAssignments {
    compiled: CompiledExactAssignmentWasm,
    layout: VarLayout,
    scratch: RefCell<Vec<f64>>,
}

impl WasmAssignments {
    pub(super) fn compile(
        source: &rumoca_ir_solve::ComputeBlock,
        owners: &rumoca_ir_solve::ContinuousRefreshOwners,
        schedule: &rumoca_ir_solve::ExactRefreshAssignmentSchedule,
        layout: &VarLayout,
        calls: &rumoca_ir_solve::SolvePureCallTable,
    ) -> Result<Self, String> {
        let compiled = rumoca_exec_wasm::compile_exact_assignment_schedule_wasm(
            source, owners, schedule, layout, calls,
        )
        .map_err(|error| error.to_string())?;
        let count = usize::try_from(compiled.scratch_bytes())
            .map_err(|_| "WASM ME assignment scratch exceeds usize")?
            .div_ceil(8);
        let mut scratch = Vec::new();
        scratch
            .try_reserve_exact(count)
            .map_err(|error| error.to_string())?;
        scratch.resize(count, 0.0);
        Ok(Self {
            compiled,
            layout: layout.clone(),
            scratch: RefCell::new(scratch),
        })
    }
}

impl rumoca_solver::CompiledSolveAssignmentSchedule for WasmAssignments {
    fn call(
        &self,
        y: &mut [f64],
        p: &[f64],
        time: f64,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        super::profile::validate_inputs(&self.layout, y.len(), p.len(), tables.len())?;
        self.compiled
            .call(y, p, time, &mut self.scratch.borrow_mut())
            .map_err(super::errors::execution_error)
    }
}
