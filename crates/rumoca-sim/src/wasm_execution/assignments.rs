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
    #[cfg(not(all(feature = "precompiled-runtime", target_arch = "wasm32")))]
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
        Self::from_compiled(compiled, layout)
    }

    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    pub(super) fn compile_product(
        source: &rumoca_ir_solve::ComputeBlock,
        owners: &rumoca_ir_solve::ContinuousRefreshOwners,
        schedule: &rumoca_ir_solve::ExactRefreshAssignmentSchedule,
        layout: &VarLayout,
        calls: &rumoca_ir_solve::SolvePureCallTable,
        mode: Option<&super::precompiled::ProgramMode>,
    ) -> Result<Self, String> {
        let key = rumoca_ir_solve::PreparedKernelSource::new(layout, calls).inspect_err(|_| {
            if let Some(mode) = mode {
                mode.unsupported("assignment source identity refused");
            }
        })?;
        if let Some(result) = mode.and_then(|mode| mode.exact(&key, source, layout)) {
            return Self::from_compiled(result?, layout);
        }
        let compiled = rumoca_exec_wasm::compile_exact_assignment_schedule_wasm(
            source, owners, schedule, layout, calls,
        )
        .map_err(|error| {
            if let Some(mode) = mode {
                mode.unsupported("assignment compilation refused");
            }
            error.to_string()
        })?;
        if let Some(mode) = mode {
            mode.record_exact(&compiled);
        }
        Self::from_compiled(compiled, layout)
    }

    fn from_compiled(
        compiled: CompiledExactAssignmentWasm,
        layout: &VarLayout,
    ) -> Result<Self, String> {
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
            .map_err(rumoca_solver::runtime::native_wasm_error::execution_error)
    }
}
