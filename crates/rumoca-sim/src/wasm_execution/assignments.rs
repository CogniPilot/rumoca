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

    /// The causal chain of an issued torn sweep as one schedule call.
    #[cfg(not(all(feature = "precompiled-runtime", target_arch = "wasm32")))]
    pub(super) fn compile_torn(
        sweep: &rumoca_eval_solve::TornSweepComposite,
        layout: &VarLayout,
        calls: &rumoca_ir_solve::SolvePureCallTable,
    ) -> Result<Self, String> {
        let (rows, chunks) = torn_rows(sweep)?;
        let compiled = rumoca_exec_wasm::compile_torn_assignment_rows_wasm(
            &rows,
            &sweep.assignment_targets,
            &chunks,
            layout,
            calls,
        )
        .map_err(|error| error.to_string())?;
        Self::from_compiled(compiled, layout)
    }

    #[cfg(all(feature = "precompiled-runtime", target_arch = "wasm32"))]
    pub(super) fn compile_torn_product(
        sweep: &rumoca_eval_solve::TornSweepComposite,
        layout: &VarLayout,
        calls: &rumoca_ir_solve::SolvePureCallTable,
        mode: Option<&super::precompiled::ProgramMode>,
    ) -> Result<Self, String> {
        let refused = |kind: &str| {
            if let Some(mode) = mode {
                mode.unsupported(kind);
            }
        };
        let (rows, chunks) = torn_rows(sweep).inspect_err(|_| refused("torn rows refused"))?;
        let key = rumoca_ir_solve::PreparedKernelSource::new(layout, calls)
            .inspect_err(|_| refused("torn assignment source identity refused"))?;
        let issued = (
            &rows,
            sweep.assignment_targets.as_slice(),
            chunks.as_slice(),
        );
        if let Some(result) = mode.and_then(|mode| mode.torn(&key, issued, layout)) {
            return Self::from_compiled(result?, layout);
        }
        let compiled = rumoca_exec_wasm::compile_torn_assignment_rows_wasm(
            &rows,
            &sweep.assignment_targets,
            &chunks,
            layout,
            calls,
        )
        .map_err(|error| {
            refused("torn assignment compilation refused");
            error.to_string()
        })?;
        if let Some(mode) = mode {
            mode.record_torn(&compiled);
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

/// The issued rows of a torn sweep as one spanned block, with their chunks.
fn torn_rows(
    sweep: &rumoca_eval_solve::TornSweepComposite,
) -> Result<
    (
        rumoca_ir_solve::ScalarProgramBlock,
        Vec<std::ops::Range<usize>>,
    ),
    String,
> {
    let rows = rumoca_ir_solve::ScalarProgramBlock::with_program_spans(
        sweep.assignment_rows.clone(),
        sweep.assignment_spans.clone(),
    )
    .map_err(|error| error.to_string())?;
    Ok((rows, sweep.assignment_chunks().collect()))
}
