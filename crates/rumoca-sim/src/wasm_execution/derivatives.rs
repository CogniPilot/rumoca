//! Runtime adapter retaining the original source and borrowing the global cache.
use rumoca_exec_wasm::{
    BoundFmiDerivativeOperation, FmiDerivativeCall, FmiDerivativeFailure, FmiKinematicCall,
    PrecompiledFmiDerivativeOperation, compile_fmi_derivative_operation_wasm,
};
use rumoca_ir_solve::{RetainedAssertionRoot, fmi::FmiDerivativeCausalRefusal};
use rumoca_solver::{
    CompiledSolveDerivativeOperation, RuntimeSolveError, SolveDerivativeCacheView,
    SolveDerivativeOperationFailure,
};
use std::cell::RefCell;

pub(super) struct WasmDerivative {
    root: RetainedAssertionRoot,
    compiled: BoundFmiDerivativeOperation,
    kinematic_rows: Result<Box<[usize]>, String>,
    workspace: RefCell<Vec<u64>>,
}
impl WasmDerivative {
    pub(super) fn prepare(
        root: RetainedAssertionRoot,
        mode: Option<&super::precompiled::ProgramMode>,
    ) -> Result<Option<Self>, String> {
        let operation = match root
            .try_derivative_operation()
            .map_err(|error| error.to_string())?
            .try_causal_operation()
        {
            Ok(operation) => operation,
            Err(FmiDerivativeCausalRefusal::ProjectionRequired) => return Ok(None),
            Err(error) => return Err(error.to_string()),
        };
        let compiled = if let Some(mode) = mode {
            mode.derivative_operation(&operation)?
        } else {
            let code = compile_fmi_derivative_operation_wasm(&operation)
                .map_err(|error| error.to_string())?;
            PrecompiledFmiDerivativeOperation::from_compiled(&code)
                .bind(&operation)
                .map_err(|error| error.to_string())?
        };
        let kinematic_rows = operation
            .kinematic_issuance()
            .map(|kinematic| kinematic.rows().iter().collect())
            .map_err(|refusal| refusal.to_string());
        let count = (compiled.workspace().bytes() as usize).div_ceil(8);
        let mut workspace = Vec::new();
        workspace
            .try_reserve_exact(count)
            .map_err(|error| error.to_string())?;
        workspace.resize(count, 0);
        Ok(Some(Self {
            root,
            compiled,
            kinematic_rows,
            workspace: RefCell::new(workspace),
        }))
    }

    fn failure(&self, error: FmiDerivativeFailure) -> SolveDerivativeOperationFailure {
        let phase = error.phase();
        let error = match error {
            FmiDerivativeFailure::NonFinite { output } => RuntimeSolveError::NonFiniteDerivative {
                state_name: self
                    .root
                    .model()
                    .visible_projection()
                    .name(output)
                    .unwrap_or_else(|| format!("state[{output}]")),
            },
            FmiDerivativeFailure::Entry(error) | FmiDerivativeFailure::Helper { error, .. } => {
                rumoca_solver::runtime::native_wasm_error::execution_error(error)
            }
        };
        SolveDerivativeOperationFailure { phase, error }
    }
}
impl CompiledSolveDerivativeOperation for WasmDerivative {
    fn call(
        &self,
        y: &mut [f64],
        parameters: &[f64],
        time: f64,
        tables: &[rumoca_core::ExternalTableData],
        output: &mut [f64],
        cache: SolveDerivativeCacheView<'_>,
    ) -> Result<(), SolveDerivativeOperationFailure> {
        let early = |error| SolveDerivativeOperationFailure { phase: None, error };
        super::profile::validate_inputs(
            &self.root.model().problem.layout,
            y.len(),
            parameters.len(),
            tables.len(),
        )
        .map_err(|error| early(RuntimeSolveError::solve_ir(error)))?;
        let mut workspace = self.workspace.try_borrow_mut().map_err(|_| {
            early(RuntimeSolveError::solve_ir(
                "complete derivative workspace is reentrant",
            ))
        })?;
        self.compiled
            .call(FmiDerivativeCall {
                y,
                parameters,
                time,
                output,
                workspace: &mut workspace,
                cache_values: cache.values,
                cache_present: cache.present,
                cache_parameters: cache.key_parameters,
                cache_valid: cache.valid,
            })
            .map_err(|error| self.failure(error))
    }

    fn kinematic_entry(&self) -> Result<(), String> {
        self.kinematic_rows
            .as_ref()
            .map(|_| ())
            .map_err(Clone::clone)
    }

    fn call_kinematic(
        &self,
        y: &[f64],
        parameters: &[f64],
        time: f64,
        tables: &[rumoca_core::ExternalTableData],
        rows: &[usize],
        output: &mut [f64],
    ) -> Option<Result<(), SolveDerivativeOperationFailure>> {
        if self.kinematic_rows.as_deref() != Ok(rows) {
            return None;
        }
        let early = |error| SolveDerivativeOperationFailure { phase: None, error };
        let result = super::profile::validate_inputs(
            &self.root.model().problem.layout,
            y.len(),
            parameters.len(),
            tables.len(),
        )
        .map_err(|error| early(RuntimeSolveError::solve_ir(error)))
        .and_then(|()| {
            let mut workspace = self.workspace.try_borrow_mut().map_err(|_| {
                early(RuntimeSolveError::solve_ir(
                    "complete derivative workspace is reentrant",
                ))
            })?;
            self.compiled
                .call_kinematic(FmiKinematicCall {
                    y,
                    parameters,
                    time,
                    output,
                    workspace: &mut workspace,
                })
                .map_err(|error| self.failure(error))
        });
        Some(result)
    }
}
