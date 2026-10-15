//! One compiled typed owner executes the complete event-entry tuple atomically.
#[cfg(test)]
mod tests;

use rumoca_exec_wasm::PreparedTypedCallWasm;
use rumoca_ir_solve::{
    EventTransactionProgram, SolvePureCallTable, SolveScalarType, SolveValueKind,
};
use std::cell::RefCell;

pub(super) struct WasmEventTransaction {
    compiled: PreparedTypedCallWasm,
    cells: RefCell<(Vec<u64>, Vec<u64>, Vec<u64>)>,
}

impl WasmEventTransaction {
    pub(super) fn compile(
        program: &EventTransactionProgram,
        calls: &SolvePureCallTable,
    ) -> Result<Self, String> {
        let compiled = PreparedTypedCallWasm::compile(calls, program.site())
            .map_err(|error| error.to_string())?;
        let layout = compiled.artifact().layout();
        let cells = RefCell::new((
            vec![0; (layout.input_bytes / 8) as usize],
            vec![0; (layout.output_bytes / 8) as usize],
            vec![0; (layout.scratch_bytes / 8) as usize],
        ));
        Ok(Self { compiled, cells })
    }
}

impl rumoca_solver::CompiledSolveEventTransaction for WasmEventTransaction {
    fn call(
        &self,
        input: &[f64],
        output: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        let layout = self.compiled.artifact().layout();
        if input.len().checked_mul(8) != Some(layout.input_bytes as usize)
            || output.len().checked_mul(8) != Some(layout.output_bytes as usize)
        {
            return Err("WASM event transaction differs from its original typed interface".into());
        }
        let binary64_required = "WASM event transaction requires Binary64";
        let invalid_boolean = "WASM event transaction returned an invalid Boolean";
        let mut cells = self.cells.borrow_mut();
        let (arguments, results, scratch) = &mut *cells;
        let mut offset = 0;
        for value_type in &layout.inputs {
            for _ in 0..value_type.scalar_count() {
                arguments[offset] =
                    match rumoca_eval_solve::typed_kind_from_scalar(input[offset], value_type)? {
                        SolveValueKind::Real64(bits) => bits,
                        SolveValueKind::Integer(value) => value as u64,
                        SolveValueKind::Boolean(value) => u64::from(value),
                        SolveValueKind::Real32(_) => return Err(binary64_required.into()),
                    };
                offset += 1;
            }
        }
        self.compiled
            .call(arguments, results, scratch)
            .map_err(rumoca_solver::runtime::native_wasm_error::execution_error)?;
        let mut staged = Vec::with_capacity(output.len());
        let mut offset = 0;
        for result in &layout.outputs {
            for _ in 0..result.value_type().scalar_count() {
                let bits = results[offset];
                let kind = match result.value_type().element_type() {
                    SolveScalarType::Real { .. } => SolveValueKind::Real64(bits),
                    SolveScalarType::Integer(_) => SolveValueKind::Integer(bits as i64),
                    SolveScalarType::Boolean if bits <= 1 => SolveValueKind::Boolean(bits != 0),
                    SolveScalarType::Boolean => return Err(invalid_boolean.into()),
                };
                staged.push(rumoca_eval_solve::checked_scalar_payload_value(kind)?);
                offset += 1;
            }
        }
        output.copy_from_slice(&staged);
        Ok(())
    }
}
