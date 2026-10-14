use rumoca_exec_wasm::CompiledJacobianVWasm;
use rumoca_ir_solve::{ScalarProgramBlock, VarLayout};
use std::cell::RefCell;

#[cfg(test)]
mod tests;

pub(super) struct WasmJacobian {
    layout: VarLayout,
    source: ScalarProgramBlock,
    full: CompiledJacobianVWasm,
    selected: Vec<CompiledJacobianVWasm>,
    required_seed: usize,
    scratch: RefCell<Vec<f64>>,
}

impl WasmJacobian {
    pub(super) fn compile(source: &ScalarProgramBlock, layout: &VarLayout) -> Result<Self, String> {
        let selected = (0..source.row_count())
            .map(|program| {
                let block = super::profile::single_directional_program(source, program, layout)?;
                rumoca_exec_wasm::compile_jacobian_scalar_program_block_wasm(&block, layout)
                    .map_err(|error| error.to_string())
            })
            .collect::<Result<Vec<_>, String>>()?;
        let local = ScalarProgramBlock::with_program_spans(
            source.programs().to_vec(),
            source.program_spans().to_vec(),
        )
        .map_err(|error| error.to_string())?;
        let full = rumoca_exec_wasm::compile_jacobian_scalar_program_block_wasm(&local, layout)
            .map_err(|error| error.to_string())?;
        let required_seed = rumoca_eval_solve::PreparedScalarProgramBlock::new(source.clone())
            .map_err(|error| error.to_string())?
            .requirements()
            .seed_len;
        Ok(Self {
            layout: layout.clone(),
            source: source.clone(),
            full,
            selected,
            required_seed,
            scratch: RefCell::new(vec![0.0; source.stored_output_count()]),
        })
    }

    fn validate(
        &self,
        y: &[f64],
        p: &[f64],
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        super::profile::validate_inputs(&self.layout, y.len(), p.len(), tables.len())?;
        if seed.len() < self.required_seed {
            return Err(
                "WASM ME directional seed is shorter than the prepared source requires".into(),
            );
        }
        Ok(())
    }

    fn selected_value(
        &self,
        program: usize,
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<f64, rumoca_solver::RuntimeSolveError> {
        let code = self.selected.get(program).ok_or_else(|| {
            rumoca_solver::RuntimeSolveError::solve_ir(
                "WASM ME directional program is outside its prepared source",
            )
        })?;
        self.validate(inputs.y, inputs.p, inputs.seed, tables)?;
        let mut value = [0.0];
        code.call(inputs.y, inputs.p, inputs.t, inputs.seed, &mut value)
            .map_err(rumoca_solver::runtime::native_wasm_error::execution_error)?;
        Ok(value[0])
    }
}

impl rumoca_solver::CompiledSolveJacobianExpression for WasmJacobian {
    fn call_program_outputs(
        &self,
        program: usize,
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, rumoca_solver::RuntimeSolveError> {
        let value = self.selected_value(program, inputs, tables)?;
        out.try_reserve(1_usize.saturating_sub(out.len()))
            .map_err(|error| error.to_string())?;
        out.clear();
        out.push(value);
        Ok(true)
    }

    fn call_program_output(
        &self,
        (program, offset): (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, rumoca_solver::RuntimeSolveError> {
        if offset != 0 {
            return Err("WASM ME directional scalar output offset must be zero".into());
        }
        self.selected_value(
            program,
            rumoca_eval_solve::JacobianEvalInputs { y, p, t, seed },
            tables,
        )
        .map(Some)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        self.validate(y, p, seed, tables)?;
        if out.len() != self.source.output_count() {
            return Err(
                "WASM ME directional output length differs from its original source".into(),
            );
        }
        let mut values = self.scratch.borrow_mut();
        self.full
            .call(y, p, t, seed, &mut values)
            .map_err(rumoca_solver::runtime::native_wasm_error::execution_error)?;
        out.fill(0.0);
        for (&index, &value) in self.source.output_indices().iter().zip(values.iter()) {
            out[index] = value;
        }
        Ok(())
    }
}
