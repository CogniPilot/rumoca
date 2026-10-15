use rumoca_ir_solve::{ScalarProgramBlock, SolvePureCallTable, VarLayout};

#[cfg(test)]
mod tests;

pub(super) struct WasmJacobian {
    code: super::expressions::WasmExpression,
    required_seed: usize,
}

impl WasmJacobian {
    pub(super) fn compile(
        source: &ScalarProgramBlock,
        layout: &VarLayout,
        calls: &SolvePureCallTable,
    ) -> Result<Self, String> {
        let required_seed = rumoca_eval_solve::PreparedScalarProgramBlock::new(source.clone())
            .map_err(|error| error.to_string())?
            .requirements()
            .seed_len;
        Ok(Self {
            code: super::expressions::WasmExpression::compile_with_seed(
                source,
                layout,
                calls,
                Some(required_seed),
            )?,
            required_seed,
        })
    }

    fn validate(
        &self,
        y: &[f64],
        p: &[f64],
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<(), rumoca_solver::RuntimeSolveError> {
        super::profile::validate_inputs(&self.code.layout, y.len(), p.len(), tables.len())?;
        if seed.len() < self.required_seed {
            return Err(
                "WASM directional seed is shorter than its original source requires".into(),
            );
        }
        Ok(())
    }
}

impl rumoca_solver::CompiledSolveJacobianExpression for WasmJacobian {
    fn bind_program(
        &self,
        source: &ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<std::rc::Rc<dyn rumoca_solver::CompiledSolveJacobianExpression>>, String>
    {
        self.code
            .bind_directional(source, program, self.required_seed)
            .map(Some)
    }

    fn call_program_outputs(
        &self,
        program: usize,
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, rumoca_solver::RuntimeSolveError> {
        self.validate(inputs.y, inputs.p, inputs.seed, tables)?;
        self.code.values_with_seed(program, inputs, tables, out)?;
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
        let source = self
            .code
            .source
            .program(program)
            .ok_or("WASM directional source program is absent")?;
        if offset >= ScalarProgramBlock::program_output_count(source) {
            return Err("WASM directional output offset exceeds its original program".into());
        }
        let mut values = Vec::new();
        self.call_program_outputs(
            program,
            rumoca_eval_solve::JacobianEvalInputs { y, p, t, seed },
            tables,
            &mut values,
        )?;
        Ok(Some(values[offset]))
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
        self.code.call_with_seed(
            rumoca_eval_solve::JacobianEvalInputs { y, p, t, seed },
            tables,
            out,
        )
    }
}
