//! Bind the already emitted entry of the exact retained scalar source.
use super::*;
use rumoca_eval_solve::JacobianEvalInputs;
use rumoca_exec_wasm::CompiledPrivateProgramWasm;
use rumoca_solver::{CompiledSolveExpression, CompiledSolveJacobianExpression, RuntimeSolveError};
use std::cell::RefMut;

#[cfg(test)]
mod tests;

pub(super) type Program = (CompiledPrivateProgramWasm, RefCell<Vec<f64>>);

struct BoundProgram {
    _source: ScalarProgramBlock,
    layout: Rc<VarLayout>,
    code: Rc<Program>,
    outputs: usize,
}

struct NumericProgram(BoundProgram);

struct DirectionalProgram {
    program: BoundProgram,
    required_seed: usize,
}

impl WasmExpression {
    fn bind_selected(
        &self,
        source: &ScalarProgramBlock,
        program: usize,
    ) -> Result<BoundProgram, String> {
        if !self.source.shares_program_owner(source) {
            return Err("WASM selected entry belongs to a different program owner".into());
        }
        let operations = source
            .program(program)
            .ok_or("WASM selected entry is outside its source programs")?;
        let outputs = ScalarProgramBlock::program_output_count(operations);
        let code = self
            .selected
            .get(program)
            .ok_or("WASM selected entry has no emitted program")?;
        if code.0.output_count() != outputs {
            return Err("WASM selected entry differs from its complete source output tuple".into());
        }
        Ok(BoundProgram {
            _source: self.source.clone(),
            layout: Rc::clone(&self.layout),
            code: Rc::clone(code),
            outputs,
        })
    }

    pub(super) fn bind_numeric(
        &self,
        source: &ScalarProgramBlock,
        program: usize,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        if self.directional {
            return Err("WASM directional program cannot bind a numeric entry".into());
        }
        self.bind_selected(source, program)
            .map(|program| Rc::new(NumericProgram(program)) as Rc<_>)
    }

    pub(in crate::wasm_execution) fn bind_directional(
        &self,
        source: &ScalarProgramBlock,
        program: usize,
        required_seed: usize,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        if !self.directional {
            return Err("WASM numeric program cannot bind a directional entry".into());
        }
        self.bind_selected(source, program).map(|program| {
            Rc::new(DirectionalProgram {
                program,
                required_seed,
            }) as Rc<_>
        })
    }
}

pub(super) fn invoke<'a>(
    program: &'a Program,
    layout: &VarLayout,
    inputs: JacobianEvalInputs<'_>,
    tables: &[rumoca_core::ExternalTableData],
    directional: bool,
) -> Result<RefMut<'a, Vec<f64>>, RuntimeSolveError> {
    let JacobianEvalInputs { y, p, t, seed } = inputs;
    profile::validate_inputs(layout, y.len(), p.len(), tables.len())?;
    let mut scratch = program.1.borrow_mut();
    if directional {
        program.0.call_directional(y, p, t, seed, &mut scratch)
    } else {
        program.0.call(y, p, t, &mut scratch)
    }
    .map_err(rumoca_solver::runtime::native_wasm_error::execution_error)?;
    Ok(scratch)
}

impl BoundProgram {
    fn call(
        &self,
        inputs: JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        directional: bool,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if out.len() != self.outputs {
            return Err("WASM selected output extent differs from its source tuple".into());
        }
        let values = invoke(&self.code, &self.layout, inputs, tables, directional)?;
        out.copy_from_slice(&values[..self.outputs]);
        Ok(())
    }
}

impl CompiledSolveExpression for NumericProgram {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.0.call(
            JacobianEvalInputs { y, p, t, seed: &[] },
            tables,
            false,
            out,
        )
    }
}

impl CompiledSolveJacobianExpression for DirectionalProgram {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if seed.len() < self.required_seed {
            return Err(
                "WASM selected directional seed is shorter than its source requires".into(),
            );
        }
        self.program
            .call(JacobianEvalInputs { y, p, t, seed }, tables, true, out)
    }
}
