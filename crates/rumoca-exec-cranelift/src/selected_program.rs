//! Selected entries retain their original compiled source and executable arena.

use std::cell::RefCell;
use std::rc::Rc;

use super::*;

pub struct CompiledExpressionProgram {
    jit: Rc<emit::CompiledResidualRows>,
    _source: ScalarProgramBlock,
    program: usize,
    outputs: usize,
    scratch: RefCell<Vec<f64>>,
}

pub struct CompiledJacobianProgram {
    jit: Rc<emit::CompiledJacobianRows>,
    _source: ScalarProgramBlock,
    program: usize,
    outputs: usize,
    scratch: RefCell<Vec<f64>>,
}

fn source_output_count(
    issued: &ScalarProgramBlock,
    requested: &ScalarProgramBlock,
    program: usize,
) -> Result<usize, CompileError> {
    if !issued.shares_program_owner(requested) {
        return Err(CompileError::Input(
            "selected entry belongs to a different program owner".into(),
        ));
    }
    requested
        .programs()
        .get(program)
        .map(|operations| ScalarProgramBlock::program_output_count(operations))
        .ok_or_else(|| CompileError::Input("selected entry is outside its source programs".into()))
}

impl CompiledExpressionRows {
    /// Bind an actual independently emitted entry of this exact source owner.
    /// An aggregate batch declines before execution or new compilation.
    pub fn bind_program(
        &self,
        source: &ScalarProgramBlock,
        program: usize,
    ) -> Result<Option<CompiledExpressionProgram>, CompileError> {
        let outputs = source_output_count(&self.source, source, program)?;
        let Some(emitted_outputs) = self.jit.independent_program_output_count(program) else {
            return Ok(None);
        };
        if outputs != emitted_outputs {
            return Err(CompileError::Input(
                "selected entry does not cover its source outputs".into(),
            ));
        }
        Ok(Some(CompiledExpressionProgram {
            jit: Rc::clone(&self.jit),
            _source: self.source.clone(),
            program,
            outputs,
            scratch: RefCell::default(),
        }))
    }
}

impl CompiledJacobianV {
    /// Directional compilation emits one independent entry per source program.
    pub fn bind_program(
        &self,
        source: &ScalarProgramBlock,
        program: usize,
    ) -> Result<CompiledJacobianProgram, CompileError> {
        let outputs = source_output_count(&self.source, source, program)?;
        if self.jit.independent_program_output_count(program) != Some(outputs) {
            return Err(CompileError::Input(
                "selected directional entry does not cover its source outputs".into(),
            ));
        }
        Ok(CompiledJacobianProgram {
            jit: Rc::clone(&self.jit),
            _source: self.source.clone(),
            program,
            outputs,
            scratch: RefCell::default(),
        })
    }
}

impl CompiledExpressionProgram {
    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        if out.len() != self.outputs {
            return Err(CompileError::Input(
                "selected entry output extent differs from its source".into(),
            ));
        }
        let mut values = self.scratch.borrow_mut();
        if !self
            .jit
            .call_program_outputs(self.program, y, p, t, tables, &mut values)?
        {
            return Err(CompileError::Input(
                "issued independent program entry declined".into(),
            ));
        }
        out.copy_from_slice(&values);
        Ok(())
    }
}

impl CompiledJacobianProgram {
    pub fn call(
        &self,
        inputs: rumoca_eval_solve::JacobianEvalInputs<'_>,
        tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        if out.len() != self.outputs {
            return Err(CompileError::Input(
                "selected directional output extent differs from its source".into(),
            ));
        }
        let mut values = self.scratch.borrow_mut();
        self.jit
            .call_program_outputs(self.program, inputs, tables, &mut values)?;
        out.copy_from_slice(&values);
        Ok(())
    }
}
