//! Cranelift compiled execution adapter for Solve-IR row blocks.
//!
//! This crate compiles already-lowered Solve-IR through Cranelift and exposes
//! callable residual/JVP functions. It does not use Jinja templates because
//! Cranelift is a programmatic JIT API rather than a textual-code target, and
//! it must not own Modelica semantics, DAE lowering, or structural rewrites.

mod emit;

use rumoca_ir_solve::ScalarProgramBlock;
use std::{cell::RefCell, rc::Rc};

#[derive(Debug)]
pub enum CompileError {
    Backend(String),
    Input(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Backend(msg) => write!(f, "cranelift execution error: {msg}"),
            Self::Input(msg) => write!(f, "invalid input: {msg}"),
        }
    }
}

impl std::error::Error for CompileError {}

impl CompiledPureCallTable {
    /// Invoke one exact checked owner from flattened runtime storage values.
    ///
    /// `input_cells` and `output_cells` are caller-owned reusable final-ABI
    /// buffers; aggregate shapes remain in `site` and are not scalar IR.
    pub fn call_scalar_payload(
        &self,
        site: &rumoca_ir_solve::SolvePureCallSite,
        input: &[f64],
        output: &mut [f64],
        input_cells: &mut Vec<u64>,
        output_cells: &mut Vec<u64>,
    ) -> Result<(), CompileError> {
        let input_count = site.inputs().iter().try_fold(0usize, |count, value_type| {
            count
                .checked_add(value_type.scalar_count() as usize)
                .ok_or_else(|| CompileError::Input("typed input payload count overflows".into()))
        })?;
        let output_count = site
            .output_scalar_count()
            .ok_or_else(|| CompileError::Input("typed output payload count overflows".into()))?;
        if input.len() != input_count || output.len() != output_count {
            return Err(CompileError::Input(format!(
                "typed scalar payload has {}/{} values; expected {input_count}/{output_count}",
                input.len(),
                output.len()
            )));
        }
        input_cells.clear();
        input_cells.reserve(input_count);
        let mut input_offset = 0usize;
        for value_type in site.inputs() {
            let end = input_offset + value_type.scalar_count() as usize;
            encode_typed_cells(value_type, &input[input_offset..end], input_cells)?;
            input_offset = end;
        }
        output_cells.clear();
        output_cells.resize(output_count, 0);
        self.jit.call_cells(site, input_cells, output_cells)?;
        let mut output_offset = 0usize;
        for value in site.outputs() {
            let count = value.value_type().scalar_count() as usize;
            let end = output_offset + count;
            decode_typed_cells(
                value.value_type(),
                &output_cells[output_offset..end],
                &mut output[output_offset..end],
            );
            output_offset = end;
        }
        Ok(())
    }
}

fn encode_typed_cells(
    value_type: &rumoca_ir_solve::SolveValueType,
    values: &[f64],
    cells: &mut Vec<u64>,
) -> Result<(), CompileError> {
    use rumoca_ir_solve::{SolveRealFormat, SolveScalarType};
    match value_type.element_type() {
        SolveScalarType::Real {
            format: SolveRealFormat::Binary32,
            ..
        } => cells.extend(
            values
                .iter()
                .map(|value| u64::from((*value as f32).to_bits())),
        ),
        SolveScalarType::Real {
            format: SolveRealFormat::Binary64,
            ..
        } => cells.extend(values.iter().map(|value| value.to_bits())),
        SolveScalarType::Integer(domain) => {
            for &value in values {
                if !value.is_finite() || value.fract() != 0.0 {
                    return Err(CompileError::Input(
                        "typed Integer input is not integral".into(),
                    ));
                }
                let integer = value as i64;
                if !domain.contains(integer) || integer as f64 != value {
                    return Err(CompileError::Input(
                        "typed Integer input is outside its domain".into(),
                    ));
                }
                cells.push(integer as u64);
            }
        }
        SolveScalarType::Boolean => {
            cells.extend(values.iter().map(|value| u64::from(*value != 0.0)));
        }
    }
    Ok(())
}

fn decode_typed_cells(
    value_type: &rumoca_ir_solve::SolveValueType,
    cells: &[u64],
    values: &mut [f64],
) {
    use rumoca_ir_solve::{SolveRealFormat, SolveScalarType};
    for (value, cell) in values.iter_mut().zip(cells) {
        *value = match value_type.element_type() {
            SolveScalarType::Real {
                format: SolveRealFormat::Binary32,
                ..
            } => f32::from_bits(*cell as u32) as f64,
            SolveScalarType::Real {
                format: SolveRealFormat::Binary64,
                ..
            } => f64::from_bits(*cell),
            SolveScalarType::Integer(_) => *cell as i64 as f64,
            SolveScalarType::Boolean => f64::from(*cell != 0),
        };
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct CompiledInputRequirements {
    pub y_len: usize,
    pub p_len: usize,
    pub seed_len: usize,
}

impl CompiledInputRequirements {
    fn from_emit(requirements: emit::EmitInputRequirements) -> Self {
        Self {
            y_len: requirements.y_len,
            p_len: requirements.p_len,
            seed_len: requirements.seed_len,
        }
    }
}

pub struct CompiledJacobianV {
    jit: emit::CompiledJacobianRows,
    output_placement: Option<OutputPlacement>,
}

impl CompiledJacobianV {
    pub fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        call_with_output_placement(self.output_placement.as_ref(), out, |dense| {
            self.jit.call(y, p, t, v, dense)
        })
    }

    pub fn rows(&self) -> usize {
        self.jit.rows()
    }

    pub fn input_requirements(&self) -> CompiledInputRequirements {
        CompiledInputRequirements::from_emit(self.jit.input_requirements())
    }
}

pub struct CompiledExpressionRows {
    jit: emit::CompiledResidualRows,
    output_placement: Option<OutputPlacement>,
}

struct OutputPlacement {
    indices: Box<[usize]>,
    required_output_len: usize,
    scratch: RefCell<Vec<f64>>,
}

impl OutputPlacement {
    fn for_block(block: &ScalarProgramBlock) -> Option<Self> {
        let indices = block.output_indices();
        if indices.iter().copied().eq(0..indices.len()) {
            return None;
        }
        Some(Self {
            required_output_len: block.output_count(),
            indices: indices.to_vec().into_boxed_slice(),
            scratch: RefCell::new(Vec::new()),
        })
    }
}

fn call_with_output_placement(
    placement: Option<&OutputPlacement>,
    out: &mut [f64],
    call: impl FnOnce(&mut [f64]) -> Result<(), CompileError>,
) -> Result<(), CompileError> {
    let Some(placement) = placement else {
        return call(out);
    };
    if out.len() < placement.required_output_len {
        return Err(CompileError::Input(format!(
            "compiled sparse output requires {} values, got {}",
            placement.required_output_len,
            out.len()
        )));
    }
    let mut scratch = placement.scratch.borrow_mut();
    scratch.resize(placement.indices.len(), 0.0);
    call(&mut scratch)?;
    for (&index, &value) in placement.indices.iter().zip(scratch.iter()) {
        out[index] = value;
    }
    Ok(())
}

/// Native machine-code helpers for one checked model-level typed pure-call
/// table. Clones share the one JIT module and therefore never recompile owners.
#[derive(Clone)]
pub struct CompiledPureCallTable {
    jit: Rc<emit::typed_program::CompiledPureCallTable>,
}

pub fn compile_pure_call_table(
    table: &rumoca_ir_solve::SolvePureCallTable,
) -> Result<CompiledPureCallTable, CompileError> {
    emit::typed_program::CompiledPureCallTable::compile(table)
        .map(|jit| CompiledPureCallTable { jit: Rc::new(jit) })
}

/// One native function containing an ordered sequence of exact algebraic
/// assignments. Each row writes directly to its compiler-owned solver-Y slot,
/// so later rows observe earlier writes exactly as in the causal interpreter.
pub struct CompiledAssignmentSchedule {
    jit: emit::CompiledAssignmentSchedule,
}

impl CompiledAssignmentSchedule {
    pub fn call(&self, y: &mut [f64], p: &[f64], t: f64) -> Result<(), CompileError> {
        self.jit.call(y, p, t)
    }

    pub fn rows(&self) -> usize {
        self.jit.rows()
    }
}

impl CompiledExpressionRows {
    pub fn call(&self, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) -> Result<(), CompileError> {
        call_with_output_placement(self.output_placement.as_ref(), out, |dense| {
            self.jit.call(y, p, t, dense)
        })
    }

    pub fn rows(&self) -> usize {
        self.jit.rows()
    }

    pub fn input_requirements(&self) -> CompiledInputRequirements {
        CompiledInputRequirements::from_emit(self.jit.input_requirements())
    }
}

pub fn compile_jacobian_scalar_program_block(
    rows: &ScalarProgramBlock,
) -> Result<CompiledJacobianV, CompileError> {
    let jit = emit::compile_jacobian_rows(rows)?;
    Ok(CompiledJacobianV {
        jit,
        output_placement: OutputPlacement::for_block(rows),
    })
}

pub fn compile_expression_scalar_program_block(
    rows: &ScalarProgramBlock,
) -> Result<CompiledExpressionRows, CompileError> {
    let jit = emit::compile_residual_rows(rows)?;
    Ok(CompiledExpressionRows {
        jit,
        output_placement: OutputPlacement::for_block(rows),
    })
}

pub fn compile_expression_scalar_program_block_with_pure_calls(
    rows: &ScalarProgramBlock,
    pure_calls: &CompiledPureCallTable,
) -> Result<CompiledExpressionRows, CompileError> {
    let jit = emit::compile_residual_rows_with_pure_calls(rows, pure_calls.jit.clone())?;
    Ok(CompiledExpressionRows {
        jit,
        output_placement: OutputPlacement::for_block(rows),
    })
}

pub fn compile_jacobian_scalar_program_block_with_pure_calls(
    rows: &ScalarProgramBlock,
    pure_calls: &CompiledPureCallTable,
) -> Result<CompiledJacobianV, CompileError> {
    let jit = emit::compile_jacobian_rows_with_pure_calls(rows, pure_calls.jit.clone())?;
    Ok(CompiledJacobianV {
        jit,
        output_placement: OutputPlacement::for_block(rows),
    })
}

#[cfg(test)]
fn compile_assignment_fixture(
    rows: &ScalarProgramBlock,
    target_y_indices: &[usize],
) -> Result<CompiledAssignmentSchedule, CompileError> {
    emit::compile_assignment_fixture(rows, target_y_indices)
        .map(|jit| CompiledAssignmentSchedule { jit })
}

pub fn compile_exact_refresh_assignment(
    execution: &rumoca_ir_solve::ExactRefreshAssignmentExecution<'_>,
) -> Result<CompiledAssignmentSchedule, CompileError> {
    emit::compile_exact_refresh_assignment(execution).map(|jit| CompiledAssignmentSchedule { jit })
}

#[cfg(test)]
mod tests;
