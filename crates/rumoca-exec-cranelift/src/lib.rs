//! Cranelift compiled execution adapter for Solve-IR row blocks.
//!
//! This crate compiles already-lowered Solve-IR through Cranelift and exposes
//! callable residual/JVP functions. It does not use Jinja templates because
//! Cranelift is a programmatic JIT API rather than a textual-code target, and
//! it must not own Modelica semantics, DAE lowering, or structural rewrites.

mod emit;

use rumoca_core::ExternalTableData;
use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};

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

pub struct CompiledResidual {
    jit: emit::CompiledResidualRows,
}

impl CompiledResidual {
    pub fn call(&self, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) -> Result<(), CompileError> {
        self.jit.call(y, p, t, out)
    }

    pub fn call_with_external_tables(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        self.jit
            .call_with_external_tables(y, p, t, external_tables, out)
    }

    pub fn rows(&self) -> usize {
        self.jit.rows()
    }

    pub fn input_requirements(&self) -> CompiledInputRequirements {
        CompiledInputRequirements::from_emit(self.jit.input_requirements())
    }
}

pub struct CompiledJacobianV {
    jit: emit::CompiledJacobianRows,
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
        self.jit.call(y, p, t, v, out)
    }

    pub fn call_with_external_tables(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        self.jit
            .call_with_external_tables(y, p, t, v, external_tables, out)
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

    pub fn call_with_external_tables(
        &self,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
    ) -> Result<(), CompileError> {
        self.jit.call_with_external_tables(y, p, t, external_tables)
    }

    pub fn rows(&self) -> usize {
        self.jit.rows()
    }
}

impl CompiledExpressionRows {
    pub fn call(&self, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) -> Result<(), CompileError> {
        self.jit.call(y, p, t, out)
    }

    pub fn call_with_external_tables(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        self.jit
            .call_with_external_tables(y, p, t, external_tables, out)
    }

    pub fn rows(&self) -> usize {
        self.jit.rows()
    }

    pub fn input_requirements(&self) -> CompiledInputRequirements {
        CompiledInputRequirements::from_emit(self.jit.input_requirements())
    }
}

pub fn compile_residual_scalar_program_block(
    rows: &ScalarProgramBlock,
) -> Result<CompiledResidual, CompileError> {
    let jit = emit::compile_residual_rows(rows.programs())?;
    Ok(CompiledResidual { jit })
}

pub fn compile_jacobian_scalar_program_block(
    rows: &ScalarProgramBlock,
) -> Result<CompiledJacobianV, CompileError> {
    let jit = emit::compile_jacobian_rows(rows.programs())?;
    Ok(CompiledJacobianV { jit })
}

pub fn compile_expression_scalar_program_block(
    rows: &ScalarProgramBlock,
) -> Result<CompiledExpressionRows, CompileError> {
    let jit = emit::compile_residual_rows(rows.programs())?;
    Ok(CompiledExpressionRows { jit })
}

pub fn compile_assignment_schedule(
    rows: &[Vec<LinearOp>],
    target_y_indices: &[usize],
) -> Result<CompiledAssignmentSchedule, CompileError> {
    emit::compile_assignment_schedule(rows, target_y_indices)
        .map(|jit| CompiledAssignmentSchedule { jit })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};

    fn fixture_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("exec_cranelift_source_53.mo"),
            0,
            1,
        )
    }

    #[test]
    fn compiles_constant_scalar_program_block() {
        let rows = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 3.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span()
                .require_provenance("Cranelift constant fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture program is computable");
        let compiled = compile_expression_scalar_program_block(&rows).expect("compile row");
        let mut out = [0.0];

        compiled.call(&[], &[], 0.0, &mut out).expect("call row");

        assert_eq!(out[0], 3.0);
    }

    #[test]
    fn compiled_expression_reports_input_requirements() {
        let rows = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 2 },
                LinearOp::LoadP { dst: 1, index: 1 },
                LinearOp::Binary {
                    dst: 2,
                    op: rumoca_ir_solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            fixture_span()
                .require_provenance("Cranelift input-requirement fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture program is computable");
        let compiled = compile_expression_scalar_program_block(&rows).expect("compile row");

        assert_eq!(
            compiled.input_requirements(),
            CompiledInputRequirements {
                y_len: 3,
                p_len: 2,
                seed_len: 0,
            }
        );
    }

    #[test]
    fn compiled_assignment_schedule_preserves_ordered_y_dependencies() {
        let rows = vec![
            vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                LinearOp::Const { dst: 1, value: 2.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: rumoca_ir_solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
        ];
        let compiled = compile_assignment_schedule(&rows, &[1, 0]).expect("compile schedule");
        let mut y = [0.0, 0.0];

        compiled.call(&mut y, &[3.0], 0.0).expect("run schedule");

        assert_eq!(y, [6.0, 3.0]);
    }

    #[test]
    fn compiled_assignment_schedule_prevalidates_before_mutating_y() {
        let rows = vec![vec![
            LinearOp::LoadP { dst: 0, index: 1 },
            LinearOp::StoreOutput { src: 0 },
        ]];
        let compiled = compile_assignment_schedule(&rows, &[0]).expect("compile schedule");
        let mut y = [7.0];

        compiled
            .call(&mut y, &[3.0], 0.0)
            .expect_err("short parameter input must fail");

        assert_eq!(y, [7.0]);
    }

    #[test]
    fn compiled_jacobian_reports_seed_requirements() {
        let rows = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadSeed { dst: 0, index: 2 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            fixture_span()
                .require_provenance("Cranelift Jacobian fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture program is computable");
        let compiled = compile_jacobian_scalar_program_block(&rows).expect("compile row");

        assert_eq!(
            compiled.input_requirements(),
            CompiledInputRequirements {
                y_len: 0,
                p_len: 0,
                seed_len: 3,
            }
        );
    }
}
