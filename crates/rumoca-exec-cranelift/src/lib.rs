//! Cranelift compiled execution adapter for Solve-IR row blocks.
//!
//! This crate compiles already-lowered Solve-IR through Cranelift and exposes
//! callable residual/JVP functions. It does not use Jinja templates because
//! Cranelift is a programmatic JIT API rather than a textual-code target, and
//! it must not own Modelica semantics, DAE lowering, or structural rewrites.

mod emit;

use rumoca_core::ExternalTableData;
use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};
use std::rc::Rc;

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

pub fn compile_expression_scalar_program_block_with_pure_calls(
    rows: &ScalarProgramBlock,
    pure_calls: &CompiledPureCallTable,
) -> Result<CompiledExpressionRows, CompileError> {
    let jit = emit::compile_residual_rows_with_pure_calls(rows.programs(), pure_calls.jit.clone())?;
    Ok(CompiledExpressionRows { jit })
}

pub fn compile_jacobian_scalar_program_block_with_pure_calls(
    rows: &ScalarProgramBlock,
    pure_calls: &CompiledPureCallTable,
) -> Result<CompiledJacobianV, CompileError> {
    let jit = emit::compile_jacobian_rows_with_pure_calls(rows.programs(), pure_calls.jit.clone())?;
    Ok(CompiledJacobianV { jit })
}

pub fn compile_assignment_schedule(
    rows: &[Vec<LinearOp>],
    target_y_indices: &[usize],
) -> Result<CompiledAssignmentSchedule, CompileError> {
    emit::compile_assignment_schedule(rows, target_y_indices)
        .map(|jit| CompiledAssignmentSchedule { jit })
}

pub fn compile_assignment_schedule_with_pure_calls(
    rows: &[Vec<LinearOp>],
    target_y_indices: &[usize],
    pure_calls: &CompiledPureCallTable,
) -> Result<CompiledAssignmentSchedule, CompileError> {
    emit::compile_assignment_schedule_with_pure_calls(
        rows,
        target_y_indices,
        pure_calls.jit.clone(),
    )
    .map(|jit| CompiledAssignmentSchedule { jit })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};
    use std::num::NonZeroU64;

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
    fn compiled_row_invokes_one_native_typed_owner() {
        let span = fixture_span();
        let provenance = span
            .require_provenance("Cranelift typed owner fixture")
            .expect("fixture span is source-backed");
        let integer_domain = rumoca_ir_solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX)
            .expect("full Integer domain");
        let profile = rumoca_ir_solve::SolveArithmeticProfile::construct(
            rumoca_ir_solve::SolveRealFormat::Binary64,
            rumoca_ir_solve::SolveRoundingMode::NearestTiesToEven,
            integer_domain,
        );
        let real = rumoca_ir_solve::SolveValueType::scalar(rumoca_ir_solve::SolveScalarType::real(
            profile,
        ));
        let mut owner_id = None;
        let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
            owner_id = Some(table.add_owner(
                rumoca_ir_solve::SolvePureCallIdentity::issued(
                    NonZeroU64::new(1).expect("nonzero identity"),
                ),
                vec![real.clone(), real.clone()],
                vec![rumoca_ir_solve::SolvePureCallOutput::result(real.clone())],
                span,
                |program, inputs, outputs| {
                    let lhs = program.load(inputs[0], span)?;
                    let rhs = program.load(inputs[1], span)?;
                    let sum = program.binary(
                        rumoca_ir_solve::SolveBinaryOperator::Add,
                        lhs,
                        rhs,
                        span,
                    )?;
                    program.store(outputs[0], sum, span)
                },
            )?);
            Ok(())
        })
        .expect("construct typed owner");
        let owner = table
            .owner(owner_id.expect("owner was issued"))
            .expect("owner resolves");
        let rows = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 4.0 },
                LinearOp::Const { dst: 1, value: 5.0 },
                LinearOp::PureCall {
                    dst_start: 2,
                    input_starts: vec![0, 1].into_boxed_slice(),
                    site: owner.call_site(),
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            provenance,
        )
        .expect("construct row");
        let pure_calls = compile_pure_call_table(&table).expect("compile typed owner table");
        let compiled = compile_expression_scalar_program_block_with_pure_calls(&rows, &pure_calls)
            .expect("compile row with typed owner");
        let mut output = [0.0];
        compiled
            .call(&[], &[], 0.0, &mut output)
            .expect("execute native typed owner");
        assert_eq!(output, [9.0]);
    }

    #[test]
    fn compiled_conditional_projections_share_issued_native_call_storage() {
        use rumoca_ir_solve::{
            SolveArithmeticProfile, SolveBinaryOperator, SolveIntegerDomain, SolvePureCallIdentity,
            SolvePureCallOutput, SolveRealFormat, SolveRoundingMode, SolveScalarType, SolveValue,
            SolveValueType,
        };

        let span = fixture_span();
        let profile = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveRoundingMode::NearestTiesToEven,
            SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
        );
        let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
        let real = SolveValueType::scalar(SolveScalarType::real(profile));
        let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
            let child = table.add_owner(
                SolvePureCallIdentity::issued(NonZeroU64::new(20).unwrap()),
                vec![real.clone()],
                vec![SolvePureCallOutput::result(real.clone())],
                span,
                |builder, inputs, outputs| {
                    let input = builder.load(inputs[0], span)?;
                    let two = builder.constant(SolveValue::real(profile, 2.0), span)?;
                    let doubled =
                        builder.binary(SolveBinaryOperator::Multiply, input, two, span)?;
                    builder.store(outputs[0], doubled, span)
                },
            )?;
            table.add_owner(
                SolvePureCallIdentity::issued(NonZeroU64::new(21).unwrap()),
                vec![boolean.clone(), real.clone()],
                vec![SolvePureCallOutput::result(real.clone())],
                span,
                |builder, inputs, outputs| {
                    let condition = builder.load(inputs[0], span)?;
                    let input = builder.load(inputs[1], span)?;
                    let mut selected_call = || {
                        builder.conditional(
                            condition,
                            &[input],
                            vec![real.clone()],
                            span,
                            |region, inputs, outputs| {
                                let input = region.load(inputs[0], span)?;
                                let values = region.call(child, &[input], span)?;
                                region.store(outputs[0], values[0], span)
                            },
                            |region, inputs, outputs| {
                                let input = region.load(inputs[0], span)?;
                                region.store(outputs[0], input, span)
                            },
                        )
                    };
                    let first = selected_call()?;
                    let second = selected_call()?;
                    let sum =
                        builder.binary(SolveBinaryOperator::Add, first[0], second[0], span)?;
                    builder.store(outputs[0], sum, span)
                },
            )?;
            Ok(())
        })
        .unwrap();
        let owner = &table.owners()[1];
        let rows = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::Const { dst: 1, value: 3.0 },
                LinearOp::PureCall {
                    dst_start: 2,
                    input_starts: Box::new([0, 1]),
                    site: owner.call_site(),
                },
                LinearOp::StoreOutput { src: 2 },
            ]],
            span.require_provenance("conditional native invocation fixture")
                .unwrap(),
        )
        .unwrap();
        let pure_calls = compile_pure_call_table(&table).unwrap();
        let compiled =
            compile_expression_scalar_program_block_with_pure_calls(&rows, &pure_calls).unwrap();
        let mut output = [0.0];

        compiled.call(&[], &[], 0.0, &mut output).unwrap();

        assert_eq!(output, [12.0]);
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
    fn compiled_assignment_schedule_invalidates_loads_across_tensor_commits() {
        let rows = vec![
            vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::TensorLoad {
                    dst_start: 0,
                    input: rumoca_ir_solve::TensorInputKind::P,
                    input_start: 0,
                    count: 1,
                    seed_start: None,
                    lanes: 1,
                },
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
        let compiled = compile_assignment_schedule(&rows, &[2, 1, 0]).expect("compile schedule");
        let mut y = [0.0, 0.0, 0.0];

        compiled.call(&mut y, &[3.0], 0.0).expect("run schedule");

        assert_eq!(y, [6.0, 3.0, 0.0]);
    }

    #[test]
    fn compiled_assignment_schedule_commits_shared_program_outputs_together() {
        let rows = vec![vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::LoadP { dst: 1, index: 1 },
            LinearOp::StoreOutput { src: 1 },
        ]];
        let compiled = compile_assignment_schedule(&rows, &[1, 0]).expect("compile schedule");
        let mut y = [0.0, 0.0];

        compiled
            .call(&mut y, &[3.0, 5.0], 0.0)
            .expect("run schedule");

        assert_eq!(y, [5.0, 3.0]);
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
