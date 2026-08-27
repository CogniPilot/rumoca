//! Cranelift compiled execution adapter for Solve-IR row blocks.
//!
//! This crate compiles already-lowered Solve-IR through Cranelift and exposes
//! callable residual/JVP functions. It does not use Jinja templates because
//! Cranelift is a programmatic JIT API rather than a textual-code target, and
//! it must not own Modelica semantics, DAE lowering, or structural rewrites.

mod emit;

use rumoca_core::ExternalTableData;
use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};
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
        self.call_with_external_tables(y, p, t, v, &[], out)
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
        call_with_output_placement(self.output_placement.as_ref(), out, |dense| {
            self.jit
                .call_with_external_tables(y, p, t, v, external_tables, dense)
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
            required_output_len: indices
                .iter()
                .copied()
                .max()
                .map_or(0, |index| index.saturating_add(1)),
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
        self.call_with_external_tables(y, p, t, &[], out)
    }

    pub fn call_with_external_tables(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        call_with_output_placement(self.output_placement.as_ref(), out, |dense| {
            self.jit
                .call_with_external_tables(y, p, t, external_tables, dense)
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
    let jit = emit::compile_jacobian_rows(rows.programs())?;
    Ok(CompiledJacobianV {
        jit,
        output_placement: OutputPlacement::for_block(rows),
    })
}

pub fn compile_expression_scalar_program_block(
    rows: &ScalarProgramBlock,
) -> Result<CompiledExpressionRows, CompileError> {
    let jit = emit::compile_residual_rows(rows.programs())?;
    Ok(CompiledExpressionRows {
        jit,
        output_placement: OutputPlacement::for_block(rows),
    })
}

pub fn compile_expression_scalar_program_block_with_pure_calls(
    rows: &ScalarProgramBlock,
    pure_calls: &CompiledPureCallTable,
) -> Result<CompiledExpressionRows, CompileError> {
    let jit = emit::compile_residual_rows_with_pure_calls(rows.programs(), pure_calls.jit.clone())?;
    Ok(CompiledExpressionRows {
        jit,
        output_placement: OutputPlacement::for_block(rows),
    })
}

pub fn compile_jacobian_scalar_program_block_with_pure_calls(
    rows: &ScalarProgramBlock,
    pure_calls: &CompiledPureCallTable,
) -> Result<CompiledJacobianV, CompileError> {
    let jit = emit::compile_jacobian_rows_with_pure_calls(rows.programs(), pure_calls.jit.clone())?;
    Ok(CompiledJacobianV {
        jit,
        output_placement: OutputPlacement::for_block(rows),
    })
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

pub fn compile_exact_assignment_schedule(
    source: &rumoca_ir_solve::ComputeBlock,
    owners: &rumoca_ir_solve::ContinuousRefreshOwners,
    schedule: &rumoca_ir_solve::ExactRefreshAssignmentSchedule,
) -> Result<CompiledAssignmentSchedule, CompileError> {
    emit::compile_exact_assignment_schedule(source, owners, schedule, None)
        .map(|jit| CompiledAssignmentSchedule { jit })
}

pub fn compile_exact_assignment_schedule_with_pure_calls(
    source: &rumoca_ir_solve::ComputeBlock,
    owners: &rumoca_ir_solve::ContinuousRefreshOwners,
    schedule: &rumoca_ir_solve::ExactRefreshAssignmentSchedule,
    pure_calls: &CompiledPureCallTable,
) -> Result<CompiledAssignmentSchedule, CompileError> {
    emit::compile_exact_assignment_schedule(source, owners, schedule, Some(pure_calls.jit.clone()))
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

    fn selected_native_call<'program>(
        builder: &mut rumoca_ir_solve::TypedProgramBuilder<'program>,
        condition: rumoca_ir_solve::ProgramRegister<'program>,
        input: rumoca_ir_solve::ProgramRegister<'program>,
        output_type: &rumoca_ir_solve::SolveValueType,
        child: rumoca_ir_solve::SolvePureCallOwnerId,
        span: rumoca_core::Span,
    ) -> Result<
        Vec<rumoca_ir_solve::ProgramRegister<'program>>,
        rumoca_ir_solve::SolveProgramConstructionError,
    > {
        builder.conditional(
            condition,
            &[input],
            vec![output_type.clone()],
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
    fn compiled_scalar_blocks_preserve_sparse_output_indices() {
        let span = fixture_span();
        let expressions = ScalarProgramBlock::with_output_indices(
            vec![
                vec![
                    LinearOp::Const { dst: 0, value: 2.0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    LinearOp::Const { dst: 0, value: 4.0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
            ],
            vec![span, span],
            vec![1, 3],
        )
        .expect("construct sparse expression rows");
        let compiled = compile_expression_scalar_program_block(&expressions)
            .expect("compile sparse expression rows");
        let mut expression_output = [-1.0; 4];
        compiled
            .call(&[], &[], 0.0, &mut expression_output)
            .expect("execute sparse expression rows");
        assert_eq!(expression_output, [-1.0, 2.0, -1.0, 4.0]);

        let jacobian = ScalarProgramBlock::with_output_indices(
            vec![
                vec![
                    LinearOp::LoadSeed { dst: 0, index: 0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    LinearOp::LoadSeed { dst: 0, index: 1 },
                    LinearOp::StoreOutput { src: 0 },
                ],
            ],
            vec![span, span],
            vec![1, 3],
        )
        .expect("construct sparse Jacobian rows");
        let compiled =
            compile_jacobian_scalar_program_block(&jacobian).expect("compile sparse Jacobian");
        let mut jacobian_output = [-1.0; 4];
        compiled
            .call(&[], &[], 0.0, &[7.0, 9.0], &mut jacobian_output)
            .expect("execute sparse Jacobian");
        assert_eq!(jacobian_output, [-1.0, 7.0, -1.0, 9.0]);
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
    fn compiled_typed_owner_invokes_one_aggregate_transaction_payload() {
        let span = fixture_span();
        let integer_domain = rumoca_ir_solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX)
            .expect("full Integer domain");
        let profile = rumoca_ir_solve::SolveArithmeticProfile::construct(
            rumoca_ir_solve::SolveRealFormat::Binary64,
            integer_domain,
        );
        let tensor = rumoca_ir_solve::SolveValueType::tensor(
            rumoca_ir_solve::SolveScalarType::real(profile),
            vec![2],
        )
        .unwrap();
        let boolean =
            rumoca_ir_solve::SolveValueType::scalar(rumoca_ir_solve::SolveScalarType::Boolean);
        let mut owner_id = None;
        let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
            owner_id = Some(table.add_owner(
                rumoca_ir_solve::SolvePureCallIdentity::issued(NonZeroU64::new(2).unwrap()),
                vec![tensor.clone(), boolean.clone()],
                vec![
                    rumoca_ir_solve::SolvePureCallOutput::result(tensor.clone()),
                    rumoca_ir_solve::SolvePureCallOutput::assertion_predicate(),
                ],
                span,
                |program, inputs, outputs| {
                    let tensor = program.load(inputs[0], span)?;
                    let predicate = program.load(inputs[1], span)?;
                    program.store(outputs[0], tensor, span)?;
                    program.store(outputs[1], predicate, span)
                },
            )?);
            Ok(())
        })
        .unwrap();
        let site = table
            .owner(owner_id.unwrap())
            .expect("owner resolves")
            .call_site();
        let compiled = compile_pure_call_table(&table).unwrap();
        let mut output = [0.0; 3];
        let mut input_cells = Vec::new();
        let mut output_cells = Vec::new();

        compiled
            .call_scalar_payload(
                &site,
                &[1.25, -2.5, 1.0],
                &mut output,
                &mut input_cells,
                &mut output_cells,
            )
            .unwrap();

        assert_eq!(output, [1.25, -2.5, 1.0]);
        assert_eq!(input_cells.len(), 3);
        assert_eq!(output_cells.len(), 3);
    }

    #[test]
    fn compiled_directional_owner_executes_checked_typed_jvp() {
        let span = fixture_span();
        let provenance = span
            .require_provenance("Cranelift directional owner fixture")
            .expect("fixture span is source-backed");
        let profile = rumoca_ir_solve::SolveArithmeticProfile::construct(
            rumoca_ir_solve::SolveRealFormat::Binary64,
            rumoca_ir_solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
        );
        let real = rumoca_ir_solve::SolveValueType::scalar(rumoca_ir_solve::SolveScalarType::real(
            profile,
        ));
        let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
            table.add_owner(
                rumoca_ir_solve::SolvePureCallIdentity::issued(NonZeroU64::new(3).unwrap()),
                vec![real.clone()],
                vec![rumoca_ir_solve::SolvePureCallOutput::result(real.clone())],
                span,
                |builder, inputs, outputs| {
                    let input = builder.load(inputs[0], span)?;
                    let square = builder.binary(
                        rumoca_ir_solve::SolveBinaryOperator::Multiply,
                        input,
                        input,
                        span,
                    )?;
                    builder.store(outputs[0], square, span)
                },
            )?;
            Ok(())
        })
        .unwrap();
        let site = table.owners()[0]
            .call_site()
            .directional()
            .expect("square owner has a directional relation")
            .clone();
        let rows = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 3.0 },
                LinearOp::Const { dst: 1, value: 1.0 },
                LinearOp::PureCallDirectional {
                    dst_start: 2,
                    input_starts: Box::new([0, 1]),
                    site,
                },
                LinearOp::StoreOutput { src: 3 },
            ]],
            provenance,
        )
        .unwrap();
        let pure_calls = compile_pure_call_table(&table).unwrap();
        let compiled =
            compile_expression_scalar_program_block_with_pure_calls(&rows, &pure_calls).unwrap();
        let mut output = [0.0];

        compiled.call(&[], &[], 0.0, &mut output).unwrap();

        assert_eq!(output, [6.0]);
    }

    #[test]
    fn repeated_directional_owner_calls_keep_distinct_inputs() {
        let span = fixture_span();
        let provenance = span
            .require_provenance("Cranelift repeated directional call fixture")
            .unwrap();
        let profile = rumoca_ir_solve::SolveArithmeticProfile::construct(
            rumoca_ir_solve::SolveRealFormat::Binary64,
            rumoca_ir_solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
        );
        let real = rumoca_ir_solve::SolveValueType::scalar(rumoca_ir_solve::SolveScalarType::real(
            profile,
        ));
        let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
            table.add_owner(
                rumoca_ir_solve::SolvePureCallIdentity::issued(NonZeroU64::new(4).unwrap()),
                vec![real.clone()],
                vec![rumoca_ir_solve::SolvePureCallOutput::result(real.clone())],
                span,
                |builder, inputs, outputs| {
                    let input = builder.load(inputs[0], span)?;
                    let square = builder.binary(
                        rumoca_ir_solve::SolveBinaryOperator::Multiply,
                        input,
                        input,
                        span,
                    )?;
                    builder.store(outputs[0], square, span)
                },
            )?;
            Ok(())
        })
        .unwrap();
        let site = table.owners()[0]
            .call_site()
            .directional()
            .expect("square owner has a directional relation")
            .clone();
        let rows = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::Const { dst: 0, value: 3.0 },
                LinearOp::Const { dst: 1, value: 1.0 },
                LinearOp::PureCallDirectional {
                    dst_start: 2,
                    input_starts: Box::new([0, 1]),
                    site: site.clone(),
                },
                LinearOp::Const { dst: 4, value: 4.0 },
                LinearOp::Const { dst: 5, value: 2.0 },
                LinearOp::PureCallDirectional {
                    dst_start: 6,
                    input_starts: Box::new([4, 5]),
                    site,
                },
                LinearOp::StoreOutput { src: 3 },
                LinearOp::StoreOutput { src: 7 },
            ]],
            provenance,
        )
        .unwrap();
        let pure_calls = compile_pure_call_table(&table).unwrap();
        let compiled =
            compile_expression_scalar_program_block_with_pure_calls(&rows, &pure_calls).unwrap();
        let mut output = [0.0; 2];

        compiled.call(&[], &[], 0.0, &mut output).unwrap();

        assert_eq!(output, [6.0, 16.0]);
    }

    #[test]
    fn compiled_conditional_projections_share_issued_native_call_storage() {
        use rumoca_ir_solve::{
            SolveArithmeticProfile, SolveBinaryOperator, SolveIntegerDomain, SolvePureCallIdentity,
            SolvePureCallOutput, SolveRealFormat, SolveScalarType, SolveValue, SolveValueType,
        };

        let span = fixture_span();
        let profile = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
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
                    let first =
                        selected_native_call(builder, condition, input, &real, child, span)?;
                    let second =
                        selected_native_call(builder, condition, input, &real, child, span)?;
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
    fn compiled_exact_owner_schedule_preserves_construction_order() {
        let source = ScalarProgramBlock::with_source_span(
            vec![
                vec![
                    LinearOp::LoadY { dst: 0, index: 1 },
                    LinearOp::Const { dst: 1, value: 3.0 },
                    LinearOp::Binary {
                        dst: 2,
                        op: rumoca_ir_solve::BinaryOp::Sub,
                        lhs: 0,
                        rhs: 1,
                    },
                    LinearOp::StoreOutput { src: 2 },
                ],
                vec![
                    LinearOp::LoadY { dst: 0, index: 0 },
                    LinearOp::LoadY { dst: 1, index: 1 },
                    LinearOp::Const { dst: 2, value: 2.0 },
                    LinearOp::Binary {
                        dst: 3,
                        op: rumoca_ir_solve::BinaryOp::Add,
                        lhs: 1,
                        rhs: 2,
                    },
                    LinearOp::Binary {
                        dst: 4,
                        op: rumoca_ir_solve::BinaryOp::Sub,
                        lhs: 0,
                        rhs: 3,
                    },
                    LinearOp::StoreOutput { src: 4 },
                ],
            ],
            fixture_span()
                .require_provenance("exact owner schedule fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture source is computable");
        let row = |owner, source, equation, target, expr_reg, expr_eval_len| {
            rumoca_ir_solve::AlgebraicRefreshRow::checked(
                rumoca_ir_solve::AlgebraicRefreshRowDraft {
                    owner_id: rumoca_ir_solve::RefreshRowOwnerId::checked(owner).unwrap(),
                    source: rumoca_ir_solve::RefreshScalarProgramSource::checked(0, source)
                        .unwrap(),
                    equation_index: equation,
                    output_offset: 0,
                    target_index: target,
                    assignment_target: Some(target),
                    assignment_shape: Some(rumoca_ir_solve::TargetAssignmentShape::Direct {
                        target_y_index: target,
                        expr_reg,
                        target_scale: 1.0,
                        expr_eval_len,
                    }),
                    direct_assignment_certified: true,
                    exact_assignment_certified: true,
                },
            )
            .unwrap()
        };
        let first = row(1, 0, 0, 1, 1, 2);
        let second = row(0, 1, 1, 0, 3, 4);
        let plan = rumoca_ir_solve::RefreshPlan {
            rows: vec![first, second],
            dynamic_causal_seed_rows: rumoca_ir_solve::RefreshRowSelection::checked(2, [0, 1])
                .unwrap(),
            ..Default::default()
        };
        let source = rumoca_ir_solve::ComputeBlock::from_scalar_program_block(source);
        let owners = rumoca_ir_solve::ContinuousRefreshOwners::checked_for_source(
            &source,
            plan,
            Default::default(),
            Default::default(),
            Default::default(),
            Vec::new(),
        )
        .expect("refresh owners should construct");
        let schedule = owners
            .exact_assignment_schedule(owners.algebraic().dynamic_causal_sequence)
            .expect("construction should freeze the exact order");
        let compiled = compile_exact_assignment_schedule(&source, &owners, schedule)
            .expect("compile owner schedule");
        let mut y = [0.0, 0.0];

        compiled.call(&mut y, &[], 0.0).expect("run schedule");

        assert_eq!(y, [5.0, 3.0]);
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
