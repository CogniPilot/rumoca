use super::*;
use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};
use std::num::NonZeroU64;

struct MatrixExecutionCase {
    lhs_dimensions: Vec<u32>,
    rhs_dimensions: Vec<u32>,
    result_dimensions: Vec<u32>,
    lhs: Vec<f64>,
    rhs: Vec<f64>,
    expected: Vec<f64>,
}

struct MatrixArithmeticCase {
    lhs: Vec<u64>,
    rhs: Vec<u64>,
    expected: MatrixArithmeticExpected,
}

#[derive(Clone, Copy)]
enum MatrixArithmeticExpected {
    Exact(u64),
    QuietNan,
}

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("exec_cranelift_source_53.mo"),
        0,
        1,
    )
}

fn assignment_block(rows: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        rows,
        fixture_span()
            .require_provenance("Cranelift assignment fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture assignment programs are checked")
}

fn add_matrix_product_owner(
    table: &mut rumoca_ir_solve::SolvePureCallTableBuilder,
    identity: u64,
    lhs_type: rumoca_ir_solve::SolveValueType,
    rhs_type: rumoca_ir_solve::SolveValueType,
    result_type: rumoca_ir_solve::SolveValueType,
    span: rumoca_core::Span,
) -> Result<rumoca_ir_solve::SolvePureCallOwnerId, rumoca_ir_solve::SolveProgramConstructionError> {
    table.add_owner(
        rumoca_ir_solve::SolvePureCallIdentity::issued(NonZeroU64::new(identity).unwrap()),
        vec![lhs_type, rhs_type],
        vec![rumoca_ir_solve::SolvePureCallOutput::result(result_type)],
        span,
        |program, inputs, outputs| {
            let lhs = program.load(inputs[0], span)?;
            let rhs = program.load(inputs[1], span)?;
            let product = program.matrix_multiply(lhs, rhs, span)?;
            program.store(outputs[0], product, span)
        },
    )
}

fn real_value_type(
    scalar: rumoca_ir_solve::SolveScalarType,
    dimensions: &[u32],
) -> rumoca_ir_solve::SolveValueType {
    if dimensions.is_empty() {
        rumoca_ir_solve::SolveValueType::scalar(scalar)
    } else {
        rumoca_ir_solve::SolveValueType::tensor(scalar, dimensions.to_vec()).unwrap()
    }
}

fn real_cell(format: rumoca_ir_solve::SolveRealFormat, value: f64) -> u64 {
    match format {
        rumoca_ir_solve::SolveRealFormat::Binary32 => u64::from((value as f32).to_bits()),
        rumoca_ir_solve::SolveRealFormat::Binary64 => value.to_bits(),
    }
}

#[test]
fn binary32_integer_conversion_rounds_directly_without_binary64_double_rounding() {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveConversionOperator, SolveIntegerDomain, SolvePureCallIdentity,
        SolvePureCallOutput, SolveRealFormat, SolveScalarType, SolveValue, SolveValueKind,
        SolveValueType,
    };

    const ABOVE_BINARY32_MIDPOINT: i64 = (1_i64 << 62) + (1_i64 << 38) + 1;
    const EXPECTED_DIRECT_BITS: u32 = 0x5e80_0001;

    let span = fixture_span();
    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let real = SolveValueType::scalar(SolveScalarType::real(profile));
    let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(51).unwrap()),
            Vec::new(),
            vec![SolvePureCallOutput::result(real.clone())],
            span,
            |program, _inputs, outputs| {
                let integer = program.constant(
                    SolveValue::integer(profile, ABOVE_BINARY32_MIDPOINT)
                        .expect("fixture belongs to the retained Integer domain"),
                    span,
                )?;
                let converted =
                    program.convert(SolveConversionOperator::IntegerToReal, integer, span)?;
                program.store(outputs[0], converted, span)
            },
        )?;
        Ok(())
    })
    .unwrap();

    let reference = rumoca_eval_solve::eval_pure_call(&table, table.owners()[0].id(), &[]).unwrap();
    assert_eq!(
        reference[0].elements(),
        [SolveValueKind::Real32(EXPECTED_DIRECT_BITS)]
    );

    let compiled = compile_pure_call_table(&table).unwrap();
    let mut native = [0.0];
    compiled
        .call_scalar_payload(
            &table.owners()[0].call_site(),
            &[],
            &mut native,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    assert_eq!((native[0] as f32).to_bits(), EXPECTED_DIRECT_BITS);
}

fn assert_integer_sign_case(
    identity: u64,
    minimum: i64,
    maximum: i64,
    operand: i64,
    expected: i64,
) {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolvePureCallIdentity, SolvePureCallOutput,
        SolveRealFormat, SolveScalarType, SolveUnaryOperator, SolveValue, SolveValueKind,
        SolveValueType,
    };

    let span = fixture_span();
    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::construct(minimum, maximum).unwrap(),
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let integer = SolveValueType::scalar(SolveScalarType::integer(profile));
    let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(identity).unwrap()),
            Vec::new(),
            vec![SolvePureCallOutput::result(integer.clone())],
            span,
            |program, _inputs, outputs| {
                let operand = program.constant(
                    SolveValue::integer(profile, operand)
                        .expect("fixture belongs to the retained domain"),
                    span,
                )?;
                let result = program.unary(SolveUnaryOperator::Sign, operand, span)?;
                program.store(outputs[0], result, span)
            },
        )?;
        Ok(())
    })
    .unwrap();

    let reference = rumoca_eval_solve::eval_pure_call(&table, table.owners()[0].id(), &[]).unwrap();
    assert_eq!(reference[0].elements(), [SolveValueKind::Integer(expected)]);

    let compiled = compile_pure_call_table(&table).unwrap();
    let mut native = [0.0];
    compiled
        .call_scalar_payload(
            &table.owners()[0].call_site(),
            &[],
            &mut native,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    assert_eq!(native, [expected as f64]);
}

#[test]
fn integer_sign_closed_domains_match_evaluator_and_cranelift() {
    for (identity, minimum, maximum, operand, expected) in
        [(52, 1, 10, 7, 1), (53, -10, -1, -7, -1), (54, 0, 0, 0, 0)]
    {
        assert_integer_sign_case(identity, minimum, maximum, operand, expected);
    }
}

#[test]
fn singleton_integer_tensor_literals_trust_construction_on_both_backends() {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolvePureCallIdentity, SolvePureCallOutput,
        SolveRealFormat, SolveScalarType, SolveValue, SolveValueKind, SolveValueType,
    };

    let span = fixture_span();
    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::construct(1, 10).unwrap(),
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let result_type =
        SolveValueType::tensor(SolveScalarType::integer(profile), vec![1, 1]).unwrap();
    let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(55).unwrap()),
            Vec::new(),
            vec![
                SolvePureCallOutput::result(result_type.clone()),
                SolvePureCallOutput::result(result_type.clone()),
            ],
            span,
            |program, _inputs, outputs| {
                let scalar = program.constant(
                    SolveValue::integer(profile, 7)
                        .expect("fixture belongs to the retained domain"),
                    span,
                )?;
                let vector = program.fill(scalar, vec![1], span)?;
                let diagonal = program.diagonal(vector, span)?;
                let identity = program.identity(SolveScalarType::integer(profile), 1, span)?;
                program.store(outputs[0], diagonal, span)?;
                program.store(outputs[1], identity, span)
            },
        )?;
        Ok(())
    })
    .unwrap();

    let reference = rumoca_eval_solve::eval_pure_call(&table, table.owners()[0].id(), &[]).unwrap();
    assert_eq!(reference[0].elements(), [SolveValueKind::Integer(7)]);
    assert_eq!(reference[1].elements(), [SolveValueKind::Integer(1)]);

    let compiled = compile_pure_call_table(&table).unwrap();
    let mut native = [0.0; 2];
    compiled
        .call_scalar_payload(
            &table.owners()[0].call_site(),
            &[],
            &mut native,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    assert_eq!(native, [7.0, 1.0]);
}

fn matrix_arithmetic_cases(format: rumoca_ir_solve::SolveRealFormat) -> Vec<MatrixArithmeticCase> {
    use rumoca_ir_solve::SolveRealFormat;

    let (quiet_nan, signaling_nan, positive_infinity, negative_infinity, smallest, minimum) =
        match format {
            SolveRealFormat::Binary32 => (
                u64::from(0x7fc1_2345_u32),
                u64::from(0x7f81_2345_u32),
                u64::from(f32::INFINITY.to_bits()),
                u64::from(f32::NEG_INFINITY.to_bits()),
                u64::from(1_u32),
                u64::from(f32::MIN_POSITIVE.to_bits()),
            ),
            SolveRealFormat::Binary64 => (
                0x7ff8_0000_0001_2345,
                0x7ff0_0000_0001_2345,
                f64::INFINITY.to_bits(),
                f64::NEG_INFINITY.to_bits(),
                1,
                f64::MIN_POSITIVE.to_bits(),
            ),
        };
    let one = real_cell(format, 1.0);
    let zero = real_cell(format, 0.0);
    let half = real_cell(format, 0.5);
    let plus_epsilon = match format {
        SolveRealFormat::Binary32 => u64::from((1.0f32 + f32::EPSILON).to_bits()),
        SolveRealFormat::Binary64 => (1.0f64 + f64::EPSILON).to_bits(),
    };
    let minus_epsilon = match format {
        SolveRealFormat::Binary32 => u64::from((1.0f32 - f32::EPSILON).to_bits()),
        SolveRealFormat::Binary64 => (1.0f64 - f64::EPSILON).to_bits(),
    };
    vec![
        MatrixArithmeticCase {
            lhs: [1.0e20, -1.0e20, 1.0]
                .map(|value| real_cell(format, value))
                .to_vec(),
            rhs: vec![one; 3],
            expected: MatrixArithmeticExpected::Exact(one),
        },
        MatrixArithmeticCase {
            lhs: vec![real_cell(format, -1.0), plus_epsilon],
            rhs: vec![one, minus_epsilon],
            expected: MatrixArithmeticExpected::Exact(zero),
        },
        MatrixArithmeticCase {
            lhs: vec![quiet_nan],
            rhs: vec![one],
            expected: MatrixArithmeticExpected::QuietNan,
        },
        MatrixArithmeticCase {
            lhs: vec![one],
            rhs: vec![quiet_nan],
            expected: MatrixArithmeticExpected::QuietNan,
        },
        MatrixArithmeticCase {
            lhs: vec![signaling_nan],
            rhs: vec![one],
            expected: MatrixArithmeticExpected::QuietNan,
        },
        MatrixArithmeticCase {
            lhs: vec![zero],
            rhs: vec![positive_infinity],
            expected: MatrixArithmeticExpected::QuietNan,
        },
        MatrixArithmeticCase {
            lhs: vec![positive_infinity, negative_infinity],
            rhs: vec![one, one],
            expected: MatrixArithmeticExpected::QuietNan,
        },
        MatrixArithmeticCase {
            lhs: vec![positive_infinity],
            rhs: vec![one],
            expected: MatrixArithmeticExpected::Exact(positive_infinity),
        },
        MatrixArithmeticCase {
            lhs: vec![negative_infinity],
            rhs: vec![one],
            expected: MatrixArithmeticExpected::Exact(negative_infinity),
        },
        MatrixArithmeticCase {
            lhs: vec![smallest],
            rhs: vec![one],
            expected: MatrixArithmeticExpected::Exact(smallest),
        },
        MatrixArithmeticCase {
            lhs: vec![minimum],
            rhs: vec![half],
            expected: MatrixArithmeticExpected::Exact(minimum >> 1),
        },
    ]
}

fn typed_real_cells(
    format: rumoca_ir_solve::SolveRealFormat,
    value_type: rumoca_ir_solve::SolveValueType,
    cells: &[u64],
) -> rumoca_eval_solve::TypedValue {
    let elements = cells
        .iter()
        .map(|cell| match format {
            rumoca_ir_solve::SolveRealFormat::Binary32 => rumoca_ir_solve::SolveValueKind::Real32(
                u32::try_from(*cell).expect("Binary32 cell fits u32"),
            ),
            rumoca_ir_solve::SolveRealFormat::Binary64 => {
                rumoca_ir_solve::SolveValueKind::Real64(*cell)
            }
        })
        .collect();
    rumoca_eval_solve::TypedValue::construct(value_type, elements).unwrap()
}

fn assert_matrix_arithmetic_output(
    format: rumoca_ir_solve::SolveRealFormat,
    actual: u64,
    expected: MatrixArithmeticExpected,
) {
    match expected {
        MatrixArithmeticExpected::Exact(expected) => assert_eq!(actual, expected),
        MatrixArithmeticExpected::QuietNan => match format {
            rumoca_ir_solve::SolveRealFormat::Binary32 => {
                let bits = u32::try_from(actual).expect("Binary32 result cell fits u32");
                assert_eq!(bits & 0x7f80_0000, 0x7f80_0000);
                assert_ne!(bits & 0x007f_ffff, 0);
                assert_ne!(bits & 0x0040_0000, 0, "signaling NaN was not quieted");
            }
            rumoca_ir_solve::SolveRealFormat::Binary64 => {
                assert_eq!(actual & 0x7ff0_0000_0000_0000, 0x7ff0_0000_0000_0000);
                assert_ne!(actual & 0x000f_ffff_ffff_ffff, 0);
                assert_ne!(
                    actual & 0x0008_0000_0000_0000,
                    0,
                    "signaling NaN was not quieted"
                );
            }
        },
    }
}

fn assert_matrix_arithmetic_case(
    format: rumoca_ir_solve::SolveRealFormat,
    profile: rumoca_ir_solve::SolveArithmeticProfile,
    real: rumoca_ir_solve::SolveScalarType,
    case_index: usize,
    case: MatrixArithmeticCase,
) {
    use rumoca_ir_solve::{SolveValueKind, SolveValueType};

    let vector = SolveValueType::tensor(
        real,
        vec![u32::try_from(case.lhs.len()).expect("small test vector")],
    )
    .unwrap();
    let scalar = SolveValueType::scalar(real);
    let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        add_matrix_product_owner(
            table,
            500 + u64::try_from(case_index).expect("small case index"),
            vector.clone(),
            vector.clone(),
            scalar,
            fixture_span(),
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let reference = rumoca_eval_solve::eval_pure_call(
        &table,
        owner.id(),
        &[
            typed_real_cells(format, vector.clone(), &case.lhs),
            typed_real_cells(format, vector, &case.rhs),
        ],
    )
    .unwrap();
    let reference_cell = match reference[0].elements()[0] {
        SolveValueKind::Real32(bits) => u64::from(bits),
        SolveValueKind::Real64(bits) => bits,
        SolveValueKind::Integer(_) | SolveValueKind::Boolean(_) => {
            unreachable!("matrix product has a checked Real result")
        }
    };
    assert_matrix_arithmetic_output(format, reference_cell, case.expected);

    let compiled = compile_pure_call_table(&table).unwrap();
    let mut native = [0];
    let input = case.lhs.into_iter().chain(case.rhs).collect::<Vec<_>>();
    compiled
        .jit
        .call_cells(&owner.call_site(), &input, &mut native)
        .unwrap();
    assert_matrix_arithmetic_output(format, native[0], case.expected);
}

fn typed_real64(
    value_type: rumoca_ir_solve::SolveValueType,
    values: &[f64],
) -> rumoca_eval_solve::TypedValue {
    rumoca_eval_solve::TypedValue::construct(
        value_type,
        values
            .iter()
            .map(|value| rumoca_ir_solve::SolveValueKind::Real64(value.to_bits()))
            .collect(),
    )
    .unwrap()
}

fn real64_elements(values: &[rumoca_eval_solve::TypedValue]) -> Vec<f64> {
    values
        .iter()
        .flat_map(rumoca_eval_solve::TypedValue::elements)
        .map(|element| match element {
            rumoca_ir_solve::SolveValueKind::Real64(bits) => f64::from_bits(*bits),
            other => panic!("matrix JVP returned non-Binary64 value {other:?}"),
        })
        .collect()
}

fn matrix_execution_cases() -> [MatrixExecutionCase; 4] {
    [
        MatrixExecutionCase {
            lhs_dimensions: vec![2],
            rhs_dimensions: vec![2],
            result_dimensions: vec![],
            lhs: vec![3.0, 4.0],
            rhs: vec![2.0, 11.0],
            expected: vec![50.0],
        },
        MatrixExecutionCase {
            lhs_dimensions: vec![2, 2],
            rhs_dimensions: vec![2],
            result_dimensions: vec![2],
            lhs: vec![1.0, 2.0, 3.0, 4.0],
            rhs: vec![5.0, 6.0],
            expected: vec![17.0, 39.0],
        },
        MatrixExecutionCase {
            lhs_dimensions: vec![2],
            rhs_dimensions: vec![2, 3],
            result_dimensions: vec![3],
            lhs: vec![1.0, 2.0],
            rhs: vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            expected: vec![9.0, 12.0, 15.0],
        },
        MatrixExecutionCase {
            lhs_dimensions: vec![2, 2],
            rhs_dimensions: vec![2, 3],
            result_dimensions: vec![2, 3],
            lhs: vec![1.0, 2.0, 3.0, 4.0],
            rhs: vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0],
            expected: vec![9.0, 12.0, 15.0, 19.0, 26.0, 33.0],
        },
    ]
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
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let real =
        rumoca_ir_solve::SolveValueType::scalar(rumoca_ir_solve::SolveScalarType::real(profile));
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
                let sum =
                    program.binary(rumoca_ir_solve::SolveBinaryOperator::Add, lhs, rhs, span)?;
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
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
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
fn binary32_one_term_negative_zero_contraction_matches_evaluator_and_cranelift() {
    let span = fixture_span();
    for (case, semantics, expected) in [
        (
            53,
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
            (-0.0f32).to_bits(),
        ),
        (
            54,
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
            0.0f32.to_bits(),
        ),
    ] {
        let profile = rumoca_ir_solve::SolveArithmeticProfile::construct(
            rumoca_ir_solve::SolveRealFormat::Binary32,
            rumoca_ir_solve::SolveIntegerDomain::FULL,
            semantics,
        );
        let vector = rumoca_ir_solve::SolveValueType::tensor(
            rumoca_ir_solve::SolveScalarType::real(profile),
            vec![1],
        )
        .unwrap();
        let scalar = rumoca_ir_solve::SolveValueType::scalar(
            rumoca_ir_solve::SolveScalarType::real(profile),
        );
        let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
            add_matrix_product_owner(table, case, vector.clone(), vector.clone(), scalar, span)?;
            Ok(())
        })
        .unwrap();
        let owner = &table.owners()[0];
        assert_eq!(
            owner
                .body()
                .operations()
                .iter()
                .filter(|operation| matches!(
                    operation.operation(),
                    rumoca_ir_solve::SolveOperation::MatrixMultiply { .. }
                ))
                .count(),
            1
        );
        let typed_lhs = rumoca_eval_solve::TypedValue::construct(
            vector.clone(),
            vec![rumoca_ir_solve::SolveValueKind::Real32((-0.0f32).to_bits())],
        )
        .unwrap();
        let typed_rhs = rumoca_eval_solve::TypedValue::construct(
            vector,
            vec![rumoca_ir_solve::SolveValueKind::Real32(1.0f32.to_bits())],
        )
        .unwrap();
        let reference =
            rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[typed_lhs, typed_rhs]).unwrap();
        let compiled = compile_pure_call_table(&table).unwrap();
        let mut native = [0.0];
        compiled
            .call_scalar_payload(
                &owner.call_site(),
                &[-0.0, 1.0],
                &mut native,
                &mut Vec::new(),
                &mut Vec::new(),
            )
            .unwrap();

        assert_eq!(
            reference[0].elements(),
            [rumoca_ir_solve::SolveValueKind::Real32(expected)]
        );
        assert_eq!((native[0] as f32).to_bits(), expected);
    }
}

#[test]
fn zero_output_matrix_product_executes_without_arithmetic() {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType,
        SolveValueKind, SolveValueType,
    };

    let span = fixture_span();
    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let real = SolveScalarType::real(profile);
    let matrix_0x3_output = SolveValueType::tensor(real, vec![0, 3]).unwrap();
    let vector_three = SolveValueType::tensor(real, vec![3]).unwrap();
    let empty_output = SolveValueType::tensor(real, vec![0]).unwrap();
    let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        add_matrix_product_owner(
            table,
            57,
            matrix_0x3_output.clone(),
            vector_three.clone(),
            empty_output,
            span,
        )?;
        Ok(())
    })
    .unwrap();
    let compiled = compile_pure_call_table(&table).unwrap();
    let reference = rumoca_eval_solve::eval_pure_call(
        &table,
        table.owners()[0].id(),
        &[
            rumoca_eval_solve::TypedValue::construct(matrix_0x3_output, vec![]).unwrap(),
            rumoca_eval_solve::TypedValue::construct(
                vector_three,
                vec![SolveValueKind::Real32(1.0f32.to_bits()); 3],
            )
            .unwrap(),
        ],
    )
    .unwrap();
    assert!(reference[0].elements().is_empty());
    compiled
        .call_scalar_payload(
            &table.owners()[0].call_site(),
            &[1.0; 3],
            &mut [],
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
}

fn assert_empty_contraction_profile(
    format: rumoca_ir_solve::SolveRealFormat,
    semantics: rumoca_core::RealMatrixMultiplySemantics,
) {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolveProgramConstructionError, SolveRealFormat,
        SolveScalarType, SolveValueKind,
    };

    let profile = SolveArithmeticProfile::construct(format, SolveIntegerDomain::FULL, semantics);
    let real = SolveScalarType::real(profile);
    let expected = match format {
        SolveRealFormat::Binary32 => SolveValueKind::Real32(0.0f32.to_bits()),
        SolveRealFormat::Binary64 => SolveValueKind::Real64(0.0f64.to_bits()),
    };
    let expected_cell = match expected {
        SolveValueKind::Real32(bits) => u64::from(bits),
        SolveValueKind::Real64(bits) => bits,
        SolveValueKind::Integer(_) | SolveValueKind::Boolean(_) => {
            unreachable!("matrix product has a checked Real result")
        }
    };
    let cases = [
        (vec![0], vec![0], Vec::new(), 1),
        (vec![2, 0], vec![0], vec![2], 2),
        (vec![0], vec![0, 3], vec![3], 3),
        (vec![2, 0], vec![0, 3], vec![2, 3], 6),
        (vec![0, 0], vec![0, 3], vec![0, 3], 0),
    ];
    for (case, (lhs_dimensions, rhs_dimensions, result_dimensions, output_count)) in
        cases.into_iter().enumerate()
    {
        let lhs_type = real_value_type(real, &lhs_dimensions);
        let rhs_type = real_value_type(real, &rhs_dimensions);
        let result_type = real_value_type(real, &result_dimensions);
        let table_result = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
            add_matrix_product_owner(
                table,
                70 + case as u64,
                lhs_type.clone(),
                rhs_type.clone(),
                result_type,
                fixture_span(),
            )?;
            Ok(())
        });
        if semantics
            == rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct
        {
            assert_eq!(
                table_result.expect_err("FirstProduct must refuse every empty inner domain"),
                SolveProgramConstructionError::EmptyFirstProductDomain {
                    provenance: fixture_span()
                }
            );
            continue;
        }
        let table = table_result.expect("PositiveZero constructs every empty-inner layout");
        let reference = rumoca_eval_solve::eval_pure_call(
            &table,
            table.owners()[0].id(),
            &[
                rumoca_eval_solve::TypedValue::construct(lhs_type, Vec::new()).unwrap(),
                rumoca_eval_solve::TypedValue::construct(rhs_type, Vec::new()).unwrap(),
            ],
        )
        .unwrap();
        assert_eq!(reference[0].elements(), vec![expected; output_count]);
        let compiled = compile_pure_call_table(&table).unwrap();
        let mut native = vec![u64::MAX; output_count];
        compiled
            .jit
            .call_cells(&table.owners()[0].call_site(), &[], &mut native)
            .unwrap();
        assert_eq!(native, vec![expected_cell; output_count]);
    }
}

#[test]
fn empty_contraction_zero_is_complete_over_real_formats_and_seed_policies() {
    use rumoca_ir_solve::SolveRealFormat;

    for (format, semantics) in [
        (
            SolveRealFormat::Binary32,
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
        ),
        (
            SolveRealFormat::Binary32,
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
        ),
        (
            SolveRealFormat::Binary64,
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
        ),
        (
            SolveRealFormat::Binary64,
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
        ),
    ] {
        assert_empty_contraction_profile(format, semantics);
    }
}

#[test]
fn empty_boolean_all_is_true_in_both_typed_executors() {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolvePureCallIdentity, SolvePureCallOutput,
        SolveRealFormat, SolveReductionOperator, SolveScalarType, SolveValueKind, SolveValueType,
    };

    let span = fixture_span();
    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let empty = SolveValueType::tensor(SolveScalarType::Boolean, vec![0]).unwrap();
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(58).unwrap()),
            vec![empty.clone()],
            vec![SolvePureCallOutput::result(boolean)],
            span,
            |program, inputs, outputs| {
                let input = program.load(inputs[0], span)?;
                let all = program.reduce(SolveReductionOperator::All, input, span)?;
                program.store(outputs[0], all, span)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let reference = rumoca_eval_solve::eval_pure_call(
        &table,
        owner.id(),
        &[rumoca_eval_solve::TypedValue::construct(empty, vec![]).unwrap()],
    )
    .unwrap();
    assert_eq!(reference[0].elements(), [SolveValueKind::Boolean(true)]);
    let compiled = compile_pure_call_table(&table).unwrap();
    let mut native = [0.0];
    compiled
        .call_scalar_payload(
            &owner.call_site(),
            &[],
            &mut native,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    assert_eq!(native, [1.0]);
}

#[test]
fn matrix_product_order_and_no_contraction_match_in_both_typed_executors() {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType,
        SolveValueKind, SolveValueType,
    };

    let span = fixture_span();
    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let real = SolveScalarType::real(profile);
    let vector_three = SolveValueType::tensor(real, vec![3]).unwrap();
    let vector_two = SolveValueType::tensor(real, vec![2]).unwrap();
    let scalar = SolveValueType::scalar(real);
    let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        for (identity, vector) in [(59, &vector_three), (60, &vector_two)] {
            add_matrix_product_owner(
                table,
                identity,
                vector.clone(),
                vector.clone(),
                scalar.clone(),
                span,
            )?;
        }
        Ok(())
    })
    .unwrap();
    let compiled = compile_pure_call_table(&table).unwrap();
    let fused_discriminator = (1.0f32 + f32::EPSILON).mul_add(1.0 - f32::EPSILON, -1.0);
    assert_eq!(fused_discriminator, -2.0f32.powi(-46));
    assert_ne!(fused_discriminator.to_bits(), 0.0f32.to_bits());
    let cases = [
        (
            vector_three,
            vec![16_777_216.0, 1.0, -16_777_216.0],
            vec![1.0, 1.0, 1.0],
        ),
        (
            vector_two,
            vec![-1.0, f64::from(1.0f32 + f32::EPSILON)],
            vec![1.0, f64::from(1.0f32 - f32::EPSILON)],
        ),
    ];
    for (index, (vector, lhs, rhs)) in cases.into_iter().enumerate() {
        let owner = &table.owners()[index];
        assert_eq!(
            owner
                .body()
                .operations()
                .iter()
                .filter(|operation| matches!(
                    operation.operation(),
                    rumoca_ir_solve::SolveOperation::MatrixMultiply { .. }
                ))
                .count(),
            1
        );
        let typed = |values: &[f64]| {
            rumoca_eval_solve::TypedValue::construct(
                vector.clone(),
                values
                    .iter()
                    .map(|value| SolveValueKind::Real32((*value as f32).to_bits()))
                    .collect(),
            )
            .unwrap()
        };
        let reference =
            rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[typed(&lhs), typed(&rhs)])
                .unwrap();
        assert_eq!(
            reference[0].elements(),
            [SolveValueKind::Real32(0.0f32.to_bits())]
        );
        let arguments = lhs.into_iter().chain(rhs).collect::<Vec<_>>();
        let mut native = [-1.0];
        compiled
            .call_scalar_payload(
                &owner.call_site(),
                &arguments,
                &mut native,
                &mut Vec::new(),
                &mut Vec::new(),
            )
            .unwrap();
        assert_eq!((native[0] as f32).to_bits(), 0.0f32.to_bits());
    }
}

#[test]
fn matrix_product_special_value_and_rounding_contract_matches_both_typed_executors() {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType,
    };

    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let profile = SolveArithmeticProfile::construct(
            format,
            SolveIntegerDomain::FULL,
            rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
        );
        let real = SolveScalarType::real(profile);
        for (case_index, case) in matrix_arithmetic_cases(format).into_iter().enumerate() {
            assert_matrix_arithmetic_case(format, profile, real, case_index, case);
        }
    }
}

fn assert_nonempty_matrix_layouts(format: rumoca_ir_solve::SolveRealFormat) {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType,
        SolveValueKind,
    };

    let cases = matrix_execution_cases();
    let profile = SolveArithmeticProfile::construct(
        format,
        SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let real = SolveScalarType::real(profile);
    let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        for (index, case) in cases.iter().enumerate() {
            add_matrix_product_owner(
                table,
                61 + index as u64,
                real_value_type(real, &case.lhs_dimensions),
                real_value_type(real, &case.rhs_dimensions),
                real_value_type(real, &case.result_dimensions),
                fixture_span(),
            )?;
        }
        Ok(())
    })
    .unwrap();
    let compiled = compile_pure_call_table(&table).unwrap();
    let kind = |value: f64| match format {
        SolveRealFormat::Binary32 => SolveValueKind::Real32((value as f32).to_bits()),
        SolveRealFormat::Binary64 => SolveValueKind::Real64(value.to_bits()),
    };

    for (owner, case) in table.owners().iter().zip(&cases) {
        let reference = rumoca_eval_solve::eval_pure_call(
            &table,
            owner.id(),
            &[
                rumoca_eval_solve::TypedValue::construct(
                    real_value_type(real, &case.lhs_dimensions),
                    case.lhs.iter().copied().map(kind).collect(),
                )
                .unwrap(),
                rumoca_eval_solve::TypedValue::construct(
                    real_value_type(real, &case.rhs_dimensions),
                    case.rhs.iter().copied().map(kind).collect(),
                )
                .unwrap(),
            ],
        )
        .unwrap();
        let expected = case.expected.iter().copied().map(kind).collect::<Vec<_>>();
        assert_eq!(reference[0].elements(), expected);

        let arguments = case
            .lhs
            .iter()
            .chain(&case.rhs)
            .copied()
            .collect::<Vec<_>>();
        let mut native = vec![f64::NAN; case.expected.len()];
        compiled
            .call_scalar_payload(
                &owner.call_site(),
                &arguments,
                &mut native,
                &mut Vec::new(),
                &mut Vec::new(),
            )
            .unwrap();
        assert_eq!(native.into_iter().map(kind).collect::<Vec<_>>(), expected);
    }
}

#[test]
fn every_nonempty_matrix_layout_and_real_format_matches_the_reference_executor() {
    for format in [
        rumoca_ir_solve::SolveRealFormat::Binary32,
        rumoca_ir_solve::SolveRealFormat::Binary64,
    ] {
        assert_nonempty_matrix_layouts(format);
    }
}

#[test]
fn boolean_tensor_stays_one_typed_dag_value_through_both_executors() {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolvePureCallIdentity, SolvePureCallOutput,
        SolveRealFormat, SolveScalarType, SolveValueKind, SolveValueType,
    };

    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let matrix = SolveValueType::tensor(SolveScalarType::Boolean, vec![2, 2]).unwrap();
    let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(900).unwrap()),
            vec![matrix.clone()],
            vec![SolvePureCallOutput::result(matrix.clone())],
            fixture_span(),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], fixture_span())?;
                let transposed = builder.transpose(input, fixture_span())?;
                builder.store(outputs[0], transposed, fixture_span())
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    assert_eq!(owner.body().operations().len(), 3);
    assert!(matches!(
        owner.body().operations()[1].operation(),
        rumoca_ir_solve::SolveOperation::Transpose { .. }
    ));
    let input = rumoca_eval_solve::TypedValue::construct(
        matrix,
        [true, false, true, false]
            .map(SolveValueKind::Boolean)
            .to_vec(),
    )
    .unwrap();
    let reference = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[input]).unwrap();
    assert_eq!(
        reference[0].elements(),
        [true, true, false, false].map(SolveValueKind::Boolean)
    );
    let compiled = compile_pure_call_table(&table).unwrap();
    let mut native = [u64::MAX; 4];
    compiled
        .jit
        .call_cells(&owner.call_site(), &[1, 0, 1, 0], &mut native)
        .unwrap();
    assert_eq!(native, [1, 1, 0, 0]);
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
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let real =
        rumoca_ir_solve::SolveValueType::scalar(rumoca_ir_solve::SolveScalarType::real(profile));
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
fn compiled_matrix_product_directional_owner_matches_reference_jvp() {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType,
        SolveValueType,
    };

    let span = fixture_span();
    let provenance = span
        .require_provenance("Cranelift matrix directional fixture")
        .unwrap();
    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let real = SolveScalarType::real(profile);
    let matrix = SolveValueType::tensor(real, vec![2, 2]).unwrap();
    let vector = SolveValueType::tensor(real, vec![2]).unwrap();
    let result = SolveValueType::tensor(real, vec![2]).unwrap();
    let table = rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        add_matrix_product_owner(table, 65, matrix.clone(), vector.clone(), result, span)?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let site = owner
        .call_site()
        .directional()
        .expect("matrix product has a checked directional relation")
        .clone();
    let matrix_primal = [1.0, 2.0, 3.0, 4.0];
    let matrix_tangent = [1.0; 4];
    let vector_primal = [5.0, 6.0];
    let vector_tangent = [2.0, 3.0];
    let values = matrix_primal
        .into_iter()
        .chain(matrix_tangent)
        .chain(vector_primal)
        .chain(vector_tangent)
        .collect::<Vec<_>>();
    let mut operations = values
        .iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value: *value,
        })
        .collect::<Vec<_>>();
    operations.push(LinearOp::PureCallDirectional {
        dst_start: 12,
        input_starts: Box::new([0, 4, 8, 10]),
        site,
    });
    for src in 12..16 {
        operations.push(LinearOp::StoreOutput { src });
    }
    let rows = ScalarProgramBlock::with_source_span(vec![operations], provenance).unwrap();
    let pure_calls = compile_pure_call_table(&table).unwrap();
    let compiled =
        compile_expression_scalar_program_block_with_pure_calls(&rows, &pure_calls).unwrap();
    let mut native = [0.0; 4];
    compiled.call(&[], &[], 0.0, &mut native).unwrap();

    let reference_values = rumoca_eval_solve::eval_pure_call_directional(
        &table,
        owner.id(),
        &[
            typed_real64(matrix.clone(), &matrix_primal),
            typed_real64(matrix, &matrix_tangent),
            typed_real64(vector.clone(), &vector_primal),
            typed_real64(vector, &vector_tangent),
        ],
    )
    .unwrap();
    let reference = real64_elements(&reference_values);

    assert_eq!(reference, [17.0, 39.0, 19.0, 29.0]);
    assert_eq!(native, reference.as_slice());
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
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let real =
        rumoca_ir_solve::SolveValueType::scalar(rumoca_ir_solve::SolveScalarType::real(profile));
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
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
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
                let doubled = builder.binary(SolveBinaryOperator::Multiply, input, two, span)?;
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
                let first = selected_native_call(builder, condition, input, &real, child, span)?;
                let second = selected_native_call(builder, condition, input, &real, child, span)?;
                let sum = builder.binary(SolveBinaryOperator::Add, first[0], second[0], span)?;
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
fn retired_table_operation_cannot_form_a_mixed_pure_call_block() {
    let linear_op = include_str!("../../rumoca-ir-solve/src/linear_op.rs");
    assert!(linear_op.contains("PureCall"));
    for suffix in ["Bounds", "Lookup", "LookupSlope", "NextEvent"] {
        let retired_operation = format!("{}{}", "Table", suffix);
        assert!(!linear_op.contains(&retired_operation));
    }
}

#[test]
fn compiled_assignment_schedule_preserves_ordered_y_dependencies() {
    let rows = assignment_block(vec![
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
    ]);
    let compiled = compile_assignment_fixture(&rows, &[1, 0]).expect("compile schedule");
    let mut y = [0.0, 0.0];

    compiled.call(&mut y, &[3.0], 0.0).expect("run schedule");

    assert_eq!(y, [6.0, 3.0]);
}

fn exact_owner_schedule_source() -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
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
    .expect("fixture source is computable")
}

fn exact_owner_schedule_system() -> rumoca_ir_solve::ContinuousSolveSystem {
    let source = exact_owner_schedule_source();
    let row = |owner, source, equation, target, expr_reg, expr_eval_len| {
        rumoca_ir_solve::AlgebraicRefreshRow::checked(rumoca_ir_solve::AlgebraicRefreshRowDraft {
            owner_id: rumoca_ir_solve::RefreshRowOwnerId::checked(owner).unwrap(),
            source: rumoca_ir_solve::RefreshScalarProgramSource::checked(0, source).unwrap(),
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
        })
        .unwrap()
    };
    let first = row(1, 0, 0, 1, 1, 2);
    let second = row(0, 1, 1, 0, 3, 4);
    let plan = rumoca_ir_solve::RefreshPlan {
        rows: vec![first, second],
        causal_seed_rows: rumoca_ir_solve::RefreshRowSelection::checked(2, [0, 1]).unwrap(),
        ..rumoca_ir_solve::RefreshPlan::empty()
    };
    let source = rumoca_ir_solve::ComputeBlock::from_scalar_program_block(source);
    let solve_layout = rumoca_ir_solve::SolveLayout {
        solver_maps: rumoca_ir_solve::SolverNameIndexMaps {
            names: vec!["y0".to_string(), "y1".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 2,
        ..Default::default()
    };
    rumoca_ir_solve::ContinuousSolveSystem::construct(
        &solve_layout,
        rumoca_ir_solve::ContinuousSolveSystemInputs::new(
            source.clone(),
            vec![
                Some(rumoca_ir_solve::scalar_slot_y(1)),
                Some(rumoca_ir_solve::scalar_slot_y(0)),
            ],
            rumoca_ir_solve::AlgebraicProjectionPlan {
                blocks: vec![
                    rumoca_ir_solve::AlgebraicProjectionBlock {
                        rows: vec![0],
                        y_indices: vec![1],
                        tearing: None,
                    },
                    rumoca_ir_solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![0],
                        tearing: None,
                    },
                ],
            },
            source,
            (
                rumoca_ir_solve::ComputeBlock::default(),
                rumoca_ir_solve::AlgebraicProjectionPlan::default(),
            ),
            rumoca_ir_solve::ComputeBlock::default(),
            rumoca_ir_solve::ContinuousRefreshPlanInputs::new(
                plan,
                rumoca_ir_solve::RefreshPlan::empty(),
                rumoca_ir_solve::RefreshPlan::empty(),
                rumoca_ir_solve::RefreshPlan::empty(),
                Vec::new(),
            ),
        ),
    )
    .expect("refresh owners should construct against their exact continuous source")
}

#[test]
fn compiled_exact_owner_schedule_preserves_construction_order() {
    let system = exact_owner_schedule_system();
    let owners = system.refresh_owners();
    let schedule = owners
        .exact_assignment_schedule(owners.algebraic().static_causal_sequence())
        .expect("construction should freeze the exact order");
    let programs = schedule
        .program_ids()
        .iter()
        .map(|id| {
            owners
                .exact_assignment_program(*id)
                .and_then(|owner| owner.final_program().program(0))
                .expect("constructed owner retains its final program")
                .to_vec()
        })
        .collect();
    let targets = schedule
        .program_ids()
        .iter()
        .flat_map(|id| {
            owners
                .exact_assignment_program(*id)
                .expect("schedule refers to its constructed owner")
                .target_indices()
                .iter()
                .copied()
        })
        .collect::<Vec<_>>();
    let compiled = compile_assignment_fixture(&assignment_block(programs), &targets)
        .expect("compile constructed owner schedule fixture");
    let mut y = [0.0, 0.0];

    compiled.call(&mut y, &[], 0.0).expect("run schedule");

    assert_eq!(y, [5.0, 3.0]);
}

#[test]
fn compiled_assignment_schedule_invalidates_loads_across_tensor_commits() {
    let rows = assignment_block(vec![
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
    ]);
    let compiled = compile_assignment_fixture(&rows, &[2, 1, 0]).expect("compile schedule");
    let mut y = [0.0, 0.0, 0.0];

    compiled.call(&mut y, &[3.0], 0.0).expect("run schedule");

    assert_eq!(y, [6.0, 3.0, 0.0]);
}

#[test]
fn compiled_assignment_schedule_commits_shared_program_outputs_together() {
    let rows = assignment_block(vec![vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::LoadP { dst: 1, index: 1 },
        LinearOp::StoreOutput { src: 1 },
    ]]);
    let compiled = compile_assignment_fixture(&rows, &[1, 0]).expect("compile schedule");
    let mut y = [0.0, 0.0];

    compiled
        .call(&mut y, &[3.0, 5.0], 0.0)
        .expect("run schedule");

    assert_eq!(y, [5.0, 3.0]);
}

#[test]
fn compiled_assignment_schedule_uses_retained_block_output_width() {
    let rows = assignment_block(vec![vec![
        LinearOp::Const { dst: 0, value: 3.0 },
        LinearOp::Const { dst: 1, value: 5.0 },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 2,
            stride: 1,
        },
    ]]);

    let error = compile_assignment_fixture(&rows, &[0])
        .err()
        .expect("the retained checked output width cannot be replaced by target count");
    assert!(
        matches!(error, CompileError::Input(message) if message.contains("2 outputs but 1 targets"))
    );
}

#[test]
fn compiled_assignment_schedule_prevalidates_before_mutating_y() {
    let rows = assignment_block(vec![vec![
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ]]);
    let compiled = compile_assignment_fixture(&rows, &[0]).expect("compile schedule");
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
