//! Expression-form lowering into register programs.
//!
//! The DAE expression families that need more than one Solve operation each:
//! the static quotient builtins, and a checked function call inlined into the
//! row that uses it.

mod function_calls;

use super::*;

fn eval_residual_rows(
    rows: &rumoca_ir_solve::ScalarProgramBlock,
    y: &[f64],
    p: &[f64],
) -> Vec<f64> {
    let output_count = rows
        .output_indices()
        .iter()
        .copied()
        .max()
        .map_or(0, |index| index + 1);
    let mut output = vec![0.0; output_count];
    rumoca_eval_solve::eval_scalar_program_block(rows, y, p, 0.0, None, &mut output).unwrap();
    output
}

fn eval_residual_rows_with_pure_calls(
    rows: &rumoca_ir_solve::ScalarProgramBlock,
    pure_calls: &rumoca_ir_solve::SolvePureCallTable,
    y: &[f64],
    p: &[f64],
) -> Vec<f64> {
    let output_count = rows
        .output_indices()
        .iter()
        .copied()
        .max()
        .map_or(0, |index| index + 1);
    let mut output = vec![0.0; output_count];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        rows,
        y,
        p,
        0.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(pure_calls),
            ..Default::default()
        },
        &mut output,
    )
    .unwrap();
    output
}

#[test]
fn pure_call_ad_invokes_one_checked_directional_owner() {
    let source = TestSource::new("Real y; y = square(time);");
    let owner = source.at(0, 27);
    let span = owner.span();
    let provenance = span
        .require_provenance("typed directional AD fixture")
        .expect("fixture span is source-backed");
    let arithmetic = rumoca_ir_solve::SolveArithmeticProfile::construct(
        rumoca_ir_solve::SolveRealFormat::Binary64,
        rumoca_ir_solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let real =
        rumoca_ir_solve::SolveValueType::scalar(rumoca_ir_solve::SolveScalarType::real(arithmetic));
    let table = rumoca_ir_solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            rumoca_ir_solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
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
    let primal = vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::PureCall {
            dst_start: 1,
            input_starts: Box::new([0]),
            site: table.owners()[0].call_site(),
        },
        LinearOp::StoreOutput { src: 1 },
    ]];

    let derived = crate::lower_scalar_program_block_ad(&primal)
        .expect("differentiate through the checked typed owner");
    assert_eq!(
        derived[0]
            .iter()
            .filter(|operation| matches!(operation, LinearOp::PureCallDirectional { .. }))
            .count(),
        1
    );
    assert!(!derived[0].iter().any(|operation| matches!(
        operation,
        LinearOp::FunctionFold { .. } | LinearOp::GuardedFunctionFold { .. }
    )));
    let block = rumoca_ir_solve::ScalarProgramBlock::with_source_span(derived, provenance).unwrap();
    let mut output = [0.0];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &block,
        &[3.0],
        &[],
        0.0,
        rumoca_eval_solve::RowEvalContext {
            seed: Some(&[1.0]),
            pure_calls: Some(&table),
            ..Default::default()
        },
        &mut output,
    )
    .unwrap();

    assert_eq!(output, [6.0]);
}

#[test]
fn function_conditional_ad_keeps_primal_predicate_and_dual_result_tuple() {
    let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked(
        1,
        [1],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 1,
                    stride: 1,
                },
            ],
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 1,
                    stride: 1,
                },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 4.0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 1,
                stride: 1,
            },
        ],
    )
    .expect("construct primal conditional");
    let primal = vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::FunctionConditional {
            dst_start: 1,
            capture_start: 0,
            program: std::sync::Arc::new(conditional),
        },
        LinearOp::StoreOutput { src: 1 },
    ]];

    let derived = crate::lower_scalar_program_block_ad(&primal).expect("differentiate conditional");
    let program = derived[0]
        .iter()
        .find_map(|operation| match operation {
            LinearOp::FunctionConditional { program, .. } => Some(program),
            _ => None,
        })
        .expect("derived row keeps one conditional owner");

    assert_eq!(program.capture_count, 2);
    assert_eq!(program.target_widths.as_ref(), &[2]);
    assert_eq!(program.result_count, 2);
    assert_eq!(
        rumoca_ir_solve::ScalarProgramBlock::program_output_count(&program.arms[0].condition),
        1,
        "branch selection is primal-only"
    );
    assert_eq!(
        rumoca_ir_solve::ScalarProgramBlock::program_output_count(&program.arms[0].result),
        2,
        "selected result retains interleaved primal/derivative lanes"
    );
    assert_eq!(
        rumoca_ir_solve::ScalarProgramBlock::program_output_count(&program.fallback),
        2
    );
}

#[test]
fn function_conditional_ad_preserves_one_interleaved_capture_range() {
    let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked(
        2,
        [2],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadFunctionConditionalCaptureRange {
                    dst_start: 0,
                    index_start: 0,
                    count: 2,
                },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 2,
                    stride: 1,
                },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::TensorFill {
                dst_start: 1,
                value_start: 0,
                count: 2,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 1,
                count: 2,
                stride: 1,
            },
        ],
    )
    .expect("construct primal capture-range conditional");
    let primal = vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::FunctionConditional {
            dst_start: 2,
            capture_start: 0,
            program: std::sync::Arc::new(conditional),
        },
        LinearOp::StoreOutputRange {
            start: 2,
            count: 2,
            stride: 1,
        },
    ]];

    let derived = crate::lower_scalar_program_block_ad(&primal).expect("differentiate range ABI");
    let program = derived[0]
        .iter()
        .find_map(|operation| match operation {
            LinearOp::FunctionConditional { program, .. } => Some(program),
            _ => None,
        })
        .expect("derived row keeps one conditional owner");

    assert_eq!(program.capture_count, 4);
    assert_eq!(program.target_widths.as_ref(), &[4]);
    assert!(matches!(
        program.arms[0].result[0],
        LinearOp::LoadFunctionConditionalCaptureRange {
            dst_start: 0,
            index_start: 0,
            count: 4,
        }
    ));
}

#[test]
fn function_conditional_ad_retains_tensor_division_as_one_dual_range() {
    let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked(
        4,
        [3],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadFunctionConditionalCaptureRange {
                    dst_start: 0,
                    index_start: 0,
                    count: 3,
                },
                LinearOp::LoadFunctionConditionalCaptureRange {
                    dst_start: 3,
                    index_start: 3,
                    count: 1,
                },
                LinearOp::TensorBinary {
                    dst_start: 4,
                    op: rumoca_ir_solve::BinaryOp::Div,
                    lhs_start: 0,
                    rhs_start: 3,
                    count: 3,
                    lhs_stride: 1,
                    rhs_stride: 0,
                    lanes: 1,
                },
                LinearOp::StoreOutputRange {
                    start: 4,
                    count: 3,
                    stride: 1,
                },
            ],
        )],
        vec![
            LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start: 0,
                index_start: 0,
                count: 3,
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 3,
                stride: 1,
            },
        ],
    )
    .expect("construct tensor-division conditional");
    let primal = vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadY { dst: 2, index: 2 },
        LinearOp::LoadY { dst: 3, index: 3 },
        LinearOp::FunctionConditional {
            dst_start: 4,
            capture_start: 0,
            program: std::sync::Arc::new(conditional),
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 3,
            stride: 1,
        },
    ]];

    let derived = crate::lower_scalar_program_block_ad(&primal)
        .expect("differentiate compact tensor division");
    let program = derived[0]
        .iter()
        .find_map(|operation| match operation {
            LinearOp::FunctionConditional { program, .. } => Some(program),
            _ => None,
        })
        .expect("derived row keeps one conditional owner");

    assert_eq!(program.capture_count, 8);
    assert_eq!(program.target_widths.as_ref(), &[6]);
    assert!(program.arms[0].result.iter().any(|operation| matches!(
        operation,
        LinearOp::TensorBinary {
            op: rumoca_ir_solve::BinaryOp::Div,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 0,
            lanes: 2,
            ..
        }
    )));
    assert!(matches!(
        program.arms[0].result.last(),
        Some(LinearOp::StoreOutputRange {
            count: 6,
            stride: 1,
            ..
        })
    ));
}

#[test]
fn tensor_cross_ad_retains_one_interleaved_owner() {
    let mut row = (0..6)
        .map(|index| LinearOp::LoadY {
            dst: index as u32,
            index,
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::TensorCross {
        dst_start: 6,
        lhs_start: 0,
        rhs_start: 3,
        lanes: 1,
    });
    row.push(LinearOp::StoreOutputRange {
        start: 6,
        count: 3,
        stride: 1,
    });

    let derived = crate::lower_scalar_program_block_ad(&[row])
        .expect("differentiate compact tensor cross product");
    assert!(
        derived[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::TensorCross { lanes: 2, .. }))
    );
    assert!(matches!(
        derived[0].last(),
        Some(LinearOp::StoreOutputRange {
            count: 3,
            stride: 2,
            ..
        })
    ));
}

#[test]
fn cubic_power_lowers_to_multiplication_chain() {
    let source = TestSource::new("Real y; y = time ^ 3;");
    let owner = source.at(0, 21);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                owner,
            )
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("y"),
                real,
                owner,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let y = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let time = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            let three = expressions.at(owner).literal(dae::DaeLiteral::Integer(3))?;
            let cubic = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Power, time, three)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, y, cubic)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    let operations = &rows.programs()[0];
    assert_eq!(
        operations
            .iter()
            .filter(|operation| matches!(
                operation,
                LinearOp::Binary {
                    op: rumoca_ir_solve::BinaryOp::Mul,
                    ..
                }
            ))
            .count(),
        2
    );
    assert!(!operations.iter().any(|operation| matches!(
        operation,
        LinearOp::Binary {
            op: rumoca_ir_solve::BinaryOp::Pow,
            ..
        }
    )));
}

#[test]
fn integer_builtin_lowers_to_floor_without_conflating_division_semantics() {
    let source = TestSource::new("Real y; y = integer(time - 0.5);");
    let owner = source.at(0, 32);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                owner,
            )
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("y"),
                real,
                owner,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let y = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let time = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            let half = expressions.at(owner).literal(dae::DaeLiteral::Real(0.5))?;
            let shifted =
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Subtract, time, half)?;
            let integer = expressions
                .at(owner)
                .builtin(dae::PureBuiltin::Integer, [shifted])?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, y, integer)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    assert!(rows.programs()[0].iter().any(|operation| matches!(
        operation,
        LinearOp::Unary {
            op: rumoca_ir_solve::UnaryOp::Floor,
            ..
        }
    )));
    assert!(!rows.programs()[0].iter().any(|operation| matches!(
        operation,
        LinearOp::Unary {
            op: rumoca_ir_solve::UnaryOp::Trunc,
            ..
        }
    )));
}

#[test]
fn promoted_concatenation_selects_each_operand_scalar_in_result_order() {
    let source = TestSource::new("[1,2;3,4]");
    let owner = source.at(0, 9);
    let model = dae::Dae::construct(source.map, |model| {
        let matrix = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2, 2]),
                owner,
            )
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("y"),
                matrix,
                owner,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
            let two = expressions.at(owner).literal(dae::DaeLiteral::Real(2.0))?;
            let three = expressions.at(owner).literal(dae::DaeLiteral::Real(3.0))?;
            let four = expressions.at(owner).literal(dae::DaeLiteral::Real(4.0))?;
            let first = expressions.at(owner).array([one, two])?;
            let second = expressions.at(owner).array([three, four])?;
            let concatenation = expressions
                .at(owner)
                .builtin(dae::PureBuiltin::PromotedCat2, [first, second])?;
            let lhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, lhs, concatenation)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    solve
        .validate()
        .expect("constructor-certified concatenation produces valid Solve rows");
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    assert_eq!(rows.row_count(), 1);
    assert_eq!(
        eval_residual_rows(rows, &[0.0; 4], &[]),
        [-1.0, -3.0, -2.0, -4.0]
    );
}

#[test]
fn identity_derives_diagonal_constants_without_materializing_dae_scalars() {
    let source = TestSource::new("Real y[2,2]; y = identity(2);");
    let owner = source.at(0, 29);
    let model = dae::Dae::construct(source.map, |model| {
        let matrix = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2, 2]),
                owner,
            )
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("y"),
                matrix,
                owner,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let extent = expressions.at(owner).literal(dae::DaeLiteral::Integer(2))?;
            let identity = expressions
                .at(owner)
                .builtin(dae::PureBuiltin::Identity, [extent])?;
            let lhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, lhs, identity)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    assert_eq!(
        model.inspect(|view| view.expression_count()),
        4,
        "identity remains one compact expression"
    );
    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected")
    };
    assert_eq!(rows.row_count(), 1);
    assert_eq!(
        eval_residual_rows(rows, &[0.0; 4], &[]),
        [-1.0, 0.0, 0.0, -1.0]
    );
}

#[test]
fn vector_lowers_each_result_scalar_directly_from_its_compact_operand() {
    let source = TestSource::new("parameter Real p[1,3,1]; Real y[3]; y = vector(p);");
    let owner = source.at(0, 50);
    let model = dae::Dae::construct(source.map, |model| {
        let tensor = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [1, 3, 1]),
                owner,
            )
        })?;
        let vector = model.types(|types| {
            types.intern(
                TypeId::new(1),
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                owner,
            )
        })?;
        let (p, y) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    tensor,
                    owner,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("y"),
                    vector,
                    owner,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let p = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(p))?;
            let vector = expressions
                .at(owner)
                .builtin(dae::PureBuiltin::Vector, [p])?;
            let y = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(y))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, y, vector)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    assert_eq!(
        model.inspect(|view| view.expression_count()),
        4,
        "vector remains one compact DAE node"
    );
    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected")
    };
    assert_eq!(rows.row_count(), 1);
    assert_eq!(
        eval_residual_rows(rows, &[0.0; 3], &[1.0, 2.0, 3.0]),
        [-1.0, -2.0, -3.0]
    );
}

#[test]
fn transpose_lowers_rank_three_rows_through_the_exact_operand_permutation() {
    let source = TestSource::new("parameter Real p[2,3,2]; Real y[3,2,2]; y = transpose(p);");
    let owner = source.at(0, 57);
    let model = dae::Dae::construct(source.map, |model| {
        let input_type = model.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [2, 3, 2]),
                owner,
            )
        })?;
        let result_type = model.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [3, 2, 2]),
                owner,
            )
        })?;
        let (p, y) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    input_type,
                    owner,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("y"),
                    result_type,
                    owner,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let p = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(p))?;
            let transpose = expressions
                .at(owner)
                .builtin(dae::PureBuiltin::Transpose, [p])?;
            let y = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(y))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, y, transpose)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    assert_eq!(model.inspect(|view| view.expression_count()), 4);
    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected")
    };
    let expected = [0, 1, 6, 7, 2, 3, 8, 9, 4, 5, 10, 11];
    assert_eq!(rows.row_count(), 1);
    let parameters = (0..12).map(|value| value as f64).collect::<Vec<_>>();
    let expected = expected.map(|index| -(index as f64));
    assert_eq!(eval_residual_rows(rows, &[0.0; 12], &parameters), expected);
}

#[test]
fn skew_lowers_each_matrix_scalar_from_one_compact_parameter_vector() {
    let source = TestSource::new("parameter Real p[3]; Real y[3,3]; y = skew(p);");
    let owner = source.at(0, 46);
    let model = dae::Dae::construct(source.map, |model| {
        let (vector, matrix) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), owner)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3, 3]), owner)?,
            ))
        })?;
        let (p, y) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    vector,
                    owner,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("y"),
                    matrix,
                    owner,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let p = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(p))?;
            let skew = expressions.at(owner).builtin(dae::PureBuiltin::Skew, [p])?;
            let y = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(y))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, y, skew)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    assert_eq!(
        model.inspect(|view| view.expression_count()),
        4,
        "skew remains one compact DAE node"
    );
    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected")
    };
    let expected = [
        None,
        Some((2, true)),
        Some((1, false)),
        Some((2, false)),
        None,
        Some((0, true)),
        Some((1, true)),
        Some((0, false)),
        None,
    ];
    assert_eq!(rows.row_count(), 1);
    let parameters = [1.0, 2.0, 3.0];
    let expected = expected.map(|entry| match entry {
        None => 0.0,
        Some((parameter, negative)) => {
            let rhs = if negative {
                -parameters[parameter]
            } else {
                parameters[parameter]
            };
            -rhs
        }
    });
    assert_eq!(eval_residual_rows(rows, &[0.0; 9], &parameters), expected);
}

#[test]
fn cross_lowers_to_one_checked_tensor_owner() {
    let source =
        TestSource::new("parameter Real p[3]; parameter Real q[3]; Real y[3]; y = cross(p,q);");
    let at = source.at(0, 67);
    let model = dae::Dae::construct(source.map, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let (p, q, y) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.parameter(
                    VarName::new("q"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("y"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let p = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(p))?;
            let q = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(q))?;
            let cross = expressions
                .at(at)
                .builtin(dae::PureBuiltin::Cross, [p, q])?;
            let y = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(y))?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, y, cross)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one tensor residual block expected");
    };
    assert_eq!(rows.row_count(), 1);
    assert_eq!(
        rows.programs()[0]
            .iter()
            .filter(|operation| matches!(operation, LinearOp::TensorCross { lanes: 1, .. }))
            .count(),
        1
    );
    assert_eq!(
        eval_residual_rows(rows, &[0.0; 3], &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0]),
        [3.0, -6.0, 3.0]
    );
}

#[test]
fn static_quotient_family_lowers_to_computable_solve_operations() {
    // `-` before `mod` (not `+`) so the three quotients do not sum
    // symmetrically: `div - mod + rem` separates `mod` from `rem`, while
    // `div + mod + rem` is invariant under exchanging them.
    let source = TestSource::new("Real y; y = div(-7,3) - mod(-7,3) + rem(-7,3);");
    let declaration = source.at(0, 6);
    let equation_owner = source.at(8, 45);
    let div_owner = source.at(12, 21);
    let mod_owner = source.at(24, 33);
    let rem_owner = source.at(36, 45);
    let div_lhs_at = source.at(16, 18);
    let div_rhs_at = source.at(19, 20);
    let mod_lhs_at = source.at(28, 30);
    let mod_rhs_at = source.at(31, 32);
    let rem_lhs_at = source.at(40, 42);
    let rem_rhs_at = source.at(43, 44);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("y"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let lhs = expressions
                .at(equation_owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let div_lhs = expressions
                .at(div_lhs_at)
                .literal(dae::DaeLiteral::Integer(-7))?;
            let div_rhs = expressions
                .at(div_rhs_at)
                .literal(dae::DaeLiteral::Integer(3))?;
            let div = expressions
                .at(div_owner)
                .builtin(dae::PureBuiltin::Div, [div_lhs, div_rhs])?;
            let mod_lhs = expressions
                .at(mod_lhs_at)
                .literal(dae::DaeLiteral::Integer(-7))?;
            let mod_rhs = expressions
                .at(mod_rhs_at)
                .literal(dae::DaeLiteral::Integer(3))?;
            let modulo = expressions
                .at(mod_owner)
                .builtin(dae::PureBuiltin::Mod, [mod_lhs, mod_rhs])?;
            let rem_lhs = expressions
                .at(rem_lhs_at)
                .literal(dae::DaeLiteral::Integer(-7))?;
            let rem_rhs = expressions
                .at(rem_rhs_at)
                .literal(dae::DaeLiteral::Integer(3))?;
            let remainder = expressions
                .at(rem_owner)
                .builtin(dae::PureBuiltin::Rem, [rem_lhs, rem_rhs])?;
            let sum = expressions.at(equation_owner).binary(
                dae::BinaryOperator::Subtract,
                div,
                modulo,
            )?;
            let sum =
                expressions
                    .at(equation_owner)
                    .binary(dae::BinaryOperator::Add, sum, remainder)?;
            expressions
                .at(equation_owner)
                .binary(dae::BinaryOperator::Subtract, lhs, sum)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    solve
        .validate()
        .expect("constructor-certified quotients produce computable Solve IR");
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    assert_static_quotient_program(&rows.programs()[0]);
    // The op-mix assertions above count Floor and Trunc without saying which
    // builtin owns which, so exchanging the `mod` and `rem` lowerings leaves
    // them all satisfied. Pin the value: MLS 3.6 §3.7.3 gives
    // `div(-7,3) = -2`, `mod(-7,3) = 2`, `rem(-7,3) = -1`, so the residual
    // `y - (div - mod + rem)` at `y = 0` is `5`.
    assert_eq!(eval_residual_rows(rows, &[0.0], &[]), [5.0]);
}

fn assert_static_quotient_program(operations: &[LinearOp]) {
    assert_eq!(
        operations
            .iter()
            .filter(|operation| matches!(
                operation,
                LinearOp::Binary {
                    op: rumoca_ir_solve::BinaryOp::Div,
                    ..
                }
            ))
            .count(),
        3
    );
    assert_eq!(
        operations
            .iter()
            .filter(|operation| matches!(
                operation,
                LinearOp::Unary {
                    op: rumoca_ir_solve::UnaryOp::Trunc,
                    ..
                }
            ))
            .count(),
        2
    );
    assert!(operations.iter().any(|operation| matches!(
        operation,
        LinearOp::Unary {
            op: rumoca_ir_solve::UnaryOp::Floor,
            ..
        }
    )));
}
