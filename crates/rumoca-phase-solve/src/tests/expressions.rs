//! Expression-form lowering into register programs.
//!
//! The DAE expression families that need more than one Solve operation each:
//! the static quotient builtins, and a checked function call inlined into the
//! row that uses it.

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
    let source = TestSource::new("Real y; y = div(-7,3) + mod(-7,3) + rem(-7,3);");
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
            let sum =
                expressions
                    .at(equation_owner)
                    .binary(dae::BinaryOperator::Add, div, modulo)?;
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

#[test]
fn demanded_function_call_issues_one_typed_owner() {
    let source =
        TestSource::new("function f input Real u; output Real y; y := u + 1; Real z; z=f(2);");
    let function_at = source.at(0, 51);
    let variable_at = source.at(52, 59);
    let owner = source.at(60, 67);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let signature = dae::FunctionSignature::new(VarName::new("f"), [real], [real], function_at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, function_at)
            })?;
            let output = model.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, function_at)
            })?;
            let local = model.functions(|functions| {
                functions.local(&reservation, VarName::new("tmp"), real, function_at)
            })?;
            let local_definition = model.expressions(|expressions| {
                let parameter = expressions.at(function_at).function_parameter(parameter)?;
                let one = expressions
                    .at(function_at)
                    .literal(dae::DaeLiteral::Real(1.0))?;
                expressions
                    .at(function_at)
                    .binary(dae::BinaryOperator::Add, parameter, one)
            })?;
            let mut body =
                model.functions(|functions| functions.begin(reservation, function_at))?;
            model.functions(|functions| {
                functions.assign(&mut body, local, local_definition, function_at)
            })?;
            let local_value =
                model.functions(|functions| functions.read(&body, local, function_at))?;
            let result = model.expressions(|expressions| {
                let two = expressions
                    .at(function_at)
                    .literal(dae::DaeLiteral::Real(2.0))?;
                expressions
                    .at(function_at)
                    .binary(dae::BinaryOperator::Multiply, local_value, two)
            })?;
            model
                .functions(|functions| functions.assign(&mut body, output, result, function_at))?;
            model.functions(|functions| functions.define(body, function_at))
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let two = expressions.at(owner).literal(dae::DaeLiteral::Real(2.0))?;
            let call = expressions.at(owner).call(function, 0, [two])?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let package = lower_solve_package(&model).unwrap();
    let [owner] = package.pure_calls.owners() else {
        panic!("one exact demanded pure-call owner expected");
    };
    assert_eq!(owner.inputs().len(), 1);
    assert_eq!(owner.outputs().len(), 1);
    assert!(owner.body().operations().len() >= 5);

    let [ComputeNode::ScalarPrograms(rows)] = package.problem.continuous.residual.nodes.as_slice()
    else {
        panic!("one scalar residual block expected");
    };
    // This assertion records the transition state. The production typed owner
    // is issued above, but scalar consumers still carry the legacy body until
    // the compact owner-projection cutover removes it.
    assert!(
        rows.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::Binary { .. }))
    );
}

#[test]
fn clocked_function_call_projects_issued_typed_owner_without_body_inlining() {
    let source = TestSource::new(
        "function f input Real u; output Real y; y := u + 1; discrete Real z; z=f(2);",
    );
    let function_at = source.at(0, 51);
    let variable_at = source.at(52, 68);
    let clock_at = source.at(68, 69);
    let owner = source.at(69, 76);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 100).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let signature = dae::FunctionSignature::new(VarName::new("f"), [real], [real], function_at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, function_at)
            })?;
            let output = model.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, function_at)
            })?;
            let value = model.expressions(|expressions| {
                let parameter = expressions.at(function_at).function_parameter(parameter)?;
                let one = expressions
                    .at(function_at)
                    .literal(dae::DaeLiteral::Real(1.0))?;
                expressions
                    .at(function_at)
                    .binary(dae::BinaryOperator::Add, parameter, one)
            })?;
            let mut body =
                model.functions(|functions| functions.begin(reservation, function_at))?;
            model.functions(|functions| functions.assign(&mut body, output, value, function_at))?;
            model.functions(|functions| functions.define(body, function_at))
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), variable, owner))?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let two = expressions.at(owner).literal(dae::DaeLiteral::Real(2.0))?;
            let call = expressions.at(owner).call(function, 0, [two])?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, call)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let package = lower_solve_package(&model).unwrap();
    assert_eq!(package.pure_calls.owners().len(), 1);
    let [row] = package.problem.discrete.rhs.programs() else {
        panic!("one clocked discrete row expected");
    };
    assert_eq!(
        row.iter()
            .filter(|operation| matches!(operation, LinearOp::PureCall { .. }))
            .count(),
        1
    );
    assert_eq!(
        row.iter()
            .filter(|operation| matches!(operation, LinearOp::Binary { .. }))
            .count(),
        0,
        "the function body is not embedded in the clocked assignment row"
    );
}

#[test]
fn function_conditional_captures_preceding_definition_once_per_call_frame() {
    let source = TestSource::new(
        "function f input Boolean c; input Real u; output Real y; Real tmp; end f; Real z; Real z2;",
    );
    let at = source.at(0, 81);
    let model = dae::Dae::construct(source.map, |model| {
        let boolean = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at))?;
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let signature = dae::FunctionSignature::new(VarName::new("f"), [boolean, real], [real], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let condition = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("c"), 0, at)
            })?;
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 1, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let base = model.functions(|functions| {
                functions.local(&reservation, VarName::new("base"), real, at)
            })?;
            let conditional_local = model.functions(|functions| {
                functions.local(&reservation, VarName::new("tmp"), real, at)
            })?;
            let local_definition = model.expressions(|expressions| {
                let input = expressions.at(at).function_parameter(input)?;
                let two = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Multiply, input, two)
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, base, local_definition, at))?;
            let local_value = model.functions(|functions| functions.read(&body, base, at))?;
            let condition = model
                .expressions(|expressions| expressions.at(at).function_parameter(condition))?;
            let (branch, fallback) = model.expressions(|expressions| {
                let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                let two = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                Ok((
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, local_value, one)?,
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, local_value, two)?,
                ))
            })?;
            model.functions(|functions| {
                functions.assign_conditional_all(
                    &mut body,
                    &[conditional_local],
                    &[condition],
                    &[vec![branch]],
                    &[fallback],
                    at,
                )
            })?;
            let conditional_value =
                model.functions(|functions| functions.read(&body, conditional_local, at))?;
            model.functions(|functions| {
                functions.assign(&mut body, output, conditional_value, at)
            })?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let (algebraic, algebraic_2) = model.variables(|variables| {
            Ok((
                variables.algebraic(
                    VarName::new("z"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("z2"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (residual, residual_2) = model.expressions(|expressions| {
            let z = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let z2 = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic_2))?;
            let time = expressions.at(at).coordinate(dae::CoordinateInput::Time)?;
            let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let condition = expressions
                .at(at)
                .binary(dae::BinaryOperator::Greater, time, zero)?;
            let input = expressions.at(at).literal(dae::DaeLiteral::Real(3.0))?;
            let call = expressions.at(at).call(function, 0, [condition, input])?;
            Ok((
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, z, call)?,
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, z2, call)?,
            ))
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(at, residual)?;
            continuous.value_equation(at, residual_2)
        })
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    let conditionals = rows
        .programs()
        .iter()
        .map(|row| {
            row.iter()
                .find_map(|operation| match operation {
                    LinearOp::FunctionConditional { program, .. } => Some(program),
                    _ => None,
                })
                .unwrap_or_else(|| {
                    panic!("function conditional remains one checked owner: {row:#?}")
                })
        })
        .collect::<Vec<_>>();
    let conditional = conditionals[0];
    assert!(
        std::sync::Arc::ptr_eq(conditionals[0], conditionals[1]),
        "independent residual projections retain one exact call-frame owner"
    );

    assert_eq!(conditional.capture_count, 1);
    assert_eq!(
        rows.programs()[0]
            .iter()
            .filter(|operation| matches!(
                operation,
                LinearOp::Binary {
                    op: rumoca_ir_solve::BinaryOp::Mul,
                    ..
                }
            ))
            .count(),
        1,
        "the preceding definition is computed once in the parent"
    );
    assert!(
        conditional
            .arms
            .iter()
            .flat_map(|arm| arm.condition.iter().chain(&arm.result))
            .chain(&conditional.fallback)
            .all(|operation| !matches!(
                operation,
                LinearOp::Binary {
                    op: rumoca_ir_solve::BinaryOp::Mul,
                    ..
                }
            )),
        "lazy regions load the captured definition instead of rebuilding it"
    );
    assert_eq!(eval_residual_rows(rows, &[0.0, 0.0], &[]), [-8.0, -8.0]);
}

#[test]
fn function_conditional_captures_tensor_definition_as_one_semantic_range() {
    let source = TestSource::new(
        "function f input Boolean c; input Real u[3]; output Real y[3]; Real base[3]; Real tmp[3]; end f; parameter Real p[3]; Real z[3];",
    );
    let at = source.at(0, 124);
    let model = dae::Dae::construct(source.map, |model| {
        let (boolean, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
            ))
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("f"), [boolean, vector], [vector], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let condition = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("c"), 0, at)
            })?;
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 1, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let base = model.functions(|functions| {
                functions.local(&reservation, VarName::new("base"), vector, at)
            })?;
            let selected = model.functions(|functions| {
                functions.local(&reservation, VarName::new("tmp"), vector, at)
            })?;
            let input_value =
                model.expressions(|expressions| expressions.at(at).function_parameter(input))?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, base, input_value, at))?;
            let base_value = model.functions(|functions| functions.read(&body, base, at))?;
            let condition = model
                .expressions(|expressions| expressions.at(at).function_parameter(condition))?;
            model.functions(|functions| {
                functions.assign_conditional_all(
                    &mut body,
                    &[selected],
                    &[condition],
                    &[vec![base_value]],
                    &[base_value],
                    at,
                )
            })?;
            let selected_value =
                model.functions(|functions| functions.read(&body, selected, at))?;
            model.functions(|functions| functions.assign(&mut body, output, selected_value, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let (parameter, algebraic) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("z"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let time = expressions.at(at).coordinate(dae::CoordinateInput::Time)?;
            let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let condition = expressions
                .at(at)
                .binary(dae::BinaryOperator::Greater, time, zero)?;
            let p = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            let call = expressions.at(at).call(function, 0, [condition, p])?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one tensor residual block expected");
    };
    let conditional = rows.programs()[0]
        .iter()
        .find_map(|operation| match operation {
            LinearOp::FunctionConditional { program, .. } => Some(program),
            _ => None,
        })
        .expect("tensor conditional remains one checked owner");
    let region_operations = conditional
        .arms
        .iter()
        .flat_map(|arm| arm.condition.iter().chain(&arm.result))
        .chain(&conditional.fallback)
        .collect::<Vec<_>>();

    assert_eq!(conditional.capture_count, 3);
    assert_eq!(conditional.target_widths.as_ref(), &[3]);
    assert_eq!(
        region_operations
            .iter()
            .filter(|operation| matches!(
                operation,
                LinearOp::LoadFunctionConditionalCaptureRange { count: 3, .. }
            ))
            .count(),
        2,
        "both result regions consume the definition as one compact tensor range"
    );
    assert!(
        region_operations
            .iter()
            .all(|operation| !matches!(operation, LinearOp::LoadFunctionConditionalCapture { .. }))
    );
    assert_eq!(
        eval_residual_rows(rows, &[0.0; 3], &[1.0, 2.0, 3.0]),
        [-1.0, -2.0, -3.0]
    );
}

#[test]
fn aggregate_conditional_expression_retains_one_lazy_tensor_result_range() {
    let source = TestSource::new(
        "function choose input Boolean c; input Real u[3]; output Real y[3]; end choose; parameter Real p[3]; Real z[3];",
    );
    let at = source.at(0, 108);
    let model = dae::Dae::construct(source.map, |model| {
        let (boolean, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
            ))
        })?;
        let identity_signature =
            dae::FunctionSignature::new(VarName::new("identity"), [vector], [vector], at);
        let (identity, ()) = model.function(identity_signature, |model, reservation| {
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let input =
                model.expressions(|expressions| expressions.at(at).function_parameter(input))?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            let (failed, message) = model.expressions(|expressions| {
                Ok((
                    expressions
                        .at(at)
                        .literal(dae::DaeLiteral::Boolean(false))?,
                    expressions.at(at).literal(dae::DaeLiteral::String(
                        "selected tensor branch executed".to_owned(),
                    ))?,
                ))
            })?;
            model.functions(|functions| functions.assertion(&mut body, failed, message, at))?;
            model.functions(|functions| functions.assign(&mut body, output, input, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let choose_signature =
            dae::FunctionSignature::new(VarName::new("choose"), [boolean, vector], [vector], at);
        let (choose, ()) = model.function(choose_signature, |model, reservation| {
            let condition = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("c"), 0, at)
            })?;
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 1, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let (condition, input) = model.expressions(|expressions| {
                Ok((
                    expressions.at(at).function_parameter(condition)?,
                    expressions.at(at).function_parameter(input)?,
                ))
            })?;
            let selected = model.expressions(|expressions| {
                let call = expressions.at(at).call(identity, 0, [input])?;
                expressions.at(at).conditional([(condition, call)], input)
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, selected, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let (parameter, algebraic) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("z"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let time = expressions.at(at).coordinate(dae::CoordinateInput::Time)?;
            let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let condition = expressions
                .at(at)
                .binary(dae::BinaryOperator::Greater, time, one)?;
            let parameter = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            let call = expressions.at(at).call(choose, 0, [condition, parameter])?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap();

    let package = lower_solve_package(&model).unwrap();
    let solve = &package.problem;
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one tensor residual block expected");
    };
    let conditional = rows.programs()[0]
        .iter()
        .find_map(|operation| match operation {
            LinearOp::FunctionConditional { program, .. } => Some(program),
            _ => None,
        })
        .expect("aggregate conditional expression remains one checked lazy owner");

    assert!(
        conditional.owner.is_some(),
        "an aggregate expression conditional receives an issued semantic owner"
    );
    assert_eq!(conditional.target_widths.as_ref(), &[3]);
    assert!(
        conditional
            .arms
            .iter()
            .flat_map(|arm| arm.result.iter())
            .chain(&conditional.fallback)
            .all(|operation| !matches!(operation, LinearOp::StoreOutput { .. }))
    );
    assert_eq!(
        conditional
            .arms
            .iter()
            .flat_map(|arm| arm.result.iter())
            .chain(&conditional.fallback)
            .filter(|operation| matches!(
                operation,
                LinearOp::StoreOutputRange {
                    count: 3,
                    stride: 1,
                    ..
                }
            ))
            .count(),
        2,
        "each lazy branch retains one checked tensor projection until its final output ABI"
    );
    assert_eq!(
        eval_residual_rows(rows, &[0.0; 3], &[1.0, 2.0, 3.0]),
        [-1.0, -2.0, -3.0]
    );
    assert_eq!(solve.events.root_conditions.len(), 1);
    let mut root = [0.0];
    let parameters = vec![0.0; solve.layout.p_scalars()];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &solve.events.root_conditions,
        &[0.0; 3],
        &parameters,
        0.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            ..Default::default()
        },
        &mut root,
    )
    .unwrap();
    assert_eq!(
        root,
        [-1.0],
        "an assertion hoisted from an inactive tensor arm retains that arm's guard"
    );
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &solve.events.root_conditions,
        &[0.0; 3],
        &parameters,
        2.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            ..Default::default()
        },
        &mut root,
    )
    .unwrap();
    assert_eq!(
        root,
        [1.0],
        "the selected tensor arm still owns its assertion"
    );
}

#[test]
fn call_scoped_assertion_constructs_guarded_root_and_action_rows() {
    let package = function_assertion_solve(true, -1.0, false, false);
    let solve = &package.problem;
    assert_eq!(solve.events.root_conditions.len(), 1);
    assert_eq!(solve.events.actions.len(), 1);
    let mut root = [0.0];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &solve.events.root_conditions,
        &[0.0],
        &vec![0.0; solve.layout.p_scalars()],
        0.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            ..Default::default()
        },
        &mut root,
    )
    .unwrap();
    assert_eq!(root, [1.0], "an active failing assertion is above zero");
    let request = rumoca_eval_solve::eval_event_action_request(
        &solve.events,
        &[0.0],
        &vec![0.0; solve.layout.p_scalars()],
        0.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            ..Default::default()
        },
    )
    .unwrap();
    assert!(matches!(
        request,
        rumoca_eval_solve::EventActionRequest::AssertionFailed { ref message }
            if message == "positive input required"
    ));
}

#[test]
fn one_typed_call_owns_all_assertion_root_and_action_outputs() {
    let package = function_two_assertions_solve();
    let solve = &package.problem;
    assert_eq!(solve.events.root_conditions.programs().len(), 1);
    assert_eq!(solve.events.root_conditions.output_indices().len(), 2);
    assert_eq!(solve.events.action_conditions.programs().len(), 1);
    assert_eq!(solve.events.action_conditions.output_indices().len(), 2);
    assert_eq!(solve.events.actions.len(), 2);
    assert_eq!(
        solve.events.root_conditions.programs()[0]
            .iter()
            .filter(|operation| matches!(operation, LinearOp::PureCall { .. }))
            .count(),
        1
    );
    assert_eq!(
        solve.events.action_conditions.programs()[0]
            .iter()
            .filter(|operation| matches!(operation, LinearOp::PureCall { .. }))
            .count(),
        1
    );
    let mut roots = [0.0; 2];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &solve.events.root_conditions,
        &[0.0],
        &vec![0.0; solve.layout.p_scalars()],
        0.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            ..Default::default()
        },
        &mut roots,
    )
    .unwrap();
    assert_eq!(roots, [1.0, -1.0]);
}

#[test]
fn inactive_conditional_call_cannot_fire_its_function_assertion() {
    let package = function_assertion_solve(false, -1.0, false, false);
    let solve = &package.problem;
    assert!(
        solve.events.root_conditions.is_empty(),
        "a statically unreachable call must not construct an event root"
    );
    assert!(
        solve.events.actions.is_empty(),
        "a statically unreachable call must not construct an action"
    );
}

fn function_two_assertions_solve() -> crate::LoweredSolvePackage {
    let source = TestSource::new(
        "function f input Real u; output Real y; assert(u > 0, \"positive\"); assert(u < 10, \"bounded\"); y := u; Real z; z = f(-1);",
    );
    let function_at = source.at(0, 108);
    let first_at = source.at(42, 68);
    let second_at = source.at(69, 96);
    let variable_at = source.at(109, 116);
    let equation_at = source.at(117, 120);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let function = model
            .function(
                dae::FunctionSignature::new(VarName::new("f"), [real], [real], function_at),
                |model, reservation| {
                    let parameter = model.functions(|functions| {
                        functions.parameter(&reservation, VarName::new("u"), 0, function_at)
                    })?;
                    let output = model.functions(|functions| {
                        functions.output(&reservation, VarName::new("y"), 0, function_at)
                    })?;
                    let parameter = model.expressions(|expressions| {
                        expressions.at(function_at).function_parameter(parameter)
                    })?;
                    let (zero, ten, first_message, second_message) =
                        model.expressions(|expressions| {
                            Ok((
                                expressions
                                    .at(first_at)
                                    .literal(dae::DaeLiteral::Real(0.0))?,
                                expressions
                                    .at(second_at)
                                    .literal(dae::DaeLiteral::Real(10.0))?,
                                expressions
                                    .at(first_at)
                                    .literal(dae::DaeLiteral::String("positive".to_owned()))?,
                                expressions
                                    .at(second_at)
                                    .literal(dae::DaeLiteral::String("bounded".to_owned()))?,
                            ))
                        })?;
                    let (positive, bounded) = model.expressions(|expressions| {
                        Ok((
                            expressions.at(first_at).binary(
                                dae::BinaryOperator::Greater,
                                parameter,
                                zero,
                            )?,
                            expressions.at(second_at).binary(
                                dae::BinaryOperator::Less,
                                parameter,
                                ten,
                            )?,
                        ))
                    })?;
                    let mut body =
                        model.functions(|functions| functions.begin(reservation, function_at))?;
                    model.functions(|functions| {
                        functions.assertion(&mut body, positive, first_message, first_at)?;
                        functions.assertion(&mut body, bounded, second_message, second_at)?;
                        functions.assign(&mut body, output, parameter, function_at)?;
                        functions.define(body, function_at)
                    })
                },
            )?
            .0;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let argument = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(-1.0))?;
            let call = expressions.at(equation_at).call(function, 0, [argument])?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .unwrap();
    lower_solve_package(&model).unwrap()
}

#[test]
fn shared_call_in_branch_condition_and_value_has_one_assertion_schedule() {
    let package = function_assertion_solve(true, 1.0, true, false);
    let solve = &package.problem;
    assert_eq!(solve.events.root_conditions.len(), 1);
    assert_eq!(solve.events.actions.len(), 1);
}

#[test]
fn nested_call_assertion_resolves_actual_argument_in_the_caller_frame() {
    let package = function_assertion_solve(true, 1.0, false, true);
    let solve = &package.problem;
    assert_eq!(solve.events.root_conditions.len(), 1);
    assert_eq!(solve.events.actions.len(), 1);
}

fn construct_asserting_identity<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    function_at: dae::DaeProvenance,
    assertion_at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(VarName::new("f"), [real], [real], function_at);
    model
        .function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, function_at)
            })?;
            let output = model.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, function_at)
            })?;
            let parameter_value = model.expressions(|expressions| {
                expressions.at(function_at).function_parameter(parameter)
            })?;
            let assertion = model.expressions(|expressions| {
                let zero = expressions
                    .at(assertion_at)
                    .literal(dae::DaeLiteral::Real(0.0))?;
                expressions.at(assertion_at).binary(
                    dae::BinaryOperator::Greater,
                    parameter_value,
                    zero,
                )
            })?;
            let message = model.expressions(|expressions| {
                expressions
                    .at(assertion_at)
                    .literal(dae::DaeLiteral::String(
                        "positive input required".to_owned(),
                    ))
            })?;
            let mut body =
                model.functions(|functions| functions.begin(reservation, function_at))?;
            model.functions(|functions| {
                functions.assertion(&mut body, assertion, message, assertion_at)
            })?;
            model.functions(|functions| {
                functions.assign(&mut body, output, parameter_value, function_at)
            })?;
            model.functions(|functions| functions.define(body, function_at))
        })
        .map(|(function, ())| function)
}

fn construct_call_wrapper<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    called: dae::FunctionId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(VarName::new("g"), [real], [real], at);
    model
        .function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("v"), 0, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let parameter_value = model
                .expressions(|expressions| expressions.at(at).function_parameter(parameter))?;
            let call = model
                .expressions(|expressions| expressions.at(at).call(called, 0, [parameter_value]))?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, call, at))?;
            model.functions(|functions| functions.define(body, at))
        })
        .map(|(function, ())| function)
}

fn function_assertion_solve(
    branch_active: bool,
    call_value: f64,
    call_controls_branch: bool,
    nested_call: bool,
) -> crate::LoweredSolvePackage {
    let source = TestSource::new(
        "function f input Real u; output Real y; assert(u > 0, \"positive input required\"); y := u; Real z; z = if active then f(value) else 0;",
    );
    let function_at = source.at(0, 94);
    let assertion_at = source.at(44, 84);
    let variable_at = source.at(95, 102);
    let equation_at = source.at(104, 133);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let function = construct_asserting_identity(model, real, function_at, assertion_at)?;
        let called_function = if nested_call {
            construct_call_wrapper(model, real, function, function_at)?
        } else {
            function
        };
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let active = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Boolean(branch_active))?;
            let argument = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(call_value))?;
            let call = expressions
                .at(equation_at)
                .call(called_function, 0, [argument])?;
            let fallback = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(0.0))?;
            let condition = if call_controls_branch {
                expressions
                    .at(equation_at)
                    .binary(dae::BinaryOperator::Greater, call, fallback)?
            } else {
                active
            };
            let selected = expressions
                .at(equation_at)
                .conditional([(condition, call)], fallback)?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, z, selected)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .unwrap();
    lower_solve_package(&model).unwrap()
}
