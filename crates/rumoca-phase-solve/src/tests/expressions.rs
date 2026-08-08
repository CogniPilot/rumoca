//! Expression-form lowering into register programs.
//!
//! The DAE expression families that need more than one Solve operation each:
//! the static quotient builtins, and a checked function call inlined into the
//! row that uses it.

use super::*;

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
    let values = rows
        .programs()
        .iter()
        .map(|program| {
            program
                .iter()
                .find_map(|operation| match operation {
                    LinearOp::Const { value, .. } => Some(*value),
                    _ => None,
                })
                .expect("each constant concatenation scalar owns a literal")
        })
        .collect::<Vec<_>>();
    assert_eq!(values, [1.0, 3.0, 2.0, 4.0]);
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
    let diagonal = rows
        .programs()
        .iter()
        .map(|program| {
            program
                .iter()
                .find_map(|operation| match operation {
                    LinearOp::Const { value, .. } => Some(*value),
                    _ => None,
                })
                .expect("each identity projection derives one constant")
        })
        .collect::<Vec<_>>();
    assert_eq!(diagonal, [1.0, 0.0, 0.0, 1.0]);
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
    assert_eq!(rows.programs().len(), 3);
    for (index, program) in rows.programs().iter().enumerate() {
        assert!(program.iter().any(
            |operation| matches!(operation, LinearOp::LoadP { index: found, .. } if *found == index)
        ));
    }
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
    assert_eq!(rows.programs().len(), expected.len());
    for (program, expected) in rows.programs().iter().zip(expected) {
        assert!(program.iter().any(
            |operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == expected)
        ));
    }
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
    assert_eq!(rows.programs().len(), expected.len());
    for (program, expected) in rows.programs().iter().zip(expected) {
        let Some((parameter, negative)) = expected else {
            assert!(program.iter().any(
                |operation| matches!(operation, LinearOp::Const { value, .. } if *value == 0.0)
            ));
            continue;
        };
        assert!(program.iter().any(
            |operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == parameter)
        ));
        assert_eq!(
            program.iter().any(|operation| matches!(
                operation,
                LinearOp::Unary {
                    op: rumoca_ir_solve::UnaryOp::Neg,
                    ..
                }
            )),
            negative
        );
    }
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
fn checked_function_call_is_inlined_into_solve_program() {
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

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    assert!(
        rows.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::Binary { .. }))
    );
}

#[test]
fn call_scoped_assertion_constructs_guarded_root_and_action_rows() {
    let solve = function_assertion_solve(true, -1.0, false, false);
    assert_eq!(solve.events.root_conditions.len(), 1);
    assert_eq!(solve.events.actions.len(), 1);
    let mut root = [0.0];
    rumoca_eval_solve::eval_scalar_program_block(
        &solve.events.root_conditions,
        &[0.0],
        &vec![0.0; solve.layout.p_scalars()],
        0.0,
        None,
        &mut root,
    )
    .unwrap();
    assert_eq!(root, [1.0], "an active failing assertion is above zero");
    let request = rumoca_eval_solve::eval_event_action_request(
        &solve.events,
        &[0.0],
        &vec![0.0; solve.layout.p_scalars()],
        0.0,
        rumoca_eval_solve::RowEvalContext::default(),
    )
    .unwrap();
    assert!(matches!(
        request,
        rumoca_eval_solve::EventActionRequest::AssertionFailed { ref message }
            if message == "positive input required"
    ));
}

#[test]
fn inactive_conditional_call_cannot_fire_its_function_assertion() {
    let solve = function_assertion_solve(false, -1.0, false, false);
    assert!(
        solve.events.root_conditions.is_empty(),
        "a statically unreachable call must not construct an event root"
    );
    assert!(
        solve.events.actions.is_empty(),
        "a statically unreachable call must not construct an action"
    );
}

#[test]
fn shared_call_in_branch_condition_and_value_has_one_assertion_schedule() {
    let solve = function_assertion_solve(true, 1.0, true, false);
    assert_eq!(solve.events.root_conditions.len(), 1);
    assert_eq!(solve.events.actions.len(), 1);
}

#[test]
fn nested_call_assertion_resolves_actual_argument_in_the_caller_frame() {
    let solve = function_assertion_solve(true, 1.0, false, true);
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
) -> rumoca_ir_solve::SolveProblem {
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
    lower_solve_problem(&model).unwrap()
}
