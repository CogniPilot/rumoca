//! Continuous-time residual and derivative program lowering.
//!
//! These cases fix the shape Solve produces for a matched continuous system:
//! which rows become scalar programs, which become one checked linear solve,
//! which storage lanes the layout binds them to, and which forms it refuses to
//! claim computable.

use super::*;

fn local_source_variable_attributes<'dae>() -> dae::VariableAttributes<'dae> {
    dae::VariableAttributes {
        component_ref: None,
        binding: None,
        start: None,
        fixed: None,
        min: None,
        max: None,
        nominal: None,
        unit: None,
        state_select: rumoca_core::StateSelect::Default,
        description: None,
        causality: dae::VariableCausality::Local,
        is_tunable: false,
        is_held: false,
        origin: dae::VariableOrigin::Source,
    }
}

#[test]
fn causal_event_held_algebraic_owns_typed_solve_time_domain() {
    let source = TestSource::new("Real x; equation x = 1;");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 23);
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
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let value = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(
        solve.solve_layout().variable_declarations[0].time_domain(),
        rumoca_ir_solve::SolveVariableTimeDomain::EventDiscontinuous
    );
}

#[test]
fn explicit_state_equation_lowers_to_derivative_program() {
    let source = TestSource::new("Real x; der(x) = -x;");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 19);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state_attributes = real_state_attributes(model, declaration, 0.0, true)?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(2),
                real,
                declaration,
                state_attributes,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let state = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            let rhs = expressions
                .at(owner)
                .unary(dae::UnaryOperator::Negate, state)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, derivative, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(solve.solve_layout().state_scalar_count(), 1);
    assert!(solve.continuous().residual().nodes.is_empty());
    assert!(solve.continuous().implicit_rhs().nodes.is_empty());
    assert!(solve.continuous().implicit_row_targets().is_empty());
    assert!(solve.continuous().algebraic_projection_plan().is_empty());
    reseal_solve_problem(&solve)
        .expect("lowered explicit state system satisfies the Solve shape contract");
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous().derivative_rhs().nodes.as_slice()
    else {
        panic!("one scalar derivative block expected");
    };
    assert_eq!(rows.output_indices(), [0]);
    assert_eq!(rows.program_spans(), [owner.span()]);
    assert!(matches!(
        rows.programs()[0][0],
        LinearOp::LoadY { index: 0, .. }
    ));
    assert!(matches!(
        rows.programs()[0][1],
        LinearOp::Unary {
            op: rumoca_ir_solve::UnaryOp::Neg,
            ..
        }
    ));
}

#[test]
fn complete_solve_model_owns_exact_state_and_static_template_values() {
    let source = TestSource::new(
        "Real x(start=-0.0); parameter Real gain=2.5; constant Real bias=7.0; der(x)=0.0;",
    );
    let state_at = source.at(0, 18);
    let parameter_at = source.at(20, 42);
    let constant_at = source.at(44, 66);
    let equation_at = source.at(68, 80);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                state_at,
            )
        })?;
        let (gain_binding, bias_binding) = model.expressions(|expressions| {
            Ok((
                expressions
                    .at(parameter_at)
                    .literal(dae::DaeLiteral::Real(2.5))?,
                expressions
                    .at(constant_at)
                    .literal(dae::DaeLiteral::Real(7.0))?,
            ))
        })?;
        let state_attributes = real_state_attributes(model, state_at, -0.0, true)?;
        let state = model.variables(|variables| {
            let state = variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(3),
                real,
                state_at,
                state_attributes,
            )?;
            variables.parameter(
                VarName::new("gain"),
                rumoca_core::InstanceId::new(4),
                real,
                parameter_at,
                dae::VariableAttributes {
                    binding: Some(gain_binding),
                    ..dae::VariableAttributes::default()
                },
            )?;
            variables.constant(
                VarName::new("bias"),
                rumoca_core::InstanceId::new(5),
                real,
                constant_at,
                dae::VariableAttributes {
                    binding: Some(bias_binding),
                    ..dae::VariableAttributes::default()
                },
            )?;
            Ok(state)
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let zero = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(0.0))?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, derivative, zero)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .unwrap();

    let model = crate::lower_solve_model(&model, &std::collections::HashMap::new(), |_| {})
        .expect("complete Solve model lowers")
        .into_model();

    assert_eq!(model.state_names(), ["x"]);
    assert_eq!(
        model.initial_state_values()[0].to_bits(),
        (-0.0_f64).to_bits()
    );
    assert_eq!(model.static_parameter_names(), ["gain", "bias"]);
    assert_eq!(model.static_parameter_values(), [2.5, 7.0]);
}

#[test]
fn explicit_array_state_equation_rejects_uncontracted_numeric_reduction_at_call() {
    let source = TestSource::new("function f Real u[3]; Real y[3]; Real x[3]; der(x) = f(x);");
    let function_at = source.at(0, 34);
    let declaration = source.at(35, 45);
    let owner = source.at(46, 58);
    let model = dae::Dae::construct(source.map, |model| {
        let vector = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [3]),
                declaration,
            )
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("f"), [vector], [vector], function_at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, function_at)
            })?;
            let output = model.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, function_at)
            })?;
            let result = model.expressions(|expressions| {
                let parameter = expressions.at(function_at).function_parameter(parameter)?;
                let sum = expressions
                    .at(function_at)
                    .builtin(dae::PureBuiltin::Sum, [parameter])?;
                let product = expressions
                    .at(function_at)
                    .builtin(dae::PureBuiltin::Product, [parameter])?;
                expressions.at(function_at).array([sum, product, sum])
            })?;
            let mut body =
                model.functions(|functions| functions.begin(reservation, function_at))?;
            model
                .functions(|functions| functions.assign(&mut body, output, result, function_at))?;
            model.functions(|functions| functions.define(body, function_at))
        })?;
        let state_attributes = real_state_attributes(model, declaration, 0.0, true)?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(6),
                vector,
                declaration,
                state_attributes,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let state = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            let rhs = expressions.at(owner).call(function, 0, [state])?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, derivative, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let error = lower_solve_problem(&model)
        .expect_err("numeric reductions have no provisional typed execution semantics");
    assert!(matches!(
        error,
        LowerError::ContractViolation { reason, span }
            if reason == "typed reduction has no construction-issued arithmetic contract"
                && span == owner.span()
    ));
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction aggregate-call fixture enumerates shared and independent projections"
)]
fn exact_aggregate_call_projections_share_one_multi_output_program() {
    let source = TestSource::new(
        "function f input Real u; output Real y[3]; end f; Real a; Real b; Real c;",
    );
    let at = source.at(0, 73);
    let model = dae::Dae::construct(source.map, |model| {
        let (real, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
            ))
        })?;
        let signature = dae::FunctionSignature::new(VarName::new("f"), [real], [vector], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let value = model.expressions(|expressions| {
                let parameter = expressions.at(at).function_parameter(parameter)?;
                let square = expressions.at(at).binary(
                    dae::BinaryOperator::Multiply,
                    parameter,
                    parameter,
                )?;
                let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                let shifted = expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Add, square, one)?;
                expressions.at(at).array([square, shifted, parameter])
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, value, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let (a, b, c) = model.variables(|variables| {
            Ok((
                variables.algebraic(
                    VarName::new("a"),
                    rumoca_core::InstanceId::new(7),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("b"),
                    rumoca_core::InstanceId::new(8),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("c"),
                    rumoca_core::InstanceId::new(9),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (first, second, independent) = model.expressions(|expressions| {
            let input = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
            let shared_call = expressions.at(at).call(function, 0, [input])?;
            let independent_call = expressions.at(at).call(function, 0, [input])?;
            let one = expressions.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let two = expressions.at(at).literal(dae::DaeLiteral::Integer(2))?;
            let shared_first = expressions.at(at).index(
                shared_call,
                [dae::Subscript::Value {
                    expression: one,
                    provenance: at,
                }],
            )?;
            let shared_second = expressions.at(at).index(
                shared_call,
                [dae::Subscript::Value {
                    expression: two,
                    provenance: at,
                }],
            )?;
            let separate = expressions.at(at).index(
                independent_call,
                [dae::Subscript::Value {
                    expression: one,
                    provenance: at,
                }],
            )?;
            let a = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(a))?;
            let b = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(b))?;
            let c = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(c))?;
            Ok((
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, a, shared_first)?,
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, b, shared_second)?,
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, c, separate)?,
            ))
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(at, first)?;
            continuous.value_equation(at, second)?;
            continuous.value_equation(at, independent)
        })
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous().residual().nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    assert_eq!(rows.row_count(), 2);
    assert_eq!(rows.stored_output_count(), 3);
    assert_eq!(rows.output_indices(), [0, 1, 2]);
    assert_eq!(
        rows.stored_output_count_for_program(0),
        Some(2),
        "two projections of one issued call occurrence share one program"
    );
    assert_eq!(
        rows.stored_output_count_for_program(1),
        Some(1),
        "a distinct call ExprId is never inferred equivalent from its arguments"
    );
}

#[test]
fn aggregate_algebraic_residual_consumes_its_owner_once() {
    let source = TestSource::new("parameter Real p[2]; Real y[2]; equation y = p;");
    let at = source.at(0, 47);
    let model = dae::Dae::construct(source.map, |model| {
        let vector = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2]),
                at,
            )
        })?;
        let (parameter, algebraic) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    rumoca_core::InstanceId::new(19),
                    vector,
                    at,
                    local_source_variable_attributes(),
                )?,
                variables.algebraic(
                    VarName::new("y"),
                    rumoca_core::InstanceId::new(20),
                    vector,
                    at,
                    local_source_variable_attributes(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let lhs = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let rhs = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous().residual().nodes.as_slice() else {
        panic!("one compact algebraic residual block expected");
    };
    assert_eq!(rows.output_indices(), [0, 1]);
    assert_eq!(rows.stored_output_count_for_program(0), Some(2));
    let [program] = rows.programs() else {
        panic!("the aggregate residual owner must be lowered exactly once");
    };
    assert!(matches!(
        program.as_slice(),
        [
            LinearOp::TensorLoad {
                dst_start: 0,
                input: rumoca_ir_solve::TensorInputKind::Y,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: 2,
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorBinary {
                dst_start: 4,
                op: rumoca_ir_solve::BinaryOp::Sub,
                lhs_start: 0,
                rhs_start: 2,
                count: 2,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 4,
                count: 2,
                stride: 1,
            },
        ]
    ));
}

#[test]
fn nested_comprehension_binders_lower_through_lexical_domain_scopes() {
    let source = TestSource::new("Real x[2,3]; equation x = {{i + j for j in 1:3} for i in 1:2};");
    let declaration = source.at(0, 11);
    let owner = source.at(22, 61);
    let outer_range = source.at(57, 60);
    let inner_range = source.at(43, 46);
    let singleton_domain = |name: &str, upper| StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: name.to_string(),
            lower: 1,
            upper,
            step: 1,
        }],
    };
    let model = dae::Dae::construct(source.map, |model| {
        let real_array = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2, 3]),
                declaration,
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                rumoca_core::InstanceId::new(10),
                real_array,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let outer =
            model.domains(|domains| domains.structured(singleton_domain("i", 2), outer_range))?;
        let i = model.domains(|domains| domains.binder(outer, 0, owner))?;
        let inner = model.domains(|domains| {
            domains.nested_in_scope([i], singleton_domain("j", 3), inner_range)
        })?;
        let j = model.domains(|domains| domains.binder(inner, 0, owner))?;
        let residual = model.expressions(|expressions| {
            let x = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let i = expressions.at(owner).binder(i)?;
            let j = expressions.at(owner).binder(j)?;
            let sum = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Add, i, j)?;
            let inner = expressions.at(owner).comprehension(inner, sum)?;
            let nested = expressions.at(owner).comprehension(outer, inner)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, x, nested)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(solve.solve_layout().algebraic_scalar_count(), 6);
    assert_eq!(solve.continuous().residual().len().unwrap(), 6);
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous().residual().nodes.as_slice() else {
        panic!("one compact multi-output residual block expected");
    };
    assert_eq!(rows.row_count(), 1);
    assert_eq!(rows.stored_output_count(), 6);
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction matrix fixture enumerates the complete checked linear-solve owner"
)]
fn square_matrix_state_equation_lowers_to_one_checked_linear_solve() {
    let source = TestSource::new("Real omega[2]; Real J[2,2]; Real tau[2]; J * der(omega) = tau;");
    let state_at = source.at(0, 14);
    let matrix_at = source.at(16, 27);
    let rhs_at = source.at(29, 40);
    let owner = source.at(42, 62);
    let model = dae::Dae::construct(source.map, |model| {
        let (vector, matrix) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [2]),
                    state_at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [2, 2]),
                    matrix_at,
                )?,
            ))
        })?;
        let state_attributes = real_state_attributes(model, state_at, 0.0, true)?;
        let (state, matrix_parameter, rhs_parameter) = model.variables(|variables| {
            Ok((
                variables.state(
                    VarName::new("omega"),
                    rumoca_core::InstanceId::new(11),
                    vector,
                    state_at,
                    state_attributes,
                )?,
                variables.parameter(
                    VarName::new("J"),
                    rumoca_core::InstanceId::new(12),
                    matrix,
                    matrix_at,
                    dae::VariableAttributes::default(),
                )?,
                variables.parameter(
                    VarName::new("tau"),
                    rumoca_core::InstanceId::new(13),
                    vector,
                    rhs_at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let matrix = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(matrix_parameter))?;
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let product =
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Multiply, matrix, derivative)?;
            let rhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(rhs_parameter))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, product, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [
        ComputeNode::LinSolve {
            setup_ops,
            matrix_start,
            rhs_start,
            n,
            next_reg,
            span,
            ..
        },
    ] = solve.continuous().derivative_rhs().nodes.as_slice()
    else {
        panic!("one checked linear-solve node expected");
    };
    assert_eq!(*n, 2);
    assert_eq!(*span, owner.span());
    assert!(matrix_start < rhs_start);
    assert!(rhs_start < next_reg);
    assert_eq!(*matrix_start, 0);
    assert_eq!(*rhs_start, 4);
    assert_eq!(*next_reg, 6);
    assert!(matches!(
        setup_ops.as_slice(),
        [
            LinearOp::TensorLoad {
                dst_start: 0,
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start: 0,
                count: 4,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: 4,
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start: 4,
                count: 2,
                seed_start: None,
                lanes: 1,
            }
        ]
    ));
    assert!(
        setup_ops
            .iter()
            .all(|op| !matches!(op, LinearOp::Move { .. })),
        "the checked linear solve consumes two compact tensor loads without scalar Move repacking"
    );

    let rows = rumoca_eval_solve::to_scalar_program_block(solve.continuous().derivative_rhs())
        .expect("checked linear solve has a scalar execution view");
    let mut derivative = [0.0; 2];
    rumoca_eval_solve::eval_scalar_program_block(
        &rows,
        &[],
        &[2.0, 0.0, 0.0, 4.0, 6.0, 8.0],
        0.0,
        None,
        &mut derivative,
    )
    .expect("checked linear solve evaluates");
    assert_eq!(derivative, [3.0, 2.0]);
}

#[test]
fn algebraic_residual_uses_checked_y_and_p_layouts() {
    let source = TestSource::new("parameter Real p = 2; Real y; y = p;");
    let parameter_at = source.at(0, 20);
    let algebraic_at = source.at(22, 28);
    let owner = source.at(30, 35);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                parameter_at,
            )
        })?;
        let two = model.expressions(|expressions| {
            expressions
                .at(parameter_at)
                .literal(dae::DaeLiteral::Real(2.0))
        })?;
        let (parameter, algebraic) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    rumoca_core::InstanceId::new(14),
                    real,
                    parameter_at,
                    dae::VariableAttributes {
                        binding: Some(two),
                        ..dae::VariableAttributes::default()
                    },
                )?,
                variables.algebraic(
                    VarName::new("y"),
                    rumoca_core::InstanceId::new(15),
                    real,
                    algebraic_at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let lhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let rhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert!(matches!(
        solve.layout().binding("y"),
        Some(ScalarSlot::Y { index: 0 })
    ));
    assert!(matches!(
        solve.layout().binding("p"),
        Some(ScalarSlot::P { index: 0 })
    ));
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous().residual().nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    assert_eq!(rows.output_indices(), [0]);
    assert!(
        rows.programs()[0]
            .iter()
            .any(|op| matches!(op, LinearOp::LoadY { index: 0, .. }))
    );
    assert!(
        rows.programs()[0]
            .iter()
            .any(|op| matches!(op, LinearOp::LoadP { index: 0, .. }))
    );
    let [ComputeNode::ScalarPrograms(implicit)] =
        solve.continuous().implicit_rhs().nodes.as_slice()
    else {
        panic!("the matched algebraic row must be executable by the runtime");
    };
    assert_eq!(implicit.output_indices(), [0]);
    assert_eq!(
        solve.continuous().implicit_row_targets(),
        [Some(ScalarSlot::Y { index: 0 })]
    );
    assert_eq!(
        solve.continuous().algebraic_projection_plan().blocks[0].rows,
        [0]
    );
    assert_eq!(
        solve.continuous().algebraic_projection_plan().blocks[0].y_indices,
        [0]
    );
}

#[test]
fn algebraic_projection_keeps_equation_rows_distinct_from_y_indices() {
    let source = TestSource::new("Real x; Real y; y = 1; der(x) = 0;");
    let declaration = source.at(0, 14);
    let algebraic_owner = source.at(16, 21);
    let derivative_owner = source.at(23, 33);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state_attributes = real_state_attributes(model, declaration, 0.0, true)?;
        let (state, algebraic) = model.variables(|variables| {
            Ok((
                variables.state(
                    VarName::new("x"),
                    rumoca_core::InstanceId::new(16),
                    real,
                    declaration,
                    state_attributes,
                )?,
                variables.algebraic(
                    VarName::new("y"),
                    rumoca_core::InstanceId::new(17),
                    real,
                    declaration,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let algebraic_residual = model.expressions(|expressions| {
            let lhs = expressions
                .at(algebraic_owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let rhs = expressions
                .at(algebraic_owner)
                .literal(dae::DaeLiteral::Real(1.0))?;
            expressions
                .at(algebraic_owner)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(algebraic_owner, algebraic_residual)
        })?;
        let derivative_residual = model.expressions(|expressions| {
            let lhs = expressions
                .at(derivative_owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let rhs = expressions
                .at(derivative_owner)
                .literal(dae::DaeLiteral::Real(0.0))?;
            expressions
                .at(derivative_owner)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(derivative_owner, derivative_residual)
        })
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(implicit)] =
        solve.continuous().implicit_rhs().nodes.as_slice()
    else {
        panic!("the algebraic equation must remain executable");
    };
    assert_eq!(implicit.output_indices(), [0]);
    assert_eq!(
        solve.continuous().implicit_row_targets(),
        [Some(ScalarSlot::Y { index: 1 })]
    );
    assert_eq!(
        solve.continuous().algebraic_projection_plan().blocks[0].rows,
        [0]
    );
    assert_eq!(
        solve.continuous().algebraic_projection_plan().blocks[0].y_indices,
        [1]
    );
    reseal_solve_problem(&solve)
        .expect("projection row ordinals need not equal their assigned Y indices");
}

#[test]
fn implicit_derivative_form_fails_at_the_equation_span() {
    let source = TestSource::new("Real x; der(x) * der(x) = x;");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 28);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state_attributes = real_state_attributes(model, declaration, 0.0, true)?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(18),
                real,
                declaration,
                state_attributes,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let squared = expressions.at(owner).binary(
                dae::BinaryOperator::Multiply,
                derivative,
                derivative,
            )?;
            let state = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, squared, state)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let error = lower_solve_problem(&model).unwrap_err();
    assert!(matches!(
        error,
        LowerError::NonComputable { span, .. } if span == owner.span()
    ));
}
