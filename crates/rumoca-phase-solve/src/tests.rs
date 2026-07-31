use rumoca_core::{SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{ComputeNode, LinearOp, ScalarSlot};

use crate::{LowerError, lower_solve_problem};

mod affine_derivatives;

struct TestSource {
    map: SourceMap,
    source: rumoca_core::SourceId,
}

impl TestSource {
    fn new(text: &str) -> Self {
        let mut map = SourceMap::new();
        let source = map.add("solve.mo", text);
        Self { map, source }
    }

    fn at(&self, start: usize, end: usize) -> dae::DaeProvenance {
        dae::DaeProvenance::source(Span::from_offsets(self.source, start, end)).unwrap()
    }
}

fn scaled_state_model(source: TestSource, coefficient: f64) -> dae::Dae {
    let parameter_at = source.at(0, 18);
    let state_at = source.at(20, 26);
    let owner = source.at(28, 40);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                parameter_at,
            )
        })?;
        let binding = model.expressions(|expressions| {
            expressions
                .at(parameter_at)
                .literal(dae::DaeLiteral::Real(coefficient))
        })?;
        let (parameter, state) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    real,
                    parameter_at,
                    dae::VariableAttributes {
                        binding: Some(binding),
                        ..dae::VariableAttributes::default()
                    },
                )?,
                variables.state(
                    VarName::new("x"),
                    real,
                    state_at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let parameter = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let scaled = expressions.at(owner).binary(
                dae::BinaryOperator::Multiply,
                parameter,
                derivative,
            )?;
            let state = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, scaled, state)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap()
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
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
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
    assert_eq!(solve.solve_layout.state_scalar_count(), 1);
    assert!(solve.continuous.residual.nodes.is_empty());
    assert!(solve.continuous.implicit_rhs.nodes.is_empty());
    assert!(solve.continuous.implicit_row_targets.is_empty());
    assert!(solve.continuous.algebraic_projection_plan.is_empty());
    solve
        .validate()
        .expect("lowered explicit state system satisfies the Solve shape contract");
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.derivative_rhs.nodes.as_slice()
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
fn affine_state_equation_preserves_its_runtime_parameter_coefficient() {
    let source = TestSource::new("parameter Real p=2; Real x; p*der(x)-x=0;");
    let model = scaled_state_model(source, 2.0);

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("one scalar derivative block expected");
    };
    let program = &rows.programs()[0];
    assert!(
        program
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadP { index: 0, .. })),
        "the coefficient must use the runtime parameter slot, not its declaration value"
    );
    assert!(
        program.iter().any(|operation| matches!(
            operation,
            LinearOp::Binary {
                op: rumoca_ir_solve::BinaryOp::Div,
                ..
            }
        )),
        "the proven affine coefficient must explicitly isolate the derivative"
    );
}

#[test]
fn zero_affine_derivative_coefficient_fails_before_runtime() {
    let source = TestSource::new("parameter Real p=0; Real x; p*der(x)-x=0;");
    let model = scaled_state_model(source, 0.0);

    let error = lower_solve_problem(&model).unwrap_err();
    assert!(matches!(
        error,
        LowerError::NonComputable { reason, .. }
            if reason.contains("zero affine coefficient")
    ));
}

#[test]
fn explicit_string_format_fails_at_its_exact_solve_lowering_span() {
    const STRING_DECLARATION: rumoca_core::DefId = rumoca_core::DefId(41);
    let source = TestSource::new(
        "Real x; equation x = 0; when true then \
         assert(true, String(1, format = \"04d\")); end when;",
    );
    let declaration_at = source.at(0, 6);
    let equation_at = source.at(17, 22);
    let condition_at = source.at(29, 33);
    let action_at = source.at(39, 78);
    let conversion_at = source.at(52, 77);
    let format_at = source.at(71, 76);
    let model = dae::Dae::construct(source.map, |model| {
        model.register_predefined_string(STRING_DECLARATION)?;
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration_at,
            )
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let variable = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let zero = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(0.0))?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, variable, zero)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))?;
        let condition = model.conditions(|conditions| conditions.reserve(condition_at))?;
        let condition_value = model.expressions(|expressions| {
            expressions
                .at(condition_at)
                .literal(dae::DaeLiteral::Boolean(true))
        })?;
        model.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Discrete(condition_value),
                condition_at,
            )
        })?;
        let message = model.expressions(|expressions| {
            let value = expressions
                .at(conversion_at)
                .literal(dae::DaeLiteral::Integer(1))?;
            let format = expressions
                .at(format_at)
                .literal(dae::DaeLiteral::String("04d".to_owned()))?;
            expressions.at(conversion_at).string_conversion(
                STRING_DECLARATION,
                value,
                dae::StringConversionFormatInput::Format { value: format },
            )
        })?;
        model.events(|events| events.assert(condition, condition, message, action_at))?;
        Ok(())
    })
    .expect("explicit formatting is valid checked DAE semantics");

    let error =
        lower_solve_problem(&model).expect_err("Solve IR cannot claim an uncomputable formatter");

    assert!(
        matches!(
            error,
            LowerError::Unsupported { ref reason, span }
                if span == format_at.span() && reason.contains("explicit String format")
        ),
        "unexpected lowering error: {error:?}"
    );
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
            id: 0,
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
    assert_eq!(solve.solve_layout.algebraic_scalar_count(), 6);
    assert_eq!(solve.continuous.residual.len().unwrap(), 6);
}

#[test]
fn primitive_relation_root_lowers_to_signed_event_program() {
    let source = TestSource::new("Real x; der(x) = -1; when x > 0 then end when;");
    let declaration = source.at(0, 6);
    let equation_owner = source.at(8, 19);
    let relation_owner = source.at(26, 31);
    let when_owner = source.at(21, 45);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (residual, relation_expression) = model.expressions(|expressions| {
            let derivative = expressions
                .at(equation_owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let negative_one = expressions
                .at(equation_owner)
                .literal(dae::DaeLiteral::Real(-1.0))?;
            let residual = expressions.at(equation_owner).binary(
                dae::BinaryOperator::Subtract,
                derivative,
                negative_one,
            )?;
            let state = expressions
                .at(relation_owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            let zero = expressions
                .at(relation_owner)
                .literal(dae::DaeLiteral::Real(0.0))?;
            let relation =
                expressions
                    .at(relation_owner)
                    .binary(dae::BinaryOperator::Greater, state, zero)?;
            Ok((residual, relation))
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_owner, residual))?;
        let (relation, activation) = model.conditions(|conditions| {
            let relation = conditions.relation(relation_expression, relation_owner)?;
            let activation = conditions.reserve(when_owner)?;
            conditions.define(
                activation,
                dae::ConditionInput::Relation(relation),
                relation_owner,
            )?;
            Ok((relation, activation))
        })?;
        model.conditions(|conditions| conditions.root(relation, activation, when_owner))?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(solve.events.root_conditions.programs().len(), 1);
    assert_eq!(
        solve.events.root_conditions.program_spans(),
        [when_owner.span()]
    );
    assert_eq!(
        solve.events.root_zero_domains,
        [rumoca_ir_solve::RootZeroDomain::Positive]
    );
    assert!(solve.events.root_relation_memory_targets[0].is_none());
}

#[test]
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
        let (state, matrix_parameter, rhs_parameter) = model.variables(|variables| {
            Ok((
                variables.state(
                    VarName::new("omega"),
                    vector,
                    state_at,
                    dae::VariableAttributes::default(),
                )?,
                variables.parameter(
                    VarName::new("J"),
                    matrix,
                    matrix_at,
                    dae::VariableAttributes::default(),
                )?,
                variables.parameter(
                    VarName::new("tau"),
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
    ] = solve.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("one checked linear-solve node expected");
    };
    assert_eq!(*n, 2);
    assert_eq!(*span, owner.span());
    assert!(matrix_start < rhs_start);
    assert!(rhs_start < next_reg);
    assert_eq!(
        setup_ops
            .iter()
            .filter(|op| matches!(op, LinearOp::Move { .. }))
            .count(),
        6,
        "four matrix and two RHS values must be packed explicitly"
    );
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
                    real,
                    parameter_at,
                    dae::VariableAttributes {
                        binding: Some(two),
                        ..dae::VariableAttributes::default()
                    },
                )?,
                variables.algebraic(
                    VarName::new("y"),
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
        solve.layout.binding("y"),
        Some(ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        solve.layout.binding("p"),
        Some(ScalarSlot::P { index: 0, .. })
    ));
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
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
    let [ComputeNode::ScalarPrograms(implicit)] = solve.continuous.implicit_rhs.nodes.as_slice()
    else {
        panic!("the matched algebraic row must be executable by the runtime");
    };
    assert_eq!(implicit.output_indices(), [0]);
    assert_eq!(
        solve.continuous.implicit_row_targets,
        [Some(ScalarSlot::Y {
            index: 0,
            byte_offset: 0,
        })]
    );
    assert_eq!(
        solve.continuous.algebraic_projection_plan.blocks[0].rows,
        [0]
    );
    assert_eq!(
        solve.continuous.algebraic_projection_plan.blocks[0].y_indices,
        [0]
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
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
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

#[test]
fn unconditional_discrete_real_definition_does_not_require_a_clock() {
    let source = TestSource::new("discrete Real d; d=time;");
    let declaration = source.at(0, 15);
    let owner = source.at(17, 23);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("d"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let time = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, time)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(
        solve.discrete.row_roles,
        [rumoca_ir_solve::DiscreteRowRole::Equation]
    );
    assert_eq!(solve.discrete.clock_owners, [None]);
}

#[test]
fn clocked_discrete_definition_lowers_with_exact_row_owner() {
    let source = TestSource::new("Real d; Clock c=Clock(0.1); d=sample(time);");
    let declaration = source.at(0, 6);
    let clock_at = source.at(16, 26);
    let owner = source.at(28, 42);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("d"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), variable, owner))?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let time = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, time)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(solve.clocks.periodic_event_schedules.len(), 1);
    assert_eq!(solve.clocks.periodic_event_schedules[0].lattice(), lattice);
    assert_eq!(solve.discrete.clock_owners.len(), 1);
    let clock = solve.discrete.clock_owners[0].expect("row has a clock owner");
    assert_eq!(
        solve
            .clocks
            .periodic_schedule(clock)
            .expect("typed clock owner resolves")
            .lattice(),
        lattice
    );
    assert!(
        solve.discrete.rhs.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadTime { .. }))
    );
}

/// Builds `a = <time>` plus `rows` further discrete `Real` residuals over the
/// pair `(a, b)`, so a test can state exactly which rows the partition owns.
fn discrete_real_pair_model(
    source: TestSource,
    couple_rows: usize,
    seed_first_row: bool,
) -> dae::Dae {
    let declaration = source.at(0, 6);
    let owner = source.at(8, 20);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let first = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("a"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let second = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("b"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        if seed_first_row {
            let seeded = model.expressions(|expressions| {
                let target = expressions
                    .at(owner)
                    .coordinate(dae::CoordinateInput::DiscreteReal(first))?;
                let time = expressions
                    .at(owner)
                    .coordinate(dae::CoordinateInput::Time)?;
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Subtract, target, time)
            })?;
            model.discrete(|discrete| {
                discrete.real_equation(owner, |equation| equation.residual(seeded))
            })?;
        }
        for _ in 0..couple_rows {
            let coupled = model.expressions(|expressions| {
                let lhs = expressions
                    .at(owner)
                    .coordinate(dae::CoordinateInput::DiscreteReal(first))?;
                let rhs = expressions
                    .at(owner)
                    .coordinate(dae::CoordinateInput::DiscreteReal(second))?;
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Subtract, lhs, rhs)
            })?;
            model.discrete(|discrete| {
                discrete.real_equation(owner, |equation| equation.residual(coupled))
            })?;
        }
        Ok(())
    })
    .unwrap()
}

/// A connection row between two clocked coordinates states only that they are
/// equal, so its causality comes from the rest of the partition: `a` is already
/// defined by `a = time`, which leaves `b` as the row's only target.
#[test]
fn coupled_discrete_real_row_is_oriented_by_the_coordinate_left_undefined() {
    let source = TestSource::new("Real a; a=time; a=b;");
    let model = discrete_real_pair_model(source, 1, true);

    let solve = lower_solve_problem(&model).unwrap();
    let first = solve.layout.binding("a").expect("`a` owns a runtime slot");
    let second = solve.layout.binding("b").expect("`b` owns a runtime slot");
    assert_ne!(first, second);
    assert_eq!(solve.discrete.update_targets, [first, second]);
}

/// Two identical connection rows leave both coordinates undefined, so neither
/// row is ever forced. The partition admits more than one causality and is
/// reported instead of guessed.
#[test]
fn ambiguous_coupled_discrete_real_rows_are_reported_before_runtime() {
    let source = TestSource::new("Real a; Real b; a=b; a=b;");
    let model = discrete_real_pair_model(source, 2, false);

    let error = lower_solve_problem(&model).unwrap_err();
    assert!(
        matches!(
            &error,
            LowerError::NonComputable { reason, .. }
                if reason.contains("coupled discrete Real residual")
        ),
        "unforced discrete Real rows must be reported: {error}"
    );
}

#[test]
fn pre_discrete_value_loads_a_distinct_bound_history_lane() {
    let source = TestSource::new("discrete Integer count; count = pre(count);");
    let declaration = source.at(0, 22);
    let owner = source.at(24, 42);
    let model = dae::Dae::construct(source.map, |model| {
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                declaration,
            )
        })?;
        let count = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("count"),
                integer,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let previous = model.expressions(|expressions| {
            expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::PreDiscreteValue(count))
        })?;
        model.b1c([count], |topology| {
            topology.owner(owner, [count], |owner_scope| {
                owner_scope.always(owner, [(previous, owner)])
            })?;
            Ok(())
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert!(matches!(
        solve.layout.binding("count"),
        Some(ScalarSlot::P { index: 0, .. })
    ));
    assert_eq!(solve.solve_layout.compiled_parameter_len, 2);
    let [binding] = solve.solve_layout.pre_param_bindings.as_slice() else {
        panic!("one exact pre-history binding expected");
    };
    assert_eq!(binding.dest_p_index, 1);
    assert!(matches!(
        binding.source,
        rumoca_ir_solve::PreParamSource::P { index: 0 }
    ));
    assert!(binding.clock_schedule.is_none());
    assert_eq!(
        solve.discrete.pre_modes,
        [rumoca_ir_solve::DiscreteEventPreMode::EventEntry]
    );
    assert!(
        solve.discrete.rhs.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadP { index: 1, .. }))
    );
}

#[test]
fn periodic_clock_interval_lowers_to_an_exact_constant() {
    let source = TestSource::new("Real x; Clock c=Clock(0.1); x=interval();");
    let declaration = source.at(0, 6);
    let clock_at = source.at(16, 26);
    let owner = source.at(28, 41);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(variable))?;
            let interval = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::ClockInterval(clock))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, interval)
        })?;
        model.continuous(|equations| {
            equations.equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    assert!(
        rows.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::Const { value, .. } if *value == 0.1))
    );
}

#[test]
fn previous_loads_history_owned_by_its_exact_clock_schedule() {
    let source = TestSource::new(
        "discrete Real x; Clock c=Clock(0.1); when c then x=previous(x)+1; end when;",
    );
    let declaration = source.at(0, 15);
    let clock_at = source.at(25, 35);
    let condition_at = source.at(42, 43);
    let owner = source.at(49, 64);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), variable, owner))?;
        let condition = model.conditions(|conditions| conditions.reserve(condition_at))?;
        model.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Clock(clock.into()),
                condition_at,
            )
        })?;
        let previous = model
            .temporal(|temporal| temporal.previous_discrete_real(clock.into(), variable, owner))?;
        let residual = model.expressions(|expressions| {
            let previous = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Previous(previous))?;
            let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
            let value = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Add, previous, one)?;
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        model.discrete(|discrete| {
            discrete.when_real_equation(condition, condition, owner, |equation| {
                equation.residual(residual)
            })
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ordinary_pre, previous] = solve.solve_layout.pre_param_bindings.as_slice() else {
        panic!("ordinary pre and clock-owned previous history bindings expected");
    };
    assert!(ordinary_pre.clock_schedule.is_none());
    assert_eq!(
        previous
            .clock_schedule
            .as_ref()
            .expect("previous has its owning periodic schedule")
            .lattice(),
        lattice
    );
    assert!(
        solve.discrete.rhs.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadP { index: 2, .. }))
    );
}

#[test]
fn nonlinear_conditional_discrete_residual_fails_before_runtime() {
    let source = TestSource::new(
        "discrete Real z; Clock c=Clock(0.1); when c then z=3*pre(z)-z^2; end when;",
    );
    let declaration = source.at(0, 15);
    let clock_at = source.at(25, 35);
    let condition_at = source.at(44, 45);
    let owner = source.at(51, 65);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("z"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), variable, owner))?;
        let condition = model.conditions(|conditions| conditions.reserve(condition_at))?;
        model.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Clock(clock.into()),
                condition_at,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let previous = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::PreDiscreteReal(variable))?;
            let three = expressions.at(owner).literal(dae::DaeLiteral::Real(3.0))?;
            let scaled_previous =
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Multiply, three, previous)?;
            let two = expressions.at(owner).literal(dae::DaeLiteral::Real(2.0))?;
            let squared = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Power, target, two)?;
            let value = expressions.at(owner).binary(
                dae::BinaryOperator::Subtract,
                scaled_previous,
                squared,
            )?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        model.discrete(|discrete| {
            discrete.when_real_equation(condition, condition, owner, |equation| {
                equation.residual(residual)
            })
        })?;
        Ok(())
    })
    .unwrap();

    let error = lower_solve_problem(&model).unwrap_err();
    assert!(matches!(
        error,
        LowerError::NonComputable { span, .. } if span == owner.span()
    ));
}

#[test]
fn initial_condition_owns_a_dedicated_runtime_flag() {
    let source = TestSource::new(
        "discrete Real x; when initial() then x = 1; elsewhen false then x = 2; end when;",
    );
    let declaration = source.at(0, 15);
    let initial_at = source.at(22, 31);
    let assignment = source.at(37, 42);
    let false_at = source.at(53, 58);
    let second_assignment = source.at(64, 69);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let condition = model.conditions(|conditions| conditions.reserve(initial_at))?;
        model.conditions(|conditions| {
            conditions.define(condition, dae::ConditionInput::Initial, initial_at)
        })?;
        let false_value = model.expressions(|expressions| {
            expressions
                .at(false_at)
                .literal(dae::DaeLiteral::Boolean(false))
        })?;
        let otherwise = model.conditions(|conditions| conditions.reserve(false_at))?;
        model.conditions(|conditions| {
            conditions.define(
                otherwise,
                dae::ConditionInput::Discrete(false_value),
                false_at,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let value = expressions
                .at(assignment)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let target = expressions
                .at(assignment)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            expressions
                .at(assignment)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        let second_residual = model.expressions(|expressions| {
            let value = expressions
                .at(second_assignment)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let target = expressions
                .at(second_assignment)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            expressions
                .at(second_assignment)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        model.discrete(|discrete| {
            discrete.when_real_equation(condition, condition, assignment, |equation| {
                equation.residual(residual)
            })?;
            discrete.when_real_equation(otherwise, otherwise, second_assignment, |equation| {
                equation.residual(second_residual)
            })?;
            Ok(())
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_grouped_initial_b1b(&solve);
}

fn assert_grouped_initial_b1b(solve: &rumoca_ir_solve::SolveProblem) {
    let flag = solve
        .solve_layout
        .initial_event_parameter_index
        .expect("initial() owns one checked runtime flag");
    let guarded_rows = solve
        .discrete
        .row_roles
        .iter()
        .enumerate()
        .filter(|(_, role)| **role == rumoca_ir_solve::DiscreteRowRole::Equation)
        .collect::<Vec<_>>();
    let [(guarded_row, _)] = guarded_rows.as_slice() else {
        panic!("one conditional B.1b equation row expected");
    };
    assert_eq!(
        solve.discrete.rhs.programs()[*guarded_row]
            .iter()
            .filter(|operation| matches!(operation, LinearOp::Select { .. }))
            .count(),
        2
    );
    assert!(
        solve
            .discrete
            .rhs
            .programs()
            .iter()
            .flatten()
            .any(|operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == flag))
    );
}

#[test]
fn homotopy_owns_a_dedicated_continuation_parameter() {
    let source = TestSource::new("Real x; der(x) = homotopy(x*x, x);");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 34);
    let homotopy_at = source.at(17, 33);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let x = expressions
                .at(homotopy_at)
                .coordinate(dae::CoordinateInput::State(state))?;
            let actual = expressions
                .at(homotopy_at)
                .binary(dae::BinaryOperator::Multiply, x, x)?;
            let homotopy = expressions
                .at(homotopy_at)
                .builtin(dae::PureBuiltin::Homotopy, [actual, x])?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, derivative, homotopy)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    model.inspect(|view| {
        let expression = (0..view.expression_count())
            .filter_map(|index| view.expression(view.expression_id(index)?))
            .find(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Builtin {
                        builtin: dae::PureBuiltin::Homotopy,
                        ..
                    }
                )
            })
            .expect("checked DAE retains the homotopy node");
        assert_eq!(
            view.source_text(expression.provenance()),
            Some("homotopy(x*x, x)")
        );
    });
    let solve = lower_solve_problem(&model).unwrap();
    let lambda = solve
        .solve_layout
        .initial_homotopy_parameter_index
        .expect("homotopy owns one checked continuation parameter");
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("one scalar derivative block expected");
    };
    assert!(
        rows.programs()[0].iter().any(
            |operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == lambda)
        )
    );
}

#[test]
fn delay_lowers_to_runtime_history_programs_and_a_typed_value_slot() {
    let source = TestSource::new("Real x; der(x) = delay(x, 0.5);");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 31);
    let derivative_at = source.at(8, 14);
    let delay_at = source.at(17, 30);
    let source_at = source.at(23, 24);
    let timing_at = source.at(26, 29);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (source, delay_time) = model.expressions(|expressions| {
            Ok((
                expressions
                    .at(source_at)
                    .coordinate(dae::CoordinateInput::State(state))?,
                expressions
                    .at(timing_at)
                    .literal(dae::DaeLiteral::Real(0.5))?,
            ))
        })?;
        let timing =
            model.temporal(|temporal| temporal.positive_parameter(delay_time, 0.5, timing_at))?;
        let delay = model
            .expressions(|expressions| expressions.at(delay_at).delay(source, timing, delay_at))?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(derivative_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            expressions.at(owner).binary(
                dae::BinaryOperator::Subtract,
                derivative,
                delay.expression(),
            )
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    model.inspect(|view| {
        let delay = view
            .delay(view.delay_id(0).expect("dense delay identity"))
            .expect("checked delay owner resolves");
        assert_eq!(view.source_text(delay.provenance()), Some("delay(x, 0.5)"));
        let dae::DelayOperation::ParameterDelay { delay_time } = delay.operation() else {
            panic!("fixed delay has parameter timing evidence");
        };
        assert_eq!(view.source_text(delay_time.provenance()), Some("0.5"));
    });

    let solve = lower_solve_problem(&model).unwrap();
    solve
        .validate()
        .expect("delay lowering produces a computable Solve problem");
    let delays = &solve.events.delays;
    assert_eq!(delays.value_parameter_indices.len(), 1);
    assert_eq!(delays.source_is_discrete, [false]);
    let delay_slot = delays.value_parameter_indices[0];
    assert!(matches!(
        delays.source_rhs.programs()[0][0],
        LinearOp::LoadY { index: 0, .. }
    ));
    assert!(matches!(
        delays.delay_time_rhs.programs()[0][0],
        LinearOp::Const { value: 0.5, .. }
    ));
    assert!(matches!(
        delays.delay_max_rhs.programs()[0][0],
        LinearOp::Const { value: 0.5, .. }
    ));
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("one scalar derivative block expected");
    };
    assert!(rows.programs()[0].iter().any(
        |operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == delay_slot)
    ));
}

/// An always-active B.1c owner inside a clocked partition.
///
/// MLS §16.5 activates every equation of a clocked partition on its clock's ticks, so
/// `counter = previous(counter) + 1` needs no `when` — the target simply carries a clock
/// ownership. Two properties follow, and both were lost for `Integer`/`Boolean` targets:
/// the row must be compiled under its owning clock (or `previous(...)` cannot resolve),
/// and its relation must not become a continuous root, because MLS §16.8.1 raises no
/// state event for a clocked relation — the tick already is the event.
///
/// This is the shape of `Modelica.Clocked.RealSignals.TickBasedSources.Ramp.counter`.
fn clocked_tick_counter_model(lattice: rumoca_core::ClockLattice) -> dae::Dae {
    let source = TestSource::new(
        "discrete Integer counter; Clock c=Clock(0.1); counter=previous(counter)+1;",
    );
    let declaration = source.at(0, 24);
    let clock_at = source.at(34, 44);
    let relation_at = source.at(54, 71);
    let owner = source.at(46, 73);
    dae::Dae::construct(source.map, |model| {
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                declaration,
            )
        })?;
        let counter = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("counter"),
                integer,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_value(clock.into(), counter, owner))?;
        let previous = model
            .temporal(|temporal| temporal.previous_discrete_value(clock.into(), counter, owner))?;
        // The saturation test `previous(counter) < 5` that the source block writes inside
        // its `if`: a relation of the clocked partition, registered as a root by the
        // checked DAE exactly as a continuous-time relation would be.
        let guard = model.expressions(|expressions| {
            let previous = expressions
                .at(relation_at)
                .coordinate(dae::CoordinateInput::Previous(previous))?;
            let limit = expressions
                .at(relation_at)
                .literal(dae::DaeLiteral::Integer(5))?;
            expressions
                .at(relation_at)
                .binary(dae::BinaryOperator::Less, previous, limit)
        })?;
        let relation = model.conditions(|conditions| conditions.relation(guard, relation_at))?;
        let condition = model.conditions(|conditions| conditions.reserve(relation_at))?;
        model.conditions(|conditions| {
            conditions.define(
                condition,
                dae::ConditionInput::Relation(relation),
                relation_at,
            )
        })?;
        model.conditions(|conditions| conditions.root(relation, condition, relation_at))?;
        let value = model.expressions(|expressions| {
            let previous = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Previous(previous))?;
            let one = expressions.at(owner).literal(dae::DaeLiteral::Integer(1))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Add, previous, one)
        })?;
        model.b1c([counter], |topology| {
            topology.owner(owner, [counter], |owner_scope| {
                owner_scope.always(owner, [(value, owner)])
            })?;
            Ok(())
        })?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn clocked_unconditional_discrete_value_owner_reads_its_previous_history() {
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = clocked_tick_counter_model(lattice);

    let solve = lower_solve_problem(&model).unwrap();
    solve
        .validate()
        .expect("the clocked B.1c owner satisfies the Solve shape contract");
    let schedule = solve
        .discrete
        .clock_owners
        .iter()
        .zip(&solve.discrete.row_roles)
        .find(|(_, role)| **role == rumoca_ir_solve::DiscreteRowRole::Equation)
        .and_then(|(clock, _)| *clock)
        .expect("the always-active clocked owner keeps its periodic row schedule");
    assert_eq!(
        solve
            .clocks
            .periodic_schedule(schedule)
            .expect("typed clock owner resolves")
            .lattice(),
        lattice
    );
}

/// MLS §16.8.1 raises no state event for a relation of a clocked partition — the clock's
/// tick already is the event, and the relation's `previous(...)` operand only resolves
/// while that schedule is active. So the relation must leave the continuous root set and
/// its condition memory must be scheduled on the owning clock instead.
#[test]
fn clocked_relation_leaves_the_continuous_root_set() {
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = clocked_tick_counter_model(lattice);

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(
        solve.events.root_conditions.len(),
        0,
        "a clocked relation is evaluated on its tick, not by continuous root finding"
    );
    assert_eq!(solve.events.root_zero_domains.len(), 0);
    assert_eq!(solve.events.root_relation_memory_targets.len(), 0);
    let memory = solve
        .discrete
        .row_roles
        .iter()
        .position(|role| *role == rumoca_ir_solve::DiscreteRowRole::ConditionMemory)
        .expect("the clocked relation still keeps a condition-memory row");
    assert!(
        solve.discrete.clock_owners[memory].is_some(),
        "the condition-memory row of a clocked relation runs on its clock's ticks"
    );
}

/// MLS §8.6: a `parameter` declared `fixed = false` is determined by the initialization
/// system, so it must appear there as an unknown with its own projection block. Without
/// one the residual can only be checked, which is what stalled
/// `Modelica.Electrical.Analog.Examples.ShowSaturatingInductor` on
/// `SaturatingInductor.Ipar`.
#[test]
fn fixed_false_parameter_becomes_an_initialization_projection_unknown() {
    let source = TestSource::new("parameter Real q(fixed=false); initial equation q*q=4;");
    let declaration = source.at(0, 29);
    let owner = source.at(48, 53);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let start = model.expressions(|expressions| {
            expressions
                .at(declaration)
                .literal(dae::DaeLiteral::Real(3.0))
        })?;
        let unsolved = model.variables(|variables| {
            variables.parameter(
                VarName::new("q"),
                real,
                declaration,
                dae::VariableAttributes {
                    start: Some(start),
                    fixed: Some(false),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let residual = model.expressions(|expressions| {
            let left = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            let right = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            let square =
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Multiply, left, right)?;
            let four = expressions.at(owner).literal(dae::DaeLiteral::Real(4.0))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, square, four)
        })?;
        model.initialization(|initialization| initialization.value_equation(owner, residual))?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    solve
        .validate()
        .expect("the initialization projection satisfies the Solve shape contract");
    let ScalarSlot::P { index, .. } = solve
        .layout
        .binding("q")
        .expect("the unsolved parameter keeps its parameter storage")
    else {
        panic!("a parameter occupies P storage");
    };
    let [block] = solve.initialization.projection_plan.blocks.as_slice() else {
        panic!(
            "one initialization projection block expected, got {:?}",
            solve.initialization.projection_plan.blocks
        );
    };
    assert_eq!(block.rows, [0]);
    assert_eq!(block.unknowns, [rumoca_ir_solve::scalar_slot_p(index)]);
    assert_eq!(
        solve.initialization.projection_unknowns,
        [rumoca_ir_solve::scalar_slot_p(index)]
    );
}
