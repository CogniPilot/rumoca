//! Continuous-time residual and derivative program lowering.
//!
//! These cases fix the shape Solve produces for a matched continuous system:
//! which rows become scalar programs, which become one checked linear solve,
//! which storage lanes the layout binds them to, and which forms it refuses to
//! claim computable.

use super::*;

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
