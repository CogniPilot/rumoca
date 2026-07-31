//! Affine-derivative and derivative-alias lowering.
//!
//! These cases fix the boundary between what Solve accepts and what it rejects
//! for a matched state derivative: which coefficient declarations it can prove
//! nonzero, which guards it may resolve at translation time, and when it can
//! recover a derivative its own row does not define.

use super::*;

/// The accepted affine-coefficient probe: literals, parameter bindings, the
/// arithmetic operators, and the pure elementary builtins. `Modelica.Constants.pi`
/// reaches DAE construction as `2*asin(1.0)`, so a coefficient scaled by it is
/// declared nonzero even though no literal names its value.
#[test]
fn builtin_valued_affine_coefficient_keeps_its_runtime_parameter_slot() {
    let source = TestSource::new("parameter Real p=2*asin(1.0); Real x; p*der(x)-x=0;");
    let parameter_at = source.at(0, 28);
    let state_at = source.at(30, 36);
    let owner = source.at(38, 50);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                parameter_at,
            )
        })?;
        let binding = model.expressions(|expressions| {
            let one = expressions
                .at(parameter_at)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let asin = expressions
                .at(parameter_at)
                .builtin(dae::PureBuiltin::Asin, [one])?;
            let two = expressions
                .at(parameter_at)
                .literal(dae::DaeLiteral::Real(2.0))?;
            expressions
                .at(parameter_at)
                .binary(dae::BinaryOperator::Multiply, two, asin)
        })?;
        let (parameter, state) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    real,
                    parameter_at,
                    dae::VariableAttributes {
                        binding: Some(binding),
                        is_tunable: true,
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
    .unwrap();

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
        "the folded declaration proves the coefficient nonzero without replacing its runtime slot"
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

/// `v = if quasiStatic then 0 else L*der(i)` is the MSL `InductorDC` shape. The
/// guard is an `Evaluate=true` parameter, which no simulation override can
/// change, so the equation has exactly one reachable form.
fn structural_inductor_model(guard_is_tunable: bool, guard_value: bool) -> dae::Dae {
    let source = TestSource::new(
        "parameter Boolean q=false; parameter Real p=2; Real x; x-(if q then 0 else p*der(x))=0;",
    );
    let guard_at = source.at(0, 25);
    let parameter_at = source.at(27, 45);
    let state_at = source.at(47, 53);
    let owner = source.at(55, 86);
    dae::Dae::construct(source.map, |model| {
        let (real, boolean) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    parameter_at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::scalar(dae::ScalarType::Boolean),
                    guard_at,
                )?,
            ))
        })?;
        let (guard_binding, binding) = model.expressions(|expressions| {
            Ok((
                expressions
                    .at(guard_at)
                    .literal(dae::DaeLiteral::Boolean(guard_value))?,
                expressions
                    .at(parameter_at)
                    .literal(dae::DaeLiteral::Real(2.0))?,
            ))
        })?;
        let (guard, parameter, state) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("q"),
                    boolean,
                    guard_at,
                    dae::VariableAttributes {
                        binding: Some(guard_binding),
                        is_tunable: guard_is_tunable,
                        ..dae::VariableAttributes::default()
                    },
                )?,
                variables.parameter(
                    VarName::new("p"),
                    real,
                    parameter_at,
                    dae::VariableAttributes {
                        binding: Some(binding),
                        is_tunable: true,
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
            let condition = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(guard))?;
            let quasi_static = expressions.at(owner).literal(dae::DaeLiteral::Real(0.0))?;
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
            let branch = expressions
                .at(owner)
                .conditional([(condition, quasi_static)], scaled)?;
            let state = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, state, branch)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap()
}

#[test]
fn translation_time_guard_selects_its_affine_derivative_branch() {
    let solve = lower_solve_problem(&structural_inductor_model(false, false)).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("one scalar derivative block expected");
    };
    let program = &rows.programs()[0];
    assert!(
        program
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadP { .. })),
        "the selected branch keeps its runtime coefficient slot"
    );
    assert!(
        !program
            .iter()
            .any(|operation| matches!(operation, LinearOp::Select { .. })),
        "a guard fixed at translation time leaves no runtime selection: {program:?}"
    );
}

#[test]
fn tunable_guard_does_not_select_an_affine_derivative_branch() {
    let error = lower_solve_problem(&structural_inductor_model(true, false)).unwrap_err();
    assert!(
        matches!(
            &error,
            LowerError::NonComputable { reason, .. }
                if reason.contains("isolated affine product")
        ),
        "an overridable guard must not fold into program semantics: {error:?}"
    );
}

/// A translation-time guard that removes the derivative altogether leaves the
/// equation unable to define it. The structural incidence still admits the
/// derivative in either branch, so the row reaches Solve matched to it and must
/// be rejected loudly rather than divided by the branch's zero coefficient.
#[test]
fn translation_time_guard_that_deletes_the_derivative_is_rejected() {
    let error = lower_solve_problem(&structural_inductor_model(false, true)).unwrap_err();
    assert!(
        matches!(
            &error,
            LowerError::NonComputable { reason, .. }
                if reason.contains("isolated affine product")
        ),
        "a branch without the derivative cannot define it: {error:?}"
    );
}

/// `HeatCapacitor` writes both `der_T = der(T)` and `C*der(T) = port.Q_flow`.
/// Only one row can be the derivative's definition; the other reads it, and
/// reading it recovers the defining right-hand side rather than a coordinate
/// with no Solve storage.
fn derivative_alias_model(read_from_discrete: bool) -> dae::Dae {
    let source = TestSource::new("Real x; Real a; discrete Real d; a=der(x); der(x)=-x; d=der(x);");
    let state_at = source.at(0, 6);
    let algebraic_at = source.at(8, 14);
    let discrete_at = source.at(16, 31);
    let alias_at = source.at(33, 40);
    let definition_at = source.at(42, 52);
    let discrete_owner = source.at(54, 62);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                state_at,
            )
        })?;
        let (state, algebraic, discrete) = model.variables(|variables| {
            Ok((
                variables.state(
                    VarName::new("x"),
                    real,
                    state_at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("a"),
                    real,
                    algebraic_at,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_real(
                    VarName::new("d"),
                    real,
                    discrete_at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (alias, definition) = model.expressions(|expressions| {
            let algebraic = expressions
                .at(alias_at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let derivative = expressions
                .at(alias_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let alias = expressions.at(alias_at).binary(
                dae::BinaryOperator::Subtract,
                algebraic,
                derivative,
            )?;
            let derivative = expressions
                .at(definition_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let value = expressions
                .at(definition_at)
                .coordinate(dae::CoordinateInput::State(state))?;
            let negated = expressions
                .at(definition_at)
                .unary(dae::UnaryOperator::Negate, value)?;
            let definition = expressions.at(definition_at).binary(
                dae::BinaryOperator::Subtract,
                derivative,
                negated,
            )?;
            Ok((alias, definition))
        })?;
        model.continuous(|continuous| continuous.value_equation(alias_at, alias))?;
        model.continuous(|continuous| continuous.value_equation(definition_at, definition))?;
        if read_from_discrete {
            let residual = model.expressions(|expressions| {
                let target = expressions
                    .at(discrete_owner)
                    .coordinate(dae::CoordinateInput::DiscreteReal(discrete))?;
                let derivative = expressions
                    .at(discrete_owner)
                    .coordinate(dae::CoordinateInput::Derivative(state))?;
                expressions.at(discrete_owner).binary(
                    dae::BinaryOperator::Subtract,
                    target,
                    derivative,
                )
            })?;
            model.discrete(|discrete| {
                discrete.real_equation(discrete_owner, |equation| equation.residual(residual))
            })?;
        }
        Ok(())
    })
    .unwrap()
}

#[test]
fn algebraic_row_reads_a_derivative_through_its_defining_equation() {
    let model = derivative_alias_model(false);
    let solve = lower_solve_problem(&model).unwrap();
    solve
        .validate()
        .expect("the derivative alias satisfies the Solve shape contract");
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!(
            "one algebraic residual block expected, got {:?}",
            solve.continuous.residual.nodes
        );
    };
    let program = &rows.programs()[0];
    assert!(
        program.iter().any(|operation| matches!(
            operation,
            LinearOp::Unary {
                op: rumoca_ir_solve::UnaryOp::Neg,
                ..
            }
        )),
        "the alias row must carry the defining right-hand side, not a derivative read: {program:?}"
    );
    assert_eq!(
        program
            .iter()
            .filter(|operation| matches!(operation, LinearOp::LoadY { .. }))
            .count(),
        2,
        "the alias reads its own algebraic and the state the definition names: {program:?}"
    );
}

#[test]
fn discrete_row_still_rejects_a_derivative_coordinate() {
    let model = derivative_alias_model(true);
    let error = lower_solve_problem(&model).unwrap_err();
    assert!(
        matches!(
            &error,
            LowerError::NonComputable { reason, .. }
                if reason.contains("escaped checked structural substitution")
        ),
        "only continuous algebraic and initial rows resolve a derivative: {error:?}"
    );
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
