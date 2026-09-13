use super::*;

fn repeated_derivative_model(operator: dae::BinaryOperator) -> dae::Dae {
    repeated_derivative_with_coefficient(operator, false)
}

fn repeated_derivative_with_coefficient(operator: dae::BinaryOperator, dynamic: bool) -> dae::Dae {
    let symbol = match operator {
        dae::BinaryOperator::Add => "+",
        dae::BinaryOperator::Subtract => "-",
        dae::BinaryOperator::Multiply => "*",
        _ => unreachable!(),
    };
    let coefficient = if dynamic { "x" } else { "p" };
    let text =
        format!("parameter Real p=1; Real x; ({coefficient}*der(x) {symbol} der(x))-1e20=0;");
    let source = TestSource::new(&text);
    let parameter_at = source.at(0, 18);
    let state_at = source.at(20, 26);
    let at = source.at(28, text.len());
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )
        })?;
        let one = model.expressions(|expressions| {
            expressions
                .at(parameter_at)
                .literal(dae::DaeLiteral::Real(1.0))
        })?;
        let (parameter, state) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    real,
                    parameter_at,
                    dae::VariableAttributes {
                        binding: Some(one),
                        is_tunable: true,
                        ..Default::default()
                    },
                )?,
                variables.state(VarName::new("x"), real, state_at, Default::default())?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let p = expressions.at(at).coordinate(if dynamic {
                dae::CoordinateInput::State(state)
            } else {
                dae::CoordinateInput::Parameter(parameter)
            })?;
            let dx = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let scaled = expressions
                .at(at)
                .binary(dae::BinaryOperator::Multiply, p, dx)?;
            let lhs = expressions.at(at).binary(operator, scaled, dx)?;
            let rhs = expressions.at(at).literal(dae::DaeLiteral::Real(1e20))?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap()
}

#[test]
fn repeated_derivatives_keep_symbolic_coefficients_and_live_parameter_values() {
    let model = repeated_derivative_model(dae::BinaryOperator::Add);
    let solve = lower_solve_problem(&model).expect("a repeated derivative remains affine");
    let program =
        rumoca_eval_solve::to_scalar_program_block(&solve.continuous.derivative_rhs).unwrap();
    for parameter in [1.0, 3.0, 9.0] {
        let mut parameters = vec![0.0; solve.solve_layout.compiled_parameter_len];
        parameters[0] = parameter;
        let mut output = [f64::NAN];
        rumoca_eval_solve::eval_scalar_program_block(
            &program,
            &[0.0],
            &parameters,
            0.0,
            None,
            &mut output,
        )
        .unwrap();
        assert_eq!(output[0], 1e20 / (parameter + 1.0));
    }
}

#[test]
fn cancellation_cannot_turn_a_zero_derivative_coefficient_into_an_ode() {
    let error =
        lower_solve_problem(&repeated_derivative_model(dae::BinaryOperator::Subtract)).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("zero or non-finite affine coefficient"),
        "{error}"
    );
}

#[test]
fn a_product_of_derivative_occurrences_is_not_affine() {
    let error =
        lower_solve_problem(&repeated_derivative_model(dae::BinaryOperator::Multiply)).unwrap_err();
    assert!(error.to_string().contains("derivative"), "{error}");
}

#[test]
fn state_dependent_affine_coefficients_are_live_and_reject_invalid_values() {
    let model = repeated_derivative_with_coefficient(dae::BinaryOperator::Add, true);
    let solve = lower_solve_problem(&model).unwrap();
    let program =
        rumoca_eval_solve::to_scalar_program_block(&solve.continuous.derivative_rhs).unwrap();
    let parameters = vec![1.0; solve.solve_layout.compiled_parameter_len];
    for state in [0.0, 1.0, 3.0, 9.0, -2.0] {
        let mut output = [f64::NAN];
        rumoca_eval_solve::eval_scalar_program_block(
            &program,
            &[state],
            &parameters,
            0.0,
            None,
            &mut output,
        )
        .unwrap();
        assert_eq!(output[0], 1e20 / (state + 1.0));
    }
    for state in [-1.0, f64::INFINITY, f64::NEG_INFINITY, f64::NAN] {
        let mut output = [0.0];
        let evaluated = rumoca_eval_solve::eval_scalar_program_block(
            &program,
            &[state],
            &parameters,
            0.0,
            None,
            &mut output,
        );
        assert!(
            evaluated.is_err() || !output[0].is_finite(),
            "invalid coefficient at {state} returned {}",
            output[0]
        );
    }
}
