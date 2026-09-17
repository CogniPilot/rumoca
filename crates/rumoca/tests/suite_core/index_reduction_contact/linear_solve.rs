use rumoca_core::{SourceMap, Span, StateSelect, VarName};
use rumoca_ir_dae as dae;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

#[test]
fn a_varying_linear_solve_preserves_its_second_derivative() {
    for theta0 in [0.2_f64, 2.0] {
        let model = inverse_motion(theta0);
        for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
            check_inverse_motion(&model, theta0, solver_mode);
        }
    }
}

fn check_inverse_motion(model: &dae::Dae, theta0: f64, solver_mode: SimSolverMode) {
    let result = simulate_dae_with_diagnostics(
        model,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.01),
            solver_mode,
            ..Default::default()
        },
    )
    .unwrap();
    for (row, &time) in result.times.iter().enumerate() {
        let theta = theta0 + time;
        let inverse = 1.0 / (1.0 + theta);
        for (name, expected) in [
            ("theta", theta),
            ("x", inverse - 1.0),
            ("v", -inverse * inverse),
            ("a", 2.0 * inverse * inverse * inverse),
        ] {
            let column = result.names.iter().position(|value| value == name).unwrap();
            let actual = result.data[column][row];
            assert!(
                (actual - expected).abs() < 1e-6,
                "{solver_mode:?} {name}({time}): {actual} != {expected}"
            );
        }
    }
}

fn inverse_motion(theta0: f64) -> dae::Dae {
    let text = "der(theta)=1; der(x)=v; der(v)=a; x+1=solve([1+theta],{1})[1];";
    let mut sources = SourceMap::new();
    let source = sources.add("inverse_motion.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |model| {
        let real = model.types(|t| t.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let initial = model.expressions(|e| {
            let inverse = 1.0 / (1.0 + theta0);
            [theta0, inverse - 1.0, -inverse * inverse, 0.0]
                .map(|value| e.at(at).literal(dae::DaeLiteral::Real(value)))
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
        })?;
        let (theta, x, v, a) = model.variables(|variables| {
            let mut state = |name, start, fixed, selection| {
                variables.state(
                    VarName::new(name),
                    real,
                    at,
                    dae::VariableAttributes {
                        start: Some(start),
                        fixed: Some(vec![fixed]),
                        state_select: selection,
                        ..Default::default()
                    },
                )
            };
            Ok((
                state("theta", initial[0], true, StateSelect::Always)?,
                state("x", initial[1], false, StateSelect::Default)?,
                state("v", initial[2], false, StateSelect::Default)?,
                variables.algebraic(
                    VarName::new("a"),
                    real,
                    at,
                    dae::VariableAttributes {
                        start: Some(initial[3]),
                        fixed: Some(vec![false]),
                        ..Default::default()
                    },
                )?,
            ))
        })?;
        let residuals = model.expressions(|e| {
            let theta_value = e.at(at).coordinate(dae::CoordinateInput::State(theta))?;
            let x_value = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let v_value = e.at(at).coordinate(dae::CoordinateInput::State(v))?;
            let a_value = e.at(at).coordinate(dae::CoordinateInput::Algebraic(a))?;
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let coefficient = e
                .at(at)
                .binary(dae::BinaryOperator::Add, one, theta_value)?;
            let matrix_row = e.at(at).array([coefficient])?;
            let matrix = e.at(at).array([matrix_row])?;
            let rhs = e.at(at).array([one])?;
            let value = e
                .at(at)
                .builtin(dae::PureBuiltin::LinearSolve, [matrix, rhs])?;
            let index = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
            let value = e.at(at).index(
                value,
                [dae::Subscript::Index {
                    expression: index,
                    provenance: at,
                }],
            )?;
            let lhs = e.at(at).binary(dae::BinaryOperator::Add, x_value, one)?;
            let mut residuals = vec![e.at(at).binary(dae::BinaryOperator::Subtract, lhs, value)?];
            for (state, rhs) in [(theta, one), (x, v_value), (v, a_value)] {
                let derivative = e
                    .at(at)
                    .coordinate(dae::CoordinateInput::Derivative(state))?;
                residuals.push(
                    e.at(at)
                        .binary(dae::BinaryOperator::Subtract, derivative, rhs)?,
                );
            }
            Ok(residuals)
        })?;
        model.continuous(|continuous| {
            for residual in residuals {
                continuous.value_equation(at, residual)?;
            }
            Ok(())
        })
    })
    .unwrap()
}
