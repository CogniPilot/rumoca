//! Source-to-session regression for atomic coupled input application (#348).

use super::*;

const INITIAL_INPUT_MODEL: &str = r#"
model InitialInputs
  input Real u[2](each start=99);
  input Real v = 17;
  output Real x;
  output Real y;
initial equation
  x = u[1] + u[2];
equation
  der(x) = v;
  y = x + u[1] - u[2];
end InitialInputs;
"#;

#[test]
fn initial_inputs_drive_initial_equations_and_reset_in_both_integrators() {
    let model = compile(INITIAL_INPUT_MODEL, "InitialInputs");
    for solver_mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                solver_mode,
                initial_inputs: vec![
                    ("u[1]".into(), 2.0),
                    ("u[2]".into(), 3.0),
                    ("v".into(), 4.0),
                ],
                ..SimOptions::default()
            },
        )
        .unwrap();
        let values = session.state().unwrap().values;
        assert!(
            (values["x"] - 5.0).abs() < 1e-8,
            "{solver_mode:?}: {values:?}"
        );
        assert!((values["y"] - 4.0).abs() < 1e-8);
        session.advance_to(0.25).unwrap();
        assert!((session.get("x").unwrap().unwrap() - 6.0).abs() < 1e-7);
        session.set_inputs(&[("u[1]", 20.0), ("v", -8.0)]).unwrap();
        session.reset(0.0).unwrap();
        let values = session.state().unwrap().values;
        assert_eq!(
            (values["u[1]"], values["u[2]"], values["v"]),
            (2.0, 3.0, 4.0)
        );
        assert!((values["x"] - 5.0).abs() < 1e-8);
        assert!((values["y"] - 4.0).abs() < 1e-8);
    }
}

#[test]
fn initial_inputs_reject_missing_scalars_invalid_names_and_nonfinite_values() {
    let model = compile(INITIAL_INPUT_MODEL, "InitialInputs");
    for (initial_inputs, expected) in [
        (vec![], "neither a checked default nor a runtime value"),
        (
            vec![("u[1]".into(), 2.0)],
            "neither a checked default nor a runtime value",
        ),
        (vec![("x".into(), 2.0)], "not an input"),
        (vec![("missing".into(), 2.0)], "not an input"),
        (vec![("u[1]".into(), f64::NAN)], "must be finite"),
    ] {
        let err = lower_dae_for_simulation(
            &model,
            &SimOptions {
                initial_inputs,
                ..SimOptions::default()
            },
        )
        .expect_err("invalid input provider must fail before construction");
        assert!(err.to_string().contains(expected), "{err}");
    }
}

#[test]
fn initial_input_reads_plan_only_state_unknowns_and_keep_consistency_checks() {
    let model = compile(INITIAL_INPUT_MODEL, "InitialInputs");
    let options = SimOptions {
        initial_inputs: vec![("u[1]".into(), 2.0), ("u[2]".into(), 3.0)],
        ..SimOptions::default()
    };
    let lowered = lower_dae_for_simulation(&model, &options).unwrap();
    assert_eq!(
        lowered.problem.initialization.projection_unknowns,
        [lowered.problem.layout.binding("x").unwrap()]
    );
    let inconsistent = compile(
        &INITIAL_INPUT_MODEL.replace("x = u[1] + u[2];", "x = u[1] + u[2]; x = 10;"),
        "InitialInputs",
    );
    let error = SimulationSession::new(&inconsistent, options)
        .err()
        .expect("input-dependent and constant initial rows must both hold");
    assert!(error.to_string().contains("residual"), "{error}");
}

#[test]
fn input_batches_preserve_atomicity_and_analytic_traces_in_both_integrators() {
    let model = compile(
        r#"
model CoupledInputs
  input Real u = 0;
  input Real v = 0;
  output Real x(start=1, fixed=true);
  output Real y;
equation
  der(x) = -x + u + v;
  y = x + 2*u - v;
end CoupledInputs;
"#,
        "CoupledInputs",
    );
    for solver_mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                solver_mode,
                t_end: 1.0,
                rtol: 1e-10,
                atol: 1e-11,
                ..SimOptions::default()
            },
        )
        .expect("both importers construct the same component");
        session.set_inputs(&[("u", 2.0), ("v", 3.0)]).unwrap();
        assert!(session.set_inputs(&[("u", 9.0), ("missing", 3.0)]).is_err());
        assert!(session.set_inputs(&[("v", 8.0), ("u", f64::NAN)]).is_err());
        for sample in 0..=10 {
            let t = f64::from(sample) / 10.0;
            session.set_inputs(&[("u", 2.0), ("v", 3.0)]).unwrap();
            session.advance_to(t).unwrap();
            let values = session.state().unwrap().values;
            let x = 5.0 - 4.0 * (-t).exp();
            assert_eq!((values["u"], values["v"]), (2.0, 3.0));
            assert!(
                (values["x"] - x).abs() < 1e-7,
                "{solver_mode:?} at {t}: {values:?}"
            );
            assert!(
                (values["y"] - (x + 1.0)).abs() < 1e-7,
                "{solver_mode:?} at {t}: {values:?}"
            );
        }
    }
}
