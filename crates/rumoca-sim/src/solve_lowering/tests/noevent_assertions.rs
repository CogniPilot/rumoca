//! An error-level assert whose condition changes inside `noEvent` owns no
//! root, yet it holds during continuous integration (MLS §8.4, §8.5), so it is
//! checked at each accepted integrator step and output point (issue #593).

use super::*;

/// Violated only for 0.42 < t < 0.48, between the output points 0.4 and 0.5.
const TRANSIENT: &str = concat!(
    "model Transient\n",
    "  Real x(start = 0, fixed = true);\n",
    "equation\n",
    "  der(x) = sin(10*time);\n",
    "  assert(noEvent(time < 0.42 or time > 0.48 or x > 10), \"transient violation\");\n",
    "end Transient;\n",
);

const SCHEDULED_ASSERTION: &str = r#"model ScheduledAssertion
  Real x(start = 0, fixed = true);
  Boolean armed(start = false, fixed = true);
equation
  der(x) = 1;
  when sample(0.5, 1.0) then
    armed = true;
  end when;
  assert(noEvent(time < 0.5) or armed, "scheduled update must settle before checking");
end ScheduledAssertion;
"#;

#[cfg(all(feature = "solver-diffsol", feature = "solver-rk45"))]
#[test]
fn a_scheduled_update_settles_before_the_accepted_point_assertion() {
    let dae = compile(SCHEDULED_ASSERTION, "ScheduledAssertion");
    let options = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    for points in [
        &[(1.0, 1.0)][..],
        &[(0.49, 0.0), (0.5, 1.0), (1.0, 1.0)][..],
    ] {
        let mut session = SimulationSession::new(&dae, options.clone()).expect("initialize");
        for &(time, armed) in points {
            session
                .advance_to(time)
                .expect("advance through scheduled event");
            assert_eq!(
                session.get("armed").expect("read settled value"),
                Some(armed)
            );
        }
    }
}

#[test]
fn an_assertion_that_fails_after_a_scheduled_update_is_reported_at_the_event() {
    let source = SCHEDULED_ASSERTION.replace("or armed", "or not armed");
    let dae = compile(&source, "ScheduledAssertion");
    let options = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let error = simulate_dae(&dae, &options).expect_err("the settled assertion must fail");
    let time = failure_time(&error, "scheduled update must settle before checking");
    assert_eq!(time.to_bits(), 0.5_f64.to_bits());
}

fn failure_time(error: &crate::SimError, message: &str) -> f64 {
    let error = error.to_string();
    let detail = error
        .strip_prefix("Modelica assert failed at t=")
        .unwrap_or_else(|| panic!("expected an assertion failure, got {error}"));
    let (time, reported) = detail
        .split_once(": ")
        .unwrap_or_else(|| panic!("malformed assertion failure {error}"));
    assert_eq!(reported, message);
    time.parse().expect("the failure reports its time")
}

fn simulate_assert(condition: &str) -> Result<rumoca_solver::SimResult, crate::SimError> {
    let source = format!(
        concat!(
            "model AssertNoEvent\n",
            "  Real x(start = 0, fixed = true);\n",
            "equation\n",
            "  der(x) = sin(10*time);\n",
            "  assert({}, \"must fail at t = 0.5\");\n",
            "end AssertNoEvent;\n",
        ),
        condition
    );
    let dae = compile(&source, "AssertNoEvent");
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.01),
        ..SimOptions::default()
    };
    simulate_dae(&dae, &options)
}

fn assert_fails_at_half_time(condition: &str) {
    let error = simulate_assert(condition)
        .expect_err("a violated error-level assertion must stop the run between events");
    let time = failure_time(&error, "must fail at t = 0.5");
    // An event crossing is located to the root tolerance; a check between
    // events happens at the first accepted step or output point from 0.5.
    assert!(
        (0.5 - 1e-6..0.5 + 0.011).contains(&time),
        "{condition}: failed at t = {time}, expected the first step or output point from 0.5"
    );
}

/// The two output points around the violation both hold, so only a check at
/// the accepted integrator steps inside it can find it.
fn assert_transient_found(solver_mode: SimSolverMode) {
    let dae = compile(TRANSIENT, "Transient");
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.1),
        solver_mode,
        ..SimOptions::default()
    };
    let error = simulate_dae(&dae, &options)
        .expect_err("a violation between two output points must stop the run");
    let time = failure_time(&error, "transient violation");
    assert!(
        (0.42..0.48).contains(&time),
        "{solver_mode:?}: failed at t = {time}, expected an accepted step inside (0.42, 0.48)"
    );
}

#[test]
fn a_violation_between_output_points_stops_a_fixed_step_run() {
    assert_transient_found(SimSolverMode::VelImplicit);
}

#[test]
fn a_violation_between_output_points_stops_an_adaptive_run() {
    assert_transient_found(SimSolverMode::RkLike);
}

/// The session locates a crossing before it completes the step, so each
/// checked step ends at or before the bounce, where the floor holds, and a
/// `noEvent` assertion beside a state event does not fail the run.
#[test]
fn a_step_past_an_unlocated_event_is_not_checked() {
    let dae = compile(
        concat!(
            "model Bounce\n",
            "  Real h(start = 1, fixed = true);\n",
            "  Real v(start = 0, fixed = true);\n",
            "equation\n",
            "  der(h) = v;\n",
            "  der(v) = -9.81;\n",
            "  when h < 0 then\n",
            "    reinit(v, -0.8*pre(v));\n",
            "  end when;\n",
            "  assert(noEvent(h > -1e-3), \"below the floor\");\n",
            "end Bounce;\n",
        ),
        "Bounce",
    );
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let result = simulate_dae(&dae, &options).expect("the floor holds at every accepted point");
    let last = *result.times.last().expect("the run records output points");
    assert!(
        (last - 1.0).abs() < 1e-9,
        "the run must reach t_end, ended at {last}"
    );
}

/// The step check reads the same rows on the native and reference backends.
#[test]
fn both_backends_check_a_noevent_assertion_at_a_step() {
    let dae = compile(TRANSIENT, "Transient");
    let model = lower_dae_for_simulation(&dae, &SimOptions::default()).unwrap();
    let backend = crate::native_execution::backend(&model.pure_calls).unwrap();
    let native =
        rumoca_solver::SolveRuntime::new_with_execution_backend(&model, Some(backend)).unwrap();
    let reference = rumoca_solver::SolveRuntime::new(&model).unwrap();
    for runtime in [&native, &reference] {
        assert!(runtime.has_unclocked_error_assertions().unwrap());
        let held = runtime
            .check_error_assertions(&model.initial_y, &model.parameters, 0.4)
            .unwrap();
        assert_eq!(held, rumoca_solver::EventActionOutcome::Continue);
        let violated = runtime
            .check_error_assertions(&model.initial_y, &model.parameters, 0.45)
            .unwrap();
        assert!(
            matches!(
                &violated,
                rumoca_solver::EventActionOutcome::AssertionFailed { message, .. }
                    if message == "transient violation"
            ),
            "{violated:?}"
        );
    }
}

#[test]
fn a_noevent_condition_is_checked_between_events() {
    assert_fails_at_half_time("noEvent(time < 0.5 or x > 10)");
}

#[test]
fn a_noevent_operand_is_checked_between_events() {
    assert_fails_at_half_time("noEvent(time < 0.5) or x > 10");
}

#[test]
fn an_event_generating_condition_still_fails_at_its_crossing() {
    assert_fails_at_half_time("time < 0.5 or x > 10");
}

#[test]
fn a_noevent_condition_that_holds_does_not_stop_the_run() {
    let result = simulate_assert("noEvent(time < 2 or x > 10)")
        .expect("an assertion that holds must not stop the run");
    let last = *result.times.last().expect("the run records output points");
    assert!(
        (last - 1.0).abs() < 1e-9,
        "the run must reach t_end, ended at {last}"
    );
}

#[cfg(feature = "fmi")]
fn fixed_step_import(
    source: &str,
    model: &str,
    step: f64,
) -> Result<rumoca_solver::fmi_me::fixed_step::FixedStepRun, rumoca_solver::fmi_me::MeError> {
    let dae = compile(source, model);
    let component = crate::solve_lowering::lower_fmi_export_component(&dae)
        .expect("the model lowers for export");
    let artifact = rumoca_solver::fmi_me::MeModelArtifact::new(component);
    rumoca_solver::fmi_me::fixed_step::fixed_step_rk4(&artifact, step, 1.0)
}

#[cfg(feature = "fmi")]
#[test]
fn a_fixed_step_import_settles_sample_updates_on_and_between_grid_points() {
    let source = SCHEDULED_ASSERTION.replace("der(x) = 1;", "der(x) = if armed then 2 else 1;");
    for (step, rows) in [(0.01, 100), (0.04, 25)] {
        let run = fixed_step_import(&source, "ScheduledAssertion", step)
            .expect("the fixed-step importer settles the sample before checking");
        assert_eq!(run.times.len(), rows);
        for (index, time) in run.times.iter().enumerate() {
            let expected = if index + 1 == rows {
                1.0
            } else {
                (index + 1) as f64 * step
            };
            assert_eq!(time.to_bits(), expected.to_bits());
        }
        let x = run.states.last().expect("recorded state")[0];
        assert!(
            (x - 1.5).abs() < 1e-12,
            "armed must change the derivative, got {x}"
        );
    }
}

#[cfg(feature = "fmi")]
#[test]
fn a_fixed_step_import_uses_the_left_derivative_at_a_scheduled_boundary() {
    let source = SCHEDULED_ASSERTION.replace(
        "der(x) = 1;",
        "der(x) = noEvent(if time < 0.5 then 1 else 2);",
    );
    for step in [0.01, 0.04] {
        let run = fixed_step_import(&source, "ScheduledAssertion", step)
            .expect("the fixed-step importer settles the sample before checking");
        let x = run.states.last().expect("recorded state")[0];
        assert!((x - 1.5).abs() < 1e-12, "x(1) = {x} with step {step}");
    }
}

#[cfg(feature = "fmi")]
#[test]
fn a_fixed_step_import_resumes_at_the_exact_sample_time_after_a_nominal_row() {
    let event_time = 0.5000000000000071;
    let source = SCHEDULED_ASSERTION
        .replace("0.5", &event_time.to_string())
        .replace("der(x) = 1;", "der(x) = if armed then 1 else 0;");
    let run = fixed_step_import(&source, "ScheduledAssertion", 0.01)
        .expect("the sample settles just past the nominal row");
    assert_eq!(run.times[49].to_bits(), 0.5_f64.to_bits());
    assert_eq!(run.times.len(), 100);
    let x = run.states.last().expect("recorded state")[0];
    let expected = 1.0 - event_time;
    assert!(
        (x - expected).abs() < 1e-15,
        "x(1) = {x}, expected {expected}"
    );
}

#[cfg(feature = "fmi")]
#[test]
fn a_fixed_step_import_stops_at_a_scheduled_termination() {
    let source = SCHEDULED_ASSERTION.replace(
        "armed = true;",
        "armed = true;\n    terminate(\"scheduled stop\");",
    );
    for (step, rows) in [(0.01, 50), (0.04, 13)] {
        let run = fixed_step_import(&source, "ScheduledAssertion", step)
            .expect("model termination completes the run");
        let termination = run.termination.expect("scheduled termination result");
        assert_eq!(termination.time.to_bits(), 0.5_f64.to_bits());
        assert_eq!(termination.message, "scheduled stop");
        assert_eq!(run.times.len(), rows);
        assert_eq!(
            run.times.last().expect("last row").to_bits(),
            0.5_f64.to_bits()
        );
        let x = run.states.last().expect("final states")[0];
        assert!((x - 0.5).abs() < 1e-12, "x(0.5) = {x} with step {step}");
    }
}

#[cfg(feature = "fmi")]
#[test]
fn a_fixed_step_import_checks_the_post_sample_assertion_at_the_boundary() {
    use rumoca_solver::fmi_me::{MeError, MeStage};

    let source = SCHEDULED_ASSERTION.replace("or armed", "or not armed");
    for step in [0.01, 0.04] {
        let Err(MeError::Staged {
            stage: MeStage::EventIteration,
            inner,
        }) = fixed_step_import(&source, "ScheduledAssertion", step)
        else {
            panic!("the assertion must fail during event iteration");
        };
        let MeError::Assertion { time, message } = *inner else {
            panic!("expected the model assertion, got {inner:?}");
        };
        assert_eq!(time.to_bits(), 0.5_f64.to_bits());
        assert_eq!(message, "scheduled update must settle before checking");
    }
}

/// An importer calls `completedIntegratorStep` after each accepted step, so a
/// Model Exchange component finds the violation at a step inside it.
#[cfg(feature = "fmi")]
#[test]
fn a_model_exchange_import_finds_a_violation_at_a_completed_step() {
    match fixed_step_import(TRANSIENT, "Transient", 0.01) {
        Err(rumoca_solver::fmi_me::MeError::Assertion { time, message }) => {
            assert_eq!(message, "transient violation");
            assert!((0.42..0.49).contains(&time), "failed at t = {time}");
        }
        other => panic!("expected an assertion failure, got {other:?}"),
    }
}

/// The supplied reproduction fails at an accepted step, before the next point
/// of a coarse output grid.
#[test]
fn the_reproduction_fails_before_the_next_coarse_output() {
    let dae = compile(
        &format!(
            concat!(
                "model AssertNoEvent\n",
                "  Real x(start = 0, fixed = true);\n",
                "equation\n",
                "  der(x) = sin(10*time);\n",
                "  assert({}, \"must fail at t = 0.5\");\n",
                "end AssertNoEvent;\n",
            ),
            "noEvent(time < 0.5 or x > 10)"
        ),
        "AssertNoEvent",
    );
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.3),
        solver_mode: SimSolverMode::VelImplicit,
        ..SimOptions::default()
    };
    let error = simulate_dae(&dae, &options).expect_err("the violation must stop the run");
    let time = failure_time(&error, "must fail at t = 0.5");
    assert!((0.5..0.51).contains(&time), "failed at t = {time}");
}

/// An assertion in a `when` clause belongs to that clause's activation, so a
/// condition that only changes later between events is not checked there.
#[test]
fn a_when_owned_assertion_is_checked_only_at_its_activation() {
    let dae = compile(
        concat!(
            "model WhenOwned\n",
            "  Real x(start = 0, fixed = true);\n",
            "equation\n",
            "  der(x) = 1;\n",
            "  when time > 0.3 then\n",
            "    assert(noEvent(time < 0.35), \"checked after its activation\");\n",
            "  end when;\n",
            "end WhenOwned;\n",
        ),
        "WhenOwned",
    );
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.01),
        ..SimOptions::default()
    };
    simulate_dae(&dae, &options).expect("the assertion held at its activation");
}

/// RK4 evaluates its stages at trial points between accepted ones. A
/// violation seen only at the stage midpoint 0.495 of the step from 0.49 to
/// 0.50 does not abort the accepted trajectory.
#[cfg(feature = "fmi")]
#[test]
fn a_model_exchange_import_does_not_check_trial_points() {
    let source = concat!(
        "model TrialOnly\n",
        "  Real x(start = 0, fixed = true);\n",
        "equation\n",
        "  der(x) = 1;\n",
        "  assert(noEvent(time < 0.4945 or time > 0.4955), \"trial point\");\n",
        "end TrialOnly;\n",
    );
    let run = fixed_step_import(source, "TrialOnly", 0.01).expect("no accepted point violates it");
    assert_eq!(run.times.len(), 100);
}

/// Whether a run of `source` checks its unclocked error-level assertions at
/// each accepted step, on the native and reference backends alike.
fn checks_at_accepted_steps(source: &str, model: &str) -> bool {
    let dae = compile(source, model);
    let model = lower_dae_for_simulation(&dae, &SimOptions::default()).unwrap();
    let backend = crate::native_execution::backend(&model.pure_calls).unwrap();
    let native =
        rumoca_solver::SolveRuntime::new_with_execution_backend(&model, Some(backend)).unwrap();
    let reference = rumoca_solver::SolveRuntime::new(&model).unwrap();
    let native = native.has_unclocked_error_assertions().unwrap();
    assert_eq!(native, reference.has_unclocked_error_assertions().unwrap());
    native
}

fn assertion_model(declarations: &str, condition: &str) -> String {
    format!(
        concat!(
            "model Checked\n",
            "  Real x(start = 0, fixed = true);\n",
            "{}",
            "equation\n",
            "  der(x) = sin(10*time);\n",
            "  assert({}, \"checked\");\n",
            "end Checked;\n",
        ),
        declarations, condition
    )
}

/// A condition that reads only parameters and constants cannot change
/// between events, so no accepted step re-observes the coordinate for it
/// (the run-phase cost of #694 on MSL models, whose asserts are parameter
/// checks); a violated one still stops the run at its first output point.
#[test]
fn a_parameter_only_assertion_is_not_checked_at_accepted_steps() {
    let source = assertion_model("  parameter Real k = 1;\n", "k > 0");
    assert!(!checks_at_accepted_steps(&source, "Checked"));
    let violated = assertion_model("  parameter Real k = -1;\n", "k > 0");
    let error = simulate_dae(&compile(&violated, "Checked"), &SimOptions::default())
        .expect_err("a violated parameter assertion must stop the run");
    assert_eq!(failure_time(&error, "checked").to_bits(), 0.0_f64.to_bits());
}

/// A condition on a state keeps the accepted-step check, whether it lies
/// inside `noEvent` (no root) or outside it (rooted).
#[test]
fn a_state_assertion_is_checked_at_accepted_steps() {
    for condition in ["noEvent(x > -10)", "x > -10"] {
        let source = assertion_model("", condition);
        assert!(checks_at_accepted_steps(&source, "Checked"), "{condition}");
    }
}

/// A top-level input can change between events without one, so an assertion
/// on it keeps the accepted-step check.
#[test]
fn an_input_assertion_is_checked_at_accepted_steps() {
    let source = assertion_model("  input Real u = 0;\n", "noEvent(u < 10)");
    assert!(checks_at_accepted_steps(&source, "Checked"));
}

/// A discrete value is held between events, so an assertion that reads only
/// it leaves the accepted-step check; when an event changes it into a
/// violation, the run still stops no later than the next output point.
#[test]
fn an_assertion_violated_by_an_event_update_stops_by_the_next_output_point() {
    let source = concat!(
        "model EventViolated\n",
        "  Real x(start = 0, fixed = true);\n",
        "  discrete Real k(start = 1, fixed = true);\n",
        "equation\n",
        "  der(x) = 1;\n",
        "  when time > 0.5 then\n",
        "    k = -1;\n",
        "  end when;\n",
        "  assert(k > 0, \"checked\");\n",
        "end EventViolated;\n",
    );
    let dae = compile(source, "EventViolated");
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.1),
        ..SimOptions::default()
    };
    let error = simulate_dae(&dae, &options).expect_err("the event update violates the assertion");
    let time = failure_time(&error, "checked");
    assert!(
        (0.5..=0.6 + 1e-9).contains(&time),
        "failed at t = {time}, expected the event at 0.5 or the next output point 0.6"
    );
}
