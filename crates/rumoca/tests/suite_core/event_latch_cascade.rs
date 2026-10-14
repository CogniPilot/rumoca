//! MLS Appendix B: solve current equations with fixed pre before advancing history.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model DelayedLatch
  Boolean enabled;
  discrete Real entryTime(start=0, fixed=true);
  Real elapsed;
  Boolean fire;
  Boolean latched(start=false, fixed=true);
  Real integral(start=0, fixed=true);
equation
  enabled = sin(2*3.141592653589793*time) > 0;
  when enabled then
    entryTime = time;
  end when;
  elapsed = if enabled then time - entryTime else 0;
  fire = elapsed > 0.1;
  latched = enabled and (pre(latched) or fire);
  der(integral) = if latched then 1 else 0;
end DelayedLatch;
"#;

fn check_delayed_latch(solver_mode: SimSolverMode) {
    let compiled = Compiler::new()
        .model("DelayedLatch")
        .compile_str(SOURCE, "delayed_latch.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.2,
            dt: Some(0.01),
            rtol: 1e-7,
            atol: 1e-9,
            solver_mode,
            ..Default::default()
        },
    )
    .unwrap();
    let latch = result
        .names
        .iter()
        .position(|name| name == "latched")
        .unwrap();
    let integral = result
        .names
        .iter()
        .position(|name| name == "integral")
        .unwrap();
    for (index, &time) in result.times.iter().enumerate() {
        let phase = time - time.floor();
        if phase.min(1.0 - phase) > 1e-8 && (phase - 0.1).abs() > 1e-8 && (phase - 0.5).abs() > 1e-8
        {
            assert_eq!(
                result.data[latch][index],
                f64::from(phase > 0.1 && phase < 0.5),
                "{solver_mode:?}: unsettled timer input latched at t={time}"
            );
        }
        let expected = 0.4 * time.floor() + (phase - 0.1).clamp(0.0, 0.4);
        assert!(
            (result.data[integral][index] - expected).abs() < 1e-6,
            "{solver_mode:?}: integral at t={time}, expected {expected}"
        );
    }
}

#[test]
fn bdf_latch_waits_for_settled_event_inputs() {
    check_delayed_latch(SimSolverMode::Bdf);
}

#[test]
fn rk_latch_waits_for_settled_event_inputs() {
    check_delayed_latch(SimSolverMode::RkLike);
}

#[test]
fn held_right_limit_preserves_event_memory_until_the_next_actual_event() {
    use rumoca_solver::{EventActionOutcome, EventUpdateRowFilter, ProjectedEventUpdateInput};

    let compiled = Compiler::new()
        .model("HeldContinuation")
        .compile_str(
            r#"
model HeldContinuation
  Boolean enabled;
  discrete Real stamp(start=0, fixed=true);
  Real elapsed;
equation
  enabled = time > 0;
  when enabled then
    stamp = time;
    terminate("enabled edge");
  end when;
  elapsed = time - stamp;
  assert(noEvent(time < 0.3), "continuous assertion");
end HeldContinuation;
"#,
            "held_continuation.mo",
        )
        .unwrap();
    let model =
        rumoca_sim::lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let runtime = rumoca_solver::SolveRuntime::new(&model).unwrap();
    let mut y = model.initial_y.to_vec();
    let mut p = model.parameters.to_vec();
    runtime.set_initial_event_flag(&mut p, false);
    let update = |filter, t, y: &mut [f64], p: &mut [f64]| {
        let event_pre_y = y.to_vec();
        let event_pre_p = p.to_vec();
        runtime
            .apply_projected_event_update(
                ProjectedEventUpdateInput {
                    y,
                    p,
                    t,
                    tol: 1e-10,
                    event_pre_y: &event_pre_y,
                    event_pre_p: &event_pre_p,
                    max_iters: 32,
                    row_filter: filter,
                    root_relation_overrides: &mut Vec::new(),
                },
                |y, p| {
                    let before = y.to_vec();
                    runtime.refresh_algebraic_and_output_slots_certified(t, y, p, 1e-10, 32)?;
                    Ok(y != before.as_slice())
                },
            )
            .unwrap()
    };
    let original_bits = p.iter().map(|value| value.to_bits()).collect::<Vec<_>>();
    assert_eq!(
        update(EventUpdateRowFilter::Hold, 0.2, &mut y, &mut p),
        EventActionOutcome::Continue
    );
    assert_eq!(
        p.iter().map(|value| value.to_bits()).collect::<Vec<_>>(),
        original_bits
    );
    let values = runtime
        .visible_values_for_names(&y, &p, 0.2, &["elapsed".into()])
        .unwrap();
    assert_eq!(values["elapsed"], 0.2);
    assert!(matches!(
        update(EventUpdateRowFilter::Hold, 0.4, &mut y, &mut p),
        EventActionOutcome::AssertionFailed { .. }
    ));
    assert_eq!(
        p.iter().map(|value| value.to_bits()).collect::<Vec<_>>(),
        original_bits
    );
    assert!(matches!(
        update(EventUpdateRowFilter::All, 0.2, &mut y, &mut p),
        EventActionOutcome::Terminated { .. }
    ));
    assert_eq!(
        update(EventUpdateRowFilter::All, 0.25, &mut y, &mut p),
        EventActionOutcome::Continue
    );
    let values = runtime
        .visible_values_for_names(&y, &p, 0.25, &["enabled".into(), "stamp".into()])
        .unwrap();
    assert_eq!(values["enabled"], 1.0);
    assert_eq!(
        values["stamp"], 0.2,
        "an already-held true condition has no second edge"
    );
}

#[test]
fn held_continuation_clears_clock_activation_without_replaying_clocked_values() {
    use rumoca_solver::{EventActionOutcome, EventUpdateRowFilter, ProjectedEventUpdateInput};

    let compiled = Compiler::new()
        .model("HeldClock")
        .compile_str(
            "model HeldClock Integer count(start=0, fixed=true); equation \
             when sample(0, 1) then count = pre(count) + 1; end when; end HeldClock;",
            "held_clock.mo",
        )
        .unwrap();
    let model =
        rumoca_sim::lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let runtime = rumoca_solver::SolveRuntime::new(&model).unwrap();
    let mut y = model.initial_y.to_vec();
    let mut p = model.parameters.to_vec();
    runtime.set_initial_event_flag(&mut p, false);
    let activation = &model.problem.clocks.activation_parameter_indices;
    assert_eq!(activation.len(), 1);
    p[activation[0]] = 1.0;
    let held = model
        .problem
        .solve_layout
        .variable_storage_runs
        .iter()
        .find(|run| run.role == rumoca_ir_solve::SolveVariableStorageRole::DiscreteValue)
        .unwrap();
    let rumoca_ir_solve::ScalarSlot::P { index, .. } = held.base else {
        panic!("the source Integer has checked parameter storage");
    };
    p[index] = 1.0;
    let event_pre_p = p.clone();
    let event_pre_y = y.clone();
    assert_eq!(
        runtime
            .apply_projected_event_update(
                ProjectedEventUpdateInput {
                    y: &mut y,
                    p: &mut p,
                    t: 0.2,
                    tol: 1e-10,
                    event_pre_y: &event_pre_y,
                    event_pre_p: &event_pre_p,
                    max_iters: 32,
                    row_filter: EventUpdateRowFilter::Hold,
                    root_relation_overrides: &mut Vec::new(),
                },
                |_, _| Ok(false),
            )
            .unwrap(),
        EventActionOutcome::Continue
    );
    let mut expected = event_pre_p;
    expected[activation[0]] = 0.0;
    assert_eq!(p, expected, "only the internal tick pulse is released");
}
