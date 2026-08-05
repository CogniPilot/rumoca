//! SPEC_0038 phase 2: event-observation parity across the two ME hosts.
//!
//! Both solver plugins drive the shared FMI 3 ME kernel. These tests pin the
//! canonical settled observation at a scheduled event boundary and the
//! continuous trajectory away from that boundary.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae_with_diagnostics};

/// One scheduled time event at `t = 0.5` and nothing else: `Vs` steps from 0 to
/// `Vb`, and the continuous state is driven through the step. The event instant
/// is exactly on the output grid, which is what makes the sampling difference
/// visible rather than hidden between two output points.
const SCHEDULED_STEP: &str = r#"
model ScheduledStep
  parameter Real Vb = 24;
  parameter Real tau = 0.1;
  Real Vs;
  Real x(start = 0, fixed = true);
equation
  Vs = if time > 0.5 then Vb else 0;
  tau*der(x) = Vs - x;
end ScheduledStep;
"#;

fn run(solver_mode: SimSolverMode) -> SimResult {
    simulate(SCHEDULED_STEP, "ScheduledStep", solver_mode, 1.0)
}

fn simulate(source: &str, model: &str, solver_mode: SimSolverMode, t_end: f64) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("compile {model}: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end,
            dt: Some(0.01),
            rtol: 1.0e-10,
            atol: 1.0e-10,
            max_wall_seconds: Some(60.0),
            solver_mode,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} on {solver_mode:?} failed: {error:?}"))
}

fn channel<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("missing `{name}` in {:?}", result.names));
    &result.data[index]
}

/// Samples at or immediately after the event instant, as `(t, Vs)` pairs.
fn samples_at_the_event(result: &SimResult) -> Vec<(f64, f64)> {
    let vs = channel(result, "Vs");
    result
        .times
        .iter()
        .copied()
        .zip(vs.iter().copied())
        .filter(|(t, _)| (*t - 0.5).abs() < 1.0e-6)
        .collect()
}

/// A sample exactly on a scheduled event observes the single settled
/// superdense value, independent of the numerical solver plugin.
#[test]
fn the_two_hosts_agree_on_the_observation_at_a_scheduled_event_instant() {
    let diffsol = run(SimSolverMode::Bdf);
    let me_kernel = run(SimSolverMode::RkLike);

    let diffsol_rows = samples_at_the_event(&diffsol);
    let me_kernel_rows = samples_at_the_event(&me_kernel);

    assert_eq!(diffsol_rows, vec![(0.5, 24.0)]);
    assert_eq!(me_kernel_rows, diffsol_rows);

    assert!(
        diffsol_rows
            .iter()
            .chain(&me_kernel_rows)
            .all(|(time, value)| time.is_finite() && value.is_finite()),
        "event observations must be finite: diffsol={diffsol_rows:?}, \
         kernel={me_kernel_rows:?}"
    );

    assert_eq!(
        diffsol.times.len(),
        me_kernel.times.len(),
        "solver plugins must preserve the shared kernel's trace shape"
    );
}

/// Away from the event instant the two hosts agree on the trajectory to
/// integrator truncation.
#[test]
fn away_from_the_event_instant_the_two_hosts_agree_to_integrator_truncation() {
    let diffsol = run(SimSolverMode::Bdf);
    let me_kernel = run(SimSolverMode::RkLike);

    let diffsol_x = channel(&diffsol, "x");
    let me_kernel_x = channel(&me_kernel, "x");

    let mut compared = 0usize;
    let mut worst = 0.0_f64;
    for (index, time) in me_kernel.times.iter().copied().enumerate() {
        assert!(time.is_finite(), "the kernel emitted a non-finite time");
        if (time - 0.5).abs() < 1.0e-6 {
            continue;
        }
        let Some(other) = diffsol
            .times
            .iter()
            .position(|candidate| (candidate - time).abs() <= 1.0e-9)
        else {
            continue;
        };
        let (a, b) = (me_kernel_x[index], diffsol_x[other]);
        assert!(
            a.is_finite() && b.is_finite(),
            "shared output at t={time} must be finite, kernel={a}, diffsol={b}"
        );
        let scale = a.abs().max(b.abs()).max(1.0e-12);
        worst = worst.max((a - b).abs() / scale);
        compared += 1;
    }

    assert!(
        compared > 50,
        "the comparison must cover the horizon, only {compared} shared output \
         points matched"
    );
    assert!(
        worst < 1.0e-5,
        "off the event instant the two hosts must agree to integrator \
         truncation; worst relative difference was {worst:e}"
    );
}
