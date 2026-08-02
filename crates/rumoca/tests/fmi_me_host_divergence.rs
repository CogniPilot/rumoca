//! SPEC_0038 phase 2: what the two solver paths still disagree about.
//!
//! `rumoca-solver-rk45` is already an FMI 3 ME host over `SolveMeKernel`
//! (phase 1); `rumoca-solver-diffsol` still drives its own event loop over the
//! private runtime. Phase 2 puts both on the one kernel, so the size and shape
//! of the remaining disagreement is the thing that decides whether that move
//! can be behaviour-freezing.
//!
//! These tests characterize one **pinned divergence**. They assert the tree is
//! currently inconsistent and that the difference is confined to event-boundary
//! observations; the migration rewrites them to state the settled semantics.
//! Without that pin, the migration could change every event-carrying model's
//! output rows and nothing in the tree would notice.

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

/// RECORDED DIVERGENCE (SPEC_0038 phase 2).
///
/// At a scheduled time-event instant the diffsol path emits **two** observation
/// rows — the left limit at the instant and the right limit at the next
/// representable time — while the ME kernel host emits **one**, already
/// carrying the right limit. Both are defensible readings of MLS §8.5, and
/// exactly one of them can survive a single shared event loop.
///
/// This is why SPEC_0038 phase 2 cannot be behaviour-freezing on the state
/// path: the difference is O(1) in the reported value at the instant, and it
/// shifts the row index of the entire remaining trace.
#[test]
fn the_two_hosts_disagree_on_the_observations_at_a_scheduled_event_instant() {
    let diffsol = run(SimSolverMode::Bdf);
    let me_kernel = run(SimSolverMode::RkLike);

    let diffsol_rows = samples_at_the_event(&diffsol);
    let me_kernel_rows = samples_at_the_event(&me_kernel);

    assert_eq!(
        diffsol_rows.len(),
        2,
        "the diffsol path records the event instant's left and right limits, \
         got {diffsol_rows:?}"
    );
    assert_eq!(
        me_kernel_rows.len(),
        1,
        "the ME kernel host records one observation at the event instant, got \
         {me_kernel_rows:?}"
    );

    assert!(
        diffsol_rows
            .iter()
            .chain(&me_kernel_rows)
            .all(|(time, value)| time.is_finite() && value.is_finite()),
        "event observations must be finite: diffsol={diffsol_rows:?}, \
         kernel={me_kernel_rows:?}"
    );

    // The left-limit row is the whole difference: 0 versus the full step.
    assert_eq!(
        diffsol_rows[0].1, 0.0,
        "the diffsol path's first row at the instant is the left limit"
    );
    assert_eq!(
        diffsol_rows[1].1, 24.0,
        "the diffsol path's second row at the instant is the full right limit"
    );
    assert!(
        diffsol_rows[1].0 > diffsol_rows[0].0,
        "the diffsol right-limit probe must follow its left-limit observation, \
         got {diffsol_rows:?}"
    );
    assert_eq!(
        me_kernel_rows[0].1, 24.0,
        "the ME kernel host's single row at the instant is the full right limit"
    );

    assert_ne!(
        diffsol.times.len(),
        me_kernel.times.len(),
        "the extra left-limit row must change the trace length, or this \
         divergence would be invisible to a row-indexed comparator"
    );
}

/// The complement, and the reason the divergence above is a *sampling*
/// divergence rather than an integration one: away from the event instant the
/// two hosts agree on the trajectory to integrator truncation. A migration that
/// moved the trajectory itself would be a different, much larger finding.
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
