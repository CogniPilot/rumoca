//! Reduced Newton over a tear whose residual is a square-root characteristic.
//!
//! A flow `m = k*law(dp)` with a regularized square-root law, driven to zero
//! when a valve closes (an `if` equation selecting `m = 0`), leaves the torn
//! block a residual in `dp` that is the inverse of a quadratic pressure loss.
//! A full Newton step on such a residual lands near the mirror image of the
//! start. The reduced Newton halves that step under its sufficient-decrease
//! acceptance and reaches the root, so no call falls back to the dense block
//! Newton (the `Modelica.Fluid` tank ports with an emptying pipe behave so).

use rumoca::Compiler;

const SOURCE: &str = r#"
package SqrtLoop
  function law "Regularized square-root flow characteristic"
    input Real dp;
    input Real delta;
    output Real m;
  algorithm
    m := dp/(dp*dp + delta*delta)^0.25;
  end law;
  model Valve
    parameter Real k = 2;
    Real dp(start = 0);
    Real m;
    Real q;
    Real level(start = 1, fixed = true);
    Boolean open = level > 0.5;
  equation
    der(level) = -0.1;
    m = k*law(dp, 1e-3);
    q = 3*m + 0.01*dp;
    if open then
      q = 2*time;
    else
      m = 0;
    end if;
  end Valve;
  model Smooth "The same loop over a smooth characteristic"
    Real dp(start = 0);
    Real m;
    Real q;
  equation
    m = 2*(dp + dp^3/3);
    q = 3*m + 0.01*dp;
    q = 0.2*time;
  end Smooth;
end SqrtLoop;
"#;

#[test]
fn closing_valve_settles_the_torn_block_without_falling_back() {
    let compiled = Compiler::new()
        .model("SqrtLoop.Valve")
        .compile_str(SOURCE, "SqrtLoop.mo")
        .unwrap_or_else(|error| panic!("SqrtLoop.Valve compiles: {error:?}"));
    rumoca_sim::reset_projection_fallbacks();
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 10.0,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("SqrtLoop.Valve simulates: {error:?}"));
    let report = rumoca_sim::projection_fallbacks();
    let fallbacks: u64 = report.sites.values().map(|counts| counts.total()).sum();
    let calls: u64 = report.sites.values().map(|counts| counts.calls).sum();
    assert!(calls > 0, "the torn block is projected");
    assert_eq!(fallbacks, 0, "{report:?}");
    let halvings: u64 = report
        .sites
        .values()
        .map(|counts| counts.torn_step_halvings)
        .sum();
    assert!(halvings > 0, "the square-root step is halved: {report:?}");
    let m = result
        .names
        .iter()
        .position(|name| name == "m")
        .expect("m is recorded");
    let last = result.data[m].last().copied().expect("samples");
    assert!(last.abs() < 1e-8, "the closed valve passes no flow: {last}");
}

/// A smooth residual near its root loses far more than half its norm in a
/// full Newton step, so the sufficient-decrease test never halves it: the
/// reduced Newton takes exactly the full steps a plain decrease test takes.
#[test]
fn a_smooth_torn_residual_keeps_the_full_newton_step() {
    let compiled = Compiler::new()
        .model("SqrtLoop.Smooth")
        .compile_str(SOURCE, "SqrtLoop.mo")
        .unwrap_or_else(|error| panic!("SqrtLoop.Smooth compiles: {error:?}"));
    rumoca_sim::reset_projection_fallbacks();
    rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 10.0,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("SqrtLoop.Smooth simulates: {error:?}"));
    let report = rumoca_sim::projection_fallbacks();
    let calls: u64 = report.sites.values().map(|counts| counts.calls).sum();
    let fallbacks: u64 = report.sites.values().map(|counts| counts.total()).sum();
    let halvings: u64 = report
        .sites
        .values()
        .map(|counts| counts.torn_step_halvings)
        .sum();
    assert!(calls > 0, "the torn block is projected");
    assert_eq!(fallbacks, 0, "{report:?}");
    assert_eq!(halvings, 0, "{report:?}");
}
