//! SPEC_0022 SIM-001 and SPEC_0044 ME-EVENT-006: a certified continuous
//! projection must be followed by consistent discrete values at an event.

use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

use super::msl_sim_regression::require_msl_compiler;

#[test]
fn pull_in_stroke_retains_a_certified_post_commit_view() {
    let compiled = require_msl_compiler()
        .model("PullInEventRegression")
        .compile_str(
            "model PullInEventRegression\n  extends Modelica.Magnetic.FluxTubes.Examples.SolenoidActuator.ComparisonPullInStroke;\nend PullInEventRegression;\n",
            "PullInEventRegression.mo",
        )
        .expect("the unchanged MSL solenoid example should compile");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.001,
            dt: Some(0.0001),
            rtol: 1.0e-7,
            atol: 1.0e-7,
            max_wall_seconds: Some(12.0),
            solver_mode: SimSolverMode::Bdf,
            ..SimOptions::default()
        },
    )
    .expect("the certified coordinate must survive the table event at t=0.0007");
    assert!(result.times.last().is_some_and(|&t| t >= 0.001));
    assert!(result.data.iter().flatten().all(|value| value.is_finite()));
}
