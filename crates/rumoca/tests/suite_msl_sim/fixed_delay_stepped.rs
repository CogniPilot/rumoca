//! `Modelica.Blocks.Nonlinear.FixedDelay` in a session stepped on an output
//! grid, as an application drives it: a step (a time event at 1 s) directly
//! and through a first-order lag, then two seconds of dead time. The step's
//! history must hold its left limit at the tick (`time < startTime` is already
//! on its post-event branch there), and the cubic read-back must not reach
//! across the lag's kink at the event: before, the delayed step ramped over
//! the last step before the tick, and the delayed lag dipped to -0.026.

use rumoca_sim::{SimOptions, SimSolverMode, SimulationSession};

use super::msl_sim_regression::require_msl_compiler;

const SOURCE: &str = "model DelayedLag
  Modelica.Blocks.Sources.Step step(height = 1, startTime = 1);
  Modelica.Blocks.Continuous.FirstOrder lag(T = 0.5, initType = Modelica.Blocks.Types.Init.InitialOutput, y_start = 0);
  Modelica.Blocks.Nonlinear.FixedDelay deadStep(delayTime = 2);
  Modelica.Blocks.Nonlinear.FixedDelay deadLag(delayTime = 2);
equation
  connect(step.y, lag.u);
  connect(step.y, deadStep.u);
  connect(lag.y, deadLag.u);
end DelayedLag;
";

#[test]
fn a_fixed_delay_transports_a_step_and_a_lag() {
    let dae = require_msl_compiler()
        .model("DelayedLag")
        .compile_str(SOURCE, "DelayedLag.mo")
        .unwrap_or_else(|error| panic!("{error}"))
        .dae;
    for solver_mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        let mut session = SimulationSession::new(
            &dae,
            SimOptions {
                solver_mode,
                t_end: 6.0,
                ..SimOptions::default()
            },
        )
        .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
        for step in 1..=60 {
            let time = f64::from(step) * 0.1;
            session
                .advance_to(time)
                .unwrap_or_else(|error| panic!("{solver_mode:?}: advance_to({time}): {error}"));
            let get = |name: &str| session.get(name).unwrap().unwrap();
            let (dead_step, dead_lag) = (get("deadStep.y"), get("deadLag.y"));
            if (time - 3.0).abs() > 1e-9 {
                let expected = if time < 3.0 { 0.0 } else { 1.0 };
                assert_eq!(dead_step, expected, "{solver_mode:?}: deadStep.y at {time}");
            }
            let lag = if time <= 3.0 {
                0.0
            } else {
                1.0 - (-(time - 3.0) / 0.5).exp()
            };
            assert!(
                (dead_lag - lag).abs() < 1e-3,
                "{solver_mode:?}: deadLag.y at {time} is {dead_lag}, expected {lag}"
            );
        }
    }
}
