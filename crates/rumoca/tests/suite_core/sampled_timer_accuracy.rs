//! MLS Appendix B: consuming a sample tick must not backdate a later state event.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model SampledTimer
  Real x(start=0, fixed=true);
  Real voltage;
  Boolean positive;
  discrete Real entryTime(start=0, fixed=true);
  Real elapsed;
  Boolean fire;
  Integer ticks(start=0, fixed=true);
equation
  voltage = sin(2*3.141592653589793*50*(time - 1.0/600));
  positive = voltage > 0;
  when positive then
    entryTime = time;
  end when;
  elapsed = if positive then time - entryTime else 0;
  fire = 100*elapsed > 1.0/6;
  der(x) = if fire then 10000 else 0;
  when sample(1.0/300, 1.0/300) then
    ticks = pre(ticks) + 1;
  end when;
end SampledTimer;
"#;

fn check_sampled_timer(solver_mode: SimSolverMode) {
    let compiled = Compiler::new()
        .model("SampledTimer")
        .compile_str(SOURCE, "sampled_timer.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.01,
            dt: Some(0.0002),
            rtol: 1e-6,
            atol: 1e-6,
            solver_mode,
            ..SimOptions::default()
        },
    )
    .unwrap();
    let x = &result.data[result.names.iter().position(|name| name == "x").unwrap()];
    let ticks = &result.data[result
        .names
        .iter()
        .position(|name| name == "ticks")
        .unwrap()];
    assert_eq!(*ticks.last().unwrap(), 3.0, "each sample executes once");
    for (time, value) in result.times.iter().zip(x) {
        let expected = 10000.0 * (time - 1.0 / 300.0).max(0.0);
        let bound = 1e-6 * expected.max(1.0);
        assert!(
            (value - expected).abs() <= bound,
            "{solver_mode:?} at {time}: integral={value}, exact={expected}, bound={bound}"
        );
    }
}

#[test]
fn bdf_sampled_timer_preserves_state_event_time() {
    check_sampled_timer(SimSolverMode::Bdf);
}

#[test]
fn rk_sampled_timer_preserves_state_event_time() {
    check_sampled_timer(SimSolverMode::RkLike);
}
