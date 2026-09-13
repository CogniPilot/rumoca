//! MLS Appendix B: relations must settle with their event-updated inputs.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model TimerCascade
  Real x(start=0, fixed=true);
  Real voltage;
  Boolean negative;
  Real elapsed;
  Boolean fire;
equation
  der(x) = 1;
  voltage = sin(2*3.141592653589793*50*(time - 1.0/600));
  negative = voltage < 0;
  elapsed = if negative then time else 0;
  fire = 100*elapsed > 1.0/6;
end TimerCascade;
"#;

fn check_event_relation(solver_mode: SimSolverMode) {
    let compiled = Compiler::new()
        .model("TimerCascade")
        .compile_str(SOURCE, "event_relation_cascade.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.01,
            dt: Some(0.0001),
            rtol: 1e-6,
            atol: 1e-6,
            solver_mode,
            ..SimOptions::default()
        },
    )
    .unwrap();
    let fire = &result.data[result.names.iter().position(|name| name == "fire").unwrap()];
    let negative = &result.data[result
        .names
        .iter()
        .position(|name| name == "negative")
        .unwrap()];
    assert_eq!(negative[0], 1.0);
    assert_eq!(*negative.last().unwrap(), 0.0);
    // Before 1/600 s, 100*elapsed <= 1/6. After that crossing, elapsed=0
    // until the next negative half-cycle (outside this experiment).
    for (time, value) in result.times.iter().zip(fire) {
        assert_eq!(
            *value, 0.0,
            "{solver_mode:?}: a timer disabled by the same event fired at t={time}"
        );
    }
}

#[test]
fn bdf_event_relation_cascade_settles() {
    check_event_relation(SimSolverMode::Bdf);
}

#[test]
fn rk_event_relation_cascade_settles() {
    check_event_relation(SimSolverMode::RkLike);
}
