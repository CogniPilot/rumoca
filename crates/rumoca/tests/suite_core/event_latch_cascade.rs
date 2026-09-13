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
