//! MLS §8.3.6: reinit preserves the event-entry value for pre(x).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model SampledIntegral
  Real x(start=0, fixed=true);
  discrete Real sampled(start=0, fixed=true);
equation
  der(x) = 50000 + 20000*sin(2*3.141592653589793*300*time);
  when sample(1.0/300, 1.0/300) then
    sampled = pre(x);
    reinit(x, 0);
  end when;
end SampledIntegral;
"#;

fn check_event_entry(solver_mode: SimSolverMode) {
    let compiled = Compiler::new()
        .model("SampledIntegral")
        .compile_str(SOURCE, "sampled_integral.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.1,
            dt: Some(0.0002),
            rtol: 1e-6,
            atol: 1e-6,
            solver_mode,
            ..SimOptions::default()
        },
    )
    .unwrap();
    let x = &result.data[result.names.iter().position(|name| name == "x").unwrap()];
    let sampled = &result.data[result
        .names
        .iter()
        .position(|name| name == "sampled")
        .unwrap()];
    let mut resets = 0;
    for index in 1..result.times.len() {
        if x[index] != 0.0 || x[index - 1] <= 0.0 {
            continue;
        }
        resets += 1;
        let time = result.times[index];
        let gap = time - result.times[index - 1];
        assert!(gap <= 32.0 * f64::EPSILON, "missing event-left sample");
        // The derivative is bounded by 70000; across one floating-point time
        // neighbor the continuous integral cannot jump by even 1e-8.
        assert!(
            (sampled[index] - x[index - 1]).abs() < 1e-8,
            "{solver_mode:?} at {time}: pre(x)={}, left x={}, gap={gap}",
            sampled[index],
            x[index - 1]
        );
    }
    assert_eq!(resets, 30, "every periodic reset must remain observable");
}

#[test]
fn bdf_sampled_integral_preserves_event_entry() {
    check_event_entry(SimSolverMode::Bdf);
}

#[test]
fn rk_sampled_integral_preserves_event_entry() {
    check_event_entry(SimSolverMode::RkLike);
}
