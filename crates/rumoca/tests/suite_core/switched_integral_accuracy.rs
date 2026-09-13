//! MLS Appendix B: a located state event must preserve the continuous integral.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model SwitchedIntegral
  Real x(start=0, fixed=true);
  Boolean active;
equation
  active = sin(100*(time - 1.0/300)) > 0;
  der(x) = if active then 10000 else 0;
end SwitchedIntegral;
"#;

fn check_switched_integral(solver_mode: SimSolverMode) {
    let compiled = Compiler::new()
        .model("SwitchedIntegral")
        .compile_str(SOURCE, "switched_integral.mo")
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
    // There is one sine zero in this interval. Integrating each constant
    // branch gives this independent exact solution, including event samples.
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
fn bdf_switched_integral_preserves_accuracy() {
    check_switched_integral(SimSolverMode::Bdf);
}

#[test]
fn rk_switched_integral_preserves_accuracy() {
    check_switched_integral(SimSolverMode::RkLike);
}
