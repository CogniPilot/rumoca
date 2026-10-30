//! A refreshed algebraic value that turns non-finite is reported by name,
//! after many finite refreshes have validated the same targets.

use super::{SimOptions, SimSolverMode, compile};

const MODEL: &str = r#"
model RefreshValidation
  Real a;
  Real b;
  Real x(start = 0, fixed = true);
equation
  a = sqrt(1 - time);
  b = a * a + x;
  der(x) = b;
end RefreshValidation;
"#;

#[test]
fn a_refreshed_value_turning_nan_is_reported_by_name() {
    let model = compile(MODEL, "RefreshValidation");
    let opts = SimOptions {
        t_end: 2.0,
        dt: Some(0.01),
        solver_mode: SimSolverMode::VelImplicit,
        ..SimOptions::default()
    };
    let error = crate::simulate_with_diagnostics(&model, &opts)
        .expect_err("sqrt(1 - time) is NaN after t = 1");
    let message = error.to_string();
    assert!(
        message.contains("non-finite (NaN) value computed for `a`"),
        "{message}"
    );
}
