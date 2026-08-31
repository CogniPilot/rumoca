//! Fail-closed regression for holonomic reduction with stated initial values.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, lower_dae_for_simulation};

const CARTESIAN_PENDULUM: &str = r#"
model CartesianPendulum
  parameter Real length = 1.0;
  parameter Real gravity = 9.81;
  Real x(start = 1.0, fixed = true);
  Real y(start = 0.0, fixed = true);
  Real vx(start = 0.0, fixed = true);
  Real vy(start = 0.0, fixed = true);
  Real lambda;
equation
  der(x) = vx;
  der(y) = vy;
  der(vx) = -lambda*x;
  der(vy) = -gravity - lambda*y;
  x*x + y*y = length*length;
end CartesianPendulum;
"#;

#[test]
fn fixed_state_holonomic_reduction_fails_before_simulation() {
    let compiled = Compiler::new()
        .model("CartesianPendulum")
        .compile_str(CARTESIAN_PENDULUM, "CartesianPendulum.mo")
        .expect("the source model constructs its checked DAE");
    let error = lower_dae_for_simulation(compiled.dae(), &SimOptions::default())
        .expect_err("reduction must not discard an untransferred fixed-state equation");
    let message = error.to_string();
    assert!(
        message.contains("would discard the stated initial value of `x`")
            && message.contains("fixed = true"),
        "expected the typed fixed-state refusal, got: {message}"
    );
}
