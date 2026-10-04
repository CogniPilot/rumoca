//! A target that enters its residual only as the denominator of a nonzero
//! literal quotient is isolated exactly (`G = 1 / R` from `R = 1 / G`), so its
//! initial value never seeds a Newton step at the pole of the quotient.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const PERMEANCE: &str = "
model ReciprocalPermeance
  parameter Real c = 0.7;
  Real x(start = 1, fixed = true);
  Real R;
  Real G(start = 0);
equation
  der(x) = -x;
  (1 - c) * R = c * (1 + x);
  R = 1 / G;
end ReciprocalPermeance;";

#[test]
fn a_reciprocal_target_starting_at_its_pole_is_assigned_exactly() {
    let dae = Compiler::new()
        .model("ReciprocalPermeance")
        .compile_str(PERMEANCE, "reciprocal_isolation.mo")
        .unwrap()
        .dae;
    let result = simulate_dae_with_diagnostics(
        &dae,
        &SimOptions {
            solver_mode: SimSolverMode::Bdf,
            t_end: 1.0,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .unwrap();
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).unwrap();
        &result.data[index]
    };
    for (row, &time) in result.times.iter().enumerate() {
        let x = (-time).exp();
        let expected = 0.3 / (0.7 * (1.0 + x));
        let g = column("G")[row];
        assert!(
            (g - expected).abs() <= 1e-5 * expected,
            "G({time}) = {g}, expected {expected}"
        );
    }
}
