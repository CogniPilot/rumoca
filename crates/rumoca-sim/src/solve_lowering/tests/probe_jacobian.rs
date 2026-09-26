//! The `--inspect jacobian` state Jacobian is the kernel's exact directional
//! derivative, through the algebraic projection's seed linearization.
//!
//! `z` solves `z + 0.1*z^3 = x*y`, so `dz/dx = y / (1 + 0.3*z^2)` and
//! `dz/dy = x / (1 + 0.3*z^2)`; the probe's dense Jacobian of
//! `der(x) = -2*x + z`, `der(y) = x*z - y` must match the implicit function
//! theorem at the start point to roundoff.

use super::compile;
use crate::SimOptions;
use crate::solve_lowering::probe::jacobian_for_dae;

const MODEL: &str = "model ProbeJacobian
  Real x(start = 0.5, fixed = true);
  Real y(start = 0.2, fixed = true);
  Real z;
equation
  der(x) = -2*x + z;
  der(y) = x*z - y;
  z + 0.1*z^3 = x*y;
end ProbeJacobian;";

#[test]
fn inspect_jacobian_is_the_exact_state_derivative() {
    let dae = compile(MODEL, "ProbeJacobian");
    let probe = jacobian_for_dae(&dae, &SimOptions::default(), &[], 0.0)
        .expect("the state Jacobian probe evaluates");
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    let (x, y) = (0.5, 0.2);
    let mut z: f64 = 0.0;
    for _ in 0..50 {
        z -= (z + 0.1 * z * z * z - x * y) / (1.0 + 0.3 * z * z);
    }
    let slope = 1.0 + 0.3 * z * z;
    let (dz_dx, dz_dy) = (y / slope, x / slope);
    let expected = [[-2.0 + dz_dx, dz_dy], [z + x * dz_dx, x * dz_dy - 1.0]];
    let index = |name: &str| {
        probe
            .report
            .state_labels
            .iter()
            .position(|label| label == name)
            .unwrap_or_else(|| panic!("state {name} in {:?}", probe.report.state_labels))
    };
    let order = [index("x"), index("y")];
    for (row, expected_row) in expected.iter().enumerate() {
        for (column, expected) in expected_row.iter().enumerate() {
            let actual = probe.report.matrix[order[row]][order[column]];
            assert!(
                (actual - expected).abs() <= 1e-12 * expected.abs().max(1.0),
                "d(der row {row})/d(state {column}): {actual:e} vs {expected:e}"
            );
        }
    }
}
