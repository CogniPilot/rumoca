//! Regression for the geared algebraic torque loop (issue #38).
//!
//! A gear couples two inertias: the torque equation is amplified by the gear
//! ratio (`0 = ratio*ga + gb`), the upstream inertia's state is demoted to a
//! constrained dummy (`w1 = ratio*w2`), and the remaining torques form a
//! coupled algebraic block whose unknowns do not correspond positionally to
//! its rows. The refresh used to pair block rows with unknowns by position
//! and silently accept rows that could not determine their paired variable,
//! converging to a wrong but stable solution (a2 = tau/4 instead of
//! ratio*tau/(ratio^2*J1 + J2) = 2*tau/5).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const MINI_GEAR: &str = r#"
model MiniGear
  parameter Real ratio = 2;
  parameter Real J1 = 1;
  parameter Real J2 = 1;
  parameter Real tau = 1;
  parameter Real d = 0.1;
  Real w1;
  Real w2(start = 0, fixed = true);
  Real a2;
  Real fa;
  Real fb;
  Real ga;
  Real gb;
equation
  w1 = ratio * w2;
  der(w2) = a2;
  J1 * der(w1) = fa + fb;
  0 = ratio * ga + gb;
  fb + ga = 0;
  gb + J2 * a2 + d * w2 = 0;
  fa = tau;
end MiniGear;
"#;

#[test]
fn gear_torque_loop_converges_to_physical_solution() {
    let compiled = Compiler::new()
        .model("MiniGear")
        .compile_str(MINI_GEAR, "MiniGear.mo")
        .expect("compile MiniGear");

    let opts = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let sim = simulate_dae_with_diagnostics(&compiled.dae, &opts).expect("simulate MiniGear");

    let w2_idx = sim
        .names
        .iter()
        .position(|name| name == "w2")
        .expect("w2 in outputs");
    let w2_end = *sim.data[w2_idx].last().expect("samples");
    // (ratio^2*J1 + J2)*a2 = ratio*tau - (ratio/2)*d*w2 reduces to
    // w2' = 0.4 - 0.02*w2, so w2(t) = 20*(1 - exp(-0.02*t)). The damping
    // term keeps the state Jacobian honestly nonzero (a constant-derivative
    // model trips the known BDF zero-Jacobian issue, which is not what this
    // test pins). With the old mis-paired refresh the gear amplification was
    // lost (a2 -> tau/4 family), which this trajectory detects at >1e-2.
    let expected = 20.0 * (1.0 - (-0.02_f64).exp());
    assert!(
        (w2_end - expected).abs() < 1.0e-6,
        "w2(1) = {w2_end}, expected {expected}: the gear torque amplification \
         must survive lowering"
    );
}
