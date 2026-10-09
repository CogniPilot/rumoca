//! Trajectory sensitivities of Modelica Standard Library circuits and
//! mechanics, against closed forms derived by hand (SPEC_0033 section 5).

use rumoca_sim::{SimOptions, SimResult};

use super::msl_sim_regression::require_msl_compiler;

fn options(t_end: f64, dt: f64) -> SimOptions {
    SimOptions {
        t_end,
        dt: Some(dt),
        rtol: 1.0e-9,
        atol: 1.0e-11,
        ..SimOptions::default()
    }
}

fn column<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("no column `{name}` in {:?}", result.names));
    &result.data[index]
}

fn assert_close(got: f64, want: f64, tolerance: f64, what: &str) {
    assert!(
        (got - want).abs() <= tolerance * (1.0 + want.abs()),
        "{what}: got {got}, want {want}"
    );
}

/// A constant source charging a capacitor through a resistor: `tau = R C`,
/// `v = V (1 - e^{-t/tau})`, so with `d = dv/dtau = -V (t/tau^2) e^{-t/tau}`:
/// `dv/dR = C d`, `dv/dC = R d`, `dv/dV = 1 - e^{-t/tau}`.
const RC_CIRCUIT: &str = "model RcCircuit
  Modelica.Electrical.Analog.Basic.Resistor R(R = 1000);
  Modelica.Electrical.Analog.Basic.Capacitor C(C = 1.0e-3, v(start = 0, fixed = true));
  Modelica.Electrical.Analog.Sources.ConstantVoltage V(V = 5);
  Modelica.Electrical.Analog.Basic.Ground G;
equation
  connect(V.p, R.p);
  connect(R.n, C.p);
  connect(C.n, V.n);
  connect(V.n, G.p);
end RcCircuit;
";

#[test]
fn rc_circuit_sensitivities_match_the_closed_form() {
    let compiled = require_msl_compiler()
        .model("RcCircuit")
        .compile_str(RC_CIRCUIT, "RcCircuit.mo")
        .unwrap_or_else(|error| panic!("{error}"));
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(
        &compiled.dae,
        &options(2.0, 0.5),
        &["R.R".to_string(), "C.C".to_string(), "V.V".to_string()],
    )
    .unwrap_or_else(|error| panic!("{error}"));
    let (r, c, volts) = (1000.0_f64, 1.0e-3_f64, 5.0_f64);
    let tau = r * c;
    let (dv_dr, dv_dc, dv_dv) = (
        column(&trace, "d(C.v)/d(R.R)"),
        column(&trace, "d(C.v)/d(C.C)"),
        column(&trace, "d(C.v)/d(V.V)"),
    );
    for (index, t) in trace.times.iter().enumerate() {
        let decay = (-t / tau).exp();
        let d_tau = -volts * (t / (tau * tau)) * decay;
        assert_close(dv_dr[index], c * d_tau, 1.0e-6, "dv/dR");
        assert_close(dv_dc[index], r * d_tau, 1.0e-6, "dv/dC");
        assert_close(dv_dv[index], 1.0 - decay, 1.0e-6, "dv/dV");
    }
}

/// The resistor's temperature assertion reads the solver vector but owns no
/// value the continuous rows read, so the circuit is differentiable, and the
/// forward and adjoint gradients of the integrated capacitor voltage agree.
#[test]
fn rc_circuit_adjoint_gradient_matches_the_closed_form() {
    let compiled = require_msl_compiler()
        .model("RcCircuit")
        .compile_str(RC_CIRCUIT, "RcCircuit.mo")
        .unwrap_or_else(|error| panic!("{error}"));
    // J = integral_0^T v dt = V (T - tau (1 - e^{-T/tau})), so
    // dJ/dR = C dJ/dtau with dJ/dtau = -V (1 - e^{-T/tau}) + V (T/tau) e^{-T/tau}.
    let objective = rumoca_sim::TrajectoryObjective {
        running: vec![rumoca_sim::RunningTerm {
            variable: "C.v".to_string(),
            weight: 1.0,
            kind: rumoca_sim::RunningKind::Value,
        }],
        terminal: Vec::new(),
    };
    let (r, c, volts, t_end) = (1000.0_f64, 1.0e-3_f64, 5.0_f64, 2.0_f64);
    let tau = r * c;
    let decay = (-t_end / tau).exp();
    let value = volts * (t_end - tau * (1.0 - decay));
    let dj_dtau = -volts * (1.0 - decay) + volts * (t_end / tau) * decay;
    for adjoint in [false, true] {
        let gradient = rumoca_sim::trajectory_objective_gradient_for_dae(
            &compiled.dae,
            &options(t_end, 0.5),
            &["R.R".to_string(), "C.C".to_string()],
            &objective,
            adjoint,
        )
        .unwrap_or_else(|error| panic!("adjoint={adjoint}: {error}"));
        assert_close(gradient.value, value, 1.0e-6, "J");
        assert_close(gradient.gradient[0], c * dj_dtau, 1.0e-5, "dJ/dR");
        assert_close(gradient.gradient[1], r * dj_dtau, 1.0e-5, "dJ/dC");
    }
}

/// A damped oscillator from Rotational components with `J = 1`: the angle is
/// the displacement of `x'' = -c x - d x'` with `x(0) = 1`, `x'(0) = 0`. With
/// `zeta = d/2` and `w = sqrt(c - zeta^2)` the closed form is the one pinned in
/// `trajectory_sensitivity_test`, with `c` the stiffness and `d` the damping.
const ROTATIONAL_OSCILLATOR: &str = "model RotationalOscillator
  Modelica.Mechanics.Rotational.Components.Inertia inertia(J = 1, phi(start = 1, fixed = true), w(start = 0, fixed = true));
  Modelica.Mechanics.Rotational.Components.Spring spring(c = 4);
  Modelica.Mechanics.Rotational.Components.Damper damper(d = 0.6);
  Modelica.Mechanics.Rotational.Components.Fixed fixed;
equation
  connect(inertia.flange_b, spring.flange_a);
  connect(spring.flange_b, fixed.flange);
  connect(inertia.flange_b, damper.flange_a);
  connect(damper.flange_b, fixed.flange);
end RotationalOscillator;
";

#[test]
fn rotational_oscillator_sensitivities_match_the_closed_form() {
    let compiled = require_msl_compiler()
        .model("RotationalOscillator")
        .compile_str(ROTATIONAL_OSCILLATOR, "RotationalOscillator.mo")
        .unwrap_or_else(|error| panic!("{error}"));
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(
        &compiled.dae,
        &options(2.0, 0.5),
        &["spring.c".to_string(), "damper.d".to_string()],
    )
    .unwrap_or_else(|error| panic!("{error}"));
    let (c, d) = (4.0_f64, 0.6_f64);
    let zeta = d / 2.0;
    let w = (c - zeta * zeta).sqrt();
    let (dphi_dc, dphi_dd) = (
        column(&trace, "d(inertia.phi)/d(spring.c)"),
        column(&trace, "d(inertia.phi)/d(damper.d)"),
    );
    for (index, t) in trace.times.iter().enumerate() {
        let decay = (-zeta * t).exp();
        let (sin, cos) = (w * t).sin_cos();
        let x = decay * (cos + zeta / w * sin);
        let dx_dw = decay * (-t * sin - zeta / (w * w) * sin + zeta / w * t * cos);
        let dx_dzeta = -t * x + decay * sin / w;
        assert_close(dphi_dc[index], dx_dw / (2.0 * w), 1.0e-6, "dphi/dc");
        assert_close(
            dphi_dd[index],
            (dx_dzeta - zeta / w * dx_dw) / 2.0,
            1.0e-6,
            "dphi/dd",
        );
    }
}
