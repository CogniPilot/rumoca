//! Regression: a differential state whose derivative appears only in an
//! `initial equation` must still be prolonged for index reduction.
//!
//! `w_alias = der(phi)` binds `w_alias` to a state derivative, and
//! `der(w_alias) = a_start` in the `initial equation` seeds the initial
//! acceleration. `der(w_alias)` never enters a continuous equation, so the
//! differential signature over continuous rows alone assigns `w_alias` offset
//! zero. Because `w_alias` is a state it owns an integration slot, so its first
//! derivative must exist in the prolonged system; without that obligation the
//! coupled system (the alias, its two dynamics rows, and the acceleration seed)
//! is reported structurally singular and never reduces. This mirrors the
//! `Set_a_start`/`Set_w_start` blocks of `Modelica.Mechanics.Rotational`'s
//! `InitializeFlange`, the first index-reduction wall of the RobotR3 examples.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

// phi and v are an oscillator; w_alias = der(phi) is a derivative alias whose
// own derivative is constrained only in the initial equation. The reduced
// system integrates phi and v; w_alias tracks der(phi).
const MODEL: &str = r#"
within;
model DerivativeAliasInitialAcceleration
  Real phi;
  Real v(start = 0.0, fixed = true);
  Real w_alias;
  parameter Real j = 1.0;
  parameter Real c = 10.0;
initial equation
  der(w_alias) = 2.0;
equation
  der(phi) = v;
  j * der(v) = (-c) * phi;
  w_alias = der(phi);
end DerivativeAliasInitialAcceleration;
"#;

#[test]
fn state_derivative_seen_only_in_initial_equation_still_reduces() {
    let compiled = Compiler::new()
        .model("DerivativeAliasInitialAcceleration")
        .compile_str(MODEL, "DerivativeAliasInitialAcceleration.mo")
        .expect("compile to DAE should succeed");

    // Lowering this coupled system was reported structurally singular (EL005)
    // before the state-derivative offset obligation was applied; the formal
    // derivative recovery must now reduce it and evaluate at the model initial
    // point without error. The alias collapses onto the two integrated
    // coordinates (phi and v), so the reduced system carries exactly two states
    // where the unreduced system had left a third, undifferentiated one.
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .expect("reduced system should lower and evaluate");
    assert!(
        probe.report.error.is_none(),
        "eval error: {:?}",
        probe.report.error
    );
    assert_eq!(
        probe.state_names.len(),
        2,
        "the derivative alias must collapse onto two integrated states, got {:?}",
        probe.state_names
    );
    for slot in probe
        .report
        .derivatives
        .iter()
        .chain(&probe.report.solver_y)
    {
        assert!(
            slot.is_finite(),
            "reduced value {} is non-finite: {}",
            slot.name,
            slot.value
        );
    }
}
