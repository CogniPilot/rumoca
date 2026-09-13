//! MLS Appendix B: switching coordinates retain their sign beside offset ports.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model InductiveCommutation
  Real current(start=0.003, fixed=true);
  Real potential;
  Real transistorVoltage;
  Real diodeVoltage;
  Real transistorS;
  Real diodeS;
  Real transistorCurrent;
  Real diodeCurrent;
  Boolean transistorOff(start=true, fixed=true);
  Boolean diodeOff(start=false, fixed=true);
equation
  transistorOff = transistorS < 0;
  diodeOff = diodeS < 0;
  transistorVoltage = potential + 50;
  diodeVoltage = -50 - potential;
  transistorVoltage = transistorS*(if transistorOff then 1 else 1e-5);
  diodeVoltage = diodeS*(if diodeOff then 1 else 1e-5);
  transistorCurrent = transistorS*(if transistorOff then 1e-5 else 1);
  diodeCurrent = diodeS*(if diodeOff then 1e-5 else 1);
  2e-5*(50-potential) + diodeCurrent = current + transistorCurrent;
  der(current) = potential - 100*current;
end InductiveCommutation;
"#;

fn check_inductive_commutation(solver_mode: SimSolverMode) {
    let compiled = Compiler::new()
        .model("InductiveCommutation")
        .compile_str(SOURCE, "inductive_commutation.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 4e-5,
            dt: Some(5e-7),
            rtol: 1e-9,
            atol: 1e-12,
            solver_mode,
            ..Default::default()
        },
    )
    .unwrap();
    let index = |name: &str| result.names.iter().position(|n| n == name).unwrap();
    let current = index("current");
    let transistor_off = index("transistorOff");
    let diode_off = index("diodeOff");
    let conductance = 100000.0 + 3e-5;
    let decay = 100.0 + 1.0 / conductance;
    let equilibrium = (-50.0 + 0.002 / conductance) / decay;
    for (row, &time) in result.times.iter().enumerate() {
        let expected = equilibrium + (0.003 - equilibrium) * (-decay * time).exp();
        assert!(
            (result.data[current][row] - expected).abs() < 1e-9,
            "{solver_mode:?}: current at t={time}"
        );
        if (expected - 0.002).abs() > 1e-8 {
            assert_eq!(
                result.data[transistor_off][row],
                f64::from(expected > 0.002),
                "{solver_mode:?}: transistor at t={time}"
            );
            assert_eq!(
                result.data[diode_off][row],
                f64::from(expected < 0.002),
                "{solver_mode:?}: diode at t={time}"
            );
        }
    }
}

#[test]
fn bdf_inductive_current_commutates_between_diode_and_transistor() {
    check_inductive_commutation(SimSolverMode::Bdf);
}

#[test]
fn rk_inductive_current_commutates_between_diode_and_transistor() {
    check_inductive_commutation(SimSolverMode::RkLike);
}
