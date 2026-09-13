//! A resistive switching loop must settle on the physical side of zero current.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model SwitchCell
  parameter Boolean gate=false;
  Real voltage;
  Real s;
  Real current;
  Boolean fire;
  Boolean off(start=true, fixed=true);
equation
  fire = gate;
  off = s < 0 or pre(off) and not fire;
  voltage = s*(if off then 1 else 1e-5);
  current = s*(if off then 1e-5 else 1);
end SwitchCell;
model ResistiveCommutation
  SwitchCell cell[3](gate={false, false, true});
  Real voltage[3];
  Real outputVoltage;
  Real integral(start=0, fixed=true);
equation
  voltage = {1, -1, 0.1-time};
  for k in 1:3 loop
    cell[k].voltage = voltage[k]-outputVoltage;
  end for;
  outputVoltage = 20*sum(cell.current);
  der(integral) = cell[3].current;
end ResistiveCommutation;
"#;

fn check_resistive_commutation(solver_mode: SimSolverMode) {
    let compiled = Compiler::new()
        .model("ResistiveCommutation")
        .compile_str(SOURCE, "resistive_commutation.mo")
        .unwrap();
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.2,
            dt: Some(0.001),
            rtol: 1e-7,
            atol: 1e-9,
            solver_mode,
            ..Default::default()
        },
    )
    .unwrap();
    let off = result
        .names
        .iter()
        .position(|n| n == "cell[3].off")
        .unwrap();
    let current = result
        .names
        .iter()
        .position(|n| n == "cell[3].current")
        .unwrap();
    let integral = result.names.iter().position(|n| n == "integral").unwrap();
    for (index, &time) in result.times.iter().enumerate() {
        if (time - 0.1).abs() > 1e-8 {
            assert_eq!(
                result.data[off][index],
                f64::from(time > 0.1),
                "{solver_mode:?} at t={time}"
            );
        }
        let (expected_current, expected_integral) = analytic_response(time);
        assert!(
            (result.data[current][index] - expected_current).abs() < 1e-10,
            "{solver_mode:?}: current at t={time}"
        );
        assert!(
            (result.data[integral][index] - expected_integral).abs() < 2e-9,
            "{solver_mode:?}: integral at t={time}"
        );
    }
}

fn analytic_response(time: f64) -> (f64, f64) {
    let resistance = 20.0;
    let conductance = 1e-5;
    let on_resistance = 1e-5;
    let on = (1.0 + 2.0 * resistance * conductance)
        / (on_resistance + resistance + 2.0 * on_resistance * resistance * conductance);
    let off = conductance * (1.0 + 2.0 * resistance * conductance)
        / (1.0 + 3.0 * resistance * conductance);
    if time <= 0.1 {
        (on * (0.1 - time), on * (0.1 * time - 0.5 * time * time))
    } else {
        (
            off * (0.1 - time),
            on * 0.005 - off * 0.5 * (time - 0.1).powi(2),
        )
    }
}

#[test]
fn bdf_resistive_commutation_settles() {
    check_resistive_commutation(SimSolverMode::Bdf);
}

#[test]
fn rk_resistive_commutation_settles() {
    check_resistive_commutation(SimSolverMode::RkLike);
}
