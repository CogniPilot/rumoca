//! An affine block whose row is scaled by a solver coordinate the block does
//! not solve, the MSL resistor's `v = R_actual*i`: the block's linearization
//! is retained while that coordinate keeps its bits, and formed again when it
//! moves.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};
use rumoca_solver::{
    AffineLinearizationCounts, affine_linearization_counts, reset_affine_linearization_counts,
};

/// The switching circuit of `affine_switching_circuit`, its series resistance
/// the algebraic `actualResistance` of a temperature that is a state.
const SOURCE: &str = r#"
model HeatedSwitchingCircuit
  parameter Real resistance = 20;
  parameter Real alpha = 0.004;
  parameter Real heating = 0;
  parameter Real snubberResistance = 100;
  parameter Real capacitance = 0.005;
  parameter Real onResistance = 1e-5;
  parameter Real offConductance = 1e-5;
  parameter Real threshold = 0.8;
  parameter Real unitVoltage = 1;
  parameter Real unitCurrent = 1;
  Real temperature(start=300, fixed=true);
  Real actualResistance;
  Real capacitorVoltage(start=0, fixed=true);
  Real capacitorCurrent;
  Real voltage;
  Real current;
  Real forwardCurrent;
  Real reverseCurrent;
  Real forwardSwitch;
  Real reverseSwitch;
  Boolean forwardOff(start=true, fixed=true);
  Boolean reverseOff(start=true, fixed=true);
  Boolean fire;
equation
  der(temperature) = heating;
  actualResistance = resistance*(1 + alpha*(temperature - 300));
  fire = (time >= 0.1 and time < 0.225) or (time >= 0.35 and time < 0.475);
  forwardOff = forwardSwitch < 0 or pre(forwardOff) and not fire;
  reverseOff = reverseSwitch < 0 or pre(reverseOff) and not fire;
  voltage = forwardSwitch*unitVoltage*(if forwardOff then 1 else onResistance) + threshold;
  forwardCurrent = forwardSwitch*unitCurrent*(if forwardOff then offConductance else 1)
                   + offConductance*threshold;
  -voltage = reverseSwitch*unitVoltage*(if reverseOff then 1 else onResistance) + threshold;
  reverseCurrent = reverseSwitch*unitCurrent*(if reverseOff then offConductance else 1)
                   + offConductance*threshold;
  voltage = 5*sin(4*3.141592653589793*time) - actualResistance*current;
  current = forwardCurrent - reverseCurrent + capacitorCurrent;
  voltage = snubberResistance*capacitorCurrent + capacitorVoltage;
  capacitance*der(capacitorVoltage) = capacitorCurrent;
end HeatedSwitchingCircuit;
"#;

/// The counts of a run with the temperature rising at `heating`, and the
/// number of output points at which a switch changed.
fn run(heating: f64) -> (AffineLinearizationCounts, usize) {
    let compiled = Compiler::new()
        .model("HeatedSwitchingCircuit")
        .compile_str(SOURCE, "heated_switching_circuit.mo")
        .unwrap();
    reset_affine_linearization_counts();
    // Debug builds form every reused linearization afresh and require the
    // reuse to be bitwise that formation, so the run is also the exactness
    // check.
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.6,
            dt: Some(0.002),
            rtol: 1e-8,
            atol: 1e-8,
            solver_mode: SimSolverMode::Bdf,
            param_overrides: vec![("heating".to_string(), heating)],
            ..Default::default()
        },
    )
    .unwrap();
    let counts = affine_linearization_counts();
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).unwrap();
        &result.data[index]
    };
    let changes = ["forwardOff", "reverseOff"]
        .into_iter()
        .map(|name| column(name).windows(2).filter(|w| w[0] != w[1]).count())
        .sum();
    (counts, changes)
}

#[test]
fn a_block_scaled_by_a_held_outside_coordinate_is_formed_only_as_its_switches_change() {
    let (counts, changes) = run(0.0);
    eprintln!("held: {counts:?}, switch changes {changes}");
    assert!(changes >= 2, "the switches open and close: {changes}");
    assert!(counts.reused > 0, "{counts:?}");
    assert!(
        counts.formed <= 8 * (changes as u64 + 1),
        "formations follow the switch changes, not the evaluations: {counts:?}, {changes} changes"
    );
    assert!(counts.reused > 20 * counts.formed, "{counts:?}");
}

#[test]
fn a_block_scaled_by_a_moving_outside_coordinate_is_formed_again_as_it_moves() {
    let (held, _) = run(0.0);
    let (moving, _) = run(1.0);
    eprintln!("moving: {moving:?}");
    assert!(
        moving.formed > 10 * held.formed,
        "a coefficient that moves every step gives a new linearization: {moving:?} against {held:?}"
    );
}
