//! End-to-end regression for MLS §3.7.4.5 Rule 1 at zero flow.
//!
//! §3.7.4.5 says the transformation exists because *"equations with the
//! `semiLinear` function become underdetermined if the first argument (`x`)
//! becomes zero, i.e., there is an infinite number of solutions"*, and Rule 1
//! is the recommended way to *"select one meaningful solution in such cases"*.
//! Nothing about that is visible in one lowered call: it only shows up when a
//! model runs a `semiLinear` pair through an interval where `x` is exactly
//! zero, which is what this fixture does.
//!
//! `ZeroFlowMixing` is the two-port mixing node of
//! `Modelica.Thermal.FluidHeatFlow.BaseClasses.TwoPort`, written flat: two
//! ports each stating `H = semiLinear(m, node_h, h)`, joined by the MLS §9.2
//! connection balances `ma + mb = 0` and `Ha + Hb = 0`. The port enthalpies
//! `ha` and `hb` are kept apart — one constant, one ramping — so a node
//! enthalpy that is merely *held* by the solver is distinguishable from one an
//! equation determines.
//!
//! The measured defect this pins: on
//! `Modelica.Thermal.FluidHeatFlow.Examples.PumpDropOut`, without the rule
//! `pipe.flowPort_b.h` freezes at 304.6364 across the whole `V_flow == 0`
//! window while `pipe.T` climbs to 315.32; with it the channel tracks
//! `pipe.h` and reads 310.2640 at `t = 0.5` against OMC's 310.2631. The repo
//! comparator scores that as `max_channel_bounded_normalized_l1`
//! 2.589e-02 → 4.913e-05 for the model.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const ZERO_FLOW_MIXING: &str = r"
model ZeroFlowMixing
  Real m;
  Real ha;
  Real hb;
  Real hn;
  Real Ha;
  Real Hb;
  Real ma;
  Real mb;
  Real T(start = 5.0, fixed = true);
equation
  ma = m;
  ma + mb = 0;
  Ha + Hb = 0;
  Ha = semiLinear(ma, hn, ha);
  Hb = semiLinear(mb, hn, hb);
  ha = 2.0;
  hb = T;
  der(T) = 1.0;
  m = if time < 1.0 then 1.0 else 0.0;
end ZeroFlowMixing;
";

fn simulate(source: &str, model: &str, t_end: f64) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .expect("the fixture compiles");
    let options = SimOptions {
        t_end,
        ..SimOptions::default()
    };
    simulate_dae_with_diagnostics(&compiled.dae, &options).expect("the fixture simulates")
}

fn channel<'sim>(sim: &'sim SimResult, name: &str) -> &'sim [f64] {
    let index = sim
        .names
        .iter()
        .position(|channel| channel == name)
        .unwrap_or_else(|| panic!("`{name}` missing from channels {:?}", sim.names));
    &sim.data[index]
}

/// Samples of `name` at times satisfying `when`, paired with the time.
fn samples(sim: &SimResult, name: &str, when: impl Fn(f64) -> bool) -> Vec<(f64, f64)> {
    let values = channel(sim, name);
    sim.times
        .iter()
        .copied()
        .zip(values.iter().copied())
        .filter(|(time, _)| when(*time))
        .collect()
}

#[test]
fn zero_flow_node_enthalpy_is_determined_by_the_rule_one_selector_not_held_by_the_solver() {
    let sim = simulate(ZERO_FLOW_MIXING, "ZeroFlowMixing", 2.0);

    let flowing = samples(&sim, "hn", |time| (0.1..0.9).contains(&time));
    assert!(!flowing.is_empty(), "the flowing interval produced samples");
    for (time, node) in &flowing {
        let upstream = 5.0 + time;
        assert!(
            (node - upstream).abs() < 1e-6,
            "while `x < 0` the node takes the chain's other end (`hb = T = {upstream}`), \
             got {node} at t={time}"
        );
    }

    // MLS §3.7.4.5 Rule 1 writes `s1 = if x >= 0 then sa else sb`, so at exactly
    // `x = 0` the node takes `sa`. Here that is the constant end `ha = 2.0`,
    // and it must hold for the WHOLE window: a node the solver merely carried
    // over would sit at its pre-window value (`hb = 6.0` at `t = 1`) or drift
    // with `hb`, never at 2.0.
    let stalled = samples(&sim, "hn", |time| (1.05..=2.0).contains(&time));
    assert!(!stalled.is_empty(), "the zero-flow window produced samples");
    for (time, node) in &stalled {
        assert!(
            (node - 2.0).abs() < 1e-9,
            "at `x == 0` Rule 1 selects the chain head `sa = ha = 2.0`, got {node} at t={time}"
        );
    }

    // The enthalpy flow itself is continuous through the transition: MLS
    // §3.7.4.5's operator returns `smooth(0, ...)`, and the collapsed
    // `y = semiLinear(x, sa, sb)` Rule 1 leaves behind still is that operator.
    for (time, flow) in samples(&sim, "Ha", |time| (1.05..=2.0).contains(&time)) {
        assert!(
            flow.abs() < 1e-9,
            "zero mass flow carries zero enthalpy flow, got {flow} at t={time}"
        );
    }
}

#[test]
fn the_rule_one_selector_is_not_a_state_event_owner() {
    let compiled = Compiler::new()
        .model("ZeroFlowMixing")
        .compile_str(ZERO_FLOW_MIXING, "ZeroFlowMixing.mo")
        .expect("the fixture compiles");

    compiled.dae.inspect(|view| {
        // The only crossing in the fixture is the `time < 1.0` driver, which MLS
        // §8.5 schedules as a time event. The `x >= 0` relation Rule 1 lifts out
        // of the operator's own `smooth(0, ...)` adds no root: rumoca keeps the
        // §3.7.5 freedom it already takes for `semiLinear`, and OMC's generated
        // C for `FluidHeatFlow.Examples.PumpDropOut` lists no zero crossing for
        // its Rule 1 selectors either.
        assert_eq!(
            view.relation_count(),
            0,
            "no state-event relation is owned by the fixture"
        );
        assert_eq!(view.root_count(), 0);
        assert_eq!(view.time_event_count(), 1, "only the `time < 1.0` driver");
    });
}
