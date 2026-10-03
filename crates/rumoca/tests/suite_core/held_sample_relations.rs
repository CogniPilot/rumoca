//! A relation over a value held between `sample` ticks is still a relation of
//! the continuous-time partition (MLS §8.5).
//!
//! `t_i` changes only on the ticks of `when sample(0, period)`, but `time >=
//! t_i + t_width` also reads `time` and the algebraic `t_width`, so its truth
//! changes between ticks and the crossing is an event of its own. The digital
//! clock source of `Modelica.Electrical.Digital.Sources.DigitalClock` has this
//! shape.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
model HeldPulse
  parameter Real period = 0.2;
  Integer y;
  Real t_i(start = 0, fixed = true);
  Real t_width = 0.1;
equation
  when sample(0, period) then
    t_i = time;
  end when;
  y = if time >= t_i + t_width then 0 else 1;
end HeldPulse;
"#;

#[test]
fn a_relation_over_a_held_sample_value_owns_its_crossing() {
    let compiled = Compiler::new()
        .model("HeldPulse")
        .compile_str(SOURCE, "HeldSampleRelations.mo")
        .unwrap_or_else(|error| panic!("HeldPulse compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.5,
            ..SimOptions::default()
        },
    )
    .expect("HeldPulse simulates");
    let index = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("HeldPulse exposes y");
    let falls = result
        .times
        .iter()
        .skip(1)
        .zip(result.data[index].windows(2))
        .filter(|(_, pair)| pair[0] == 1.0 && pair[1] == 0.0)
        .map(|(time, _)| *time)
        .collect::<Vec<_>>();
    assert_eq!(falls.len(), 3, "y falls once per period: {falls:?}");
    for (fall, expected) in falls.iter().zip([0.1, 0.3, 0.5]) {
        assert!(
            (fall - expected).abs() < 1.0e-6,
            "y falls at the located crossing {expected}, not at {fall}"
        );
    }
}
