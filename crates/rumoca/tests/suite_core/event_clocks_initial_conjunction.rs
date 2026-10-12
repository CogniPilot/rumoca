//! MLS §16.3/16.5: an initialization clock tick remains observable by its latch.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model FirstConjunctiveTick
  Modelica.Blocks.Sources.Sine sine_angle_input(amplitude=10, f=1);
  Modelica.Blocks.Sources.Cosine cosine_angle_input(amplitude=10, f=1);
  Modelica.Clocked.ClockSignals.Clocks.Rotational.RotationalClock rotational_clock_1;
  Modelica.Clocked.ClockSignals.Clocks.Rotational.RotationalClock rotational_clock_2;
  Modelica.Clocked.ClockSignals.Clocks.Logical.ConjunctiveClock conjunctiveClock;
  Modelica.Clocked.RealSignals.Sampler.SampleClocked sample_conjunctive;
equation
  connect(sine_angle_input.y, rotational_clock_1.angle);
  connect(cosine_angle_input.y, rotational_clock_2.angle);
  rotational_clock_1.trigger_interval = 3;
  rotational_clock_2.trigger_interval = 3;
  connect(rotational_clock_1.y, conjunctiveClock.u[1]);
  connect(rotational_clock_2.y, conjunctiveClock.u[2]);
  connect(conjunctiveClock.y, sample_conjunctive.clock);
  connect(sample_conjunctive.u, sine_angle_input.y);
  annotation(experiment(StopTime=0.13));
end FirstConjunctiveTick;
"#;

#[test]
fn first_conjunctive_tick_retains_the_initialized_input_latch() {
    let root = super::native_linear_solve::msl_root().expect("the required MSL fixture exists");
    let compiled = Compiler::new()
        .model("FirstConjunctiveTick")
        .source_root(root.to_string_lossy().as_ref())
        .compile_str(SOURCE, "FirstConjunctiveTick.mo")
        .expect("the original rotational and logical clock components compile");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.13,
            dt: Some(0.01),
            ..SimOptions::default()
        },
    )
    .expect("the first conjunction cycle completes");
    let value_at = |name: &str, time: f64| {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        let row = result
            .times
            .iter()
            .rposition(|candidate| *candidate <= time + 1e-12)
            .unwrap();
        result.data[column][row]
    };
    assert_eq!(value_at("rotational_clock_2.update_offset.y", 0.02), 10.0);
    assert_eq!(
        value_at("conjunctiveClock.input_ticked[2].input_hold.y", 0.02),
        1.0
    );
    assert_eq!(value_at("conjunctiveClock.input_memory[2].Q", 0.02), 1.0);
    let first_tick = 0.3_f64.asin() / (2.0 * std::f64::consts::PI);
    assert!(first_tick > 0.04 && first_tick < 0.05);
    assert_eq!(value_at("sample_conjunctive.y", 0.04), 0.0);
    assert!((value_at("sample_conjunctive.y", 0.06) - 3.0).abs() < 1e-7);
}

#[test]
fn an_unticked_clock_to_boolean_retains_its_false_hold_start() {
    let root = super::native_linear_solve::msl_root().expect("the required MSL fixture exists");
    let compiled = Compiler::new()
        .model("UntickedClockHold")
        .source_root(root.to_string_lossy().as_ref())
        .compile_str(
            r#"model UntickedClockHold
              Boolean gate = time > 1;
              Clock c = Clock(gate);
              Modelica.Clocked.BooleanSignals.NonPeriodic.ClockToBoolean conversion;
            equation
              conversion.u = c;
            end UntickedClockHold;"#,
            "UntickedClockHold.mo",
        )
        .expect("the inactive event clock compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 0.05,
            dt: Some(0.01),
            ..SimOptions::default()
        },
    )
    .expect("an inactive clock holds its declared start");
    assert!(result.times.len() >= 6);
    for name in [
        "conversion.input_sample.y",
        "conversion.input_tick.y",
        "conversion.input_hold.y",
        "conversion.y",
    ] {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap();
        assert!(
            result.data[column].iter().all(|value| *value == 0.0),
            "{name}"
        );
    }
}
