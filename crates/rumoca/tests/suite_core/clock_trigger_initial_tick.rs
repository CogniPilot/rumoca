//! MLS §§16.3, 16.5.1 and 8.3.5: a held clock update can activate a Boolean when.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

#[test]
fn a_clock_tick_at_initial_time_activates_its_unclocked_sampler() {
    check("baseClock", 0.0);
}

#[test]
fn a_shifted_first_tick_keeps_the_sampler_initial_value_until_it_fires() {
    check("shiftSample(baseClock, 1, 2)", 0.01);
}

fn check(clock: &str, first_tick: f64) {
    let source = format!(
        r#"
model ClockTrigger
  Clock baseClock = Clock(1, 50);
  Clock selectedClock = {clock};
  Real u = 0.1 + time;
  Real sampled;
  Boolean toggle(start=false);
  Boolean held(start=false, fixed=true);
  Boolean trigger;
  discrete Real y;
  Integer initialCount(start=0, fixed=true);
equation
  sampled = sample(u, selectedClock);
  when selectedClock then
    toggle = not previous(toggle);
  end when;
  held = hold(toggle);
  trigger = change(held);
  when trigger then
    y = u;
  end when;
  when initial() then
    initialCount = pre(initialCount) + 1;
  end when;
initial equation
  y = 0;
end ClockTrigger;
"#
    );
    let compiled = Compiler::new()
        .model("ClockTrigger")
        .compile_str(&source, "clock_trigger.mo")
        .expect("clock-trigger fixture compiles");
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.055,
            dt: Some(0.005),
            ..Default::default()
        },
    )
    .expect("clock-trigger fixture simulates");
    assert!(result.times.len() > 1);
    let shift = if first_tick == 0.0 { 0 } else { 1 };
    for name in ["y", "initialCount"] {
        let column = result.names.iter().position(|n| n == name).unwrap();
        assert_eq!(result.data[column].len(), result.times.len());
        for (time, actual) in result.times.iter().zip(&result.data[column]) {
            let expected = if name == "initialCount" {
                1.0
            } else {
                (0..4)
                    .rev()
                    .map(|tick| f64::from(2 * tick + shift) / 100.0)
                    .find(|tick| tick <= time)
                    .map_or(0.0, |tick| 0.1 + tick)
            };
            assert!(
                (actual - expected).abs() < 1.0e-12,
                "{name} at {time}: expected {expected}, got {actual}"
            );
        }
    }
}
