//! A source jump at an event is a jump of `delay(u, d)` exactly `d` later
//! (MLS §3.7.2), and `integer` of that delayed value is an event-generating
//! step (MLS §3.7.1, §8.5), so it switches at the transported instant.
//!
//! The pure-discrete fixture below is stepped at the delay time, so its
//! periodic time events are reached by roundoff-coincident adoption rather
//! than by an exact step end. Each tick must still record the source jump as
//! an event-coordinate left/right pair and switch the delayed integer one
//! delay later, as the transport delay of `Modelica.Electrical.Digital` does.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
model DelayedToggle
  Integer c(start = 0, fixed = true);
  Real xr;
  Integer y;
equation
  when sample(0.5, 1) then
    c = 1 - pre(c);
  end when;
  xr = pre(c);
  y = integer(delay(xr, 0.001));
end DelayedToggle;
"#;

#[test]
fn every_periodic_source_jump_switches_the_delayed_integer() {
    let compiled = Compiler::new()
        .model("DelayedToggle")
        .compile_str(SOURCE, "DelayedToggle.mo")
        .unwrap_or_else(|error| panic!("DelayedToggle compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 10.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("DelayedToggle simulates: {error:?}"));
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == "y")
        .expect("DelayedToggle exposes y");
    let value_at = |time: f64| {
        let row = result
            .times
            .iter()
            .rposition(|sample| *sample <= time)
            .expect("a sample at or before the requested time");
        result.data[column][row]
    };
    // Tick k at 0.5 + k sets c to (k + 1) mod 2; y follows 0.001 later and
    // holds until the next tick's delayed jump.
    let mismatches: Vec<(f64, f64, f64)> = (0..9)
        .map(|k| {
            let time = 1.0 + f64::from(k);
            let expected = f64::from((k + 1) % 2);
            (time, expected, value_at(time))
        })
        .filter(|(_, expected, actual)| expected != actual)
        .collect();
    assert!(
        mismatches.is_empty(),
        "delayed integer missed transported jumps (time, expected, actual): {mismatches:?}"
    );
}
