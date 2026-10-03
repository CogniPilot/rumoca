//! `delay(u, d)` before `time.start + d` is `u(time.start)` (MLS §3.7.2).
//!
//! The start value of `u` is the one the initial event iteration settles, so
//! a delay of an algebraic source reads the reconstructed source rather than
//! its declaration seed, and a discrete row reading the delay reads the value
//! the iteration settles. The transport delay of
//! `Modelica.Electrical.Digital.Delay.TransportDelay` indexes a Logic table
//! with `integer(delay(Integer(pre(x)), d))`. A loop through a delay identity
//! settles to its initial solution within the event iteration tolerance,
//! the same change test every other settle step uses.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
package P
  type Logic = enumeration('U', 'X', '0', '1');
  model AlgebraicSource
    Real xr;
    Real xd;
    Integer k;
  equation
    xr = 3 + 0.1 * time;
    xd = delay(xr, 0.1);
    k = integer(xd);
  end AlgebraicSource;
  model TransportIndex
    constant Logic values[:] = Logic.'U':Logic.'1';
    Logic x(start = Logic.'U', fixed = true);
    Logic delayed;
    Integer k;
    Real xr;
  equation
    x = if time < 0.3 then Logic.'0' else Logic.'1';
    xr = Integer(pre(x));
    k = integer(delay(xr, 0.1));
    delayed = values[k];
  end TransportIndex;
  model IdentityLoop
    Real x;
    Integer k;
  equation
    x = 1 + 0.5 * delay(x, 0.1);
    k = if x > 1.5 then 1 else 0;
  end IdentityLoop;
end P;
"#;

fn value_at(model: &str, name: &str, time: f64) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "DelayInitialValues.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 0.6,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"));
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{model} exposes {name}"));
    let row = result
        .times
        .iter()
        .rposition(|sample| *sample <= time)
        .expect("a sample at or before the requested time");
    result.data[column][row]
}

#[test]
fn a_delay_of_an_algebraic_starts_at_its_settled_value() {
    assert_eq!(value_at("P.AlgebraicSource", "k", 0.05), 3.0);
}

#[test]
fn a_discrete_row_reads_the_delay_the_initial_event_settles() {
    assert_eq!(value_at("P.TransportIndex", "k", 0.05), 3.0);
    assert_eq!(value_at("P.TransportIndex", "delayed", 0.05), 3.0);
    assert_eq!(value_at("P.TransportIndex", "k", 0.55), 4.0);
}

#[test]
fn initialization_solves_a_loop_through_a_delay_identity() {
    // delay(x, d) = x at the start, so x = 1 + 0.5 * x gives x = 2.
    assert!((value_at("P.IdentityLoop", "x", 0.05) - 2.0).abs() < 1.0e-6);
    assert_eq!(value_at("P.IdentityLoop", "k", 0.05), 1.0);
}
