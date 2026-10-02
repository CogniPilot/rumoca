//! An initial equation relating two discrete coordinates (MLS 3.7 §8.6).
//!
//! `Modelica.StateGraph` steps state `pre(newActive) = pre(localActive)`. When
//! neither side has another initial owner, the right side takes its start
//! value ("a missing initial value of a discrete-time variable ... may be
//! automatically set to the start value") and the relation determines the
//! left side; when one side is determined elsewhere, the relation determines
//! the other from it.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model Step
  parameter Boolean initiallyActive = false;
  Boolean localActive(start = initiallyActive);
  Boolean newActive;
  Boolean oldActive;
initial equation
  pre(newActive) = pre(localActive);
  pre(oldActive) = pre(localActive);
equation
  localActive = pre(newActive);
  newActive = time > 0.5 or pre(oldActive) and localActive;
  oldActive = localActive;
end Step;
model Seeded
  Boolean a;
  Boolean b;
initial equation
  pre(a) = pre(b);
  pre(b) = true;
equation
  a = pre(a) and time < 0.5;
  b = pre(b);
end Seeded;
"#;

fn last_and_first(model: &str, names: &[&str]) -> Vec<(f64, f64)> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "Steps.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error}"));
    names
        .iter()
        .map(|name| {
            let index = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap_or_else(|| panic!("{name} is recorded"));
            let column = &result.data[index];
            (column[0], *column.last().expect("samples"))
        })
        .collect()
}

#[test]
fn a_free_relation_takes_the_right_side_start_value() {
    let values = last_and_first("Step", &["localActive", "newActive"]);
    // localActive starts at its start value false; newActive turns on at 0.5.
    assert_eq!(values[0].0, 0.0);
    assert_eq!(values[1].1, 1.0);
}

#[test]
fn a_relation_with_one_determined_side_determines_the_other() {
    let values = last_and_first("Seeded", &["a", "b"]);
    assert_eq!(values[0], (1.0, 0.0));
    assert_eq!(values[1], (1.0, 1.0));
}
