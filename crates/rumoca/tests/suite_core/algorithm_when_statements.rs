//! Unclocked `when` statements in model algorithm sections (MLS 3.7 §11.2.7,
//! §8.3.5, §11.1.2; SPEC_0040 DAE-C25).
//!
//! An algorithm section runs its statements in order every time it runs; a
//! `when` statement assigns only at the instant its condition rises, and a
//! later statement overrides an earlier one. These shapes are pervasive in
//! the MSL (`Modelica.Blocks.Sources.RadioButtonSource` resets on the vector
//! `pre(reset)` it assigns just before its `when`).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

fn simulate(model: &str, source: &str) -> Result<SimResult, String> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .map_err(|error| format!("{error:?}"))?;
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .map_err(|error| format!("{error:?}"))
}

/// The value of `name` at the last sample at or before `time`.
fn value_at(result: &SimResult, name: &str, time: f64) -> f64 {
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is recorded"));
    let sample = result
        .times
        .iter()
        .rposition(|sample| *sample <= time)
        .expect("a sample precedes the probe");
    result.data[column][sample]
}

#[test]
fn a_counter_increments_once_per_rising_condition() {
    let source = r#"
model Counter
  Integer n(start = 0, fixed = true);
algorithm
  when {time > 0.25, time > 0.5} then
    n := pre(n) + 1;
  end when;
end Counter;
"#;
    let result = simulate("Counter", source).expect("Counter simulates");
    assert_eq!(value_at(&result, "n", 0.2), 0.0);
    assert_eq!(value_at(&result, "n", 0.4), 1.0);
    assert_eq!(value_at(&result, "n", 0.9), 2.0);
}

#[test]
fn a_when_condition_reads_the_value_an_earlier_statement_assigned() {
    let source = r#"
model ResetButton
  Boolean reset[1] = {time > 0.5};
  Boolean pre_reset[1];
  Boolean on(start = false, fixed = true);
initial equation
  pre(pre_reset) = {false};
algorithm
  pre_reset := pre(reset);
  when pre_reset then
    on := false;
  end when;
  when time > 0.25 then
    on := true;
  end when;
end ResetButton;
"#;
    let result = simulate("ResetButton", source).expect("ResetButton simulates");
    assert_eq!(value_at(&result, "on", 0.2), 0.0);
    assert_eq!(value_at(&result, "on", 0.4), 1.0);
    assert_eq!(value_at(&result, "pre_reset[1]", 0.9), 1.0);
    assert_eq!(value_at(&result, "on", 0.9), 0.0);
}

#[test]
fn a_later_when_statement_overrides_an_earlier_one_at_the_same_instant() {
    let source = r#"
model LaterWins
  Boolean on(start = false, fixed = true);
algorithm
  when time > 0.5 then
    on := false;
  end when;
  when time > 0.5 then
    on := true;
  end when;
end LaterWins;
"#;
    let result = simulate("LaterWins", source).expect("LaterWins simulates");
    assert_eq!(value_at(&result, "on", 0.4), 0.0);
    assert_eq!(value_at(&result, "on", 0.9), 1.0);
}

#[test]
fn a_continuous_assignment_and_an_event_section_share_one_algorithm() {
    let source = r#"
model Mixed
  Real x(start = 0, fixed = true);
  Real y;
  Integer n(start = 0, fixed = true);
equation
  der(x) = 1;
algorithm
  y := 2*x;
  when y > 0.6 then
    n := pre(n) + 1;
  end when;
end Mixed;
"#;
    let result = simulate("Mixed", source).expect("Mixed simulates");
    let y = value_at(&result, "y", 0.5);
    assert!((y - 1.0).abs() < 1e-6, "y(0.5) = {y}");
    assert_eq!(value_at(&result, "n", 0.25), 0.0);
    assert_eq!(value_at(&result, "n", 0.9), 1.0);
}

/// Simultaneous statements that write different targets each define their
/// own: the first keeps `n`, the later one overrides `b` (MLS 3.7 §11.1.2).
#[test]
fn simultaneous_statements_writing_disjoint_targets_each_define_them() {
    let source = r#"
model Disjoint
  Boolean b(start = false, fixed = true);
  Integer n(start = 0, fixed = true);
algorithm
  when time > 0.5 then
    n := pre(n) + 1;
    b := true;
  end when;
  when time > 0.5 then
    b := false;
  end when;
end Disjoint;
"#;
    let result = simulate("Disjoint", source).expect("Disjoint simulates");
    assert_eq!(value_at(&result, "n", 0.4), 0.0);
    assert_eq!(value_at(&result, "n", 0.9), 1.0);
    assert_eq!(value_at(&result, "b", 0.9), 0.0);
}
