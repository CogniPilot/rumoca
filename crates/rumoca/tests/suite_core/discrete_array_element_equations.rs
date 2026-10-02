//! An array equation is its element equations (MLS 3.7 §10.6.1). A slice
//! over a component array, `split.set = fill(inPort.set, n)` in
//! `Modelica.StateGraph.Parallel`, flattens to `{split[1].set, ...} = e`;
//! each discrete-valued element is defined by its own element equation
//! `split[i].set = e[i]`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
connector C
  output Boolean set;
  output Integer k;
end C;
model Split
  parameter Integer n = 3;
  C split[n];
  Boolean inSet = time > 0.5;
equation
  split.set = fill(inSet, n);
  split.k = {1, 2, 3}*(if inSet then 2 else 1);
end Split;
"#;

#[test]
fn a_discrete_component_array_slice_equation_defines_each_element() {
    let compiled = Compiler::new()
        .model("Split")
        .compile_str(SOURCE, "Split.mo")
        .unwrap_or_else(|error| panic!("Split compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Split simulates: {error}"));
    let last = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        *result.data[index].last().expect("samples")
    };
    for element in 1..=3 {
        assert_eq!(last(&format!("split[{element}].set")), 1.0);
        assert_eq!(last(&format!("split[{element}].k")), 2.0 * element as f64);
    }
}

/// A for-equation over the fields of a component array
/// (`inPort[i].occupied = if i == 1 then localActive else ...` in
/// `Modelica.StateGraph.Step`) materializes one discrete-value assignment per
/// element field; those rows are the definitions, and the loop template that
/// groups them names no declared coordinate of its own.
const FIELD_LOOP: &str = r#"
connector In
  output Boolean occupied;
  input Boolean set;
end In;
model StepLike
  parameter Integer nIn = 2;
  In inPort[nIn];
  Boolean localActive = time > 0.5;
equation
  for i in 1:nIn loop
    inPort[i].occupied = if i == 1 then localActive else inPort[i-1].occupied or inPort[i-1].set;
  end for;
end StepLike;
model FieldLoop
  StepLike s(nIn = 2);
  StepLike single(nIn = 1);
equation
  s.inPort[1].set = time > 0.2;
  s.inPort[2].set = time > 0.8;
  single.inPort[1].set = time > 0.7;
end FieldLoop;
"#;

#[test]
fn a_for_equation_over_component_array_fields_defines_each_field() {
    let compiled = Compiler::new()
        .model("FieldLoop")
        .compile_str(FIELD_LOOP, "FieldLoop.mo")
        .unwrap_or_else(|error| panic!("FieldLoop compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("FieldLoop simulates: {error}"));
    let at = |name: &str, sample: usize| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        result.data[index][sample]
    };
    let last = result.times.len() - 1;
    // s.inPort[2].occupied = s.inPort[1].occupied or s.inPort[1].set.
    assert_eq!(at("s.inPort[2].occupied", 0), 0.0);
    assert_eq!(at("s.inPort[2].occupied", last), 1.0);
    assert_eq!(at("single.inPort[1].occupied", last), 1.0);
}
