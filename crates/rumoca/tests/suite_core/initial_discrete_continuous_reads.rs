//! Discrete initial definitions that read continuous coordinates.
//!
//! MLS 3.7 §8.6 solves the initial equations together with the model
//! equations, so `pre(m) = f(x)` determines `m` from the value `x` takes in
//! that solution. Solve applies the definition after the initialization
//! projection once the definition's reads are proven independent of every
//! discrete value; a definition whose reads lead back to a discrete value has
//! no such proof and is rejected rather than iterated.

use rumoca::Compiler;

const SOURCE: &str = r#"
connector RealInput = input Real;

model ParameterThroughConnection
  parameter Integer n = 2;
  parameter Real h[n] = {0.5, 1.5};
  parameter Real l0 = 1;
  Real level(start = l0, fixed = true);
  Boolean above[n];
protected
  RealInput hIn[n] = h;
  RealInput hIn2[n];
equation
  connect(hIn, hIn2);
  der(level) = -0.1;
  for i in 1:n loop
    above[i] = level >= hIn2[i] + 0.1 or pre(above[i]) and level >= hIn2[i] - 0.1;
  end for;
initial equation
  for i in 1:n loop
    pre(above[i]) = l0 >= hIn2[i];
  end for;
end ParameterThroughConnection;

model StateRead
  Real level(start = 1, fixed = true);
  Boolean above;
equation
  der(level) = -0.1;
  above = level >= 0.6 or pre(above) and level >= 0.4;
initial equation
  pre(above) = level >= 0.5;
end StateRead;

model ReadsItsOwnValue
  Boolean b;
  Real x;
equation
  x = if b then 1 else -1;
  b = pre(b) and time < 1;
initial equation
  pre(b) = x > 0;
end ReadsItsOwnValue;
"#;

fn simulate(model: &str, t_end: f64) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "InitialDiscreteContinuousReads.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end,
            ..Default::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"))
}

fn value_at(result: &rumoca_sim::SimResult, name: &str, time: f64) -> f64 {
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} in {:?}", result.names));
    let row = result
        .times
        .iter()
        .rposition(|t| *t <= time)
        .expect("a sample at or before the time");
    result.data[column][row]
}

#[test]
fn initial_pre_definition_reads_a_connected_parameter_value() {
    let result = simulate("ParameterThroughConnection", 10.0);
    // above[1] starts true (1 >= 0.5) and holds through the hysteresis band
    // until level falls below 0.4 at t = 6; above[2] starts and stays false.
    assert_eq!(value_at(&result, "above[1]", 0.0), 1.0);
    assert_eq!(value_at(&result, "above[2]", 0.0), 0.0);
    assert_eq!(value_at(&result, "above[1]", 5.5), 1.0);
    assert_eq!(value_at(&result, "above[1]", 6.5), 0.0);
    assert_eq!(value_at(&result, "above[2]", 9.0), 0.0);
}

#[test]
fn initial_pre_definition_reads_the_initialized_state() {
    let result = simulate("StateRead", 10.0);
    assert_eq!(value_at(&result, "above", 0.0), 1.0);
    assert_eq!(value_at(&result, "above", 5.5), 1.0);
    assert_eq!(value_at(&result, "above", 6.5), 0.0);
}

#[test]
fn initial_pre_definition_that_reads_its_own_value_is_rejected() {
    let compiled = Compiler::new()
        .model("ReadsItsOwnValue")
        .compile_str(SOURCE, "InitialDiscreteContinuousReads.mo")
        .expect("the DAE accepts the definition; Solve proves its order");
    let error = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 2.0,
            ..Default::default()
        },
    )
    .expect_err("a definition that reads its own value has no proven order");
    let message = format!("{error:?}");
    assert!(
        message.contains("not proven independent of discrete values"),
        "{message}"
    );
}
