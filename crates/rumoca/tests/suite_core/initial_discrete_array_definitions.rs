//! Discrete array coordinates determined element by element.
//!
//! MLS 3.7 §8.6 gives each discrete coordinate one initialization value. A
//! `for` loop of initial equations such as `pre(above[i]) = l0 >= h[i]`
//! determines it element by element, so the elements form one aggregate
//! definition. A loop body that selects its equation through a parameter
//! `if` keeps its rows as ordinary discrete-valued assignments (Appendix B),
//! exactly as the same rows outside a loop.

use rumoca::Compiler;

const SOURCE: &str = r#"
model InitialPre
  parameter Integer n = 2;
  parameter Real h[n] = {0.5, 1.5};
  parameter Real l0 = 1;
  Real level(start = l0, fixed = true);
  Boolean above[n];
equation
  der(level) = -0.1;
  for i in 1:n loop
    above[i] = level >= h[i] + 0.1 or pre(above[i]) and level >= h[i] - 0.1;
  end for;
initial equation
  for i in 1:n loop
    pre(above[i]) = l0 >= h[i];
  end for;
end InitialPre;

model SelectedRows
  parameter Integer n = 2;
  parameter Boolean stiff = false;
  Real x(start = 1, fixed = true);
  Boolean out[n];
equation
  der(x) = -1;
  for i in 1:n loop
    if stiff then
      out[i] = false;
    else
      out[i] = pre(out[i]) or x < 0.5*i;
    end if;
  end for;
end SelectedRows;

model SelectedConstantRows
  extends SelectedRows(stiff = true);
end SelectedConstantRows;
"#;

fn simulate(model: &str, t_end: f64) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "InitialDiscreteArrays.mo")
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
fn element_wise_initial_pre_definitions_seed_the_whole_boolean_array() {
    let result = simulate("InitialPre", 10.0);
    // above[1] starts true (1 >= 0.5) and holds through the hysteresis band
    // until level falls below 0.4 at t = 6; above[2] starts and stays false.
    assert_eq!(value_at(&result, "above[1]", 0.0), 1.0);
    assert_eq!(value_at(&result, "above[2]", 0.0), 0.0);
    assert_eq!(value_at(&result, "above[1]", 5.5), 1.0);
    assert_eq!(value_at(&result, "above[1]", 6.5), 0.0);
    assert_eq!(value_at(&result, "above[2]", 9.0), 0.0);
}

#[test]
fn parameter_selected_boolean_rows_in_a_loop_keep_their_discrete_owners() {
    let result = simulate("SelectedRows", 1.0);
    assert_eq!(value_at(&result, "out[1]", 0.4), 0.0);
    assert_eq!(value_at(&result, "out[2]", 0.4), 1.0);
    assert_eq!(value_at(&result, "out[1]", 0.6), 1.0);
    let constant = simulate("SelectedConstantRows", 1.0);
    assert_eq!(value_at(&constant, "out[1]", 0.9), 0.0);
    assert_eq!(value_at(&constant, "out[2]", 0.9), 0.0);
}
