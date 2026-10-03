//! Model-algorithm `for` loops that carry a whole value through their
//! iterations (MLS §11.1.2, §11.2.2).
//!
//! `Modelica.Electrical.Digital.Sources.Table` steps its output through a
//! parameter table by overwriting one scalar in each iteration of a loop over
//! `final parameter Integer n = size(x, 1)`, after an initial-time check whose
//! own loop body is empty. The range is fixed at translation, so the loop is
//! its unrolled sequence and each `time >= t[i]` is its own scheduled event.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const TABLE_SOURCE: &str = r#"
package P
block Table
  parameter Integer x[:] = {4};
  parameter Real t[size(x, 1)] = {1};
  parameter Integer y0 = 1;
  final parameter Integer n = size(x, 1);
  Integer y;
algorithm
  if initial() then
    assert(n > 0, "Invalid size of table (n < 1)");
    for i in 1:n loop
    end for;
  end if;
  y := y0;
  for i in 1:n loop
    if time >= t[i] then
      y := x[i];
    end if;
  end for;
end Table;
model TableSource
  Table table(x = {4, 3, 4}, t = {0.2, 0.4, 0.6});
end TableSource;
end P;
"#;

const WEIGHTED_SUM: &str = r#"
model WeightedSum
  parameter Real w[:] = {1, 2, 3};
  final parameter Integer n = size(w, 1);
  Real s;
algorithm
  s := 0;
  for i in 1:n loop
    s := s + w[i]*time;
  end for;
end WeightedSum;
"#;

fn simulate(source: &str, model: &str) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"))
}

fn column<'r>(result: &'r rumoca_sim::SimResult, name: &str) -> &'r [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("simulation exposes {name}"));
    &result.data[index]
}

#[test]
fn a_table_loop_steps_through_every_iteration() {
    let result = simulate(TABLE_SOURCE, "P.TableSource");
    let y = column(&result, "table.y");
    for (time, value) in result.times.iter().zip(y) {
        let expected = match *time {
            t if t < 0.2 => 1.0,
            t if t < 0.4 => 4.0,
            t if t < 0.6 => 3.0,
            _ => 4.0,
        };
        let at_edge = [0.2, 0.4, 0.6]
            .iter()
            .any(|edge| (time - edge).abs() < 1.0e-9);
        if !at_edge {
            assert_eq!(*value, expected, "y at t = {time}");
        }
    }
}

#[test]
fn a_scalar_accumulator_loop_sums_every_iteration() {
    let result = simulate(WEIGHTED_SUM, "WeightedSum");
    let s = column(&result, "s");
    for (time, value) in result.times.iter().zip(s) {
        assert!(
            (value - 6.0 * time).abs() <= 1.0e-9,
            "s({time}) should be 6 t, found {value}"
        );
    }
}
