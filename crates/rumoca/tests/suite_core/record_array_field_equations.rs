//! A whole-record equation whose record has an array-of-records field
//! (MLS 3.7 §8.3.1, §10.6.1): Flat holds the array of records as one column
//! per leaf field, and each column equals the matching field projection of
//! the record value.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const RECORD_ARRAY_FIELD_EQUATION: &str = r#"
within;
package P
  record Edge
    Boolean enabled;
    Integer id;
    Real r[2];
  end Edge;
  record Graph
    Integer generation;
    Edge edges[3];
  end Graph;
  function Seed
    input Integer g;
    output Graph result;
  algorithm
    result.generation := g;
    for slot in 1:3 loop
      result.edges[slot] := Edge(slot > 1, slot, {slot, 2 * slot});
    end for;
  end Seed;
  model M
    parameter Integer g = 2;
    Graph next = Seed(g);
  end M;
end P;
"#;

#[test]
fn record_equation_defines_every_record_array_column() {
    Compiler::new()
        .model("P.M")
        .compile_str(RECORD_ARRAY_FIELD_EQUATION, "RecordArrayFieldEquation.mo")
        .expect("a record equation reaches the columns of an array-of-records field");
}

/// A model algorithm reads fields of array-of-records elements (MLS 3.7
/// §11.1, §10.6.1): a carrying loop unrolls `ps[i].x` to literal element
/// selections, each the Flat coordinate of that element's field, and the
/// run-time input reaches the result.
#[test]
fn a_model_algorithm_loop_reads_record_array_element_fields() {
    let source = r#"
record Point
  Real x;
  Real w;
end Point;
model ModelLoop
  input Real u = 1;
  Point ps[3](x = {u, 2, 3}, w = {1, 2, 3});
  Real s;
algorithm
  s := 0;
  for i in 1:3 loop
    s := s + ps[i].x * ps[i].w;
  end for;
end ModelLoop;
"#;
    let dae = Compiler::new()
        .model("ModelLoop")
        .compile_str(source, "ModelLoop.mo")
        .expect("an algorithm loop reads record array element fields")
        .dae;
    let result = simulate_dae_with_diagnostics(
        &dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("the model simulates");
    let index = result
        .names
        .iter()
        .position(|name| name == "s")
        .expect("the result records s");
    let value = *result.data[index].last().expect("a sample");
    assert!((value - 14.0).abs() < 1e-12, "s = {value}");
}
