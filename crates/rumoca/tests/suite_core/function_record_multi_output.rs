//! A function statement whose receivers are exactly the fields of one record
//! result, `(p.a, p.b) := split(x)` (MLS 3.7 §11.2.1.1), assembles that record
//! from the call's outputs in field order.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model RecordMultiOutput
  record Pair
    Real a;
    Real b;
  end Pair;
  function split
    input Real x;
    output Real lo;
    output Real hi;
  algorithm
    lo := x - 1;
    hi := x + 1;
  end split;
  function pair
    input Real x;
    output Pair p;
  algorithm
    (p.a, p.b) := split(2*x);
  end pair;
  Pair q = pair(time);
end RecordMultiOutput;
"#;

#[test]
fn a_multi_output_call_assembles_the_record_its_receivers_name() {
    let compiled = Compiler::new()
        .model("RecordMultiOutput")
        .compile_str(SOURCE, "RecordMultiOutput.mo")
        .unwrap_or_else(|error| panic!("RecordMultiOutput compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("RecordMultiOutput simulates: {error}"));
    let last = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        *result.data[index].last().expect("samples")
    };
    // split(2*t) at t = 1 gives (1, 3).
    assert!((last("q.a") - 1.0).abs() < 1e-12);
    assert!((last("q.b") - 3.0).abs() < 1e-12);
}
