//! `size` of a function call result (MLS 3.7 §10.3.1).
//!
//! `size(a, k)` reads only the shape of `a`, which the checked type of a call
//! result fixes, so it is that Integer and the call is not evaluated for it.
//! `Media.Incompressible.TableBased.specificEntropy` declares
//! `Integer npol = size(poly_Cp, 1) - 1` over a coefficient vector bound to
//! a least-squares fit whose body carries an assertion.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model SizeOfCall
  function coefficients
    input Integer k;
    output Real y[k + 1];
  algorithm
    y := fill(1.0, k + 1);
    assert(sum(y) > 0, "empty coefficients");
  end coefficients;
  function degreeOffset
    input Real t;
    output Real d;
  protected
    Integer n = size(coefficients(2), 1) - 1;
  algorithm
    d := t + n;
  end degreeOffset;
  Real x = degreeOffset(time);
end SizeOfCall;
"#;

#[test]
fn size_of_a_call_result_is_its_static_extent() {
    let compiled = Compiler::new()
        .model("SizeOfCall")
        .compile_str(SOURCE, "SizeOfCall.mo")
        .unwrap_or_else(|error| panic!("SizeOfCall compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("SizeOfCall simulates");
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == "x")
        .expect("x is recorded");
    for (sample, time) in result.times.iter().enumerate() {
        assert!((result.data[index][sample] - (time + 2.0)).abs() < 1e-12);
    }
}
