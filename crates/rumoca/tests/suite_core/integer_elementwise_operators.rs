//! Element-wise operators over Integer arrays inside a function body
//! (MLS 3.7 §10.6). `{2, 3} .^ 2` is an Integer array whose power is computed
//! in Real and returned as the exact Integer it rounds to; `{2, 3} .* 2` keeps
//! its Integer elements, and both convert to Real where a Real array is
//! assigned. An Integer-valued tensor result is piecewise constant, so its
//! directional derivative is zero.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model IntegerElementwise
  function f
    input Integer k;
    input Real t;
    output Real y;
  protected
    Integer s[2] = {2, k} .^ 2;
    Integer q = k ^ 3;
    Real r[2] = {2, 3} .^ 2;
    Real m[2] = {2, 3} .* 2;
  algorithm
    y := t*(s[1] + s[2] + q + r[1] + r[2] + m[1] + m[2]);
  end f;
  parameter Integer k = 3;
  Real y = f(k, time);
end IntegerElementwise;
"#;

#[test]
fn integer_element_wise_operators_lower_in_function_bodies() {
    let compiled = Compiler::new()
        .model("IntegerElementwise")
        .compile_str(SOURCE, "IntegerElementwise.mo")
        .unwrap_or_else(|error| panic!("IntegerElementwise compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("IntegerElementwise simulates: {error}"));
    let index = result.names.iter().position(|n| n == "y").expect("y");
    // 4 + 9 + 27 + 4 + 9 + 4 + 6 = 63.
    for (row, &time) in result.times.iter().enumerate() {
        assert!((result.data[index][row] - 63.0 * time).abs() < 1e-9);
    }
}
