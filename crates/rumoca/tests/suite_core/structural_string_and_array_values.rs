//! String and array parameter values at translation (MLS 3.7 §10.1, §12.4;
//! SPEC_0040 FLAT-C01).
//!
//! A colon dimension takes the shape of its binding, and a function result
//! shape reads the call's arguments: `A[:, :] = values(file, dim[1], dim[2])`
//! reads an element of an Integer array parameter and a String parameter.
//! The runtime owns no String storage, so a String parameter is its declared
//! value, and a binding that reads a String can only be evaluated at
//! translation: an invariant one becomes its value, array-valued included.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const MODELS: &str = r#"
package Values
  function sizes
    input String name;
    output Integer dim[2];
  algorithm
    dim := if name == "A" then {2, 3} else {1, 1};
  end sizes;
  function values
    input String name;
    input Integer n;
    input Integer m;
    output Real y[n, m];
  algorithm
    y := fill(if name == "A" then 2.0 else 1.0, n, m);
  end values;
  model ColonFromStringCall
    parameter String file = "A";
    final parameter Integer dim[2] = sizes(file);
    final parameter Real A[:, :] = values(file, dim[1], dim[2]);
    Real x(start = 1, fixed = true);
    Real n = 10*size(A, 1) + size(A, 2);
  equation
    der(x) = -A[2, 3]*x;
  end ColonFromStringCall;
  model DimensionFromString
    parameter String s = "A";
    final parameter Integer n = if s == "A" then 2 else 3;
    Real x[n](each start = 1, each fixed = true);
  equation
    der(x) = -x;
  end DimensionFromString;
  model FixedShapeStringCall
    parameter String s = "A";
    final parameter Real A[2, 3] = values(s, 2, 3);
    Real x(start = 1, fixed = true);
  equation
    der(x) = -A[1, 1]*x;
  end FixedShapeStringCall;
end Values;
"#;

fn final_value(model: &str, variable: &str) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "Values.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.25),
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"));
    let column = result
        .names
        .iter()
        .position(|name| name == variable)
        .unwrap_or_else(|| panic!("{variable} is recorded in {:?}", result.names));
    *result.data[column].last().expect("trace is nonempty")
}

#[test]
fn a_colon_dimension_reads_string_and_array_element_arguments() {
    assert_eq!(final_value("Values.ColonFromStringCall", "n"), 23.0);
    let x = final_value("Values.ColonFromStringCall", "x");
    assert!((x - (-2.0_f64).exp()).abs() < 1e-5, "x(1) = {x}");
}

#[test]
fn a_dimension_reads_a_string_parameter_as_declared() {
    let x = final_value("Values.DimensionFromString", "x[2]");
    assert!((x - (-1.0_f64).exp()).abs() < 1e-5, "x[2](1) = {x}");
}

#[test]
fn an_invariant_array_binding_that_reads_a_string_folds() {
    let x = final_value("Values.FixedShapeStringCall", "x");
    assert!((x - (-2.0_f64).exp()).abs() < 1e-5, "x(1) = {x}");
}
