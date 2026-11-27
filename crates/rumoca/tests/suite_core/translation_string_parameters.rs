//! A parameter whose binding reads a String is evaluated at translation
//! (MLS 3.7 §3.5, §12.9; SPEC_0022 ALG-015).
//!
//! The runtime owns no String storage, so a comparison of a String parameter
//! has no value at simulation time: `parameter Boolean b = names[2] == w`
//! with `parameter String w` can only be decided where `w` is known. The
//! binding of such a parameter is evaluated at translation, as a String
//! parameter's own binding already is.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const PACKAGE: &str = r#"
package Lib
  type Compare = enumeration(Less, Equal, Greater);
  pure function compare
    input String string1;
    input String string2;
    input Boolean caseSensitive = true;
    output Compare result;
  external "C" result = ModelicaStrings_compare(string1, string2, caseSensitive);
  end compare;
  function isEqual
    input String string1;
    input String string2;
    input Boolean caseSensitive = true;
    output Boolean identical;
  algorithm
    identical := compare(string1, string2, caseSensitive) == Compare.Equal;
  end isEqual;
  model BuiltIn
    parameter String wanted = "CO2";
    parameter String other = "H2O";
    constant String names[2] = {"N2", "CO2"};
    parameter Boolean hit = names[2] == wanted;
    parameter Boolean miss = names[2] == other;
    Real n(start = 0, fixed = true);
  equation
    der(n) = (if hit then 1 else 0) + (if miss then 10 else 0);
  end BuiltIn;
  model Called
    parameter String wanted = "CO2";
    constant String names[3] = {"N2", "CO2", "H2O"};
    parameter Boolean second = isEqual(names[2], wanted);
    parameter Boolean third = isEqual(names[3], wanted);
    Real n(start = 0, fixed = true);
  equation
    der(n) = (if second then 1 else 0) + (if third then 10 else 0);
  end Called;
end Lib;
"#;

/// The value of `variable` at the end of a one-second simulation of `model`.
fn last(model: &str, variable: &str) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(PACKAGE, "Lib.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"));
    let column = result
        .names
        .iter()
        .position(|name| name == variable)
        .unwrap_or_else(|| panic!("{model} records {variable}"));
    let value = result.data[column]
        .last()
        .copied()
        .expect("the trace has samples");
    (value * 1e6).round() / 1e6
}

#[test]
fn a_parameter_bound_to_a_string_equality_is_decided_at_translation() {
    assert_eq!(last("Lib.BuiltIn", "n"), 1.0, "hit is true, miss is false");
}

#[test]
fn a_parameter_bound_to_a_string_function_of_a_parameter_is_decided_at_translation() {
    assert_eq!(
        last("Lib.Called", "n"),
        1.0,
        "second is true, third is false"
    );
}

/// A parameter folded because its binding reads a String is fixed at
/// translation, so it is not published as tunable: setting it at run time
/// would change nothing.
#[test]
fn a_parameter_decided_by_a_string_is_not_tunable() {
    let compiled = Compiler::new()
        .model("Lib.BuiltIn")
        .compile_str(PACKAGE, "Lib.mo")
        .unwrap_or_else(|error| panic!("Lib.BuiltIn compiles: {error:?}"));
    let tunable = |name: &str| {
        compiled.dae.inspect(|view| {
            view.variables()
                .find(|(_, variable)| variable.name().as_str() == name)
                .map(|(_, variable)| variable.is_tunable())
                .unwrap_or_else(|| panic!("{name} is a DAE variable"))
        })
    };
    assert!(!tunable("hit"), "hit is decided at translation");
    assert!(!tunable("miss"), "miss is decided at translation");
}
