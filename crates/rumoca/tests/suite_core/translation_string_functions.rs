//! Cataloged foreign String functions at translation (MLS 3.7 §12.9;
//! SPEC_0040 FLAT-C06).
//!
//! `Modelica.Utilities.Strings.compare` and `length` are external C functions
//! (`ModelicaStrings_compare`, `ModelicaStrings_length`). Their String
//! operands exist only at translation, so their bodies execute there, and a
//! constant bound to their result is the value the C source gives.

use std::path::PathBuf;

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
  pure function length
    input String string;
    output Integer result;
  external "C" result = ModelicaStrings_length(string);
  end length;
  model Equality
    constant Boolean same = isEqual("CO2", "CO2");
    constant Boolean differs = isEqual("CO2", "N2");
    constant Boolean folded = isEqual("co2", "CO2", false);
    constant Boolean exact = isEqual("co2", "CO2");
    Real n = (if same then 1 else 0) + (if differs then 10 else 0)
      + (if folded then 100 else 0) + (if exact then 1000 else 0);
  end Equality;
  model Ordering
    constant Boolean less = compare("CO", "CO2") == Compare.Less;
    constant Boolean greater = compare("N2", "CO2") == Compare.Greater;
    constant Boolean equal = compare("N2", "N2") == Compare.Equal;
    constant Boolean notLess = compare("N2", "CO2") == Compare.Less;
    Real n = (if less then 1 else 0) + (if greater then 10 else 0)
      + (if equal then 100 else 0) + (if notLess then 1000 else 0);
  end Ordering;
  model Length
    constant Integer n = length("CO2");
    constant Integer empty = length("");
    Real count = n + 10*empty;
  end Length;
  pure function misdeclared
    input String string1;
    input String string2;
    output Real result;
  external "C" result = ModelicaStrings_compare(string1, string2);
  end misdeclared;
  model Misdeclared
    constant Real r = misdeclared("a", "b");
    Real y = r;
  end Misdeclared;
end Lib;
"#;

/// The value of `variable` at the end of a short simulation of `model`.
fn last(model: &str, variable: &str) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(PACKAGE, "Lib.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error:?}"));
    let column = result
        .names
        .iter()
        .position(|name| name == variable)
        .unwrap_or_else(|| panic!("{variable} is recorded"));
    *result.data[column].last().expect("trace is nonempty")
}

#[test]
fn string_equality_of_literals_is_decided_at_translation() {
    assert_eq!(
        last("Lib.Equality", "n"),
        101.0,
        "equal strings and case-folded equal strings are equal; the others are not"
    );
}

#[test]
fn string_comparison_returns_the_literal_of_its_declared_enumeration() {
    assert_eq!(
        last("Lib.Ordering", "n"),
        111.0,
        "a prefix is Less, a later byte is Greater, the same string is Equal"
    );
}

#[test]
fn string_length_is_its_byte_count_at_translation() {
    assert_eq!(last("Lib.Length", "count"), 3.0);
}

#[test]
fn an_interface_outside_the_catalog_row_is_not_evaluated() {
    let compiled = Compiler::new()
        .model("Lib.Misdeclared")
        .compile_str(PACKAGE, "Lib.mo")
        .expect("Lib.Misdeclared compiles; the call stays unevaluated");
    let options = SimOptions {
        t_end: 1.0,
        dt: Some(0.5),
        ..SimOptions::default()
    };
    let error = simulate_dae(&compiled.dae, &options)
        .map(|_| ())
        .expect_err("a two-argument Real-valued compare is not the cataloged function");
    let text = format!("{error:?}");
    assert!(
        text.contains("ModelicaStrings_compare"),
        "the refusal names the entry point: {text}"
    );
}

const MSL_PROBE: &str = r#"
model MslStrings
  import Modelica.Utilities.Strings;
  import Modelica.Utilities.Types.Compare;
  constant Boolean same = Strings.isEqual("CO2", "CO2");
  constant Boolean folded = Strings.isEqual("co2", "CO2", false);
  constant Boolean exact = Strings.isEqual("co2", "CO2");
  constant Boolean less = Strings.compare("CO", "CO2") == Compare.Less;
  constant Integer n = Strings.length("CO2");
  Real code = (if same then 1 else 0) + (if folded then 10 else 0)
    + (if exact then 100 else 0) + (if less then 1000 else 0) + 10000*n;
end MslStrings;
"#;

#[test]
fn the_msl_string_functions_fold_at_translation() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/msl/ModelicaStandardLibrary-4.1.0");
    if !root.is_dir() {
        eprintln!("skipping: the MSL is not available");
        return;
    }
    let compiled = Compiler::new()
        .model("MslStrings")
        .source_root(root.to_string_lossy().as_ref())
        .compile_str(MSL_PROBE, "MslStrings.mo")
        .unwrap_or_else(|error| panic!("MslStrings compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("MslStrings simulates: {error:?}"));
    let column = result
        .names
        .iter()
        .position(|name| name == "code")
        .expect("code is recorded");
    assert_eq!(
        *result.data[column].last().expect("trace is nonempty"),
        31011.0,
        "isEqual, case-folded isEqual, compare Less and length 3 of the MSL functions"
    );
}
