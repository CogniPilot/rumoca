//! An initial algorithm that only String comparisons decide runs at
//! translation (MLS 3.7 §8.6, §11.1; SPEC_0022 ALG-015).
//!
//! `Modelica.Fluid.Sensors.TraceSubstances` finds its substance's index in an
//! initial algorithm that compares Strings in a loop. The runtime owns no
//! String storage, so the loop runs at translation and the index becomes the
//! parameter's fixed, non-tunable value.

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
  model Called
    constant String names[3] = {"N2", "CO2", "H2O"};
    parameter String wanted = "CO2";
    parameter Integer ind(fixed = false);
    Real x(start = 0, fixed = true);
  initial algorithm
    ind := -1;
    for i in 1:3 loop
      if isEqual(names[i], wanted) then
        ind := i;
      end if;
    end for;
  equation
    der(x) = ind;
  end Called;
  model BuiltIn
    constant String names[3] = {"N2", "CO2", "H2O"};
    parameter String wanted = "H2O";
    parameter Integer ind(fixed = false);
    Real x(start = 0, fixed = true);
  initial algorithm
    ind := -1;
    for i in 1:3 loop
      if names[i] == wanted then
        ind := i;
      end if;
    end for;
  equation
    der(x) = ind;
  end BuiltIn;
  model Absent
    constant String names[3] = {"N2", "CO2", "H2O"};
    parameter String wanted = "Ar";
    parameter Integer ind(fixed = false);
    Real x(start = 0, fixed = true);
  initial algorithm
    ind := -1;
    for i in 1:3 loop
      if names[i] == wanted then
        ind := i;
      end if;
    end for;
  equation
    der(x) = ind;
  end Absent;
  model Present
    constant String names[3] = {"N2", "CO2", "H2O"};
    parameter String wanted = "CO2";
    parameter Integer ind(fixed = false);
    Real x(start = 0, fixed = true);
  initial algorithm
    ind := -1;
    for i in 1:3 loop
      if names[i] == wanted then
        ind := i;
      end if;
    end for;
    assert(ind > 0, "Trace substance '" + wanted + "' is not present");
  equation
    der(x) = ind;
  end Present;
  model AbsentError
    constant String names[3] = {"N2", "CO2", "H2O"};
    parameter String wanted = "Ar";
    parameter Integer ind(fixed = false);
    Real x(start = 0, fixed = true);
  initial algorithm
    ind := -1;
    for i in 1:3 loop
      if names[i] == wanted then
        ind := i;
      end if;
    end for;
    assert(ind > 0, "Trace substance '" + wanted + "' is not present");
  equation
    der(x) = ind;
  end AbsentError;
  model AbsentWarning
    constant String names[3] = {"N2", "CO2", "H2O"};
    parameter String wanted = "Ar";
    parameter Integer ind(fixed = false);
    Real x(start = 0, fixed = true);
  initial algorithm
    ind := -1;
    for i in 1:3 loop
      if names[i] == wanted then
        ind := i;
      end if;
    end for;
    assert(ind > 0, "Trace substance '" + wanted + "' is not present", AssertionLevel.warning);
  equation
    der(x) = ind;
  end AbsentWarning;
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
    let value = *result.data[column].last().expect("trace is nonempty");
    (value * 1e9).round() / 1e9
}

#[test]
fn an_index_found_by_a_string_function_in_an_initial_algorithm_is_decided_at_translation() {
    assert_eq!(last("Lib.Called", "x"), 2.0, "ind = 2, so x(1) = 2");
}

#[test]
fn an_index_found_by_string_equality_in_an_initial_algorithm_is_decided_at_translation() {
    assert_eq!(last("Lib.BuiltIn", "x"), 3.0, "ind = 3, so x(1) = 3");
}

#[test]
fn an_absent_name_leaves_the_algorithm_s_own_default() {
    assert_eq!(last("Lib.Absent", "x"), -1.0, "ind stays -1");
}

#[test]
fn an_index_decided_at_translation_is_not_tunable() {
    let compiled = Compiler::new()
        .model("Lib.Called")
        .compile_str(PACKAGE, "Lib.mo")
        .unwrap_or_else(|error| panic!("Lib.Called compiles: {error:?}"));
    let tunable = compiled.dae.inspect(|view| {
        view.variables()
            .find(|(_, variable)| variable.name().as_str() == "ind")
            .map(|(_, variable)| variable.is_tunable())
            .expect("ind is a DAE variable")
    });
    assert!(!tunable, "ind is decided at translation");
}

#[test]
fn a_true_assertion_in_a_decided_initial_algorithm_goes_with_it() {
    assert_eq!(
        last("Lib.Present", "x"),
        2.0,
        "ind = 2, the assertion holds"
    );
}

/// MLS 3.7 §8.3.7: a false error-level assertion decided at translation is the
/// EF030 translation error at the assertion, with its evaluated message.
#[test]
fn a_false_error_assertion_in_a_decided_initial_algorithm_is_a_translation_error() {
    let error = Compiler::new()
        .model("Lib.AbsentError")
        .compile_str(PACKAGE, "Lib.mo")
        .expect_err("the substance is absent, so the model cannot initialize");
    let text = format!("{error:?}");
    assert!(
        text.contains("EF030") && text.contains("Trace substance 'Ar' is not present"),
        "EF030 with the evaluated message: {text}"
    );
}

/// A false warning-level assertion does not stop the model: the index is
/// decided and the assertion stays, condition and message evaluated, for the
/// runtime to report.
#[test]
fn a_false_warning_assertion_in_a_decided_initial_algorithm_stays_a_warning() {
    let flat = Compiler::new()
        .model("Lib.AbsentWarning")
        .compile_str_flat(PACKAGE, "Lib.mo")
        .unwrap_or_else(|error| panic!("Lib.AbsentWarning flattens: {error:?}"));
    let ind = &flat.variables[&rumoca_core::VarName::new("ind")];
    assert!(
        matches!(
            ind.binding,
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(-1),
                ..
            })
        ),
        "ind is decided: {:?}",
        ind.binding
    );
    let kept = flat
        .initial_algorithms
        .iter()
        .flat_map(|algorithm| &algorithm.statements)
        .filter_map(|statement| match statement {
            rumoca_core::Statement::Assert { message, .. } => Some(format!("{message:?}")),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert!(
        kept.len() == 1 && kept[0].contains("Trace substance 'Ar' is not present"),
        "one warning assertion with its evaluated message: {kept:?}"
    );
    assert_eq!(last("Lib.AbsentWarning", "x"), -1.0, "the model still runs");
}
