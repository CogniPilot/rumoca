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
  model AssertFirst
    constant String names[2] = {"N2", "CO2"};
    parameter String w = "CO2";
    parameter Integer ind(fixed = false);
    Real x(start = 0, fixed = true);
  initial algorithm
    ind := 0;
    assert(ind == 0, "ind starts at zero");
    for i in 1:2 loop
      if names[i] == w then
        ind := i;
      end if;
    end for;
  equation
    der(x) = ind;
  end AssertFirst;
  pure function unknownTable
    output Real table[3];
  external "C" rumocaTestUnknownTable(table);
  end unknownTable;
  model SizeBound
    constant String names[3] = {"N2", "CO2", "H2O"};
    parameter String wanted = "CO2";
    parameter Real table[3] = unknownTable() annotation(Evaluate = true);
    parameter Integer ind(fixed = false);
    Real x(start = 0, fixed = true);
  initial algorithm
    ind := -1;
    for i in 1:3 loop
      if names[i] == wanted then
        ind := i;
      end if;
    end for;
    assert(ind <= size(table, 1), "the index fits the table");
  equation
    der(x) = ind;
  end SizeBound;
  pure function unknownStart
    output Integer start;
  external "C" start = rumocaTestUnknownStart();
  end unknownStart;
  model UnknownStart
    constant String names[3] = {"N2", "CO2", "H2O"};
    parameter String wanted = "CO2";
    parameter Integer start = unknownStart() annotation(Evaluate = true);
    parameter Integer ind(fixed = false);
    Real x(start = 0, fixed = true);
  initial algorithm
    ind := -1;
    for i in 1:3 loop
      if names[i] == wanted then
        ind := i;
      end if;
    end for;
    assert(ind >= start, "the index is past the start");
  equation
    der(x) = ind;
  end UnknownStart;
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

/// An assertion written before the last assignment of the target holds at
/// its own point (`ind` is 0 there) and fails at the end (`ind` is 2), so it
/// cannot be decided with the final value: the algorithm is left as it is,
/// not rejected with EF030 (Ada's probe).
#[test]
fn an_assertion_before_the_last_assignment_leaves_the_algorithm_as_it_is() {
    let flat = Compiler::new()
        .model("Lib.AssertFirst")
        .compile_str_flat(PACKAGE, "Lib.mo")
        .unwrap_or_else(|error| panic!("Lib.AssertFirst flattens: {error:?}"));
    let ind = &flat.variables[&rumoca_core::VarName::new("ind")];
    assert!(
        ind.binding.is_none(),
        "ind is not decided: {:?}",
        ind.binding
    );
    assert_eq!(flat.initial_algorithms.len(), 1, "the algorithm stays");
}

/// An assertion after the loop reads the shape of a fixed array whose values
/// the translation cannot compute (`size(table, 1)`): its declared shape
/// decides the assertion, and the algorithm with it.
#[test]
fn an_assertion_on_the_size_of_a_fixed_array_is_decided_by_its_shape() {
    let flat = Compiler::new()
        .model("Lib.SizeBound")
        .compile_str_flat(PACKAGE, "Lib.mo")
        .unwrap_or_else(|error| panic!("Lib.SizeBound flattens: {error:?}"));
    let ind = &flat.variables[&rumoca_core::VarName::new("ind")];
    assert!(
        matches!(
            ind.binding,
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                ..
            })
        ),
        "ind is decided as 2: {:?}",
        ind.binding
    );
}

/// An assertion reading a fixed name the translation cannot evaluate (an
/// evaluated parameter bound to an external call) cannot be decided, so the
/// algorithm stays as it is.
#[test]
fn an_assertion_reading_a_fixed_value_the_translation_cannot_evaluate_keeps_the_algorithm() {
    let flat = Compiler::new()
        .model("Lib.UnknownStart")
        .compile_str_flat(PACKAGE, "Lib.mo")
        .unwrap_or_else(|error| panic!("Lib.UnknownStart flattens: {error:?}"));
    let ind = &flat.variables[&rumoca_core::VarName::new("ind")];
    assert!(
        ind.binding.is_none(),
        "ind is not decided: {:?}",
        ind.binding
    );
    assert_eq!(flat.initial_algorithms.len(), 1, "the algorithm stays");
}
