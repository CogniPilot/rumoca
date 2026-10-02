//! MLS 3.7 §12.4.6 allows a vectorized call only of a transitively
//! non-replaceable function class (§6.3.1). SPEC_0022 FUNC-026 records an
//! accepted extension that MSL 4.1 relies on: after replaceable-package
//! selection (§7.3) a call such as `Medium.prandtlNumber(states)` is respelled
//! through the selected package, whose enclosing classes are all transitively
//! non-replaceable, so the selected package fixes the function at translation
//! even though the function is declared `replaceable` in it. A call spelled
//! through a model-level replaceable package alias (`replaceable package
//! Medium = A`) is equally fixed by that selection. A call whose callee is
//! still unresolved after selection (a partial function) is refused.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package VSel
  partial package PartialMed
    replaceable function prop
      input Real T;
      output Real y;
    algorithm
      y := T;
    end prop;
  end PartialMed;
  package A
    extends PartialMed;
    redeclare function prop
      input Real T;
      output Real y;
    algorithm
      y := 2*T + 1;
    end prop;
  end A;
  model Heat
    replaceable package Medium = PartialMed;
    Real T[2] = {300, 310}*(1 + time);
    Real y[2] = Medium.prop(T);
  end Heat;
  model Top
    Heat h(redeclare package Medium = A);
    annotation(experiment(StopTime = 1));
  end Top;
  model Root
    replaceable package Medium = A;
    Real y[2] = Medium.prop({1, 2}*time);
  end Root;
  partial package PartialAbstract
    replaceable partial function prop
      input Real T;
      output Real y;
    end prop;
  end PartialAbstract;
  model Unresolved
    replaceable package Medium = PartialAbstract;
    Real y[2] = Medium.prop({1, 2}*time);
  end Unresolved;
end VSel;
"#;

#[test]
fn a_vectorized_call_through_a_selected_package_is_accepted() {
    let compiled = Compiler::new()
        .model("VSel.Top")
        .compile_str(SOURCE, "VSel.mo")
        .unwrap_or_else(|error| panic!("VSel.Top compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("VSel.Top simulates: {error}"));
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).expect(name);
        &result.data[index]
    };
    // The selected package's prop is 2*T + 1 (OMC agrees to 2e-13).
    for (row, &time) in result.times.iter().enumerate() {
        assert!((column("h.y[1]")[row] - (600.0 * (1.0 + time) + 1.0)).abs() < 1e-9);
        assert!((column("h.y[2]")[row] - (620.0 * (1.0 + time) + 1.0)).abs() < 1e-9);
    }
}

#[test]
fn a_vectorized_call_through_a_model_level_replaceable_alias_is_accepted() {
    let compiled = Compiler::new()
        .model("VSel.Root")
        .compile_str(SOURCE, "VSel.mo")
        .unwrap_or_else(|error| panic!("VSel.Root compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("VSel.Root simulates: {error}"));
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name).expect(name);
        &result.data[index]
    };
    for (row, &time) in result.times.iter().enumerate() {
        assert!((column("y[1]")[row] - (2.0 * time + 1.0)).abs() < 1e-9);
        assert!((column("y[2]")[row] - (4.0 * time + 1.0)).abs() < 1e-9);
    }
}

#[test]
fn a_vectorized_call_of_a_callee_unresolved_after_selection_is_refused() {
    let error = Compiler::new()
        .model("VSel.Unresolved")
        .compile_str(SOURCE, "VSel.mo")
        .expect_err("a partial callee is not selected at translation");
    assert!(
        error.to_string().contains("Medium.prop"),
        "unexpected diagnostic: {error}"
    );
}
