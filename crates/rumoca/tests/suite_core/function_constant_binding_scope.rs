//! Names in a constant binding expanded into a function body (MLS 3.7 §5.3,
//! SPEC_0040 FLAT-C02).
//!
//! A package constant read inside a function is replaced by its binding, and
//! the names that binding reads resolve in the package that declares it. A
//! function local of the same name must not capture them:
//! `Media.Incompressible.TableBased.specificEntropy` declares a local `npol`
//! while the package constant `poly_Cp` is bound to a fit of degree
//! `npolHeatCapacity = npol`, the package constant.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package Scope
  constant Integer n = 2;
  constant Integer m = n;
  constant Real c[:] = coefficients(m);
  function coefficients
    input Integer k;
    output Real y[k + 1];
  algorithm
    y := fill(1.0, k + 1);
  end coefficients;
  function shadowedExtent
    input Real t;
    output Real d;
  protected
    Integer n = size(c, 1) - 1;
  algorithm
    d := t * sum(c) + n;
  end shadowedExtent;
  function shadowedValue
    input Real t;
    output Real d;
  protected
    Integer n = 10;
  algorithm
    d := t + m + 0 * n;
  end shadowedValue;
  model M
    Real x = shadowedExtent(time);
    Real y = shadowedValue(time);
  end M;
end Scope;
"#;

#[test]
fn function_locals_do_not_capture_names_in_constant_bindings() {
    let compiled = Compiler::new()
        .model("Scope.M")
        .compile_str(SOURCE, "Scope.mo")
        .unwrap_or_else(|error| panic!("Scope.M compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("Scope.M simulates");
    let column = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        &result.data[index]
    };
    for (sample, time) in result.times.iter().enumerate() {
        // `c` has `m + 1 = 3` ones and the local `n` is its degree 2.
        assert!((column("x")[sample] - (3.0 * time + 2.0)).abs() < 1e-12);
        // `m` is the package constant `n = 2`, not the local `n = 10`.
        assert!((column("y")[sample] - (time + 2.0)).abs() < 1e-12);
    }
}

/// A Boolean package constant bound to `not e` is a constant expression
/// (MLS 3.7 §3.5) and folds into a function body that reads it, as
/// `TableBased.hasHeatCapacity = not (size(tableHeatCapacity, 1) == 0)` does
/// in the `assert` of `specificHeatCapacityCv`.
const NEGATED_SOURCE: &str = r#"
package Negated
  partial package Base
    constant Real table[:, 2];
    constant Boolean hasTable = not (size(table, 1) == 0);
    function scaled
      input Real t;
      output Real d;
    algorithm
      assert(hasTable, "no table");
      d := if hasTable then 2 * t else t;
    end scaled;
  end Base;
  package Med
    extends Base(table = [0, 1; 1, 2]);
  end Med;
  model M
    package Medium = Med;
    Real x = Medium.scaled(time);
  end M;
end Negated;
"#;

#[test]
fn negated_boolean_constants_fold_into_function_bodies() {
    let compiled = Compiler::new()
        .model("Negated.M")
        .compile_str(NEGATED_SOURCE, "Negated.mo")
        .unwrap_or_else(|error| panic!("Negated.M compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("Negated.M simulates");
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == "x")
        .expect("x is recorded");
    for (sample, time) in result.times.iter().enumerate() {
        assert!((result.data[index][sample] - 2.0 * time).abs() < 1e-12);
    }
}
