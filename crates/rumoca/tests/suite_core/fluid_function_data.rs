//! Function data the Modelica.Fluid fittings and incompressible media read:
//!
//! - A protected binding reads fields of a record input (MLS 3.7 §12.4.4), as
//!   `Real k1 = f(if data.zeta1_at_a then data.diameter_a else data.diameter_b)`
//!   in `QuadraticTurbulent.pressureLoss_m_flow`.
//! - A record result assembled field by field has fields that share a name
//!   prefix (`zeta1` and `zeta1_at_a`); assigning `zeta1` does not assign
//!   `zeta1_at_a`.
//! - A package constant whose binding selects from a sibling constant an extends
//!   modification binds (`poly_rho = fit(tableDensity[:, 1], ...)` in
//!   `Incompressible.TableBased`) reads the modified value (MLS 3.7 §7.2).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

fn final_value(source: &str, model: &str, column: &str) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, "FluidFunctionData.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error}"));
    let index = result
        .names
        .iter()
        .position(|name| name == column)
        .unwrap_or_else(|| panic!("{column} in {:?}", result.names));
    *result.data[index].last().expect("trace has samples")
}

const PROTECTED_RECORD_FIELDS: &str = r#"
package F1
  record Data
    Real diameter_a;
    Real diameter_b;
    Boolean zeta1_at_a = true;
    Real zeta1;
  end Data;
  function k
    input Real D;
    input Real zeta;
    output Real y;
  algorithm
    y := zeta/D^2;
  end k;
  function loss
    input Real m;
    input Data data;
    output Real dp;
  protected
    Real k1 = k(if data.zeta1_at_a then data.diameter_a else data.diameter_b, data.zeta1);
  algorithm
    dp := k1*m;
  end loss;
  model Top
    parameter Data data(diameter_a = 0.1, diameter_b = 0.2, zeta1 = 1);
    Real dp = loss(1 + time, data);
  end Top;
end F1;
"#;

#[test]
fn a_protected_binding_reads_record_input_fields() {
    // dp = zeta1/diameter_a^2*m = 1/0.01*2 at t = 1.
    let dp = final_value(PROTECTED_RECORD_FIELDS, "F1.Top", "dp");
    assert!((dp - 200.0).abs() < 1e-9, "dp = {dp}");
}

const PREFIX_SHARING_FIELDS: &str = r#"
package F3
  record Data
    Real zeta1;
    Boolean zeta1_at_a;
  end Data;
  function make
    input Real d;
    output Data data;
  algorithm
    if d > 0.5 then
      data.zeta1 := d;
      data.zeta1_at_a := true;
    else
      data.zeta1 := 2*d;
      data.zeta1_at_a := false;
    end if;
  end make;
  function signed
    input Real d;
    output Real y;
  protected
    Data data = make(d);
  algorithm
    y := if data.zeta1_at_a then data.zeta1 else -data.zeta1;
  end signed;
  model Top
    Real y = signed(time);
  end Top;
end F3;
"#;

#[test]
fn a_record_field_sharing_a_name_prefix_is_assembled_on_its_own() {
    // At t = 1 the first branch sets zeta1 = 1 and zeta1_at_a = true.
    let y = final_value(PREFIX_SHARING_FIELDS, "F3.Top", "y");
    assert!((y - 1.0).abs() < 1e-9, "y = {y}");
}

const MODIFIED_TABLE_CONSTANT: &str = r#"
package G1
  function fit
    input Real u[:];
    input Real y[:];
    input Integer n;
    output Real p[n + 1];
  algorithm
    for i in 1:n + 1 loop
      p[i] := sum(y)/size(y, 1)/i;
    end for;
  end fit;
  function evaluate
    input Real p[:];
    input Real x;
    output Real y;
  algorithm
    y := 0;
    for i in 1:size(p, 1) loop
      y := y*x + p[i];
    end for;
  end evaluate;
  partial package TableBased
    constant Real[:, 2] tableDensity;
    constant Integer npol = 2;
    constant Boolean hasDensity = not (size(tableDensity, 1) == 0);
    final constant Real poly_rho[:] = if hasDensity then fit(tableDensity[:, 1], tableDensity[:, 2], npol) else zeros(npol + 1);
    function density
      input Real T;
      output Real d;
    algorithm
      d := evaluate(poly_rho, T);
    end density;
  end TableBased;
  package Glycol
    extends TableBased(tableDensity = [0, 1000; 10, 1010; 20, 1030]);
  end Glycol;
  model Top
    package Medium = Glycol;
    Real d = Medium.density(time);
  end Top;
end G1;
"#;

#[test]
fn a_sibling_binding_selects_from_an_extends_modified_constant() {
    // fit gives p[i] = 1013.33/i from Glycol's table; Horner at T = 1 sums them.
    let d = final_value(MODIFIED_TABLE_CONSTANT, "G1.Top", "d");
    let expected = 3040.0 / 3.0 * (1.0 + 0.5 + 1.0 / 3.0);
    assert!((d - expected).abs() < 1e-9, "d = {d}, expected {expected}");
}
