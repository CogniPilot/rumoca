//! A modifier value is found in the context in which the modifier occurs
//! (MLS 3.7 §7.2, SPEC_0022 INST-001).
//!
//! `tank(s = s)` binds the component's `s` to the enclosing `s`. Translation
//! time evaluation that selects the component's `if s then` branch must read
//! the enclosing value, not the component's own declaration default: the
//! `Modelica.Fluid` AST_BatchPlant tanks pass `stiffCharacteristicForEmptyPort`
//! down this way.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
package SameName
  model Tank
    parameter Boolean s = false annotation(Evaluate = true);
    parameter Real k = 1;
    parameter Integer n = 0;
    Real y[n];
    Real z[n];
    Real w;
  equation
    for i in 1:n loop
      if s then
        z[i] = 1 + (if y[i] > 0 then 0 else 10);
        y[i] = time;
      else
        y[i] = 2;
        z[i] = 0;
      end if;
    end for;
    w = k;
  end Tank;
  model Test
    parameter Boolean s = true;
    parameter Real k = 3;
    parameter Integer n = 2;
    Tank tank(s = s, k = k, n = n);
  end Test;
end SameName;
"#;

#[test]
fn a_same_named_modifier_value_is_read_from_the_enclosing_scope() {
    let compiled = Compiler::new()
        .model("SameName.Test")
        .compile_str(SOURCE, "SameName.mo")
        .unwrap_or_else(|error| panic!("SameName.Test compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("SameName.Test simulates: {error}"));
    let last = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        *result.data[index].last().expect("samples")
    };
    // The enclosing `s = true` selects the first branch for both elements.
    for element in 1..=2 {
        assert!((last(&format!("tank.y[{element}]")) - 1.0).abs() < 1e-9);
        assert!((last(&format!("tank.z[{element}]")) - 1.0).abs() < 1e-9);
    }
    assert!((last("tank.w") - 3.0).abs() < 1e-12);
}
