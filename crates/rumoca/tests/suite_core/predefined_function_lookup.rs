//! MLS 3.7 §5.3: a predefined function name is looked up like any other name.
//!
//! The predefined functions (`exp`, `sin`, ...) are members of the global
//! scope, so an import or an enclosing declaration of the same name shadows
//! one (§5.3.1), and a leading-dot name such as `.exp` is looked up in the
//! global scope alone (§5.3.3), reaching the predefined function past any
//! enclosing declaration. `Modelica.ComplexMath` relies on both: its
//! functions call `.sqrt` and `.exp` for the Real functions, and models import
//! `Modelica.ComplexMath.exp` to call the complex exponential as `exp`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const MODELS: &str = r#"
package P
  function exp "shadows the predefined exp inside P"
    input Real x;
    output Real y;
  algorithm
    y := 2*x;
  end exp;
  model Enclosing
    Real global = .exp(1.0);
    Real enclosing = exp(1.0);
  end Enclosing;
end P;
model Imported
  import P.exp;
  Real imported = exp(1.0);
  Real global = .exp(1.0);
end Imported;
"#;

fn simulate(model: &str) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "Lookup.mo")
        .expect("the model compiles");
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .expect("the model simulates")
}

fn final_value(result: &SimResult, name: &str) -> f64 {
    let column = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .expect("the result records the column");
    *result.data[column].last().expect("a sample")
}

fn assert_value(result: &SimResult, name: &str, expected: f64) {
    let actual = final_value(result, name);
    assert!(
        (actual - expected).abs() < 1e-12,
        "{name} = {actual}, expected {expected}"
    );
}

#[test]
fn an_enclosing_function_shadows_a_predefined_name_but_not_a_global_one() {
    let result = simulate("P.Enclosing");
    assert_value(&result, "enclosing", 2.0);
    assert_value(&result, "global", 1.0_f64.exp());
}

#[test]
fn an_imported_function_shadows_a_predefined_name() {
    let result = simulate("Imported");
    assert_value(&result, "imported", 2.0);
    assert_value(&result, "global", 1.0_f64.exp());
}
