//! Translation-time evaluation of vectorized calls and element-wise operators.
//!
//! `Incompressible.TableBased` binds
//! `invTK = 1 ./ Cv.from_degC(tableViscosity[:, 1])`: a scalar function
//! applied to an array argument maps over its elements (MLS 3.7 §12.4.6), and
//! `./` with a scalar operand applies to every element (MLS 3.7 §10.6.5). A
//! parameter binding that reads such a function result is evaluated at
//! translation, so the evaluator must apply both rules exactly as the run-time
//! lowering does.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model Vectorized
  function toK
    input Real c;
    output Real k;
  algorithm
    k := c + 273.15;
  end toK;
  function f
    input Real T;
    output Real y;
  protected
    Real invTK[2] = 1 ./ toK({10, 20});
    Real shifted[2] = {1, 2} .+ 1;
    Real halved[2] = {1, 2} / 2;
  algorithm
    y := invTK[1]*T + invTK[2] + sum(shifted) + sum(halved);
  end f;
  parameter Real p = f(300);
  Real y = f(300 + time);
  Real z = p*(1 + time);
end Vectorized;
"#;

#[test]
fn a_vectorized_call_in_a_parameter_binding_evaluates_element_wise() {
    let compiled = Compiler::new()
        .model("Vectorized")
        .compile_str(SOURCE, "Vectorized.mo")
        .unwrap_or_else(|error| panic!("Vectorized compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Vectorized simulates: {error}"));
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    let f = |t: f64| t / 283.15 + 1.0 / 293.15 + 5.0 + 1.5;
    assert!((column("z")[0] - f(300.0)).abs() < 1e-12);
    for (row, &time) in result.times.iter().enumerate() {
        assert!((column("y")[row] - f(300.0 + time)).abs() < 1e-9);
    }
}
