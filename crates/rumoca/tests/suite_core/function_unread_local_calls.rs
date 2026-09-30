//! A function local that no result reads still owns its calls.
//!
//! The typed body lowering evaluates every statement in order, so a call in a
//! local nothing reads (the `aux5` local of
//! `Modelica.Fluid.Dissipation.Utilities.Functions.General.CubicInterpolation_lambda`)
//! needs a registered call owner exactly like a call a result reaches.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model UnreadLocalCall
  function g
    input Real u;
    output Real y;
  algorithm
    y := log10(u);
  end g;
  function f
    input Real u;
    output Real y;
  protected
    Real unused = -2*sqrt(u)*g(u + 1);
    Real k = g(10);
  algorithm
    y := k*u;
  end f;
  function h
    input Real u;
    output Real y;
  algorithm
    y := f(u) + 1;
  end h;
  Real x(start = 1, fixed = true);
equation
  der(x) = -h(x);
end UnreadLocalCall;
"#;

#[test]
fn a_call_in_an_unread_local_has_a_registered_owner() {
    let compiled = Compiler::new()
        .model("UnreadLocalCall")
        .compile_str(SOURCE, "UnreadLocalCall.mo")
        .unwrap_or_else(|error| panic!("UnreadLocalCall compiles: {error}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("UnreadLocalCall simulates: {error}"));
    let index = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("x is recorded");
    let x = *result.data[index].last().expect("x has samples");
    // der(x) = -(x + 1), x(0) = 1, so x(1) = 2 e^-1 - 1.
    let expected = 2.0 * (-1.0_f64).exp() - 1.0;
    assert!(
        (x - expected).abs() < 1e-5,
        "x(1) = {x}, expected {expected}"
    );
}
