//! A package function named like a built-in at translation (MLS 3.7 §5.3).
//!
//! Name lookup searches the enclosing scopes before the global built-ins, so
//! a call to `abs` inside a package that declares its own `abs` names that
//! function. A constant bound to such a call folds to that function's value,
//! the same value the call has at run time; elsewhere `abs` is the built-in.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const PACKAGE: &str = r#"
package Lib
  function abs
    input Real x;
    output Real y;
  algorithm
    y := 100*x;
  end abs;
  function sign
    input Real x;
    output Real y;
  algorithm
    y := x + 10;
  end sign;
  function twiceAbs
    input Real x;
    output Real y;
  algorithm
    y := 2*abs(x);
  end twiceAbs;
  model Folded
    constant Real a = abs(-2);
    constant Real s = sign(-2);
    constant Real t = twiceAbs(-2);
    Real ya = a;
    Real ys = s;
    Real yt = t;
  end Folded;
  model AtRunTime
    Real ya = abs(-2 - 0*time);
    Real ys = sign(-2 - 0*time);
    Real yt = twiceAbs(-2 - 0*time);
  end AtRunTime;
end Lib;

model Outside
  constant Real a = abs(-2);
  Real ya = a;
end Outside;
"#;

/// The final value of each of `variables` in a short simulation of `model`.
fn values(model: &str, variables: &[&str]) -> Vec<f64> {
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
    variables
        .iter()
        .map(|variable| {
            let column = result
                .names
                .iter()
                .position(|name| name == variable)
                .unwrap_or_else(|| panic!("{variable} is recorded"));
            *result.data[column].last().expect("trace is nonempty")
        })
        .collect()
}

#[test]
fn a_folded_call_to_a_package_function_named_like_a_builtin_is_that_function() {
    let folded = values("Lib.Folded", &["ya", "ys", "yt"]);
    assert_eq!(folded, [-200.0, 8.0, -400.0]);
    assert_eq!(
        folded,
        values("Lib.AtRunTime", &["ya", "ys", "yt"]),
        "translation and run time give the call the same value"
    );
}

#[test]
fn outside_the_package_the_name_is_the_builtin() {
    assert_eq!(values("Outside", &["ya"]), [2.0]);
}
