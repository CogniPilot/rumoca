//! A function that extends both a bodyless interface and an implementation.
//!
//! MLS §7.1 merges the elements a class inherits from several bases, and §12.2
//! gives a function at most one algorithm section or external interface. A
//! function alias that extends a partial interface function and, separately,
//! a function implementing that interface (the pattern of
//! `Modelica.Utilities.Files.loadResource`) therefore has exactly one body: the
//! implementing base's.

use rumoca_sim::{SimOptions, simulate_dae};

const SOURCE: &str = r#"
package Interfaces
  partial function Interface
    input Real u;
    output Real y;
  end Interface;
end Interfaces;

package Services
  function Implementation
    extends Interfaces.Interface;
  algorithm
    y := 2*u + 1;
  end Implementation;
end Services;

package Lib
  function Alias
    extends Interfaces.Interface;
    extends Services.Implementation;
  end Alias;
end Lib;

model InterfaceAndBody
  parameter Real p = 3;
  Real x(start = 0, fixed = true);
equation
  der(x) = Lib.Alias(p);
end InterfaceAndBody;
"#;

#[test]
fn an_alias_of_an_interface_and_its_implementation_calls_the_implementation() {
    let compiled = rumoca::Compiler::new()
        .model("InterfaceAndBody")
        .compile_str(SOURCE, "interface_and_body.mo")
        .expect("the alias selects the one base that declares a body");
    let sim = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .expect("model simulates");
    let column = sim
        .names
        .iter()
        .position(|name| name == "x")
        .expect("trace contains x");
    let last = *sim.data[column].last().expect("trace is nonempty");
    assert!(
        (last - 7.0).abs() < 1.0e-6,
        "der(x) = 2*p + 1 = 7 integrates to 7 at t = 1, got {last}"
    );
}
