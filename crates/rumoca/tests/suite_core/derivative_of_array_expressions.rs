//! MLS §3.7.4.2: the derivative of an array expression is the array of its
//! elements' derivatives. `der` therefore distributes over an array
//! constructor, and over a reference through an array of components
//! (`mediums.h`), which denotes the array of the elements' members (MLS §10.5).
//!
//! `Modelica.Fluid` writes the second form in an initial equation selected by
//! a parameter of its `outer System` (`der(mediums.h) = zeros(n)` under
//! `SteadyStateInitial`).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const COMPONENT_ARRAY_MEMBER: &str = r#"
model ComponentArrayMember
  model Sys
    parameter Integer mode = 1;
  end Sys;
  inner Sys system(mode = 2);
  model Props
    Real h(start = 1);
  equation
    der(h) = 3 - h;
  end Props;
  model Pipe
    outer Sys system;
    parameter Integer mode = system.mode;
    parameter Integer n = 2;
    Props mediums[n];
  initial equation
    if mode == 1 then
      mediums.h = fill(2.0, n);
    elseif mode == 2 then
      der(mediums.h) = zeros(n);
    end if;
  end Pipe;
  Pipe p;
end ComponentArrayMember;
"#;

const ARRAY_CONSTRUCTOR: &str = r#"
model ArrayConstructor
  Real x(start = 1, fixed = true);
  Real y(start = 2, fixed = true);
equation
  der({x, y}) = {-x, -2*y};
end ArrayConstructor;
"#;

fn simulate(model: &str, source: &str) -> SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("{model} simulates: {error}"))
}

fn column<'result>(result: &'result SimResult, name: &str) -> &'result [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("{name} is recorded"));
    &result.data[index]
}

/// `der(h) = 3 - h` starts at rest only from `h = 3`, so the steady-state
/// initial equation holds each element there for the whole run.
#[test]
fn der_of_a_member_through_a_component_array_is_each_elements_derivative() {
    let result = simulate("ComponentArrayMember", COMPONENT_ARRAY_MEMBER);
    for name in ["p.mediums[1].h", "p.mediums[2].h"] {
        for (sample, value) in column(&result, name).iter().enumerate() {
            assert!((value - 3.0).abs() < 1e-6, "{name}[{sample}] = {value}");
        }
    }
}

#[test]
fn der_of_an_array_constructor_is_each_elements_derivative() {
    let result = simulate("ArrayConstructor", ARRAY_CONSTRUCTOR);
    let (x, y) = (column(&result, "x"), column(&result, "y"));
    for (sample, time) in result.times.iter().enumerate() {
        assert!(
            (x[sample] - (-time).exp()).abs() < 1e-4,
            "x({time}) = {}",
            x[sample]
        );
        assert!(
            (y[sample] - 2.0 * (-2.0 * time).exp()).abs() < 1e-4,
            "y({time}) = {}",
            y[sample]
        );
    }
}
