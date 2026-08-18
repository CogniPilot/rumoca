//! Model-algorithm ranges over translation-time parameters (MLS §10.4.1,
//! §11.2.2).
//!
//! `Modelica.Electrical.Digital.Sources.Table` declares
//! `final parameter Integer n = size(x, 1)` and retains `for i in 1:n` in its
//! Flat algorithm. DAE construction already proves the same model-scope range
//! in equation expressions; algorithm validation must consume that proof too.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const SHAPE_DERIVED_RANGE: &str = r#"
model ShapeDerivedAlgorithmRange
  parameter Real values[:] = {2.0, 4.0, 6.0};
  final parameter Integer n = size(values, 1);
  Real y[n];
algorithm
  for i in 1:n loop
    y[i] := values[i] + time;
  end for;
end ShapeDerivedAlgorithmRange;
"#;

fn simulate(source: &str, model: &str) -> rumoca_sim::SimResult {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .expect("a translation-settled algorithm range has a checked DAE owner");
    let wire = serde_json::to_string(&compiled.dae).expect("the checked DAE serializes");
    let decoded: rumoca_compile::compile::Dae =
        serde_json::from_str(&wire).expect("the checked DAE reconstructs from wire form");
    simulate_dae(&decoded, &SimOptions::default()).expect("the compact algorithm range simulates")
}

#[test]
fn shape_derived_final_parameter_proves_the_algorithm_domain() {
    let simulation = simulate(SHAPE_DERIVED_RANGE, "ShapeDerivedAlgorithmRange");
    for (name, offset) in [("y[1]", 2.0), ("y[2]", 4.0), ("y[3]", 6.0)] {
        let output = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("simulation exposes {name}"));
        for (time, value) in simulation.times.iter().zip(&simulation.data[output]) {
            assert!(
                (*value - (offset + time)).abs() <= 1.0e-9,
                "{name}({time}) should equal {offset} + time, found {value}"
            );
        }
    }
}

#[test]
fn plain_evaluable_parameter_proves_the_algorithm_domain() {
    let simulation = simulate(
        r#"
model PlainParameterAlgorithmRange
  parameter Integer n = 3;
  Real y[3];
algorithm
  for i in 1:n loop
    y[i] := i;
  end for;
end PlainParameterAlgorithmRange;
"#,
        "PlainParameterAlgorithmRange",
    );
    for (name, expected) in [("y[1]", 1.0), ("y[2]", 2.0), ("y[3]", 3.0)] {
        let output = simulation
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("simulation exposes {name}"));
        assert!(
            simulation.data[output]
                .iter()
                .all(|value| (*value - expected).abs() <= 1.0e-12),
            "{name} should remain {expected}"
        );
    }
}

#[test]
fn fixed_false_parameter_default_does_not_prove_the_algorithm_domain() {
    let error = Compiler::new()
        .model("InitializationOwnedAlgorithmRange")
        .compile_str(
            r#"
model InitializationOwnedAlgorithmRange
  parameter Integer n(fixed = false) = 4;
  Real y[4];
algorithm
  for i in 1:n loop
    y[i] := i;
  end for;
end InitializationOwnedAlgorithmRange;
"#,
            "InitializationOwnedAlgorithmRange.mo",
        )
        .expect_err("a fixed=false default is not a translation-time loop-domain proof");
    let rendered = format!("{error:?}");
    assert!(
        rendered.contains("range end")
            && rendered.contains("canonical compact range requires an integer literal bound"),
        "the unproved range must keep its typed DAE rejection: {rendered}"
    );
}
