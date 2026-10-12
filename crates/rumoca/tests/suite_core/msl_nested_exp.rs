//! Distinguish a nested checked call from the direct exponential wrapper.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
function polynomialExponential
  input Real T;
  output Real y;
algorithm
  y := Modelica.Math.exp(Modelica.Math.Polynomials.evaluate({2, 3}, 1/T));
end polynomialExponential;
model DirectExponential
  output Real y = Modelica.Math.exp(2/(1 + time) + 3);
end DirectExponential;
model NestedExponential
  output Real y = polynomialExponential(1 + time);
end NestedExponential;
"#;

#[test]
fn nested_exponential_polynomial_call_uses_its_checked_interface() {
    let root = super::native_linear_solve::msl_root().expect("the required MSL fixture exists");
    for model in ["DirectExponential", "NestedExponential"] {
        let compiled = Compiler::new()
            .model(model)
            .source_root(root.to_string_lossy().as_ref())
            .compile_str(SOURCE, "NestedExponential.mo")
            .unwrap_or_else(|error| panic!("{model} compiles: {error}"));
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.01,
                dt: Some(0.005),
                ..SimOptions::default()
            },
        )
        .unwrap_or_else(|error| panic!("{model} simulates: {error}"));
        assert_eq!(result.times, [0.0, 0.005, 0.01]);
        let column = result.names.iter().position(|name| name == "y").unwrap();
        assert_eq!(result.data[column].len(), result.times.len());
        for (time, actual) in result.times.iter().zip(&result.data[column]) {
            let expected = (2.0 / (1.0 + time) + 3.0).exp();
            assert!(
                actual.is_finite() && (actual - expected).abs() < 1e-10,
                "{model}: y at {time}: {actual} != {expected}"
            );
        }
    }
}
