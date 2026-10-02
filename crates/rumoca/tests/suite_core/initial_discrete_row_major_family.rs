//! An initial equation over a whole discrete array (MLS 3.7 §8.6).

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

/// An initial equation over a whole discrete array (`pre(reset) =
/// fill(false, nReset)` in `Modelica.Blocks.Sources.RadioButtonSource`) is
/// one materialized row whose structured family projects its elements
/// row-major; claiming that row claims the family, so no element becomes an
/// initialization residual.
const ARRAY_SOURCE: &str = r#"
model ArrayInit
  parameter Integer n = 3;
  Boolean reset[n] = {time > 0.2, time > 0.4, time > 0.6};
  Boolean seen[n];
initial equation
  pre(reset) = fill(false, n);
equation
  seen = pre(reset);
end ArrayInit;
"#;

#[test]
fn an_initial_array_definition_claims_its_row_major_family() {
    let compiled = Compiler::new()
        .model("ArrayInit")
        .compile_str(ARRAY_SOURCE, "ArrayInit.mo")
        .unwrap_or_else(|error| panic!("ArrayInit compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("ArrayInit simulates: {error}"));
    for element in 1..=3 {
        let index = result
            .names
            .iter()
            .position(|candidate| *candidate == format!("seen[{element}]"))
            .expect("seen is recorded");
        assert_eq!(result.data[index][0], 0.0);
        assert_eq!(*result.data[index].last().expect("samples"), 1.0);
    }
}
