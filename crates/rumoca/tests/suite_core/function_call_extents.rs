//! MLS 3.7 §12.2: a dimension of a function local may call another function
//! of the inputs, as `Modelica.Electrical.Polyphase.Functions` sizes
//! `oBase[numberOfSymmetricBaseSystems(m)]`. Each call site proves the input
//! values, and the extent is the value the called function returns for them.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const MODELS: &str = r#"
package Extents
  function bases
    input Integer m;
    output Integer n;
  algorithm
    n := if mod(m, 2) == 0 then 2 else 1;
  end bases;
  function total
    input Integer m;
    output Real y;
  protected
    Real o[bases(m)] = fill(1.0, bases(m));
  algorithm
    y := sum(o) + m;
  end total;
  model M
    Real even = total(4)*time;
    Real odd = total(3)*time;
  end M;
end Extents;
"#;

#[test]
fn a_local_extent_calls_a_function_of_the_inputs() {
    let compiled = Compiler::new()
        .model("Extents.M")
        .compile_str(MODELS, "Extents.mo")
        .expect("the model compiles");
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .expect("the model simulates");
    let last = |name: &str| {
        let column = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .expect("the result records the column");
        *result.data[column].last().expect("a sample")
    };
    // total(4) sums two ones, total(3) one.
    assert!(
        (last("even") - 6.0).abs() < 1e-12,
        "even = {}",
        last("even")
    );
    assert!((last("odd") - 4.0).abs() < 1e-12, "odd = {}", last("odd"));
}
