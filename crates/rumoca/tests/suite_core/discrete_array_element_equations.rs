//! An array equation is its element equations (MLS 3.7 §10.6.1). A slice
//! over a component array, `split.set = fill(inPort.set, n)` in
//! `Modelica.StateGraph.Parallel`, flattens to `{split[1].set, ...} = e`;
//! each discrete-valued element is defined by its own element equation
//! `split[i].set = e[i]`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
connector C
  output Boolean set;
  output Integer k;
end C;
model Split
  parameter Integer n = 3;
  C split[n];
  Boolean inSet = time > 0.5;
equation
  split.set = fill(inSet, n);
  split.k = {1, 2, 3}*(if inSet then 2 else 1);
end Split;
"#;

#[test]
fn a_discrete_component_array_slice_equation_defines_each_element() {
    let compiled = Compiler::new()
        .model("Split")
        .compile_str(SOURCE, "Split.mo")
        .unwrap_or_else(|error| panic!("Split compiles: {error:?}"));
    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .unwrap_or_else(|error| panic!("Split simulates: {error}"));
    let last = |name: &str| {
        let index = result
            .names
            .iter()
            .position(|candidate| candidate == name)
            .unwrap_or_else(|| panic!("{name} is recorded"));
        *result.data[index].last().expect("samples")
    };
    for element in 1..=3 {
        assert_eq!(last(&format!("split[{element}].set")), 1.0);
        assert_eq!(last(&format!("split[{element}].k")), 2.0 * element as f64);
    }
}
