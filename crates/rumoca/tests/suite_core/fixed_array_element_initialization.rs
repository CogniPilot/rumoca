//! Per-element `fixed` on an array component (MLS 3.6 section 4.8 and 4.8.6).
//!
//! An attribute modifier on an array component is itself an array of the
//! component's dimensions; each element governs the corresponding component
//! element independently. For `Real x[3](start = {2, 1, 0}, fixed = {true,
//! false, false})` the first element is initialized from its start value while
//! the other two are free initialization unknowns. The fixture pairs the two
//! free elements with explicit initial equations so a regression that collapses
//! the array to a single Boolean is observable: taking element zero would pin
//! every element to its start and clash with those initial equations, and
//! taking `false` would leave the fixed element unpinned.
//!
//! Expected values are the OpenModelica reference on the same source (dassl,
//! default tolerance): x = {2, 5, 7} at every sample.

use std::collections::HashMap;

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimResult, simulate_dae_with_diagnostics};

const FIXTURE: &str = include_str!("../fixtures/attribute_semantics/FixedArrayElementInit.mo");

fn series<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("simulation result missing column {name}"));
    result.data[index].as_slice()
}

#[test]
fn fixed_array_pins_only_the_fixed_element_and_frees_the_rest() {
    let compiled = Compiler::new()
        .model("FixedArrayElementInit")
        .compile_str(FIXTURE, "FixedArrayElementInit.mo")
        .expect("a per-element Boolean `fixed` array instantiates and compiles");

    // Exactly one scalar coordinate carries a source-fixed start into the
    // initialization system: the single `fixed = true` element. A whole-array
    // reading would report three (or none).
    let lowered =
        rumoca_phase_solve::lower_solve_model(&compiled.dae, &HashMap::new(), |_| {}).unwrap();
    assert_eq!(
        lowered
            .model()
            .problem
            .initialization
            .given_state_indices()
            .len(),
        1
    );

    let result = simulate_dae_with_diagnostics(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .expect("the per-element initialization is well posed and simulates");

    // x[1] is fixed, so it keeps its start value 2; x[2] and x[3] are free and
    // are owned by the initial equations, taking 5 and 7 rather than their
    // start guesses 1 and 0. der(x) = 0 holds every element constant.
    for (name, expected) in [("x[1]", 2.0), ("x[2]", 5.0), ("x[3]", 7.0)] {
        let values = series(&result, name);
        for value in values {
            assert!(
                (value - expected).abs() < 1.0e-9,
                "{name}: expected {expected}, lowered {value}"
            );
        }
    }
}
