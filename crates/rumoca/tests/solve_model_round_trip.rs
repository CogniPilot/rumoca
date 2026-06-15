//! Gating check for the lazy diffsol WASM addon: a `SolveModel` produced by the
//! main module must survive a serialize→deserialize round-trip and simulate
//! identically. VarLayout has `#[serde(skip)]` fields (`indexed_bindings`,
//! `shape_indexed_keys`, ...) that are lowering-only; this proves they aren't
//! needed at simulation time. An array model is used so `indexed_bindings`
//! (array element → scalar slot) is actually populated and exercised.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, lower_dae_for_simulation, simulate_solve_model};

const ARRAY_SOURCE: &str = r#"
model ArrayDecay
  Real x[3](each start = 1.0);
equation
  for i in 1:3 loop
    der(x[i]) = -x[i];
  end for;
end ArrayDecay;
"#;

#[test]
fn solve_model_round_trip_simulates_identically() {
    let compiled = Compiler::new()
        .model("ArrayDecay")
        .compile_str(ARRAY_SOURCE, "array.mo")
        .expect("compile ArrayDecay");
    let opts = SimOptions {
        solver_mode: SimSolverMode::RkLike,
        t_end: 1.0,
        dt: Some(0.05),
        ..Default::default()
    };
    let model = lower_dae_for_simulation(&compiled.dae, &opts).expect("lower solve model");

    // The addon boundary: hand the SolveModel across as JSON.
    let json = serde_json::to_string(&model).expect("serialize SolveModel");
    let round_tripped: rumoca_ir_solve::SolveModel =
        serde_json::from_str(&json).expect("deserialize SolveModel");

    let original = simulate_solve_model(&model, &opts).expect("simulate original");
    let after = simulate_solve_model(&round_tripped, &opts).expect("simulate round-tripped");

    assert_eq!(
        original.names, after.names,
        "variable names changed after round-trip"
    );
    assert_eq!(
        original.data.len(),
        after.data.len(),
        "series count changed after round-trip"
    );
    for (i, (a, b)) in original.data.iter().zip(after.data.iter()).enumerate() {
        assert_eq!(
            a.len(),
            b.len(),
            "series {i} length changed after round-trip"
        );
        for (j, (va, vb)) in a.iter().zip(b.iter()).enumerate() {
            assert!(
                (va - vb).abs() < 1e-12,
                "series {i}[{j}] differs after round-trip: {va} vs {vb}"
            );
        }
    }

    // Sanity: the indexed array variable is present and physically reasonable.
    let xi = original
        .names
        .iter()
        .position(|n| n == "x[1]")
        .expect("x[1] should be an output of the array model");
    let last = *original.data[xi].last().expect("non-empty x[1] series");
    assert!(
        (last - (-1.0_f64).exp()).abs() < 0.05,
        "x[1](1) should be ~e^-1, got {last}"
    );
}
