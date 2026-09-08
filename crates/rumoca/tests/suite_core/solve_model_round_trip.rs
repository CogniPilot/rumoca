//! Gating check for the lazy diffsol WASM addon: a correlated FMI component
//! produced by the main module must survive canonical serialization and
//! checked phase-Solve/FMI replay, then simulate identically. Derived JVP and
//! structural artifacts are absent from the wire and mechanically reconstructed
//! before component construction. An array model exercises the compact tensor
//! derivative path.

use rumoca::Compiler;
use rumoca_sim::{
    SimOptions, SimSolverMode, lower_correlated_for_simulation_with_overrides,
    lower_dae_for_simulation, simulate_fmi_component,
};

const ARRAY_SOURCE: &str = r#"
model ArrayDecay
  Real x[3](each start = 1.0);
equation
  for i in 1:3 loop
    der(x[i]) = -x[i];
  end for;
end ArrayDecay;
"#;

fn replay_wire(wire: &serde_json::Value) -> Result<rumoca_ir_solve::SolveModel, serde_json::Error> {
    let encoded = serde_json::to_string(wire).expect("encode SolveModel wire fixture");
    let mut deserializer = serde_json::Deserializer::from_str(&encoded);
    rumoca_phase_solve::deserialize_solve_model(&mut deserializer)
}

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
    let lowered = lower_correlated_for_simulation_with_overrides(compiled.dae(), &opts)
        .expect("lower correlated model");

    // The addon boundary: hand the correlated component construction across as JSON.
    let wire = rumoca_phase_solve::fmi::fmi_component_wire(&lowered)
        .expect("construct FMI component wire");
    let json = serde_json::to_string(&wire).expect("serialize FMI component");
    let mut deserializer = serde_json::Deserializer::from_str(&json);
    let round_tripped = rumoca_phase_solve::fmi::deserialize_fmi_component(&mut deserializer)
        .expect("replay FMI component");
    let round_tripped_model = round_tripped.runtime_model();
    assert!(
        !round_tripped_model
            .artifacts()
            .continuous()
            .full_jacobian_v
            .is_empty(),
        "checked replay must mechanically reconstruct the JVP"
    );
    assert!(
        round_tripped_model
            .artifacts()
            .continuous()
            .structural
            .derivative()
            .is_some(),
        "checked replay must reconstruct derivative structure"
    );

    let original_component =
        rumoca_phase_solve::fmi::finish_fmi_component(lowered).expect("finish original component");
    let original = simulate_fmi_component(original_component, &opts).expect("simulate original");
    let after = simulate_fmi_component(round_tripped, &opts).expect("simulate round-tripped");

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

#[test]
fn solve_model_wire_rejects_caller_supplied_jvp_artifacts() {
    let compiled = Compiler::new()
        .model("ArrayDecay")
        .compile_str(ARRAY_SOURCE, "array.mo")
        .expect("compile ArrayDecay");
    let model = lower_dae_for_simulation(compiled.dae(), &SimOptions::default())
        .expect("lower solve model");
    assert!(
        !model.artifacts().continuous().full_jacobian_v.is_empty(),
        "fixture must carry a mechanically derived JVP"
    );

    let model_wire =
        rumoca_phase_solve::solve_model_wire(&model).expect("construct SolveModel wire");
    let mut wire = serde_json::to_value(model_wire).expect("serialize SolveModel");
    assert!(
        wire.get("artifacts").is_none(),
        "derived artifacts must be unrepresentable in the canonical wire"
    );
    wire["artifacts"] = serde_json::json!({
        "continuous": {
            "full_jacobian_v": {
                "programs": [],
                "program_spans": [],
                "output_indices": []
            }
        }
    });

    let error = replay_wire(&wire)
        .expect_err("wire replay must reject caller-supplied derivative artifacts");
    assert!(
        error.to_string().contains("unknown field `artifacts`"),
        "the rejection must identify the unproved artifact claim: {error}"
    );
}

#[test]
fn solve_model_replay_rejects_unproved_root_correlations() {
    let compiled = Compiler::new()
        .model("ArrayDecay")
        .compile_str(ARRAY_SOURCE, "array.mo")
        .expect("compile ArrayDecay");
    let model = lower_dae_for_simulation(compiled.dae(), &SimOptions::default())
        .expect("lower solve model");
    let model_wire =
        rumoca_phase_solve::solve_model_wire(&model).expect("construct SolveModel wire");
    let wire = serde_json::to_value(model_wire).expect("serialize SolveModel");

    let mut missing_calls = wire.clone();
    missing_calls
        .as_object_mut()
        .expect("SolveModel wire is an object")
        .remove("pure_calls");
    let error = replay_wire(&missing_calls).expect_err("pure-call owner table is mandatory");
    assert!(error.to_string().contains("pure_calls"), "{error}");

    let mut wrong_vector = wire.clone();
    wrong_vector["initial_y"] = serde_json::json!([1.0, 1.0]);
    let error =
        replay_wire(&wrong_vector).expect_err("runtime vector extent is construction-owned");
    assert!(error.to_string().contains("initial_y"), "{error}");

    let mut wrong_metadata = wire.clone();
    assert!(
        wrong_metadata.get("variable_meta").is_none(),
        "trace metadata must be derived rather than serialized as an authority"
    );
    wrong_metadata["variable_catalog"]["entries"][0]["scalar_names"] =
        serde_json::json!(["forged"]);
    let error = replay_wire(&wrong_metadata)
        .expect_err("catalog scalar identity and storage extent are one correlated projection");
    assert!(error.to_string().contains("names contain 1"), "{error}");

    let mut unsupported = wire;
    unsupported["schema_version"] =
        serde_json::json!(rumoca_phase_solve::SOLVE_MODEL_SCHEMA_VERSION.saturating_add(1));
    let error = replay_wire(&unsupported).expect_err("unsupported wire schema must fail closed");
    assert!(
        error
            .to_string()
            .contains("unsupported SolveModel schema_version"),
        "{error}"
    );
}
