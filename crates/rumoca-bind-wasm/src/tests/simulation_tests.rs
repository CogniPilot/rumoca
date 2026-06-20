use super::*;

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
#[test]
fn test_simulate_model_wrapper_returns_time_series_payload() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");

    let source = r#"
    model Decay
      Real x(start=1, fixed=true);
    equation
      der(x) = -x;
    end Decay;
    "#;

    let json = simulate_model(source, "Decay", 0.2, 0.1, "auto", "{}")
        .expect("simulate_model wrapper should return simulation output");
    let simulation: serde_json::Value =
        serde_json::from_str(&json).expect("simulation payload should be valid JSON");
    let payload = simulation
        .get("payload")
        .expect("simulation output should include nested payload");
    let names = payload
        .get("names")
        .and_then(serde_json::Value::as_array)
        .expect("simulation payload should include names");
    let all_data = payload
        .get("allData")
        .and_then(serde_json::Value::as_array)
        .expect("simulation payload should include allData columns");
    let times = all_data
        .first()
        .and_then(serde_json::Value::as_array)
        .expect("simulation payload should include time samples in allData[0]");
    let data = &all_data[1..];

    assert!(
        !times.is_empty(),
        "expected simulation payload to include sampled times"
    );
    assert!(
        names.iter().any(|value| value.as_str() == Some("x")),
        "expected simulation payload to include state name `x`: {simulation:?}"
    );
    assert_eq!(
        data.len(),
        names.len(),
        "expected one data column per variable after the time column"
    );
    assert!(
        data.iter().all(|series| {
            series
                .as_array()
                .is_some_and(|samples| samples.len() == times.len())
        }),
        "expected each data series to align with the sampled times"
    );
    assert_eq!(
        payload.get("nStates").and_then(serde_json::Value::as_u64),
        Some(1)
    );

    clear_source_root_cache().expect("clear source-root cache");
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
#[test]
fn test_parameter_metadata_and_overrides_are_used_by_simulation() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");

    let source = r#"
    model TunableDecay
      parameter Real k(unit = "1/s", min = 0, max = 10, nominal = 1) = 0.5 "Decay rate";
      Real x(start = 1, fixed = true);
    equation
      der(x) = -k * x;
    end TunableDecay;
    "#;

    let metadata = crate::simulation_api::model_parameter_metadata_impl(source, "TunableDecay")
        .expect("parameter metadata should compile");
    let metadata: serde_json::Value =
        serde_json::from_str(&metadata).expect("metadata payload should be JSON");
    let parameter = metadata
        .as_array()
        .and_then(|items| items.iter().find(|item| item["name"] == "k"))
        .expect("metadata should include tunable parameter k");
    assert_eq!(parameter["unit"], "1/s");
    assert_eq!(parameter["description"], "Decay rate");
    assert_eq!(parameter["minValue"], 0.0);
    assert_eq!(parameter["maxValue"], 10.0);
    assert!(
        (parameter["defaultValue"].as_f64().unwrap_or_default() - 0.5).abs() < 1e-9,
        "expected default k = 0.5, got {parameter:?}"
    );

    let baseline = simulate_model(source, "TunableDecay", 1.0, 0.05, "auto", "{}")
        .expect("baseline simulation should run");
    let overridden = simulate_model(source, "TunableDecay", 1.0, 0.05, "auto", r#"{"k": 2.0}"#)
        .expect("override simulation should run");

    let baseline_x = last_series_value(&baseline, "x");
    let overridden_x = last_series_value(&overridden, "x");
    assert!(
        overridden_x < baseline_x,
        "larger k should decay x faster: baseline={baseline_x}, overridden={overridden_x}"
    );

    clear_source_root_cache().expect("clear source-root cache");
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
#[test]
fn test_simulate_model_wrapper_surfaces_velocity_series_for_reinit_model() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");

    let source = r#"
    model BallWasmSmoke
      Real x(start = 1.0, fixed = true);
      Real v(start = 0.0, fixed = true);
    equation
      der(x) = v;
      der(v) = -9.81;
      when time >= 0.5 then
        reinit(v, 2.0);
      end when;
    end BallWasmSmoke;
    "#;

    let json = simulate_model(source, "BallWasmSmoke", 1.5, 0.01, "auto", "{}")
        .expect("simulate_model wrapper should handle time-event reinit state export");
    let simulation: serde_json::Value =
        serde_json::from_str(&json).expect("simulation payload should be valid JSON");
    let payload = simulation
        .get("payload")
        .expect("simulation output should include nested payload");
    let names = payload["names"]
        .as_array()
        .expect("simulation payload should include names");
    let all_data = payload["allData"]
        .as_array()
        .expect("simulation payload should include allData columns");
    let times = all_data[0]
        .as_array()
        .expect("simulation payload should include time samples");
    let name_index = |name: &str| {
        names
            .iter()
            .position(|value| value.as_str() == Some(name))
            .expect("expected named simulation series")
    };
    let x_idx = name_index("x");
    let v_idx = name_index("v");
    let x: Vec<f64> = all_data[x_idx + 1]
        .as_array()
        .expect("x series should be present")
        .iter()
        .map(|value| value.as_f64().expect("x samples must be numeric"))
        .collect();
    let v: Vec<f64> = all_data[v_idx + 1]
        .as_array()
        .expect("v series should be present")
        .iter()
        .map(|value| value.as_f64().expect("v samples must be numeric"))
        .collect();

    assert!(
        times.len() >= 20,
        "expected at least 20 samples for time-event reinit smoke, got {}",
        times.len()
    );
    assert_eq!(
        payload.get("nStates").and_then(serde_json::Value::as_u64),
        Some(2)
    );
    assert!(
        x.iter()
            .copied()
            .zip(x.iter().copied().skip(1))
            .any(|(prev, next)| next < prev),
        "expected x to decrease under gravity, got x={x:?}"
    );
    assert!(
        v.iter().copied().any(|value| value < -0.5),
        "expected nonzero downward speed, got v={v:?}"
    );
    let first_downward = v
        .iter()
        .position(|value| *value < -0.5)
        .expect("expected downward velocity before bounce");
    assert!(
        v.iter().skip(first_downward).any(|value| *value > 0.5),
        "expected time-event reinit to reset velocity upward, got v={v:?}"
    );

    clear_source_root_cache().expect("clear source-root cache");
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
#[test]
fn test_simulate_model_wrapper_advances_after_relation_root_crossing() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");

    let source = r#"
    model RootCrossWasmSmoke
      Real x(start = 0.0);
      Real s(start = 0.0);
      Real y;
    equation
      der(x) = 1.0;
      der(s) = if x < 0.53 then 0.0 else 1.0;
      y = if x < 0.53 then 0.0 else 1.0;
    end RootCrossWasmSmoke;
    "#;

    let json = simulate_model(source, "RootCrossWasmSmoke", 1.0, 0.05, "auto", "{}")
        .expect("simulate_model wrapper should advance after relation root crossing");
    let simulation: serde_json::Value =
        serde_json::from_str(&json).expect("simulation payload should be valid JSON");
    let payload = simulation
        .get("payload")
        .expect("simulation output should include nested payload");
    let names = payload["names"]
        .as_array()
        .expect("simulation payload should include names");
    let all_data = payload["allData"]
        .as_array()
        .expect("simulation payload should include allData columns");
    let name_index = |name: &str| {
        names
            .iter()
            .position(|value| value.as_str() == Some(name))
            .expect("expected named simulation series")
    };
    let values = |name: &str| -> Vec<f64> {
        all_data[name_index(name) + 1]
            .as_array()
            .expect("series should be present")
            .iter()
            .map(|value| value.as_f64().expect("samples must be numeric"))
            .collect()
    };
    let y = values("y");
    let s = values("s");

    assert!(y.iter().copied().fold(0.0, f64::max) >= 0.9);
    assert!(s.last().copied().unwrap_or(0.0) >= 0.3);

    clear_source_root_cache().expect("clear source-root cache");
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
fn last_series_value(simulation_json: &str, name: &str) -> f64 {
    let simulation: serde_json::Value =
        serde_json::from_str(simulation_json).expect("simulation payload should be JSON");
    let payload = simulation
        .get("payload")
        .expect("simulation output should include nested payload");
    let names = payload["names"]
        .as_array()
        .expect("simulation payload should include names");
    let all_data = payload["allData"]
        .as_array()
        .expect("simulation payload should include allData columns");
    let index = names
        .iter()
        .position(|value| value.as_str() == Some(name))
        .expect("series should be present");
    all_data[index + 1]
        .as_array()
        .and_then(|values| values.last())
        .and_then(serde_json::Value::as_f64)
        .expect("series should have numeric samples")
}

/// The lazy-diffsol path end-to-end (native): the main module lowers a model to
/// SolveModel JSON (`lower_model_to_solve_json`), and that JSON deserializes and
/// simulates with diffsol — proving the JSON the addon receives is complete.
#[cfg(feature = "sim-diffsol")]
#[test]
fn lower_to_solve_json_feeds_diffsol_simulation() {
    let _guard = session_test_guard();
    let source = "model Decay\n  parameter Real k = 1.0;\n  Real x(start = 1.0);\nequation\n  der(x) = -k * x;\nend Decay;\n";

    let payload =
        crate::simulation_api::lower_model_to_solve_json_impl(source, "Decay", 1.0, 0.05, "{}")
            .expect("lower model to solve-model payload");
    let value: serde_json::Value = serde_json::from_str(&payload).expect("parse lowering payload");
    assert!(
        (value["t_end"].as_f64().unwrap() - 1.0).abs() < 1e-9,
        "payload should carry the resolved t_end"
    );
    let model: rumoca_ir_solve::SolveModel =
        serde_json::from_value(value["solve_model"].clone()).expect("deserialize solve_model");

    let opts = rumoca_sim::SimOptions {
        solver_mode: rumoca_sim::SimSolverMode::Bdf,
        t_end: 1.0,
        dt: Some(0.05),
        ..Default::default()
    };
    let result = rumoca_sim::simulate_solve_model(&model, &opts).expect("diffsol simulation");
    let xi = result
        .names
        .iter()
        .position(|n| n == "x")
        .expect("x in results");
    let last = *result.data[xi].last().expect("non-empty x series");
    assert!(
        (last - (-1.0_f64).exp()).abs() < 0.02,
        "x(1) should be ~e^-1, got {last}"
    );
}
