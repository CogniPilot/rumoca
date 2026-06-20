#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
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
// SPEC_0021: Exception - native kernel schedule exposure is validated through
// the full wasm preparation payload in one regression.
#[allow(clippy::too_many_lines)]
fn test_prepare_gpu_simulation_exposes_native_kernel_schedules() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");

    let source = r#"
    model GpuImplicitMap
      Real x[3](each start = 1.0, each fixed = true);
    equation
      for i in 1:3 loop
        der(x[i]) = -x[i];
      end for;
    end GpuImplicitMap;
    "#;

    let json = prepare_gpu_simulation(source, "GpuImplicitMap")
        .expect("prepare_gpu_simulation should render wgsl-solve payload");
    let payload: serde_json::Value =
        serde_json::from_str(&json).expect("GPU preparation payload should be valid JSON");
    let wgsl = payload
        .get("wgsl")
        .and_then(serde_json::Value::as_str)
        .expect("GPU payload should include WGSL source");
    let derivative_kernels = payload
        .pointer("/layout/kernels")
        .and_then(serde_json::Value::as_array)
        .expect("GPU layout should include derivative RHS kernel schedule");
    let derivative_native_families = payload
        .pointer("/layout/native_families")
        .and_then(serde_json::Value::as_array)
        .expect("GPU layout should include derivative RHS native family metadata");
    let implicit_kernels = payload
        .pointer("/layout/implicit_rhs/kernels")
        .and_then(serde_json::Value::as_array)
        .expect("GPU layout should include implicit RHS kernel schedule");
    let implicit_native_families = payload
        .pointer("/layout/implicit_rhs/native_families")
        .and_then(serde_json::Value::as_array)
        .expect("GPU layout should include implicit RHS native family metadata");

    assert!(wgsl.contains("fn derivative_rhs_map0"));
    assert!(wgsl.contains("fn implicit_rhs_map0"));
    let workgroup_size = payload
        .pointer("/layout/workgroup_size")
        .and_then(serde_json::Value::as_u64)
        .expect("GPU layout should expose workgroup_size");
    let chunk_size = payload
        .pointer("/layout/chunk_size")
        .and_then(serde_json::Value::as_u64)
        .expect("GPU layout should expose chunk_size");
    assert_eq!(
        wgsl.matches(&format!("// workgroup_size={workgroup_size}"))
            .count(),
        2,
        "GPU WGSL inventory should report derivative and implicit workgroup sizes"
    );
    assert_eq!(
        wgsl.matches(&format!("// chunk_size={chunk_size}")).count(),
        2,
        "GPU WGSL inventory should report derivative and implicit scalar chunk sizes"
    );
    assert_eq!(
        payload.get("n_states").and_then(serde_json::Value::as_u64),
        Some(3)
    );
    assert_eq!(
        payload
            .pointer("/state_names/0")
            .and_then(serde_json::Value::as_str),
        Some("x[1]")
    );
    assert!(
        payload.pointer("/layout/bindings").is_none(),
        "GPU layout should not carry per-scalar name bindings: {payload:?}"
    );
    assert!(
        payload.pointer("/layout/kernel_prefix").is_none(),
        "GPU layout should not expose stale kernel_prefix metadata: {payload:?}"
    );
    assert_gpu_entry_prefixes(
        &payload,
        "/layout",
        &["derivative_rhs_map", "derivative_rhs_stencil"],
        "derivative_rhs_chunk",
        "derivative RHS",
    );
    assert_gpu_entry_prefixes(
        &payload,
        "/layout/implicit_rhs",
        &["implicit_rhs_map", "implicit_rhs_stencil"],
        "implicit_rhs_chunk",
        "implicit RHS",
    );
    assert_eq!(
        payload.pointer("/layout/implicit_rhs/scalar_fallback_rows"),
        Some(&serde_json::Value::from(0))
    );
    assert_eq!(
        payload.pointer("/layout/chunks"),
        Some(&serde_json::Value::from(0))
    );
    assert_eq!(
        payload.pointer("/layout/implicit_rhs/chunks"),
        Some(&serde_json::Value::from(0))
    );
    assert_eq!(
        payload.pointer("/layout/implicit_rhs/workgroup_size"),
        payload.pointer("/layout/workgroup_size")
    );
    assert_eq!(
        payload.pointer("/layout/implicit_rhs/chunk_size"),
        payload.pointer("/layout/chunk_size")
    );
    assert!(
        has_native_map_kernel(derivative_kernels, "derivative_rhs_map", 0, 1),
        "GPU layout should schedule native derivative RHS kernels with output offsets: {payload:?}"
    );
    assert!(
        has_native_map_kernel(implicit_kernels, "implicit_rhs_map", 0, 1),
        "GPU layout should schedule native implicit RHS kernels with output offsets: {payload:?}"
    );
    assert_gpu_kernel_entry_kinds(
        derivative_kernels,
        "derivative_rhs",
        GpuKernelOutputKind::Native,
    );
    assert_gpu_kernel_entry_kinds(
        implicit_kernels,
        "implicit_rhs",
        GpuKernelOutputKind::Native,
    );
    assert!(
        has_native_map_family(derivative_native_families, 0, 1, 3),
        "GPU layout should expose derivative family shape and output offset metadata: {payload:?}"
    );
    assert!(
        has_native_map_family(implicit_native_families, 0, 1, 3),
        "GPU layout should expose implicit family shape and output offset metadata: {payload:?}"
    );

    clear_source_root_cache().expect("clear source-root cache");
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
#[test]
fn test_prepare_gpu_simulation_exposes_scalar_chunk_output_indices() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");

    let source = r#"
    model GpuScalarChunk
      Real x(start = 1.0, fixed = true);
    equation
      der(x) = -x;
    end GpuScalarChunk;
    "#;

    let json = prepare_gpu_simulation(source, "GpuScalarChunk")
        .expect("prepare_gpu_simulation should render scalar chunk payload");
    let payload: serde_json::Value =
        serde_json::from_str(&json).expect("GPU preparation payload should be valid JSON");
    let derivative_kernels = payload
        .pointer("/layout/kernels")
        .and_then(serde_json::Value::as_array)
        .expect("GPU layout should include derivative RHS kernel schedule");
    let implicit_kernels = payload
        .pointer("/layout/implicit_rhs/kernels")
        .and_then(serde_json::Value::as_array)
        .expect("GPU layout should include implicit RHS kernel schedule");

    assert!(
        payload.pointer("/layout/kernel_prefix").is_none(),
        "GPU layout should not expose stale kernel_prefix metadata: {payload:?}"
    );
    assert_gpu_entry_prefixes(
        &payload,
        "/layout",
        &["derivative_rhs_map", "derivative_rhs_stencil"],
        "derivative_rhs_chunk",
        "derivative RHS",
    );
    assert_gpu_entry_prefixes(
        &payload,
        "/layout/implicit_rhs",
        &["implicit_rhs_map", "implicit_rhs_stencil"],
        "implicit_rhs_chunk",
        "implicit RHS",
    );
    assert!(
        has_scalar_chunk_output_indices(derivative_kernels, "derivative_rhs_chunk", &[0]),
        "GPU layout should expose derivative scalar chunk output slots: {payload:?}"
    );
    assert!(
        has_scalar_chunk_output_indices(implicit_kernels, "implicit_rhs_chunk", &[0]),
        "GPU layout should expose implicit scalar chunk output slots: {payload:?}"
    );
    assert_gpu_kernel_entry_kinds(
        derivative_kernels,
        "derivative_rhs",
        GpuKernelOutputKind::ScalarChunk,
    );
    assert_gpu_kernel_entry_kinds(
        implicit_kernels,
        "implicit_rhs",
        GpuKernelOutputKind::ScalarChunk,
    );

    clear_source_root_cache().expect("clear source-root cache");
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
#[derive(Clone, Copy)]
enum GpuKernelOutputKind {
    Native,
    ScalarChunk,
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
fn assert_gpu_entry_prefixes(
    payload: &serde_json::Value,
    layout_pointer: &str,
    native: &[&str],
    scalar: &str,
    block_name: &str,
) {
    let prefixes = payload
        .pointer(&format!("{layout_pointer}/entry_prefixes"))
        .unwrap_or_else(|| panic!("GPU layout should expose {block_name} entry_prefixes"));
    let expected_native = native
        .iter()
        .map(|prefix| serde_json::Value::from(*prefix))
        .collect::<Vec<_>>();
    assert_eq!(
        prefixes.get("native").and_then(serde_json::Value::as_array),
        Some(&expected_native),
        "GPU layout should expose {block_name} native entry prefixes: {payload:?}"
    );
    assert_eq!(
        prefixes.get("scalar").and_then(serde_json::Value::as_str),
        Some(scalar),
        "GPU layout should expose {block_name} scalar entry prefix: {payload:?}"
    );
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
fn assert_gpu_kernel_entry_kinds(
    kernels: &[serde_json::Value],
    entry_prefix: &str,
    output_kind: GpuKernelOutputKind,
) {
    for kernel in kernels {
        let entry = kernel
            .get("entry")
            .and_then(serde_json::Value::as_str)
            .unwrap_or_else(|| panic!("GPU kernel should expose an entry: {kernel:?}"));
        match output_kind {
            GpuKernelOutputKind::Native => {
                assert!(
                    entry.starts_with(&format!("{entry_prefix}_map"))
                        || entry.starts_with(&format!("{entry_prefix}_stencil")),
                    "GPU native kernel entry should be map/stencil, got {entry}"
                );
                assert!(
                    kernel.get("output_map").is_some(),
                    "GPU native kernel {entry} should expose tensor output_map metadata"
                );
                assert!(
                    kernel.get("start_slot").is_none(),
                    "GPU native kernel {entry} should not expose scalar start_slot metadata"
                );
                assert!(
                    kernel.get("output_indices").is_none(),
                    "GPU native kernel {entry} should not expose scalar output_indices metadata"
                );
            }
            GpuKernelOutputKind::ScalarChunk => {
                assert!(
                    entry.starts_with(&format!("{entry_prefix}_chunk")),
                    "GPU scalar kernel entry should be chunk, got {entry}"
                );
                assert!(
                    kernel.get("output_map").is_none(),
                    "GPU scalar kernel {entry} should not expose tensor output_map metadata"
                );
                assert!(
                    kernel.get("start_slot").is_some(),
                    "GPU scalar kernel {entry} should expose scalar start_slot metadata"
                );
                assert!(
                    kernel.get("output_indices").is_some(),
                    "GPU scalar kernel {entry} should expose scalar output_indices metadata"
                );
            }
        }
    }
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
fn has_native_map_kernel(
    kernels: &[serde_json::Value],
    entry_prefix: &str,
    offset: u64,
    stride: i64,
) -> bool {
    kernels.iter().any(|kernel| {
        kernel
            .get("entry")
            .and_then(serde_json::Value::as_str)
            .is_some_and(|entry| entry.starts_with(entry_prefix))
            && kernel
                .pointer("/output_map/start")
                .and_then(serde_json::Value::as_u64)
                == Some(offset)
            && has_dense_output_stride(kernel, stride)
    })
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
fn has_scalar_chunk_output_indices(
    kernels: &[serde_json::Value],
    entry_prefix: &str,
    output_indices: &[u64],
) -> bool {
    kernels.iter().any(|kernel| {
        kernel
            .get("entry")
            .and_then(serde_json::Value::as_str)
            .is_some_and(|entry| entry.starts_with(entry_prefix))
            && kernel
                .get("output_indices")
                .and_then(serde_json::Value::as_array)
                .is_some_and(|slots| {
                    slots
                        .iter()
                        .map(serde_json::Value::as_u64)
                        .eq(output_indices.iter().copied().map(Some))
                })
    })
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
fn has_native_map_family(
    families: &[serde_json::Value],
    output_start: u64,
    output_stride: i64,
    first_domain_dim: u64,
) -> bool {
    families.iter().any(|family| {
        family.get("kind").and_then(serde_json::Value::as_str) == Some("map")
            && family
                .pointer("/output_map/start")
                .and_then(serde_json::Value::as_u64)
                == Some(output_start)
            && has_dense_output_stride(family, output_stride)
            && family
                .pointer("/domain_shape/0")
                .and_then(serde_json::Value::as_u64)
                == Some(first_domain_dim)
    })
}

#[cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]
fn has_dense_output_stride(value: &serde_json::Value, stride: i64) -> bool {
    value
        .pointer("/output_map/strides")
        .and_then(serde_json::Value::as_array)
        .is_some_and(|strides| {
            matches!(
                strides.as_slice(),
                [term]
                    if term.get("dimension").and_then(serde_json::Value::as_u64) == Some(0)
                        && term.get("stride").and_then(serde_json::Value::as_i64) == Some(stride)
            )
        })
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
