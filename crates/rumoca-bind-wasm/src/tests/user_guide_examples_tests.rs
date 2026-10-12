//! The user guide's GPU live examples run through the same wasm entry points a
//! reader's browser calls: `prepare_gpu_simulation` for the default GPU path,
//! the live-input rewrite of the Interactive toggle, and `simulate_model` for
//! the CPU path offered when WebGPU is unavailable.
#![cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]

use super::user_guide_samples::{finite_sample_grid, finite_vector};
use super::*;
use std::path::PathBuf;

/// One fenced ```` ```modelica,…,gpu ```` block of the user guide.
struct GuideGpuExample {
    page: PathBuf,
    model: String,
    source: String,
    interactive: bool,
}

fn guide_gpu_examples() -> Vec<GuideGpuExample> {
    super::user_guide_sources::guide_fences()
        .into_iter()
        .filter(|fence| fence.has_flag("gpu"))
        .map(|fence| GuideGpuExample {
            model: fence.model_name().unwrap_or_else(|| {
                panic!("GPU example in {} names no model", fence.page.display())
            }),
            interactive: fence.has_flag("interactive"),
            page: fence.page,
            source: fence.source,
        })
        .collect()
}

/// The Interactive toggle of the live runner flips the structural
/// `interactive` flag before preparing the GPU session.
fn live_input_source(source: &str) -> Option<String> {
    let needle = "parameter Boolean interactive = false";
    source
        .contains(needle)
        .then(|| source.replacen(needle, "parameter Boolean interactive = true", 1))
}

fn declared_inputs(source: &str) -> Vec<String> {
    source
        .lines()
        .filter_map(|line| line.trim_start().strip_prefix("input Real "))
        .map(|rest| {
            rest.chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                .collect()
        })
        .collect()
}

fn prepare_guide_gpu_payload(example: &GuideGpuExample, source: &str) -> serde_json::Value {
    let json = prepare_gpu_simulation(source, &example.model).unwrap_or_else(|error| {
        panic!(
            "{} ({}) must prepare for the guide's GPU path: {error}",
            example.model,
            example.page.display()
        )
    });
    let payload: serde_json::Value =
        serde_json::from_str(&json).expect("GPU preparation payload should be valid JSON");
    assert!(
        payload["n_states"].as_u64().is_some_and(|count| count > 0),
        "{} must expose integrated states to the GPU driver",
        example.model
    );
    for (vector, extent) in [("y0", "y_len"), ("p0", "p_len")] {
        let len = payload["layout"][extent]
            .as_u64()
            .and_then(|len| usize::try_from(len).ok())
            .expect("the checked shader layout issues a physical vector extent");
        finite_vector(&payload[vector], len)
            .unwrap_or_else(|error| panic!("{} {vector}: {error}", example.model));
    }
    let state_count = payload["n_states"].as_u64().unwrap();
    assert_eq!(
        payload["state_names"].as_array().unwrap().len() as u64,
        state_count
    );
    assert!(state_count <= payload["layout"]["y_len"].as_u64().unwrap());
    for setting in ["t_start", "t_end", "dt", "internal_dt"] {
        assert!(
            payload[setting].as_f64().is_some_and(f64::is_finite),
            "{} finite {setting}",
            example.model
        );
    }
    assert!(payload["t_end"].as_f64().unwrap() > payload["t_start"].as_f64().unwrap());
    assert!(payload["dt"].as_f64().unwrap() > 0.0);
    assert!(payload["internal_dt"].as_f64().unwrap() > 0.0);
    payload
}

#[test]
fn user_guide_gpu_examples_prepare_for_webgpu() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");

    let examples = guide_gpu_examples();
    assert!(
        examples
            .iter()
            .any(|example| example.model == "AirfoilFlow"),
        "the NACA airfoil example must stay covered"
    );
    for example in &examples {
        prepare_guide_gpu_payload(example, &example.source);
        if !example.interactive {
            continue;
        }
        let Some(live_source) = live_input_source(&example.source) else {
            continue;
        };
        let payload = prepare_guide_gpu_payload(example, &live_source);
        // The live stepper writes each command input into its parameter slot
        // before every interval.
        for input in declared_inputs(&live_source) {
            assert!(
                payload
                    .pointer(&format!("/var_layout/bindings/{input}/P/index"))
                    .and_then(serde_json::Value::as_u64)
                    .is_some(),
                "{}: live input `{input}` needs a host-writable parameter slot: {}",
                example.model,
                payload["var_layout"]["bindings"]
            );
        }
    }

    clear_source_root_cache().expect("clear source-root cache");
}

#[test]
fn user_guide_gpu_examples_simulate_on_the_cpu_path() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");

    let mut airfoil_live_covered = false;
    for example in guide_gpu_examples() {
        simulate_guide_cpu(&example, &example.source);
        let live_source = if example.interactive {
            live_input_source(&example.source)
        } else {
            None
        };
        if example.model == "AirfoilFlow" {
            assert!(
                live_source.is_some(),
                "AirfoilFlow must retain its Interactive source variant"
            );
        }
        if let Some(live_source) = live_source {
            let simulation = simulate_guide_cpu(&example, &live_source);
            if example.model == "AirfoilFlow" {
                assert_airfoil_default_inputs(&simulation);
                airfoil_live_covered = true;
            }
        }
    }
    assert!(airfoil_live_covered, "the live Airfoil CPU path must run");

    clear_source_root_cache().expect("clear source-root cache");
}

fn simulate_guide_cpu(example: &GuideGpuExample, source: &str) -> serde_json::Value {
    let json =
        simulate_model(source, &example.model, 0.02, 0.01, "auto", "{}").unwrap_or_else(|error| {
            panic!(
                "{} ({}) must simulate on the CPU path: {error}",
                example.model,
                example.page.display()
            )
        });
    let simulation: serde_json::Value =
        serde_json::from_str(&json).expect("simulation payload should be valid JSON");
    finite_sample_grid(&simulation, &[0.0, 0.01, 0.02])
        .unwrap_or_else(|error| panic!("{} CPU samples: {error}", example.model));
    simulation
}

fn assert_airfoil_default_inputs(simulation: &serde_json::Value) {
    let names = simulation["payload"]["names"]
        .as_array()
        .expect("named CPU sample channels");
    let columns = simulation["payload"]["allData"].as_array().unwrap();
    assert_eq!(columns.len(), names.len() + 1, "time plus named channels");
    // With no host input frame, the authored bindings command each actuator's
    // initial value. Its first-order motor must therefore stay at that value.
    for (input, motor, default) in [
        ("aoa_cmd", "aoa_motor", 8.0),
        ("mc", "mc_motor", 0.02),
        ("pc", "pc_motor", 0.4),
        ("tk", "tk_motor", 0.12),
    ] {
        for name in [input, motor] {
            let index = names
                .iter()
                .position(|value| value.as_str() == Some(name))
                .unwrap_or_else(|| panic!("AirfoilFlow must sample {name}"));
            for sample in columns[index + 1].as_array().unwrap() {
                let actual = sample.as_f64().unwrap();
                assert!(
                    (actual - default).abs() < 1e-9,
                    "AirfoilFlow {name}: {actual} != authored default {default}"
                );
            }
        }
    }
}

/// The airfoil's AoA slider re-runs the GPU path through
/// `update_gpu_parameters`; the frame coordinates are states seeded by initial
/// equations, so the new angle must re-seed them.
#[test]
fn user_guide_airfoil_aoa_slider_reseeds_the_airfoil_frame() {
    let _guard = session_test_guard();
    clear_source_root_cache().expect("clear source-root cache");

    let example = guide_gpu_examples()
        .into_iter()
        .find(|example| example.model == "AirfoilFlow")
        .expect("the NACA airfoil example is in the guide");
    let prep = prepare_guide_gpu_payload(&example, &example.source);
    let frame_slot = prep["state_names"]
        .as_array()
        .and_then(|names| names.iter().position(|name| name == "nc[10,9]"))
        .expect("the airfoil frame coordinates are integrated states");
    let updated: serde_json::Value = serde_json::from_str(
        &update_gpu_parameters(&example.source, &example.model, r#"{"aoa": 20.0}"#)
            .expect("the AoA slider re-settles the prepared vectors"),
    )
    .expect("parameter update payload should be valid JSON");

    // Cell (10, 9) sits on the chord line ahead of mid-chord; rotating the
    // frame by `aoa` gives its chord-normal coordinate x * sin(aoa) +
    // y * cos(aoa) with x, y its offset from the leading edge.
    let (x, y) = (9.5 * 4.0 / 30.0 - 1.0, 8.5 * 1.5 / 18.0 - 0.75);
    let expected = |aoa: f64| x * aoa.to_radians().sin() + y * aoa.to_radians().cos();
    let initial = prep["y0"][frame_slot].as_f64().expect("seeded frame state");
    let reseeded = updated["y0"][frame_slot]
        .as_f64()
        .expect("re-seeded frame state");
    assert!((initial - expected(8.0)).abs() < 1e-9, "{initial}");
    assert!((reseeded - expected(20.0)).abs() < 1e-9, "{reseeded}");

    clear_source_root_cache().expect("clear source-root cache");
}
