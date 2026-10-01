//! The user guide's GPU live examples run through the same wasm entry points a
//! reader's browser calls: `prepare_gpu_simulation` for the default GPU path,
//! the live-input rewrite of the Interactive toggle, and `simulate_model` for
//! the CPU path offered when WebGPU is unavailable.
#![cfg(any(feature = "sim-wasm", feature = "sim-diffsol", feature = "sim-rk45"))]

use super::*;
use std::path::{Path, PathBuf};

/// One fenced ```` ```modelica,…,gpu ```` block of the user guide.
struct GuideGpuExample {
    page: PathBuf,
    model: String,
    source: String,
    interactive: bool,
}

fn user_guide_src() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../docs/user-guide/src")
}

fn markdown_pages(dir: &Path, pages: &mut Vec<PathBuf>) {
    let mut entries: Vec<PathBuf> = std::fs::read_dir(dir)
        .unwrap_or_else(|error| panic!("read {}: {error}", dir.display()))
        .map(|entry| entry.expect("guide directory entry").path())
        .collect();
    entries.sort();
    for path in entries {
        if path.is_dir() {
            markdown_pages(&path, pages);
        } else if path.extension().is_some_and(|ext| ext == "md") {
            pages.push(path);
        }
    }
}

fn top_level_model_name(source: &str) -> Option<String> {
    source.lines().rev().find_map(|line| {
        let rest = line.strip_prefix("model ")?;
        let name: String = rest
            .chars()
            .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
            .collect();
        (!name.is_empty()).then_some(name)
    })
}

fn guide_gpu_examples() -> Vec<GuideGpuExample> {
    let mut pages = Vec::new();
    markdown_pages(&user_guide_src(), &mut pages);
    let mut examples = Vec::new();
    for page in pages {
        let text = std::fs::read_to_string(&page)
            .unwrap_or_else(|error| panic!("read {}: {error}", page.display()));
        let mut lines = text.lines();
        while let Some(line) = lines.next() {
            let Some(info) = line.strip_prefix("```modelica") else {
                continue;
            };
            let flags: Vec<&str> = info.split(',').map(str::trim).collect();
            let body: Vec<&str> = lines
                .by_ref()
                .take_while(|l| !l.starts_with("```"))
                .collect();
            if !flags.contains(&"gpu") {
                continue;
            }
            let source = body.join("\n") + "\n";
            let model = top_level_model_name(&source)
                .unwrap_or_else(|| panic!("GPU example in {} names no model", page.display()));
            examples.push(GuideGpuExample {
                page: page.clone(),
                model,
                source,
                interactive: flags.contains(&"interactive"),
            });
        }
    }
    examples
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

    for example in guide_gpu_examples() {
        let json = simulate_model(&example.source, &example.model, 0.02, 0.01, "auto", "{}")
            .unwrap_or_else(|error| {
                panic!(
                    "{} ({}) must simulate on the CPU path: {error}",
                    example.model,
                    example.page.display()
                )
            });
        let simulation: serde_json::Value =
            serde_json::from_str(&json).expect("simulation payload should be valid JSON");
        let times = simulation
            .pointer("/payload/allData/0")
            .and_then(serde_json::Value::as_array)
            .expect("simulation payload should include sampled times");
        assert!(
            times.len() >= 2,
            "{} must produce samples on the CPU path",
            example.model
        );
    }

    clear_source_root_cache().expect("clear source-root cache");
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
