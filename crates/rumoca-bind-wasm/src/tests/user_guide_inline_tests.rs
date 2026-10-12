//! Standalone inline guide models use the reader's actual binding front door.
#![cfg(feature = "sim-rk45")]

use super::user_guide_samples::finite_vector;
use super::user_guide_sources::{GuideFence, guide_fences};
use super::*;

fn inline_examples() -> Vec<GuideFence> {
    guide_fences()
        .into_iter()
        .filter(|fence| fence.model_name().is_some() && !fence.has_flag("fragment"))
        .collect()
}

#[test]
fn user_guide_scenarios_keep_their_actual_model_and_settings() {
    let root = super::user_guide_sources::repository_root();
    for fence in guide_fences() {
        let Some(path) = fence.scenario_path() else {
            continue;
        };
        let text = std::fs::read_to_string(root.join(&path)).unwrap();
        let sources = serde_json::json!({path.to_str().unwrap(): text}).to_string();
        let response = scenario_get_scenario_config_full(&sources, path.to_str().unwrap()).unwrap();
        let response: serde_json::Value = serde_json::from_str(&response).unwrap();
        assert_eq!(
            response["ok"],
            true,
            "{}:{}: {response}",
            fence.page.display(),
            fence.line
        );
        let config = &response["config"];
        assert!(
            config["model"]["name"]
                .as_str()
                .is_some_and(|name| !name.is_empty())
        );
        let file = config["model"]["file"]
            .as_str()
            .expect("authored model file");
        assert!(
            root.join(path.parent().unwrap()).join(file).is_file(),
            "{}: {file}",
            path.display()
        );
        assert!(matches!(
            config["rumoca"]["task"].as_str(),
            Some("simulate" | "codegen")
        ));
    }
}

#[test]
fn user_guide_inline_models_compile_as_authored() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let examples = inline_examples();
    assert_eq!(
        examples.len(),
        16,
        "new inline models need runtime expectations"
    );
    let mut failures = Vec::new();
    for example in examples {
        let model = example.model_name().unwrap();
        let started = std::time::Instant::now();
        match compile(&example.source, &model) {
            Ok(json) => {
                let result: serde_json::Value = serde_json::from_str(&json).unwrap();
                if result["balance"]["is_balanced"] != true {
                    failures.push(format!("{model}: unbalanced checked compilation"));
                }
                eprintln!(
                    "[guide-compile] {model} {}:{} {:.6}s",
                    example.page.display(),
                    example.line,
                    started.elapsed().as_secs_f64()
                );
            }
            Err(error) => failures.push(format!(
                "{model} {}:{}: {error}",
                example.page.display(),
                example.line
            )),
        }
    }
    clear_source_root_cache().unwrap();
    assert!(
        failures.is_empty(),
        "authored guide models:\n{}",
        failures.join("\n")
    );
}

#[test]
fn user_guide_inline_cpu_samples_are_finite() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let mut failures = Vec::new();
    for example in inline_examples().into_iter().filter(|e| !e.has_flag("gpu")) {
        let model = example.model_name().unwrap();
        if model == "Plant" {
            continue;
        } // Supplied input is exercised below.
        let started = std::time::Instant::now();
        match simulate_model(&example.source, &model, 0.02, 0.01, "rk-like", "{}") {
            Ok(json) => {
                let result: serde_json::Value = serde_json::from_str(&json).unwrap();
                let columns = result
                    .pointer("/payload/allData")
                    .and_then(serde_json::Value::as_array)
                    .expect("sample columns");
                assert!(!columns.is_empty(), "{model}");
                for column in columns {
                    let values = column.as_array().expect("sample array");
                    assert!(
                        values.len() >= 3,
                        "{model} must reach the requested endpoint"
                    );
                    finite_vector(column, values.len())
                        .unwrap_or_else(|error| panic!("{model} CPU samples: {error}"));
                }
                assert_eq!(
                    columns[0].as_array().unwrap().last().unwrap(),
                    0.02,
                    "{model}"
                );
                eprintln!(
                    "[guide-runtime] {model} {:.6}s",
                    started.elapsed().as_secs_f64()
                );
            }
            Err(error) => failures.push(format!(
                "{model} {}:{}: {error}",
                example.page.display(),
                example.line
            )),
        }
    }
    clear_source_root_cache().unwrap();
    assert!(
        failures.is_empty(),
        "guide CPU path:\n{}",
        failures.join("\n")
    );
}

#[test]
fn user_guide_coffee_full_horizon_and_supplied_plant_match_independent_curves() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    for (model, initial_inputs, end, dt, variable) in [
        ("Coffee", "[]", 1800.0, 10.0, "T"),
        ("Plant", r#"[["u",0]]"#, 1.0, 0.01, "y"),
    ] {
        let example = inline_examples()
            .into_iter()
            .find(|e| e.model_name().as_deref() == Some(model))
            .unwrap();
        let mut session = WasmSimulationSession::with_interactive_options(
            &example.source,
            model,
            dt,
            "rk-like",
            1e-10,
            1e-10,
            initial_inputs,
        )
        .unwrap();
        for step in 0..=(end / dt) as u32 {
            let time = f64::from(step) * dt;
            session.advance_to(time).unwrap();
            if model == "Plant" && step == 25 {
                session.set_input("u", 0.25).unwrap();
            }
            let actual = session.get(variable).unwrap().unwrap();
            let expected = if model == "Coffee" {
                20.0 + 70.0 * (-time / 300.0).exp()
            } else {
                0.5 * (1.0 - (-2.0 * (time - 0.25).max(0.0)).exp())
            };
            assert!(
                (actual - expected).abs() < 1e-5,
                "{model}@{time}: {actual} != {expected}"
            );
        }
    }
    clear_source_root_cache().unwrap();
}

#[test]
fn user_guide_rover_host_step_matches_the_authored_bicycle_model() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let root = super::user_guide_sources::repository_root();
    let source = std::fs::read_to_string(root.join("examples/interactive/rover/Rover.mo")).unwrap();
    let mut session = WasmSimulationSession::with_interactive_options(
        &source,
        "Rover",
        0.01,
        "rk-like",
        1e-10,
        1e-10,
        r#"[["throttle",0],["steering",0]]"#,
    )
    .unwrap();
    session.advance_to(0.1).unwrap();
    assert_eq!(session.get("x").unwrap(), Some(0.0));
    assert_eq!(session.get("v").unwrap(), Some(0.0));
    session.set_input("throttle", 0.5).unwrap();
    for step in 1..=10 {
        let elapsed = f64::from(step) * 0.05;
        session.advance_to(0.1 + elapsed).unwrap();
        let v = 2.0 * (1.0 - (-elapsed / 0.25).exp());
        let x = 2.0 * (elapsed - 0.25 * (1.0 - (-elapsed / 0.25).exp()));
        assert!((session.get("v").unwrap().unwrap() - v).abs() < 1e-8);
        assert!((session.get("x").unwrap().unwrap() - x).abs() < 1e-8);
        for variable in ["y", "theta", "delta", "yaw_rate"] {
            assert_eq!(session.get(variable).unwrap(), Some(0.0), "{variable}");
        }
    }
    clear_source_root_cache().unwrap();
}

fn thermostat_solution(mut time: f64) -> (f64, bool, usize) {
    let mut temperature: f64 = 15.0;
    let mut on = true;
    let mut switches = 0;
    loop {
        let (equilibrium, threshold): (f64, f64) = if on { (35.0, 22.0) } else { (5.0, 20.0) };
        let duration = 600.0 * ((temperature - equilibrium) / (threshold - equilibrium)).ln();
        if time < duration {
            return (
                equilibrium + (temperature - equilibrium) * (-time / 600.0).exp(),
                on,
                switches,
            );
        }
        time -= duration;
        temperature = threshold;
        on = !on;
        switches += 1;
    }
}

#[test]
fn user_guide_thermostat_heats_and_cools_through_independent_hysteresis_cycles() {
    let _guard = session_test_guard();
    clear_source_root_cache().unwrap();
    let example = inline_examples()
        .into_iter()
        .find(|example| example.model_name().as_deref() == Some("Thermostat"))
        .unwrap();
    assert!(example.source.contains("parameter Real heat = 0.05"));
    let mut session = WasmSimulationSession::with_interactive_options(
        &example.source,
        "Thermostat",
        30.0,
        "rk-like",
        1e-10,
        1e-10,
        "[]",
    )
    .unwrap();
    let mut maximum_switches = 0;
    for step in 0..=240 {
        let time = f64::from(step) * 30.0;
        session.advance_to(time).unwrap();
        let actual = session.get("T").unwrap().unwrap();
        let (expected, on, switches) = thermostat_solution(time);
        maximum_switches = maximum_switches.max(switches);
        assert!(
            (actual - expected).abs() < 1e-5,
            "T({time})={actual}, expected {expected}"
        );
        assert_eq!(
            session.get("on").unwrap(),
            Some(f64::from(on)),
            "on({time})"
        );
    }
    assert!(
        maximum_switches > 20,
        "the authored horizon covers repeated cycles"
    );
    clear_source_root_cache().unwrap();
}
