use crate::{
    scenario_default_scenario_config, scenario_get_codegen_config,
    scenario_get_scenario_config_full, scenario_get_simulation_config, scenario_get_source_roots,
    scenario_set_codegen_config, scenario_set_scenario_config, scenario_set_simulation_preset,
    scenario_set_source_roots,
};

#[test]
fn scenario_get_simulation_config_reads_in_memory_scenario_toml() {
    let sources = serde_json::json!({
        "rumoca-scenario.Ball.toml": "[rumoca]\nversion = \"1\"\ntask = \"simulate\"\n\n[model]\nname = \"Ball\"\n\n[sim]\nsolver = \"bdf\"\nt_end = 9.0\n",
        "Ball.mo": "model Ball end Ball;",
    })
    .to_string();

    let response =
        scenario_get_simulation_config(&sources, "Ball", "").expect("simulation config response");
    let parsed: serde_json::Value = serde_json::from_str(&response).expect("valid JSON");
    assert_eq!(parsed["effective"]["solver"], "bdf");
    assert_eq!(parsed["effective"]["tEnd"], 9.0);

    let preset = serde_json::json!({ "solver": "rk-like", "tEnd": 3.0 }).to_string();
    let write_response =
        scenario_set_simulation_preset(&sources, "Ball", &preset).expect("set preset response");
    let written: serde_json::Value = serde_json::from_str(&write_response).expect("valid JSON");
    let writes = written["writes"].as_array().expect("writes array");
    assert_eq!(writes.len(), 1);
    assert_eq!(writes[0]["path"], "rumoca-scenario.Ball.toml");
    assert!(
        writes[0]["content"]
            .as_str()
            .is_some_and(|content| content.contains("solver = \"rk-like\""))
    );
}

#[test]
fn scenario_codegen_config_round_trips_in_model_scenario() {
    let sources = serde_json::json!({
        "src/rumoca-scenario.Ball.toml": "source_roots = [\"lib\"]\n\n[rumoca]\nversion = \"1\"\ntask = \"codegen\"\n\n[model]\nname = \"Ball\"\n\n[codegen]\ntarget = \"sympy\"\n",
        "src/Ball.mo": "model Ball end Ball;",
    })
    .to_string();

    let response = scenario_get_codegen_config(&sources, "Ball").expect("codegen config");
    let parsed: serde_json::Value = serde_json::from_str(&response).expect("valid JSON");
    assert_eq!(parsed["target"], "sympy");

    let config =
        serde_json::json!({ "target": "targets/custom", "outputDir": "generated" }).to_string();
    let write_response = scenario_set_codegen_config(&sources, "Ball", &config)
        .expect("set codegen config response");
    let written: serde_json::Value = serde_json::from_str(&write_response).expect("valid JSON");
    let content = written["writes"][0]["content"].as_str().expect("content");
    assert!(content.contains("[codegen]"));
    assert!(content.contains("target = \"targets/custom\""));
    assert!(content.contains("output_dir = \"generated\""));

    let roots_response =
        scenario_get_source_roots(&sources, "Ball", "codegen").expect("source roots");
    let roots: serde_json::Value = serde_json::from_str(&roots_response).expect("valid JSON");
    assert_eq!(roots["sourceRootPaths"][0], "lib");

    let roots_config =
        serde_json::json!({ "task": "codegen", "sourceRootPaths": ["../vendor", ""] }).to_string();
    let roots_write = scenario_set_source_roots(&sources, "Ball", &roots_config)
        .expect("set source roots response");
    let written_roots: serde_json::Value = serde_json::from_str(&roots_write).expect("valid JSON");
    let roots_content = written_roots["writes"][0]["content"]
        .as_str()
        .expect("content");
    assert!(roots_content.contains("source_roots = [\"../vendor\"]"));

    let clear_config = serde_json::json!({ "task": "codegen", "sourceRootPaths": [] }).to_string();
    let clear_write = scenario_set_source_roots(&sources, "Ball", &clear_config)
        .expect("clear source roots response");
    let cleared: serde_json::Value = serde_json::from_str(&clear_write).expect("valid JSON");
    let cleared_content = cleared["writes"][0]["content"].as_str().expect("content");
    assert!(!cleared_content.contains("source_roots"));
    assert!(cleared_content.contains("target = \"sympy\""));
}

#[test]
fn scenario_task_configs_do_not_collide_for_same_model() {
    let sources = serde_json::json!({
        "src/rumoca-scenario.Ball.toml": "[rumoca]\nversion = \"1\"\ntask = \"simulate\"\n\n[model]\nname = \"Ball\"\n\n[sim]\nsolver = \"bdf\"\nt_end = 7.0\n",
        "src/rumoca-scenario.Ball.codegen.toml": "[rumoca]\nversion = \"1\"\ntask = \"codegen\"\n\n[model]\nname = \"Ball\"\n\n[codegen]\ntarget = \"sympy\"\n",
        "src/Ball.mo": "model Ball end Ball;",
    })
    .to_string();

    let simulation =
        scenario_get_simulation_config(&sources, "Ball", "").expect("simulation config");
    let simulation: serde_json::Value = serde_json::from_str(&simulation).expect("valid JSON");
    assert_eq!(simulation["effective"]["solver"], "bdf");
    assert_eq!(simulation["effective"]["tEnd"], 7.0);

    let codegen = scenario_get_codegen_config(&sources, "Ball").expect("codegen config");
    let codegen: serde_json::Value = serde_json::from_str(&codegen).expect("valid JSON");
    assert_eq!(codegen["target"], "sympy");
}

#[test]
fn scenario_config_full_round_trips_interactive_io() {
    let scenario_toml = r#"[rumoca]
version = "1"
task = "simulate"

[model]
name = "Rover"
file = "Rover.mo"

[sim]
solver = "auto"
mode = "realtime"

[parameters]
gain = 2.5

[input.keyboard.keys.ArrowUp]
action = "set"
target = "throttle"
value = 1.0

[signals.viewer]
theta = "stepper:theta"
"#;
    let sources = serde_json::json!({ "rumoca-scenario.toml": scenario_toml }).to_string();

    let full =
        scenario_get_scenario_config_full(&sources, "rumoca-scenario.toml").expect("full config");
    let parsed: serde_json::Value = serde_json::from_str(&full).expect("json");
    assert_eq!(parsed["ok"], true);
    let mut config = parsed["config"].clone();
    assert_eq!(config["parameters"]["gain"], 2.5);
    assert_eq!(
        config["input"]["keyboard"]["keys"]["ArrowUp"]["action"],
        "set"
    );
    assert_eq!(config["signals"]["viewer"]["theta"], "stepper:theta");

    config["sim"]["solver"] = serde_json::Value::from("bdf");
    config["parameters"]["gain"] = serde_json::Value::from(3.0);
    let write_response = scenario_set_scenario_config("rumoca-scenario.toml", &config.to_string())
        .expect("set scenario");
    let written: serde_json::Value = serde_json::from_str(&write_response).expect("json");
    let content = written["writes"][0]["content"].as_str().expect("content");
    assert!(content.contains("solver = \"bdf\""));
    assert!(content.contains("[parameters]"));
    assert!(content.contains("gain = 3.0"));
    assert!(content.contains("[input.keyboard.keys.ArrowUp]"));
    assert!(content.contains("theta = \"stepper:theta\""));
    assert!(content.contains("[rumoca]"));
}

#[test]
fn default_scenario_config_creates_minimal_scenario_toml() {
    let sources = serde_json::json!({ "models/Rover.mo": "model Rover end Rover;" }).to_string();
    let response = scenario_default_scenario_config(&sources, "Rover", "").expect("default config");
    let parsed: serde_json::Value = serde_json::from_str(&response).expect("json");
    assert_eq!(parsed["ok"], true);
    assert_eq!(parsed["path"], "models/rumoca-scenario.rover.toml");
    let content = parsed["content"].as_str().expect("content");
    assert!(content.contains("[rumoca]"));
    assert!(content.contains("task = \"simulate\""));
    assert!(content.contains("file = \"Rover.mo\""));
    assert!(content.contains("name = \"Rover\""));

    let codegen =
        scenario_default_scenario_config(&sources, "Rover", "codegen").expect("codegen config");
    let parsed: serde_json::Value = serde_json::from_str(&codegen).expect("json");
    assert_eq!(parsed["path"], "models/rumoca-scenario.rover.codegen.toml");
    let content = parsed["content"].as_str().expect("content");
    assert!(content.contains("task = \"codegen\""));
    assert!(content.contains("file = \"Rover.mo\""));
}
