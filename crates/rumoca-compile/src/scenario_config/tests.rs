use super::*;

#[test]
fn discovers_colocated_config_and_resolves_effective_settings() {
    let temp = tempfile::tempdir().expect("tempdir");
    let config_path = temp.path().join("rumoca-scenario.toml");
    fs::write(
        &config_path,
        r#"
source_roots = ["libs/msl"]

[rumoca]
version = "1"
task = "simulate"

[model]
file = "Ball.mo"
name = "Examples.Ball"

[sim]
solver = "bdf"
t_end = 12.0
dt = 0.01
source_root_overrides = ["libs/custom"]
"#,
    )
    .expect("write config");

    let config = ScenarioConfig::discover(temp.path())
        .expect("discover")
        .expect("exists");
    let fallback = EffectiveSimulationConfig {
        solver: "auto".to_string(),
        t_end: 5.0,
        dt: None,
        output_dir: String::new(),
        source_root_paths: vec!["fallback".to_string()],
    };
    let effective = config.effective_for_model("Examples.Ball", &fallback);

    assert_eq!(effective.solver, "bdf");
    assert_eq!(effective.t_end, 12.0);
    assert_eq!(effective.dt, Some(0.01));
    assert!(
        effective
            .source_root_paths
            .iter()
            .any(|path| path.ends_with("libs/msl")),
        "resolved paths: {:?}",
        effective.source_root_paths
    );
    assert!(
        effective
            .source_root_paths
            .iter()
            .any(|path| path.ends_with("libs/custom")),
        "resolved paths: {:?}",
        effective.source_root_paths
    );

    let all_paths = config.resolve_all_source_root_paths();
    assert_eq!(all_paths.len(), 2);
    assert!(all_paths.iter().any(|path| path.ends_with("libs/msl")));
    assert!(all_paths.iter().any(|path| path.ends_with("libs/custom")));
}

#[test]
fn load_or_default_without_colocated_configs_is_empty() {
    let temp = tempfile::tempdir().expect("tempdir");
    let config = ScenarioConfig::load_or_default(temp.path()).expect("load");
    assert!(config.configs.is_empty());
    assert!(config.resolve_all_source_root_paths().is_empty());
}

#[test]
fn load_or_default_ignores_unrelated_toml_files() {
    let temp = tempfile::tempdir().expect("tempdir");
    fs::write(
        temp.path().join("Cargo.toml"),
        r#"
[package]
name = "not-a-rumoca-scenario"
version = "0.8.13"
"#,
    )
    .expect("write cargo toml");

    let config = ScenarioConfig::load_or_default(temp.path()).expect("load");
    assert!(config.configs.is_empty());
    assert!(
        config.diagnostics.is_empty(),
        "unexpected diagnostics: {:?}",
        config.diagnostics
    );
}

#[test]
fn load_or_default_reports_malformed_scenario_file() {
    let temp = tempfile::tempdir().expect("tempdir");
    fs::write(
        temp.path().join("rumoca-scenario.toml"),
        r#"
[rumoca]
version = "1"
task = "simulate"
[model
"#,
    )
    .expect("write scenario toml");

    let config = ScenarioConfig::load_or_default(temp.path()).expect("load");
    assert!(config.configs.is_empty());
    assert!(
        config
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.contains("Failed to parse TOML config")),
        "expected malformed scenario diagnostic, got {:?}",
        config.diagnostics
    );
}

#[test]
fn write_preset_updates_colocated_model_scenario() {
    let temp = tempfile::tempdir().expect("tempdir");
    let source_path = temp.path().join("Examples").join("Ball.mo");
    fs::create_dir_all(source_path.parent().expect("source parent")).expect("mkdir");
    fs::write(
        &source_path,
        r#"
within Examples;
model Ball
  Real x;
equation
  der(x) = 1;
end Ball;
"#,
    )
    .expect("write model");

    write_model_simulation_preset(
        temp.path(),
        "Examples.Ball",
        SimulationModelOverride {
            solver: Some("bdf".to_string()),
            t_end: Some(3.0),
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("write preset");

    let config_path = source_path.with_file_name("rumoca-scenario.ball.toml");
    let text = fs::read_to_string(&config_path).expect("read colocated config");
    assert!(text.contains("[rumoca]"));
    assert!(text.contains("version = \"1\""));
    assert!(text.contains("name = \"Examples.Ball\""));
    assert!(text.contains("[sim]"));
    assert!(text.contains("solver = \"bdf\""));
    assert!(
        temp.path()
            .read_dir()
            .expect("temp root entries")
            .all(|entry| {
                !entry
                    .expect("temp root entry")
                    .file_name()
                    .to_string_lossy()
                    .starts_with(".rumoca")
            }),
        "scenario config must not create hidden rumoca project state"
    );

    let snapshot = load_simulation_snapshot_for_model(
        temp.path(),
        "Examples.Ball",
        &EffectiveSimulationConfig::default(),
    )
    .expect("snapshot");
    assert_eq!(snapshot.effective.solver, "bdf");
    assert_eq!(snapshot.effective.t_end, 3.0);
}

#[test]
fn write_views_preserves_existing_non_rumoca_sections() {
    let temp = tempfile::tempdir().expect("tempdir");
    let config_path = temp.path().join("rumoca-scenario.interactive.toml");
    fs::write(
        &config_path,
        r#"
source_roots = ["../lib"]

[rumoca]
version = "1"
task = "simulate"

[model]
name = "Examples.Ball"
file = "Ball.mo"

[input]
mode = "auto"
"#,
    )
    .expect("write config");

    write_plot_views_for_model(
        temp.path(),
        "Examples.Ball",
        vec![PlotViewConfig {
            id: "states".to_string(),
            title: "States".to_string(),
            view_type: "timeseries".to_string(),
            x: Some("time".to_string()),
            y: vec!["x".to_string()],
            script: None,
            script_path: None,
        }],
    )
    .expect("write views");

    let text = fs::read_to_string(&config_path).expect("read config");
    assert!(text.contains("[input]"));
    assert!(text.contains("mode = \"auto\""));
    assert!(text.contains("[[plot.views]]"));
}

#[test]
fn input_enabled_scenario_defaults_to_results_panel_viewer() {
    let descriptor = scenario_config_response(
        r#"
[rumoca]
version = "1"
task = "simulate"

[model]
name = "Examples.Vehicle"
file = "Vehicle.mo"

[input]
mode = "auto"

[locals.throttle]
type = "float"
default = 0.0

[signals.stepper_inputs]
throttle = "local:throttle"
"#,
    );

    assert_eq!(
        descriptor
            .get("viewerMode")
            .and_then(serde_json::Value::as_str),
        Some("results_panel")
    );
    assert_eq!(
        descriptor
            .get("inputEnabled")
            .and_then(serde_json::Value::as_bool),
        Some(true)
    );
}

#[test]
fn write_preset_preserves_unmanaged_sim_keys() {
    let temp = tempfile::tempdir().expect("tempdir");
    let config_path = temp.path().join("rumoca-scenario.toml");
    fs::write(
        &config_path,
        r#"
[rumoca]
version = "1"
task = "simulate"

[model]
name = "Examples.Ball"
file = "Ball.mo"

[sim]
mode = "realtime"
output = "ball.html"
solver = "auto"
"#,
    )
    .expect("write config");

    write_model_simulation_preset(
        temp.path(),
        "Examples.Ball",
        SimulationModelOverride {
            solver: Some("bdf".to_string()),
            t_end: Some(4.0),
            ..Default::default()
        },
    )
    .expect("write preset");

    let text = fs::read_to_string(&config_path).expect("read config");
    assert!(text.contains("mode = \"realtime\""));
    assert!(text.contains("output = \"ball.html\""));
    assert!(text.contains("solver = \"bdf\""));
    assert!(text.contains("t_end = 4.0"));
}

#[test]
fn clear_preset_preserves_unmanaged_sim_keys() {
    let temp = tempfile::tempdir().expect("tempdir");
    let config_path = temp.path().join("rumoca-scenario.toml");
    fs::write(
        &config_path,
        r#"
[rumoca]
version = "1"
task = "simulate"

[model]
name = "Examples.Ball"
file = "Ball.mo"

[sim]
mode = "lockstep"
output = "ball.html"
solver = "bdf"
t_end = 2.0
dt = 0.01
"#,
    )
    .expect("write config");

    clear_model_simulation_preset(temp.path(), "Examples.Ball").expect("clear preset");

    let text = fs::read_to_string(&config_path).expect("read config");
    assert!(text.contains("[sim]"));
    assert!(text.contains("mode = \"lockstep\""));
    assert!(text.contains("output = \"ball.html\""));
    assert!(!text.contains("solver ="));
    assert!(!text.contains("t_end ="));
    assert!(!text.contains("dt ="));
}

#[test]
fn from_files_reads_config_without_filesystem() {
    // The browser editor path: parse an in-memory file set (no disk access).
    let files = vec![
        (
            PathBuf::from("rumoca-scenario.Ball.toml"),
            r#"
[rumoca]
version = "1"
task = "simulate"

[model]
name = "Examples.Ball"

[sim]
solver = "bdf"
t_end = 7.0
"#
            .to_string(),
        ),
        (PathBuf::from("Ball.mo"), "model Ball end Ball;".to_string()),
    ];

    let config = ScenarioConfig::from_files(Path::new(""), files);
    let effective =
        config.effective_for_model("Examples.Ball", &EffectiveSimulationConfig::default());
    assert_eq!(effective.solver, "bdf");
    assert_eq!(effective.t_end, 7.0);
}

#[test]
fn from_files_colocates_new_config_next_to_model_source() {
    // A new preset for a model whose .mo lives in a subdirectory should write a
    // colocated `rum.<stem>.toml` next to it, resolved from the in-memory map.
    let files = vec![(
        PathBuf::from("models/Ball.mo"),
        "model Ball end Ball;".to_string(),
    )];
    let mut config = ScenarioConfig::from_files(Path::new(""), files);
    config.set_model_simulation_preset(
        "Ball",
        SimulationModelOverride {
            solver: Some("bdf".to_string()),
            ..SimulationModelOverride::default()
        },
    );
    let (path, text) = config
        .compute_write_for_model("Ball")
        .expect("write exists")
        .expect("render ok");
    assert_eq!(path, PathBuf::from("models/rumoca-scenario.ball.toml"));
    assert!(text.contains("solver = \"bdf\""));
    assert!(text.contains("[rumoca]"));
}
