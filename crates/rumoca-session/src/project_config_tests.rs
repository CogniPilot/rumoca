use super::*;

#[test]
fn discovers_project_and_resolves_model_override() {
    let temp = tempfile::tempdir().expect("tempdir");
    let rumoca_dir = temp.path().join(".rumoca");
    let by_id_dir = rumoca_dir
        .join(MODEL_CONFIG_DIR)
        .join(MODEL_BY_ID_DIR)
        .join("00000000-0000-0000-0000-000000000001");
    fs::create_dir_all(&by_id_dir).expect("mkdir");
    fs::write(
        rumoca_dir.join("project.toml"),
        r#"
version = 1

[libraries]
paths = ["libs/msl"]

[simulation.defaults]
solver = "auto"
t_end = 10.0
"#,
    )
    .expect("write");
    fs::write(
        by_id_dir.join(MODEL_IDENTITY_FILE),
        r#"
version = 1
uuid = "00000000-0000-0000-0000-000000000001"
qualified_name = "Ball"
class_name = "Ball"
"#,
    )
    .expect("write identity");
    fs::write(
        by_id_dir.join(MODEL_SIMULATION_FILE),
        r#"
solver = "bdf"
t_end = 12.0
dt = 0.01
library_overrides = ["libs/custom"]
"#,
    )
    .expect("write simulation");

    let config = ProjectConfig::discover(temp.path())
        .expect("discover")
        .expect("exists");

    let fallback = EffectiveSimulationConfig {
        solver: "auto".to_string(),
        t_end: 5.0,
        dt: None,
        output_dir: String::new(),
        library_paths: vec!["fallback".to_string()],
    };
    let effective = config.effective_for_model("Ball", &fallback);
    assert_eq!(effective.solver, "bdf");
    assert_eq!(effective.t_end, 12.0);
    assert_eq!(effective.dt, Some(0.01));
    assert_eq!(effective.library_paths.len(), 1);
    assert!(
        effective.library_paths[0].ends_with("libs/custom"),
        "resolved path: {}",
        effective.library_paths[0]
    );

    let all_paths = config.resolve_all_library_paths();
    assert_eq!(all_paths.len(), 2);
    assert!(all_paths.iter().any(|path| path.ends_with("libs/msl")));
    assert!(all_paths.iter().any(|path| path.ends_with("libs/custom")));
}

#[test]
fn load_or_default_without_file() {
    let temp = tempfile::tempdir().expect("tempdir");
    let config = ProjectConfig::load_or_default(temp.path()).expect("load");
    assert_eq!(config.data.version, 1);
    assert!(config.data.simulation.models.is_empty());
}

#[test]
fn validates_unknown_top_level_keys() {
    let value: toml::Value = toml::from_str("badkey = 1").expect("parse");
    let diagnostics = validate_top_level_keys(&value);
    assert_eq!(diagnostics.len(), 1);
    assert!(diagnostics[0].contains("badkey"));
}

#[test]
fn save_writes_uuid_sidecars_and_keeps_root_compact() {
    let temp = tempfile::tempdir().expect("tempdir");
    let workspace_root = temp.path();

    let mut config = ProjectConfig::load_or_default(workspace_root).expect("load_or_default");
    config.data.libraries.paths.push("libs/msl".to_string());
    config.set_model_override(
        "Ball",
        SimulationModelOverride {
            solver: Some("bdf".to_string()),
            t_end: Some(12.0),
            dt: Some(0.02),
            output_dir: None,
            library_overrides: vec!["libs/custom".to_string()],
        },
    );
    config.data.plot.models.insert(
        "Ball".to_string(),
        PlotModelConfig {
            views: vec![PlotViewConfig {
                id: "states_time".to_string(),
                title: "States vs Time".to_string(),
                view_type: "timeseries".to_string(),
                x: Some("time".to_string()),
                y: vec!["*states".to_string()],
                script: None,
                script_path: None,
            }],
        },
    );
    config.save().expect("save");

    let root_text = fs::read_to_string(workspace_root.join(".rumoca").join("project.toml"))
        .expect("read root project.toml");
    assert!(!root_text.contains("[simulation.models"));
    assert!(!root_text.contains("[plot.models"));
    assert!(root_text.contains("[libraries]"));

    let by_id_root = workspace_root
        .join(".rumoca")
        .join(MODEL_CONFIG_DIR)
        .join(MODEL_BY_ID_DIR);
    assert!(by_id_root.is_dir());
    let mut ids = collect_uuid_dirs(&by_id_root).expect("collect_uuid_dirs");
    assert_eq!(ids.len(), 1);
    let model_id = ids.remove(0);
    let model_dir = by_id_root.join(&model_id);
    assert!(model_dir.join(MODEL_SIMULATION_FILE).is_file());
    assert!(model_dir.join(MODEL_VIEWS_FILE).is_file());
    assert!(model_dir.join(MODEL_IDENTITY_FILE).is_file());

    let identity_text =
        fs::read_to_string(model_dir.join(MODEL_IDENTITY_FILE)).expect("read identity");
    let identity: ModelIdentityRecord = toml::from_str(&identity_text).expect("parse identity");
    assert_eq!(identity.uuid, model_id);
    assert_eq!(identity.qualified_name, "Ball");
    assert_eq!(identity.class_name, "Ball");

    let reloaded = ProjectConfig::discover(workspace_root)
        .expect("discover")
        .expect("project exists");
    let override_cfg = reloaded
        .data
        .simulation
        .models
        .get("Ball")
        .expect("Ball simulation override");
    assert_eq!(override_cfg.solver.as_deref(), Some("bdf"));
    assert_eq!(override_cfg.t_end, Some(12.0));
    assert_eq!(override_cfg.dt, Some(0.02));
    assert_eq!(
        override_cfg.library_overrides,
        vec!["libs/custom".to_string()]
    );
    let views = reloaded
        .data
        .plot
        .models
        .get("Ball")
        .expect("Ball plot views")
        .views
        .clone();
    assert_eq!(views.len(), 1);
    assert_eq!(views[0].id, "states_time");
    assert_eq!(views[0].view_type, "timeseries");
}

#[test]
fn legacy_model_dirs_are_ignored() {
    let temp = tempfile::tempdir().expect("tempdir");
    let rumoca_dir = temp.path().join(".rumoca");
    let legacy_model_dir = rumoca_dir
        .join(MODEL_CONFIG_DIR)
        .join("Vehicle")
        .join("Race");
    fs::create_dir_all(&legacy_model_dir).expect("mkdir legacy model dir");
    fs::write(
        rumoca_dir.join("project.toml"),
        r#"
version = 1

[project]
"#,
    )
    .expect("write root config");
    fs::write(
        legacy_model_dir.join(MODEL_SIMULATION_FILE),
        r#"
solver = "bdf"
t_end = 15.0
dt = 0.01
"#,
    )
    .expect("write legacy simulation");
    fs::write(
        legacy_model_dir.join(MODEL_VIEWS_FILE),
        r#"
[[views]]
id = "track_xy"
title = "Vehicle Track"
type = "scatter"
x = "px"
y = ["py"]
"#,
    )
    .expect("write legacy views");

    let loaded = ProjectConfig::discover(temp.path())
        .expect("discover")
        .expect("project exists");
    assert!(!loaded.data.simulation.models.contains_key("Vehicle.Race"));
    assert!(!loaded.data.plot.models.contains_key("Vehicle.Race"));
    assert!(loaded.model_identities.is_empty());
}
