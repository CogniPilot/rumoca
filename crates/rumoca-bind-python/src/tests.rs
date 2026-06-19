use super::*;
use std::time::{SystemTime, UNIX_EPOCH};

fn assert_dae_model_class_type(dae: &Value) {
    assert_eq!(
        dae.get("metadata")
            .and_then(|metadata| metadata.get("class_type"))
            .and_then(Value::as_str),
        Some("Model")
    );
}

#[test]
fn test_version() {
    let v = version();
    assert!(!v.is_empty());
}

#[test]
fn test_parse_valid() {
    let result = parse("model M Real x; end M;", None);
    assert!(result.success);
    assert!(result.error.is_none());
}

#[test]
fn test_parse_invalid() {
    let result = parse("model M Real x end M;", None);
    assert!(!result.success);
    assert!(result.error.is_some());
}

#[test]
fn test_lint() {
    let messages = lint("model m Real x; end m;", None);
    assert!(!messages.is_empty());
}

#[test]
fn test_format_valid() {
    let source = "model M Real x; end M;";
    let formatted = format_source(source, None).expect("format");
    assert_eq!(formatted, "model M Real x; end M;\n");
}

#[test]
fn test_format_or_original_invalid() {
    let source = "model M Real x end M;";
    let out = format_or_original(source, None);
    assert_eq!(out, source);
}

#[test]
fn test_get_builtin_targets_includes_sympy() {
    let targets = get_builtin_targets().expect("targets");
    let parsed: Value = serde_json::from_str(&targets).expect("json");
    assert!(parsed.as_array().is_some_and(|items| {
        items
            .iter()
            .any(|item| item.get("id").and_then(Value::as_str) == Some("sympy"))
    }));
}

#[test]
fn test_compile_inline_source() {
    let dae_json = compile(
        "model Ball\n  Real x(start=0);\nequation\n  der(x) = -x;\nend Ball;\n",
        Some("Ball"),
        Some("Ball.mo"),
        None,
    )
    .expect("compile inline");
    let dae: Value = serde_json::from_str(&dae_json).expect("valid DAE JSON");
    assert_dae_model_class_type(&dae);
    assert!(
        dae.get("x")
            .and_then(|v| v.as_object())
            .is_some_and(|state_map| state_map.contains_key("x"))
    );
}

#[test]
fn test_compile_to_json_returns_canonical_payload() {
    let dae_json = compile_to_json(
        "model Ball\n  Real x(start=0);\nequation\n  der(x) = -x;\nend Ball;\n",
        Some("Ball"),
        Some("Ball.mo"),
        None,
    )
    .expect("compile to json");
    let dae: Value = serde_json::from_str(&dae_json).expect("valid JSON");
    assert!(dae.get("x").is_some(), "canonical payload should contain x");
    assert!(
        dae.get("f_x").is_some(),
        "canonical payload should contain f_x"
    );
}

#[test]
fn test_compile_file_reads_source_from_path() {
    let temp_dir = unique_temp_model_dir("rumoca_bind_python_ball");
    fs::create_dir(&temp_dir).expect("create temp model dir");
    let temp_file = temp_dir.join("Ball.mo");
    fs::write(
        &temp_file,
        "model Ball\n  Real x(start=0);\nequation\n  der(x) = -x;\nend Ball;\n",
    )
    .expect("write temp model");

    let dae_json = compile_file(temp_file.to_string_lossy().as_ref(), Some("Ball"), None)
        .expect("compile from file");
    let dae: Value = serde_json::from_str(&dae_json).expect("valid DAE JSON");
    assert_dae_model_class_type(&dae);

    let _ = fs::remove_dir_all(temp_dir);
}

#[test]
fn test_compile_file_fixture_loads_source_root_and_infers_model() {
    let fixture = fixture_path("UsesLib.mo");
    let source_root = fixture_path("Lib");

    let dae_json = compile_file(
        fixture.to_string_lossy().as_ref(),
        None,
        Some(vec![source_root.to_string_lossy().to_string()]),
    )
    .expect("compile from fixture");
    let dae: Value = serde_json::from_str(&dae_json).expect("valid DAE JSON");
    assert_dae_model_class_type(&dae);
    assert!(
        dae.get("x")
            .and_then(|v| v.as_object())
            .is_some_and(|state_map| state_map.contains_key("x"))
    );
}

#[test]
fn test_project_session_reuses_fixture_source_root_for_simulation() {
    let fixture = fixture_path("UsesLib.mo");
    let source_root = fixture_path("Lib");
    let mut session = ProjectSession::new(Some(vec![source_root.to_string_lossy().to_string()]));

    let dae_json = session
        .compile_file(fixture.to_string_lossy().as_ref(), None)
        .expect("compile from project session");
    let dae: Value = serde_json::from_str(&dae_json).expect("valid DAE JSON");
    assert_dae_model_class_type(&dae);

    let statuses = session.source_root_statuses().expect("statuses");
    let parsed: Value = serde_json::from_str(&statuses).expect("status json");
    assert!(
        parsed.as_array().is_some_and(|items| !items.is_empty()),
        "expected loaded source root status after compile"
    );

    let sim_json = simulate_file_in_session(
        &mut session.session,
        fixture.to_string_lossy().as_ref(),
        None,
        &session.effective_source_root_paths,
        SimRequest {
            t_start: 0.0,
            t_end: 0.2,
            dt: Some(0.1),
            solver: Some("auto".to_string()),
            rtol: None,
            atol: None,
            max_wall_seconds: None,
        },
    )
    .expect("simulate fixture");
    let sim: Value = serde_json::from_str(&sim_json).expect("valid sim JSON");
    assert!(
        sim.get("metrics")
            .and_then(|value| value.get("points"))
            .and_then(Value::as_u64)
            .is_some_and(|points| points >= 2)
    );
}

#[test]
fn test_render_target_file_uses_native_dae_template() {
    let fixture = fixture_path("UsesLib.mo");
    let source_root = fixture_path("Lib");
    let rendered_json = render_target_file(
        fixture.to_string_lossy().as_ref(),
        "dae-modelica",
        None,
        Some(vec![source_root.to_string_lossy().to_string()]),
    )
    .expect("render target");
    let rendered_files: Value = serde_json::from_str(&rendered_json).expect("target JSON");
    let rendered = rendered_files[0]["content"].as_str().expect("content");
    assert!(rendered.contains("class UsesLib"));
}

#[test]
fn test_compile_source_alias_matches_compile() {
    let dae_json = compile_source(
        "model Ball\n  Real x(start=0);\nequation\n  der(x) = -x;\nend Ball;\n",
        Some("Ball"),
        Some("Ball.mo"),
        None,
    )
    .expect("compile via alias");
    let dae: Value = serde_json::from_str(&dae_json).expect("valid DAE JSON");
    assert_dae_model_class_type(&dae);
}

#[test]
fn test_compile_rejects_empty_source() {
    let err =
        compile("", Some("Ball"), Some("Ball.mo"), None).expect_err("empty source should fail");
    assert!(!err.0.is_empty());
}

#[test]
fn test_compile_file_rejects_missing_file() {
    let err = compile_file("missing.mo", Some("Ball"), None).expect_err("missing file should fail");
    assert!(err.0.contains("Failed to read"));
}

#[test]
fn test_load_source_roots_reports_fixture_load() {
    let source_root = fixture_path("Lib");
    let mut session = ProjectSession::new(Some(vec![source_root.to_string_lossy().to_string()]));
    let summary = session.load_source_roots().expect("load roots");
    let parsed: Value = serde_json::from_str(&summary).expect("summary json");
    assert!(
        parsed
            .get("reports")
            .and_then(Value::as_array)
            .is_some_and(|reports| !reports.is_empty())
    );
}

#[test]
fn test_workspace_source_roots_reads_project_config() {
    let temp_dir = unique_temp_model_dir("rumoca_bind_python_workspace_config");
    fs::create_dir(&temp_dir).expect("create temp dir");
    let lib_dir = temp_dir.join("Lib");
    fs::create_dir(&lib_dir).expect("create lib dir");
    fs::write(
        temp_dir.join("rumoca-workspace.toml"),
        "source_roots = [\"Lib\"]\n",
    )
    .expect("write workspace config");

    let focus = temp_dir.join("M.mo");
    let roots = workspace_source_roots(
        temp_dir.to_string_lossy().as_ref(),
        Some(focus.to_string_lossy().as_ref()),
    )
    .expect("workspace roots");
    let parsed: Value = serde_json::from_str(&roots).expect("valid roots JSON");
    assert!(parsed.as_array().is_some_and(|items| {
        items
            .iter()
            .any(|item| item.as_str().is_some_and(|path| path.ends_with("Lib")))
    }));

    let _ = fs::remove_dir_all(temp_dir);
}

#[test]
fn test_scenario_simulation_config_applies_toml_settings() {
    let temp_dir = unique_temp_model_dir("rumoca_bind_python_scenario_config");
    fs::create_dir(&temp_dir).expect("create temp dir");
    fs::write(
        temp_dir.join("rumoca-scenario.ball.toml"),
        r#"
[rumoca]
version = "1"
task = "simulate"

[model]
name = "Ball"

[sim]
solver = "rk-like"
t_end = 2.5
dt = 0.25
"#,
    )
    .expect("write scenario config");

    let config = scenario_simulation_config(
        temp_dir.to_string_lossy().as_ref(),
        "Ball",
        None,
        None,
        None,
        None,
        None,
    )
    .expect("scenario config");
    let parsed: Value = serde_json::from_str(&config).expect("valid config JSON");
    assert_eq!(parsed["effective"]["solver"], "rk-like");
    assert_eq!(parsed["effective"]["t_end"], 2.5);
    assert_eq!(parsed["effective"]["dt"], 0.25);

    let _ = fs::remove_dir_all(temp_dir);
}

fn fixture_path(relative: &str) -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(relative)
}

fn unique_temp_model_dir(stem: &str) -> std::path::PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time")
        .as_nanos();
    std::env::temp_dir().join(format!("{stem}_{nanos}"))
}
