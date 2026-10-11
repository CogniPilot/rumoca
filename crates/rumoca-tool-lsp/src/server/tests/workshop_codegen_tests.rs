//! Replay tracked workshop scenarios through the editor's Generate Code command.

use super::*;
use std::collections::{BTreeMap, BTreeSet};

fn repository_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn required_text(path: &Path) -> String {
    std::fs::read_to_string(path)
        .unwrap_or_else(|error| panic!("required workshop fixture {}: {error}", path.display()))
}

async fn render_workshop_request(path: &Path, model: &str, target: &str) -> serde_json::Value {
    let path = path.canonicalize().expect("required fixture path");
    let service = new_test_service();
    execute_workshop_command(
        service.inner(),
        "rumoca.workspace.renderTarget",
        serde_json::json!({
            "uri": Url::from_file_path(&path).expect("fixture URI").to_string(),
            "model": model,
            "target": target,
        }),
    )
    .await
}

async fn execute_workshop_command(
    server: &ModelicaLanguageServer,
    command: &str,
    argument: serde_json::Value,
) -> serde_json::Value {
    server
        .execute_command(ExecuteCommandParams {
            command: command.to_owned(),
            arguments: vec![argument],
            work_done_progress_params: WorkDoneProgressParams::default(),
        })
        .await
        .expect("Generate Code request")
        .expect("Generate Code response")
}

fn rendered_files(response: &serde_json::Value) -> BTreeMap<String, String> {
    assert_eq!(
        response["ok"], true,
        "workshop rendering must succeed: {response}"
    );
    let mut files = BTreeMap::new();
    for file in response["files"]
        .as_array()
        .expect("complete files payload")
    {
        let path = file["path"].as_str().expect("artifact path");
        let content = file["content"].as_str().expect("artifact content");
        assert!(!content.trim().is_empty(), "empty artifact {path}");
        assert!(
            files.insert(path.to_owned(), content.to_owned()).is_none(),
            "duplicate artifact {path}"
        );
    }
    assert!(!files.is_empty(), "success needs artifacts");
    files
}

fn assert_scenario_artifacts(target: &str, files: &BTreeMap<String, String>) {
    let expected: &[(&str, &str)] = match target {
        "jax-ode" => &[("SympyDecay_jax_ode.py", "def rhs(t, x, u, p):")],
        "c-ode" => &[
            ("SympyDecay_ode.h", "SympyDecay_derivative_rhs"),
            ("SympyDecay_ode.c", "SympyDecay_derivative_rhs"),
        ],
        "checked_dae_report" => &[("SympyDecay_checked_dae.txt", "model SympyDecay")],
        "custom_checked_variables.jinja" => &[("custom_checked_variables", "state x real")],
        "galec-production" => &[
            ("AlgorithmCode/GalecCounter.alg", "method DoStep"),
            ("ProductionCode/rumoca_galec_kernels.h", "rumoca_galec"),
            ("ProductionCode/rumoca_galec_kernels.c", "rumoca_galec"),
            ("ProductionCode/GalecCounter.h", "GalecCounterState"),
            ("ProductionCode/GalecCounter.c", "GalecCounter_dostep"),
        ],
        other => panic!("new workshop scenario target needs behavioral expectations: {other}"),
    };
    assert_eq!(
        files.len(),
        expected.len(),
        "complete {target} source preview"
    );
    for &(path, marker) in expected {
        let source = files
            .get(path)
            .unwrap_or_else(|| panic!("missing {target} artifact {path}"));
        assert!(
            source.contains(marker),
            "{target}/{path} must contain {marker}: {source}"
        );
    }
    if target == "jax-ode" {
        let source = &files["SympyDecay_jax_ode.py"];
        assert_python_defaults(source);
    }
    if matches!(
        target,
        "checked_dae_report" | "custom_checked_variables.jinja"
    ) {
        let source = files.values().next().expect("report");
        assert!(
            source.contains("state x real"),
            "checked state metadata: {source}"
        );
        assert!(
            source.contains("parameter k real"),
            "checked parameter metadata: {source}"
        );
        assert!(
            source.contains("continuous_owners 1"),
            "checked equation owner: {source}"
        );
    }
}

#[test]
fn actual_codegen_scenarios_render_complete_editor_artifacts() {
    run_async_test(async {
        let directory = repository_root().join("examples/codegen");
        let mut scenarios: Vec<_> = std::fs::read_dir(&directory)
            .expect("tracked scenarios")
            .map(|entry| entry.expect("scenario entry").path())
            .filter(|path| {
                path.file_name()
                    .and_then(|name| name.to_str())
                    .is_some_and(|name| {
                        name.starts_with("rumoca-scenario.") && name.ends_with(".toml")
                    })
            })
            .collect();
        scenarios.sort();
        assert_eq!(
            scenarios.len(),
            5,
            "update the workshop roster and expectations for new scenarios"
        );
        for path in scenarios {
            let config: toml::Value =
                toml::from_str(&required_text(&path)).expect("actual scenario TOML");
            assert_eq!(config["rumoca"]["task"].as_str(), Some("codegen"));
            let model = config["model"]["name"].as_str().expect("configured model");
            let model_path = path.parent().expect("scenario directory").join(
                config["model"]["file"]
                    .as_str()
                    .expect("configured model source"),
            );
            assert!(
                model_path.is_file(),
                "missing actual source {}",
                model_path.display()
            );
            let target = config["codegen"]["target"]
                .as_str()
                .expect("configured target");
            let service = new_test_service();
            let server = service.inner();
            let uri = Url::from_file_path(path.canonicalize().expect("scenario path"))
                .expect("scenario URI");
            // The extension asks for the descriptor, then issues Generate Code
            // using its scenario URI. The handler reads TOML from disk itself;
            // TOML must never be injected as a Modelica compilation document.
            let scenario = execute_workshop_command(
                server,
                "rumoca.scenario.getScenarioConfig",
                serde_json::json!({"uri": uri.to_string()}),
            )
            .await;
            assert_eq!(
                scenario["ok"], true,
                "actual scenario descriptor: {scenario}"
            );
            assert_eq!(scenario["task"], "codegen");
            assert_eq!(scenario["model"], model);
            assert_eq!(scenario["target"], target);
            assert_eq!(
                scenario["outputDir"].as_str(),
                config["codegen"]["output_dir"].as_str()
            );
            let response = execute_workshop_command(server, "rumoca.workspace.renderTarget",
                serde_json::json!({"uri": uri.to_string(), "model": scenario["model"], "target": scenario["target"]})).await;
            assert_scenario_artifacts(target, &rendered_files(&response));
            assert!(
                server
                    .document_snapshot(&session_document_uri_key(&uri))
                    .await
                    .is_none(),
                "scenario TOML is configuration, not a Modelica session document"
            );
            if matches!(
                target,
                "checked_dae_report" | "custom_checked_variables.jinja"
            ) {
                assert_relative_target_requires_scenario_uri(server, &model_path, model, target)
                    .await;
            }
        }
    });
}

async fn assert_relative_target_requires_scenario_uri(
    server: &ModelicaLanguageServer,
    model_path: &Path,
    model: &str,
    target: &str,
) {
    let uri = Url::from_file_path(model_path.canonicalize().expect("actual model path"))
        .expect("model URI");
    let response = execute_workshop_command(
        server,
        "rumoca.workspace.renderTarget",
        serde_json::json!({"uri": uri.to_string(), "model": model, "target": target}),
    )
    .await;
    assert_eq!(
        response["ok"], false,
        "relative custom target cannot resolve beside model source: {response}"
    );
    assert!(
        response.get("files").is_none(),
        "wrong target base cannot expose artifacts"
    );
    assert!(
        response["error"]
            .as_str()
            .expect("target path error")
            .contains(target),
        "refusal must identify the misplaced relative target: {response}"
    );
}

#[test]
fn actual_ball_jax_editor_request_preserves_event_refusal() {
    run_async_test(async {
        let path = repository_root().join("examples/models/Ball.mo");
        let response = render_workshop_request(&path, "Ball", "jax-ode").await;
        assert_eq!(
            response["ok"], false,
            "eventful Ball/JAX must refuse: {response}"
        );
        assert!(
            response.to_string().contains("unsupported-feature:events"),
            "{response}"
        );
        assert!(
            response.get("files").is_none(),
            "a refusal cannot expose success artifacts"
        );
    });
}

fn assert_python_defaults(source: &str) {
    for (name, expected) in [("DEFAULT_X", vec![1.0]), ("DEFAULT_P", vec![0.5])] {
        let values: Vec<f64> = source
            .split(&format!("{name} = ["))
            .nth(1)
            .expect("checked defaults")
            .split(']')
            .next()
            .expect("default list")
            .split(',')
            .filter(|value| !value.trim().is_empty())
            .map(|value| value.trim().parse().expect("numeric default"))
            .collect();
        assert_eq!(values, expected, "actual SympyDecay {name}");
    }
}

#[test]
fn actual_sympy_decay_casadi_editor_export_contains_checked_callable_and_initial_values() {
    run_async_test(async {
        let path = repository_root().join("examples/models/SympyDecay.mo");
        let response = render_workshop_request(&path, "SympyDecay", "casadi-ode").await;
        let files = rendered_files(&response);
        assert_eq!(files.len(), 1);
        let source = files
            .get("SympyDecay_casadi_ode.py")
            .expect("CasADi artifact");
        for marker in [
            "def init_params(p):",
            "def init_state(p):",
            "ca.Function('rhs'",
            "N_Y = 1",
            "N_P = 1",
        ] {
            assert!(
                source.contains(marker),
                "missing checked callable surface {marker}: {source}"
            );
        }
        assert_python_defaults(source);
    });
}

fn xml_attribute<'a>(element: &'a str, name: &str) -> &'a str {
    element
        .split(&format!("{name}=\""))
        .nth(1)
        .expect("required XML attribute")
        .split('"')
        .next()
        .expect("attribute value")
}

fn assert_fmi_preview(target: &str, files: &BTreeMap<String, String>) {
    let manifest_path = repository_root()
        .join("crates/rumoca-phase-codegen/src/templates")
        .join(target)
        .join("target.toml");
    let manifest: toml::Value =
        toml::from_str(&required_text(&manifest_path)).expect("FMI target manifest");
    let expected: BTreeSet<_> = manifest["files"]
        .as_array()
        .expect("manifest file list")
        .iter()
        .map(|file| {
            format!(
                "FmiTensorDecay/{}",
                file["path"].as_str().expect("source path")
            )
        })
        .collect();
    assert_eq!(
        files.keys().cloned().collect::<BTreeSet<_>>(),
        expected,
        "every manifest-declared FMI source file"
    );
    let description = &files["FmiTensorDecay/modelDescription.xml"];
    for marker in [
        "modelName=\"FmiTensorDecay\"",
        "<ModelExchange",
        "<CoSimulation",
    ] {
        assert!(
            description.contains(marker),
            "missing FMI deployment metadata {marker}"
        );
    }
    if target == "fmi3" {
        for marker in [
            "fmiVersion=\"3.0\"",
            "name=\"x\"",
            "<Dimension start=\"2\"/>",
        ] {
            assert!(
                description.contains(marker),
                "native tensor FMI metadata {marker}: {description}"
            );
        }
        let state = description
            .split("<Float64 name=\"x\"")
            .nth(1)
            .expect("FMI3 output coordinate")
            .split('>')
            .next()
            .expect("state element");
        let starts: Vec<f64> = xml_attribute(state, "start")
            .split_whitespace()
            .map(|value| value.parse().expect("numeric state start"))
            .collect();
        assert_eq!(starts, vec![1.0, 1.0], "actual tensor starts");
        assert!(files["FmiTensorDecay/sources/buildDescription.xml"].contains("model.c"));
    } else {
        for marker in ["fmiVersion=\"2.0\"", "name=\"x[1]\"", "name=\"x[2]\""] {
            assert!(
                description.contains(marker),
                "scalarized FMI2 tensor metadata {marker}: {description}"
            );
        }
        for name in ["x[1]", "x[2]"] {
            let state = description
                .split(&format!("<ScalarVariable name=\"{name}\""))
                .nth(1)
                .expect("FMI2 output coordinate")
                .split("</ScalarVariable>")
                .next()
                .expect("state element");
            let start: f64 = xml_attribute(state, "start")
                .parse()
                .expect("numeric state start");
            assert_eq!(start, 1.0, "actual scalarized tensor start {name}");
        }
    }
    let identity_attribute = if target == "fmi3" {
        "instantiationToken=\""
    } else {
        "guid=\""
    };
    let identity = description
        .split(identity_attribute)
        .nth(1)
        .expect("FMI identity")
        .split('"')
        .next()
        .expect("identity value")
        .trim_matches(['{', '}', ' ']);
    assert_eq!(
        uuid::Uuid::parse_str(identity)
            .expect("valid minted FMI identity")
            .get_version_num(),
        4
    );
}

#[test]
fn actual_tensor_decay_fmi2_and_fmi3_editor_exports_include_entire_source_project() {
    run_async_test(async {
        let path = repository_root().join("examples/models/FmiTensorDecay.mo");
        for target in ["fmi2", "fmi3"] {
            let response = render_workshop_request(&path, "FmiTensorDecay", target).await;
            assert_fmi_preview(target, &rendered_files(&response));
        }
    });
}
