//! Finite execution owners for the exact guide fence inventory.

use super::user_guide_sources::{markdown_fences, repository_root};
use serde_json::Value;
use sha2::{Digest, Sha256};

fn profile_owner(profile: &Value, language: &str, page: &str) -> Result<&'static str, String> {
    let class = profile["classification"]
        .as_str()
        .ok_or("missing classification")?;
    let owner = profile["owner"].as_str().ok_or("missing owner")?;
    let allowed = match (language, class) {
        (
            "bash",
            "project-template" | "executable-command" | "executable-tool" | "executable-benchmark",
        ) => "cli-context",
        ("bash", "environment-setup") => "package-install",
        ("toml", "configuration-context") => "guide-toml",
        ("js", "executable-visualizer") => "book-browser",
        ("rust", "executable-context") => "opt-rhs",
        ("python", "executable-context") if page == "simulation/stdio-session.md" => "cli-stdio",
        ("python" | "cmake", "executable-context") => "python-guide",
        ("text", "explanatory-output") => "displayed-output",
        ("xml", "explanatory-output") => "metadata-output",
        ("json", "explanatory-output") => "protocol-output",
        _ => {
            return Err(format!(
                "unsupported guide classification {language}/{class}"
            ));
        }
    };
    if owner != allowed {
        return Err(format!(
            "{language}/{class} requires {allowed}, got {owner}"
        ));
    }
    if !profile["scope"]
        .as_str()
        .is_some_and(|scope| !scope.is_empty())
    {
        return Err("missing proof scope".to_owned());
    }
    Ok(allowed)
}

fn require_source(path: &str, marker: &str) {
    let text = std::fs::read_to_string(repository_root().join(path))
        .unwrap_or_else(|error| panic!("required guide owner {path}: {error}"));
    assert!(
        text.contains(marker),
        "required guide owner {path} lost {marker}"
    );
}

fn require_execution_owner(owner: &str) {
    match owner {
        "cli-context" => {
            require_source(
                "crates/rumoca/tests/suite_core/user_guide_cli.rs",
                "fn user_guide_cli_templates_execute(",
            );
            require_source(
                "crates/rumoca/tests/suite_core/main.rs",
                "mod user_guide_cli;",
            );
            require_source(".github/workflows/ci.yml", "cargo xtask verify workspace");
        }
        "package-install" => {
            require_source(
                "crates/rumoca/tests/suite_core/user_guide_cli.rs",
                "fn user_guide_bash_syntax(",
            );
            require_source(".github/workflows/ci.yml", "build-python-wheels:");
        }
        "cli-stdio" => {
            require_source(
                "crates/rumoca/tests/suite_core/user_guide_cli.rs",
                "fn user_guide_stdio_python(",
            );
            require_source("crates/xtask/src/verify_cmd.rs", "user_guide_stdio_python");
        }
        "guide-toml" => {
            require_source(
                "crates/rumoca-bind-wasm/src/tests/user_guide_config_fragment_tests.rs",
                "fn user_guide_toml_fragments_parse_in_their_documented_configuration_context(",
            );
            require_source(
                "crates/rumoca-bind-wasm/src/tests.rs",
                "mod user_guide_config_fragment_tests;",
            );
            require_source(".github/workflows/ci.yml", "cargo xtask playground test");
        }
        "opt-rhs" => {
            require_source(
                "crates/rumoca-opt/src/tests.rs",
                "fn user_guide_rhs_fit_uses_the_authored_neural_model(",
            );
            require_source(".github/workflows/ci.yml", "cargo xtask verify workspace");
        }
        "python-guide" => {
            require_source(
                "crates/rumoca-bind-python/tests/guide_scenario_test.py",
                "def test_guide_cmake_fences(",
            );
            require_source(
                ".github/workflows/ci.yml",
                "python crates/rumoca-bind-python/tests/guide_scenario_test.py",
            );
            require_source(
                "crates/rumoca-bind-python/tests/guide_test.py",
                "def test_guide_python_fences(",
            );
            require_source(
                ".github/workflows/ci.yml",
                "python crates/rumoca-bind-python/tests/guide_test.py",
            );
        }
        "book-browser" => {
            require_source(
                "packages/playground/tests/book_live_smoke.mjs",
                "All book live-example checks passed",
            );
            require_source(".github/workflows/ci.yml", "book_live_smoke.mjs");
        }
        "displayed-output" | "metadata-output" | "protocol-output" => {}
        _ => panic!("unregistered guide owner {owner}"),
    }
}

fn check_model_contexts(inventory: &Value) {
    let contexts = inventory["contexts"]
        .as_object()
        .expect("authored CLI contexts");
    assert_eq!(contexts.len(), 2, "review new teaching model contexts");
    let fences = markdown_fences();
    for (key, model) in [
        ("stdio-plant", "Plant"),
        ("first-hanging-mass", "HangingMass"),
    ] {
        let entry = &contexts[key];
        let page = entry["page"].as_str().unwrap();
        let fence = fences
            .iter()
            .find(|fence| {
                fence.page == repository_root().join("docs/user-guide/src").join(page)
                    && entry["line"].as_u64() == Some(fence.line as u64)
            })
            .expect("authored model context must identify an actual fence");
        assert_eq!(fence.language, "modelica");
        assert_eq!(fence.model_name().as_deref(), Some(model));
        assert_eq!(entry["source_line_count"], fence.source_line_count);
        let digest = format!("{:x}", Sha256::digest(fence.source.as_bytes()));
        assert_eq!(entry["source_sha256"], digest);
    }
}

#[test]
fn user_guide_other_language_inventory_rejects_unclassified_or_changed_examples() {
    let inventory = std::fs::read_to_string(
        repository_root().join("docs/verification/user-guide-ci/non-modelica-inventory.json"),
    )
    .unwrap();
    let inventory: Value = serde_json::from_str(&inventory).unwrap();
    assert_eq!(inventory["schema"], 2);
    check_model_contexts(&inventory);
    let entries = inventory["entries"].as_array().unwrap();
    let fences: Vec<_> = markdown_fences()
        .into_iter()
        .filter(|fence| fence.language != "modelica")
        .collect();
    assert_eq!(fences.len(), 117, "review newly added guide contexts");
    assert_eq!(
        entries.len(),
        fences.len(),
        "every fence needs an execution owner"
    );
    let mut owners = std::collections::BTreeSet::new();
    for (fence, entry) in fences.into_iter().zip(entries) {
        let source_root = repository_root().join("docs/user-guide/src");
        let page = fence
            .page
            .strip_prefix(source_root)
            .unwrap()
            .to_str()
            .unwrap();
        assert_eq!(entry["page"], page);
        assert_eq!(entry["line"], fence.line);
        assert_eq!(entry["source_line_count"], fence.source_line_count);
        assert_eq!(entry["language"], fence.language);
        let digest = format!("{:x}", Sha256::digest(fence.source.as_bytes()));
        assert_eq!(entry["source_sha256"], digest, "{page}:{}", fence.line);
        let key = entry["profile"].as_str().expect("classified example");
        owners.insert(
            profile_owner(&inventory["profiles"][key], &fence.language, page)
                .unwrap_or_else(|error| panic!("{page}:{}: {error}", fence.line)),
        );
    }
    for owner in owners {
        require_execution_owner(owner);
    }
}

#[test]
fn user_guide_inventory_rejects_unknown_or_missing_execution_owners() {
    let profile = serde_json::json!({"classification": "executable-context", "owner": "someone", "scope": "not proof"});
    assert!(profile_owner(&profile, "python", "tools/python.md").is_err());
    let profile = serde_json::json!({"classification": "executable-context", "scope": "not proof"});
    assert!(profile_owner(&profile, "python", "tools/python.md").is_err());
    let profile = serde_json::json!({"classification": "placeholder", "owner": "displayed-output", "scope": "not proof"});
    assert!(profile_owner(&profile, "python", "tools/python.md").is_err());
    assert!(profile_owner(&serde_json::Value::Null, "bash", "tools/cli.md").is_err());
}
