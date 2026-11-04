#[cfg(windows)]
mod windows_paths;

use super::*;
use serde_json::{Value, json};

fn fixture() -> Value {
    json!({
        "packages": [
            {"id":"entry", "name":"printer", "version":"1", "source":null,
             "manifest_path":"/repo/crates/printer/Cargo.toml", "features":{"default":["checked"],"checked":[]}},
            {"id":"plan", "name":"plan", "version":"2", "source":null,
             "manifest_path":"/repo/crates/plan/Cargo.toml", "features":{}},
            {"id":"external", "name":"format", "version":"3", "source":"registry+checksum-bound-index",
             "manifest_path":"/cache/format/Cargo.toml", "features":{}},
            {"id":"test", "name":"test-compiler", "version":"1", "source":null,
             "manifest_path":"/repo/crates/test-compiler/Cargo.toml", "features":{}}
        ],
        "resolve":{"nodes":[
            {"id":"entry", "features":["checked","default"], "deps":[
                {"name":"issued", "pkg":"plan", "dep_kinds":[{"kind":null,"target":null}]},
                {"name":"formatter", "pkg":"external", "dep_kinds":[{"kind":"build","target":"cfg(unix)"}]},
                {"name":"fixture", "pkg":"test", "dep_kinds":[{"kind":"dev","target":null}]}
            ]},
            {"id":"plan", "features":[], "deps":[]},
            {"id":"external", "features":[], "deps":[]},
            {"id":"test", "features":[], "deps":[]}
        ]}
    })
}

fn snapshot(value: &Value) -> Checked<ProductionClosure> {
    collect(
        &value.to_string(),
        Path::new("/repo"),
        Path::new("/repo/crates/printer/Cargo.toml"),
    )
}

#[test]
fn production_graph_keeps_renames_build_edges_and_target_conditions() {
    let result = snapshot(&fixture()).expect("resolved production graph");
    assert_eq!(result.packages.len(), 3);
    let printer = result
        .packages
        .values()
        .find(|p| p.identity.name == "printer")
        .unwrap();
    assert_eq!(printer.edges.len(), 2);
    assert!(
        printer
            .edges
            .iter()
            .any(|e| e.alias == "issued" && e.package.name == "plan")
    );
    assert!(
        printer
            .edges
            .iter()
            .any(|e| e.kind == "build" && e.target.as_deref() == Some("cfg(unix)"))
    );
}

#[test]
fn optional_features_renames_versions_and_transitive_edges_change_identity() {
    let base = fixture();
    let expected = snapshot(&base).unwrap();
    let mut changes = Vec::new();
    let mut value = base.clone();
    value["resolve"]["nodes"][0]["features"] = json!(["unchecked"]);
    changes.push(value);
    let mut value = base.clone();
    value["packages"][0]["features"]["checked"] = json!(["new-helper"]);
    changes.push(value);
    let mut value = base.clone();
    value["resolve"]["nodes"][0]["deps"][0]["name"] = json!("other");
    changes.push(value);
    let mut value = base.clone();
    value["packages"][1]["version"] = json!("9");
    changes.push(value);
    let mut value = base.clone();
    value["resolve"]["nodes"][1]["deps"] = json!([
        {"name":"hidden_compiler","pkg":"test","dep_kinds":[{"kind":null,"target":null}]}
    ]);
    changes.push(value);
    let mut value = base.clone();
    value["resolve"]["nodes"][0]["deps"][1]["dep_kinds"][0]["target"] = json!("cfg(windows)");
    changes.push(value);
    for value in changes {
        assert_ne!(snapshot(&value).unwrap(), expected);
    }
}

#[test]
fn cycles_terminate_and_dev_only_edges_do_not_grant_production_authority() {
    let mut value = fixture();
    value["resolve"]["nodes"][1]["deps"] = json!([
        {"name":"printer","pkg":"entry","dep_kinds":[{"kind":null,"target":null}]}
    ]);
    assert_eq!(snapshot(&value).unwrap().packages.len(), 3);
    assert!(
        !snapshot(&fixture())
            .unwrap()
            .packages
            .values()
            .any(|p| p.identity.name == "test-compiler")
    );
}

#[test]
fn incomplete_ambiguous_and_unknown_resolution_are_refused() {
    let mut value = fixture();
    value["resolve"] = Value::Null;
    assert_eq!(snapshot(&value).unwrap_err().rule, "missing-resolution");
    let mut value = fixture();
    value["resolve"]["nodes"][0]["deps"][0]["pkg"] = json!("gone");
    assert_eq!(snapshot(&value).unwrap_err().rule, "incomplete-resolution");
    let mut value = fixture();
    value["resolve"]["nodes"][0]["deps"][0]["dep_kinds"] = json!([]);
    assert_eq!(
        snapshot(&value).unwrap_err().rule,
        "missing-dependency-kind"
    );
    let mut value = fixture();
    value["resolve"]["nodes"][0]["deps"][0]["dep_kinds"][0]["kind"] = json!("unknown");
    assert_eq!(
        snapshot(&value).unwrap_err().rule,
        "unknown-dependency-kind"
    );
    let mut value = fixture();
    let node = value["resolve"]["nodes"][0].clone();
    value["resolve"]["nodes"].as_array_mut().unwrap().push(node);
    assert_eq!(snapshot(&value).unwrap_err().rule, "duplicate-resolution");
}

#[test]
fn dependency_paths_cannot_escape_the_audited_workspace() {
    let mut value = fixture();
    value["packages"][1]["manifest_path"] = json!("/elsewhere/plan/Cargo.toml");
    assert_eq!(
        snapshot(&value).unwrap_err().rule,
        "external-path-dependency"
    );
    let mut value = fixture();
    value["packages"][1]["manifest_path"] = json!("/repo/../outside/Cargo.toml");
    assert_eq!(snapshot(&value).unwrap_err().rule, "dependency-path");
}

#[test]
fn entry_manifest_requires_the_entire_path_not_a_parent_or_child() {
    let value = fixture().to_string();
    for entry in [
        "/repo/crates/printer",
        "/repo/crates/printer/Cargo.toml/child",
    ] {
        assert_eq!(
            collect(&value, Path::new("/repo"), Path::new(entry))
                .unwrap_err()
                .rule,
            "missing-entry"
        );
    }
}

#[test]
fn graph_order_is_not_authority_and_checkout_paths_are_portable() {
    let base = fixture();
    let expected = snapshot(&base).unwrap();
    let mut reordered = base.clone();
    reordered["packages"].as_array_mut().unwrap().reverse();
    reordered["resolve"]["nodes"]
        .as_array_mut()
        .unwrap()
        .reverse();
    assert_eq!(snapshot(&reordered).unwrap(), expected);
    let relocated = base.to_string().replace("/repo/", "/other/");
    assert_eq!(
        collect(
            &relocated,
            Path::new("/other"),
            Path::new("/other/crates/printer/Cargo.toml")
        )
        .unwrap(),
        expected
    );
}

#[test]
fn actual_locked_cargo_resolution_binds_optional_target_build_and_renamed_edges() {
    let tree = tempfile::tempdir().unwrap();
    let root = tree.path().canonicalize().unwrap();
    std::fs::write(root.join("Cargo.toml"), "[workspace]\nmembers = [\"printer\", \"plan\", \"helper\", \"format-support\", \"test-only\"]\nresolver = \"2\"\n").unwrap();
    for name in ["printer", "plan", "helper", "format-support", "test-only"] {
        let dir = root.join(name);
        std::fs::create_dir_all(dir.join("src")).unwrap();
        let extra = if name == "printer" {
            "[features]\ndefault = [\"dep:issued\"]\nextra = []\n[dependencies]\nissued = { package = \"plan\", path = \"../plan\", optional = true }\n[build-dependencies]\nhelper = { path = \"../helper\" }\n[target.'cfg(windows)'.dependencies]\nformatter = { package = \"format-support\", path = \"../format-support\" }\n[dev-dependencies]\ntest-only = { path = \"../test-only\" }\n"
        } else {
            ""
        };
        std::fs::write(
            dir.join("Cargo.toml"),
            format!(
                "[package]\nname = \"{name}\"\nversion = \"0.1.0\"\nedition = \"2024\"\n{extra}"
            ),
        )
        .unwrap();
        std::fs::write(dir.join("src/lib.rs"), "").unwrap();
    }
    let capture = |locked: bool| {
        let mut command = std::process::Command::new(env!("CARGO"));
        command.current_dir(&root).args([
            "metadata",
            "--offline",
            "--all-features",
            "--format-version",
            "1",
        ]);
        if locked {
            command.arg("--locked");
        }
        let output = command.output().unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8(output.stdout).unwrap()
    };
    capture(false);
    let metadata = capture(true);
    let closure = collect(&metadata, &root, &root.join("printer/Cargo.toml")).unwrap();
    assert_eq!(closure.packages.len(), 4);
    assert!(
        !closure
            .packages
            .values()
            .any(|p| p.identity.name == "test-only")
    );
    let printer = closure
        .packages
        .values()
        .find(|p| p.identity.name == "printer")
        .unwrap();
    assert_eq!(printer.edges.len(), 3);
    assert!(
        printer
            .edges
            .iter()
            .any(|e| e.alias == "issued" && e.package.name == "plan")
    );
    assert!(
        printer
            .edges
            .iter()
            .any(|e| e.alias == "formatter" && e.target.as_deref() == Some("cfg(windows)"))
    );
    assert!(
        printer
            .edges
            .iter()
            .any(|e| e.kind == "build" && e.package.name == "helper")
    );
    assert!(printer.resolved_features.contains("extra"));
}

#[test]
fn serialized_dependency_identity_preserves_edges_features_and_portable_sources() {
    let capture = snapshot(&fixture()).unwrap();
    let encoded = serde_json::to_value(&capture).unwrap();
    let packages = encoded["packages"].as_object().unwrap();
    assert_eq!(packages.len(), 3);
    let printer = packages
        .values()
        .find(|value| value["identity"]["name"] == "printer")
        .unwrap();
    assert_eq!(
        printer["identity"]["source"],
        "workspace:crates/printer/Cargo.toml"
    );
    assert_eq!(printer["resolved_features"], json!(["checked", "default"]));
    assert_eq!(
        printer["feature_definitions"]["default"],
        json!(["checked"])
    );
    let edges = printer["edges"].as_array().unwrap();
    assert_eq!(edges.len(), 2);
    assert!(edges.iter().any(|edge| edge["alias"] == "formatter"
        && edge["kind"] == "build"
        && edge["target"] == "cfg(unix)"));
    assert!(!encoded.to_string().contains("/repo/"));
}
