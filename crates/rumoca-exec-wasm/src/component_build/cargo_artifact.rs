//! Cargo-owned emitted artifact identity, independent of reused cache contents.

use std::path::Path;

pub(super) fn require_exact_artifact(
    messages: &str,
    manifest: &Path,
    library: &str,
    component: &Path,
) -> Result<(), String> {
    let manifest = manifest.canonicalize().map_err(|error| error.to_string())?;
    let component = component
        .canonicalize()
        .map_err(|error| error.to_string())?;
    let mut matches = 0;
    for line in messages.lines() {
        let Ok(message) = serde_json::from_str::<serde_json::Value>(line) else {
            continue;
        };
        if is_exact_artifact(&message, &manifest, library, &component) {
            matches += 1;
        }
    }
    if matches != 1 {
        return Err(
            "Cargo did not emit exactly one component for the retained manifest/library".into(),
        );
    }
    Ok(())
}

fn is_exact_artifact(
    message: &serde_json::Value,
    manifest: &Path,
    library: &str,
    component: &Path,
) -> bool {
    if message["reason"] != "compiler-artifact" || message["target"]["name"] != library {
        return false;
    }
    let Some(emitted_manifest) = message["manifest_path"].as_str() else {
        return false;
    };
    if Path::new(emitted_manifest).canonicalize().ok().as_deref() != Some(manifest) {
        return false;
    }
    let Some(kinds) = message["target"]["kind"].as_array() else {
        return false;
    };
    if !kinds.iter().any(|kind| kind == "cdylib") {
        return false;
    }
    message["filenames"].as_array().is_some_and(|filenames| {
        filenames
            .iter()
            .filter_map(|name| name.as_str())
            .any(|name| Path::new(name).canonicalize().ok().as_deref() == Some(component))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn exact_artifact_refuses_same_cache_foreign_manifest_library_and_missing_output() {
        let work = tempfile::tempdir().unwrap();
        let manifest = work.path().join("Cargo.toml");
        let foreign = work.path().join("foreign.toml");
        let component = work.path().join("component.wasm");
        for path in [&manifest, &foreign, &component] {
            std::fs::write(path, b"fixture").unwrap();
        }
        let exact = serde_json::json!({
            "reason": "compiler-artifact", "manifest_path": manifest,
            "target": {"name": "component", "kind": ["cdylib"]},
            "filenames": [component]
        });
        let check = |message: &serde_json::Value| {
            require_exact_artifact(&message.to_string(), &manifest, "component", &component)
        };
        check(&exact).unwrap();
        let mut wrong_manifest = exact.clone();
        wrong_manifest["manifest_path"] = serde_json::json!(foreign);
        assert!(check(&wrong_manifest).is_err());
        let mut wrong_library = exact.clone();
        wrong_library["target"]["name"] = serde_json::json!("foreign");
        assert!(check(&wrong_library).is_err());
        let mut no_output = exact.clone();
        no_output["filenames"] = serde_json::json!([]);
        assert!(check(&no_output).is_err());
        assert!(require_exact_artifact("", &manifest, "component", &component).is_err());
        assert!(
            require_exact_artifact(
                &format!("{exact}\n{exact}"),
                &manifest,
                "component",
                &component
            )
            .is_err()
        );
    }
}
