//! xtask builds no compiler.
//!
//! `cargo xtask` runs on every developer and CI path, so it must not pull the
//! compiler into its build. It may depend on a workspace crate only when that
//! crate depends on no workspace crate itself, so the rule holds transitively
//! by construction. The shared MSL quality rules live in such a leaf crate
//! (`rumoca-msl-quality`) for exactly this reason.

use super::*;
use std::collections::{BTreeMap, BTreeSet};

/// The workspace crates a manifest's build-relevant tables name: normal,
/// build and target-specific dependencies (dev-dependencies don't build xtask).
fn workspace_dependencies(
    manifest: &str,
    workspace: &BTreeSet<String>,
) -> Result<BTreeSet<String>, String> {
    let table = toml::from_str::<toml::Table>(manifest)
        .map_err(|error| format!("manifest does not parse as TOML: {error}"))?;
    const KEYS: [&str; 2] = ["dependencies", "build-dependencies"];
    let targets = table
        .get("target")
        .and_then(toml::Value::as_table)
        .into_iter()
        .flat_map(|targets| targets.values());
    let mut tables = KEYS
        .iter()
        .filter_map(|key| table.get(*key))
        .collect::<Vec<_>>();
    tables.extend(targets.flat_map(|target| KEYS.iter().filter_map(move |key| target.get(*key))));
    let mut found = BTreeSet::new();
    for deps in tables {
        let deps = deps
            .as_table()
            .ok_or_else(|| "a dependency table is not a table".to_string())?;
        for (alias, spec) in deps {
            let name = spec
                .get("package")
                .and_then(toml::Value::as_str)
                .unwrap_or(alias);
            if workspace.contains(name) {
                found.insert(name.to_string());
            }
        }
    }
    Ok(found)
}

/// Every way xtask's dependencies break the leaf rule, given each workspace
/// crate's manifest by package name.
fn xtask_leaf_offences(manifests: &BTreeMap<String, String>) -> Vec<String> {
    let workspace = manifests.keys().cloned().collect::<BTreeSet<_>>();
    let Some(xtask) = manifests.get("xtask") else {
        return vec!["no xtask manifest".to_string()];
    };
    let direct = match workspace_dependencies(xtask, &workspace) {
        Ok(direct) => direct,
        Err(error) => return vec![format!("xtask: {error}")],
    };
    let mut offences = Vec::new();
    for dependency in direct {
        match workspace_dependencies(&manifests[&dependency], &workspace) {
            Ok(own) if own.is_empty() => {}
            Ok(own) => offences.push(format!(
                "xtask depends on `{dependency}`, which depends on workspace crates {own:?}"
            )),
            Err(error) => offences.push(format!("{dependency}: {error}")),
        }
    }
    offences
}

fn workspace_manifests() -> BTreeMap<String, String> {
    let mut manifests = BTreeMap::new();
    for entry in fs::read_dir(workspace_root().join("crates")).expect("read crates/") {
        let path = entry.expect("crates/ entry").path().join("Cargo.toml");
        let Ok(manifest) = fs::read_to_string(&path) else {
            continue;
        };
        let name = toml::from_str::<toml::Table>(&manifest)
            .ok()
            .and_then(|table| {
                table
                    .get("package")?
                    .get("name")?
                    .as_str()
                    .map(str::to_string)
            });
        if let Some(name) = name {
            manifests.insert(name, manifest);
        }
    }
    manifests
}

#[test]
fn xtask_depends_only_on_leaf_workspace_crates() {
    let offences = xtask_leaf_offences(&workspace_manifests());
    assert!(offences.is_empty(), "{offences:#?}");
}

#[test]
fn the_leaf_rule_refuses_a_dependency_that_reaches_the_compiler() {
    let manifests = |leaf_deps: &str| {
        BTreeMap::from([
            (
                "xtask".to_string(),
                "[package]\nname = \"xtask\"\n[dependencies]\nleaf = { path = \"../leaf\" }\n"
                    .to_string(),
            ),
            (
                "leaf".to_string(),
                format!("[package]\nname = \"leaf\"\n[dependencies]\n{leaf_deps}"),
            ),
            (
                "rumoca-core".to_string(),
                "[package]\nname = \"rumoca-core\"\n".to_string(),
            ),
        ])
    };
    assert!(xtask_leaf_offences(&manifests("serde = \"1\"\n")).is_empty());
    let offences = xtask_leaf_offences(&manifests("rumoca-core = { path = \"../rumoca-core\" }\n"));
    assert_eq!(offences.len(), 1, "{offences:?}");
    assert!(offences[0].contains("rumoca-core"), "{offences:?}");
    // A target-specific or renamed dependency is no way around the rule.
    let renamed = "[target.'cfg(unix)'.dependencies]\ncore = { package = \"rumoca-core\", path = \"../rumoca-core\" }\n";
    assert_eq!(xtask_leaf_offences(&manifests(renamed)).len(), 1);
}
