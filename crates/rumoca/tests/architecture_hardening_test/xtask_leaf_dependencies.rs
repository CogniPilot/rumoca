//! xtask builds no compiler.
//!
//! `cargo xtask` runs on every developer and CI path, so it must not pull the
//! compiler into its build. It may depend only on the workspace crates declared
//! in [`DECLARED_LEAVES`], and each of those must depend on no workspace crate
//! itself, so the rule holds transitively by construction (SPEC_0041 §1). The
//! shared MSL quality rules live in such a leaf crate (`rumoca-msl-quality`)
//! for exactly this reason.

use super::*;
use std::collections::{BTreeMap, BTreeSet};

/// The workspace crates xtask may depend on. Each is a reviewed leaf: adding
/// one is a SPEC_0041 change, and it must keep depending on no workspace crate.
const DECLARED_LEAVES: [&str; 1] = ["rumoca-msl-quality"];

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
/// crate's manifest by package name and the crates declared as leaves.
fn xtask_leaf_offences(manifests: &BTreeMap<String, String>, leaves: &[&str]) -> Vec<String> {
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
        if !leaves.contains(&dependency.as_str()) {
            offences.push(format!(
                "xtask depends on `{dependency}`, which is not a declared leaf \
                 (DECLARED_LEAVES, SPEC_0041 §1); move the work behind a command \
                 in the owning package"
            ));
            continue;
        }
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
    let offences = xtask_leaf_offences(&workspace_manifests(), &DECLARED_LEAVES);
    assert!(offences.is_empty(), "{offences:#?}");
}

#[test]
fn every_declared_leaf_is_a_workspace_crate_with_no_workspace_dependency() {
    let manifests = workspace_manifests();
    let workspace = manifests.keys().cloned().collect::<BTreeSet<_>>();
    for leaf in DECLARED_LEAVES {
        let manifest = manifests
            .get(leaf)
            .unwrap_or_else(|| panic!("declared leaf `{leaf}` is not a workspace crate"));
        let own = workspace_dependencies(manifest, &workspace).expect("leaf manifest parses");
        assert!(
            own.is_empty(),
            "declared leaf `{leaf}` depends on workspace crates {own:?}"
        );
    }
}

#[test]
fn the_leaf_rule_refuses_an_undeclared_workspace_dependency() {
    // A workspace crate with no workspace dependencies of its own is still
    // refused until it is declared, and the compiler crates the old manifest
    // ban named are refused by the same rule.
    let manifests = |xtask_deps: &str| {
        BTreeMap::from([
            (
                "xtask".to_string(),
                format!("[package]\nname = \"xtask\"\n[dependencies]\n{xtask_deps}"),
            ),
            (
                "leaf".to_string(),
                "[package]\nname = \"leaf\"\n[dependencies]\nserde = \"1\"\n".to_string(),
            ),
            (
                "rumoca-core".to_string(),
                "[package]\nname = \"rumoca-core\"\n".to_string(),
            ),
        ])
    };
    let leaf = "leaf = { path = \"../leaf\" }\n";
    assert!(xtask_leaf_offences(&manifests(leaf), &["leaf"]).is_empty());
    let offences = xtask_leaf_offences(&manifests(leaf), &[]);
    assert_eq!(offences.len(), 1, "{offences:?}");
    assert!(offences[0].contains("not a declared leaf"), "{offences:?}");
    let compiler = "rumoca-core = { path = \"../rumoca-core\" }\n";
    let offences = xtask_leaf_offences(&manifests(compiler), &["leaf"]);
    assert_eq!(offences.len(), 1, "{offences:?}");
    assert!(offences[0].contains("rumoca-core"), "{offences:?}");
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
    assert!(xtask_leaf_offences(&manifests("serde = \"1\"\n"), &["leaf"]).is_empty());
    let offences = xtask_leaf_offences(
        &manifests("rumoca-core = { path = \"../rumoca-core\" }\n"),
        &["leaf"],
    );
    assert_eq!(offences.len(), 1, "{offences:?}");
    assert!(offences[0].contains("rumoca-core"), "{offences:?}");
    // A target-specific or renamed dependency is no way around the rule.
    let renamed = "[target.'cfg(unix)'.dependencies]\ncore = { package = \"rumoca-core\", path = \"../rumoca-core\" }\n";
    assert_eq!(xtask_leaf_offences(&manifests(renamed), &["leaf"]).len(), 1);
}
