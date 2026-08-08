//! Drift guards for the `verify template-runtimes` gate.
//!
//! The gate pins Cargo test-target names and libtest filters as string
//! literals. Renaming or deleting a test in `crates/rumoca/tests` used to leave
//! those pins stale: `cargo test --test <gone>` fails the whole gate, and a
//! filter that matches nothing silently drops a runtime check. These tests read
//! the real manifest and the real test sources so both failure modes surface as
//! a precise `xtask` unit-test failure instead of an opaque gate abort.

use super::{RequiredExternalToolsMarker, TEMPLATE_RUNTIME_GROUPS, template_runtime_test_stems};
use crate::util::workspace_root_from_manifest_dir;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

/// Feature the gate passes to `cargo test`; every pinned target must require it
/// so the target is actually buildable under that invocation.
const GATE_FEATURE: &str = "template-runtime-tests";

#[test]
fn required_external_tools_marker_is_scoped_to_the_gate() {
    let root = tempfile::tempdir().expect("temporary workspace root");
    let path = root.path().join("target/template-runtimes/strict");
    {
        let _marker =
            RequiredExternalToolsMarker::new(root.path(), true).expect("create strict marker");
        assert!(path.is_file());
    }
    assert!(
        !path.exists(),
        "gate-created marker must be removed on exit"
    );
}

#[test]
fn required_external_tools_marker_preserves_explicit_local_policy() {
    let root = tempfile::tempdir().expect("temporary workspace root");
    let path = root.path().join("target/template-runtimes/strict");
    fs::create_dir_all(path.parent().expect("marker parent")).expect("create marker directory");
    fs::write(&path, b"local policy\n").expect("write local marker");
    {
        let _marker =
            RequiredExternalToolsMarker::new(root.path(), true).expect("reuse strict marker");
    }
    assert!(path.is_file(), "pre-existing marker belongs to the caller");
}

fn rumoca_manifest_path() -> PathBuf {
    workspace_root_from_manifest_dir(env!("CARGO_MANIFEST_DIR"))
        .join("crates")
        .join("rumoca")
        .join("Cargo.toml")
}

struct ManifestTestTarget {
    path: PathBuf,
    required_features: Vec<String>,
}

/// `required-features` of one `[[test]]` table, empty when the key is absent.
fn required_features(test: &toml::Value, name: &str) -> Vec<String> {
    let Some(features) = test
        .get("required-features")
        .and_then(toml::Value::as_array)
    else {
        return Vec::new();
    };
    features
        .iter()
        .map(|feature| {
            feature
                .as_str()
                .unwrap_or_else(|| panic!("[[test]] {name} required-features are strings"))
                .to_string()
        })
        .collect()
}

/// Read every `[[test]]` target declared by the `rumoca` package.
fn manifest_test_targets() -> BTreeMap<String, ManifestTestTarget> {
    let manifest_path = rumoca_manifest_path();
    let manifest_text = fs::read_to_string(&manifest_path)
        .unwrap_or_else(|err| panic!("read {}: {err}", manifest_path.display()));
    let manifest: toml::Value = toml::from_str(&manifest_text)
        .unwrap_or_else(|err| panic!("parse {}: {err}", manifest_path.display()));
    let package_dir = manifest_path
        .parent()
        .expect("rumoca manifest has a package directory")
        .to_path_buf();

    let tests = manifest
        .get("test")
        .and_then(toml::Value::as_array)
        .unwrap_or_else(|| panic!("{} declares [[test]] targets", manifest_path.display()));

    let mut targets = BTreeMap::new();
    for test in tests {
        let name = test
            .get("name")
            .and_then(toml::Value::as_str)
            .unwrap_or_else(|| panic!("every [[test]] in {} has a name", manifest_path.display()))
            .to_string();
        let path = test
            .get("path")
            .and_then(toml::Value::as_str)
            .unwrap_or_else(|| panic!("[[test]] {name} has an explicit path"));
        let required_features = required_features(test, &name);
        targets.insert(
            name,
            ManifestTestTarget {
                path: package_dir.join(path),
                required_features,
            },
        );
    }
    targets
}

/// `#[path = "…"]` on a module declaration, resolved against `dir`.
///
/// Umbrella test binaries pull their member files in this way, so the gate has
/// to follow the include to see the tests it selects.
fn path_attribute_target(item: &syn::ItemMod, dir: &Path) -> Option<PathBuf> {
    item.attrs
        .iter()
        .find(|attr| attr.path().is_ident("path"))
        .and_then(|attr| attr.meta.require_name_value().ok())
        .and_then(|meta| match &meta.value {
            syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(literal),
                ..
            }) => Some(dir.join(literal.value())),
            _ => None,
        })
}

/// Libtest paths of every `#[test]` function reachable from a test source file,
/// following `#[path]` module includes and prefixing each name with its module
/// path exactly the way libtest reports it.
fn declared_test_functions(path: &Path) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    collect_test_functions(path, "", &mut names);
    assert!(
        !names.is_empty(),
        "{} declares no #[test] functions",
        path.display()
    );
    names
}

fn collect_test_functions(path: &Path, prefix: &str, names: &mut BTreeSet<String>) {
    let source =
        fs::read_to_string(path).unwrap_or_else(|err| panic!("read {}: {err}", path.display()));
    let file =
        syn::parse_file(&source).unwrap_or_else(|err| panic!("parse {}: {err}", path.display()));
    let dir = path
        .parent()
        .unwrap_or_else(|| panic!("{} has a parent directory", path.display()));
    for item in &file.items {
        match item {
            syn::Item::Fn(function) => {
                let is_test = function
                    .attrs
                    .iter()
                    .any(|attr| attr.path().is_ident("test"));
                if is_test {
                    names.insert(format!("{prefix}{}", function.sig.ident));
                }
            }
            syn::Item::Mod(module) if module.content.is_none() => {
                let Some(target) = path_attribute_target(module, dir) else {
                    continue;
                };
                let nested = format!("{prefix}{}::", module.ident);
                collect_test_functions(&target, &nested, names);
            }
            _ => {}
        }
    }
}

#[test]
fn gate_pins_only_test_targets_that_exist_and_require_the_gate_feature() {
    let targets = manifest_test_targets();
    for group in TEMPLATE_RUNTIME_GROUPS {
        let target = targets.get(group.test).unwrap_or_else(|| {
            panic!(
                "template runtime gate pins test target `{}`, which crates/rumoca/Cargo.toml does not declare; declared targets: {:?}",
                group.test,
                targets.keys().collect::<Vec<_>>()
            )
        });
        assert!(
            target.path.is_file(),
            "test target `{}` points at missing source {}",
            group.test,
            target.path.display()
        );
        assert!(
            target
                .required_features
                .iter()
                .any(|feature| feature == GATE_FEATURE),
            "test target `{}` must require `{GATE_FEATURE}` because the gate builds it with that feature; found {:?}",
            group.test,
            target.required_features
        );
    }
}

#[test]
fn gate_filters_select_at_least_one_declared_test() {
    let targets = manifest_test_targets();
    for group in TEMPLATE_RUNTIME_GROUPS {
        let target = targets
            .get(group.test)
            .unwrap_or_else(|| panic!("test target `{}` is declared", group.test));
        let declared = declared_test_functions(&target.path);
        for filter in group.filters {
            assert!(
                declared.iter().any(|name| name.contains(filter)),
                "template runtime filter `{filter}` for target `{}` matches no #[test] in {}; declared: {declared:?}",
                group.test,
                target.path.display()
            );
        }
    }
}

#[test]
fn gate_runs_every_declared_test_of_every_pinned_target() {
    let targets = manifest_test_targets();
    let mut filters_by_target: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    let mut unfiltered_targets: BTreeSet<&str> = BTreeSet::new();
    for group in TEMPLATE_RUNTIME_GROUPS {
        if group.filters.is_empty() {
            unfiltered_targets.insert(group.test);
        }
        filters_by_target
            .entry(group.test)
            .or_default()
            .extend(group.filters.iter().copied());
    }

    for (test, filters) in filters_by_target {
        if unfiltered_targets.contains(test) {
            continue;
        }
        let target = targets
            .get(test)
            .unwrap_or_else(|| panic!("test target `{test}` is declared"));
        for name in declared_test_functions(&target.path) {
            assert!(
                filters.iter().any(|filter| name.contains(filter)),
                "`{name}` in {} is never selected by the template runtime gate; filters for `{test}`: {filters:?}",
                target.path.display()
            );
        }
    }
}

#[test]
fn artifact_trimmer_covers_exactly_the_pinned_targets() {
    let stems = template_runtime_test_stems();
    let expected: Vec<&str> = {
        let mut seen: Vec<&str> = Vec::new();
        for group in TEMPLATE_RUNTIME_GROUPS {
            if !seen.contains(&group.test) {
                seen.push(group.test);
            }
        }
        seen
    };
    assert_eq!(
        stems, expected,
        "the artifact trimmer must trim exactly the gate's test targets"
    );

    let targets = manifest_test_targets();
    let reachable: BTreeSet<String> = expected
        .iter()
        .flat_map(|test| {
            let target = targets
                .get(*test)
                .unwrap_or_else(|| panic!("test target `{test}` is declared"));
            declared_test_functions(&target.path)
        })
        .collect();
    assert!(
        reachable
            .iter()
            .any(|name| name.starts_with("backend_template_runtime_regression::")),
        "the backend runtime suite stays part of the gate; reachable tests: {reachable:?}"
    );
}
