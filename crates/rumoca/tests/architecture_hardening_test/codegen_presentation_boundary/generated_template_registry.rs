//! Runtime proof that build-generated template tables equal their owned files.

use super::*;
use rumoca_phase_codegen::templates;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, PartialEq, Eq)]
struct TemplateInventories {
    targets: Vec<String>,
    templates: Vec<String>,
    assets: Vec<String>,
}

#[test]
fn build_generated_template_registry_equals_the_owned_files() {
    let expected =
        expected_inventories(&workspace_root().join(CODEGEN_CRATE).join("src/templates"));
    let actual = runtime_inventories();
    assert_eq!(
        actual, expected,
        "build.rs/generated Rust may only expose the exact owned manifests, READMEs, artifact Jinja sources, and target asset paths/bytes"
    );
}

fn expected_inventories(templates_root: &Path) -> TemplateInventories {
    let mut directories = fs::read_dir(templates_root)
        .expect("read built-in template directory")
        .map(|entry| entry.expect("read built-in target entry").path())
        .filter(|path| path.join("target.toml").is_file())
        .collect::<Vec<_>>();
    directories.sort();
    let mut inventories = TemplateInventories {
        targets: Vec::new(),
        templates: Vec::new(),
        assets: expected_asset_inventory(&directories),
    };
    for directory in directories {
        append_expected_target(&directory, &mut inventories);
    }
    inventories.targets.sort();
    inventories.templates.sort();
    inventories
}

#[test]
fn mutation_generated_asset_injection_and_rewrite_change_inventory() {
    let baseline = vec![asset_entry("target", "runtime/model.c", b"original")];
    let mut injected = baseline.clone();
    injected.push(asset_entry("target", "runtime/hidden.c", b"lowering"));
    assert_ne!(
        baseline, injected,
        "an injected runtime asset changes identity"
    );
    let rewritten = vec![asset_entry("target", "runtime/model.c", b"rewritten")];
    assert_ne!(baseline, rewritten, "rewritten asset bytes change identity");
}

fn append_expected_target(directory: &Path, inventories: &mut TemplateInventories) {
    let target = directory
        .file_name()
        .and_then(|name| name.to_str())
        .expect("built-in target directory has a UTF-8 name");
    let manifest =
        fs::read_to_string(directory.join("target.toml")).expect("read built-in target manifest");
    let readme = fs::read_to_string(directory.join("README.md")).expect("read target README");
    inventories
        .targets
        .push(target_entry(target, &manifest, &readme));
    let manifest_value = manifest
        .parse::<toml::Value>()
        .expect("parse built-in target manifest");
    let mut declarations = declared_templates(&manifest_value);
    for path in jinja_files(directory) {
        assert!(
            declarations.remove(&path),
            "undeclared built-in template {target}/{path}"
        );
        let source = fs::read_to_string(directory.join(&path)).expect("read built-in template");
        inventories
            .templates
            .push(template_entry(target, &path, &source));
    }
    for (path, owner) in declared_borrowed_templates(&manifest_value) {
        assert!(
            declarations.remove(&path),
            "borrowed template {target}/{path} must be one declared file template"
        );
        assert!(
            !directory.join(&path).exists(),
            "borrowed template {target}/{path} must not duplicate owner bytes locally"
        );
        let owner_path = directory
            .parent()
            .expect("built-in target lives below the template root")
            .join(&owner)
            .join(&path);
        let _source = fs::read_to_string(&owner_path).unwrap_or_else(|error| {
            panic!("read borrowed template {target}/{path} from exact owner {owner}: {error}")
        });
        // Borrowing resolves through the canonical owner's generated entry;
        // it must not manufacture a second borrower-owned registry entry.
    }
    assert!(
        declarations.is_empty(),
        "manifest declarations without Jinja files in {target}: {declarations:#?}"
    );
}

fn declared_borrowed_templates(manifest: &toml::Value) -> BTreeMap<String, String> {
    manifest
        .get("files")
        .and_then(toml::Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|row| {
            let owner = row
                .get("template_shared_from")?
                .as_str()
                .unwrap_or_else(|| {
                    panic!("template_shared_from must be a string in a checked built-in manifest")
                });
            let template = row
                .get("template")
                .and_then(toml::Value::as_str)
                .expect("[[files]] requires a string template");
            Some((template.to_owned(), owner.to_owned()))
        })
        .collect()
}

fn declared_templates(manifest: &toml::Value) -> BTreeSet<String> {
    assert!(
        manifest.get("partials").is_none(),
        "built-in manifests must not declare global support partials"
    );
    let mut declarations = BTreeSet::new();
    let rows = manifest
        .get("files")
        .and_then(toml::Value::as_array)
        .into_iter()
        .flatten();
    for row in rows {
        assert!(
            row.get("shared_as").is_none(),
            "built-in artifact templates must not publish global shared aliases"
        );
        let template = row
            .get("template")
            .and_then(toml::Value::as_str)
            .expect("[[files]] requires a string template");
        assert!(
            declarations.insert(template.to_string()),
            "template {template} is declared more than once"
        );
    }
    declarations
}

fn jinja_files(directory: &Path) -> Vec<String> {
    let mut paths = fs::read_dir(directory)
        .expect("read built-in target directory")
        .map(|entry| entry.expect("read built-in target file").path())
        .filter(|path| {
            path.extension()
                .is_some_and(|extension| extension == "jinja")
        })
        .map(|path| {
            path.file_name()
                .and_then(|name| name.to_str())
                .expect("built-in template has a UTF-8 name")
                .to_string()
        })
        .collect::<Vec<_>>();
    paths.sort();
    paths
}

fn expected_asset_inventory(directories: &[PathBuf]) -> Vec<String> {
    let mut owned = BTreeMap::<String, Vec<(String, Vec<u8>)>>::new();
    let mut borrowed = Vec::new();
    for directory in directories {
        let target = directory
            .file_name()
            .and_then(|name| name.to_str())
            .expect("built-in target directory has a UTF-8 name")
            .to_string();
        owned.insert(target.clone(), owned_assets(directory));
        borrowed.extend(borrowed_asset_bundles(directory, &target));
    }
    let mut inventory = owned
        .iter()
        .flat_map(|(target, assets)| {
            assets
                .iter()
                .map(|(path, bytes)| asset_entry(target, path, bytes))
        })
        .collect::<Vec<_>>();
    for (target, source, owner) in borrowed {
        let prefix = format!("{}/", source.trim_end_matches('/'));
        let owner_assets = owned
            .get(&owner)
            .unwrap_or_else(|| panic!("borrowed assets name unknown owner {owner}"));
        inventory.extend(
            owner_assets
                .iter()
                .filter(|(path, _)| path.starts_with(&prefix))
                .map(|(path, bytes)| asset_entry(&target, path, bytes)),
        );
    }
    inventory.sort();
    inventory
}

fn owned_assets(directory: &Path) -> Vec<(String, Vec<u8>)> {
    fn collect(root: &Path, directory: &Path, assets: &mut Vec<(String, Vec<u8>)>) {
        let mut entries = fs::read_dir(directory)
            .expect("read target asset directory")
            .map(|entry| entry.expect("read target asset entry").path())
            .collect::<Vec<_>>();
        entries.sort();
        for path in entries {
            let metadata = fs::symlink_metadata(&path).expect("stat target asset");
            assert!(
                !metadata.file_type().is_symlink(),
                "target assets may not be symlinks"
            );
            if metadata.is_dir() {
                collect(root, &path, assets);
            } else if is_owned_asset(root, &path) {
                let relative = path
                    .strip_prefix(root)
                    .expect("target asset is under its root")
                    .to_string_lossy()
                    .replace(std::path::MAIN_SEPARATOR, "/");
                assets.push((relative, fs::read(&path).expect("read target asset bytes")));
            }
        }
    }

    let mut assets = Vec::new();
    collect(directory, directory, &mut assets);
    assets
}

fn is_owned_asset(root: &Path, path: &Path) -> bool {
    path != root.join("target.toml")
        && path != root.join("README.md")
        && path
            .extension()
            .is_none_or(|extension| extension != "jinja")
}

fn borrowed_asset_bundles(directory: &Path, target: &str) -> Vec<(String, String, String)> {
    let manifest = fs::read_to_string(directory.join("target.toml"))
        .expect("read target manifest for asset declarations")
        .parse::<toml::Value>()
        .expect("parse target manifest for asset declarations");
    manifest
        .get("assets")
        .and_then(toml::Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|row| {
            let source = row.get("source")?.as_str()?;
            let owner = row.get("shared_from")?.as_str()?;
            Some((target.to_string(), source.to_string(), owner.to_string()))
        })
        .collect()
}

fn runtime_inventories() -> TemplateInventories {
    let mut targets = Vec::new();
    let mut template_entries = Vec::new();
    for target in templates::builtin_targets() {
        targets.push(target_entry(target.name, target.manifest, target.readme));
        for template in target.templates {
            template_entries.push(template_entry(target.name, template.path, template.source));
        }
    }
    let mut assets = templates::builtin_targets()
        .iter()
        .flat_map(|target| {
            target
                .assets
                .iter()
                .map(|asset| asset_entry(target.name, asset.path, asset.bytes))
        })
        .collect::<Vec<_>>();
    targets.sort();
    template_entries.sort();
    assets.sort();
    TemplateInventories {
        targets,
        templates: template_entries,
        assets,
    }
}

fn target_entry(target: &str, manifest: &str, readme: &str) -> String {
    format!(
        "{target}:{}:{}",
        content_fingerprint(manifest),
        content_fingerprint(readme)
    )
}

fn template_entry(target: &str, path: &str, source: &str) -> String {
    format!("{target}:{path}:{}", content_fingerprint(source))
}

fn asset_entry(target: &str, path: &str, bytes: &[u8]) -> String {
    format!("{target}:{path}:{}", blake3::hash(bytes).to_hex())
}
