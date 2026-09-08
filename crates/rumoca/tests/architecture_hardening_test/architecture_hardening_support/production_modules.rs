//! Shared Cargo library/binary module-graph reachability.
//!
//! Compiler library roots, nondefault `[lib]` roots, `src/main.rs`, automatic
//! `src/bin` roots, and explicit `[[bin]]` roots are all production-capable and
//! therefore included. Build scripts and example/bench/test targets are outside
//! this compiler-production graph; gates using this helper must state that
//! boundary rather than presenting it as macro/build-generated-code proof.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ProductionRustSourceContext {
    pub(crate) canonical_path: PathBuf,
    pub(crate) crate_aliases: BTreeMap<String, String>,
    pub(crate) module_path: Vec<String>,
    pub(crate) path: PathBuf,
    pub(crate) source: String,
    pub(crate) target: String,
}

pub(crate) fn production_rust_sources(
    crate_root: &Path,
    display_base: &Path,
) -> Vec<(PathBuf, String)> {
    let mut emitted = BTreeSet::new();
    production_rust_source_contexts(crate_root, display_base)
        .into_iter()
        .filter_map(|context| {
            emitted
                .insert(context.canonical_path)
                .then_some((context.path, context.source))
        })
        .collect()
}

pub(crate) fn production_rust_source_contexts(
    crate_root: &Path,
    display_base: &Path,
) -> Vec<ProductionRustSourceContext> {
    let mut collection = ProductionModuleCollection {
        crate_aliases: cargo_dependency_aliases(crate_root, display_base),
        display_base,
        visited_contexts: BTreeSet::new(),
        contexts: Vec::new(),
    };
    for (target, entry) in production_rust_target_roots(crate_root, display_base) {
        let target_identity = if target == "bin:auto" {
            format!("{target}:{}", entry.display())
        } else {
            target
        };
        let absolute = display_base.join(&entry);
        let module_dir = absolute
            .parent()
            .expect("Rust target root has a parent directory");
        collect_production_module(
            &absolute,
            module_dir,
            &target_identity,
            &[],
            &mut collection,
        );
    }
    collection.contexts.sort();
    collection.contexts
}

struct ProductionModuleCollection<'a> {
    contexts: Vec<ProductionRustSourceContext>,
    crate_aliases: BTreeMap<String, String>,
    display_base: &'a Path,
    visited_contexts: BTreeSet<(String, Vec<String>, PathBuf, PathBuf)>,
}

pub(crate) fn production_rust_target_roots(
    crate_root: &Path,
    display_base: &Path,
) -> Vec<(String, PathBuf)> {
    let manifest_path = crate_root.join("Cargo.toml");
    let manifest_source = fs::read_to_string(&manifest_path)
        .unwrap_or_else(|error| panic!("read Cargo manifest {}: {error}", manifest_path.display()));
    let manifest = manifest_source
        .parse::<toml::Value>()
        .unwrap_or_else(|error| {
            panic!("parse Cargo manifest {}: {error}", manifest_path.display())
        });
    let mut roots = Vec::new();
    collect_library_root(crate_root, &manifest, &mut roots);
    collect_explicit_binary_roots(crate_root, &manifest, &mut roots);
    if package_auto_target_enabled(&manifest, "autobins") {
        collect_automatic_binary_roots(crate_root, &mut roots);
    }
    roots.sort();
    roots.dedup();
    assert!(
        !roots.is_empty(),
        "Cargo crate has no production Rust target roots"
    );
    roots
        .into_iter()
        .map(|(kind, path)| {
            assert!(
                path.is_file(),
                "Cargo production target `{kind}` is missing: {}",
                path.display()
            );
            let display = path
                .strip_prefix(display_base)
                .unwrap_or(&path)
                .to_path_buf();
            (kind, display)
        })
        .collect()
}

fn collect_library_root(
    crate_root: &Path,
    manifest: &toml::Value,
    roots: &mut Vec<(String, PathBuf)>,
) {
    let explicit_library = manifest.get("lib");
    let configured = manifest
        .get("lib")
        .and_then(|library| library.get("path"))
        .and_then(toml::Value::as_str)
        .map(|path| crate_root.join(path));
    if let Some(path) = configured {
        roots.push(("lib".to_string(), path));
    } else if explicit_library.is_some()
        || (package_auto_target_enabled(manifest, "autolib")
            && crate_root.join("src/lib.rs").is_file())
    {
        roots.push(("lib".to_string(), crate_root.join("src/lib.rs")));
    }
}

fn package_auto_target_enabled(manifest: &toml::Value, key: &str) -> bool {
    manifest
        .get("package")
        .and_then(|package| package.get(key))
        .and_then(toml::Value::as_bool)
        .unwrap_or(true)
}

fn collect_explicit_binary_roots(
    crate_root: &Path,
    manifest: &toml::Value,
    roots: &mut Vec<(String, PathBuf)>,
) {
    let package_name = manifest
        .get("package")
        .and_then(|package| package.get("name"))
        .and_then(toml::Value::as_str);
    let Some(binaries) = manifest.get("bin").and_then(toml::Value::as_array) else {
        return;
    };
    for binary in binaries {
        let name = binary.get("name").and_then(toml::Value::as_str);
        let path = binary
            .get("path")
            .and_then(toml::Value::as_str)
            .map(|path| crate_root.join(path))
            .unwrap_or_else(|| inferred_binary_path(crate_root, name, package_name));
        let label = name.map_or_else(|| path.display().to_string(), str::to_string);
        roots.push((format!("bin:{label}"), path));
    }
}

fn inferred_binary_path(
    crate_root: &Path,
    name: Option<&str>,
    package_name: Option<&str>,
) -> PathBuf {
    let name = name.expect("an explicit [[bin]] without `path` must declare `name`");
    let mut candidates = vec![
        crate_root.join("src/bin").join(format!("{name}.rs")),
        crate_root.join("src/bin").join(name).join("main.rs"),
    ];
    if package_name == Some(name) {
        candidates.push(crate_root.join("src/main.rs"));
    }
    let existing = candidates
        .into_iter()
        .filter(|candidate| candidate.is_file())
        .collect::<Vec<_>>();
    assert_eq!(
        existing.len(),
        1,
        "explicit Cargo binary `{name}` must resolve to exactly one source, found {existing:?}"
    );
    existing.into_iter().next().expect("one binary source")
}

fn collect_automatic_binary_roots(crate_root: &Path, roots: &mut Vec<(String, PathBuf)>) {
    let main = crate_root.join("src/main.rs");
    if main.is_file() {
        roots.push(("bin:auto:main".to_string(), main));
    }
    let directory = crate_root.join("src/bin");
    let Ok(entries) = fs::read_dir(&directory) else {
        return;
    };
    let mut entries = entries
        .map(|entry| entry.expect("automatic binary directory entry").path())
        .collect::<Vec<_>>();
    entries.sort();
    for path in entries {
        let root = if path.extension().is_some_and(|extension| extension == "rs") {
            Some(path)
        } else if path.is_dir() && path.join("main.rs").is_file() {
            Some(path.join("main.rs"))
        } else {
            None
        };
        if let Some(root) = root {
            roots.push(("bin:auto".to_string(), root));
        }
    }
}

fn collect_production_module(
    source_path: &Path,
    module_dir: &Path,
    target: &str,
    module_path: &[String],
    collection: &mut ProductionModuleCollection<'_>,
) {
    let source_path = source_path
        .canonicalize()
        .unwrap_or_else(|error| panic!("resolve Rust module {}: {error}", source_path.display()));
    let lexical_module_dir = module_dir.to_path_buf();
    if !collection.visited_contexts.insert((
        target.to_string(),
        module_path.to_vec(),
        source_path.clone(),
        lexical_module_dir,
    )) {
        return;
    }
    let source = fs::read_to_string(&source_path)
        .unwrap_or_else(|error| panic!("read Rust module {}: {error}", source_path.display()));
    let syntax = syn::parse_file(&source)
        .unwrap_or_else(|error| panic!("parse Rust module {}: {error}", source_path.display()));
    let display_path = source_path
        .strip_prefix(collection.display_base)
        .unwrap_or(&source_path)
        .to_path_buf();
    collection.contexts.push(ProductionRustSourceContext {
        canonical_path: source_path.clone(),
        crate_aliases: collection.crate_aliases.clone(),
        module_path: module_path.to_vec(),
        path: display_path,
        source: source.clone(),
        target: target.to_string(),
    });
    collect_declared_modules(
        &syntax.items,
        &source_path,
        module_dir,
        target,
        module_path,
        source_path
            .parent()
            .expect("Rust module source has a parent directory"),
        collection,
    );
}

fn cargo_dependency_aliases(crate_root: &Path, workspace_root: &Path) -> BTreeMap<String, String> {
    let crate_manifest = read_manifest(&crate_root.join("Cargo.toml"));
    let workspace_manifest_path = workspace_root.join("Cargo.toml");
    let workspace_manifest = workspace_manifest_path
        .is_file()
        .then(|| read_manifest(&workspace_manifest_path));
    let workspace_dependencies = workspace_manifest
        .as_ref()
        .and_then(|manifest| manifest.get("workspace"))
        .and_then(|workspace| workspace.get("dependencies"))
        .and_then(toml::Value::as_table);
    let mut aliases = BTreeMap::new();
    collect_dependency_aliases(
        crate_manifest
            .get("dependencies")
            .and_then(toml::Value::as_table),
        workspace_dependencies,
        &mut aliases,
    );
    if let Some(targets) = crate_manifest.get("target").and_then(toml::Value::as_table) {
        for target in targets.values() {
            collect_dependency_aliases(
                target.get("dependencies").and_then(toml::Value::as_table),
                workspace_dependencies,
                &mut aliases,
            );
        }
    }
    aliases
}

fn read_manifest(path: &Path) -> toml::Value {
    fs::read_to_string(path)
        .unwrap_or_else(|error| panic!("read Cargo manifest {}: {error}", path.display()))
        .parse()
        .unwrap_or_else(|error| panic!("parse Cargo manifest {}: {error}", path.display()))
}

fn collect_dependency_aliases(
    dependencies: Option<&toml::map::Map<String, toml::Value>>,
    workspace_dependencies: Option<&toml::map::Map<String, toml::Value>>,
    aliases: &mut BTreeMap<String, String>,
) {
    let Some(dependencies) = dependencies else {
        return;
    };
    for (alias, dependency) in dependencies {
        let workspace_dependency = dependency
            .get("workspace")
            .and_then(toml::Value::as_bool)
            .is_some_and(|enabled| enabled)
            .then(|| workspace_dependencies.and_then(|entries| entries.get(alias)))
            .flatten();
        let package = dependency
            .get("package")
            .and_then(toml::Value::as_str)
            .or_else(|| {
                workspace_dependency
                    .and_then(|dependency| dependency.get("package"))
                    .and_then(toml::Value::as_str)
            })
            .unwrap_or(alias);
        let canonical = package.replace('-', "_");
        aliases.insert(alias.replace('-', "_"), canonical);
    }
}

fn collect_declared_modules(
    items: &[syn::Item],
    containing_file: &Path,
    module_dir: &Path,
    target: &str,
    module_path: &[String],
    path_attribute_base: &Path,
    collection: &mut ProductionModuleCollection<'_>,
) {
    for item in items {
        let syn::Item::Mod(item_module) = item else {
            continue;
        };
        if attributes_require_test(&item_module.attrs) {
            continue;
        }
        let module_name = item_module.ident.to_string();
        let mut child_module_path = module_path.to_vec();
        child_module_path.push(module_name.clone());
        if let Some((_, nested)) = &item_module.content {
            collect_declared_modules(
                nested,
                containing_file,
                &module_dir.join(&module_name),
                target,
                &child_module_path,
                &path_attribute_base.join(&module_name),
                collection,
            );
            continue;
        }
        let explicit_path = module_path_attribute(&item_module.attrs)
            .map(|relative| path_attribute_base.join(relative));
        let candidates = explicit_path.map_or_else(
            || {
                vec![
                    module_dir.join(format!("{module_name}.rs")),
                    module_dir.join(&module_name).join("mod.rs"),
                ]
            },
            |path| vec![path],
        );
        let existing = candidates
            .into_iter()
            .filter(|candidate| candidate.is_file())
            .collect::<Vec<_>>();
        assert_eq!(
            existing.len(),
            1,
            "production module `{module_name}` in {} must resolve to exactly one source, found {existing:?}",
            containing_file.display()
        );
        let child_path = &existing[0];
        let child_module_dir = if child_path.file_name().is_some_and(|name| name == "mod.rs") {
            child_path
                .parent()
                .expect("mod.rs has a parent directory")
                .to_path_buf()
        } else {
            child_path.with_extension("")
        };
        collect_production_module(
            child_path,
            &child_module_dir,
            target,
            &child_module_path,
            collection,
        );
    }
}

fn module_path_attribute(attributes: &[syn::Attribute]) -> Option<PathBuf> {
    attributes.iter().find_map(|attribute| {
        if !attribute.path().is_ident("path") {
            return None;
        }
        let syn::Meta::NameValue(name_value) = &attribute.meta else {
            panic!("Rust #[path] attribute must be a string name-value");
        };
        let syn::Expr::Lit(literal) = &name_value.value else {
            panic!("Rust #[path] value must be a string literal");
        };
        let syn::Lit::Str(path) = &literal.lit else {
            panic!("Rust #[path] value must be a string literal");
        };
        Some(PathBuf::from(path.value()))
    })
}

/// Whether these attributes keep their item out of every production build.
///
/// The `cfg` half reads the parsed [`syn::Meta`] rather than its flattened
/// tokens, so a value containing a comma or a parenthesis cannot split the
/// predicate. Only a predicate absent from every production build exempts the
/// item; a flag-dependent one ships whenever its flag is on.
pub(crate) fn attributes_require_test(attributes: &[syn::Attribute]) -> bool {
    attributes.iter().any(|attribute| {
        if attribute.path().is_ident("test") {
            return true;
        }
        let syn::Meta::List(list) = &attribute.meta else {
            return false;
        };
        !super::cfg_predicate::cfg_list_visibility(list).reaches_production()
    })
}
