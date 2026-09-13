//! Module reachability for the production-source accounting gates.

use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

use syn::ext::IdentExt;

struct ModuleEdge {
    path: PathBuf,
    requires_test: bool,
}

/// Exclude a file only when a test-gated declaration reaches it and no
/// production path does. Undeclared source files remain counted.
pub(super) fn test_only_files(files: &[PathBuf]) -> BTreeSet<PathBuf> {
    let graph: BTreeMap<_, _> = files
        .iter()
        .map(|path| (path.clone(), module_edges(path)))
        .collect();
    let mut frontier: Vec<_> = graph
        .values()
        .flatten()
        .filter(|edge| edge.requires_test)
        .map(|edge| edge.path.clone())
        .collect();
    let mut candidates = BTreeSet::new();
    while let Some(path) = frontier.pop() {
        if candidates.insert(path.clone())
            && let Some(edges) = graph.get(&path)
        {
            frontier.extend(edges.iter().map(|edge| edge.path.clone()));
        }
    }

    frontier.extend(
        files
            .iter()
            .filter(|path| !candidates.contains(*path))
            .cloned(),
    );
    while let Some(path) = frontier.pop() {
        let Some(edges) = graph.get(&path) else {
            continue;
        };
        for edge in edges {
            if !edge.requires_test && candidates.remove(&edge.path) {
                frontier.push(edge.path.clone());
            }
        }
    }
    candidates
}

fn module_edges(path: &Path) -> Vec<ModuleEdge> {
    let source = fs::read_to_string(path)
        .unwrap_or_else(|error| panic!("cannot read module source {}: {error}", path.display()));
    let syntax = syn::parse_file(&source)
        .unwrap_or_else(|error| panic!("cannot parse module source {}: {error}", path.display()));
    let mut edges = Vec::new();
    collect_module_edges(&syntax.items, &module_directory(path), false, &mut edges);
    edges
}

fn collect_module_edges(
    items: &[syn::Item],
    directory: &Path,
    requires_test: bool,
    edges: &mut Vec<ModuleEdge>,
) {
    for item in items {
        let syn::Item::Mod(module) = item else {
            continue;
        };
        let requires_test = requires_test
            || module.attrs.iter().any(|attribute| {
                attribute.path().is_ident("cfg")
                    && attribute
                        .parse_args::<syn::Path>()
                        .is_ok_and(|path| path.is_ident("test"))
            });
        let name = module.ident.unraw().to_string();
        if let Some((_, items)) = &module.content {
            collect_module_edges(items, &directory.join(name), requires_test, edges);
        } else if let Some(path) = module_file_in(directory, &name) {
            edges.push(ModuleEdge {
                path,
                requires_test,
            });
        }
    }
}

fn module_directory(declaring: &Path) -> PathBuf {
    match declaring.file_stem().and_then(|stem| stem.to_str()) {
        Some("mod" | "lib" | "main") => declaring
            .parent()
            .expect("source files have a containing directory")
            .to_path_buf(),
        _ => declaring.with_extension(""),
    }
}

fn module_file_in(directory: &Path, name: &str) -> Option<PathBuf> {
    let flat = directory.join(format!("{name}.rs"));
    if flat.is_file() {
        return Some(flat);
    }
    let nested = directory.join(name).join("mod.rs");
    nested.is_file().then_some(nested)
}

#[cfg(test)]
pub(super) fn resolve_module_file(declaring: &Path, name: &str) -> Option<PathBuf> {
    module_file_in(&module_directory(declaring), name)
}
