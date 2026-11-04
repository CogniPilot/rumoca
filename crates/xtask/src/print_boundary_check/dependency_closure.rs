//! Canonical production dependency closure from Cargo's resolved graph.
//!
//! Cargo resolves renames, optional features and target edges; this module does
//! not infer authority from crate-name prefixes or a hand-written TOML parser.
//! Snapshot equality binds identities. It is not an authorization to change a
//! frozen legacy source, and does not replace checking the concrete API.

#[cfg(test)]
mod tests;

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path};

use serde::{Deserialize, Serialize};

use super::{Checked, Refusal, refuse};

#[derive(Deserialize)]
struct Metadata {
    packages: Vec<Package>,
    resolve: Option<Resolve>,
}

#[derive(Deserialize)]
struct Package {
    id: String,
    name: String,
    version: String,
    source: Option<String>,
    manifest_path: String,
    features: BTreeMap<String, Vec<String>>,
}

#[derive(Deserialize)]
struct Resolve {
    nodes: Vec<Node>,
}

#[derive(Deserialize)]
struct Node {
    id: String,
    deps: Vec<Dependency>,
    features: Vec<String>,
}

#[derive(Deserialize)]
struct Dependency {
    name: String,
    pkg: String,
    dep_kinds: Vec<DependencyKind>,
}

#[derive(Deserialize)]
struct DependencyKind {
    kind: Option<String>,
    target: Option<String>,
}

/// Stable package identities avoid embedding a particular seat's checkout path.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
struct Identity {
    name: String,
    version: String,
    source: String,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
struct Edge {
    alias: String,
    package: Identity,
    kind: String,
    target: Option<String>,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
struct PackageClosure {
    identity: Identity,
    feature_definitions: BTreeMap<String, BTreeSet<String>>,
    resolved_features: BTreeSet<String>,
    edges: BTreeSet<Edge>,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ProductionClosure {
    packages: BTreeMap<String, PackageClosure>,
}

impl Package {
    fn identity(&self, root: &Path) -> Checked<Identity> {
        let source = match &self.source {
            Some(source) => source.clone(),
            None => {
                let relative = strip_workspace_prefix(Path::new(&self.manifest_path), root)
                    .ok_or_else(|| Refusal {
                        rule: "external-path-dependency",
                        item: self.id.clone(),
                    })?;
                if relative
                    .components()
                    .any(|part| !matches!(part, Component::Normal(_)))
                {
                    return refuse("dependency-path", &self.manifest_path);
                }
                format!(
                    "workspace:{}",
                    relative.to_string_lossy().replace('\\', "/")
                )
            }
        };
        Ok(Identity {
            name: self.name.clone(),
            version: self.version.clone(),
            source,
        })
    }
}

/// Canonical Windows roots may be verbatim while Cargo emits ordinary paths.
/// Only disk/UNC prefix representation is shared; every other component and
/// the caller's rejection of parent/root components remain strict.
fn strip_workspace_prefix<'a>(path: &'a Path, root: &Path) -> Option<&'a Path> {
    let mut remaining = path.components();
    for expected in root.components() {
        if !matching_component(remaining.next()?, expected) {
            return None;
        }
    }
    Some(remaining.as_path())
}

fn matching_component(actual: Component<'_>, expected: Component<'_>) -> bool {
    if actual == expected {
        return true;
    }
    #[cfg(windows)]
    if let (Component::Prefix(actual), Component::Prefix(expected)) = (actual, expected) {
        return ordinary_windows_prefix(actual.kind()) == ordinary_windows_prefix(expected.kind());
    }
    false
}

#[cfg(windows)]
fn ordinary_windows_prefix(prefix: std::path::Prefix<'_>) -> std::path::Prefix<'_> {
    use std::path::Prefix;

    match prefix {
        Prefix::VerbatimDisk(drive) => Prefix::Disk(drive),
        Prefix::VerbatimUNC(server, share) => Prefix::UNC(server, share),
        other => other,
    }
}

/// `metadata` must be captured by the eventual gate's own locked Cargo command;
/// a renderer-supplied metadata document is never an authority source.
pub fn collect(metadata: &str, root: &Path, entry_manifest: &Path) -> Checked<ProductionClosure> {
    if metadata.len() > 32 * 1024 * 1024 {
        return refuse("metadata-bound", "32 MiB");
    }
    let metadata: Metadata = serde_json::from_str(metadata).map_err(|error| Refusal {
        rule: "cargo-metadata",
        item: error.to_string(),
    })?;
    let resolve = metadata.resolve.ok_or(Refusal {
        rule: "missing-resolution",
        item: "Cargo resolve graph".into(),
    })?;
    let mut packages = BTreeMap::new();
    let mut identities = BTreeMap::new();
    let mut identity_keys = BTreeSet::new();
    let mut entry = None;
    for package in metadata.packages {
        if strip_workspace_prefix(Path::new(&package.manifest_path), entry_manifest)
            .is_some_and(|relative| relative.as_os_str().is_empty())
            && entry.replace(package.id.clone()).is_some()
        {
            return refuse("duplicate-entry", "manifest");
        }
        let identity = package.identity(root)?;
        let key = serde_json::to_string(&identity).map_err(|error| Refusal {
            rule: "package-identity",
            item: error.to_string(),
        })?;
        if !identity_keys.insert(key)
            || identities.insert(package.id.clone(), identity).is_some()
            || packages.insert(package.id.clone(), package).is_some()
        {
            return refuse("duplicate-package", "Cargo package identity");
        }
    }
    let mut nodes = BTreeMap::new();
    for node in resolve.nodes {
        if nodes.insert(node.id.clone(), node).is_some() {
            return refuse("duplicate-resolution", "Cargo node");
        }
    }
    let entry = entry.ok_or(Refusal {
        rule: "missing-entry",
        item: entry_manifest.display().to_string(),
    })?;
    walk(&entry, &packages, &identities, &nodes)
}

fn walk(
    entry: &str,
    packages: &BTreeMap<String, Package>,
    identities: &BTreeMap<String, Identity>,
    nodes: &BTreeMap<String, Node>,
) -> Checked<ProductionClosure> {
    let mut pending = BTreeSet::from([entry.to_owned()]);
    let mut visited = BTreeSet::new();
    let mut closure = BTreeMap::new();
    while let Some(id) = pending.pop_first() {
        if !visited.insert(id.clone()) {
            continue;
        }
        let package = required(packages, &id)?;
        let node = required(nodes, &id)?;
        let edges = production_edges(node, identities, &mut pending)?;
        let identity = required(identities, &id)?;
        let key = serde_json::to_string(identity).map_err(|error| Refusal {
            rule: "package-identity",
            item: error.to_string(),
        })?;
        closure.insert(
            key,
            PackageClosure {
                identity: Identity {
                    name: identity.name.clone(),
                    version: identity.version.clone(),
                    source: identity.source.clone(),
                },
                feature_definitions: package
                    .features
                    .iter()
                    .map(|(k, v)| (k.clone(), v.iter().cloned().collect()))
                    .collect(),
                resolved_features: node.features.iter().cloned().collect(),
                edges,
            },
        );
    }
    Ok(ProductionClosure { packages: closure })
}

fn production_edges(
    node: &Node,
    identities: &BTreeMap<String, Identity>,
    pending: &mut BTreeSet<String>,
) -> Checked<BTreeSet<Edge>> {
    let mut edges = BTreeSet::new();
    for dependency in &node.deps {
        let identity = required(identities, &dependency.pkg)?;
        if dependency.dep_kinds.is_empty() {
            return refuse("missing-dependency-kind", &dependency.name);
        }
        for kind in &dependency.dep_kinds {
            let kind_name = kind.kind.as_deref().unwrap_or("normal");
            match kind_name {
                "dev" => continue,
                "normal" | "build" => {}
                _ => return refuse("unknown-dependency-kind", kind_name),
            }
            pending.insert(dependency.pkg.clone());
            edges.insert(Edge {
                alias: dependency.name.clone(),
                package: Identity {
                    name: identity.name.clone(),
                    version: identity.version.clone(),
                    source: identity.source.clone(),
                },
                kind: kind_name.to_owned(),
                target: kind.target.clone(),
            });
        }
    }
    Ok(edges)
}

fn required<'a, T>(map: &'a BTreeMap<String, T>, id: &str) -> Checked<&'a T> {
    map.get(id).ok_or(Refusal {
        rule: "incomplete-resolution",
        item: id.to_owned(),
    })
}
