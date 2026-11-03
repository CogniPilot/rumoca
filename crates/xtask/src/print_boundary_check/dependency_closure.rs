//! Canonical production dependency closure from Cargo's resolved graph.
//!
//! Cargo resolves renames, optional features and target edges; this module does
//! not infer authority from crate-name prefixes or a hand-written TOML parser.
//! Snapshot equality binds identities. It is not an authorization to change a
//! frozen legacy source, and does not replace checking the concrete API.

#[cfg(test)]
mod tests;

use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

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
                let relative = Path::new(&self.manifest_path)
                    .strip_prefix(root)
                    .map_err(|_| Refusal {
                        rule: "external-path-dependency",
                        item: self.id.clone(),
                    })?;
                if relative
                    .components()
                    .any(|part| !matches!(part, std::path::Component::Normal(_)))
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
        if Path::new(&package.manifest_path) == entry_manifest
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
