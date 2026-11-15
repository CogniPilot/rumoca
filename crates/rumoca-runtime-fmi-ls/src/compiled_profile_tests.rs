//! The published compiled guest enables no reference (interpreted) execution.
//!
//! A static walk of the manifests the guest builds with, from the published and
//! certification product features through every production edge it activates
//! for `wasm32-wasip2`, applying Cargo's default-feature inheritance. It guards
//! the feature graph only: `cargo tree -e features` over the real resolve and a
//! name census of the built cores remain the build-time proof.

use std::collections::{BTreeMap, BTreeSet};

const WORKSPACE: &str = include_str!("../../../Cargo.toml");

const MANIFESTS: [(&str, &str); 7] = [
    ("rumoca-runtime-fmi-ls", include_str!("../Cargo.toml")),
    ("rumoca-sim", include_str!("../../rumoca-sim/Cargo.toml")),
    ("rumoca-exec-wasm", include_str!("../../rumoca-exec-wasm/Cargo.toml")),
    ("rumoca-solver", include_str!("../../rumoca-solver/Cargo.toml")),
    ("rumoca-solver-rk45", include_str!("../../rumoca-solver-rk45/Cargo.toml")),
    ("rumoca-eval-solve", include_str!("../../rumoca-eval-solve/Cargo.toml")),
    ("rumoca-phase-solve", include_str!("../../rumoca-phase-solve/Cargo.toml")),
];

/// The features that compile reference execution into a crate.
const REFERENCE: [(&str, &str); 3] = [
    ("rumoca-eval-solve", "reference-eval"),
    ("rumoca-solver", "reference-execution"),
    ("rumoca-sim", "reference-execution"),
];

struct Edge {
    dependency: String,
    features: Vec<String>,
    default_features: bool,
    optional: bool,
}

struct Manifest {
    features: BTreeMap<String, Vec<String>>,
    edges: Vec<Edge>,
}

fn quoted(text: &str) -> Vec<String> {
    text.split('"')
        .skip(1)
        .step_by(2)
        .map(str::to_string)
        .collect()
}

/// The `features = [..]` list of one inline dependency table, never the
/// `default-features` key.
fn edge_features(body: &str) -> Vec<String> {
    let mut search = 0;
    while let Some(found) = body[search..].find("features = [") {
        let start = search + found;
        if !body[..start].ends_with('-') {
            let list = &body[start..];
            let end = list.find(']').expect("a closed features list");
            return quoted(&list[..end]);
        }
        search = start + 1;
    }
    Vec::new()
}

/// Whether a dependency table under `section` is linked into a
/// `wasm32-wasip2` production build.
fn production_section_for_wasip2(section: &str) -> bool {
    if section == "[dependencies]" {
        return true;
    }
    if !section.starts_with("[target.") || !section.ends_with(".dependencies]") {
        return false;
    }
    !section.contains("not(target_arch = \"wasm32\")")
        && !section.contains("target_os = \"unknown\"")
}

fn workspace_default_features(dependency: &str) -> bool {
    let prefix = format!("{dependency} = ");
    WORKSPACE
        .lines()
        .find(|line| line.starts_with(&prefix))
        .is_none_or(|line| !line.contains("default-features = false"))
}

fn parse(names: &BTreeSet<&str>, text: &str) -> Manifest {
    let mut features = BTreeMap::new();
    let mut edges = Vec::new();
    let mut section = String::new();
    let mut open: Option<(String, String)> = None;
    for line in text.lines() {
        let line = line.trim();
        if let Some((name, mut list)) = open.take() {
            list.push_str(line);
            if line.contains(']') {
                features.insert(name, quoted(&list));
            } else {
                open = Some((name, list));
            }
            continue;
        }
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        // A key line never starts with `[`; a target header may contain `=`.
        if line.starts_with('[') {
            section = line.to_string();
            continue;
        }
        let Some((key, value)) = line.split_once('=') else {
            continue;
        };
        let (key, value) = (key.trim(), value.trim());
        if section == "[features]" {
            if value.contains(']') {
                features.insert(key.to_string(), quoted(value));
            } else {
                open = Some((key.to_string(), value.to_string()));
            }
        } else if production_section_for_wasip2(&section) && names.contains(key) {
            let inherited = value.contains("workspace = true");
            let default_features = if inherited {
                workspace_default_features(key)
            } else {
                !value.contains("default-features = false")
            };
            edges.push(Edge {
                dependency: key.to_string(),
                features: edge_features(value),
                default_features,
                optional: value.contains("optional = true"),
            });
        }
    }
    Manifest { features, edges }
}

struct Closure {
    manifests: BTreeMap<String, Manifest>,
    active: BTreeSet<String>,
    enabled: BTreeSet<(String, String)>,
}

impl Closure {
    fn new() -> Self {
        let names = MANIFESTS.iter().map(|(name, _)| *name).collect();
        let manifests = MANIFESTS
            .iter()
            .map(|(name, text)| (name.to_string(), parse(&names, text)))
            .collect();
        Self {
            manifests,
            active: BTreeSet::new(),
            enabled: BTreeSet::new(),
        }
    }

    fn activate(&mut self, krate: &str, default_features: bool) {
        if self.active.insert(krate.to_string()) {
            let required: Vec<String> = self.manifests[krate]
                .edges
                .iter()
                .filter(|edge| !edge.optional)
                .map(|edge| edge.dependency.clone())
                .collect();
            for dependency in required {
                self.follow(krate, &dependency);
            }
        }
        if default_features {
            self.enable(krate, "default");
        }
    }

    /// Activate every production edge of `krate` to `dependency`.
    fn follow(&mut self, krate: &str, dependency: &str) {
        let edges: Vec<(bool, Vec<String>)> = self.manifests[krate]
            .edges
            .iter()
            .filter(|edge| edge.dependency == dependency)
            .map(|edge| (edge.default_features, edge.features.clone()))
            .collect();
        for (default_features, features) in edges {
            self.activate(dependency, default_features);
            for feature in features {
                self.enable(dependency, &feature);
            }
        }
    }

    fn enable(&mut self, krate: &str, feature: &str) {
        if !self.enabled.insert((krate.to_string(), feature.to_string())) {
            return;
        }
        let entries = self.manifests[krate]
            .features
            .get(feature)
            .cloned()
            .unwrap_or_default();
        for entry in entries {
            self.enable_entry(krate, &entry);
        }
    }

    /// One entry of a feature list: `dep:x`, `x/f`, `x?/f`, or a local feature.
    fn enable_entry(&mut self, krate: &str, entry: &str) {
        if let Some(dependency) = entry.strip_prefix("dep:") {
            if self.manifests.contains_key(dependency) {
                self.follow(krate, dependency);
            }
            return;
        }
        let Some((dependency, feature)) = entry.split_once('/') else {
            self.enable(krate, entry);
            return;
        };
        let (dependency, weak) = dependency
            .strip_suffix('?')
            .map_or((dependency, false), |dependency| (dependency, true));
        if !self.manifests.contains_key(dependency) {
            return;
        }
        if !weak {
            self.follow(krate, dependency);
        }
        if self.active.contains(dependency) {
            self.enable(dependency, feature);
        }
    }

    fn reference_features(&self) -> Vec<(String, String)> {
        REFERENCE
            .iter()
            .map(|(krate, feature)| (krate.to_string(), feature.to_string()))
            .filter(|enabled| self.enabled.contains(enabled))
            .collect()
    }
}

fn product_closure(feature: &str) -> Closure {
    let mut closure = Closure::new();
    closure.activate("rumoca-runtime-fmi-ls", true);
    closure.enable("rumoca-runtime-fmi-ls", feature);
    closure
}

#[test]
fn the_published_and_certification_guests_enable_no_reference_execution() {
    // The published core builds the product feature; the certifying core
    // builds it with `certify-linked` (the packager runner).
    for extra in [None, Some("certify-linked")] {
        let mut closure = product_closure("scalar-linked-product");
        if let Some(extra) = extra {
            closure.enable("rumoca-runtime-fmi-ls", extra);
        }
        let feature = extra.unwrap_or("scalar-linked-product");
        // The walk reached the compiled guest it guards, not an empty graph.
        for krate in ["rumoca-sim", "rumoca-solver", "rumoca-solver-rk45", "rumoca-eval-solve"] {
            assert!(closure.active.contains(krate), "{feature}: {krate} not reached");
        }
        assert!(
            closure
                .enabled
                .contains(&("rumoca-sim".to_string(), "linked-guest".to_string())),
            "{feature}: the linked guest feature is not reached"
        );
        assert_eq!(
            closure.reference_features(),
            Vec::<(String, String)>::new(),
            "{feature}: the published compiled guest enables reference execution"
        );
    }
}

#[test]
fn the_walk_finds_reference_execution_where_a_host_feature_enables_it() {
    let mut closure = Closure::new();
    closure.activate("rumoca-sim", true);
    let found = closure.reference_features();
    for expected in REFERENCE {
        assert!(
            found.contains(&(expected.0.to_string(), expected.1.to_string())),
            "the host simulation default must reach {expected:?}"
        );
    }
}
