//! The published compiled guest enables no reference (interpreted) execution.
//!
//! A bounded static walk of every workspace manifest: from the published and
//! certification product features through each production dependency edge a
//! `wasm32-wasip2` library build activates. Only workspace crates can enable
//! the reference features, so the walk covers every crate that can. It applies
//! Cargo's workspace default-feature inheritance. It skips dev-dependencies
//! (including this workspace's self edges) and build-dependencies, which a
//! non-test target build does not unify under resolver 2. It guards the
//! feature graph only: `cargo tree -e features` over the real resolve and a
//! name census of the built cores remain the build proof.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

/// The features that compile reference execution into a crate.
const REFERENCE: [(&str, &str); 3] = [
    ("rumoca-eval-solve", "reference-eval"),
    ("rumoca-solver", "reference-execution"),
    ("rumoca-sim", "reference-execution"),
];

/// The manifests the first static check whitelisted; the walk must reach
/// beyond them. Model lowering (`rumoca-phase-solve`) left the guest with the
/// optional `lowering` feature, so it is listed but never reached.
const FIRST_WHITELIST: [&str; 7] = [
    "rumoca-runtime-fmi-ls",
    "rumoca-sim",
    "rumoca-exec-wasm",
    "rumoca-solver",
    "rumoca-solver-rk45",
    "rumoca-eval-solve",
    "rumoca-phase-solve",
];

/// Translation-only crates the published guest never links.
const TRANSLATION_ONLY: [&str; 1] = ["rumoca-phase-solve"];

const EVAL_REFERENCE_EDGE: &str =
    "rumoca-eval-solve = { workspace = true, features = [\"reference-eval\"] }";

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

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

/// The workspace manifest and every member manifest, keyed by package name.
fn read_workspace() -> (String, BTreeMap<String, String>) {
    let root = workspace_root();
    let workspace = std::fs::read_to_string(root.join("Cargo.toml")).expect("workspace manifest");
    let mut members = BTreeMap::new();
    for entry in std::fs::read_dir(root.join("crates")).expect("crates directory") {
        let path = entry.expect("crate entry").path().join("Cargo.toml");
        if let Ok(text) = std::fs::read_to_string(&path) {
            members.insert(package_name(&text), text);
        }
    }
    (workspace, members)
}

fn package_name(manifest: &str) -> String {
    let package = manifest
        .split("[package]")
        .nth(1)
        .expect("a [package] table");
    let line = package
        .lines()
        .find(|line| line.trim_start().starts_with("name = "))
        .expect("a package name");
    quoted(line).remove(0)
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

fn cfg_call<'a>(predicate: &'a str, name: &str) -> Option<&'a str> {
    predicate
        .strip_prefix(name)?
        .trim_start()
        .strip_prefix('(')?
        .strip_suffix(')')
}

fn cfg_arguments(inner: &str) -> Vec<&str> {
    let mut arguments = Vec::new();
    let (mut depth, mut start) = (0usize, 0);
    for (index, character) in inner.char_indices() {
        match character {
            '(' => depth += 1,
            ')' => depth -= 1,
            ',' if depth == 0 => {
                arguments.push(inner[start..index].trim());
                start = index + 1;
            }
            _ => {}
        }
    }
    arguments.push(inner[start..].trim());
    arguments
}

/// The workspace's `cfg(..)` target predicates, evaluated for `wasm32-wasip2`.
/// An unrecognized predicate fails the check instead of guessing.
fn cfg_holds(predicate: &str) -> bool {
    let predicate = predicate.trim();
    if let Some(inner) = cfg_call(predicate, "all") {
        return cfg_arguments(inner).into_iter().all(cfg_holds);
    }
    if let Some(inner) = cfg_call(predicate, "any") {
        return cfg_arguments(inner).into_iter().any(cfg_holds);
    }
    if let Some(inner) = cfg_call(predicate, "not") {
        return !cfg_holds(inner);
    }
    let Some((key, value)) = predicate.split_once('=') else {
        return match predicate {
            "unix" | "windows" | "kani" | "test" => false,
            other => panic!("unrecognized cfg predicate `{other}`"),
        };
    };
    let value = quoted(value).remove(0);
    match key.trim() {
        "target_arch" => value == "wasm32",
        "target_os" => value == "wasi",
        "target_env" => value == "p2",
        "target_family" => value == "wasm",
        other => panic!("unrecognized cfg key `{other}`"),
    }
}

/// Whether a dependency table under `section` is in a `wasm32-wasip2`
/// production build. Dev- and build-dependency tables never are.
fn production_section(section: &str) -> bool {
    if section == "[dependencies]" {
        return true;
    }
    section
        .strip_prefix("[target.'cfg(")
        .and_then(|rest| rest.strip_suffix(")'.dependencies]"))
        .is_some_and(cfg_holds)
}

fn workspace_default_features(workspace: &str, dependency: &str) -> bool {
    let prefix = format!("{dependency} = ");
    workspace
        .lines()
        .find(|line| line.starts_with(&prefix))
        .is_none_or(|line| !line.contains("default-features = false"))
}

/// Complete logical entries: an array or inline table that spans lines is
/// joined into one entry.
fn entries(text: &str) -> Vec<String> {
    let mut joined = Vec::new();
    let mut open = String::new();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        open.push_str(line);
        let depth = open.matches(['[', '{']).count();
        if depth <= open.matches([']', '}']).count() || open.starts_with('[') {
            joined.push(std::mem::take(&mut open));
        }
    }
    joined
}

fn parse(workspace: &str, names: &BTreeSet<String>, text: &str) -> Manifest {
    let mut features = BTreeMap::new();
    let mut edges = Vec::new();
    let mut section = String::new();
    for entry in entries(text) {
        if entry.starts_with('[') {
            section = entry;
            continue;
        }
        let Some((key, value)) = entry.split_once('=') else {
            continue;
        };
        let (key, value) = (key.trim(), value.trim());
        if section == "[features]" {
            features.insert(key.to_string(), quoted(value));
        } else if production_section(&section) && names.contains(key) {
            let default_features = if value.contains("workspace = true") {
                workspace_default_features(workspace, key)
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
    fn new(workspace: &str, members: &BTreeMap<String, String>) -> Self {
        let names = members.keys().cloned().collect();
        let manifests = members
            .iter()
            .map(|(name, text)| (name.clone(), parse(workspace, &names, text)))
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

/// The guest closure: the published core (`scalar-linked-product`), or the
/// certifying core (with `certify-linked`, as the packager runner builds it).
fn guest_closure(
    workspace: &str,
    members: &BTreeMap<String, String>,
    certify: bool,
) -> Closure {
    let mut closure = Closure::new(workspace, members);
    closure.activate("rumoca-runtime-fmi-ls", true);
    closure.enable("rumoca-runtime-fmi-ls", "scalar-linked-product");
    if certify {
        closure.enable("rumoca-runtime-fmi-ls", "certify-linked");
    }
    closure
}

/// A crate the guest reaches only beyond the first whitelist.
fn crate_beyond_first_whitelist(closure: &Closure) -> String {
    closure
        .active
        .iter()
        .find(|krate| !FIRST_WHITELIST.contains(&krate.as_str()))
        .cloned()
        .expect("the guest reaches a crate beyond the first whitelist")
}

fn insert_production_edge(members: &mut BTreeMap<String, String>, krate: &str, edge: &str) {
    let manifest = members.get_mut(krate).expect("a workspace member");
    assert!(manifest.contains("\n[dependencies]\n"), "{krate} has no [dependencies]");
    *manifest = manifest.replacen("\n[dependencies]\n", &format!("\n[dependencies]\n{edge}\n"), 1);
}

#[test]
fn the_published_and_certification_guests_enable_no_reference_execution() {
    let (workspace, members) = read_workspace();
    for certify in [false, true] {
        let closure = guest_closure(&workspace, &members, certify);
        // The walk reached the compiled guest it guards, not an empty graph.
        for krate in FIRST_WHITELIST.iter().filter(|krate| !TRANSLATION_ONLY.contains(*krate)) {
            assert!(closure.active.contains(*krate), "certify={certify}: {krate} not reached");
        }
        for krate in TRANSLATION_ONLY {
            assert!(
                !closure.active.contains(krate),
                "certify={certify}: the guest links translation-only {krate}"
            );
        }
        crate_beyond_first_whitelist(&closure);
        assert!(
            closure
                .enabled
                .contains(&("rumoca-sim".to_string(), "linked-guest".to_string())),
            "certify={certify}: the linked guest feature is not reached"
        );
        assert_eq!(
            closure.reference_features(),
            Vec::<(String, String)>::new(),
            "certify={certify}: the published compiled guest enables reference execution"
        );
    }
}

#[test]
fn the_walk_finds_reference_execution_where_a_host_feature_enables_it() {
    let (workspace, members) = read_workspace();
    let mut closure = Closure::new(&workspace, &members);
    closure.activate("rumoca-sim", true);
    let found = closure.reference_features();
    for expected in REFERENCE {
        assert!(
            found.contains(&(expected.0.to_string(), expected.1.to_string())),
            "the host simulation default must reach {expected:?}"
        );
    }
}

#[test]
fn an_edge_from_a_crate_beyond_the_first_whitelist_is_found() {
    let (workspace, mut members) = read_workspace();
    let deep = crate_beyond_first_whitelist(&guest_closure(&workspace, &members, false));
    insert_production_edge(&mut members, &deep, EVAL_REFERENCE_EDGE);
    let found = guest_closure(&workspace, &members, false).reference_features();
    assert!(
        found.contains(&("rumoca-eval-solve".to_string(), "reference-eval".to_string())),
        "an omitted edge from {deep} enabling reference-eval must be found"
    );
}

#[test]
fn dev_self_edges_are_skipped_and_inherited_defaults_are_followed() {
    let (workspace, mut members) = read_workspace();
    // The solver's own tests opt in through a dev self edge, which a non-test
    // build of the guest does not unify.
    assert!(
        members["rumoca-solver"]
            .contains("rumoca-solver = { path = \".\", features = [\"reference-execution\"] }"),
        "the solver's test-only self edge is the one under check"
    );
    // The guest reaches the solver through inherited-default edges
    // (solver-rk45), so a default that carried reference execution is found.
    let manifest = members.get_mut("rumoca-solver").expect("the solver manifest");
    let default = "default = [\"sparse-linalg\"]";
    assert!(manifest.contains(default), "the solver default under check");
    *manifest = manifest.replacen(
        default,
        "default = [\"sparse-linalg\", \"reference-execution\"]",
        1,
    );
    let found = guest_closure(&workspace, &members, false).reference_features();
    assert!(
        found.contains(&("rumoca-solver".to_string(), "reference-execution".to_string())),
        "a reference default inherited by the guest must be found"
    );
}

/// Whether `krate` with `feature` (its default when `None`) runs the solver
/// without reference execution, the policy the host and browser rely on.
fn solver_without_reference(
    workspace: &str,
    members: &BTreeMap<String, String>,
    krate: &str,
    feature: Option<&str>,
) -> bool {
    let mut closure = Closure::new(workspace, members);
    closure.activate(krate, true);
    if let Some(feature) = feature {
        closure.enable(krate, feature);
    }
    closure.active.contains("rumoca-solver")
        && !closure
            .enabled
            .contains(&("rumoca-solver".to_string(), "reference-execution".to_string()))
}

/// Every host crate and feature outside the guest closure that would run the
/// solver without reference execution.
fn host_reference_gaps(workspace: &str, members: &BTreeMap<String, String>) -> Vec<String> {
    let guest = guest_closure(workspace, members, true).active;
    let names: BTreeSet<String> = members.keys().cloned().collect();
    let mut gaps = Vec::new();
    for (krate, text) in members.iter().filter(|(krate, _)| !guest.contains(*krate)) {
        let features = parse(workspace, &names, text).features;
        let selections = std::iter::once(None)
            .chain(features.keys().filter(|feature| *feature != "default").map(Some));
        for feature in selections {
            let feature = feature.map(String::as_str);
            if solver_without_reference(workspace, members, krate, feature) {
                gaps.push(format!("{krate}[{}]", feature.unwrap_or("default")));
            }
        }
    }
    gaps
}

#[test]
fn every_host_feature_that_reaches_the_solver_enables_reference_execution() {
    let (workspace, members) = read_workspace();
    assert_eq!(
        host_reference_gaps(&workspace, &members),
        Vec::<String>::new(),
        "host features would run the solver with its reference execution excluded"
    );
}

#[test]
fn a_host_feature_without_the_opt_in_is_found() {
    let (workspace, mut members) = read_workspace();
    let manifest = members.get_mut("rumoca-bind-wasm").expect("the browser binding manifest");
    let opt_in = "\"reference-execution\", ";
    assert!(manifest.contains(opt_in), "the browser binding opt-in under check");
    *manifest = manifest.replace(opt_in, "");
    let gaps = host_reference_gaps(&workspace, &members);
    assert!(
        gaps.iter().any(|gap| gap == "rumoca-bind-wasm[sim-rk45]"),
        "the browser simulation without its opt-in must be found: {gaps:?}"
    );
}
