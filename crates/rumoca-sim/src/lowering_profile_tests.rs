//! The fixed linked guest never lowers a model, so its features never reach
//! `lowering` (rumoca-phase-solve and its translation-time evaluation); every
//! feature that simulates, exports or packages from a model does.
//!
//! A static check of this crate's own feature table. The guest's cross-crate
//! closure is the rumoca-runtime-fmi-ls profile walk, and a name census of the
//! built cores remains the build-time proof.

use std::collections::{BTreeMap, BTreeSet};

const MANIFEST: &str = include_str!("../Cargo.toml");

/// Features that consume a model: each must reach `lowering`.
const MODEL_CONSUMERS: [&str; 6] = [
    "native-solvers",
    "wasm-solvers",
    "solver-diffsol",
    "native-fmi-ls",
    "native-fmi-ls-build",
    "scheduled-sim",
];

fn manifest() -> toml::Table {
    MANIFEST.parse().expect("the rumoca-sim manifest parses")
}

fn features(manifest: &toml::Table) -> BTreeMap<String, Vec<String>> {
    manifest["features"]
        .as_table()
        .expect("the manifest has a feature table")
        .iter()
        .map(|(name, items)| {
            let items = items
                .as_array()
                .expect("a feature is a list")
                .iter()
                .map(|item| {
                    item.as_str()
                        .expect("a feature item is a string")
                        .to_string()
                })
                .collect();
            (name.clone(), items)
        })
        .collect()
}

/// Every feature `root` enables in this crate, and every non-local item
/// (`dep:`, `crate/feature`, `crate?/feature`) those features name.
fn closure(features: &BTreeMap<String, Vec<String>>, root: &str) -> BTreeSet<String> {
    let mut reached = BTreeSet::new();
    let mut pending = vec![root.to_string()];
    while let Some(name) = pending.pop() {
        if !reached.insert(name.clone()) {
            continue;
        }
        for item in features.get(&name).into_iter().flatten() {
            if features.contains_key(item) {
                pending.push(item.clone());
            } else {
                reached.insert(item.clone());
            }
        }
    }
    reached
}

/// A weak `rumoca-phase-solve?/feature` only forwards a feature to an already
/// enabled dependency, so it does not reach lowering.
fn reaches_lowering(reached: &BTreeSet<String>) -> bool {
    reached.iter().any(|item| {
        item == "lowering"
            || item == "dep:rumoca-phase-solve"
            || item.starts_with("rumoca-phase-solve/")
    })
}

#[test]
fn the_linked_guest_never_reaches_lowering() {
    let reached = closure(&features(&manifest()), "linked-guest");
    assert!(
        !reaches_lowering(&reached),
        "linked-guest reaches lowering: {reached:?}"
    );
}

#[test]
fn phase_solve_is_an_optional_dependency() {
    let manifest = manifest();
    let edge = &manifest["dependencies"]["rumoca-phase-solve"];
    assert_eq!(
        edge.get("optional").and_then(toml::Value::as_bool),
        Some(true),
        "rumoca-phase-solve must be optional so only `lowering` links it"
    );
}

#[test]
fn every_model_consuming_feature_reaches_lowering() {
    let features = features(&manifest());
    for consumer in MODEL_CONSUMERS {
        assert!(
            reaches_lowering(&closure(&features, consumer)),
            "{consumer} consumes a model but does not reach lowering"
        );
    }
}

/// Mutation control: the walk sees lowering reached transitively, through a
/// host feature the guest does not enable today.
#[test]
fn the_guest_check_sees_a_transitive_lowering_edge() {
    let mut features = features(&manifest());
    features
        .get_mut("linked-guest")
        .expect("the guest feature exists")
        .push("solver-diffsol".to_string());
    assert!(reaches_lowering(&closure(&features, "linked-guest")));
}
