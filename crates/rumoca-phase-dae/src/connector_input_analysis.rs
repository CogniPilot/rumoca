//! Helpers for classifying top-level connector members as external inputs.

use super::{collect_continuous_equation_lhs, is_internal_input};
use crate::ToDaeError;
use crate::path_utils::{
    is_top_level_member, normalized_top_level_names, subscript_fallback_chain,
};
use indexmap::IndexSet;
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;
use std::collections::{HashSet, VecDeque};

/// Normalize a connection variable name for flat.variables lookup.
///
/// Connection equations may use element names (e.g. `bus.ref[1]`) while the flat
/// variable map stores the base (`bus.ref`). This maps to the base when available.
fn normalize_connection_var_name(name: &str, flat: &flat::Model) -> rumoca_core::VarName {
    let lookup_name = rumoca_core::VarName::new(name);
    if let Some((key, _)) = flat.variables.get_key_value(&lookup_name) {
        return key.clone();
    }
    for candidate in subscript_fallback_chain(name) {
        if let Some((key, _)) = flat.variables.get_key_value(&candidate) {
            return key.clone();
        }
    }
    lookup_name
}

fn peer_is_internal_input_with_fallback(
    peer: &rumoca_core::VarName,
    flat: &flat::Model,
) -> Result<bool, ToDaeError> {
    if is_internal_input(peer, flat)? {
        return Ok(true);
    }
    for candidate in subscript_fallback_chain(peer.as_str()) {
        if is_internal_input(&candidate, flat)? {
            return Ok(true);
        }
    }
    Ok(false)
}

/// Build connected components for the graph induced by connection equations.
fn build_connection_components(
    peers: &FxHashMap<rumoca_core::VarName, HashSet<rumoca_core::VarName>>,
) -> FxHashMap<rumoca_core::VarName, usize> {
    let mut comp_of: FxHashMap<rumoca_core::VarName, usize> = FxHashMap::default();
    let mut queue = VecDeque::new();
    let mut next_comp = 0usize;

    for node in peers.keys() {
        if comp_of.contains_key(node) {
            continue;
        }

        comp_of.insert(node.clone(), next_comp);
        queue.push_back(node.clone());

        while let Some(curr) = queue.pop_front() {
            let new_neighbors: Vec<rumoca_core::VarName> = peers
                .get(&curr)
                .into_iter()
                .flat_map(|neighbors| neighbors.iter())
                .filter(|neigh| !comp_of.contains_key(*neigh))
                .cloned()
                .collect();
            for neigh in new_neighbors {
                comp_of.insert(neigh.clone(), next_comp);
                queue.push_back(neigh);
            }
        }

        next_comp += 1;
    }

    comp_of
}

/// Find top-level connector members that should behave as external inputs.
///
/// For connector fields without explicit causality (common in bus connectors), a field
/// connected only to internal input pins is an external value source and should not
/// be treated as an algebraic unknown.
pub(super) fn find_top_level_connector_input_members(
    flat: &flat::Model,
    state_vars: &IndexSet<rumoca_core::VarName>,
) -> Result<HashSet<rumoca_core::VarName>, ToDaeError> {
    let normalized_top_level_connectors =
        normalized_top_level_names(flat.top_level_connectors.iter());
    let mut peers: FxHashMap<rumoca_core::VarName, HashSet<rumoca_core::VarName>> =
        FxHashMap::default();

    for eq in flat.equations.iter().filter(|eq| eq.origin.is_connection()) {
        let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_core::OpBinary::Sub) {
            continue;
        }
        let (
            rumoca_core::Expression::VarRef { name: lhs_name, .. },
            rumoca_core::Expression::VarRef { name: rhs_name, .. },
        ) = (lhs.as_ref(), rhs.as_ref())
        else {
            continue;
        };

        let lhs_n = normalize_connection_var_name(lhs_name.as_str(), flat);
        let rhs_n = normalize_connection_var_name(rhs_name.as_str(), flat);
        peers
            .entry(lhs_n.clone())
            .or_default()
            .insert(rhs_n.clone());
        peers.entry(rhs_n).or_default().insert(lhs_n);
    }

    let comp_of = build_connection_components(&peers);
    let (directly_defined, _) = collect_continuous_equation_lhs(flat);

    let n_components = comp_of
        .values()
        .copied()
        .max()
        .map_or(0usize, |max_id| max_id + 1);
    let mut component_has_internal_anchor = vec![false; n_components];
    for (name, comp_id) in &comp_of {
        if is_top_level_member(name, &normalized_top_level_connectors) {
            continue;
        }
        let Some(var) = flat.variables.get(name) else {
            continue;
        };
        let has_anchor = directly_defined.contains(name)
            || var.binding.is_some()
            || state_vars.contains(name)
            || matches!(&var.causality, rumoca_core::Causality::Output(_));
        if has_anchor {
            component_has_internal_anchor[*comp_id] = true;
        }
    }

    let mut result = HashSet::default();
    for (name, var) in &flat.variables {
        if !var.is_primitive || var.flow || var.stream {
            continue;
        }
        if !is_top_level_member(name, &normalized_top_level_connectors) {
            continue;
        }
        let Some(connected_peers) = peers.get(name) else {
            continue;
        };
        let Some(comp_id) = comp_of.get(name).copied() else {
            continue;
        };

        // If an expandable top-level connector member is in a rootless/unanchored
        // connection component, it behaves as an external interface value and should
        // not be treated as an unknown.
        let is_unanchored_expandable_interface = var.from_expandable_connector
            && matches!(&var.causality, rumoca_core::Causality::Empty)
            && !component_has_internal_anchor[comp_id];
        if is_unanchored_expandable_interface {
            result.insert(name.clone());
            continue;
        }

        let mut saw_internal_input_peer = false;
        let mut all_non_top_peers_are_internal_inputs = true;
        for peer in connected_peers {
            if is_top_level_member(peer, &normalized_top_level_connectors) {
                continue;
            }
            if peer_is_internal_input_with_fallback(peer, flat)? {
                saw_internal_input_peer = true;
                continue;
            }
            all_non_top_peers_are_internal_inputs = false;
            break;
        }

        // If the connection component already has an internal anchor (output, binding,
        // state, or equation-defined var), keep this member as an internal unknown.
        if saw_internal_input_peer
            && all_non_top_peers_are_internal_inputs
            && !component_has_internal_anchor[comp_id]
        {
            result.insert(name.clone());
        }
    }

    Ok(result)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_build_connection_components_groups_disconnected_subgraphs() {
        let a = rumoca_core::VarName::new("a");
        let b = rumoca_core::VarName::new("b");
        let c = rumoca_core::VarName::new("c");
        let d = rumoca_core::VarName::new("d");

        let mut peers: FxHashMap<rumoca_core::VarName, HashSet<rumoca_core::VarName>> =
            FxHashMap::default();
        peers.entry(a.clone()).or_default().insert(b.clone());
        peers.entry(b.clone()).or_default().insert(a.clone());
        peers.entry(c.clone()).or_default().insert(d.clone());
        peers.entry(d.clone()).or_default().insert(c.clone());

        let comp_of = build_connection_components(&peers);

        assert_eq!(comp_of.get(&a), comp_of.get(&b));
        assert_eq!(comp_of.get(&c), comp_of.get(&d));
        assert_ne!(comp_of.get(&a), comp_of.get(&c));
    }

    #[test]
    fn test_normalize_connection_var_name_peels_multiple_subscript_layers() {
        let mut flat = flat::Model::new();
        flat.add_variable(
            rumoca_core::VarName::new("a.b.c"),
            flat::Variable {
                name: rumoca_core::VarName::new("a.b.c"),
                variability: rumoca_core::Variability::Empty,
                causality: rumoca_core::Causality::Input(Default::default()),
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let normalized = normalize_connection_var_name("a[1].b[2].c[3]", &flat);
        assert_eq!(normalized, rumoca_core::VarName::new("a.b.c"));
    }

    #[test]
    fn test_peer_is_internal_input_with_fallback_handles_multiple_layers() {
        let mut flat = flat::Model::new();
        flat.add_variable(
            rumoca_core::VarName::new("bus.signal"),
            flat::Variable {
                name: rumoca_core::VarName::new("bus.signal"),
                variability: rumoca_core::Variability::Empty,
                causality: rumoca_core::Causality::Input(Default::default()),
                ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        assert!(
            peer_is_internal_input_with_fallback(
                &rumoca_core::VarName::new("bus[1].signal[2]"),
                &flat
            )
            .expect("internal input fallback should not fail")
        );
    }
}
