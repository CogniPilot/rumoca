use crate::path_utils::{normalized_top_level_names, path_is_in_top_level_set};
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;
use std::collections::HashSet;

/// Compute overconstrained balance correction per MLS §4.8 Definition 4.3.
///
/// For over-determined connectors, each spanning tree without any root node
/// adds the difference between the size of the over-determined type or record
/// and the size of the output of `equalityConstraint`.
///
/// For example, `Orientation` has `total_scalar_size=12` (T[3,3]=9 + w[3]=3)
/// and `equalityConstraint` returns `Real[3]` (size=3), so each rootless
/// spanning tree contributes `12 - 3 = 9` to the interface equation count.
///
/// This function groups OC variables by their `oc_record_path`, builds the
/// virtual connection graph from `flat.branches`, identifies roots from
/// `flat.definite_roots` and `flat.potential_roots`, and computes the
/// correction for connected components that touch top-level connectors
/// and have no root.
pub(crate) fn count_overconstrained_interface(
    flat: &flat::Model,
    state_vars: &HashSet<flat::VarName>,
) -> i64 {
    let defined_lhs_paths = collect_nonconnection_lhs_paths(flat);

    // Step 1: Collect OC record groups from flat.variables.
    // Group by oc_record_path -> compute total_scalar_size per group.
    let mut record_groups: FxHashMap<String, OcRecordGroup> = FxHashMap::default();
    for (name, var) in &flat.variables {
        if !var.is_overconstrained {
            continue;
        }
        let Some(ref rec_path) = var.oc_record_path else {
            continue;
        };
        // Only count continuous unknowns (not params/constants)
        if !crate::is_continuous_unknown(flat, state_vars, name) {
            continue;
        }
        let group = record_groups
            .entry(rec_path.clone())
            .or_insert_with(|| OcRecordGroup {
                total_scalar_size: 0,
                eq_constraint_size: var.oc_eq_constraint_size.unwrap_or(3),
                has_internal_definition: defined_lhs_paths
                    .iter()
                    .any(|lhs| is_same_or_child(lhs, rec_path)),
            });
        // Each variable contributes its scalar size (product of dims, min 1)
        let scalar_size = if var.dims.is_empty() {
            1usize
        } else {
            var.dims.iter().map(|&d| d.max(1) as usize).product()
        };
        group.total_scalar_size += scalar_size;
    }

    if record_groups.is_empty() {
        return 0;
    }

    // Step 2: Identify which record paths are in top-level connectors.
    let normalized_top_level_connectors =
        normalized_top_level_names(flat.top_level_connectors.iter());
    let top_level_records: HashSet<&str> = record_groups
        .keys()
        .filter(|rec_path| path_is_in_top_level_set(rec_path, &normalized_top_level_connectors))
        .map(|s| s.as_str())
        .collect();

    if top_level_records.is_empty() {
        return 0;
    }

    // Step 3: Build VCG connected components from record paths, required branches,
    // and optional edges derived from connect() statements.
    let record_paths: Vec<&str> = record_groups.keys().map(|s| s.as_str()).collect();
    let (comp_of, n_comps) =
        build_record_components(&record_paths, &flat.branches, &flat.optional_edges);

    // Step 4: Determine roots per component.
    let mut has_root = vec![false; n_comps];
    let mut has_top_level = vec![false; n_comps];

    for (rec_path, &comp_id) in &comp_of {
        // Check if this record path is in a top-level connector
        if top_level_records.contains(rec_path) {
            has_top_level[comp_id] = true;
        }
        // Check definite roots
        if flat.definite_roots.contains(*rec_path) {
            has_root[comp_id] = true;
        }
        // Check if a parent connector is a definite root
        // e.g., if definite_roots has "frame_a" and rec_path is "frame_a.R"
        for root in &flat.definite_roots {
            if rec_path.starts_with(root.as_str())
                && rec_path.as_bytes().get(root.len()) == Some(&b'.')
            {
                has_root[comp_id] = true;
            }
        }
    }

    // Check potential roots for components without definite roots
    for (pot_root, _priority) in &flat.potential_roots {
        // Find which component this potential root belongs to
        for (rec_path, &comp_id) in &comp_of {
            let matches = rec_path.starts_with(pot_root.as_str())
                || pot_root.starts_with(*rec_path)
                || crate::path_utils::top_level_segment(rec_path)
                    .is_some_and(|prefix| pot_root.starts_with(prefix));
            if matches && !has_root[comp_id] {
                // Select this potential root (first one encountered per component)
                has_root[comp_id] = true;
            }
        }
    }

    // Step 5: For each component that touches a top-level connector OC record
    // and has NO root, add (total_scalar_size - eq_constraint_size).
    let mut correction: i64 = 0;
    for comp_id in 0..n_comps {
        if !has_top_level[comp_id] || has_root[comp_id] {
            continue;
        }
        // Sum the correction only for OC records that are part of the model's
        // top-level connector interface. Internal records in the same connected
        // component are constrained by internal model equations and must not be
        // charged to the external interface balance correction.
        for (rec_path, group) in &record_groups {
            if comp_of.get(rec_path.as_str()) != Some(&comp_id)
                || group.has_internal_definition
                || !top_level_records.contains(rec_path.as_str())
            {
                continue;
            }
            correction += (group.total_scalar_size as i64) - (group.eq_constraint_size as i64);
        }
    }

    correction
}

/// Build connected components from record paths using VCG branches.
/// Returns (path -> component_id, number_of_components).
pub(crate) fn build_record_components<'a>(
    record_paths: &[&'a str],
    branches: &[(String, String)],
    optional_edges: &[(String, String)],
) -> (FxHashMap<&'a str, usize>, usize) {
    let mut comp_of: FxHashMap<&str, usize> = FxHashMap::default();
    let mut n_comps = 0;

    // Initialize: each record path gets its own component
    for &path in record_paths {
        comp_of.insert(path, n_comps);
        n_comps += 1;
    }

    // Merge components connected by branches.
    // A branch (a, b) connects record paths that start with a or b.
    for (branch_a, branch_b) in branches.iter().chain(optional_edges.iter()) {
        let comp_a = find_component_for_vcg_node(branch_a, record_paths, &comp_of);
        let comp_b = find_component_for_vcg_node(branch_b, record_paths, &comp_of);

        if let (Some(ca), Some(cb)) = (comp_a, comp_b)
            && ca != cb
        {
            // Union: relabel all with comp cb to comp ca
            let (keep, replace) = (ca.min(cb), ca.max(cb));
            relabel_component_ids(&mut comp_of, keep, replace);
        }
    }

    (comp_of, n_comps)
}

/// Information about an overconstrained record group.
struct OcRecordGroup {
    /// Total scalar size of all continuous unknowns in this OC record.
    total_scalar_size: usize,
    /// Output size of the equalityConstraint function.
    eq_constraint_size: usize,
    /// True if this OC record path is already defined by non-connection equations.
    has_internal_definition: bool,
}

/// Collect LHS variable paths from non-connection equations.
/// These represent internal definitions and should not trigger interface OC correction.
fn collect_nonconnection_lhs_paths(flat: &flat::Model) -> HashSet<String> {
    let mut lhs_paths = HashSet::default();

    for eq in &flat.equations {
        if matches!(
            &eq.origin,
            flat::EquationOrigin::Connection { .. }
                | flat::EquationOrigin::FlowSum { .. }
                | flat::EquationOrigin::UnconnectedFlow { .. }
        ) {
            continue;
        }

        let flat::Expression::Binary { op, lhs, .. } = &eq.residual else {
            continue;
        };
        if !matches!(op, rumoca_ir_ast::OpBinary::Sub(_)) {
            continue;
        }
        if let flat::Expression::VarRef { name, .. } = lhs.as_ref() {
            lhs_paths.insert(name.as_str().to_string());
        }
    }

    lhs_paths
}

fn is_same_or_child(path: &str, base: &str) -> bool {
    path == base
        || path
            .strip_prefix(base)
            .is_some_and(|suffix| suffix.starts_with('.'))
}

fn vcg_node_matches_record_path(node: &str, record_path: &str) -> bool {
    is_same_or_child(record_path, node) || is_same_or_child(node, record_path)
}

fn find_component_for_vcg_node<'a>(
    node: &str,
    record_paths: &[&'a str],
    comp_of: &FxHashMap<&'a str, usize>,
) -> Option<usize> {
    // Prefer exact/ancestor matches first.
    if let Some(path) = record_paths
        .iter()
        .find(|p| vcg_node_matches_record_path(node, p))
    {
        return comp_of.get(path).copied();
    }

    None
}

fn relabel_component_ids(comp_of: &mut FxHashMap<&str, usize>, keep: usize, replace: usize) {
    for v in comp_of.values_mut() {
        if *v == replace {
            *v = keep;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_ast as ast;
    use rumoca_ir_flat as flat;
    use std::collections::HashSet;

    fn add_oc_record(flat: &mut flat::Model, rec_path: &str) {
        for (suffix, dims) in [("T", vec![3, 3]), ("w", vec![3])] {
            let name = format!("{rec_path}.{suffix}");
            flat.add_variable(
                flat::VarName::new(&name),
                flat::Variable {
                    name: flat::VarName::new(&name),
                    dims,
                    variability: ast::Variability::Empty,
                    is_primitive: true,
                    is_overconstrained: true,
                    oc_record_path: Some(rec_path.to_string()),
                    oc_eq_constraint_size: Some(3),
                    ..Default::default()
                },
            );
        }
    }

    #[test]
    fn test_build_record_components_does_not_merge_unrelated_world_prefix() {
        let record_paths = vec!["world.x_label.R", "frame_a.R", "world.frame_b.R"];
        let branches: Vec<(String, String)> = Vec::new();
        let optional_edges = vec![("frame_a.R".to_string(), "world.frame_b.R".to_string())];

        let (comp_of, _n_comps) =
            build_record_components(&record_paths, &branches, &optional_edges);

        let frame_comp = comp_of["frame_a.R"];
        let world_frame_b_comp = comp_of["world.frame_b.R"];
        let world_label_comp = comp_of["world.x_label.R"];

        assert_eq!(frame_comp, world_frame_b_comp);
        assert_ne!(frame_comp, world_label_comp);
    }

    #[test]
    fn test_potential_root_prefix_matching_ignores_dot_inside_brackets() {
        let mut flat = flat::Model::new();
        flat.top_level_connectors
            .insert("bus[data.other]".to_string());
        add_oc_record(&mut flat, "bus[data.other].R");
        flat.potential_roots
            .push(("bus[data.medium].anchor".to_string(), 0));

        let state_vars: HashSet<flat::VarName> = HashSet::default();
        let correction = count_overconstrained_interface(&flat, &state_vars);
        assert_eq!(
            correction, 9,
            "mismatched bracket expression must not be treated as a potential-root match"
        );
    }

    #[test]
    fn test_potential_root_prefix_matching_handles_dot_inside_brackets() {
        let mut flat = flat::Model::new();
        flat.top_level_connectors
            .insert("bus[data.medium]".to_string());
        add_oc_record(&mut flat, "bus[data.medium].R");
        flat.potential_roots
            .push(("bus[data.medium].anchor".to_string(), 0));

        let state_vars: HashSet<flat::VarName> = HashSet::default();
        let correction = count_overconstrained_interface(&flat, &state_vars);
        assert_eq!(
            correction, 0,
            "matching bracket expression should mark component rooted"
        );
    }
}
