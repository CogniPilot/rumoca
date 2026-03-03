//! Virtual Connection Graph spanning tree algorithm (MLS §9.4).
//!
//! This module implements the VCG algorithm for overconstrained connectors:
//! - Pre-scans equations for `Connections.branch/root/potentialRoot` calls
//! - Derives optional edges from `connect()` statements on overconstrained connectors
//! - Builds a spanning tree per connected component
//! - Provides `isRoot(N)` and `rooted(N)` query results for boolean evaluation

use rustc_hash::{FxHashMap, FxHashSet};

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

use crate::Context;
use crate::equations::build_qualified_name;
use crate::path_utils::{find_last_top_level_dot, normalize_path_without_indices};

/// Result of the VCG spanning tree computation.
pub struct VcgResult {
    /// For each VCG node path, whether it is the root of its component.
    pub is_root: FxHashMap<String, bool>,
    /// For each VCG node path, whether it is on the "rooted" side
    /// (i.e., has a parent in the spanning tree and the component has a definite root).
    pub rooted: FxHashMap<String, bool>,
}

/// Pre-scanned VCG data from equations before full flattening.
pub struct VcgPreScanData {
    /// Definite roots from `Connections.root(a)`.
    pub definite_roots: FxHashSet<String>,
    /// Branches from `Connections.branch(a, b)` — required edges.
    pub branches: Vec<(String, String)>,
    /// Potential roots from `Connections.potentialRoot(a, priority)`.
    pub potential_roots: Vec<(String, i64)>,
}

/// Pre-scan overlay classes for VCG data (branch/root/potentialRoot calls).
///
/// Iterates over all equations in all class instances, extracting
/// VCG-related function calls. Handles both top-level and for-loop nested calls.
pub fn pre_collect_vcg_data(overlay: &ast::InstanceOverlay, ctx: &Context) -> VcgPreScanData {
    let mut data = VcgPreScanData {
        definite_roots: FxHashSet::default(),
        branches: Vec::new(),
        potential_roots: Vec::new(),
    };

    for (_def_id, class_data) in &overlay.classes {
        if crate::is_in_disabled_component(&class_data.qualified_name, &overlay.disabled_components)
        {
            continue;
        }

        let prefix = &class_data.qualified_name;
        for inst_eq in &class_data.equations {
            collect_vcg_from_equation(&inst_eq.equation, prefix, ctx, &mut data);
        }
    }

    data
}

/// Recursively collect VCG data from an equation.
fn collect_vcg_from_equation(
    eq: &ast::Equation,
    prefix: &ast::QualifiedName,
    ctx: &Context,
    data: &mut VcgPreScanData,
) {
    match eq {
        ast::Equation::FunctionCall { comp, args } => {
            collect_vcg_from_function_call(comp, args, prefix, data);
        }
        ast::Equation::For { indices, equations } => {
            collect_vcg_from_for(indices, equations, prefix, ctx, data);
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            collect_vcg_from_if(cond_blocks, else_block, prefix, ctx, data);
        }
        _ => {}
    }
}

/// Collect VCG data from an if-equation.
///
/// VCG calls like `Connections.root()` and `Connections.potentialRoot()` can appear
/// inside if-equation branches (e.g., `if enforceStates then root else potentialRoot`).
/// We try to evaluate the condition to select the right branch; if we can't, we scan
/// all branches since VCG calls are structural.
fn collect_vcg_from_if(
    cond_blocks: &[rumoca_ir_ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
    ctx: &Context,
    data: &mut VcgPreScanData,
) {
    // Try to evaluate conditions to select the right branch
    for block in cond_blocks {
        let eval =
            crate::boolean_eval::try_eval_boolean_with_ctx_inner(&block.cond, Some(ctx), prefix);
        match eval {
            Some(true) => {
                // This branch is taken — scan it and return
                for eq in &block.eqs {
                    collect_vcg_from_equation(eq, prefix, ctx, data);
                }
                return;
            }
            Some(false) => {
                // This branch is not taken — skip it
                continue;
            }
            None => {
                // Can't evaluate — scan ALL branches conservatively
                scan_all_if_branches(cond_blocks, else_block, prefix, ctx, data);
                return;
            }
        }
    }
    // All conditions were false — use else branch
    if let Some(else_eqs) = else_block {
        for eq in else_eqs {
            collect_vcg_from_equation(eq, prefix, ctx, data);
        }
    }
}

/// Scan all branches of an if-equation when conditions can't be evaluated.
fn scan_all_if_branches(
    cond_blocks: &[rumoca_ir_ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
    ctx: &Context,
    data: &mut VcgPreScanData,
) {
    for block in cond_blocks {
        for eq in &block.eqs {
            collect_vcg_from_equation(eq, prefix, ctx, data);
        }
    }
    if let Some(else_eqs) = else_block {
        for eq in else_eqs {
            collect_vcg_from_equation(eq, prefix, ctx, data);
        }
    }
}

/// Collect VCG data from a Connections.* function call.
fn collect_vcg_from_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    data: &mut VcgPreScanData,
) {
    let Some((parent, func)) = get_connections_func(comp) else {
        return;
    };
    if parent != "Connections" {
        return;
    }

    match func {
        "root" => {
            if let Some(ast::Expression::ComponentReference(cr)) = args.first() {
                data.definite_roots.insert(build_qualified_name(prefix, cr));
            }
        }
        "branch" => extract_branch(args, prefix, data),
        "potentialRoot" => extract_potential_root(args, prefix, data),
        _ => {}
    }
}

/// Extract a branch edge from Connections.branch(a, b) arguments.
fn extract_branch(
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    data: &mut VcgPreScanData,
) {
    if args.len() >= 2
        && let ast::Expression::ComponentReference(cr_a) = &args[0]
        && let ast::Expression::ComponentReference(cr_b) = &args[1]
    {
        let a = build_qualified_name(prefix, cr_a);
        let b = build_qualified_name(prefix, cr_b);
        data.branches.push((a, b));
    }
}

/// Extract a potential root from Connections.potentialRoot(a, priority) arguments.
fn extract_potential_root(
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    data: &mut VcgPreScanData,
) {
    if let Some(ast::Expression::ComponentReference(cr)) = args.first() {
        let priority = extract_priority(args);
        let path = build_qualified_name(prefix, cr);
        data.potential_roots.push((path, priority));
    }
}

/// Expand for-loop indices and collect VCG calls from nested equations.
fn collect_vcg_from_for(
    indices: &[rumoca_ir_ast::ForIndex],
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    ctx: &Context,
    data: &mut VcgPreScanData,
) {
    if indices.is_empty() {
        for eq in equations {
            collect_vcg_from_equation(eq, prefix, ctx, data);
        }
        return;
    }

    let first = &indices[0];
    let remaining = &indices[1..];

    let Ok(index_values) =
        crate::equations::expand_range_indices(ctx, &first.range, prefix, rumoca_core::Span::DUMMY)
    else {
        return;
    };

    let index_name = &first.ident.text;
    for value in index_values {
        let substituted: Vec<ast::Equation> = equations
            .iter()
            .map(|eq| crate::equations::substitute_index_in_equation(eq, index_name, value))
            .collect();
        collect_vcg_from_for(remaining, &substituted, prefix, ctx, data);
    }
}

/// Extract priority from `Connections.potentialRoot(a, priority)`.
/// Default priority is 0 per MLS §9.4.
fn extract_priority(args: &[ast::Expression]) -> i64 {
    if args.len() >= 2
        && let ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } = &args[1]
    {
        return token.text.parse().unwrap_or(0);
    }
    0
}

/// Get (parent, func) from a `Connections.<func>` component reference.
fn get_connections_func(comp: &ast::ComponentReference) -> Option<(&str, &str)> {
    if comp.parts.len() >= 2 {
        let parent = comp.parts.first().map(|p| p.ident.text.as_ref())?;
        let func = comp.parts.last().map(|p| p.ident.text.as_ref())?;
        Some((parent, func))
    } else {
        None
    }
}

/// Derive optional edges from connection statements on overconstrained connectors.
///
/// For each `connect(a, b)` where the connectors have overconstrained descendants,
/// we create an optional edge between the overconstrained records.
/// The overconstrained suffix is derived from the VCG node paths.
pub fn derive_optional_edges(
    overlay: &ast::InstanceOverlay,
    vcg_data: &VcgPreScanData,
) -> Vec<(String, String)> {
    let vcg_nodes = collect_vcg_node_set(vcg_data, overlay);
    if vcg_nodes.is_empty() {
        return Vec::new();
    }
    let node_index = build_vcg_node_index(&vcg_nodes);

    let suffixes = extract_overconstrained_suffixes(&vcg_nodes);
    let mut optional_edges = Vec::new();
    let mut seen_edges: FxHashSet<(String, String)> = FxHashSet::default();

    for (_def_id, class_data) in &overlay.classes {
        if crate::is_in_disabled_component(&class_data.qualified_name, &overlay.disabled_components)
        {
            continue;
        }
        collect_optional_edges_from_connections(
            &class_data.connections,
            &suffixes,
            &node_index,
            &mut optional_edges,
            &mut seen_edges,
        );
    }

    optional_edges
}

/// Collect all VCG node paths from pre-scanned data.
fn collect_vcg_node_set<'a>(
    vcg_data: &'a VcgPreScanData,
    overlay: &'a ast::InstanceOverlay,
) -> FxHashSet<&'a str> {
    let mut nodes: FxHashSet<&'a str> = FxHashSet::default();
    for (a, b) in &vcg_data.branches {
        nodes.insert(a.as_str());
        nodes.insert(b.as_str());
    }
    for root in &vcg_data.definite_roots {
        nodes.insert(root.as_str());
    }
    for (path, _) in &vcg_data.potential_roots {
        nodes.insert(path.as_str());
    }

    // Include all instantiated overconstrained record paths so connect() edges can
    // map through alias connectors that do not appear in branch/root pre-scan data.
    for (_instance_id, component) in &overlay.components {
        if crate::is_in_disabled_component(&component.qualified_name, &overlay.disabled_components)
        {
            continue;
        }
        if let Some(path) = &component.oc_record_path {
            nodes.insert(path.as_str());
        }
    }
    nodes
}

/// Fast lookup index for VCG node path resolution.
struct VcgNodeIndex<'a> {
    exact: FxHashSet<&'a str>,
    by_normalized: FxHashMap<String, Vec<&'a str>>,
}

fn build_vcg_node_index<'a>(vcg_nodes: &FxHashSet<&'a str>) -> VcgNodeIndex<'a> {
    let mut by_normalized: FxHashMap<String, Vec<&'a str>> = FxHashMap::default();
    for &node in vcg_nodes {
        by_normalized
            .entry(normalize_vcg_path(node))
            .or_default()
            .push(node);
    }

    VcgNodeIndex {
        exact: vcg_nodes.iter().copied().collect(),
        by_normalized,
    }
}

/// Extract overconstrained suffixes from VCG node paths.
/// E.g., "body.frame_a.R" → ".R"
fn extract_overconstrained_suffixes(vcg_nodes: &FxHashSet<&str>) -> FxHashSet<String> {
    let mut suffixes = FxHashSet::default();
    for node in vcg_nodes {
        if let Some(dot_pos) = find_last_top_level_dot(node) {
            suffixes.insert(node[dot_pos..].to_string());
        }
    }
    suffixes
}

/// Collect optional edges from a set of connections.
fn collect_optional_edges_from_connections(
    connections: &[rumoca_ir_ast::InstanceConnection],
    suffixes: &FxHashSet<String>,
    node_index: &VcgNodeIndex<'_>,
    optional_edges: &mut Vec<(String, String)>,
    seen_edges: &mut FxHashSet<(String, String)>,
) {
    for conn in connections {
        let a_str = conn.a.to_flat_string();
        let b_str = conn.b.to_flat_string();
        for suffix in suffixes {
            let a_oc = format!("{a_str}{suffix}");
            let b_oc = format!("{b_str}{suffix}");
            extend_unique_optional_edges(&a_oc, &b_oc, node_index, optional_edges, seen_edges);
        }
    }
}

fn extend_unique_optional_edges(
    a_oc: &str,
    b_oc: &str,
    node_index: &VcgNodeIndex<'_>,
    optional_edges: &mut Vec<(String, String)>,
    seen_edges: &mut FxHashSet<(String, String)>,
) {
    for (a, b) in expand_optional_edges_for_suffix(a_oc, b_oc, node_index) {
        let key = normalize_edge_key(&a, &b);
        if !seen_edges.insert(key) {
            continue;
        }
        optional_edges.push((a, b));
    }
}

/// Expand a possibly-non-indexed optional VCG edge into indexed node pairs.
///
/// `connect(a, b)` endpoints can be array paths (e.g., `source.a`), while VCG
/// branch nodes are scalarized (e.g., `source[1].a.ref`). This helper resolves
/// both endpoints against known VCG nodes and pairs matching index signatures.
fn expand_optional_edges_for_suffix(
    a_oc: &str,
    b_oc: &str,
    node_index: &VcgNodeIndex<'_>,
) -> Vec<(String, String)> {
    let mut a_nodes = resolve_vcg_nodes_for_endpoint(a_oc, node_index);
    let mut b_nodes = resolve_vcg_nodes_for_endpoint(b_oc, node_index);

    if a_nodes.is_empty() && b_nodes.is_empty() {
        return Vec::new();
    }
    if a_nodes.is_empty() || b_nodes.is_empty() {
        return vec![(a_oc.to_string(), b_oc.to_string())];
    }

    a_nodes.sort();
    b_nodes.sort();

    // Scalar-to-array connect: pair scalar endpoint with every array element.
    if a_nodes.len() == 1 && b_nodes.len() > 1 {
        return b_nodes
            .into_iter()
            .map(|b| (a_nodes[0].clone(), b))
            .collect();
    }
    if b_nodes.len() == 1 && a_nodes.len() > 1 {
        return a_nodes
            .into_iter()
            .map(|a| (a, b_nodes[0].clone()))
            .collect();
    }

    // Pair by bracket-index signature when possible (e.g., [1], [2], ...).
    let mut b_by_sig: FxHashMap<String, Vec<String>> = FxHashMap::default();
    for b in b_nodes {
        b_by_sig.entry(index_signature(&b)).or_default().push(b);
    }
    for bucket in b_by_sig.values_mut() {
        bucket.sort();
    }

    let mut paired = Vec::new();
    let mut unmatched_a = Vec::new();
    for a in a_nodes {
        let sig = index_signature(&a);
        if let Some(bucket) = b_by_sig.get_mut(&sig)
            && !bucket.is_empty()
        {
            paired.push((a, bucket.remove(0)));
            continue;
        }
        unmatched_a.push(a);
    }

    let mut remaining_b: Vec<String> = b_by_sig.into_values().flatten().collect();
    remaining_b.sort();
    paired.extend(unmatched_a.into_iter().zip(remaining_b));
    paired
}

/// Resolve a connect endpoint to matching VCG node paths.
fn resolve_vcg_nodes_for_endpoint(endpoint: &str, node_index: &VcgNodeIndex<'_>) -> Vec<String> {
    if node_index.exact.contains(endpoint) {
        return vec![endpoint.to_string()];
    }

    let normalized = normalize_vcg_path(endpoint);
    node_index
        .by_normalized
        .get(&normalized)
        .map(|nodes| nodes.iter().map(|node| (*node).to_string()).collect())
        .unwrap_or_default()
}

/// Return an undirected edge key for deduplicating optional edges.
fn normalize_edge_key(a: &str, b: &str) -> (String, String) {
    if a <= b {
        (a.to_string(), b.to_string())
    } else {
        (b.to_string(), a.to_string())
    }
}

/// Normalize a path by removing bracket indices (`[i]`, `[1]`, ...).
fn normalize_vcg_path(path: &str) -> String {
    normalize_path_without_indices(path)
}

/// Extract bracket index signature from a path: `a[1].b[2].R` -> `1|2`.
fn index_signature(path: &str) -> String {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut in_brackets = false;
    for ch in path.chars() {
        match ch {
            '[' => {
                in_brackets = true;
                current.clear();
            }
            ']' if in_brackets => {
                in_brackets = false;
                parts.push(current.clone());
            }
            _ if in_brackets => current.push(ch),
            _ => {}
        }
    }
    parts.join("|")
}

/// Build the VCG spanning tree and compute isRoot/rooted for each node.
///
/// Algorithm per MLS §9.4:
/// 1. Build undirected graph from required (branch) + optional (connect) edges
/// 2. Find connected components via BFS
/// 3. For each component, select root:
///    - definite root > lowest-priority potential root > alphabetical first
/// 4. BFS from root to build spanning tree
/// 5. isRoot(N) = true iff N is the selected root
/// 6. rooted(N) = true iff N has a parent in the spanning tree
pub fn build_vcg(
    definite_roots: &FxHashSet<String>,
    potential_roots: &[(String, i64)],
    branches: &[(String, String)],
    optional_edges: &[(String, String)],
) -> VcgResult {
    let all_nodes = collect_all_nodes(definite_roots, potential_roots, branches, optional_edges);
    if all_nodes.is_empty() {
        return VcgResult {
            is_root: FxHashMap::default(),
            rooted: FxHashMap::default(),
        };
    }

    let adj = build_adjacency_list(&all_nodes, branches, optional_edges);
    let components = find_connected_components(&all_nodes, &adj);

    let mut is_root_map: FxHashMap<String, bool> = FxHashMap::default();
    let mut rooted_map: FxHashMap<String, bool> = FxHashMap::default();

    for component in &components {
        let root = select_root(component, definite_roots, potential_roots);
        let has_definite_root = component.iter().any(|n| definite_roots.contains(*n));

        for &node in component {
            let node_is_root = node == root;
            is_root_map.insert(node.to_string(), node_is_root);
            // rooted(N) = true iff N is NOT the root and the component has a root
            let node_rooted = !node_is_root && (has_definite_root || !potential_roots.is_empty());
            rooted_map.insert(node.to_string(), node_rooted);
        }
    }

    VcgResult {
        is_root: is_root_map,
        rooted: rooted_map,
    }
}

/// Collect all unique nodes from all edge sources.
fn collect_all_nodes(
    definite_roots: &FxHashSet<String>,
    potential_roots: &[(String, i64)],
    branches: &[(String, String)],
    optional_edges: &[(String, String)],
) -> FxHashSet<String> {
    let mut all_nodes: FxHashSet<String> = FxHashSet::default();
    for (a, b) in branches.iter().chain(optional_edges.iter()) {
        all_nodes.insert(a.clone());
        all_nodes.insert(b.clone());
    }
    for root in definite_roots {
        all_nodes.insert(root.clone());
    }
    for (path, _) in potential_roots {
        all_nodes.insert(path.clone());
    }
    all_nodes
}

/// Build an undirected adjacency list from edges.
fn build_adjacency_list<'a>(
    all_nodes: &'a FxHashSet<String>,
    branches: &'a [(String, String)],
    optional_edges: &'a [(String, String)],
) -> FxHashMap<&'a str, Vec<&'a str>> {
    let mut adj: FxHashMap<&str, Vec<&str>> = FxHashMap::default();
    for node in all_nodes {
        adj.entry(node.as_str()).or_default();
    }
    for (a, b) in branches.iter().chain(optional_edges.iter()) {
        adj.entry(a.as_str()).or_default().push(b.as_str());
        adj.entry(b.as_str()).or_default().push(a.as_str());
    }
    adj
}

/// Find connected components via BFS.
fn find_connected_components<'a>(
    all_nodes: &'a FxHashSet<String>,
    adj: &FxHashMap<&'a str, Vec<&'a str>>,
) -> Vec<Vec<&'a str>> {
    let mut visited: FxHashSet<&str> = FxHashSet::default();
    let mut components: Vec<Vec<&str>> = Vec::new();

    for node in all_nodes {
        if visited.contains(node.as_str()) {
            continue;
        }
        let component = bfs_component(node.as_str(), adj, &mut visited);
        components.push(component);
    }

    components
}

/// BFS from a starting node to find all reachable nodes.
fn bfs_component<'a>(
    start: &'a str,
    adj: &FxHashMap<&'a str, Vec<&'a str>>,
    visited: &mut FxHashSet<&'a str>,
) -> Vec<&'a str> {
    let mut component = Vec::new();
    let mut queue = std::collections::VecDeque::new();
    queue.push_back(start);
    visited.insert(start);
    while let Some(current) = queue.pop_front() {
        component.push(current);
        let Some(neighbors) = adj.get(current) else {
            continue;
        };
        for &neighbor in neighbors {
            if visited.insert(neighbor) {
                queue.push_back(neighbor);
            }
        }
    }
    component
}

/// Select the root for a connected component per MLS §9.4:
/// 1. Definite root (from Connections.root()) takes priority
/// 2. Potential root with lowest priority number
/// 3. Alphabetically first node as tiebreaker
fn select_root<'a>(
    component: &[&'a str],
    definite_roots: &FxHashSet<String>,
    potential_roots: &[(String, i64)],
) -> &'a str {
    // Check for definite root
    for &node in component {
        if definite_roots.contains(node) {
            return node;
        }
    }

    // Check for potential root with lowest priority
    if let Some(node) = find_best_potential_root(component, potential_roots) {
        return node;
    }

    // Alphabetically first as tiebreaker
    component.iter().copied().min().unwrap_or(component[0])
}

/// Find the best potential root in a component (lowest priority, alphabetical tiebreaker).
fn find_best_potential_root<'a>(
    component: &[&'a str],
    potential_roots: &[(String, i64)],
) -> Option<&'a str> {
    let component_set: FxHashSet<&str> = component.iter().copied().collect();
    let mut best: Option<(&str, i64)> = None;

    for (path, priority) in potential_roots {
        if !component_set.contains(path.as_str()) {
            continue;
        }
        let dominated = match best {
            None => true,
            Some((best_path, best_prio)) => {
                *priority < best_prio || (*priority == best_prio && path.as_str() < best_path)
            }
        };
        if dominated {
            best = Some((path.as_str(), *priority));
        }
    }

    let (best_path, _) = best?;
    // Return the &'a str from component (not from potential_roots)
    component.iter().find(|&&n| n == best_path).copied()
}

/// Compute the total scalar count of excess equations from VCG break edges (MLS §9.4).
///
/// For an overconstrained connection graph with V vertices and E edges per connected
/// component, the spanning tree has V-1 edges. The remaining E-(V-1) edges are "break
/// edges" whose equality equations should be replaced by `equalityConstraint()` calls.
/// Since we don't yet generate `equalityConstraint()`, these are excess equations.
///
/// Each break edge contributes one excess equality equation per scalar field in the
/// overconstrained record (e.g., 1 for Reference.gamma, 12 for Orientation.T+w).
pub fn compute_break_edge_scalar_count(
    branches: &[(String, String)],
    optional_edges: &[(String, String)],
    definite_roots: &FxHashSet<String>,
    potential_roots: &[(String, i64)],
    flat: &flat::Model,
) -> usize {
    let all_nodes = collect_all_nodes(definite_roots, potential_roots, branches, optional_edges);
    if all_nodes.is_empty() {
        return 0;
    }

    let adj = build_adjacency_list(&all_nodes, branches, optional_edges);
    let components = find_connected_components(&all_nodes, &adj);

    let mut total_excess = 0;
    for component in &components {
        let break_edges = count_component_break_edges(component, branches, optional_edges);
        if break_edges == 0 {
            continue;
        }
        let oc_scalar = component_oc_record_scalar_count(component, flat);
        total_excess += break_edges * oc_scalar;
    }
    total_excess
}

/// Compute overconstrained-record scalar size for a VCG component.
///
/// In well-formed models all nodes in a VCG component should have compatible
/// overconstrained record sizes (MLS §9.4). We use the maximum observed size to
/// avoid order-dependent undercounting when some nodes are flattened incompletely.
fn component_oc_record_scalar_count(component: &[&str], flat: &flat::Model) -> usize {
    component
        .iter()
        .map(|node| oc_record_scalar_count(node, flat))
        .max()
        .unwrap_or(0)
}

/// Count break edges for a single connected component.
fn count_component_break_edges(
    component: &[&str],
    branches: &[(String, String)],
    optional_edges: &[(String, String)],
) -> usize {
    let component_set: FxHashSet<&str> = component.iter().copied().collect();
    let mut edge_count: usize = 0;
    for (a, b) in branches.iter().chain(optional_edges.iter()) {
        if component_set.contains(a.as_str()) && component_set.contains(b.as_str()) {
            edge_count += 1;
        }
    }
    edge_count.saturating_sub(component.len().saturating_sub(1))
}

/// Compute the scalar count of an overconstrained record's fields.
///
/// Given a VCG node path like "resistor.pin_p.reference", finds all primitive
/// flat variables starting with that prefix and sums their scalar sizes.
fn oc_record_scalar_count(vcg_node: &str, flat: &flat::Model) -> usize {
    let prefix = format!("{vcg_node}.");
    let mut count = 0;
    for (name, var) in &flat.variables {
        if name.as_str().starts_with(&prefix) && var.is_primitive {
            count += var_scalar_size(var);
        }
    }
    // If no child fields found, the node itself might be a scalar variable
    if count == 0
        && let Some(var) = flat.variables.get(&flat::VarName::new(vcg_node))
    {
        count = var_scalar_size(var);
    }
    count
}

/// Compute scalar size of a flat variable from its dimensions.
fn var_scalar_size(var: &rumoca_ir_flat::Variable) -> usize {
    if var.dims.is_empty() {
        1
    } else {
        var.dims
            .iter()
            .copied()
            .map(|d| d.max(0) as usize)
            .product()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;
    use rumoca_ir_ast as ast;
    use rumoca_ir_flat as flat;

    fn add_orientation_record(flat: &mut flat::Model, base: &str) {
        for (suffix, dims) in [("T", vec![3, 3]), ("w", vec![3])] {
            let name = flat::VarName::new(format!("{base}.{suffix}"));
            flat.add_variable(
                name.clone(),
                rumoca_ir_flat::Variable {
                    name,
                    dims,
                    is_primitive: true,
                    ..Default::default()
                },
            );
        }
    }

    #[test]
    fn test_compute_break_edge_scalar_count_orientation_cycle() {
        // 3-node cycle => exactly one break edge.
        let branches = vec![
            ("a.R".to_string(), "b.R".to_string()),
            ("b.R".to_string(), "c.R".to_string()),
            ("c.R".to_string(), "a.R".to_string()),
        ];
        let optional_edges: Vec<(String, String)> = Vec::new();
        let definite_roots: FxHashSet<String> = ["a.R".to_string()].into_iter().collect();
        let potential_roots: Vec<(String, i64)> = Vec::new();

        let mut flat = flat::Model::new();
        add_orientation_record(&mut flat, "a.R");
        add_orientation_record(&mut flat, "b.R");
        add_orientation_record(&mut flat, "c.R");

        let break_edge_scalars = compute_break_edge_scalar_count(
            &branches,
            &optional_edges,
            &definite_roots,
            &potential_roots,
            &flat,
        );
        assert_eq!(
            break_edge_scalars, 12,
            "one Orientation break edge should contribute 12 scalars (T[3,3] + w[3])"
        );
    }

    #[test]
    fn test_component_oc_record_scalar_count_uses_max_node_size() {
        let mut flat = flat::Model::default();
        for (base, dims) in [("a.R", vec![1]), ("b.R", vec![3]), ("c.R", vec![2])] {
            let name = flat::VarName::new(format!("{base}.x"));
            flat.add_variable(
                name.clone(),
                rumoca_ir_flat::Variable {
                    name,
                    dims,
                    is_primitive: true,
                    ..Default::default()
                },
            );
        }

        let component = vec!["a.R", "b.R", "c.R"];
        assert_eq!(component_oc_record_scalar_count(&component, &flat), 3);
    }

    #[test]
    fn test_expand_optional_edges_for_suffix_matches_indexed_nodes() {
        let vcg_nodes: FxHashSet<&str> = [
            "source[1].pin_p.reference",
            "source[2].pin_p.reference",
            "resistor[1].pin_p.reference",
            "resistor[2].pin_p.reference",
        ]
        .into_iter()
        .collect();
        let node_index = build_vcg_node_index(&vcg_nodes);

        let edges = expand_optional_edges_for_suffix(
            "source.pin_p.reference",
            "resistor.pin_p.reference",
            &node_index,
        );

        assert_eq!(edges.len(), 2);
        assert!(edges.contains(&(
            "source[1].pin_p.reference".to_string(),
            "resistor[1].pin_p.reference".to_string()
        )));
        assert!(edges.contains(&(
            "source[2].pin_p.reference".to_string(),
            "resistor[2].pin_p.reference".to_string()
        )));
    }

    #[test]
    fn test_extract_overconstrained_suffixes_ignores_dot_inside_subscript_expression() {
        let vcg_nodes: FxHashSet<&str> = [
            "adapter[data.medium]",
            "adapter[data.medium].reference",
            "body.frame_a.R",
        ]
        .into_iter()
        .collect();

        let suffixes = extract_overconstrained_suffixes(&vcg_nodes);

        assert!(suffixes.contains(".reference"));
        assert!(suffixes.contains(".R"));
        assert!(
            !suffixes.contains(".medium]"),
            "dot inside bracketed subscript content must not be treated as a suffix separator"
        );
    }

    #[test]
    fn test_collect_vcg_node_set_includes_overconstrained_overlay_paths() {
        let vcg_data = VcgPreScanData {
            definite_roots: FxHashSet::default(),
            branches: vec![("a.reference".to_string(), "b.reference".to_string())],
            potential_roots: Vec::new(),
        };

        let mut overlay = ast::InstanceOverlay::default();
        overlay.components.insert(
            ast::InstanceId::new(1),
            ast::InstanceData {
                qualified_name: ast::QualifiedName::from_ident("alias"),
                oc_record_path: Some("alias.pin[1].reference".to_string()),
                ..Default::default()
            },
        );

        let nodes = collect_vcg_node_set(&vcg_data, &overlay);
        assert!(nodes.contains("a.reference"));
        assert!(nodes.contains("b.reference"));
        assert!(nodes.contains("alias.pin[1].reference"));
    }

    #[test]
    fn test_derive_optional_edges_maps_wrapper_alias_arrays() {
        fn q(parts: &[(&str, &[i64])]) -> ast::QualifiedName {
            ast::QualifiedName {
                parts: parts
                    .iter()
                    .map(|(name, idx)| ((*name).to_string(), idx.to_vec()))
                    .collect(),
            }
        }

        let vcg_data = VcgPreScanData {
            definite_roots: FxHashSet::default(),
            branches: vec![
                (
                    "adapter.plugToPin[1].plug.reference".to_string(),
                    "adapter.plugToPin[1].pin.reference".to_string(),
                ),
                (
                    "adapter.plugToPin[2].plug.reference".to_string(),
                    "adapter.plugToPin[2].pin.reference".to_string(),
                ),
                (
                    "resistor[1].pin_p.reference".to_string(),
                    "resistor[1].pin_n.reference".to_string(),
                ),
                (
                    "resistor[2].pin_p.reference".to_string(),
                    "resistor[2].pin_n.reference".to_string(),
                ),
            ],
            potential_roots: Vec::new(),
        };

        let mut overlay = ast::InstanceOverlay::default();
        overlay.classes.insert(
            ast::InstanceId::new(1),
            ast::ClassInstanceData {
                qualified_name: ast::QualifiedName::from_ident("root"),
                resolved_imports: Vec::new(),
                connections: vec![
                    ast::InstanceConnection {
                        a: q(&[("adapter", &[]), ("plugToPin", &[1]), ("pin", &[])]),
                        b: q(&[("adapter", &[]), ("pin", &[1])]),
                        connector_type: None,
                        span: Span::DUMMY,
                        scope: String::new(),
                    },
                    ast::InstanceConnection {
                        a: q(&[("adapter", &[]), ("plugToPin", &[2]), ("pin", &[])]),
                        b: q(&[("adapter", &[]), ("pin", &[2])]),
                        connector_type: None,
                        span: Span::DUMMY,
                        scope: String::new(),
                    },
                    ast::InstanceConnection {
                        a: q(&[("adapter", &[]), ("pin", &[])]),
                        b: q(&[("resistor", &[]), ("pin_p", &[])]),
                        connector_type: None,
                        span: Span::DUMMY,
                        scope: String::new(),
                    },
                ],
                ..Default::default()
            },
        );

        for (id, path) in [
            (10, "adapter.pin[1].reference"),
            (11, "adapter.pin[2].reference"),
            (12, "resistor[1].pin_p.reference"),
            (13, "resistor[2].pin_p.reference"),
        ] {
            overlay.components.insert(
                ast::InstanceId::new(id),
                ast::InstanceData {
                    qualified_name: ast::QualifiedName::from_ident("root"),
                    oc_record_path: Some(path.to_string()),
                    ..Default::default()
                },
            );
        }

        let edges = derive_optional_edges(&overlay, &vcg_data);

        assert!(edges.contains(&(
            "adapter.pin[1].reference".to_string(),
            "resistor[1].pin_p.reference".to_string()
        )));
        assert!(edges.contains(&(
            "adapter.pin[2].reference".to_string(),
            "resistor[2].pin_p.reference".to_string()
        )));
        assert!(
            !edges.contains(&(
                "adapter.pin.reference".to_string(),
                "resistor.pin_p.reference".to_string()
            )),
            "unindexed wrapper edges should expand to indexed edges"
        );
    }
}
