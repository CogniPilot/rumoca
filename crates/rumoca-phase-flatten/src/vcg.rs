//! Virtual Connection Graph spanning tree algorithm (MLS §9.4).
//!
//! This module implements the VCG algorithm for overconstrained connectors:
//! - Pre-scans equations for `Connections.branch/root/potentialRoot` calls
//! - Derives optional edges from `connect()` statements on overconstrained connectors
//! - Builds a spanning tree per connected component
//! - Provides `isRoot(N)` and `rooted(N)` query results for boolean evaluation

#[cfg(test)]
mod tests;

use rustc_hash::{FxHashMap, FxHashSet};

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

use crate::Context;
use crate::FlattenError;
use crate::connections_builtin::extract_potential_root_priority;
use crate::equations::build_qualified_name;
use crate::path_utils::{scope_split, segments, strip_array_index};

/// Result of the VCG spanning tree computation.
#[derive(Debug)]
pub(crate) struct VcgResult {
    /// For each VCG node path, whether it is the root of its component.
    pub is_root: FxHashMap<String, bool>,
    /// For the first endpoint `a` of each `Connections.branch(a, b)`, whether
    /// `a` is closer than `b` to the selected root in the spanning tree.
    pub rooted: FxHashMap<String, bool>,
}

/// Proof that required VCG edges form a forest with at most one definite root
/// in each tree.
#[derive(Clone, Debug)]
pub(crate) struct RequiredEdgeForest(VcgEdgeForest);

/// Pre-scanned VCG data from equations before full flattening.
pub(crate) struct VcgPreScanData {
    /// Definite roots from `Connections.root(a)`.
    pub definite_roots: FxHashSet<String>,
    /// Branches from `Connections.branch(a, b)` — required edges.
    pub branches: Vec<(String, String)>,
    /// Exact source span for each required branch edge, parallel to `branches`.
    pub branch_spans: Vec<rumoca_core::Span>,
    /// Potential roots from `Connections.potentialRoot(a, priority)`.
    pub potential_roots: Vec<(String, i64)>,
}

/// Pre-scan overlay classes for VCG data (branch/root/potentialRoot calls).
///
/// Iterates over all equations in all class instances, extracting
/// VCG-related function calls. Handles both top-level and for-loop nested calls.
pub(crate) fn pre_collect_vcg_data(
    overlay: &ast::InstanceOverlay,
    ctx: &Context,
    operators: &ast::ConnectionOperatorCatalog,
) -> Result<VcgPreScanData, FlattenError> {
    let mut data = VcgPreScanData {
        definite_roots: FxHashSet::default(),
        branches: Vec::new(),
        branch_spans: Vec::new(),
        potential_roots: Vec::new(),
    };

    for (_def_id, class_data) in &overlay.classes {
        if crate::is_in_disabled_component(&class_data.qualified_name, &overlay.disabled_components)
        {
            continue;
        }

        let prefix = &class_data.qualified_name;
        for inst_eq in &class_data.equations {
            collect_vcg_from_equation(&inst_eq.equation, prefix, ctx, operators, &mut data)?;
        }
    }

    Ok(data)
}

/// Recursively collect VCG data from an equation.
fn collect_vcg_from_equation(
    eq: &ast::Equation,
    prefix: &ast::QualifiedName,
    ctx: &Context,
    operators: &ast::ConnectionOperatorCatalog,
    data: &mut VcgPreScanData,
) -> Result<(), FlattenError> {
    match eq {
        ast::Equation::FunctionCall { comp, args, span } => {
            collect_vcg_from_function_call(comp, args, *span, prefix, operators, data)?;
        }
        ast::Equation::For { indices, equations } => {
            collect_vcg_from_for(indices, equations, prefix, ctx, operators, data)?;
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            collect_vcg_from_if(cond_blocks, else_block, prefix, ctx, operators, data)?;
        }
        _ => {}
    }
    Ok(())
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
    operators: &ast::ConnectionOperatorCatalog,
    data: &mut VcgPreScanData,
) -> Result<(), FlattenError> {
    // The pre-scan constructs the catalog that `isRoot`/`rooted` consume.  A
    // conditional with no graph-shaping declaration contributes nothing to
    // that catalog, so evaluating its condition here would be both unnecessary
    // and, for a graph query, circular.
    let contains_graph_declaration = cond_blocks
        .iter()
        .any(|block| equations_contain_vcg_calls(&block.eqs, operators))
        || else_block
            .as_deref()
            .is_some_and(|equations| equations_contain_vcg_calls(equations, operators));
    if !contains_graph_declaration {
        return Ok(());
    }

    // Try to evaluate conditions to select the right branch
    for block in cond_blocks {
        let eval = crate::boolean_eval::try_eval_boolean_with_ctx_inner(
            &block.cond,
            Some(ctx),
            prefix,
            operators,
        )?;
        match eval {
            Some(true) => {
                // This branch is taken — scan it and return
                for eq in &block.eqs {
                    collect_vcg_from_equation(eq, prefix, ctx, operators, data)?;
                }
                return Ok(());
            }
            Some(false) => {
                // This branch is not taken — skip it
                continue;
            }
            None => {
                // Can't evaluate — scan ALL branches conservatively
                scan_all_if_branches(cond_blocks, else_block, prefix, ctx, operators, data)?;
                return Ok(());
            }
        }
    }
    // All conditions were false — use else branch
    if let Some(else_eqs) = else_block {
        for eq in else_eqs {
            collect_vcg_from_equation(eq, prefix, ctx, operators, data)?;
        }
    }
    Ok(())
}

/// Scan all branches of an if-equation when conditions can't be evaluated.
fn scan_all_if_branches(
    cond_blocks: &[rumoca_ir_ast::EquationBlock],
    else_block: &Option<Vec<ast::Equation>>,
    prefix: &ast::QualifiedName,
    ctx: &Context,
    operators: &ast::ConnectionOperatorCatalog,
    data: &mut VcgPreScanData,
) -> Result<(), FlattenError> {
    for block in cond_blocks {
        for eq in &block.eqs {
            collect_vcg_from_equation(eq, prefix, ctx, operators, data)?;
        }
    }
    if let Some(else_eqs) = else_block {
        for eq in else_eqs {
            collect_vcg_from_equation(eq, prefix, ctx, operators, data)?;
        }
    }
    Ok(())
}

/// Collect VCG data from a Connections.* function call.
fn collect_vcg_from_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    call_span: rumoca_core::Span,
    prefix: &ast::QualifiedName,
    operators: &ast::ConnectionOperatorCatalog,
    data: &mut VcgPreScanData,
) -> Result<(), FlattenError> {
    let Some(declaration) = comp.target_def_id() else {
        return Ok(());
    };
    match operators.role(declaration) {
        Some(rumoca_core::ConnectionGraphOperatorRole::Root) => {
            if let Some(ast::Expression::ComponentReference(cr)) = args.first() {
                data.definite_roots.insert(build_qualified_name(prefix, cr));
            }
        }
        Some(rumoca_core::ConnectionGraphOperatorRole::Branch) => {
            extract_branch(args, prefix, call_span, data)
        }
        Some(rumoca_core::ConnectionGraphOperatorRole::PotentialRoot) => {
            extract_potential_root(args, prefix, call_span, data)?
        }
        Some(
            rumoca_core::ConnectionGraphOperatorRole::IsRoot
            | rumoca_core::ConnectionGraphOperatorRole::Rooted,
        )
        | None => {}
    }
    Ok(())
}

/// Extract a branch edge from Connections.branch(a, b) arguments.
fn extract_branch(
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    data: &mut VcgPreScanData,
) {
    if args.len() >= 2
        && let ast::Expression::ComponentReference(cr_a) = &args[0]
        && let ast::Expression::ComponentReference(cr_b) = &args[1]
    {
        let a = build_qualified_name(prefix, cr_a);
        let b = build_qualified_name(prefix, cr_b);
        data.branches.push((a, b));
        data.branch_spans.push(span);
    }
}

/// Extract a potential root from Connections.potentialRoot(a, priority) arguments.
fn extract_potential_root(
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
    data: &mut VcgPreScanData,
) -> Result<(), FlattenError> {
    if let Some(ast::Expression::ComponentReference(cr)) = args.first() {
        let priority = extract_potential_root_priority(args, span)?;
        let path = build_qualified_name(prefix, cr);
        data.potential_roots.push((path, priority));
    }
    Ok(())
}

/// Expand for-loop indices and collect VCG calls from nested equations.
fn collect_vcg_from_for(
    indices: &[rumoca_ir_ast::ForIndex],
    equations: &[ast::Equation],
    prefix: &ast::QualifiedName,
    ctx: &Context,
    operators: &ast::ConnectionOperatorCatalog,
    data: &mut VcgPreScanData,
) -> Result<(), FlattenError> {
    if !equations_contain_vcg_calls(equations, operators) {
        return Ok(());
    }

    if indices.is_empty() {
        for eq in equations {
            collect_vcg_from_equation(eq, prefix, ctx, operators, data)?;
        }
        return Ok(());
    }

    let first = &indices[0];
    let remaining = &indices[1..];

    let index_values =
        crate::equations::expand_range_indices(ctx, &first.range, prefix, first.range.span())?;

    let index_name = &first.ident.text;
    for value in index_values {
        let substituted: Vec<ast::Equation> = equations
            .iter()
            .map(|eq| crate::equations::substitute_index_in_equation(eq, index_name, value))
            .collect();
        collect_vcg_from_for(remaining, &substituted, prefix, ctx, operators, data)?;
    }
    Ok(())
}

fn equations_contain_vcg_calls(
    equations: &[ast::Equation],
    operators: &ast::ConnectionOperatorCatalog,
) -> bool {
    equations
        .iter()
        .any(|equation| equation_contains_vcg_call(equation, operators))
}

fn equation_contains_vcg_call(
    eq: &ast::Equation,
    operators: &ast::ConnectionOperatorCatalog,
) -> bool {
    match eq {
        ast::Equation::FunctionCall { comp, .. } => is_vcg_function_call(comp, operators),
        ast::Equation::For { equations, .. } => equations_contain_vcg_calls(equations, operators),
        ast::Equation::When(blocks) => blocks
            .iter()
            .any(|block| equations_contain_vcg_calls(&block.eqs, operators)),
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            cond_blocks
                .iter()
                .any(|block| equations_contain_vcg_calls(&block.eqs, operators))
                || else_block
                    .as_deref()
                    .is_some_and(|equations| equations_contain_vcg_calls(equations, operators))
        }
        _ => false,
    }
}

fn is_vcg_function_call(
    comp: &ast::ComponentReference,
    operators: &ast::ConnectionOperatorCatalog,
) -> bool {
    matches!(
        comp.target_def_id()
            .and_then(|declaration| operators.role(declaration)),
        Some(
            rumoca_core::ConnectionGraphOperatorRole::Root
                | rumoca_core::ConnectionGraphOperatorRole::Branch
                | rumoca_core::ConnectionGraphOperatorRole::PotentialRoot
        )
    )
}

/// Derive optional edges from connection statements on overconstrained connectors.
///
/// For each `connect(a, b)` where the connectors have overconstrained descendants,
/// we create an optional edge between the overconstrained records.
/// The overconstrained suffix is derived from the VCG node paths.
pub(crate) fn derive_optional_edges(
    overconstrained: &ast::FinalizedOverconstrainedCatalog<'_>,
    vcg_data: &VcgPreScanData,
) -> Result<Vec<(String, String)>, crate::FlattenError> {
    let overlay = overconstrained.overlay();
    crate::connections::ensure_connection_scalarization_budget(overlay)?;
    let vcg_nodes = collect_vcg_node_set(vcg_data, overconstrained);
    if vcg_nodes.is_empty() {
        return Ok(Vec::new());
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
        // SPEC_0032 §1: compact families stay authoritative and zero-cardinality
        // families naturally contribute no scalar optional edges.
        for connection in
            rumoca_eval_ast::connection::scalar_connection_view(&class_data.connections)
        {
            let connection = connection.map_err(crate::structured_connection_error)?;
            if crate::connections::connection_involves_disabled(
                &connection,
                &overlay.disabled_components,
            ) {
                continue;
            }
            collect_optional_edges_from_connection(
                &connection,
                &suffixes,
                &node_index,
                &mut optional_edges,
                &mut seen_edges,
            );
        }
    }

    Ok(optional_edges)
}

/// Collect all VCG node paths from pre-scanned data.
fn collect_vcg_node_set<'a>(
    vcg_data: &'a VcgPreScanData,
    overconstrained: &'a ast::FinalizedOverconstrainedCatalog<'_>,
) -> FxHashSet<String> {
    let mut nodes: FxHashSet<String> = FxHashSet::default();
    for (a, b) in &vcg_data.branches {
        nodes.insert(a.clone());
        nodes.insert(b.clone());
    }
    for root in &vcg_data.definite_roots {
        nodes.insert(root.clone());
    }
    for (path, _) in &vcg_data.potential_roots {
        nodes.insert(path.clone());
    }

    // Include all instantiated overconstrained record paths so connect() edges can
    // map through alias connectors that do not appear in branch/root pre-scan data.
    for record in overconstrained.records() {
        if crate::is_in_disabled_component(
            record.qualified_name(),
            &overconstrained.overlay().disabled_components,
        ) {
            continue;
        }
        nodes.insert(record.qualified_name().to_flat_string());
    }
    nodes
}

/// Fast lookup index for VCG node path resolution.
struct VcgNodeIndex<'a> {
    exact: FxHashSet<&'a str>,
}

fn build_vcg_node_index(vcg_nodes: &FxHashSet<String>) -> VcgNodeIndex<'_> {
    VcgNodeIndex {
        exact: vcg_nodes.iter().map(String::as_str).collect(),
    }
}

/// Extract overconstrained suffixes from VCG node paths.
/// E.g., "body.frame_a.R" → ".R"
fn extract_overconstrained_suffixes(vcg_nodes: &FxHashSet<String>) -> FxHashSet<String> {
    let mut suffixes = FxHashSet::default();
    for node in vcg_nodes {
        if let Some((_, leaf)) = scope_split(node) {
            suffixes.insert(format!(".{leaf}"));
        }
    }
    suffixes
}

/// Collect optional edges from a set of connections.
fn collect_optional_edges_from_connection(
    conn: &rumoca_ir_ast::InstanceScalarConnection,
    suffixes: &FxHashSet<String>,
    node_index: &VcgNodeIndex<'_>,
    optional_edges: &mut Vec<(String, String)>,
    seen_edges: &mut FxHashSet<(String, String)>,
) {
    let a_str = conn.a().to_flat_string();
    let b_str = conn.b().to_flat_string();
    for suffix in suffixes {
        let a_oc = format!("{a_str}{suffix}");
        let b_oc = format!("{b_str}{suffix}");
        extend_unique_optional_edges(&a_oc, &b_oc, node_index, optional_edges, seen_edges);
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
        return Vec::new();
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

/// MLS §9.4 / CONN-013: every connected VCG component containing a required
/// branch edge must declare a definite or potential root in that component.
///
/// Validate before `build_vcg` so the alphabetical fallback is never used for
/// a semantically invalid graph.
pub(crate) fn validate_component_roots(
    data: &VcgPreScanData,
    optional_edges: &[(String, String)],
) -> Result<(), FlattenError> {
    let all_nodes = collect_all_nodes(
        &data.definite_roots,
        &data.potential_roots,
        &data.branches,
        optional_edges,
    );
    let adjacency = build_adjacency_list(&all_nodes, &data.branches, optional_edges);
    let mut visited = FxHashSet::default();
    for (edge_index, (lhs, rhs)) in data.branches.iter().enumerate() {
        let branch_span = data.branch_spans.get(edge_index).copied().ok_or_else(|| {
            FlattenError::missing_source_context(format!(
                "Connections.branch({lhs}, {rhs}) has no parallel source provenance"
            ))
        })?;
        if visited.contains(lhs.as_str()) {
            continue;
        }
        // Branches retain source order, so the first diagnostic remains
        // deterministic even though graph membership uses hash collections.
        let component = bfs_component(lhs, &adjacency, &mut visited);
        let has_root = component.iter().any(|node| {
            data.definite_roots.contains(*node)
                || data.potential_roots.iter().any(|(root, _)| root == node)
        });
        if has_root {
            continue;
        }
        let span = (!branch_span.is_dummy())
            .then_some(branch_span)
            .ok_or_else(|| {
                FlattenError::missing_source_context(format!(
                    "Connections.branch({lhs}, {rhs}) has no source span"
                ))
            })?;
        return Err(FlattenError::unsupported_equation(
            format!(
                "Connections.branch({lhs}, {rhs}) belongs to a virtual connection graph \
                 component with no Connections.root() or Connections.potentialRoot(); every \
                 component needs a root (MLS §9.4)"
            ),
            span,
        ));
    }
    Ok(())
}

/// Resolve a connect endpoint to matching VCG node paths.
fn resolve_vcg_nodes_for_endpoint(endpoint: &str, node_index: &VcgNodeIndex<'_>) -> Vec<String> {
    if node_index.exact.contains(endpoint) {
        return vec![endpoint.to_string()];
    }

    node_index
        .exact
        .iter()
        .copied()
        .filter(|candidate| vcg_endpoint_matches_node(endpoint, candidate))
        .map(str::to_string)
        .collect()
}

/// Return an undirected edge key for deduplicating optional edges.
fn normalize_edge_key(a: &str, b: &str) -> (String, String) {
    if a <= b {
        (a.to_string(), b.to_string())
    } else {
        (b.to_string(), a.to_string())
    }
}

/// MLS §9.4: VCG nodes are connector-instance specific. Fallback endpoint
/// resolution may wildcard omitted array indices, but it must preserve any
/// indices explicitly named in the connect/root/branch endpoint.
fn vcg_endpoint_matches_node(endpoint: &str, candidate: &str) -> bool {
    let endpoint_parts = segments(endpoint);
    let candidate_parts = segments(candidate);
    if endpoint_parts.len() != candidate_parts.len() {
        return false;
    }

    endpoint_parts
        .into_iter()
        .zip(candidate_parts)
        .all(|(endpoint_part, candidate_part)| {
            strip_array_index(endpoint_part) == strip_array_index(candidate_part)
                && (rumoca_core::split_trailing_subscript_suffix(endpoint_part).is_none()
                    || endpoint_part == candidate_part)
        })
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
/// 1. Seed the forest with required branch edges and admit optional edges that
///    connect two forest components without joining distinct definite roots
/// 2. Find connected components in the selected spanning forest via BFS
/// 3. For each component, select root:
///    - definite root > lowest-priority potential root > alphabetical first
/// 4. BFS from root to compute node depths
/// 5. isRoot(N) = true iff N is the selected root
/// 6. rooted(a) = depth(a) < depth(b) for `Connections.branch(a, b)`
pub(crate) fn build_vcg(
    data: &VcgPreScanData,
    optional_edges: &[(String, String)],
    required_forest: &RequiredEdgeForest,
) -> VcgResult {
    let all_nodes = collect_all_nodes(
        &data.definite_roots,
        &data.potential_roots,
        &data.branches,
        optional_edges,
    );
    if all_nodes.is_empty() {
        return VcgResult {
            is_root: FxHashMap::default(),
            rooted: FxHashMap::default(),
        };
    }

    let mut forest = required_forest.0.clone();
    let mut selected_optional_edges = Vec::new();
    for (lhs, rhs) in optional_edges {
        if !forest.reject_optional_edge(lhs, rhs) {
            selected_optional_edges.push((lhs.clone(), rhs.clone()));
        }
    }

    let adj = build_adjacency_list(&all_nodes, &data.branches, &selected_optional_edges);
    let components = find_connected_components(&all_nodes, &adj);

    let mut is_root_map: FxHashMap<String, bool> = FxHashMap::default();
    let mut rooted_map: FxHashMap<String, bool> = FxHashMap::default();
    let mut depths: FxHashMap<&str, usize> = FxHashMap::default();

    for component in &components {
        let root = select_root(component, &data.definite_roots, &data.potential_roots);
        let has_definite_root = component
            .iter()
            .any(|node| data.definite_roots.contains(*node));
        depths.extend(bfs_depths(root, &adj));

        for &node in component {
            let node_is_root =
                data.definite_roots.contains(node) || (!has_definite_root && node == root);
            is_root_map.insert(node.to_string(), node_is_root);
            rooted_map.insert(node.to_string(), false);
        }
    }

    for (lhs, rhs) in &data.branches {
        if let (Some(lhs_depth), Some(rhs_depth)) =
            (depths.get(lhs.as_str()), depths.get(rhs.as_str()))
        {
            rooted_map.insert(lhs.clone(), lhs_depth < rhs_depth);
        }
    }

    VcgResult {
        is_root: is_root_map,
        rooted: rooted_map,
    }
}

/// Compute shortest-path depths from one selected component root.
fn bfs_depths<'a>(
    root: &'a str,
    adj: &FxHashMap<&'a str, Vec<&'a str>>,
) -> FxHashMap<&'a str, usize> {
    let mut depths = FxHashMap::default();
    let mut queue = std::collections::VecDeque::new();
    depths.insert(root, 0);
    queue.push_back(root);

    while let Some(current) = queue.pop_front() {
        let depth = depths[current];
        let Some(neighbors) = adj.get(current) else {
            continue;
        };
        for &neighbor in neighbors {
            if !depths.contains_key(neighbor) {
                depths.insert(neighbor, depth + 1);
                queue.push_back(neighbor);
            }
        }
    }

    depths
}

#[derive(Clone, Debug)]
#[cfg_attr(test, derive(PartialEq, Eq))]
struct VcgEdgeForest {
    index: FxHashMap<String, usize>,
    parent: Vec<usize>,
    has_definite_root: Vec<bool>,
}

/// Disposition of one generated primitive equality from an overconstrained
/// record connection edge.
pub(crate) enum GeneratedEqualityDisposition {
    /// Keep the ordinary primitive equality because the record edge belongs to
    /// the selected spanning forest (or is not overconstrained).
    Retain,
    /// Omit a later primitive field of a broken record edge after the exact
    /// RecordInstance-bound equalityConstraint plan was handled once.
    Omit,
    /// Resolve the complete broken record edge through its exact
    /// RecordInstance-bound equalityConstraint plan. This variant is emitted
    /// exactly once for the normalized pair and carries no cardinality claim.
    Replace {
        lhs_record: String,
        rhs_record: String,
    },
}

#[derive(Clone, Copy)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
enum RecordPairDisposition {
    Retain,
    ReplacePending,
    ReplaceEmitted,
}

/// MLS §9.4 forest used while emitting equations for overconstrained records.
///
/// Connection sets are already transitive closures, so their generated equality
/// chain need not preserve the original `connect` edge identity. Any
/// deterministic spanning forest over the same VCG nodes is equivalent.
/// Required `Connections.branch` edges seed this forest, while generated
/// potential equalities are admitted only when they extend it. Broken edges
/// are resolved once by the exact RecordInstance-bound equalityConstraint
/// plan; VCG metadata never decides whether that plan is vacuous.
#[derive(Clone)]
#[cfg_attr(test, derive(Debug, PartialEq, Eq))]
pub(crate) struct OverconstrainedEquationForest {
    forest: VcgEdgeForest,
    pair_dispositions: FxHashMap<(String, String), RecordPairDisposition>,
}

impl OverconstrainedEquationForest {
    pub(crate) fn new(required_forest: RequiredEdgeForest) -> Self {
        Self {
            forest: required_forest.0,
            pair_dispositions: FxHashMap::default(),
        }
    }

    #[cfg(test)]
    pub(crate) fn empty() -> Self {
        Self::new(RequiredEdgeForest(VcgEdgeForest::new_empty(
            &FxHashSet::default(),
            &[],
            &[],
        )))
    }

    #[cfg(test)]
    pub(crate) fn state_snapshot(&self) -> Self {
        self.clone()
    }

    pub(crate) fn generated_equality_disposition(
        &mut self,
        overconstrained: &ast::FinalizedOverconstrainedCatalog<'_>,
        flat: &flat::Model,
        lhs: &rumoca_core::VarName,
        rhs: &rumoca_core::VarName,
        span: rumoca_core::Span,
    ) -> Result<GeneratedEqualityDisposition, FlattenError> {
        let lhs_evidence = overconstrained_record_evidence(overconstrained, flat, lhs, span)?;
        let rhs_evidence = overconstrained_record_evidence(overconstrained, flat, rhs, span)?;
        let (
            OverconstrainedRecordEvidence::Valid { record: lhs_record },
            OverconstrainedRecordEvidence::Valid { record: rhs_record },
        ) = (lhs_evidence, rhs_evidence)
        else {
            if matches!(lhs_evidence, OverconstrainedRecordEvidence::Ordinary)
                && matches!(rhs_evidence, OverconstrainedRecordEvidence::Ordinary)
            {
                return Ok(GeneratedEqualityDisposition::Retain);
            }
            return Err(FlattenError::invalid_connection_evidence(
                format!(
                    "connection members `{lhs}` and `{rhs}` disagree on overconstrained-record evidence"
                ),
                span,
            ));
        };
        let lhs_record = lhs_record.qualified_name().to_flat_string();
        let rhs_record = rhs_record.qualified_name().to_flat_string();
        let key = normalize_edge_key(&lhs_record, &rhs_record);
        let disposition = self.pair_dispositions.entry(key).or_insert_with(|| {
            if !self.forest.reject_optional_edge(&lhs_record, &rhs_record) {
                RecordPairDisposition::Retain
            } else {
                RecordPairDisposition::ReplacePending
            }
        });
        Ok(match *disposition {
            RecordPairDisposition::Retain => GeneratedEqualityDisposition::Retain,
            RecordPairDisposition::ReplaceEmitted => GeneratedEqualityDisposition::Omit,
            RecordPairDisposition::ReplacePending => {
                *disposition = RecordPairDisposition::ReplaceEmitted;
                GeneratedEqualityDisposition::Replace {
                    lhs_record,
                    rhs_record,
                }
            }
        })
    }
}

impl RequiredEdgeForest {
    pub(crate) fn construct(
        data: &VcgPreScanData,
        optional_edges: &[(String, String)],
    ) -> Result<Self, FlattenError> {
        let mut forest =
            VcgEdgeForest::new_empty(&data.definite_roots, &data.branches, optional_edges);
        for (edge_index, (lhs, rhs)) in data.branches.iter().enumerate() {
            let span = data
                .branch_spans
                .get(edge_index)
                .copied()
                .filter(|span| !span.is_dummy())
                .ok_or_else(|| {
                    FlattenError::missing_source_context(format!(
                        "Connections.branch({lhs}, {rhs}) has no source span"
                    ))
                })?;
            forest.insert_required_edge(lhs, rhs, span)?;
        }
        if data.branch_spans.len() != data.branches.len() {
            return Err(FlattenError::missing_source_context(
                "virtual connection graph has provenance without a required edge",
            ));
        }
        Ok(Self(forest))
    }
}

#[cfg(test)]
pub(crate) fn test_required_forest(
    definite_roots: &FxHashSet<String>,
    branches: &[(String, String)],
    optional_edges: &[(String, String)],
) -> RequiredEdgeForest {
    let source = rumoca_core::SourceId::from_source_name("vcg_required_forest_test.mo");
    let data = VcgPreScanData {
        definite_roots: definite_roots.clone(),
        branches: branches.to_vec(),
        branch_spans: (0..branches.len())
            .map(|index| rumoca_core::Span::from_offsets(source, index * 2, index * 2 + 1))
            .collect(),
        potential_roots: Vec::new(),
    };
    RequiredEdgeForest::construct(&data, optional_edges)
        .expect("test required edges must satisfy the VCG construction contract")
}

impl VcgEdgeForest {
    fn new_empty(
        definite_roots: &FxHashSet<String>,
        branches: &[(String, String)],
        optional_edges: &[(String, String)],
    ) -> Self {
        let mut nodes: Vec<String> = branches
            .iter()
            .chain(optional_edges)
            .flat_map(|(lhs, rhs)| [lhs.clone(), rhs.clone()])
            .chain(definite_roots.iter().cloned())
            .collect();
        nodes.sort();
        nodes.dedup();
        let index: FxHashMap<String, usize> = nodes
            .into_iter()
            .enumerate()
            .map(|(index, node)| (node, index))
            .collect();
        let mut forest = Self {
            parent: (0..index.len()).collect(),
            has_definite_root: vec![false; index.len()],
            index,
        };
        for root in definite_roots {
            if let Some(index) = forest.index.get(root).copied() {
                forest.has_definite_root[index] = true;
            }
        }
        forest
    }

    fn insert_required_edge(
        &mut self,
        lhs: &str,
        rhs: &str,
        span: rumoca_core::Span,
    ) -> Result<(), FlattenError> {
        let lhs_index = self.index[lhs];
        let rhs_index = self.index[rhs];
        let lhs_root = self.find(lhs_index);
        let rhs_root = self.find(rhs_index);
        if lhs_root == rhs_root {
            return Err(FlattenError::invalid_connection_graph(
                format!(
                    "Connections.branch({lhs}, {rhs}) closes a cycle of required spanning-tree edges"
                ),
                span,
            ));
        }
        if self.has_definite_root[lhs_root] && self.has_definite_root[rhs_root] {
            return Err(FlattenError::invalid_connection_graph(
                format!(
                    "Connections.branch({lhs}, {rhs}) connects two required-edge trees that each contain a definite root"
                ),
                span,
            ));
        }
        self.union(lhs_root, rhs_root);
        Ok(())
    }

    fn reject_optional_edge(&mut self, lhs: &str, rhs: &str) -> bool {
        let Some(lhs) = self.index.get(lhs).copied() else {
            return false;
        };
        let Some(rhs) = self.index.get(rhs).copied() else {
            return false;
        };
        let lhs_root = self.find(lhs);
        let rhs_root = self.find(rhs);
        if lhs_root == rhs_root
            || self.has_definite_root[lhs_root] && self.has_definite_root[rhs_root]
        {
            return true;
        }
        self.union(lhs_root, rhs_root);
        false
    }

    fn find(&mut self, index: usize) -> usize {
        let parent = self.parent[index];
        if parent == index {
            return index;
        }
        let root = self.find(parent);
        self.parent[index] = root;
        root
    }

    fn union(&mut self, lhs: usize, rhs: usize) {
        let lhs = self.find(lhs);
        let rhs = self.find(rhs);
        if lhs == rhs {
            return;
        }
        let (keep, merge) = if lhs < rhs { (lhs, rhs) } else { (rhs, lhs) };
        self.parent[merge] = keep;
        self.has_definite_root[keep] =
            self.has_definite_root[keep] || self.has_definite_root[merge];
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
    for &node in component {
        if definite_roots.contains(node) {
            return node;
        }
    }

    if let Some(node) = find_best_potential_root(component, potential_roots) {
        return node;
    }

    // Alphabetically first as tiebreaker
    component
        .iter()
        .copied()
        .min()
        .expect("component is non-empty")
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
/// This is conservative metadata: zero-result constraints can already be omitted
/// exactly during connection-equation generation, while balance accounting clamps
/// the correction to the excess still present. Nonempty constraints remain
/// unresolved metadata until their replacement equations are lowered.
///
/// Each break edge contributes one excess equality equation per scalar field in the
/// overconstrained record (e.g., 1 for Reference.gamma, 12 for Orientation.T+w).
pub(crate) fn compute_break_edge_scalar_count(
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
        let break_edges =
            count_component_break_edges(component, branches, optional_edges, definite_roots);
        if break_edges == 0 {
            continue;
        }
        let oc_scalar = component_oc_record_scalar_count(component, flat);
        total_excess += break_edges * oc_scalar;
    }
    total_excess
}

#[derive(Clone, Copy)]
enum OverconstrainedRecordEvidence<'a> {
    Ordinary,
    Valid {
        record: ast::FinalizedOverconstrainedRecord<'a>,
    },
}

fn overconstrained_record_evidence<'a>(
    overconstrained: &'a ast::FinalizedOverconstrainedCatalog<'_>,
    flat: &'a flat::Model,
    name: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<OverconstrainedRecordEvidence<'a>, FlattenError> {
    let variable = if let Some(variable) = flat.variables.get(name) {
        variable
    } else {
        let selected = crate::connections::declared_array_element_evidence(name, flat).map_err(
            |reason| {
                FlattenError::invalid_connection_evidence(
                    format!(
                        "connection member `{name}` has invalid overconstrained selection evidence: {reason}"
                    ),
                    span,
                )
            },
        )?;
        selected
            .ok_or_else(|| {
                FlattenError::invalid_connection_evidence(
                    format!(
                        "connection member `{name}` has no Flat declaration for overconstrained classification"
                    ),
                    span,
                )
            })?
            .declaration
    };

    match overconstrained.classify_component(variable.instance_id) {
        ast::FinalizedOverconstrainedComponent::Ordinary => {
            Ok(OverconstrainedRecordEvidence::Ordinary)
        }
        ast::FinalizedOverconstrainedComponent::Record(record) => {
            Ok(OverconstrainedRecordEvidence::Valid { record })
        }
        ast::FinalizedOverconstrainedComponent::Foreign => {
            Err(FlattenError::invalid_connection_evidence(
                format!(
                    "connection member `{name}` carries foreign or unset Instance identity {}",
                    variable.instance_id
                ),
                span,
            ))
        }
    }
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
    definite_roots: &FxHashSet<String>,
) -> usize {
    let component_set: FxHashSet<&str> = component.iter().copied().collect();
    let mut edge_count: usize = 0;
    for (a, b) in branches.iter().chain(optional_edges.iter()) {
        if component_set.contains(a.as_str()) && component_set.contains(b.as_str()) {
            edge_count += 1;
        }
    }
    let root_count = component
        .iter()
        .filter(|node| definite_roots.contains(**node))
        .count()
        .max(1);
    edge_count.saturating_sub(component.len().saturating_sub(root_count))
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
        && let Some(var) = flat.variables.get(&rumoca_core::VarName::new(vcg_node))
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
