use super::*;
use indexmap::IndexSet;
use rumoca_ir_ast as ast;

type FlowVarSet = IndexSet<rumoca_core::VarName>;

/// Compute scalar count from variable dimensions.
///
/// For array variables, scalar_count = product of dimensions.
/// For scalars (empty dims), returns 1.
fn compute_var_scalar_count(var: &flat::Variable) -> usize {
    if var.dims.is_empty() {
        1
    } else {
        var.dims.iter().copied().map(|d| d.max(0)).product::<i64>() as usize
    }
}

fn resolve_flow_var_scalar_count(flat: &flat::Model, var: &rumoca_core::VarName) -> Option<usize> {
    if let Some(v) = flat.variables.get(var) {
        return Some(compute_var_scalar_count(v));
    }
    if subscripted_base_var(var, flat).is_some() {
        return Some(1);
    }
    strip_embedded_array_indices(var.as_str()).and_then(|base| {
        if var.as_str().contains('[') {
            Some(1)
        } else {
            flat.variables
                .get(&rumoca_core::VarName::new(base))
                .map(compute_var_scalar_count)
        }
    })
}

pub(super) fn strip_embedded_array_indices(path: &str) -> Option<String> {
    let parts = split_path_with_indices(path);
    if !parts.iter().any(|p| p.contains('[')) {
        return None;
    }
    Some(
        parts
            .into_iter()
            .map(strip_array_index)
            .collect::<Vec<_>>()
            .join("."),
    )
}

fn weather_bus_field_name(path: &str) -> Option<rumoca_core::VarName> {
    let parts = split_path_with_indices(path);
    let field = parts.last()?;
    if parts.len() < 2 || !parts[..parts.len() - 1].contains(&"weaBus") {
        return None;
    }
    let known = matches!(
        *field,
        "TDryBul"
            | "TWetBul"
            | "TDewPoi"
            | "TBlaSky"
            | "relHum"
            | "winSpe"
            | "winDir"
            | "HGloHor"
            | "HDifHor"
            | "HDirNor"
            | "HHorIR"
            | "nTot"
            | "nOpa"
            | "pAtm"
            | "ceiHei"
            | "cloTim"
            | "lat"
            | "lon"
            | "solAlt"
            | "solDec"
            | "solHouAng"
            | "solTim"
            | "solZen"
    );
    known.then(|| rumoca_core::VarName::new(path))
}

fn materialize_weather_bus_field(flat: &mut flat::Model, name: rumoca_core::VarName) {
    if flat.variables.contains_key(&name) {
        return;
    }
    flat.add_variable(
        name.clone(),
        flat::Variable {
            name,
            is_primitive: true,
            from_expandable_connector: true,
            ..Default::default()
        },
    );
}

fn materialize_weather_bus_fields_from_refs(
    flat: &mut flat::Model,
    refs: impl IntoIterator<Item = rumoca_core::VarName>,
) {
    for reference in refs {
        if let Some(name) = weather_bus_field_name(reference.as_str()) {
            materialize_weather_bus_field(flat, name);
        }
    }
}

fn materialize_weather_bus_fields(flat: &mut flat::Model, connections: &[ast::InstanceConnection]) {
    let mut refs = std::collections::HashSet::new();
    for equation in &flat.equations {
        equation.residual.collect_var_refs(&mut refs);
    }
    materialize_weather_bus_fields_from_refs(flat, refs);

    let connection_refs = connections.iter().flat_map(|connection| {
        [
            rumoca_core::VarName::new(connection.a.to_flat_string()),
            rumoca_core::VarName::new(connection.b.to_flat_string()),
        ]
    });
    materialize_weather_bus_fields_from_refs(flat, connection_refs);
}

fn mark_connected(flat: &mut flat::Model, var: &rumoca_core::VarName) {
    if let Some(v) = flat.variables.get_mut(var) {
        v.connected = true;
        return;
    }
    if let Some(base) = subscripted_base_var(var, flat)
        && let Some(v) = flat.variables.get_mut(&base)
    {
        v.connected = true;
    }
}

pub(super) fn mark_stream_connection_set(
    flat: &mut flat::Model,
    variables: &[rumoca_core::VarName],
) {
    for var in variables {
        mark_connected(flat, var);
    }
}

/// Generate equality equations for potential (non-flow) variables.
///
/// For n variables in a connection set, generates n-1 equations:
/// `v1 = v2, v2 = v3, ..., v(n-1) = vn`
///
/// In residual form: `v1 - v2 = 0, v2 - v3 = 0, ...`
pub(super) fn generate_equality_equations(
    flat: &mut flat::Model,
    variables: &[rumoca_core::VarName],
    span: rumoca_core::Span,
) -> Result<(), FlattenError> {
    // Generate chain of equality equations: v1 - v2 = 0, v2 - v3 = 0, ...
    for window in variables.windows(2) {
        let var_a = &window[0];
        let var_b = &window[1];

        // Get scalar count from variable dimensions (MLS §8.4)
        let mut scalar_count = flat
            .variables
            .get(var_a)
            .map(compute_var_scalar_count)
            .filter(|&c| c > 1)
            .or_else(|| flat.variables.get(var_b).map(compute_var_scalar_count))
            .unwrap_or(1);

        // When both variables are arrays with different sizes (from subrange
        // connections like connect(a.y, b.u[1:3])), cap at the smaller size.
        let rhs_size = flat.variables.get(var_b).map(compute_var_scalar_count);
        if let Some(b) = rhs_size.filter(|&b| scalar_count > 1 && b > 1 && b < scalar_count) {
            scalar_count = b;
        }

        // Skip empty arrays (Real[0]) — no equations needed
        if scalar_count == 0 {
            continue;
        }

        // Mark both variables as connected
        mark_connected(flat, var_a);
        mark_connected(flat, var_b);

        // Create residual: var_a - var_b = 0
        let expr_a = var_to_expr(var_a);
        let expr_b = var_to_expr(var_b);
        let residual = create_equality_residual(expr_a, expr_b);

        let origin = rumoca_ir_flat::EquationOrigin::Connection {
            lhs: var_a.as_str().to_string(),
            rhs: var_b.as_str().to_string(),
        };
        let eq = flat::Equation::new_array(residual, span, origin, scalar_count);
        flat.add_equation(eq);
    }

    Ok(())
}

// =============================================================================
// Task 2.3: Generate Flow Sum Equations (CONN-003, CONN-026)
// =============================================================================

/// Generate sum-to-zero equation for flow variables.
///
/// For n flow variables in a connection set: `sign_1*f1 + sign_2*f2 + ... + sign_n*fn = 0`
///
/// Per MLS §9.2 (CONN-026):
/// - Inside connectors (component ports): sign = +1
/// - Outside connectors (model boundary): sign = -1
pub(super) fn generate_flow_equation(
    flat: &mut flat::Model,
    variables: &[rumoca_core::VarName],
    scope: &str,
    interface_flow_vars_by_scope: &IndexMap<String, FlowVarSet>,
    span: rumoca_core::Span,
) -> Result<(), FlattenError> {
    if variables.is_empty() {
        return Ok(());
    }

    // Get scalar count from the first variable's dimensions (MLS §8.4)
    // All variables in a flow connection set should have the same dimensions.
    // First check for empty arrays (Real[0]) which have scalar_count=0.
    let first_count = variables
        .iter()
        .find_map(|var| resolve_flow_var_scalar_count(flat, var));
    if first_count == Some(0) {
        return Ok(());
    }
    let flow_sizes: Vec<usize> = variables
        .iter()
        .filter_map(|var| resolve_flow_var_scalar_count(flat, var))
        .collect();
    let has_scalar_flow = flow_sizes.contains(&1);
    let array_sizes: Vec<usize> = flow_sizes.iter().copied().filter(|&c| c > 1).collect();
    // Mixed scalar + array flow sets (e.g., scalar heat port connected to an array
    // of heat ports) represent one scalar Kirchhoff equation over all elements in
    // the set when there is exactly one array term.
    // If multiple array terms are present, keep array-sized scalarization.
    let scalar_count = if has_scalar_flow && array_sizes.len() == 1 {
        1
    } else {
        array_sizes.into_iter().next().unwrap_or(1)
    };

    // Mark all variables as connected
    for var in variables {
        mark_connected(flat, var);
    }

    // Create sum expression with proper signs per MLS §9.2
    // Inside connectors: +f, Outside connectors: -f
    let flow_exprs: Vec<rumoca_core::Expression> = variables
        .iter()
        .map(|var| {
            let expr = var_to_expr(var);
            if is_outside_flow_var_for_scope(var, scope, interface_flow_vars_by_scope) {
                // Outside connector: negate (sign = -1)
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Minus,
                    rhs: Box::new(expr),
                    span: rumoca_core::Span::DUMMY,
                }
            } else {
                // Inside connector: positive (sign = +1)
                expr
            }
        })
        .collect();
    let sum = create_sum(flow_exprs);

    // Build origin string with signs for clarity
    let signed_vars: Vec<String> = variables
        .iter()
        .map(|v| {
            if is_outside_flow_var_for_scope(v, scope, interface_flow_vars_by_scope) {
                format!("-{}", v.as_str())
            } else {
                v.as_str().to_string()
            }
        })
        .collect();
    let origin = rumoca_ir_flat::EquationOrigin::FlowSum {
        description: format!("{} = 0", signed_vars.join(" + ")),
    };
    let eq = flat::Equation::new_array(sum, span, origin, scalar_count);
    flat.add_equation(eq);

    Ok(())
}

fn is_outside_flow_var_for_scope(
    var_name: &rumoca_core::VarName,
    scope: &str,
    interface_flow_vars_by_scope: &IndexMap<String, FlowVarSet>,
) -> bool {
    interface_flow_vars_by_scope
        .get(scope)
        .is_some_and(|scope_vars| scope_vars.contains(var_name))
}

// =============================================================================
// Main Entry Point
// =============================================================================

/// Process all connections in the instance overlay.
///
/// MLS §9.2: For each connection set:
/// - Potential variables: v1 = v2 = ... = vn (n-1 equations)
/// - Flow variables: f1 + f2 + ... + fn = 0 (1 equation)
///
/// Additionally, per MLS §9.2: "For every outside connector of the model,
/// the sum of the corresponding flow variables is also set equal to zero."
/// This means unconnected flow variables get `flow_var = 0` equations.
/// Check if a connection involves a disabled component.
/// MLS §4.8: Conditional components with false conditions are disabled.
pub(crate) fn connection_involves_disabled(
    conn: &ast::InstanceConnection,
    disabled_components: &indexmap::IndexSet<rumoca_core::ComponentPath>,
) -> bool {
    for disabled in disabled_components {
        if conn.a.starts_with_component_path(disabled) {
            return true;
        }
        if conn.b.starts_with_component_path(disabled) {
            return true;
        }
    }

    false
}

/// Build a prefix-to-children index for O(1) sub-variable lookups.
///
/// Maps each dotted prefix to all descendant variable names.
/// For flat variables `["a.b.c", "a.b.d", "a.e"]`, produces:
/// - `"a.b"` → `["a.b.c", "a.b.d"]`
/// - `"a"` → `["a.b.c", "a.b.d", "a.e"]`
pub(super) fn build_prefix_children(
    flat: &flat::Model,
) -> FxHashMap<String, Vec<rumoca_core::VarName>> {
    let mut children: FxHashMap<String, Vec<rumoca_core::VarName>> = FxHashMap::default();
    for name in flat.variables.keys() {
        let s = name.as_str();
        for (i, ch) in s.char_indices() {
            if ch == '.' {
                let prefix = &s[..i];
                children
                    .entry(prefix.to_string())
                    .or_default()
                    .push(name.clone());
            }
        }
    }
    children
}

pub(crate) fn process_connections(
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    strict_validation: bool,
) -> Result<(), FlattenError> {
    // Collect all connections from class instances, excluding disabled components.
    // MLS §5.4: Redirect outer-prefixed connection paths to their inner equivalents.
    let mut owned_connections: Vec<ast::InstanceConnection> = Vec::new();

    for (_def_id, class_data) in &overlay.classes {
        for conn in &class_data.connections {
            // MLS §4.8: Skip connections involving disabled conditional components
            if connection_involves_disabled(conn, &overlay.disabled_components) {
                continue;
            }
            let redirected = redirect_connection_for_inner_outer(conn, overlay);
            owned_connections.push(redirected);
        }
    }

    materialize_weather_bus_fields(flat, &owned_connections);

    // Build prefix-to-children index once for O(1) sub-variable lookups
    let prefix_children = build_prefix_children(flat);

    let all_connections: Vec<&ast::InstanceConnection> = owned_connections.iter().collect();
    let var_index = ConnectionVarIndex::new(flat);

    #[cfg(feature = "tracing")]
    {
        tracing::debug!(
            connection_count = all_connections.len(),
            "processing flattened connections"
        );
        for conn in &all_connections {
            tracing::debug!(scope = %conn.scope, a = %conn.a, b = %conn.b, "flattened connection");
        }
    }

    // Validate connections first (Task 2.4)
    if strict_validation {
        validate_connections(
            &all_connections,
            flat,
            &overlay.type_roots,
            &prefix_children,
            &var_index,
        )?;
    }

    // Track which flow variables participate in connections at each scope.
    // Used to detect sub-component interface flows that need external flow=0.
    let flow_vars_at_scope =
        collect_flow_vars_by_scope(&all_connections, flat, &prefix_children, &var_index);

    let interface_flow_vars_by_scope =
        collect_interface_flow_vars_by_scope(&all_connections, flat, &prefix_children, &var_index);

    // Build connection sets (variables connected together)
    let (connection_sets, stream_interface_equation_count) =
        build_connection_sets(&all_connections, flat, &prefix_children, &var_index);
    flat.stream_interface_equation_count = stream_interface_equation_count;

    // Generate equations for each connection set
    for set in connection_sets {
        match set.kind {
            ConnectionKind::Flow => generate_flow_equation(
                flat,
                &set.variables,
                set.scope.as_str(),
                &interface_flow_vars_by_scope,
                set.span,
            )?,
            ConnectionKind::Potential => {
                generate_equality_equations(flat, &set.variables, set.span)?
            }
            ConnectionKind::Stream => mark_stream_connection_set(flat, &set.variables),
            ConnectionKind::StreamAlias => {
                generate_equality_equations(flat, &set.variables, set.span)?
            }
        }
    }

    // MLS §9.2: Generate equations for unconnected flow variables.
    // Flow variables not in any connection set get `flow_var = 0` equations.
    generate_unconnected_flow_equations(flat)?;

    // MLS §9.2: Generate flow=0 for interface flow variables not connected
    // at their parent scope or at the model boundary for standalone checking.
    generate_external_unconnected_flow_equations(
        flat,
        &flow_vars_at_scope,
        &all_connections,
        &prefix_children,
        &var_index,
    )?;

    Ok(())
}

/// Generate `flow_var = 0` equations for unconnected flow variables.
///
/// Per MLS §9.2: "For every outside connector of the model, the sum of
/// the corresponding flow variables is also set equal to zero."
/// For a single unconnected flow variable, this means `flow_var = 0`.
fn generate_unconnected_flow_equations(flat: &mut flat::Model) -> Result<(), FlattenError> {
    // Find all flow variables that are NOT marked as connected
    let unconnected_flows: Vec<(rumoca_core::VarName, usize)> = flat
        .variables
        .iter()
        .filter(|(_, var)| var.flow && !var.connected)
        .map(|(name, var)| (name.clone(), compute_var_scalar_count(var)))
        .collect();

    for (var_name, scalar_count) in unconnected_flows {
        // Skip empty arrays (Real[0]) — no equations needed
        if scalar_count == 0 {
            continue;
        }

        // Per MLS §9.2, unconnected flow variables always get zero-flow
        // equations, even if their parent record appears in a body equation.
        // Both record-level body equations (like `port_p.Phi = Phi`) AND
        // scalar zero-flow equations (like `port_p.Phi.re = 0`) are generated.
        // The balance check counts both.

        // Create equation: flow_var = 0 (in residual form: flow_var - 0 = flow_var)
        let var_expr = var_to_expr(&var_name);

        let origin = rumoca_ir_flat::EquationOrigin::UnconnectedFlow {
            variable: var_name.as_str().to_string(),
        };
        let eq = flat::Equation::new_array(var_expr, Span::DUMMY, origin, scalar_count);
        flat.add_equation(eq);

        // Note: We do NOT mark the variable as connected here because it's
        // semantically UNCONNECTED. The `connected` flag indicates involvement
        // in actual connection equations (flow sums with other components),
        // not just having any equation. This distinction is important for
        // interface flow detection per MLS §4.7.
    }

    Ok(())
}

/// Collect flow variables that participate in connections at each scope level.
///
/// Returns a map from scope string to the set of flow variable names that appear
/// in connections at that scope. Used to detect sub-component interface connectors
/// that are internally connected but not externally connected.
fn collect_flow_vars_by_scope(
    connections: &[&ast::InstanceConnection],
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) -> IndexMap<String, FlowVarSet> {
    let mut result: IndexMap<String, FlowVarSet> = IndexMap::default();

    for conn in connections {
        let path_a = conn.a.to_flat_string();
        let path_b = conn.b.to_flat_string();

        // Collect flow sub-variables for each side of the connection
        let scope_set = result.entry(conn.scope.clone()).or_default();
        collect_flow_vars_from_conn_path(flat, &path_a, scope_set, prefix_children, var_index);
        collect_flow_vars_from_conn_path(flat, &path_b, scope_set, prefix_children, var_index);
    }

    result
}

/// Add flow variables from a connection path to the given set.
fn collect_flow_vars_from_conn_path(
    flat: &flat::Model,
    path: &str,
    dest: &mut FlowVarSet,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) {
    let var_name = rumoca_core::VarName::new(path);

    // Check if it's a direct flow variable
    if let Some(var) = flat.variables.get(&var_name) {
        if var.flow {
            dest.insert(var_name);
        }
        return;
    }

    // It's a connector - find flow sub-variables
    let subs = find_sub_variables_indexed(path, prefix_children, var_index);
    for sub in subs {
        if flat.variables.get(&sub).is_some_and(|v| v.flow) {
            dest.insert(sub);
        }
    }
}

/// Collect flow variables on interface connectors at each scope level (MLS §9.2).
///
/// An interface connector is referenced as a single identifier (no dots) relative
/// to its connection scope. These are the model boundary connectors that may need
/// `flow = 0` at the parent scope if not connected there.
fn collect_interface_flow_vars_by_scope(
    connections: &[&ast::InstanceConnection],
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) -> IndexMap<String, FlowVarSet> {
    let mut result: IndexMap<String, FlowVarSet> = IndexMap::default();

    for conn in connections {
        let scope = &conn.scope;

        for path_qn in [&conn.a, &conn.b] {
            let path = path_qn.to_flat_string();

            let Some(relative) = relative_component_path(&path, scope) else {
                continue;
            };

            // Interface connector: single identifier (no top-level dots in relative path).
            // Dots inside bracketed subscripts are part of subscript expressions.
            if is_single_identifier_path(&relative) {
                let scope_set = result.entry(scope.clone()).or_default();
                collect_flow_vars_from_conn_path(
                    flat,
                    &path,
                    scope_set,
                    prefix_children,
                    var_index,
                );
            }
        }
    }

    result
}

#[cfg(test)]
fn is_single_identifier_relative_path(relative: &str) -> bool {
    is_single_identifier_path(&rumoca_core::ComponentPath::from_flat_path(relative))
}

fn is_single_identifier_path(path: &rumoca_core::ComponentPath) -> bool {
    path.len() == 1
}

fn relative_component_path(path: &str, scope: &str) -> Option<rumoca_core::ComponentPath> {
    let path = rumoca_core::ComponentPath::from_flat_path(path);
    let scope = rumoca_core::ComponentPath::from_flat_path(scope);
    if scope.is_root() {
        return Some(path);
    }
    component_path_has_scope_prefix(&path, &scope)
        .then(|| path.suffix_from(scope.len()))
        .flatten()
}

fn component_path_has_scope_prefix(
    path: &rumoca_core::ComponentPath,
    scope: &rumoca_core::ComponentPath,
) -> bool {
    scope.len() <= path.len()
        && path
            .parts()
            .iter()
            .zip(scope.parts().iter())
            .all(|(path_part, scope_part)| same_scope_segment(path_part, scope_part))
}

fn is_proper_component_path_ancestor(
    candidate: &rumoca_core::ComponentPath,
    scope: &rumoca_core::ComponentPath,
) -> bool {
    candidate.len() < scope.len() && component_path_has_scope_prefix(scope, candidate)
}

fn same_scope_segment(path_part: &str, scope_part: &str) -> bool {
    strip_array_index(path_part) == strip_array_index(scope_part)
}

/// Check if a flow variable is connected at any scope that is a proper
/// ancestor of the given scope (MLS §9.2).
fn is_at_ancestor_scope(
    var_name: &rumoca_core::VarName,
    scope: &str,
    flow_vars_at_scope: &IndexMap<String, FlowVarSet>,
) -> bool {
    let scope_path = rumoca_core::ComponentPath::from_flat_path(scope);
    for (s, vars) in flow_vars_at_scope {
        let candidate = rumoca_core::ComponentPath::from_flat_path(s);
        let is_ancestor = is_proper_component_path_ancestor(&candidate, &scope_path);

        if is_ancestor && vars.contains(var_name) {
            return true;
        }
    }
    false
}

/// Generate `flow = 0` for interface flow variables not connected externally.
///
/// Per MLS §9.2: When a connector is connected internally but not at the
/// enclosing scope, its flow variables need `flow = 0`. This handles:
/// - Sub-component interface connectors not connected at the parent level
/// - flat::Model-level external connectors for standalone checking (no parent)
///
/// Interface connectors are identified by being single identifiers relative
/// to their connection scope, which correctly handles record-typed flows
/// (e.g., Complex `Phi.re`/`Phi.im`) without dot-count heuristics.
fn generate_external_unconnected_flow_equations(
    flat: &mut flat::Model,
    flow_vars_at_scope: &IndexMap<String, FlowVarSet>,
    connections: &[&ast::InstanceConnection],
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Result<(), FlattenError> {
    let interface_flow_vars_by_scope =
        collect_interface_flow_vars_by_scope(connections, flat, prefix_children, var_index);
    let need_flow_zero =
        find_unconnected_interface_flows(&interface_flow_vars_by_scope, flow_vars_at_scope, flat);

    for (var_name, scalar_count) in need_flow_zero {
        // Skip empty arrays (Real[0]) — no equations needed
        if scalar_count == 0 {
            continue;
        }
        let origin = rumoca_ir_flat::EquationOrigin::UnconnectedFlow {
            variable: var_name.as_str().to_string(),
        };
        let eq =
            flat::Equation::new_array(var_to_expr(&var_name), Span::DUMMY, origin, scalar_count);
        flat.add_equation(eq);
    }

    Ok(())
}

/// Find interface flow variables that are not connected at any ancestor scope.
fn find_unconnected_interface_flows(
    interface_flows: &IndexMap<String, FlowVarSet>,
    flow_vars_at_scope: &IndexMap<String, FlowVarSet>,
    flat: &flat::Model,
) -> IndexMap<rumoca_core::VarName, usize> {
    let mut result: IndexMap<rumoca_core::VarName, usize> = IndexMap::default();

    for (scope, interface_vars) in interface_flows {
        for var_name in interface_vars {
            if result.contains_key(var_name) {
                continue;
            }

            // Root scope has no parent → always needs flow=0 for standalone checking.
            // Non-root scopes: check if connected at any ancestor scope.
            let connected_externally =
                !scope.is_empty() && is_at_ancestor_scope(var_name, scope, flow_vars_at_scope);

            if !connected_externally && let Some(var) = flat.variables.get(var_name) {
                result.insert(var_name.clone(), compute_var_scalar_count(var));
            }
        }
    }

    result
}

/// Redirect a ast::QualifiedName if its flat string starts with an outer prefix (MLS §5.4).
///
/// When outer components are not instantiated, connection paths like
/// `initialStep.stateGraphRoot.resume` must be redirected to `stateGraphRoot.resume`.
fn redirect_qualified_name(
    qn: &mut ast::QualifiedName,
    outer_to_inner: &ast::AstIndexMap<String, String>,
) {
    if outer_to_inner.is_empty() {
        return;
    }
    let flat = qn.to_flat_string();
    for (outer_prefix, inner_prefix) in outer_to_inner {
        if flat == *outer_prefix || flat.starts_with(&format!("{outer_prefix}.")) {
            let new_flat = if flat == *outer_prefix {
                inner_prefix.clone()
            } else {
                format!("{}{}", inner_prefix, &flat[outer_prefix.len()..])
            };
            *qn = ast::QualifiedName::from_dotted(&new_flat);
            return;
        }
    }
}

/// MLS §5.4: Apply outer→inner and inner-outer bridge redirects to a connection.
///
/// First pass: redirect pure `outer` component references to their matching `inner`.
/// Second pass: if no redirect happened (same-level connection), redirect `inner outer`
/// component references to the parent's inner for correct flow equation scoping.
/// In both cases, reset the scope to root so flow sums merge properly.
fn redirect_connection_for_inner_outer(
    conn: &ast::InstanceConnection,
    overlay: &ast::InstanceOverlay,
) -> ast::InstanceConnection {
    let mut redirected = conn.clone();
    let a_before = redirected.a.to_flat_string();
    let b_before = redirected.b.to_flat_string();

    // First pass: redirect pure outer→inner
    redirect_qualified_name(&mut redirected.a, &overlay.outer_prefix_to_inner);
    redirect_qualified_name(&mut redirected.b, &overlay.outer_prefix_to_inner);
    let a_after = redirected.a.to_flat_string();
    let b_after = redirected.b.to_flat_string();

    if a_before != a_after || b_before != b_after {
        redirected.scope = String::new();
        return redirected;
    }

    // Second pass: inner outer bridge redirect (only when first pass had no effect)
    if !overlay.inner_outer_to_parent_inner.is_empty() {
        redirect_qualified_name(&mut redirected.a, &overlay.inner_outer_to_parent_inner);
        redirect_qualified_name(&mut redirected.b, &overlay.inner_outer_to_parent_inner);
        let a_bridged = a_after != redirected.a.to_flat_string();
        let b_bridged = b_after != redirected.b.to_flat_string();
        if a_bridged || b_bridged {
            redirected.scope = String::new();
        }
    }
    redirected
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod equation_generation_tests {
    use super::is_single_identifier_relative_path;

    #[test]
    fn single_identifier_relative_path_ignores_dot_inside_subscript_expression() {
        assert!(is_single_identifier_relative_path("plug[data.medium]"));
        assert!(is_single_identifier_relative_path("plug[medium.nXi]"));
    }

    #[test]
    fn single_identifier_relative_path_rejects_top_level_member_access() {
        assert!(!is_single_identifier_relative_path("plug.p"));
        assert!(!is_single_identifier_relative_path("plug[data.medium].p"));
    }
}
