use super::*;
use indexmap::IndexSet;
use rumoca_ir_ast as ast;

type FlowVarSet = IndexSet<rumoca_core::VarName>;
pub(super) type InterfaceStreamEndpointsByScope =
    IndexMap<String, IndexMap<rumoca_core::VarName, rumoca_core::Span>>;
type InterfaceConnectorRootSet = IndexSet<rumoca_core::ComponentPath>;
pub(super) type InterfaceConnectorRootsByScope = IndexMap<String, InterfaceConnectorRootSet>;

/// Compute scalar count from variable dimensions.
///
/// For array variables, scalar_count = product of dimensions.
/// For scalars (empty dims), returns 1.
fn compute_var_scalar_count(var: &flat::Variable) -> usize {
    scalar_count_of_dims(&var.dims)
}

fn add_connection_equation(
    flat: &mut flat::Model,
    equation: flat::Equation,
    preferred_dims: Option<&[i64]>,
) -> Result<(), FlattenError> {
    let equation_index = flat.equations.len();
    let family = crate::equations::array_family::structured_array_equation_family(
        equation_index,
        &equation,
        preferred_dims,
    )?;
    flat.add_equation(equation);
    if let Some(family) = family {
        flat.structured_equations.push(family);
    }
    Ok(())
}

/// Scalar leaves denoted by one connection-set member.
///
/// MLS §9.2 generates one scalar equation per matched leaf and MLS §4.8 counts
/// those scalars when balancing the model, so an element or slice endpoint
/// contributes the leaves of what it *denotes* (MLS §10.5), not one leaf per
/// subscripted path.
fn resolve_var_scalar_count(flat: &flat::Model, var: &rumoca_core::VarName) -> Option<usize> {
    if let Some(dims) = connection_endpoint_dims(flat, var) {
        return Some(scalar_count_of_dims(&dims));
    }
    strip_embedded_array_indices(var.as_str()).map(|_| 1)
}

/// Scalar count of a dimension list, sharing one clamp with
/// [`compute_var_scalar_count`] so a declaration and one of its elements can
/// never be counted by two different rules.
fn scalar_count_of_dims(dims: &[i64]) -> usize {
    if dims.is_empty() {
        1
    } else {
        dims.iter().copied().map(|d| d.max(0)).product::<i64>() as usize
    }
}

pub(super) fn strip_embedded_array_indices(path: &str) -> Option<String> {
    let parts = crate::path_utils::segments(path);
    if !parts
        .iter()
        .any(|part| rumoca_core::split_trailing_subscript_suffix(part).is_some())
    {
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

/// Generate one connection equation for every connected outside stream
/// connector (MLS §15.1, STRM-004). Inside stream connectors generate no
/// equation (STRM-005).
///
/// Per MLS §15.2 the right-hand side is the mixing enthalpy of the connection
/// set declared inside the connector's own model, with the connector itself
/// excluded — *not* `inStream()` of the connector, which by definition looks in
/// the opposite direction (at the set one level up, where the same connector is
/// an inside connector).
///
/// These equations are created before stream-operator rewriting so any nested
/// `inStream()` of a further outside peer is expanded by that pass.
fn generate_outside_stream_equations(
    flat: &mut flat::Model,
    endpoints_by_scope: &InterfaceStreamEndpointsByScope,
    stream_endpoints: &super::stream_operators::StreamConnectionEndpoints,
) -> Result<(), FlattenError> {
    for endpoints in endpoints_by_scope.values() {
        for (stream, span) in endpoints {
            let scalar_count = resolve_var_scalar_count(flat, stream).unwrap_or(1);
            if scalar_count == 0 {
                continue;
            }
            let provenance =
                require_connection_provenance(*span, "outside stream connection equation")?;
            let stream_expr = connection_member_expr(flat, stream, provenance);
            // A connector that reached interface discovery without joining a
            // stream connection set at that scope is the MLS §15.2 unconnected
            // case; it keeps the conceptual `inStream()` right-hand side.
            let mix = stream_endpoints
                .outside_equation_rhs(stream, provenance.span())
                .unwrap_or_else(|| rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::Reference::generated("inStream"),
                    args: vec![stream_expr.clone()],
                    is_constructor: false,
                    span: provenance.span(),
                });
            let residual = create_equality_residual(stream_expr, mix, provenance);
            let origin = rumoca_ir_flat::EquationOrigin::Connection {
                lhs: stream.as_str().to_string(),
                rhs: format!("inStream({stream})"),
            };
            let preferred_dims = connection_endpoint_dims(flat, stream);
            let equation = flat::Equation::new_array(residual, *span, origin, scalar_count);
            add_connection_equation(flat, equation, preferred_dims.as_deref())?;
            mark_connected(flat, stream);
        }
    }
    Ok(())
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
    oc_forest: &mut crate::vcg::OverconstrainedEquationForest,
) -> Result<(), FlattenError> {
    let provenance = require_connection_provenance(span, "connection equality equation")?;
    // Generate chain of equality equations: v1 - v2 = 0, v2 - v3 = 0, ...
    for window in variables.windows(2) {
        let var_a = &window[0];
        let var_b = &window[1];

        // MLS §10.5: an element/slice member denotes the dimensions its
        // subscripts leave, so both sides are measured by what they denote.
        let lhs_dims = connection_endpoint_dims(flat, var_a);
        let rhs_dims = connection_endpoint_dims(flat, var_b);
        let lhs_size = lhs_dims.as_deref().map(scalar_count_of_dims);
        let rhs_size = rhs_dims.as_deref().map(scalar_count_of_dims);
        if let (Some(lhs_size), Some(rhs_size)) = (lhs_size, rhs_size)
            && lhs_size != rhs_size
        {
            return Err(FlattenError::incompatible_connectors(
                var_a.as_str(),
                var_b.as_str(),
                span,
            ));
        }
        let scalar_count = lhs_size.or(rhs_size).unwrap_or(1);

        // Skip empty arrays (Real[0]) — no equations needed
        if scalar_count == 0 {
            continue;
        }

        match oc_forest.generated_equality_disposition(flat, var_a, var_b)? {
            crate::vcg::GeneratedEqualityDisposition::Retain => {}
            crate::vcg::GeneratedEqualityDisposition::Omit => continue,
            crate::vcg::GeneratedEqualityDisposition::Replace {
                lhs_record,
                rhs_record,
                constraint_size,
            } => {
                generate_equality_constraint_equation(
                    flat,
                    &lhs_record,
                    &rhs_record,
                    constraint_size,
                    span,
                )?;
                continue;
            }
        }

        // Mark both variables as connected
        mark_connected(flat, var_a);
        mark_connected(flat, var_b);

        // Create residual: var_a - var_b = 0
        let expr_a = connection_member_expr(flat, var_a, provenance);
        let expr_b = connection_member_expr(flat, var_b, provenance);
        let residual = create_equality_residual(expr_a, expr_b, provenance);

        let origin = rumoca_ir_flat::EquationOrigin::Connection {
            lhs: var_a.as_str().to_string(),
            rhs: var_b.as_str().to_string(),
        };
        let preferred_dims = lhs_dims
            .into_iter()
            .chain(rhs_dims)
            .find(|dims| scalar_count_of_dims(dims) == scalar_count);
        let eq = flat::Equation::new_array(residual, span, origin, scalar_count);
        add_connection_equation(flat, eq, preferred_dims.as_deref())?;
    }

    Ok(())
}

/// One whole-record argument of a generated `equalityConstraint` call.
///
/// The Flat record instance carries the exact `ComponentReference` proven when
/// the instance was recorded; keeping it on the argument lets later record
/// lowering project fields by identity instead of re-deriving them from the
/// rendered name.
fn record_instance_expr(
    rendered: &str,
    instance: &flat::RecordInstance,
    span: ProvenanceSpan,
) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(
            rendered,
            instance.component_ref.clone(),
        )
        .with_instance_id(instance.instance_id),
        subscripts: Vec::new(),
        span: span.span(),
    }
}

fn generate_equality_constraint_equation(
    flat: &mut flat::Model,
    lhs_record: &str,
    rhs_record: &str,
    constraint_size: usize,
    span: rumoca_core::Span,
) -> Result<(), FlattenError> {
    let provenance = require_connection_provenance(span, "overconstrained equalityConstraint")?;
    let lhs_name = rumoca_core::VarName::new(lhs_record);
    let rhs_name = rumoca_core::VarName::new(rhs_record);
    let lhs_instance = flat.record_instances.get(&lhs_name).ok_or_else(|| {
        FlattenError::internal(format!(
            "overconstrained record `{lhs_record}` is absent from Flat record metadata"
        ))
    })?;
    let rhs_instance = flat.record_instances.get(&rhs_name).ok_or_else(|| {
        FlattenError::internal(format!(
            "overconstrained record `{rhs_record}` is absent from Flat record metadata"
        ))
    })?;
    if lhs_instance.type_def_id != rhs_instance.type_def_id {
        return Err(FlattenError::internal(format!(
            "overconstrained record edge `{lhs_record}`--`{rhs_record}` has incompatible record types"
        )));
    }
    let record_type = flat
        .record_types
        .get(&lhs_instance.type_def_id)
        .ok_or_else(|| {
            FlattenError::internal(format!(
                "overconstrained record `{lhs_record}` has no Flat record type metadata"
            ))
        })?;
    let function_name = format!("{}.equalityConstraint", record_type.name);
    // MLS §9.3.1: both arguments are the whole overdetermined record instances.
    // Record-parameter lowering projects each declared field off them, which
    // requires the exact structured identity of the instance rather than its
    // rendered flat name, so the argument references are built from the Flat
    // record metadata that already proved that identity.
    let lhs_arg = record_instance_expr(lhs_record, lhs_instance, provenance);
    let rhs_arg = record_instance_expr(rhs_record, rhs_instance, provenance);
    let residual = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::generated(function_name.clone()),
        args: vec![lhs_arg, rhs_arg],
        is_constructor: false,
        span: provenance.span(),
    };
    let origin = flat::EquationOrigin::Connection {
        lhs: format!("zeros({constraint_size})"),
        rhs: format!("{function_name}({lhs_record}, {rhs_record})"),
    };
    add_connection_equation(
        flat,
        flat::Equation::new_array(residual, span, origin, constraint_size),
        Some(&[i64::try_from(constraint_size).map_err(|_| {
            FlattenError::unsupported_equation(
                "equalityConstraint output exceeds structured-domain index range",
                span,
            )
        })?]),
    )?;
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
    let provenance = require_connection_provenance(span, "connection flow equation")?;

    // Get scalar count from the first variable's dimensions (MLS §8.4)
    // All variables in a flow connection set should have the same dimensions.
    // First check for empty arrays (Real[0]) which have scalar_count=0.
    let first_count = variables
        .iter()
        .find_map(|var| resolve_var_scalar_count(flat, var));
    if first_count == Some(0) {
        return Ok(());
    }
    let flow_sizes: Vec<usize> = variables
        .iter()
        .filter_map(|var| resolve_var_scalar_count(flat, var))
        .collect();
    let has_scalar_flow = flow_sizes.contains(&1);
    let array_sizes: Vec<usize> = flow_sizes.iter().copied().filter(|&c| c > 1).collect();
    let array_var_sizes: Vec<_> = variables
        .iter()
        .filter_map(|var| {
            resolve_var_scalar_count(flat, var)
                .filter(|&count| count > 1)
                .map(|count| (var, count))
        })
        .collect();
    if let Some((first_var, first_size)) = array_var_sizes.first()
        && let Some((other_var, _)) = array_var_sizes.iter().find(|(_, size)| size != first_size)
    {
        return Err(FlattenError::incompatible_connectors(
            first_var.as_str(),
            other_var.as_str(),
            span,
        ));
    }
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
            let expr = connection_member_expr(flat, var, provenance);
            if is_outside_flow_var_for_scope(var, scope, interface_flow_vars_by_scope) {
                // Outside connector: negate (sign = -1)
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Minus,
                    rhs: Box::new(expr),
                    span: provenance.span(),
                }
            } else {
                // Inside connector: positive (sign = +1)
                expr
            }
        })
        .collect();
    let sum = create_sum(flow_exprs, provenance);

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
    let preferred_dims = variables
        .iter()
        .filter_map(|name| connection_endpoint_dims(flat, name))
        .find(|dims| scalar_count_of_dims(dims) == scalar_count);
    let eq = flat::Equation::new_array(sum, span, origin, scalar_count);
    add_connection_equation(flat, eq, preferred_dims.as_deref())?;

    Ok(())
}

fn is_outside_flow_var_for_scope(
    var_name: &rumoca_core::VarName,
    scope: &str,
    interface_flow_vars_by_scope: &IndexMap<String, FlowVarSet>,
) -> bool {
    let Some(scope_vars) = interface_flow_vars_by_scope.get(scope) else {
        return false;
    };
    has_outside_connector_role(var_name, |candidate| scope_vars.contains(candidate))
}

/// MLS §15.2 counterpart of [`is_outside_flow_var_for_scope`] for the stream
/// members of a connector, resolved against the same per-scope interface
/// discovery. The stream mixing formula flips the flow sign for outside
/// connectors exactly like the §9.2 flow sum does.
pub(super) fn is_outside_stream_var_for_scope(
    var_name: &rumoca_core::VarName,
    scope: &str,
    interface_stream_endpoints_by_scope: &InterfaceStreamEndpointsByScope,
) -> bool {
    let Some(scope_endpoints) = interface_stream_endpoints_by_scope.get(scope) else {
        return false;
    };
    has_outside_connector_role(var_name, |candidate| {
        scope_endpoints.contains_key(candidate)
    })
}

/// Shared MLS §9.1.2 role lookup for the flow and stream connector members of
/// one scope.
///
/// Connector-array expansion can generate scalar members such as `plug.pin[1].i`
/// while interface discovery records the member as `plug.pin.i`. They denote the
/// same connector and therefore have the same inside/outside role.
fn has_outside_connector_role(
    var_name: &rumoca_core::VarName,
    is_interface_member: impl Fn(&rumoca_core::VarName) -> bool,
) -> bool {
    if is_interface_member(var_name) {
        return true;
    }
    strip_embedded_array_indices(var_name.as_str())
        .is_some_and(|base_name| is_interface_member(&rumoca_core::VarName::new(base_name)))
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
    oc_forest: &mut crate::vcg::OverconstrainedEquationForest,
) -> Result<(), FlattenError> {
    // Build prefix-to-children index once for O(1) sub-variable lookups
    let prefix_children = build_prefix_children(flat);

    // Collect all connections from class instances, excluding disabled components.
    // MLS §5.4: Redirect outer-prefixed connection paths to their inner equivalents.
    let mut owned_connections: Vec<ast::InstanceConnection> = Vec::new();

    for (_def_id, class_data) in &overlay.classes {
        // SPEC_0032 §1: compact connection families stay authoritative in the
        // instance overlay; derive their scalar members lazily here instead of
        // materializing a second copy of the whole overlay up front.
        for conn in rumoca_eval_ast::connection::scalar_connection_view(&class_data.connections) {
            let conn = conn.map_err(crate::structured_connection_error)?;
            // MLS §4.8: Skip connections involving disabled conditional components
            if connection_involves_disabled(&conn, &overlay.disabled_components) {
                continue;
            }
            let redirected = redirect_connection_for_inner_outer(&conn, overlay);
            owned_connections.push(redirected);
        }
    }

    let all_connections: Vec<&ast::InstanceConnection> = owned_connections.iter().collect();
    let var_index = ConnectionVarIndex::new(flat);
    let endpoint_index = ConnectionEndpointIndex::new(overlay);

    // MLS §10.5: an endpoint subscript must select along a declared dimension.
    // Checked before any path matching, because path matching normalizes
    // indices away and would otherwise connect the whole component the
    // subscript was meant to index.
    endpoint_index.check_connection_endpoint_subscripts(&all_connections)?;

    // MLS §9.1.3 augmentation must happen before connection-set construction.
    // Until the union elaboration exists, reject the unsupported case instead
    // of silently connecting only the intersection of declared bus members.
    reject_expandable_connector_augmentation(
        &all_connections,
        flat,
        &endpoint_index,
        &prefix_children,
        &var_index,
    )?;

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

    let interface_connector_roots_by_scope = collect_interface_connector_roots_by_scope(overlay);
    let interface_flow_vars_by_scope = collect_interface_flow_vars_by_scope(
        &all_connections,
        flat,
        &prefix_children,
        &var_index,
        &interface_connector_roots_by_scope,
    );
    let interface_stream_endpoints_by_scope = collect_interface_stream_endpoints_by_scope(
        &all_connections,
        flat,
        &prefix_children,
        &var_index,
        &interface_connector_roots_by_scope,
    );

    // Build connection sets (variables connected together)
    let (connection_sets, stream_sets) =
        build_connection_sets(&all_connections, flat, &prefix_children, &var_index)?;

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
                generate_equality_equations(flat, &set.variables, set.span, oc_forest)?;
            }
        }
    }

    for stream_set in &stream_sets {
        mark_stream_connection_set(flat, &stream_set.variables);
    }
    let stream_endpoints = super::stream_operators::build_stream_connection_endpoints(
        flat,
        &stream_sets,
        &interface_stream_endpoints_by_scope,
    )?;
    generate_outside_stream_equations(
        flat,
        &interface_stream_endpoints_by_scope,
        &stream_endpoints,
    )?;

    // MLS §15.2-15.3: eliminate inStream()/actualStream() while the semantic
    // stream connection sets and their associated flow variables are present.
    super::stream_operators::rewrite_stream_operators(flat, &stream_sets, &stream_endpoints)?;

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
        &interface_connector_roots_by_scope,
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
        let provenance =
            require_flat_variable_provenance(flat, &var_name, "unconnected flow equation")?;
        let var_expr = var_to_expr(&var_name, provenance);

        let origin = rumoca_ir_flat::EquationOrigin::UnconnectedFlow {
            variable: var_name.as_str().to_string(),
        };
        let preferred_dims = flat
            .variables
            .get(&var_name)
            .map(|variable| variable.dims.clone());
        let eq = flat::Equation::new_array(var_expr, provenance.span(), origin, scalar_count);
        add_connection_equation(flat, eq, preferred_dims.as_deref())?;

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

fn collect_stream_vars_from_conn_path(
    flat: &flat::Model,
    path: &str,
    dest: &mut IndexMap<rumoca_core::VarName, rumoca_core::Span>,
    span: rumoca_core::Span,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) {
    let var_name = rumoca_core::VarName::new(path);
    if let Some(variable) = flat.variables.get(&var_name) {
        if variable.stream {
            dest.entry(var_name).or_insert(span);
        }
        return;
    }

    for sub in find_sub_variables_indexed(path, prefix_children, var_index) {
        if flat
            .variables
            .get(&sub)
            .is_some_and(|variable| variable.stream)
        {
            dest.entry(sub).or_insert(span);
        }
    }
}

fn collect_interface_connector_roots_by_scope(
    overlay: &ast::InstanceOverlay,
) -> InterfaceConnectorRootsByScope {
    let mut result: InterfaceConnectorRootsByScope = IndexMap::default();

    for instance in overlay.components.values() {
        if !instance.is_connector_type || instance.is_protected {
            continue;
        }
        let path = instance.qualified_name.to_component_path();
        let Some(parent) = path.parent() else {
            continue;
        };
        result
            .entry(parent.to_flat_string())
            .or_default()
            .insert(path);
    }

    result
}

/// Collect flow variables on interface connectors at each scope level (MLS §9.2).
///
/// An interface connector is a public connector-typed component declared directly
/// in the connection scope. Connection paths can name the connector itself or a
/// nested connector member below that root, e.g. `plug.pin`.
fn collect_interface_flow_vars_by_scope(
    connections: &[&ast::InstanceConnection],
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
    interface_connector_roots_by_scope: &InterfaceConnectorRootsByScope,
) -> IndexMap<String, FlowVarSet> {
    let mut result: IndexMap<String, FlowVarSet> = IndexMap::default();

    for conn in connections {
        let scope = &conn.scope;

        for path_qn in [&conn.a, &conn.b] {
            let path = path_qn.to_flat_string();
            if is_interface_connection_path_for_scope(
                &path,
                scope,
                interface_connector_roots_by_scope,
            ) {
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

fn collect_interface_stream_endpoints_by_scope(
    connections: &[&ast::InstanceConnection],
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
    interface_connector_roots_by_scope: &InterfaceConnectorRootsByScope,
) -> InterfaceStreamEndpointsByScope {
    let mut result = InterfaceStreamEndpointsByScope::default();
    for conn in connections {
        for path_qn in [&conn.a, &conn.b] {
            let path = path_qn.to_flat_string();
            if !is_interface_connection_path_for_scope(
                &path,
                &conn.scope,
                interface_connector_roots_by_scope,
            ) {
                continue;
            }
            collect_stream_vars_from_conn_path(
                flat,
                &path,
                result.entry(conn.scope.clone()).or_default(),
                conn.span,
                prefix_children,
                var_index,
            );
        }
    }
    result
}

pub(super) fn is_interface_connection_path_for_scope(
    path: &str,
    scope: &str,
    interface_connector_roots_by_scope: &InterfaceConnectorRootsByScope,
) -> bool {
    let path = rumoca_core::ComponentPath::from_flat_path(path);
    if let Some(scope_roots) = interface_connector_roots_by_scope.get(scope)
        && scope_roots
            .iter()
            .any(|root| path == *root || path.starts_with(root))
    {
        return true;
    }

    relative_component_path_from_path(&path, scope)
        .is_some_and(|relative| is_single_identifier_path(&relative))
}

#[cfg(test)]
fn is_single_identifier_relative_path(relative: &str) -> bool {
    is_single_identifier_path(&rumoca_core::ComponentPath::from_flat_path(relative))
}

fn is_single_identifier_path(path: &rumoca_core::ComponentPath) -> bool {
    path.len() == 1
}

fn relative_component_path_from_path(
    path: &rumoca_core::ComponentPath,
    scope: &str,
) -> Option<rumoca_core::ComponentPath> {
    let scope = rumoca_core::ComponentPath::from_flat_path(scope);
    if scope.is_root() {
        return Some(path.clone());
    }
    component_path_has_scope_prefix(path, &scope)
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
    interface_connector_roots_by_scope: &InterfaceConnectorRootsByScope,
) -> Result<(), FlattenError> {
    let interface_flow_vars_by_scope = collect_interface_flow_vars_by_scope(
        connections,
        flat,
        prefix_children,
        var_index,
        interface_connector_roots_by_scope,
    );
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
        let provenance = require_flat_variable_provenance(
            flat,
            &var_name,
            "external unconnected flow equation",
        )?;
        let preferred_dims = flat
            .variables
            .get(&var_name)
            .map(|variable| variable.dims.clone());
        let eq = flat::Equation::new_array(
            var_to_expr(&var_name, provenance),
            provenance.span(),
            origin,
            scalar_count,
        );
        add_connection_equation(flat, eq, preferred_dims.as_deref())?;
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

fn bridge_scope_matches_connection_scope(inner_outer_prefix: &str, connection_scope: &str) -> bool {
    let bridge_scope = ast::QualifiedName::from_dotted(inner_outer_prefix)
        .parent()
        .unwrap_or_default();
    bridge_scope == ast::QualifiedName::from_dotted(connection_scope)
}

fn redirect_inner_outer_bridge_for_scope(
    qn: &mut ast::QualifiedName,
    inner_outer_to_parent_inner: &ast::AstIndexMap<String, String>,
    connection_scope: &str,
) {
    if inner_outer_to_parent_inner.is_empty() {
        return;
    }
    let flat = qn.to_flat_string();
    for (inner_outer_prefix, parent_inner_prefix) in inner_outer_to_parent_inner {
        if !bridge_scope_matches_connection_scope(inner_outer_prefix, connection_scope) {
            continue;
        }
        if flat == *inner_outer_prefix || flat.starts_with(&format!("{inner_outer_prefix}.")) {
            let new_flat = if flat == *inner_outer_prefix {
                parent_inner_prefix.clone()
            } else {
                format!(
                    "{}{}",
                    parent_inner_prefix,
                    &flat[inner_outer_prefix.len()..]
                )
            };
            *qn = ast::QualifiedName::from_dotted(&new_flat);
            return;
        }
    }
}

/// MLS §5.4: Apply outer→inner and inner-outer bridge redirects to a connection.
///
/// First pass: redirect pure `outer` component references to their matching `inner`.
/// Second pass: if no redirect happened, redirect same-level `inner outer`
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
        redirect_inner_outer_bridge_for_scope(
            &mut redirected.a,
            &overlay.inner_outer_to_parent_inner,
            &conn.scope,
        );
        redirect_inner_outer_bridge_for_scope(
            &mut redirected.b,
            &overlay.inner_outer_to_parent_inner,
            &conn.scope,
        );
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
    use super::*;

    fn conn(a: &str, b: &str, scope: &str) -> ast::InstanceConnection {
        ast::InstanceConnection {
            a: ast::QualifiedName::from_dotted(a),
            b: ast::QualifiedName::from_dotted(b),
            connector_type: None,
            span: rumoca_core::Span::DUMMY,
            scope: scope.to_string(),
            family: None,
        }
    }

    fn overlay_with_inner_outer_bridge() -> ast::InstanceOverlay {
        let mut overlay = ast::InstanceOverlay::default();
        overlay.inner_outer_to_parent_inner.insert(
            "tankController.makeProduct.stateGraphRoot".to_string(),
            "stateGraphRoot".to_string(),
        );
        overlay
    }

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

    #[test]
    fn inner_outer_bridge_redirects_same_scope_connection_to_parent_inner() {
        let overlay = overlay_with_inner_outer_bridge();
        let input = conn(
            "tankController.makeProduct.outerState.subgraphStatePort",
            "tankController.makeProduct.stateGraphRoot.subgraphStatePort",
            "tankController.makeProduct",
        );

        let redirected = redirect_connection_for_inner_outer(&input, &overlay);

        assert_eq!(
            redirected.a.to_flat_string(),
            "tankController.makeProduct.outerState.subgraphStatePort"
        );
        assert_eq!(
            redirected.b.to_flat_string(),
            "stateGraphRoot.subgraphStatePort"
        );
        assert_eq!(redirected.scope, "");
    }

    #[test]
    fn inner_outer_bridge_keeps_child_scope_connection_on_local_inner() {
        let overlay = overlay_with_inner_outer_bridge();
        let input = conn(
            "tankController.makeProduct.fillTank1.outerStatePort.subgraphStatePort",
            "tankController.makeProduct.stateGraphRoot.subgraphStatePort",
            "tankController.makeProduct.fillTank1",
        );

        let redirected = redirect_connection_for_inner_outer(&input, &overlay);

        assert_eq!(
            redirected.a.to_flat_string(),
            "tankController.makeProduct.fillTank1.outerStatePort.subgraphStatePort"
        );
        assert_eq!(
            redirected.b.to_flat_string(),
            "tankController.makeProduct.stateGraphRoot.subgraphStatePort"
        );
        assert_eq!(redirected.scope, "tankController.makeProduct.fillTank1");
    }
}
