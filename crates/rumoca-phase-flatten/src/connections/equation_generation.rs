use super::*;
use indexmap::IndexSet;
use rumoca_ir_ast as ast;

type FlowVarSet = IndexSet<rumoca_core::VarName>;
pub(super) type InterfaceStreamEndpointsByScope =
    IndexMap<String, IndexMap<rumoca_core::VarName, rumoca_core::Span>>;
type InterfaceConnectorRootSet = IndexSet<rumoca_core::ComponentPath>;
pub(super) type InterfaceConnectorRootsByScope = IndexMap<String, InterfaceConnectorRootSet>;

/// One connection value derived from one required declaration-selection proof.
///
/// This is deliberately phase-local. Flat does not expose a caller-authored
/// endpoint/origin constructor: the connection transaction derives the value,
/// role, sign, cardinality, and residual together and commits the completed
/// projection atomically.
struct ConnectionMemberValue {
    expression: rumoca_core::Expression,
    owner: ConnectionDeclarationOwner,
}

fn connection_member_value(
    flat: &flat::Model,
    var_name: &rumoca_core::VarName,
    span: ProvenanceSpan,
) -> Result<ConnectionMemberValue, FlattenError> {
    let evidence = require_connection_declaration(flat, var_name, span.span())?;
    let owner = ConnectionDeclarationOwner::from_evidence(&evidence)?;
    let declaration = evidence.declaration();
    let name = match declaration.component_ref.clone() {
        Some(component_ref) => rumoca_core::Reference::with_component_reference(
            evidence.base().as_str(),
            component_ref,
        ),
        None => rumoca_core::Reference::from_var_name(evidence.base().clone()),
    };
    let expression = rumoca_core::Expression::VarRef {
        name: name.with_instance_id(declaration.instance_id),
        subscripts: evidence
            .indices()
            .iter()
            .copied()
            .map(|value| rumoca_core::Subscript::generated_index_with_provenance(value, span))
            .collect(),
        span: span.span(),
    };
    Ok(ConnectionMemberValue { expression, owner })
}

/// Compute scalar count from variable dimensions.
///
/// For array variables, scalar_count = product of dimensions.
/// For scalars (empty dims), returns 1.
fn compute_var_scalar_count(var: &flat::Variable) -> Result<usize, FlattenError> {
    scalar_count_of_dims(&var.dims).map_err(|reason| {
        FlattenError::invalid_connection_evidence(
            format!(
                "invalid dimensions for Flat variable `{}`: {reason}",
                var.name
            ),
            var.source_span,
        )
    })
}

/// Scalar leaves denoted by one connection-set member.
///
/// MLS §9.2 generates one scalar equation per matched leaf and MLS §4.8 counts
/// those scalars when balancing the model, so an element or slice endpoint
/// contributes the leaves of what it *denotes* (MLS §10.5), not one leaf per
/// subscripted path.
///
/// A member whose subscript sits on an inner path segment (`a[1].e`) has no
/// declaration of its own and is not a trailing element of one either. It is
/// measured by what MLS §10.5 says it denotes: the declaration its checked
/// selection evidence names,
/// with one leading dimension consumed per literal subscript the path carries.
/// Returning a constant 1 instead is the same defect the trailing-subscript
/// case had — it silently shrinks the generated equation to one scalar and
/// leaves the rest of the connected array unconstrained (MLS §4.8).
///
/// When the index-free path names no declaration either, the answer is `None`
/// (unknown), never 1: callers distinguish "denotes one scalar" from "no
/// evidence", and a fabricated 1 is what makes a mixed scalar/array flow set
/// collapse to a single Kirchhoff equation in [`generate_flow_equation`].
#[cfg(test)]
pub(super) fn resolve_var_scalar_count(
    flat: &flat::Model,
    var: &rumoca_core::VarName,
) -> Option<usize> {
    match var_shape_evidence(flat, var) {
        ShapeEvidence::Known(shape) => Some(shape.scalar_count),
        ShapeEvidence::Missing | ShapeEvidence::Invalid(_) => None,
    }
}

#[derive(Clone, Debug)]
struct ConnectionShape {
    dims: Vec<i64>,
    scalar_count: usize,
}

enum ShapeEvidence {
    Known(ConnectionShape),
    Missing,
    Invalid(String),
}

fn var_shape_evidence(flat: &flat::Model, var: &rumoca_core::VarName) -> ShapeEvidence {
    if let Some(declared) = flat.variables.get(var) {
        return shape_from_dims(&declared.dims);
    }
    match declared_array_element_evidence(var, flat) {
        Ok(Some(selected)) => {
            if !selected.declaration.is_primitive {
                return ShapeEvidence::Invalid(
                    "a selected composite declaration must be expanded to exact primitive members before equation planning"
                        .to_string(),
                );
            }
            let selected_shape =
                shape_from_dims(&selected.declaration.dims[selected.indices.len()..]);
            if matches!(
                &selected_shape,
                ShapeEvidence::Known(ConnectionShape {
                    scalar_count: 0,
                    ..
                })
            ) {
                // A strict selection of an empty value denotes no scalar
                // connection members. It neither needs a connected-subdomain
                // owner nor marks any part of the compact declaration connected.
                return selected_shape;
            }
            let selects_strict_subdomain = selected
                .declaration
                .dims
                .iter()
                .take(selected.indices.len())
                .any(|extent| *extent > 1);
            if selects_strict_subdomain && selected.declaration.stream {
                // A strict subdomain of a compact stream array has a checked
                // connected-domain owner, but MLS §15.2 mixing pairs each
                // stream member with the flow member of the same connector
                // occurrence, and the stream rewrite resolves that pairing per
                // declaration rather than per element. Refusing here keeps the
                // element and its whole-declaration flow partner from being
                // mixed at different granularities.
                return ShapeEvidence::Invalid(
                    "partial connectivity of a compact stream array is not representable; expand the connector occurrence before connection lowering"
                    .to_string(),
                );
            }
            // A strict subdomain of a flow or potential declaration is
            // representable: the member's owner carries its selection, the
            // connection transaction marks exactly those elements, and the
            // MLS §9.2 zero-flow planner reads the per-element domain.
            selected_shape
        }
        Ok(None) => ShapeEvidence::Missing,
        Err(reason) => ShapeEvidence::Invalid(reason),
    }
}

fn shape_from_dims(dims: &[i64]) -> ShapeEvidence {
    match scalar_count_of_dims(dims) {
        Ok(scalar_count) => ShapeEvidence::Known(ConnectionShape {
            dims: dims.to_vec(),
            scalar_count,
        }),
        Err(reason) => ShapeEvidence::Invalid(reason),
    }
}

fn require_var_shape(
    flat: &flat::Model,
    var: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<ConnectionShape, FlattenError> {
    match var_shape_evidence(flat, var) {
        ShapeEvidence::Known(shape) => Ok(shape),
        ShapeEvidence::Missing => Err(FlattenError::undefined_variable(var.as_str(), span)),
        ShapeEvidence::Invalid(reason) => Err(FlattenError::invalid_connection_evidence(
            format!("connection member `{var}` cannot be represented safely: {reason}"),
            span,
        )),
    }
}

/// Scalar count of a dimension list, sharing one clamp with
/// [`compute_var_scalar_count`] so a declaration and one of its elements can
/// never be counted by two different rules.
pub(super) fn scalar_count_of_dims(dims: &[i64]) -> Result<usize, String> {
    for extent in dims {
        if *extent < 0 {
            return Err(format!("negative array extent `{extent}`"));
        }
    }
    // An array with any zero extent is empty regardless of the other extents.
    // Decide that only after validating every extent so a later negative
    // dimension cannot be hidden by an earlier zero.
    if dims.contains(&0) {
        return Ok(0);
    }

    let mut count = 1usize;
    for extent in dims {
        let extent = usize::try_from(*extent)
            .map_err(|_| format!("array extent `{extent}` exceeds the host index range"))?;
        count = count
            .checked_mul(extent)
            .ok_or_else(|| "array cardinality exceeds the host index range".to_string())?;
        let ir_limit = usize::try_from(i64::MAX).unwrap_or(usize::MAX);
        if count > ir_limit {
            return Err("array cardinality exceeds the Flat IR index range".to_string());
        }
    }
    Ok(count)
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

#[cfg(test)]
pub(super) fn mark_stream_connection_set(
    flat: &mut flat::Model,
    variables: &[rumoca_core::VarName],
    span: Span,
) -> Result<(), FlattenError> {
    let mut projection = OpenConnectionProjection::new(flat);
    plan_mark_stream_connection_set(flat, variables, span, &mut projection)?;
    projection.seal_without_stream().commit(flat)
}

fn plan_mark_stream_connection_set(
    flat: &flat::Model,
    variables: &[rumoca_core::VarName],
    span: Span,
    projection: &mut OpenConnectionProjection,
) -> Result<(), FlattenError> {
    let connected = variables
        .iter()
        .map(|var| {
            let evidence = require_connection_declaration(flat, var, span)?;
            let shape = require_var_shape(flat, var, span)?;
            Ok((var, shape.scalar_count, evidence))
        })
        .collect::<Result<Vec<_>, FlattenError>>()?;
    for (var, scalar_count, evidence) in connected {
        if scalar_count == 0 {
            continue;
        }
        let provenance = require_connection_provenance(
            evidence.declaration().source_span,
            "stream connected-state owner",
        )?;
        let value = connection_member_value(flat, var, provenance)?;
        projection.mark_connected(flat, &value.owner)?;
    }
    Ok(())
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
#[cfg(test)]
pub(super) fn generate_outside_stream_equations(
    flat: &mut flat::Model,
    endpoints_by_scope: &InterfaceStreamEndpointsByScope,
    stream_endpoints: &super::stream_operators::StreamConnectionEndpoints,
) -> Result<(), FlattenError> {
    let mut projection = OpenConnectionProjection::new(flat);
    plan_outside_stream_equations(
        flat,
        endpoints_by_scope,
        stream_endpoints,
        super::stream_operators::StreamOperatorIdentities::fixture(),
        &mut projection,
    )?;
    projection.seal_without_stream().commit(flat)
}

fn plan_outside_stream_equations(
    flat: &flat::Model,
    endpoints_by_scope: &InterfaceStreamEndpointsByScope,
    stream_endpoints: &super::stream_operators::StreamConnectionEndpoints,
    operator_identities: super::stream_operators::StreamOperatorIdentities,
    projection: &mut OpenConnectionProjection,
) -> Result<(), FlattenError> {
    let measured_endpoints = endpoints_by_scope
        .values()
        .flat_map(|endpoints| endpoints.iter())
        .map(|(stream, span)| {
            let shape = require_var_shape(flat, stream, *span)?;
            let provenance = (shape.scalar_count != 0)
                .then(|| require_connection_provenance(*span, "outside stream connection equation"))
                .transpose()?;
            Ok((stream, *span, shape, provenance))
        })
        .collect::<Result<Vec<_>, _>>()?;

    for (stream, span, shape, provenance) in measured_endpoints {
        if shape.scalar_count == 0 {
            continue;
        }
        let provenance = provenance.expect("nonempty endpoints were prevalidated with provenance");
        let stream_value = connection_member_value(flat, stream, provenance)?;
        let stream_owner = stream_value.owner.clone();
        // A connector that reached interface discovery without joining a
        // stream connection set at that scope is the MLS §15.2 unconnected
        // case; it keeps the conceptual `inStream()` right-hand side.
        let mix = stream_endpoints
            .outside_equation_rhs(stream, operator_identities, provenance.span())
            .unwrap_or_else(|| {
                operator_identities.call(
                    super::stream_operators::StreamOperatorRole::InStream,
                    vec![stream_value.expression.clone()],
                    provenance.span(),
                )
            });
        let residual = create_equality_residual(stream_value.expression, mix, provenance);
        let origin = flat::EquationOrigin::OutsideStream {
            variable: stream.as_str().to_string(),
        };
        let equation = flat::Equation::new_array(residual, span, origin, shape.scalar_count);
        projection.plan_equation(equation, Some(&shape.dims))?;
        projection.mark_connected(flat, &stream_owner)?;
    }
    Ok(())
}

/// Preserve MLS §9.3 equality checks for connected parameter/constant members.
///
/// Scalar members become ordinary Flat assertion owners. Empty values require
/// no scalar assertions. Flat currently has no compact assertion-family owner,
/// so a nonempty array is refused instead of constructing an array-valued `==`
/// where an assertion requires one scalar Boolean condition.
fn generate_structural_connection_assertions(
    flat: &flat::Model,
    variables: &[rumoca_core::VarName],
    span: rumoca_core::Span,
    projection: &mut OpenConnectionProjection,
) -> Result<(), FlattenError> {
    let provenance = require_connection_provenance(span, "structural connection assertion")?;
    let shapes = variables
        .iter()
        .map(|variable| require_var_shape(flat, variable, span))
        .collect::<Result<Vec<_>, _>>()?;
    for (window, pair) in variables.windows(2).zip(shapes.windows(2)) {
        if pair[0].dims != pair[1].dims {
            return Err(FlattenError::incompatible_connectors(
                window[0].as_str(),
                window[1].as_str(),
                span,
            ));
        }
    }
    let Some(shape) = shapes.first() else {
        return Ok(());
    };
    if shape.scalar_count == 0 {
        return Ok(());
    }
    if !shape.dims.is_empty() {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "nonempty structural connection array with dimensions {:?} requires a compact assertion-family owner",
                shape.dims
            ),
            span,
        ));
    }

    let mut planned = Vec::with_capacity(variables.len().saturating_sub(1));
    for window in variables.windows(2) {
        let lhs_name = &window[0];
        let rhs_name = &window[1];
        let lhs = connection_member_value(flat, lhs_name, provenance)?;
        let rhs = connection_member_value(flat, rhs_name, provenance)?;
        let condition = rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Box::new(lhs.expression),
            rhs: Box::new(rhs.expression),
            span,
        };
        let message = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(
                "Connected constants/parameters must be equal".to_string(),
            ),
            span,
        };
        let origin = flat::EquationOrigin::Connection {
            lhs: lhs_name.as_str().to_string(),
            rhs: rhs_name.as_str().to_string(),
        };
        planned.push(flat::AssertEquation::new(
            condition, message, None, span, origin,
        ));
    }
    projection.extend_assertions(planned);
    Ok(())
}

/// Generate equality equations for potential (non-flow) variables.
///
/// For n variables in a connection set, generates n-1 equations:
/// `v1 = v2, v2 = v3, ..., v(n-1) = vn`
///
/// In residual form: `v1 - v2 = 0, v2 - v3 = 0, ...`
#[cfg(test)]
pub(super) fn generate_equality_equations(
    flat: &mut flat::Model,
    overconstrained: &ast::FinalizedOverconstrainedCatalog<'_>,
    variables: &[rumoca_core::VarName],
    span: rumoca_core::Span,
    oc_forest: &mut crate::vcg::OverconstrainedEquationForest,
) -> Result<(), FlattenError> {
    let rollback_forest = oc_forest.clone();
    let mut owned_forest = std::mem::replace(
        oc_forest,
        crate::vcg::OverconstrainedEquationForest::empty(),
    );
    let mut projection = OpenConnectionProjection::new(flat);
    let result = plan_equality_equations(
        flat,
        overconstrained,
        variables,
        span,
        &mut owned_forest,
        &mut projection,
    )
    .and_then(|()| projection.seal_without_stream().commit(flat));
    match result {
        Ok(()) => {
            *oc_forest = owned_forest;
            Ok(())
        }
        Err(error) => {
            *oc_forest = rollback_forest;
            Err(error)
        }
    }
}

fn plan_equality_equations(
    flat: &flat::Model,
    overconstrained: &ast::FinalizedOverconstrainedCatalog<'_>,
    variables: &[rumoca_core::VarName],
    span: rumoca_core::Span,
    oc_forest: &mut crate::vcg::OverconstrainedEquationForest,
    projection: &mut OpenConnectionProjection,
) -> Result<(), FlattenError> {
    let provenance = require_connection_provenance(span, "connection equality equation")?;
    let shapes = variables
        .iter()
        .map(|var| require_var_shape(flat, var, span))
        .collect::<Result<Vec<_>, _>>()?;
    for (window, pair) in variables.windows(2).zip(shapes.windows(2)) {
        if pair[0].dims != pair[1].dims {
            return Err(FlattenError::incompatible_connectors(
                window[0].as_str(),
                window[1].as_str(),
                span,
            ));
        }
    }
    for (window, shape) in variables.windows(2).zip(shapes) {
        let var_a = &window[0];
        let var_b = &window[1];
        let scalar_count = shape.scalar_count;

        // Skip empty arrays (Real[0]) — no equations needed
        if scalar_count == 0 {
            continue;
        }

        match oc_forest.generated_equality_disposition(overconstrained, flat, var_a, var_b, span)? {
            crate::vcg::GeneratedEqualityDisposition::Retain => {}
            crate::vcg::GeneratedEqualityDisposition::Omit => continue,
            crate::vcg::GeneratedEqualityDisposition::Replace {
                lhs_record,
                rhs_record,
            } => {
                if let Some(equation) = plan_equality_constraint_equation(
                    flat,
                    overconstrained,
                    &lhs_record,
                    &rhs_record,
                    span,
                    projection.next_equation_index(),
                )? {
                    projection.push_planned_equation(equation);
                }
                continue;
            }
        }

        // Create residual: var_a - var_b = 0
        let value_a = connection_member_value(flat, var_a, provenance)?;
        let value_b = connection_member_value(flat, var_b, provenance)?;
        let owner_a = value_a.owner.clone();
        let owner_b = value_b.owner.clone();
        let residual = create_equality_residual(value_a.expression, value_b.expression, provenance);

        let origin = flat::EquationOrigin::Connection {
            lhs: var_a.as_str().to_string(),
            rhs: var_b.as_str().to_string(),
        };
        projection.plan_equation(
            flat::Equation::new_array(residual, span, origin, scalar_count),
            Some(&shape.dims),
        )?;
        projection.mark_connected(flat, &owner_a)?;
        projection.mark_connected(flat, &owner_b)?;
    }

    Ok(())
}

/// Generate sum-to-zero equation for flow variables.
///
/// For n flow variables in a connection set: `sign_1*f1 + sign_2*f2 + ... + sign_n*fn = 0`
///
/// Per MLS §9.2 (CONN-026):
/// - Inside connectors (component ports): sign = +1
/// - Outside connectors (model boundary): sign = -1
#[cfg(test)]
pub(super) fn generate_flow_equation(
    flat: &mut flat::Model,
    variables: &[rumoca_core::VarName],
    scope: &str,
    interface_flow_vars_by_scope: &IndexMap<String, FlowVarSet>,
    span: rumoca_core::Span,
) -> Result<(), FlattenError> {
    let mut projection = OpenConnectionProjection::new(flat);
    plan_flow_equation(
        flat,
        variables,
        scope,
        interface_flow_vars_by_scope,
        span,
        &mut projection,
    )?;
    projection.seal_without_stream().commit(flat)
}

fn plan_flow_equation(
    flat: &flat::Model,
    variables: &[rumoca_core::VarName],
    scope: &str,
    interface_flow_vars_by_scope: &IndexMap<String, FlowVarSet>,
    span: rumoca_core::Span,
    projection: &mut OpenConnectionProjection,
) -> Result<(), FlattenError> {
    if variables.is_empty() {
        return Ok(());
    }
    let provenance = require_connection_provenance(span, "connection flow equation")?;

    let flow_shapes = variables
        .iter()
        .map(|var| require_var_shape(flat, var, span))
        .collect::<Result<Vec<_>, _>>()?;
    if flow_shapes.iter().all(|shape| shape.scalar_count == 0) {
        if let Some((index, _)) = flow_shapes
            .iter()
            .enumerate()
            .skip(1)
            .find(|(_, shape)| shape.dims != flow_shapes[0].dims)
        {
            return Err(FlattenError::incompatible_connectors(
                variables[0].as_str(),
                variables[index].as_str(),
                span,
            ));
        }
        return Ok(());
    }
    if let Some(zero_index) = flow_shapes.iter().position(|shape| shape.scalar_count == 0) {
        let nonzero_index = flow_shapes
            .iter()
            .position(|shape| shape.scalar_count != 0)
            .expect("not all flow cardinalities are zero");
        return Err(FlattenError::incompatible_connectors(
            variables[zero_index].as_str(),
            variables[nonzero_index].as_str(),
            span,
        ));
    }
    if let Some((other_index, _)) = flow_shapes
        .iter()
        .enumerate()
        .skip(1)
        .find(|(_, shape)| shape.dims != flow_shapes[0].dims)
    {
        return Err(FlattenError::incompatible_connectors(
            variables[0].as_str(),
            variables[other_index].as_str(),
            span,
        ));
    }
    // A Flat flow-sum equation is pointwise over one proven common shape. A
    // scalar plus an array would construct a scalar residual containing an
    // aggregate operand (or claim array scalarization for a scalar operand),
    // neither of which is valid IR. Connector expansion must supply matching
    // scalar members before reaching this owner.
    let scalar_count = flow_shapes[0].scalar_count;

    // Create sum expression with proper signs per MLS §9.2
    // Inside connectors: +f, Outside connectors: -f
    let checked_members = variables
        .iter()
        .map(|var| {
            let value = connection_member_value(flat, var, provenance)?;
            let negative = is_outside_flow_var_for_scope(var, scope, interface_flow_vars_by_scope);
            let expression = if negative {
                // Outside connector: negate (sign = -1)
                rumoca_core::Expression::Unary {
                    op: rumoca_core::OpUnary::Minus,
                    rhs: Box::new(value.expression),
                    span: provenance.span(),
                }
            } else {
                // Inside connector: positive (sign = +1)
                value.expression
            };
            let rendered = if negative {
                format!("-{}", var.as_str())
            } else {
                var.as_str().to_string()
            };
            Ok((expression, rendered, value.owner))
        })
        .collect::<Result<Vec<_>, FlattenError>>()?;
    let mut flow_exprs = Vec::with_capacity(checked_members.len());
    let mut rendered_members = Vec::with_capacity(checked_members.len());
    let mut owners = Vec::with_capacity(checked_members.len());
    for (expression, rendered, owner) in checked_members {
        flow_exprs.push(expression);
        rendered_members.push(rendered);
        owners.push(owner);
    }
    let sum = create_sum(flow_exprs, provenance);

    let origin = flat::EquationOrigin::FlowSum {
        description: format!("{} = 0", rendered_members.join(" + ")),
    };
    let eq = flat::Equation::new_array(sum, span, origin, scalar_count);
    projection.plan_equation(eq, Some(&flow_shapes[0].dims))?;
    for owner in &owners {
        // Claiming precedes marking so that an element summed by two sets at
        // this scope refuses before any of its connected state is planned.
        projection.claim_flow_member(flat, scope, owner)?;
        projection.mark_connected(flat, owner)?;
    }

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
    conn: &ast::InstanceScalarConnection,
    disabled_components: &indexmap::IndexSet<rumoca_core::ComponentPath>,
) -> bool {
    for disabled in disabled_components {
        if super::qualified_connection_endpoint_starts_with(conn.a(), disabled) {
            return true;
        }
        if super::qualified_connection_endpoint_starts_with(conn.b(), disabled) {
            return true;
        }
    }

    false
}

/// Build the dotted-prefix-to-descendant index for sub-variable lookup.
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
    overconstrained: &ast::FinalizedOverconstrainedCatalog<'_>,
    mut oc_forest: crate::vcg::OverconstrainedEquationForest,
    operator_identities: super::stream_operators::StreamOperatorIdentities,
) -> Result<crate::vcg::OverconstrainedEquationForest, FlattenError> {
    let overlay = overconstrained.overlay();
    super::ensure_connection_scalarization_budget(overlay)?;
    let projection = OpenConnectionProjection::new(flat);
    let projection = plan_connections(
        flat,
        overlay,
        overconstrained,
        &mut oc_forest,
        operator_identities,
        projection,
    )?;
    projection.commit(flat)?;
    Ok(oc_forest)
}

#[cfg(test)]
pub(super) fn process_connections_for_test(
    flat: &mut flat::Model,
    overconstrained: &ast::FinalizedOverconstrainedCatalog<'_>,
    oc_forest: &mut crate::vcg::OverconstrainedEquationForest,
) -> Result<(), FlattenError> {
    let owned = std::mem::replace(
        oc_forest,
        crate::vcg::OverconstrainedEquationForest::empty(),
    );
    // Production consumes the forest and aborts compilation on failure. This
    // borrowed test adapter must preserve its caller-owned fixture instead;
    // clone only that bounded fixture before transferring ownership.
    let rollback_forest = owned.clone();
    match process_connections(
        flat,
        overconstrained,
        owned,
        super::stream_operators::StreamOperatorIdentities::fixture(),
    ) {
        Ok(updated) => {
            *oc_forest = updated;
            Ok(())
        }
        Err(error) => {
            *oc_forest = rollback_forest;
            Err(error)
        }
    }
}

fn plan_connections(
    flat: &flat::Model,
    overlay: &ast::InstanceOverlay,
    overconstrained: &ast::FinalizedOverconstrainedCatalog<'_>,
    oc_forest: &mut crate::vcg::OverconstrainedEquationForest,
    operator_identities: super::stream_operators::StreamOperatorIdentities,
    mut projection: OpenConnectionProjection,
) -> Result<SealedConnectionProjection, FlattenError> {
    let prefix_children = build_prefix_children(flat);
    let var_index = ConnectionVarIndex::new(flat);
    let endpoint_index = ConnectionEndpointIndex::new(overlay);
    // FLAT-C01: pruning and augmentation precede source identification.
    let pruned_sources = prune_connection_sources(overlay)?;
    let closed_sources = pruned_sources
        .plan_after_expandable_check(flat, &endpoint_index, &prefix_children, &var_index)?
        .close()?;
    let topology_inputs = closed_sources.topology_inputs();
    let all_connections = topology_inputs
        .iter()
        .map(|input| input.connection)
        .collect::<Vec<_>>();
    let mut source_consumption = closed_sources.consumption()?;
    validate_closed_connection_inputs(
        flat,
        &closed_sources,
        &all_connections,
        &prefix_children,
        &var_index,
    )?;

    // Retain per-scope roles for external flow-zero planning.
    let flow_vars_at_scope =
        collect_flow_vars_by_scope(&all_connections, flat, &prefix_children, &var_index)?;

    let interface_connector_roots_by_scope = collect_interface_connector_roots_by_scope(overlay);
    let interface_flow_vars_by_scope = collect_interface_flow_vars_by_scope(
        &all_connections,
        flat,
        &prefix_children,
        &var_index,
        &interface_connector_roots_by_scope,
    )?;
    let interface_stream_endpoints_by_scope = collect_interface_stream_endpoints_by_scope(
        &all_connections,
        flat,
        &prefix_children,
        &var_index,
        &interface_connector_roots_by_scope,
    )?;

    let (connection_sets, stream_sets) = build_connection_sets(
        &topology_inputs,
        flat,
        &prefix_children,
        &var_index,
        &mut source_consumption,
    )?;

    let endpoints = {
        let mut derived = DerivedConnectionProjection {
            flat,
            overconstrained,
            oc_forest,
            operator_identities,
            projection: &mut projection,
        };
        derived.plan_sets_and_outside_streams(
            connection_sets,
            &stream_sets,
            &interface_flow_vars_by_scope,
            &interface_stream_endpoints_by_scope,
        )?
    };

    // MLS §9.2 unconnected flow rows remain in the same projection.
    plan_unconnected_flow_equations(flat, &mut projection)?;

    plan_external_unconnected_flow_equations(
        flat,
        &flow_vars_at_scope,
        &all_connections,
        &prefix_children,
        &var_index,
        &interface_connector_roots_by_scope,
        &mut projection,
    )?;

    source_consumption.finish()?;

    projection.seal_stream_rewrite(flat, &stream_sets, &endpoints, operator_identities)
}

fn validate_closed_connection_inputs(
    flat: &flat::Model,
    _sources: &ClosedConnectionSources,
    connections: &[&ast::InstanceScalarConnection],
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Result<(), FlattenError> {
    flat.validate().map_err(|error| {
        let reason = format!(
            "connection compatibility requires a complete finalized Flat effective-type catalog: {error:?}"
        );
        match connections.first() {
            Some(connection) => {
                FlattenError::invalid_connection_evidence(reason, connection.span())
            }
            None => FlattenError::internal(reason),
        }
    })?;
    #[cfg(feature = "tracing")]
    {
        tracing::debug!(
            connection_count = connections.len(),
            source_count = _sources.source_count(),
            "processing flattened connections"
        );
        for connection in connections {
            tracing::debug!(scope = %connection.scope(), a = %connection.a(), b = %connection.b(), "flattened connection");
        }
    }
    validate_connections(connections, flat, prefix_children, var_index)
}

struct DerivedConnectionProjection<'a, 'catalog, 'overlay> {
    flat: &'a flat::Model,
    overconstrained: &'catalog ast::FinalizedOverconstrainedCatalog<'overlay>,
    oc_forest: &'a mut crate::vcg::OverconstrainedEquationForest,
    operator_identities: super::stream_operators::StreamOperatorIdentities,
    projection: &'a mut OpenConnectionProjection,
}

impl DerivedConnectionProjection<'_, '_, '_> {
    fn plan_sets_and_outside_streams(
        &mut self,
        connection_sets: Vec<ConnectionSet>,
        stream_sets: &[StreamConnectionSet],
        interface_flows: &IndexMap<String, FlowVarSet>,
        interface_streams: &InterfaceStreamEndpointsByScope,
    ) -> Result<super::stream_operators::StreamConnectionEndpoints, FlattenError> {
        let endpoints = super::stream_operators::build_stream_connection_endpoints(
            self.flat,
            stream_sets,
            interface_streams,
        )?;
        for set in connection_sets {
            self.plan_set(set, interface_flows)?;
        }
        plan_outside_stream_equations(
            self.flat,
            interface_streams,
            &endpoints,
            self.operator_identities,
            self.projection,
        )?;
        for set in stream_sets {
            plan_mark_stream_connection_set(self.flat, &set.variables, set.span, self.projection)?;
        }
        Ok(endpoints)
    }

    fn plan_set(
        &mut self,
        set: ConnectionSet,
        interface_flows: &IndexMap<String, FlowVarSet>,
    ) -> Result<(), FlattenError> {
        match set.kind {
            ConnectionKind::Flow => plan_flow_equation(
                self.flat,
                &set.variables,
                set.scope.as_str(),
                interface_flows,
                set.span,
                self.projection,
            ),
            ConnectionKind::Potential => plan_equality_equations(
                self.flat,
                self.overconstrained,
                &set.variables,
                set.span,
                self.oc_forest,
                self.projection,
            ),
            ConnectionKind::StructuralAssertion => generate_structural_connection_assertions(
                self.flat,
                &set.variables,
                set.span,
                self.projection,
            ),
        }
    }
}

/// Generate `flow_var = 0` equations for unconnected flow variables.
///
/// Per MLS §9.2: "For every outside connector of the model, the sum of
/// the corresponding flow variables is also set equal to zero."
/// For a single unconnected flow variable, this means `flow_var = 0`.
#[cfg(test)]
pub(super) fn generate_unconnected_flow_equations(
    flat: &mut flat::Model,
) -> Result<(), FlattenError> {
    let mut projection = OpenConnectionProjection::new(flat);
    plan_unconnected_flow_equations(flat, &mut projection)?;
    projection.seal_without_stream().commit(flat)?;
    Ok(())
}

fn plan_unconnected_flow_equations(
    flat: &flat::Model,
    projection: &mut OpenConnectionProjection,
) -> Result<(), FlattenError> {
    // The effective domain of a declaration is what earlier transactions
    // committed plus what this open transaction has planned; both are read so
    // an element summed by a set planned moments ago is not zeroed as well.
    let mut zero_rows: Vec<UnconnectedFlowRows> = Vec::new();
    for (name, var) in &flat.variables {
        if !var.flow {
            continue;
        }
        let mut domain = var.connected.clone();
        if let Some(pending) = projection.pending_domain(name) {
            domain.union_with(pending);
        }
        zero_rows.push(unconnected_flow_rows(name, var, &domain)?);
    }

    for rows in zero_rows {
        plan_unconnected_flow_rows(flat, rows, "unconnected flow equation", projection)?;
        // The zeroed elements are NOT marked connected: the domain records
        // participation in actual connection sets (flow sums with other
        // members), not the mere presence of an equation. Interface flow
        // detection per MLS §4.7 depends on that distinction.
    }

    Ok(())
}

/// Zero-flow rows owed to one flow declaration by MLS §9.2, decided from its
/// checked connected domain.
///
/// An unconnected declaration keeps its compact array row so that structured
/// lowering sees one family; a partially connected declaration emits one
/// scalar row per untouched element, because a compact row over the whole
/// declaration would also zero the connected elements on top of their flow
/// sums. Per MLS §9.2, unconnected flow variables always get zero-flow
/// equations, even if their parent record appears in a body equation. Both
/// record-level body equations (like `port_p.Phi = Phi`) AND scalar zero-flow
/// equations (like `port_p.Phi.re = 0`) are generated; the balance check
/// counts both.
enum UnconnectedFlowRows {
    None,
    Whole {
        variable: rumoca_core::VarName,
        dims: Vec<i64>,
        scalar_count: usize,
    },
    Elements {
        variable: rumoca_core::VarName,
        coordinates: Vec<Vec<i64>>,
    },
}

fn unconnected_flow_rows(
    name: &rumoca_core::VarName,
    var: &flat::Variable,
    domain: &flat::ConnectedDomain,
) -> Result<UnconnectedFlowRows, FlattenError> {
    let scalar_count = compute_var_scalar_count(var)?;
    // An empty value (some extent is 0) has no element to zero.
    if scalar_count == 0 {
        return Ok(UnconnectedFlowRows::None);
    }
    let coverage = domain.coverage(&var.dims).map_err(|reason| {
        FlattenError::invalid_connection_evidence(
            format!("connected domain of `{name}` contradicts its declared dimensions: {reason}"),
            var.source_span,
        )
    })?;
    match coverage {
        flat::ConnectedCoverage::Whole => Ok(UnconnectedFlowRows::None),
        flat::ConnectedCoverage::Unconnected => Ok(UnconnectedFlowRows::Whole {
            variable: name.clone(),
            dims: var.dims.clone(),
            scalar_count,
        }),
        flat::ConnectedCoverage::Partial => {
            // Enumerating the complement materializes one scalar row per
            // untouched element, so it obeys the same eager materialization
            // budget as every other compact connection family.
            let limit = crate::equations::MAX_EAGER_RANGE_ELEMENTS;
            if scalar_count > limit {
                return Err(FlattenError::RangeMaterializationLimit {
                    element_count: scalar_count as u128,
                    limit,
                    span: var.source_span,
                });
            }
            let coordinates = domain.unconnected_coordinates(&var.dims).map_err(|reason| {
                FlattenError::invalid_connection_evidence(
                    format!(
                        "connected domain of `{name}` contradicts its declared dimensions: {reason}"
                    ),
                    var.source_span,
                )
            })?;
            debug_assert!(
                !coordinates.is_empty(),
                "partial coverage of `{name}` leaves at least one element unconnected"
            );
            Ok(UnconnectedFlowRows::Elements {
                variable: name.clone(),
                coordinates,
            })
        }
    }
}

/// Rendered Flat name of one element of a compact declaration, in the same
/// `base[i,j]` spelling the selection-evidence resolver accepts.
fn rendered_element_name(base: &rumoca_core::VarName, coordinates: &[i64]) -> rumoca_core::VarName {
    let rendered = coordinates
        .iter()
        .map(i64::to_string)
        .collect::<Vec<_>>()
        .join(",");
    rumoca_core::VarName::new(format!("{}[{rendered}]", base.as_str()))
}

fn plan_unconnected_flow_rows(
    flat: &flat::Model,
    rows: UnconnectedFlowRows,
    context: &'static str,
    projection: &mut OpenConnectionProjection,
) -> Result<(), FlattenError> {
    match rows {
        UnconnectedFlowRows::None => Ok(()),
        UnconnectedFlowRows::Whole {
            variable,
            dims,
            scalar_count,
        } => plan_zero_flow_row(
            flat,
            &variable,
            Some(&dims),
            scalar_count,
            context,
            projection,
        ),
        UnconnectedFlowRows::Elements {
            variable,
            coordinates,
        } => {
            for coordinate in coordinates {
                let element = rendered_element_name(&variable, &coordinate);
                plan_zero_flow_row(flat, &element, Some(&[]), 1, context, projection)?;
            }
            Ok(())
        }
    }
}

/// Plan `member = 0` for one flow member (a whole declaration, a leading
/// selection, or one element), in residual form `member`.
fn plan_zero_flow_row(
    flat: &flat::Model,
    member: &rumoca_core::VarName,
    preferred_dims: Option<&[i64]>,
    scalar_count: usize,
    context: &'static str,
    projection: &mut OpenConnectionProjection,
) -> Result<(), FlattenError> {
    let provenance = require_flat_variable_provenance(flat, member, context)?;
    let value = connection_member_value(flat, member, provenance)?;
    let origin = flat::EquationOrigin::UnconnectedFlow {
        variable: member.as_str().to_string(),
    };
    let equation =
        flat::Equation::new_array(value.expression, provenance.span(), origin, scalar_count);
    projection.plan_equation(equation, preferred_dims)
}

/// Collect flow variables that participate in connections at each scope level.
///
/// Returns a map from scope string to the set of flow variable names that appear
/// in connections at that scope. Used to detect sub-component interface connectors
/// that are internally connected but not externally connected.
fn collect_flow_vars_by_scope(
    connections: &[&ast::InstanceScalarConnection],
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Result<IndexMap<String, FlowVarSet>, FlattenError> {
    let mut result: IndexMap<String, FlowVarSet> = IndexMap::default();

    for conn in connections {
        let path_a = conn.a().to_flat_string();
        let path_b = conn.b().to_flat_string();

        // Collect flow sub-variables for each side of the connection
        let scope_set = result.entry(conn.scope().to_string()).or_default();
        collect_flow_vars_from_conn_path(
            flat,
            &path_a,
            scope_set,
            conn.span(),
            prefix_children,
            var_index,
        )?;
        collect_flow_vars_from_conn_path(
            flat,
            &path_b,
            scope_set,
            conn.span(),
            prefix_children,
            var_index,
        )?;
    }

    Ok(result)
}

/// Add flow variables from a connection path to the given set.
fn collect_flow_vars_from_conn_path(
    flat: &flat::Model,
    path: &str,
    dest: &mut FlowVarSet,
    span: rumoca_core::Span,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Result<(), FlattenError> {
    let var_name = rumoca_core::VarName::new(path);

    // Check if it's a direct flow variable
    if let Some(var) = flat.variables.get(&var_name)
        && var.is_primitive
    {
        if var.flow {
            dest.insert(var_name);
        }
        return Ok(());
    }

    // A compact primitive array selection has no exact synthetic Flat row.
    // Resolve it through the same required selection proof used by validation;
    // otherwise interface role collection can silently lose its flow prefix.
    if let Some(evidence) = classify_connection_declaration(flat, &var_name, span)?
        && evidence.declaration().is_primitive
    {
        if evidence.declaration().flow {
            dest.insert(var_name);
        }
        return Ok(());
    }

    // It's a connector - find flow sub-variables
    let subs = find_sub_variables_indexed(path, prefix_children, var_index);
    for sub in subs {
        if flat.variables.get(&sub).is_some_and(|v| v.flow) {
            dest.insert(sub);
        }
    }
    Ok(())
}

fn collect_stream_vars_from_conn_path(
    flat: &flat::Model,
    path: &str,
    dest: &mut IndexMap<rumoca_core::VarName, rumoca_core::Span>,
    span: rumoca_core::Span,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Result<(), FlattenError> {
    let var_name = rumoca_core::VarName::new(path);
    if let Some(variable) = flat.variables.get(&var_name)
        && variable.is_primitive
    {
        if variable.stream {
            dest.entry(var_name).or_insert(span);
        }
        return Ok(());
    }

    if let Some(evidence) = classify_connection_declaration(flat, &var_name, span)?
        && evidence.declaration().is_primitive
    {
        if evidence.declaration().stream {
            dest.entry(var_name).or_insert(span);
        }
        return Ok(());
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
    Ok(())
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
pub(super) fn collect_interface_flow_vars_by_scope(
    connections: &[&ast::InstanceScalarConnection],
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
    interface_connector_roots_by_scope: &InterfaceConnectorRootsByScope,
) -> Result<IndexMap<String, FlowVarSet>, FlattenError> {
    let mut result: IndexMap<String, FlowVarSet> = IndexMap::default();

    for conn in connections {
        let scope = &conn.scope();

        for path_qn in [&conn.a(), &conn.b()] {
            let path = path_qn.to_flat_string();
            if is_interface_connection_path_for_scope(
                &path,
                scope,
                interface_connector_roots_by_scope,
            ) {
                let scope_set = result.entry(scope.to_string()).or_default();
                collect_flow_vars_from_conn_path(
                    flat,
                    &path,
                    scope_set,
                    conn.span(),
                    prefix_children,
                    var_index,
                )?;
            }
        }
    }

    Ok(result)
}

pub(super) fn collect_interface_stream_endpoints_by_scope(
    connections: &[&ast::InstanceScalarConnection],
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
    interface_connector_roots_by_scope: &InterfaceConnectorRootsByScope,
) -> Result<InterfaceStreamEndpointsByScope, FlattenError> {
    let mut result = InterfaceStreamEndpointsByScope::default();
    for conn in connections {
        for path_qn in [&conn.a(), &conn.b()] {
            let path = path_qn.to_flat_string();
            if !is_interface_connection_path_for_scope(
                &path,
                conn.scope(),
                interface_connector_roots_by_scope,
            ) {
                continue;
            }
            collect_stream_vars_from_conn_path(
                flat,
                &path,
                result.entry(conn.scope().to_string()).or_default(),
                conn.span(),
                prefix_children,
                var_index,
            )?;
        }
    }
    Ok(result)
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

/// One flow member of a connection path resolved to its Flat declaration and
/// the leading selection it denotes.
struct FlowMemberSelection<'flat> {
    base: rumoca_core::VarName,
    declaration: &'flat flat::Variable,
    indices: Vec<i64>,
}

fn resolve_flow_member<'flat>(
    flat: &'flat flat::Model,
    member: &rumoca_core::VarName,
) -> Result<Option<FlowMemberSelection<'flat>>, FlattenError> {
    let diagnostic_span = flat
        .variables
        .values()
        .map(|declaration| declaration.source_span)
        .find(|span| !span.is_dummy())
        .unwrap_or(rumoca_core::Span::DUMMY);
    Ok(
        classify_connection_declaration(flat, member, diagnostic_span)?.map(|evidence| {
            FlowMemberSelection {
                base: evidence.base().clone(),
                declaration: evidence.declaration(),
                indices: evidence.indices().to_vec(),
            }
        }),
    )
}

fn contradicting_domain(
    name: &rumoca_core::VarName,
    span: rumoca_core::Span,
    reason: flat::ConnectedDomainError,
) -> FlattenError {
    FlattenError::invalid_connection_evidence(
        format!("connection member `{name}` selects outside its declaration: {reason}"),
        span,
    )
}

/// Elements of each flow declaration that participate in a flow set at each
/// scope, derived from the members the scope's connections name.
///
/// Membership is measured on checked domains rather than rendered names so a
/// whole-declaration connection at the parent covers an element connection in
/// the child and vice versa.
fn flow_domains_by_scope(
    flat: &flat::Model,
    flow_vars_at_scope: &IndexMap<String, FlowVarSet>,
) -> Result<IndexMap<String, IndexMap<rumoca_core::VarName, flat::ConnectedDomain>>, FlattenError> {
    let mut result: IndexMap<String, IndexMap<rumoca_core::VarName, flat::ConnectedDomain>> =
        IndexMap::default();
    for (scope, members) in flow_vars_at_scope {
        for member in members {
            let Some(selection) = resolve_flow_member(flat, member)? else {
                continue;
            };
            result
                .entry(scope.clone())
                .or_default()
                .entry(selection.base.clone())
                .or_default()
                .mark(&selection.declaration.dims, &selection.indices)
                .map_err(|reason| {
                    contradicting_domain(member, selection.declaration.source_span, reason)
                })?;
        }
    }
    Ok(result)
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
fn plan_external_unconnected_flow_equations(
    flat: &flat::Model,
    flow_vars_at_scope: &IndexMap<String, FlowVarSet>,
    connections: &[&ast::InstanceScalarConnection],
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
    interface_connector_roots_by_scope: &InterfaceConnectorRootsByScope,
    projection: &mut OpenConnectionProjection,
) -> Result<(), FlattenError> {
    let interface_flow_vars_by_scope = collect_interface_flow_vars_by_scope(
        connections,
        flat,
        prefix_children,
        var_index,
        interface_connector_roots_by_scope,
    )?;
    let need_flow_zero =
        find_unconnected_interface_flows(&interface_flow_vars_by_scope, flow_vars_at_scope, flat)?;

    for rows in need_flow_zero {
        plan_unconnected_flow_rows(flat, rows, "external unconnected flow equation", projection)?;
    }

    Ok(())
}

/// Find the elements of interface flow members that are not connected at any
/// ancestor scope, as the zero-flow rows they need.
///
/// Coverage is decided per element: a parent that connects the whole
/// declaration covers a child's element member, and a parent that connects one
/// element leaves the rest of a child's whole-declaration member owed a zero
/// row. Every element receives at most one external zero row even when several
/// members of one scope denote overlapping selections.
fn find_unconnected_interface_flows(
    interface_flows: &IndexMap<String, FlowVarSet>,
    flow_vars_at_scope: &IndexMap<String, FlowVarSet>,
    flat: &flat::Model,
) -> Result<Vec<UnconnectedFlowRows>, FlattenError> {
    let domains_by_scope = flow_domains_by_scope(flat, flow_vars_at_scope)?;
    let mut zeroed: IndexMap<rumoca_core::VarName, flat::ConnectedDomain> = IndexMap::default();
    let mut rows = Vec::new();

    for (scope, interface_vars) in interface_flows {
        let scope_path = rumoca_core::ComponentPath::from_flat_path(scope);
        for var_name in interface_vars {
            let Some(selection) = resolve_flow_member(flat, var_name)? else {
                continue;
            };
            let dims = &selection.declaration.dims;
            let span = selection.declaration.source_span;
            let scalar_count = compute_var_scalar_count(selection.declaration)?;
            if scalar_count == 0 {
                continue;
            }

            // Root scope has no parent, so its interface flows always need
            // flow = 0 for standalone checking. Non-root scopes are covered by
            // whatever proper ancestor scopes connected of the same declaration.
            let covered =
                ancestor_covered_domain(&domains_by_scope, scope, &scope_path, &selection.base);

            let mut selected = flat::ConnectedDomain::unconnected();
            selected
                .mark(dims, &selection.indices)
                .map_err(|reason| contradicting_domain(var_name, span, reason))?;
            let already = zeroed.entry(selection.base.clone()).or_default();
            if selection.indices.is_empty() && covered.is_unconnected() && already.is_unconnected()
            {
                // A whole interface declaration with no ancestor coverage keeps
                // its compact array row.
                already
                    .mark(dims, &[])
                    .map_err(|reason| contradicting_domain(var_name, span, reason))?;
                rows.push(UnconnectedFlowRows::Whole {
                    variable: selection.base.clone(),
                    dims: dims.clone(),
                    scalar_count,
                });
                continue;
            }

            let limit = crate::equations::MAX_EAGER_RANGE_ELEMENTS;
            if scalar_count > limit {
                return Err(FlattenError::RangeMaterializationLimit {
                    element_count: scalar_count as u128,
                    limit,
                    span,
                });
            }
            let coordinates =
                uncovered_flow_coordinates(&selected, dims, &covered, already, var_name, span)?;
            if !coordinates.is_empty() {
                rows.push(UnconnectedFlowRows::Elements {
                    variable: selection.base.clone(),
                    coordinates,
                });
            }
        }
    }

    Ok(rows)
}

/// Redirect a ast::QualifiedName if its flat string starts with an outer prefix (MLS §5.4).
///
/// When outer components are not instantiated, connection paths like
/// `initialStep.stateGraphRoot.resume` must be redirected to `stateGraphRoot.resume`.
fn redirect_qualified_name(
    qn: &mut ast::QualifiedName,
    outer_to_inner: &ast::AstIndexMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
) {
    if outer_to_inner.is_empty() {
        return;
    }
    let path = qn.to_component_path();
    for (outer_prefix, inner_prefix) in outer_to_inner {
        if let Some(relative) = path.strip_prefix(outer_prefix) {
            *qn = ast::QualifiedName::from_dotted(inner_prefix.join(&relative).as_str());
            return;
        }
    }
}

fn bridge_scope_matches_connection_scope(
    inner_outer_prefix: &rumoca_core::ComponentPath,
    connection_scope: &str,
) -> bool {
    let bridge_scope = inner_outer_prefix.parent().unwrap_or_default();
    bridge_scope == rumoca_core::ComponentPath::from_flat_path(connection_scope)
}

fn redirect_inner_outer_bridge_for_scope(
    qn: &mut ast::QualifiedName,
    inner_outer_to_parent_inner: &ast::AstIndexMap<
        rumoca_core::ComponentPath,
        rumoca_core::ComponentPath,
    >,
    connection_scope: &str,
) {
    if inner_outer_to_parent_inner.is_empty() {
        return;
    }
    let path = qn.to_component_path();
    for (inner_outer_prefix, parent_inner_prefix) in inner_outer_to_parent_inner {
        if !bridge_scope_matches_connection_scope(inner_outer_prefix, connection_scope) {
            continue;
        }
        if let Some(relative) = path.strip_prefix(inner_outer_prefix) {
            *qn = ast::QualifiedName::from_dotted(parent_inner_prefix.join(&relative).as_str());
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
pub(super) fn redirect_connection_for_inner_outer(
    conn: &ast::InstanceScalarConnection,
    overlay: &ast::InstanceOverlay,
) -> Result<ast::InstanceScalarConnection, ast::InstanceConnectionConstructionError> {
    let mut a = conn.a().clone();
    let mut b = conn.b().clone();
    let mut scope = conn.scope().to_string();
    let a_before = a.to_flat_string();
    let b_before = b.to_flat_string();

    // First pass: redirect pure outer→inner
    redirect_qualified_name(&mut a, &overlay.outer_prefix_to_inner);
    redirect_qualified_name(&mut b, &overlay.outer_prefix_to_inner);
    let a_after = a.to_flat_string();
    let b_after = b.to_flat_string();

    if a_before != a_after || b_before != b_after {
        scope.clear();
    } else if !overlay.inner_outer_to_parent_inner.is_empty() {
        // Second pass: inner outer bridge redirect (only when first pass had no effect)
        redirect_inner_outer_bridge_for_scope(
            &mut a,
            &overlay.inner_outer_to_parent_inner,
            conn.scope(),
        );
        redirect_inner_outer_bridge_for_scope(
            &mut b,
            &overlay.inner_outer_to_parent_inner,
            conn.scope(),
        );
        let a_bridged = a_after != a.to_flat_string();
        let b_bridged = b_after != b.to_flat_string();
        if a_bridged || b_bridged {
            scope.clear();
        }
    }
    ast::InstanceScalarConnection::new(a, b, conn.connector_type(), conn.span(), scope)
}

/// Union the connected domains that proper ancestor scopes already cover for
/// this selection.
///
/// Root scope has no parent, so its interface flows always need `flow = 0` for
/// standalone checking; non-root scopes are covered by whatever proper ancestor
/// scopes connected of the same declaration. Extracting the walk keeps that rule
/// in one named place rather than nested inside the row loop.
fn ancestor_covered_domain(
    domains_by_scope: &IndexMap<String, IndexMap<rumoca_core::VarName, flat::ConnectedDomain>>,
    scope: &str,
    scope_path: &rumoca_core::ComponentPath,
    base: &rumoca_core::VarName,
) -> flat::ConnectedDomain {
    let mut covered = flat::ConnectedDomain::unconnected();
    if scope.is_empty() {
        return covered;
    }
    for (candidate, domains) in domains_by_scope {
        let candidate_path = rumoca_core::ComponentPath::from_flat_path(candidate);
        if is_proper_component_path_ancestor(&candidate_path, scope_path)
            && let Some(domain) = domains.get(base)
        {
            covered.union_with(domain);
        }
    }
    covered
}

/// Collect the coordinates of `selected` that neither an ancestor scope nor an
/// earlier selection has already claimed, marking each one as claimed.
///
/// `span` is supplied by the caller and used verbatim for both refusals. It is
/// deliberately never reconstructed here: a contradicting-domain diagnostic must
/// keep pointing at the connection that produced it, and rebuilding a span from
/// whatever is in scope is how that silently stops being true.
fn uncovered_flow_coordinates(
    selected: &flat::ConnectedDomain,
    dims: &[i64],
    covered: &flat::ConnectedDomain,
    already: &mut flat::ConnectedDomain,
    var_name: &rumoca_core::VarName,
    span: Span,
) -> Result<Vec<Vec<i64>>, FlattenError> {
    let mut coordinates = Vec::new();
    for coordinate in selected
        .connected_coordinates(dims)
        .map_err(|reason| contradicting_domain(var_name, span, reason))?
    {
        if covered.covers(&coordinate) || already.covers(&coordinate) {
            continue;
        }
        already
            .mark(dims, &coordinate)
            .map_err(|reason| contradicting_domain(var_name, span, reason))?;
        coordinates.push(coordinate);
    }
    Ok(coordinates)
}

#[cfg(test)]
mod equation_generation_tests {
    use super::is_single_identifier_relative_path;
    use super::*;

    fn conn(a: &str, b: &str, scope: &str) -> ast::InstanceScalarConnection {
        ast::InstanceScalarConnection::new(
            ast::QualifiedName::from_dotted(a),
            ast::QualifiedName::from_dotted(b),
            None,
            rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("inner_outer_connection_test.mo"),
                1,
                2,
            ),
            scope.to_string(),
        )
        .expect("test connection has valid endpoints and provenance")
    }

    #[test]
    fn equation_shape_refuses_a_selected_composite_declaration() {
        let mut model = flat::Model::new();
        model.add_variable(
            rumoca_core::VarName::new("bus"),
            flat::Variable {
                dims: vec![2],
                is_primitive: false,
                ..flat::Variable::empty_with_span(rumoca_core::Span::DUMMY)
            },
        );

        let error = require_var_shape(
            &model,
            &rumoca_core::VarName::new("bus[1]"),
            rumoca_core::Span::DUMMY,
        )
        .expect_err("a selected connector cannot masquerade as an equation value");

        assert!(error.to_string().contains("selected composite declaration"));
    }

    fn overlay_with_inner_outer_bridge() -> ast::InstanceOverlay {
        let mut overlay = ast::InstanceOverlay::default();
        overlay.inner_outer_to_parent_inner.insert(
            rumoca_core::ComponentPath::from_flat_path("tankController.makeProduct.stateGraphRoot"),
            rumoca_core::ComponentPath::from_flat_path("stateGraphRoot"),
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

        let redirected = redirect_connection_for_inner_outer(&input, &overlay)
            .expect("redirected connection remains valid");

        assert_eq!(
            redirected.a().to_flat_string(),
            "tankController.makeProduct.outerState.subgraphStatePort"
        );
        assert_eq!(
            redirected.b().to_flat_string(),
            "stateGraphRoot.subgraphStatePort"
        );
        assert_eq!(redirected.scope(), "");
    }

    #[test]
    fn inner_outer_bridge_keeps_child_scope_connection_on_local_inner() {
        let overlay = overlay_with_inner_outer_bridge();
        let input = conn(
            "tankController.makeProduct.fillTank1.outerStatePort.subgraphStatePort",
            "tankController.makeProduct.stateGraphRoot.subgraphStatePort",
            "tankController.makeProduct.fillTank1",
        );

        let redirected = redirect_connection_for_inner_outer(&input, &overlay)
            .expect("redirected connection remains valid");

        assert_eq!(
            redirected.a().to_flat_string(),
            "tankController.makeProduct.fillTank1.outerStatePort.subgraphStatePort"
        );
        assert_eq!(
            redirected.b().to_flat_string(),
            "tankController.makeProduct.stateGraphRoot.subgraphStatePort"
        );
        assert_eq!(redirected.scope(), "tankController.makeProduct.fillTank1");
    }

    #[test]
    fn connection_entry_refuses_an_unfinalized_empty_occurrence_catalog_before_mutation() {
        let overlay = ast::InstanceOverlay::new();
        let Err(error) = crate::finalized_overconstrained_catalog(&overlay) else {
            panic!("even an empty overconstrained catalog requires one-shot finalization");
        };

        assert!(
            error
                .to_string()
                .contains("finalized overconstrained occurrence catalog"),
            "unexpected refusal: {error}"
        );
    }
}
