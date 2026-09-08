//! Fail-closed semantic validation for connection construction.

use super::*;

/// Validate all connections before processing.
///
/// Checks for:
/// - CONN-001/CONN-003: Flow/non-flow prefix consistency (homogeneity)
/// - CONN-002: Type compatibility (Real vs Integer vs Boolean)
/// - CONN-008: Array dimension compatibility
///
/// For connector-level connections (non-primitive paths), validation is
/// performed on the expanded sub-variables during connection set building.
pub(super) fn validate_connections(
    connections: &[&ast::InstanceScalarConnection],
    flat: &flat::Model,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Result<(), FlattenError> {
    for conn in connections {
        let path_a = conn.a().to_flat_string();
        let path_b = conn.b().to_flat_string();
        let var_a = rumoca_core::VarName::new(&path_a);
        let var_b = rumoca_core::VarName::new(&path_b);
        let span = conn.span();

        // Only validate primitive-to-primitive connections directly
        // Connector-level connections are validated when expanded to sub-variables
        let a_is_primitive = is_primitive_flat_var(flat, &var_a);
        let b_is_primitive = is_primitive_flat_var(flat, &var_b);
        let a_subscript_prim = if a_is_primitive {
            false
        } else {
            has_proven_primitive_array_selection(&var_a, flat, span)?
        };
        let b_subscript_prim = if b_is_primitive {
            false
        } else {
            has_proven_primitive_array_selection(&var_b, flat, span)?
        };

        if (a_is_primitive || a_subscript_prim) && (b_is_primitive || b_subscript_prim) {
            // Validate flow prefix consistency (CONN-001/CONN-003)
            validate_flow_consistency(flat, &var_a, &var_b, span)?;

            // Validate type compatibility (CONN-002)
            validate_type_compatibility(flat, &var_a, &var_b, span)?;

            // Validate array dimension compatibility (CONN-008)
            validate_dimension_compatibility(flat, &var_a, &var_b, span)?;
            validate_quantity_compatibility(flat, &var_a, &var_b, span)?;
            continue;
        }

        // Connector-level connection: validate matched primitive members after expansion.
        let subs_a = find_sub_variables_indexed(&path_a, prefix_children, var_index);
        let subs_b = find_sub_variables_indexed(&path_b, prefix_children, var_index);
        if !subs_a.is_empty() && !subs_b.is_empty() {
            let ctx = ExpandedValidationCtx {
                path_a: &path_a,
                path_b: &path_b,
                flat,
                span,
                var_index,
            };
            validate_expanded_connector_connection(&subs_a, &subs_b, &ctx)?;
        }
    }
    Ok(())
}

#[derive(Debug, Clone)]
pub(super) struct ValidationVarInfo {
    flow: bool,
    type_id: TypeId,
    declaration_dims: Vec<i64>,
    pub(super) dims: Vec<i64>,
    quantity: Option<String>,
}

struct ExpandedValidationCtx<'a> {
    path_a: &'a str,
    path_b: &'a str,
    flat: &'a flat::Model,
    span: Span,
    var_index: &'a ConnectionVarIndex,
}

pub(super) fn require_validation_var_info(
    flat: &flat::Model,
    var: &rumoca_core::VarName,
    span: Span,
) -> Result<ValidationVarInfo, FlattenError> {
    if let Some(v) = flat.variables.get(var) {
        return Ok(ValidationVarInfo {
            flow: v.flow,
            type_id: v.type_id,
            declaration_dims: v.dims.clone(),
            dims: v.dims.clone(),
            quantity: v.quantity.clone(),
        });
    }

    // Subscripted references (e.g., "x[1]") select from an array declaration.
    let selected = declared_array_element_evidence(var, flat).map_err(|reason| {
        FlattenError::invalid_connection_evidence(
            format!("connection member `{var}` has invalid declaration evidence: {reason}"),
            span,
        )
    })?;
    let Some(selected) = selected else {
        return Err(FlattenError::invalid_connection_evidence(
            format!("connection member `{var}` has no Flat declaration evidence"),
            span,
        ));
    };
    let base_var = selected.declaration;

    Ok(ValidationVarInfo {
        flow: base_var.flow,
        type_id: base_var.type_id,
        declaration_dims: base_var.dims.clone(),
        // MLS §10.5: indexing a subset of dimensions preserves the remaining
        // dimensions (e.g., `A[1]` of `A[2,3]` denotes `Real[3]`). One shared
        // resolution keeps CONN-008 validation and the generated equations'
        // scalar counts from drifting apart.
        dims: base_var.dims[selected.indices.len()..].to_vec(),
        quantity: base_var.quantity.clone(),
    })
}

/// Validate that connected variables have consistent flow prefixes.
///
/// Per CONN-001 (Homogeneity) and CONN-003 (Flow-to-flow):
/// Both must be flow or both must be non-flow.
pub(super) fn validate_flow_consistency(
    flat: &flat::Model,
    var_a: &rumoca_core::VarName,
    var_b: &rumoca_core::VarName,
    span: Span,
) -> Result<(), FlattenError> {
    let info_a = require_validation_var_info(flat, var_a, span)?;
    let info_b = require_validation_var_info(flat, var_b, span)?;
    let is_flow_a = info_a.flow;
    let is_flow_b = info_b.flow;

    if is_flow_a != is_flow_b {
        return Err(FlattenError::incompatible_connectors(
            format!(
                "{} ({})",
                var_a.as_str(),
                if is_flow_a { "flow" } else { "non-flow" }
            ),
            format!(
                "{} ({})",
                var_b.as_str(),
                if is_flow_b { "flow" } else { "non-flow" }
            ),
            span,
        ));
    }
    Ok(())
}

/// Validate that connected variables agree on the quantity attribute.
///
/// Per CONN-005 (MLS §9.2): variables with non-empty quantity attributes
/// must match.
pub(super) fn validate_quantity_compatibility(
    flat: &flat::Model,
    var_a: &rumoca_core::VarName,
    var_b: &rumoca_core::VarName,
    span: Span,
) -> Result<(), FlattenError> {
    let quantity_a = require_validation_var_info(flat, var_a, span)?.quantity;
    let quantity_b = require_validation_var_info(flat, var_b, span)?.quantity;
    if let (Some(qa), Some(qb)) = (&quantity_a, &quantity_b)
        && !qa.is_empty()
        && !qb.is_empty()
        && qa != qb
    {
        return Err(FlattenError::incompatible_connectors(
            format!("{} (quantity: {qa})", var_a.as_str()),
            format!("{} (quantity: {qb})", var_b.as_str()),
            span,
        ));
    }
    Ok(())
}

/// Validate that connected variables have compatible types.
///
/// Per CONN-002 (Type matching): Matched primitive components must have
/// the same primitive types (Real, Integer, Boolean, String).
pub(super) fn validate_type_compatibility(
    flat: &flat::Model,
    var_a: &rumoca_core::VarName,
    var_b: &rumoca_core::VarName,
    span: Span,
) -> Result<(), FlattenError> {
    let info_a = require_validation_var_info(flat, var_a, span)?;
    let info_b = require_validation_var_info(flat, var_b, span)?;
    let type_a = require_effective_connection_type(flat, var_a, &info_a, span)?;
    let type_b = require_effective_connection_type(flat, var_b, &info_b, span)?;
    if type_a != type_b {
        return Err(FlattenError::incompatible_connectors(
            format!("{} (type_id: {:?})", var_a.as_str(), type_a),
            format!("{} (type_id: {:?})", var_b.as_str(), type_b),
            span,
        ));
    }
    Ok(())
}

fn require_effective_connection_type(
    flat: &flat::Model,
    variable: &rumoca_core::VarName,
    info: &ValidationVarInfo,
    span: Span,
) -> Result<TypeId, FlattenError> {
    let effective = flat.effective_types.get(&info.type_id).ok_or_else(|| {
        FlattenError::invalid_connection_evidence(
            format!(
                "connection member `{variable}` has type identity {:?} missing from the finalized Flat effective-type catalog",
                info.type_id
            ),
            span,
        )
    })?;
    if effective.dimensions() != info.declaration_dims {
        return Err(FlattenError::invalid_connection_evidence(
            format!(
                "connection member `{variable}` has declaration dimensions {:?}, but effective type {:?} records {:?}",
                info.declaration_dims,
                info.type_id,
                effective.dimensions()
            ),
            span,
        ));
    }
    Ok(effective.canonical_type())
}

/// Validate that connected variables have compatible array dimensions.
///
/// Per CONN-008 (MLS §9.2): Array dimensions must match for connection.
/// Per SPEC_0007: dimension evaluation happens in typecheck before flatten.
///
/// Empty dimensions `[]` indicates a scalar variable (0-dimensional).
/// Scalars must connect to scalars; arrays must connect to same-dimension arrays.
pub(super) fn validate_dimension_compatibility(
    flat: &flat::Model,
    var_a: &rumoca_core::VarName,
    var_b: &rumoca_core::VarName,
    span: Span,
) -> Result<(), FlattenError> {
    let info_a = require_validation_var_info(flat, var_a, span)?;
    let info_b = require_validation_var_info(flat, var_b, span)?;
    let dims_a = &info_a.dims;
    let dims_b = &info_b.dims;

    if dims_a != dims_b {
        return Err(FlattenError::incompatible_connectors(
            format!("{} (dims: {:?})", var_a.as_str(), dims_a),
            format!("{} (dims: {:?})", var_b.as_str(), dims_b),
            span,
        ));
    }
    Ok(())
}

fn validate_expanded_connector_connection(
    subs_a: &[rumoca_core::VarName],
    subs_b: &[rumoca_core::VarName],
    ctx: &ExpandedValidationCtx<'_>,
) -> Result<(), FlattenError> {
    match complete_expanded_member_direction(
        ctx.path_a,
        subs_a,
        ctx.path_b,
        subs_b,
        ctx.var_index,
        ctx.span,
    )? {
        ExpandedMemberDirection::Forward => validate_expanded_member_direction(subs_a, subs_b, ctx),
        ExpandedMemberDirection::Reverse => {
            let reverse_ctx = ExpandedValidationCtx {
                path_a: ctx.path_b,
                path_b: ctx.path_a,
                flat: ctx.flat,
                span: ctx.span,
                var_index: ctx.var_index,
            };
            validate_expanded_member_direction(subs_b, subs_a, &reverse_ctx)
        }
    }
}

fn validate_expanded_member_direction(
    source_members: &[rumoca_core::VarName],
    target_members: &[rumoca_core::VarName],
    ctx: &ExpandedValidationCtx<'_>,
) -> Result<(), FlattenError> {
    let sub_match_index = ConnectionSubMatchIndex::new(ctx.path_b, target_members, ctx.var_index);

    for sub_a in source_members {
        let Some((suffix_a, indices_a)) = extract_suffix(sub_a.as_str(), ctx.path_a) else {
            continue;
        };
        let normalized_indices_a = strip_explicit_path_indices(&indices_a, ctx.path_a);

        let Some(var_b_match) =
            find_matching_var_b_indexed(&suffix_a, &normalized_indices_a, &sub_match_index)
        else {
            continue;
        };
        let (conn_a, conn_b) = resolved_expanded_member_pair(
            sub_a,
            ctx.path_a,
            ctx.path_b,
            &var_b_match,
            &indices_a,
            ctx.flat,
            ctx.span,
        )?;

        validate_flow_consistency(ctx.flat, &conn_a, &conn_b, ctx.span)?;
        validate_type_compatibility(ctx.flat, &conn_a, &conn_b, ctx.span)?;
        validate_dimension_compatibility(ctx.flat, &conn_a, &conn_b, ctx.span)?;
        validate_quantity_compatibility(ctx.flat, &conn_a, &conn_b, ctx.span)?;
    }
    Ok(())
}

#[derive(Clone, Copy)]
pub(super) enum ExpandedMemberDirection {
    Forward,
    Reverse,
}

pub(super) fn complete_expanded_member_direction(
    path_a: &str,
    subs_a: &[rumoca_core::VarName],
    path_b: &str,
    subs_b: &[rumoca_core::VarName],
    var_index: &ConnectionVarIndex,
    span: Span,
) -> Result<ExpandedMemberDirection, FlattenError> {
    let matched_a = count_expanded_connector_matches(path_a, subs_a, path_b, subs_b, var_index);
    let matched_b = count_expanded_connector_matches(path_b, subs_b, path_a, subs_a, var_index);
    let forward_complete = matched_a == subs_a.len() && matched_a != 0;
    let reverse_complete = matched_b == subs_b.len() && matched_b != 0;

    if forward_complete && (reverse_complete || matched_b == 0) {
        return Ok(ExpandedMemberDirection::Forward);
    }
    if reverse_complete && matched_a == 0 {
        return Ok(ExpandedMemberDirection::Reverse);
    }
    Err(FlattenError::incompatible_connectors(path_a, path_b, span))
}

fn count_expanded_connector_matches(
    source_path: &str,
    source_members: &[rumoca_core::VarName],
    target_path: &str,
    target_members: &[rumoca_core::VarName],
    var_index: &ConnectionVarIndex,
) -> usize {
    let target_index = ConnectionSubMatchIndex::new(target_path, target_members, var_index);
    source_members
        .iter()
        .filter(|member| {
            let Some((suffix, indices)) = extract_suffix(member.as_str(), source_path) else {
                return false;
            };
            let indices = strip_explicit_path_indices(&indices, source_path);
            find_matching_var_b_indexed(&suffix, &indices, &target_index).is_some()
        })
        .count()
}

/// Reject the unsupported part of MLS §9.1.3 before connection-set building.
///
/// Identically declared expandable connectors need no augmentation and can use
/// the normal connector expansion below. If either side is expandable and any
/// existing member is absent on the peer, connecting only the intersection
/// would silently change the model. Full member-union augmentation belongs
/// before connection-set construction; until that elaboration exists, fail
/// explicitly at this boundary.
pub(super) fn reject_expandable_connector_augmentation(
    connections: &[&ast::InstanceScalarConnection],
    flat: &flat::Model,
    endpoint_index: &ConnectionEndpointIndex,
    prefix_children: &FxHashMap<String, Vec<rumoca_core::VarName>>,
    var_index: &ConnectionVarIndex,
) -> Result<(), FlattenError> {
    for conn in connections {
        let path_a = conn.a().to_flat_string();
        let path_b = conn.b().to_flat_string();
        let subs_a = find_sub_variables_indexed(&path_a, prefix_children, var_index);
        let subs_b = find_sub_variables_indexed(&path_b, prefix_children, var_index);
        if subs_a.is_empty() || subs_b.is_empty() {
            if endpoint_index.needs_expandable_augmentation(conn.a())
                || endpoint_index.needs_expandable_augmentation(conn.b())
            {
                return Err(FlattenError::unsupported_expandable_connector_augmentation(
                    path_a,
                    path_b,
                    conn.span(),
                ));
            }
            continue;
        }

        let is_expandable = |members: &[rumoca_core::VarName]| {
            members.iter().any(|name| {
                flat.variables
                    .get(name)
                    .is_some_and(|var| var.from_expandable_connector)
            })
        };
        if !is_expandable(&subs_a) && !is_expandable(&subs_b) {
            continue;
        }

        let matched_a =
            count_expanded_connector_matches(&path_a, &subs_a, &path_b, &subs_b, var_index);
        let matched_b =
            count_expanded_connector_matches(&path_b, &subs_b, &path_a, &subs_a, var_index);
        if matched_a != subs_a.len() || matched_b != subs_b.len() {
            return Err(FlattenError::unsupported_expandable_connector_augmentation(
                path_a,
                path_b,
                conn.span(),
            ));
        }
    }
    Ok(())
}
