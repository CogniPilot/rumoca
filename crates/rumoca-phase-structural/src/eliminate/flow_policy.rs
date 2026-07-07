use super::scalar_shape::var_ref_is_scalar_after_subscripts;
use super::{
    Dae, Expression, StructuralError, VarName, collect_var_ref_nodes,
    exact_reference_expr_name_in_dae,
};
use crate::variable_scope::{DaeVariableScope, DaeVariableShape, scalar_count_from_dims};

pub(super) fn is_flow_equation_origin(origin: &str) -> bool {
    origin == "flow" || origin.starts_with("flow sum equation:")
}

pub(super) fn expr_contains_indexed_multiscalar_ref(
    expr: &Expression,
    dae: &Dae,
) -> Result<bool, StructuralError> {
    let scope = DaeVariableScope::new(dae);
    if let Some(exact_name) = exact_reference_expr_name_in_dae(dae, expr)
        && let Some(var) = scope.exact(&exact_name)
        && scalar_count_from_dims(&exact_name, &var.dims)? == 1
    {
        return Ok(false);
    }
    let mut refs = Vec::new();
    collect_var_ref_nodes(expr, &mut refs);
    for (name, subscripts) in refs {
        if name.as_str() == "time" {
            continue;
        }
        if !reference_touches_continuous_unknown(dae, name.var_name()) {
            continue;
        }
        match reference_has_embedded_multiscalar_index(dae, name.var_name()) {
            Ok(true) => return Ok(true),
            Ok(false) => {}
            Err(StructuralError::ContractViolation { reason, .. })
            | Err(StructuralError::UnspannedContractViolation { reason })
                if reason.contains("missing DAE variable metadata") =>
            {
                return Ok(true);
            }
            Err(err) => return Err(err),
        }
        if subscripts.is_empty() {
            continue;
        }
        match scope.shape_for_reference(&name) {
            Err(StructuralError::ContractViolation { reason, .. })
            | Err(StructuralError::UnspannedContractViolation { reason })
                if reason.contains("missing DAE variable metadata") =>
            {
                return Ok(true);
            }
            Err(err) => return Err(err),
            Ok(DaeVariableShape::Dimensions(dims)) => {
                if scalar_count_from_dims(name.var_name(), &dims)? > 1 {
                    return Ok(true);
                }
            }
            Ok(DaeVariableShape::StructuredAggregate) => return Ok(true),
        }
    }
    Ok(false)
}

pub(super) fn expr_contains_indexed_multiscalar_slice_ref(
    expr: &Expression,
    dae: &Dae,
) -> Result<bool, StructuralError> {
    let mut refs = Vec::new();
    collect_var_ref_nodes(expr, &mut refs);
    for (name, subscripts) in refs {
        if subscripts.is_empty() || name.as_str() == "time" {
            continue;
        }
        if !reference_touches_continuous_unknown(dae, name.var_name()) {
            continue;
        }
        let reference_span = name
            .span()
            .or_else(|| subscripts.first().map(rumoca_core::Subscript::span))
            .ok_or_else(|| StructuralError::UnspannedContractViolation {
                reason: format!(
                    "cannot classify indexed multiscalar reference `{}` without source provenance",
                    name.as_str()
                ),
            })?;
        if !var_ref_is_scalar_after_subscripts(&name, &subscripts, reference_span, dae)? {
            return Ok(true);
        }
    }
    Ok(false)
}

fn reference_touches_continuous_unknown(dae: &Dae, name: &VarName) -> bool {
    continuous_unknown_exists(dae, name)
        || strip_embedded_subscripts(name.as_str())
            .is_some_and(|base| continuous_unknown_exists(dae, &VarName::new(base)))
        || rumoca_ir_dae::split_complex_field_suffix(name.as_str())
            .is_some_and(|(base, _)| continuous_unknown_exists(dae, &VarName::new(base)))
}

fn continuous_unknown_exists(dae: &Dae, name: &VarName) -> bool {
    dae.variables.states.contains_key(name)
        || dae.variables.algebraics.contains_key(name)
        || dae.variables.outputs.contains_key(name)
}

fn reference_has_embedded_multiscalar_index(
    dae: &Dae,
    name: &VarName,
) -> Result<bool, StructuralError> {
    if let Some(base) = strip_embedded_subscripts(name.as_str()) {
        let base_name = VarName::new(base);
        if let Some(base_var) = DaeVariableScope::new(dae).exact(&base_name) {
            return Ok(scalar_count_from_dims(&base_name, &base_var.dims)? > 1);
        }
    }
    let Some(scalar) = rumoca_core::parse_scalar_name(name.as_str()) else {
        return Ok(false);
    };
    let base_name = VarName::new(scalar.base);
    let Some(base_var) = DaeVariableScope::new(dae).exact(&base_name) else {
        return Ok(false);
    };
    Ok(scalar_count_from_dims(&base_name, &base_var.dims)? > 1)
}

fn strip_embedded_subscripts(name: &str) -> Option<String> {
    if !name.contains('[') {
        return None;
    }
    let mut stripped = String::with_capacity(name.len());
    let mut depth = 0usize;
    for ch in name.chars() {
        match ch {
            '[' => depth = depth.checked_add(1)?,
            ']' => depth = depth.checked_sub(1)?,
            _ if depth == 0 => stripped.push(ch),
            _ => {}
        }
    }
    (depth == 0 && stripped != name).then_some(stripped)
}
