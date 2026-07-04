//! Aggregate (array) alias elimination helpers for the structural elimination
//! pass: recognizing whole-array `a = b` aliases and whether an array variable is
//! fully resolved element-by-element. Split out of `eliminate/mod.rs` to keep that
//! module under the SPEC_0021 size limit.

use super::*;

pub(super) fn aggregate_variable_fully_resolved(
    name: &VarName,
    var: &dae::Variable,
    resolved: &HashSet<VarName>,
) -> Result<bool, StructuralError> {
    if var.dims.is_empty() {
        return Ok(false);
    }
    let scalar_count = scalar_count_from_dims(name, &var.dims)?;
    if scalar_count <= 1 {
        return Ok(false);
    }
    for flat_index in 0..scalar_count {
        let scalar_name = VarName::new(dae::scalar_name_text_for_flat_index(
            name.as_str(),
            &var.dims,
            flat_index,
        ));
        if !resolved.contains(&scalar_name) {
            return Ok(embedded_component_elements_fully_resolved(
                name,
                scalar_count,
                resolved,
            ));
        }
    }
    if var.dims.len() == 1 {
        return Ok(false);
    }
    Ok(true)
}

fn embedded_component_elements_fully_resolved(
    name: &VarName,
    scalar_count: usize,
    resolved: &HashSet<VarName>,
) -> bool {
    let resolved_elements = resolved
        .iter()
        .filter(|resolved_name| {
            scalarized_leaf_base_name(resolved_name).is_some_and(|base| base == name.as_str())
        })
        .count();
    resolved_elements >= scalar_count
}

fn scalarized_leaf_base_name(name: &VarName) -> Option<&str> {
    let text = name.as_str();
    let open = text.rfind('[')?;
    if !text.ends_with(']')
        || !text[open + 1..text.len() - 1]
            .chars()
            .all(|ch| ch.is_ascii_digit() || ch == ',')
    {
        return None;
    }
    Some(&text[..open])
}

pub(super) fn is_scalarized_element_of_aggregate(
    dae: &Dae,
    name: &VarName,
) -> Result<bool, StructuralError> {
    let Some(scalar) = rumoca_core::parse_scalar_name(name.as_str()) else {
        return Ok(false);
    };
    if DaeVariableScope::new(dae).exact(name).is_some() {
        return Ok(false);
    }
    let base_name = VarName::new(scalar.base);
    let Some(base_var) = DaeVariableScope::new(dae).exact(&base_name) else {
        return Ok(false);
    };
    Ok(scalar_count_from_dims(&base_name, &base_var.dims)? > 1)
}

pub(super) fn aggregate_alias_for_elimination(
    dae: &Dae,
    rhs: &Expression,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> Result<Option<(VarName, Expression)>, StructuralError> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = rhs
    else {
        return Ok(None);
    };
    let Some(lhs_ref) = full_var_ref(lhs) else {
        return Ok(None);
    };
    let Some(rhs_ref) = full_var_ref(rhs) else {
        return Ok(None);
    };
    if lhs_ref.var_name() == rhs_ref.var_name() {
        return Ok(None);
    }
    if !same_aggregate_shape(dae, lhs_ref.var_name(), rhs_ref.var_name())? {
        return Ok(None);
    }

    let lhs_candidate = aggregate_alias_candidate(
        dae,
        lhs_ref,
        rhs_ref,
        rhs.span(),
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    )?;
    let rhs_candidate = aggregate_alias_candidate(
        dae,
        rhs_ref,
        lhs_ref,
        lhs.span(),
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    )?;
    Ok(match (lhs_candidate, rhs_candidate) {
        (Some(lhs), Some(rhs)) => Some(preferred_aggregate_alias_candidate(lhs, rhs)),
        (Some(candidate), None) | (None, Some(candidate)) => Some(candidate),
        (None, None) => None,
    })
}
