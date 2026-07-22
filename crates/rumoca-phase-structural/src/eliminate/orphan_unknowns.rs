use rumoca_core::{Expression, Reference, Subscript, VarName};
use rumoca_ir_dae::{Dae, expr_contains_var};

use super::{
    collect_exact_reference_expr_names_in_dae, exact_subscript_index_in_dae, scalar_count_from_dims,
};

pub(super) fn drop_unreferenced_continuous_unknowns(dae: &mut Dae) {
    let exact_references = collect_continuous_exact_references(dae);
    let referenced = |name: &VarName| exact_reference_keeps_unknown(dae, &exact_references, name);
    let algebraics = dae
        .variables
        .algebraics
        .keys()
        .filter(|name| !referenced(name))
        .cloned()
        .collect::<Vec<_>>();
    let outputs = dae
        .variables
        .outputs
        .keys()
        .filter(|name| !referenced(name))
        .cloned()
        .collect::<Vec<_>>();
    for name in algebraics {
        dae.variables.algebraics.shift_remove(&name);
    }
    for name in outputs {
        dae.variables.outputs.shift_remove(&name);
    }
}

fn collect_continuous_exact_references(dae: &Dae) -> Vec<VarName> {
    let mut refs = Vec::new();
    for equation in &dae.continuous.equations {
        if let Some(lhs) = &equation.lhs {
            collect_exact_reference_expr_names_in_dae(
                dae,
                &Expression::VarRef {
                    name: lhs.clone(),
                    subscripts: Vec::new(),
                    span: equation.span,
                },
                &mut refs,
            );
            collect_scalarized_lhs_owners(dae, lhs, equation.scalar_count, &mut refs);
        }
        collect_exact_reference_expr_names_in_dae(dae, &equation.rhs, &mut refs);
    }
    refs.sort();
    refs.dedup();
    refs
}

fn collect_scalarized_lhs_owners(
    dae: &Dae,
    lhs: &Reference,
    scalar_count: usize,
    out: &mut Vec<VarName>,
) {
    if scalar_count == 0 {
        return;
    }
    let Some((base, selectors)) = lhs_base_and_selectors(lhs) else {
        return;
    };
    let Some(base_var) = crate::variable_scope::DaeVariableScope::new(dae).exact(&base) else {
        return;
    };
    let Some(owned) =
        projected_lhs_owner_names(dae, &base, &base_var.dims, selectors, scalar_count)
    else {
        return;
    };
    out.extend(owned);
}

fn lhs_base_and_selectors(lhs: &Reference) -> Option<(VarName, &[Subscript])> {
    let Some(component_ref) = lhs.component_ref() else {
        return Some((lhs.var_name().clone(), &[]));
    };
    let selectors = component_ref.parts.last()?.subs.as_slice();
    let mut base_ref = component_ref.clone();
    base_ref.parts.last_mut()?.subs.clear();
    Some((base_ref.to_var_name(), selectors))
}

fn projected_lhs_owner_names(
    dae: &Dae,
    base: &VarName,
    dims: &[i64],
    selectors: &[Subscript],
    scalar_count: usize,
) -> Option<Vec<VarName>> {
    if dims.is_empty() || (!selectors.is_empty() && selectors.len() != dims.len()) {
        return None;
    }
    let fixed_indices = if selectors.is_empty() {
        vec![None; dims.len()]
    } else {
        selectors
            .iter()
            .zip(dims)
            .map(|(selector, dim)| match selector {
                Subscript::Colon { .. } if *dim > 0 => Some(None),
                Subscript::Index { value, .. } if *value > 0 && *value <= *dim => {
                    usize::try_from(*value).ok().map(Some)
                }
                Subscript::Index { .. } => None,
                Subscript::Expr { .. } => exact_subscript_index_in_dae(dae, selector)
                    .filter(|value| *value > 0 && *value <= *dim)
                    .and_then(|value| usize::try_from(value).ok())
                    .map(Some),
                Subscript::Colon { .. } => None,
            })
            .collect::<Option<Vec<_>>>()?
    };
    let projected_dims = fixed_indices
        .iter()
        .zip(dims)
        .filter_map(|(index, dim)| index.is_none().then_some(*dim))
        .collect::<Vec<_>>();
    if scalar_count_from_dims(base, &projected_dims).ok()? != scalar_count {
        return None;
    }

    let owned = (0..scalar_count)
        .map(|flat_index| {
            let projected = if projected_dims.is_empty() {
                Vec::new()
            } else {
                rumoca_ir_dae::flat_index_to_subscripts(&projected_dims, flat_index)?
            };
            let mut projected = projected.into_iter();
            let indices = fixed_indices
                .iter()
                .map(|index| index.or_else(|| projected.next()))
                .collect::<Option<Vec<_>>>()?;
            Some(VarName::new(rumoca_ir_dae::format_subscript_key(
                base.as_str(),
                &indices,
            )))
        })
        .collect::<Option<Vec<_>>>()?;
    owned
        .iter()
        .all(|name| {
            dae.variables.algebraics.contains_key(name) || dae.variables.outputs.contains_key(name)
        })
        .then_some(owned)
}

fn exact_reference_keeps_unknown(dae: &Dae, exact_refs: &[VarName], name: &VarName) -> bool {
    if exact_refs.binary_search(name).is_ok() {
        return true;
    }
    if exact_refs.iter().any(|exact_ref| {
        rumoca_core::parse_scalar_name(exact_ref.as_str())
            .is_some_and(|scalar| scalar.base == name.as_str())
    }) {
        return true;
    }
    if continuous_unknown_is_scalar(dae, name) {
        return false;
    }
    dae.continuous
        .equations
        .iter()
        .any(|eq| expr_contains_var(&eq.rhs, name))
}

fn continuous_unknown_is_scalar(dae: &Dae, name: &VarName) -> bool {
    dae.variables
        .algebraics
        .get(name)
        .or_else(|| dae.variables.outputs.get(name))
        .is_none_or(|var| var.dims.iter().all(|dim| *dim == 1))
}

pub(super) fn output_partition_contains_unknown(dae: &Dae, name: &VarName) -> bool {
    dae.variables.outputs.contains_key(name)
        || rumoca_ir_dae::component_base_name(name.as_str())
            .is_some_and(|base| dae.variables.outputs.contains_key(&VarName::new(base)))
}
