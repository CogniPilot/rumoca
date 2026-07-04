use std::collections::HashSet;

use super::runtime_protection::assignment_var_ref_name;
use super::{
    Dae, Expression, OpBinary, VarName, exact_reference_expr_name_in_dae,
    runtime_partition_or_event_refs_var, unknown_is_fixed,
};

pub(super) fn should_skip_connection_equation(
    dae: &Dae,
    eq_rhs: &Expression,
    is_connection_eq: bool,
    live: &[VarName],
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    if !is_connection_eq {
        return false;
    }
    let touches_runtime_discrete_path = super::expr_references_any_runtime_discrete_target(
        eq_rhs,
        runtime_defined_discrete_targets,
    ) || super::expr_references_any_discrete_name(dae, eq_rhs);
    if touches_runtime_discrete_path {
        return true;
    }
    if live.len() == 1
        && can_eliminate_scalar_connection_alias_var(
            dae,
            &live[0],
            runtime_defined_discrete_targets,
        )
    {
        return false;
    }
    let alias_refs_are_continuous = connection_alias_refs_are_continuous_scalar_algebraics(
        dae,
        eq_rhs,
        runtime_defined_discrete_targets,
    );
    let single_live_has_known_boundary = single_live_connection_alias_has_known_boundary(
        dae,
        eq_rhs,
        live,
        runtime_defined_discrete_targets,
    );
    let can_eliminate_multi_live = can_eliminate_continuous_connection_alias(
        dae,
        eq_rhs,
        live,
        runtime_defined_discrete_targets,
    );
    if !alias_refs_are_continuous && !single_live_has_known_boundary {
        return true;
    }
    live.len() > 1 && !can_eliminate_multi_live
}

fn connection_alias_refs_are_continuous_scalar_algebraics(
    dae: &Dae,
    eq_rhs: &Expression,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = eq_rhs
    else {
        return false;
    };
    let Some(lhs_name) = exact_reference_expr_name_in_dae(dae, lhs) else {
        return false;
    };
    let Some(rhs_name) = exact_reference_expr_name_in_dae(dae, rhs) else {
        return false;
    };
    [&lhs_name, &rhs_name].iter().all(|name| {
        can_eliminate_scalar_connection_alias_var(dae, name, runtime_defined_discrete_targets)
            || connection_alias_ref_is_structurally_known(dae, name)
    })
}

fn single_live_connection_alias_has_known_boundary(
    dae: &Dae,
    eq_rhs: &Expression,
    live: &[VarName],
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    let [live_name] = live else {
        return false;
    };
    can_eliminate_scalar_connection_alias_var(dae, live_name, runtime_defined_discrete_targets)
        && connection_expr_refs_are_live_or_structurally_known(dae, eq_rhs, live_name)
}

fn connection_expr_refs_are_live_or_structurally_known(
    dae: &Dae,
    eq_rhs: &Expression,
    live_name: &VarName,
) -> bool {
    let mut refs = indexmap::IndexSet::new();
    eq_rhs.collect_var_refs(&mut refs);
    refs.iter()
        .all(|name| name == live_name || connection_alias_ref_is_structurally_known(dae, name))
}

fn can_eliminate_continuous_connection_alias(
    dae: &Dae,
    eq_rhs: &Expression,
    live: &[VarName],
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = eq_rhs
    else {
        return false;
    };
    let Some(lhs_name) = exact_reference_expr_name_in_dae(dae, lhs) else {
        return false;
    };
    let Some(rhs_name) = exact_reference_expr_name_in_dae(dae, rhs) else {
        return false;
    };
    let refs = [&lhs_name, &rhs_name];
    let has_eliminable_live = refs.iter().any(|name| {
        live.iter().any(|live_name| live_name == *name)
            && can_eliminate_scalar_connection_alias_var(
                dae,
                name,
                runtime_defined_discrete_targets,
            )
    });
    has_eliminable_live
        && refs.iter().all(|name| {
            can_eliminate_scalar_connection_alias_var(dae, name, runtime_defined_discrete_targets)
                || connection_alias_ref_is_structurally_known(dae, name)
        })
}

fn can_eliminate_scalar_connection_alias_var(
    dae: &Dae,
    var_name: &VarName,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    scalar_connection_alias_var_size(dae, var_name).is_some_and(|size| size == 1)
        && !unknown_is_fixed(dae, var_name)
        && !runtime_defined_discrete_targets.contains(var_name.as_str())
        && !runtime_partition_or_event_refs_var(dae, var_name)
}

fn scalar_connection_alias_var_size(dae: &Dae, var_name: &VarName) -> Option<usize> {
    if is_scalarized_aggregate_element(dae, var_name)
        && !scalarized_element_has_non_connection_use(dae, var_name)
    {
        return None;
    }
    if is_scalarized_aggregate_element(dae, var_name) {
        return Some(1);
    }
    dae.variables
        .algebraics
        .get(var_name)
        .or_else(|| dae.variables.outputs.get(var_name))
        .map(|var| var.size())
}

fn is_scalarized_aggregate_element(dae: &Dae, var_name: &VarName) -> bool {
    scalar_element_base_name(var_name)
        .and_then(|base| {
            dae.variables
                .algebraics
                .get(&base)
                .or_else(|| dae.variables.outputs.get(&base))
        })
        .is_some_and(|var| var.size() > 1)
}

fn scalar_element_base_name(var_name: &VarName) -> Option<VarName> {
    let text = var_name.as_str();
    if !text.ends_with(']') {
        return None;
    }
    let open = text.rfind('[')?;
    if text[open + 1..text.len() - 1]
        .chars()
        .all(|ch| ch.is_ascii_digit() || ch == ',')
    {
        Some(VarName::new(&text[..open]))
    } else {
        None
    }
}

fn scalarized_element_has_non_connection_use(dae: &Dae, var_name: &VarName) -> bool {
    dae.continuous.equations.iter().any(|eq| {
        !eq.origin.starts_with("connection equation:")
            && expr_references_canonical_scalar(&eq.rhs, var_name)
    })
}

fn expr_references_canonical_scalar(expr: &Expression, var_name: &VarName) -> bool {
    let mut refs = Vec::new();
    super::collect_var_ref_nodes(expr, &mut refs);
    refs.iter().any(|(name, subscripts)| {
        assignment_var_ref_name(name.var_name(), subscripts)
            .as_ref()
            .is_some_and(|referenced| referenced == var_name)
            || aggregate_ref_matches_scalarized_var(name.var_name(), subscripts, var_name)
    })
}

fn aggregate_ref_matches_scalarized_var(
    referenced: &VarName,
    subscripts: &[rumoca_core::Subscript],
    var_name: &VarName,
) -> bool {
    if !subscripts.is_empty() {
        return false;
    }
    rumoca_core::parse_scalar_name(var_name.as_str())
        .is_some_and(|scalar| referenced.as_str() == scalar.base)
}

fn connection_alias_ref_is_structurally_known(dae: &Dae, var_name: &VarName) -> bool {
    let mut visiting = HashSet::new();
    connection_alias_ref_is_structurally_known_inner(dae, var_name, &mut visiting)
}

fn connection_alias_ref_is_structurally_known_inner(
    dae: &Dae,
    var_name: &VarName,
    visiting: &mut HashSet<String>,
) -> bool {
    if dae.variables.parameters.contains_key(var_name)
        || dae.variables.constants.contains_key(var_name)
        || var_name.as_str() == "time"
    {
        return true;
    }
    if !dae.variables.algebraics.contains_key(var_name) {
        return false;
    }
    if !visiting.insert(var_name.as_str().to_string()) {
        return false;
    }
    dae.continuous
        .equations
        .iter()
        .filter_map(|eq| direct_definition_expr_for_var(dae, &eq.rhs, var_name))
        .any(|expr| connection_alias_expr_is_structurally_known(dae, expr, visiting))
}

fn direct_definition_expr_for_var<'a>(
    dae: &Dae,
    expr: &'a Expression,
    var_name: &VarName,
) -> Option<&'a Expression> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expr
    else {
        return None;
    };
    let lhs_name = exact_reference_expr_name_in_dae(dae, lhs);
    let rhs_name = exact_reference_expr_name_in_dae(dae, rhs);
    if lhs_name.as_ref() == Some(&var_name) {
        Some(rhs)
    } else if rhs_name.as_ref() == Some(var_name) {
        Some(lhs)
    } else {
        None
    }
}

fn connection_alias_expr_is_structurally_known(
    dae: &Dae,
    expr: &Expression,
    visiting: &mut HashSet<String>,
) -> bool {
    let mut refs = indexmap::IndexSet::new();
    expr.collect_var_refs(&mut refs);
    refs.iter()
        .all(|name| connection_alias_ref_is_structurally_known_inner(dae, name, visiting))
}
