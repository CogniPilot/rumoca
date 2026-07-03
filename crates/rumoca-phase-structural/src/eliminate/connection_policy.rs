use std::collections::HashSet;

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
    if !connection_alias_refs_are_continuous_scalar_algebraics(
        dae,
        eq_rhs,
        runtime_defined_discrete_targets,
    ) {
        return true;
    }
    live.len() > 1
        && !can_eliminate_continuous_connection_alias(
            dae,
            eq_rhs,
            live,
            runtime_defined_discrete_targets,
        )
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
    !unknown_is_fixed(dae, var_name)
        && dae.variables.algebraics.contains_key(var_name)
        && !runtime_defined_discrete_targets.contains(var_name.as_str())
        && !runtime_partition_or_event_refs_var(dae, var_name)
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
    } else if rhs_name.as_ref() == Some(&var_name) {
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
