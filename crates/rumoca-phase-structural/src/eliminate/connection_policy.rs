use std::collections::HashSet;

use super::{
    Dae, Expression, OpBinary, VarName, full_var_ref, runtime_partition_or_event_refs_var,
    unknown_is_fixed,
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
    let Some(lhs_ref) = full_var_ref(lhs) else {
        return false;
    };
    let Some(rhs_ref) = full_var_ref(rhs) else {
        return false;
    };
    [lhs_ref.var_name(), rhs_ref.var_name()].iter().all(|name| {
        can_eliminate_scalar_connection_alias_var(dae, name, runtime_defined_discrete_targets)
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
    let Some(lhs_ref) = full_var_ref(lhs) else {
        return false;
    };
    let Some(rhs_ref) = full_var_ref(rhs) else {
        return false;
    };
    [lhs_ref.var_name(), rhs_ref.var_name()].iter().all(|name| {
        live.iter().any(|live_name| live_name == *name)
            && can_eliminate_scalar_connection_alias_var(
                dae,
                name,
                runtime_defined_discrete_targets,
            )
    })
}

fn can_eliminate_scalar_connection_alias_var(
    dae: &Dae,
    var_name: &VarName,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    !var_name.as_str().contains('[')
        && !unknown_is_fixed(dae, var_name)
        && dae.variables.algebraics.contains_key(var_name)
        && !runtime_defined_discrete_targets.contains(var_name.as_str())
        && !runtime_partition_or_event_refs_var(dae, var_name)
}
