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
    has_aggregate_alias: bool,
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
    if has_aggregate_alias {
        return false;
    }
    if !connection_alias_refs_are_supported_continuous_scalars(
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

fn connection_alias_refs_are_supported_continuous_scalars(
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
    let names = [lhs_ref.var_name(), rhs_ref.var_name()];
    names.iter().all(|name| {
        can_eliminate_scalar_connection_alias_var(dae, name, runtime_defined_discrete_targets)
    }) || (names
        .iter()
        .any(|name| independently_scalar_indexed_component_field(dae, name))
        && names.iter().all(|name| {
            can_participate_in_indexed_component_connection_alias(
                dae,
                name,
                runtime_defined_discrete_targets,
            )
        }))
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
    let names = [lhs_ref.var_name(), rhs_ref.var_name()];
    names
        .iter()
        .all(|name| live.iter().any(|live_name| live_name == *name))
        && connection_alias_refs_are_supported_continuous_scalars(
            dae,
            eq_rhs,
            runtime_defined_discrete_targets,
        )
}

fn can_eliminate_scalar_connection_alias_var(
    dae: &Dae,
    var_name: &VarName,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    rumoca_core::strip_trailing_subscript_suffix(var_name.as_str()).is_none()
        && !unknown_is_fixed(dae, var_name)
        && dae.variables.algebraics.contains_key(var_name)
        && !runtime_defined_discrete_targets.contains(var_name.as_str())
        && !runtime_partition_or_event_refs_var(dae, var_name)
}

fn independently_scalar_indexed_component_field(dae: &Dae, var_name: &VarName) -> bool {
    dae.variables
        .algebraics
        .get(var_name)
        .is_some_and(|variable| variable.dims.is_empty())
        && rumoca_core::strip_trailing_subscript_suffix(var_name.as_str()).is_none()
        && super::DaeVariableScope::new(dae).is_indexed_component_variable(var_name)
}

fn can_participate_in_indexed_component_connection_alias(
    dae: &Dae,
    var_name: &VarName,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    dae.variables
        .algebraics
        .get(var_name)
        .or_else(|| dae.variables.outputs.get(var_name))
        .is_some_and(|variable| variable.dims.is_empty())
        && !runtime_defined_discrete_targets.contains(var_name.as_str())
        && !runtime_partition_or_event_refs_var(dae, var_name)
}
