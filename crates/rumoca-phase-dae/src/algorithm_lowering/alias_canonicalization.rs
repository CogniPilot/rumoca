use super::*;

pub(super) fn is_connection_equation_origin(origin: &str) -> bool {
    origin.contains("connection equation:")
}

fn is_reroutable_alias_equation_origin(origin: &str) -> bool {
    is_connection_equation_origin(origin)
}

fn is_stategraph_port_alias_target(name: &VarName) -> bool {
    name.as_str().contains(".outerStatePort.")
        || name.as_str().contains(".subgraphStatePort.")
        || name.as_str().contains(".stateGraphRoot.")
}

pub(super) fn is_binding_equation_origin(origin: &str) -> bool {
    origin.starts_with("binding equation for ")
}

pub(super) fn reroutable_alias_rhs_target(equation: &rumoca_ir_dae::Equation) -> Option<VarName> {
    let rhs_expr = dae_to_flat_expression(&equation.rhs);
    let rhs_target = discrete_assignment_rhs_var_name(&rhs_expr)?;
    if is_reroutable_alias_equation_origin(&equation.origin) {
        return Some(rhs_target);
    }
    if equation.origin.starts_with("explicit equation from ")
        && equation
            .lhs
            .as_ref()
            .map(|lhs| dae_to_flat_var_name(lhs.var_name()))
            .is_some_and(|lhs| {
                is_stategraph_port_alias_target(&lhs)
                    || is_stategraph_port_alias_target(&rhs_target)
            })
    {
        return Some(rhs_target);
    }
    None
}

pub(super) fn defined_target_reroute_alias_rhs_target(
    equation: &rumoca_ir_dae::Equation,
) -> Option<VarName> {
    if !is_connection_equation_origin(&equation.origin)
        && !equation.origin.starts_with("explicit equation from ")
    {
        return None;
    }
    let rhs_expr = dae_to_flat_expression(&equation.rhs);
    discrete_assignment_rhs_var_name(&rhs_expr)
}
