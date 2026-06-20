use std::collections::{HashMap, HashSet};

use indexmap::IndexSet;

use super::{
    Dae, Expression, VarName, equation_defining_expr_for_unknown,
    expr_refs_only_parameters_constants_or_time, is_connection_equation_origin,
    try_extract_state_alias_pair,
};

fn connection_alias_adjacency(dae: &Dae) -> HashMap<String, IndexSet<String>> {
    let mut adjacency: HashMap<String, IndexSet<String>> = HashMap::new();
    for eq in &dae.continuous.equations {
        if !is_connection_equation_origin(&eq.origin) {
            continue;
        }
        let Some((lhs, rhs)) = try_extract_state_alias_pair(&eq.rhs) else {
            continue;
        };
        adjacency
            .entry(lhs.as_str().to_string())
            .or_default()
            .insert(rhs.as_str().to_string());
        adjacency
            .entry(rhs.as_str().to_string())
            .or_default()
            .insert(lhs.as_str().to_string());
    }
    adjacency
}

fn collect_connection_alias_component(
    adjacency: &HashMap<String, IndexSet<String>>,
    start: &VarName,
) -> IndexSet<String> {
    let mut component = IndexSet::from([start.as_str().to_string()]);
    let mut stack = vec![start.as_str().to_string()];
    while let Some(name) = stack.pop() {
        for neighbor in adjacency.get(&name).into_iter().flatten() {
            if component.insert(neighbor.clone()) {
                stack.push(neighbor.clone());
            }
        }
    }
    component
}

pub(super) fn connection_component_fixed_defining_expr(
    dae: &Dae,
    state_name: &VarName,
    state_name_set: &HashSet<String>,
) -> Option<Expression> {
    let adjacency = connection_alias_adjacency(dae);
    let component = collect_connection_alias_component(&adjacency, state_name);
    if component.len() <= 1 {
        return None;
    }
    if component
        .iter()
        .any(|name| name != state_name.as_str() && state_name_set.contains(name.as_str()))
    {
        return None;
    }

    let mut terminal_exprs = Vec::new();
    for name in component.iter().map(|name| VarName::new(name.clone())) {
        if let Some(var) = dae
            .variables
            .parameters
            .get(&name)
            .or_else(|| dae.variables.constants.get(&name))
        {
            terminal_exprs.push(Expression::VarRef {
                name: rumoca_core::Reference::from_var_name(name),
                subscripts: vec![],
                span: var.source_span,
            });
            continue;
        }
        terminal_exprs.extend(
            dae.continuous
                .equations
                .iter()
                .filter(|eq| !is_connection_equation_origin(&eq.origin))
                .filter_map(|eq| equation_defining_expr_for_unknown(eq, &name))
                .filter(|expr| expr_refs_only_parameters_constants_or_time(dae, expr)),
        );
    }

    match terminal_exprs.as_slice() {
        [expr] => Some(expr.clone()),
        [] | [_, _, ..] => None,
    }
}
