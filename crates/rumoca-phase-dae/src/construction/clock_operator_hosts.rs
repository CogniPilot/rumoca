use super::*;

/// The clock partitions that read the MLS §16.10 `operator` (`firstTick()` or
/// `interval()`), with one occurrence span each.
pub(super) fn clock_operator_hosts(
    flat: &flat::Model,
    analysis: &Analysis,
    operator: BuiltinFunction,
) -> Vec<(ClockPlan, Span)> {
    let mut hosts = Vec::new();
    for (row, equation) in flat.equations.iter().enumerate() {
        if let (Some(span), Some(plan)) = (
            operator_span(&equation.residual, operator),
            analysis.clocked_equation_owners.get(&row),
        ) {
            hosts.push((*plan, span));
        }
    }
    for (chain_index, chain) in flat.when_chains.iter().enumerate() {
        for (branch_index, branch) in chain.branches().enumerate() {
            let Some(span) = branch
                .equations
                .iter()
                .find_map(|equation| when_equation_operator(equation, operator))
            else {
                continue;
            };
            let key = WhenBranchKey {
                chain: chain_index,
                branch: branch_index,
            };
            let named = match &branch.condition {
                Expression::VarRef { name, .. } => flat
                    .variables
                    .get(name.var_name())
                    .and_then(|variable| analysis.clock_plans.get(&variable.instance_id)),
                _ => None,
            };
            if let Some(plan) = analysis.clocked_when_owners.get(&key).or(named) {
                hosts.push((*plan, span));
            }
        }
    }
    hosts
}

fn operator_span(expression: &Expression, operator: BuiltinFunction) -> Option<Span> {
    if let Expression::BuiltinCall { function, span, .. } = expression
        && *function == operator
    {
        return Some(*span);
    }
    expression_children(expression)
        .into_iter()
        .find_map(|child| operator_span(child, operator))
}

fn when_equation_operator(
    equation: &flat::WhenEquation,
    operator: BuiltinFunction,
) -> Option<Span> {
    match equation {
        flat::WhenEquation::Assign { value, .. }
        | flat::WhenEquation::AssignElement { value, .. } => operator_span(value, operator),
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            ..
        } => branches
            .iter()
            .find_map(|(condition, equations)| {
                operator_span(condition, operator).or_else(|| {
                    equations
                        .iter()
                        .find_map(|equation| when_equation_operator(equation, operator))
                })
            })
            .or_else(|| {
                else_branch
                    .iter()
                    .flatten()
                    .find_map(|equation| when_equation_operator(equation, operator))
            }),
        _ => None,
    }
}
