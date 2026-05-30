use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;

use super::{SourceMap, canonical_var_ref_key};

pub(super) fn is_non_static_event_clock_constructor(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> bool {
    let rumoca_core::Expression::FunctionCall { name, args, .. } = expr else {
        return false;
    };
    let short = name.last_segment();
    if short != "Clock" || args.is_empty() {
        return false;
    }
    expression_is_event_clock_condition(
        &args[0],
        Some(dae_model),
        constants,
        sources,
        24,
        &mut HashSet::new(),
    )
}

pub(super) fn expression_resolves_to_boolean_condition(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> bool {
    expression_is_event_clock_condition(expr, None, constants, sources, 24, &mut HashSet::new())
}

pub(super) fn event_clock_variable_names(
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> HashSet<String> {
    let mut roots = HashSet::new();
    for eq in dae_model
        .discrete
        .real_updates
        .iter()
        .chain(dae_model.discrete.valued_updates.iter())
    {
        collect_event_clock_assignment_targets(eq, constants, sources, &mut roots);
    }
    if roots.is_empty() {
        return roots;
    }

    let aliases = build_variable_alias_graph(dae_model, constants);
    let mut seen = HashSet::new();
    let mut pending = roots.into_iter().collect::<Vec<_>>();
    while let Some(name) = pending.pop() {
        if !seen.insert(name.clone()) {
            continue;
        }
        if let Some(neighbors) = aliases.get(&name) {
            pending.extend(neighbors.iter().cloned());
        }
    }
    seen
}

fn collect_event_clock_assignment_targets(
    eq: &dae::Equation,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    out: &mut HashSet<String>,
) {
    if let Some(lhs) = eq.lhs.as_ref()
        && expression_is_event_clock_constructor(&eq.rhs, constants, sources)
    {
        out.insert(lhs.as_str().to_string());
        return;
    }
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &eq.rhs
    else {
        return;
    };
    if expression_is_event_clock_constructor(rhs, constants, sources)
        && let Some(key) = var_ref_key(lhs, constants)
    {
        out.insert(key);
    }
    if expression_is_event_clock_constructor(lhs, constants, sources)
        && let Some(key) = var_ref_key(rhs, constants)
    {
        out.insert(key);
    }
}

fn expression_is_event_clock_constructor(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> bool {
    let rumoca_core::Expression::FunctionCall { name, args, .. } = expr else {
        return false;
    };
    let short = name.last_segment();
    short == "Clock"
        && args
            .first()
            .is_some_and(|arg| expression_resolves_to_boolean_condition(arg, constants, sources))
}

fn build_variable_alias_graph(
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
) -> HashMap<String, Vec<String>> {
    let mut graph = HashMap::new();
    for eq in dae_model
        .continuous
        .equations
        .iter()
        .chain(dae_model.discrete.real_updates.iter())
        .chain(dae_model.discrete.valued_updates.iter())
    {
        if let Some((lhs, rhs)) = equation_var_alias(eq, constants) {
            insert_alias_edge(&mut graph, lhs.as_str(), rhs.as_str());
            insert_alias_edge(&mut graph, rhs.as_str(), lhs.as_str());
        }
    }
    graph
}

fn equation_var_alias(
    eq: &dae::Equation,
    constants: &HashMap<String, f64>,
) -> Option<(String, String)> {
    if let Some(lhs) = eq.lhs.as_ref()
        && let Some(rhs) = var_ref_key(&eq.rhs, constants)
    {
        return Some((lhs.as_str().to_string(), rhs));
    }
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &eq.rhs
    else {
        return None;
    };
    Some((var_ref_key(lhs, constants)?, var_ref_key(rhs, constants)?))
}

fn var_ref_key(expr: &rumoca_core::Expression, constants: &HashMap<String, f64>) -> Option<String> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    canonical_var_ref_key(name, subscripts, constants)
}

fn insert_alias_edge(graph: &mut HashMap<String, Vec<String>>, lhs: &str, rhs: &str) {
    let edges = graph.entry(lhs.to_string()).or_default();
    if !edges.iter().any(|existing| existing == rhs) {
        edges.push(rhs.to_string());
    }
}

fn expression_is_event_clock_condition(
    expr: &rumoca_core::Expression,
    dae_model: Option<&dae::Dae>,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> bool {
    if remaining_depth == 0 {
        return false;
    }
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(_),
            ..
        } => true,
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => expression_var_ref_is_event_clock_condition(
            name,
            subscripts,
            dae_model,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            matches!(op, rumoca_core::OpUnary::Not)
                || expression_is_event_clock_condition(
                    rhs,
                    dae_model,
                    constants,
                    sources,
                    remaining_depth.saturating_sub(1),
                    visiting,
                )
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            binary_operator_is_boolean(op)
                || expression_is_event_clock_condition(
                    lhs,
                    dae_model,
                    constants,
                    sources,
                    remaining_depth.saturating_sub(1),
                    visiting,
                )
                || expression_is_event_clock_condition(
                    rhs,
                    dae_model,
                    constants,
                    sources,
                    remaining_depth.saturating_sub(1),
                    visiting,
                )
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            matches!(function, rumoca_core::BuiltinFunction::Pre)
                && args.first().is_some_and(|arg| {
                    expression_is_event_clock_condition(
                        arg,
                        dae_model,
                        constants,
                        sources,
                        remaining_depth.saturating_sub(1),
                        visiting,
                    )
                })
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(cond, value)| {
                expression_is_event_clock_condition(
                    cond,
                    dae_model,
                    constants,
                    sources,
                    remaining_depth.saturating_sub(1),
                    visiting,
                ) || expression_is_event_clock_condition(
                    value,
                    dae_model,
                    constants,
                    sources,
                    remaining_depth.saturating_sub(1),
                    visiting,
                )
            }) || expression_is_event_clock_condition(
                else_branch,
                dae_model,
                constants,
                sources,
                remaining_depth.saturating_sub(1),
                visiting,
            )
        }
        _ => false,
    }
}

fn binary_operator_is_boolean(op: &rumoca_core::OpBinary) -> bool {
    matches!(
        op,
        rumoca_core::OpBinary::And
            | rumoca_core::OpBinary::Or
            | rumoca_core::OpBinary::Lt
            | rumoca_core::OpBinary::Le
            | rumoca_core::OpBinary::Gt
            | rumoca_core::OpBinary::Ge
            | rumoca_core::OpBinary::Eq
            | rumoca_core::OpBinary::Neq
    )
}

fn expression_var_ref_is_event_clock_condition(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    dae_model: Option<&dae::Dae>,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> bool {
    if !subscripts.is_empty() {
        return false;
    }
    if dae_model.is_some_and(|model| {
        model
            .variables
            .discrete_valued
            .contains_key(name.var_name())
            || model
                .symbols
                .enum_literal_ordinals
                .contains_key(name.as_str())
    }) {
        return true;
    }
    let Some(key) = canonical_var_ref_key(name, subscripts, constants) else {
        return false;
    };
    if !visiting.insert(key.clone()) {
        return false;
    }
    let result = sources.get(&key).is_some_and(|source_exprs| {
        source_exprs.iter().any(|source| {
            expression_is_event_clock_condition(
                source,
                dae_model,
                constants,
                sources,
                remaining_depth.saturating_sub(1),
                visiting,
            )
        })
    });
    visiting.remove(&key);
    result
}
