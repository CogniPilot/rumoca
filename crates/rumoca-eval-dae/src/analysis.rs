//! DAE-level analysis helpers.
//!
//! This module intentionally owns behavioral analysis over DAE IR so
//! `rumoca-ir-dae` can remain schema-focused.

use std::collections::{HashSet, VecDeque};

use rumoca_ir_dae as dae;

/// Get total number of variables in the DAE variable partitions.
pub fn num_variables(dae_model: &dae::Dae) -> usize {
    dae_model.states.len()
        + dae_model.algebraics.len()
        + dae_model.inputs.len()
        + dae_model.outputs.len()
        + dae_model.parameters.len()
        + dae_model.constants.len()
        + dae_model.discrete_reals.len()
        + dae_model.discrete_valued.len()
}

/// Compute scalar sizes for runtime partitions `(p, t, x, y, z, m)`.
pub fn runtime_partition_scalar_counts(dae_model: &dae::Dae) -> dae::RuntimePartitionScalarCounts {
    dae::RuntimePartitionScalarCounts {
        p: dae_model
            .parameters
            .values()
            .map(|v| v.size())
            .sum::<usize>()
            + dae_model
                .constants
                .values()
                .map(|v| v.size())
                .sum::<usize>(),
        t: 1,
        x: dae_model.states.values().map(|v| v.size()).sum(),
        y: dae_model
            .algebraics
            .values()
            .chain(dae_model.outputs.values())
            .map(|v| v.size())
            .sum(),
        z: dae_model.discrete_reals.values().map(|v| v.size()).sum(),
        m: dae_model.discrete_valued.values().map(|v| v.size()).sum(),
    }
}

/// Get total number of continuous equations (`f_x`).
pub fn num_equations(dae_model: &dae::Dae) -> usize {
    dae_model.f_x.len()
}

/// Check if the system is balanced (equations match unknowns).
pub fn is_balanced(dae_model: &dae::Dae) -> bool {
    balance(dae_model) == 0
}

/// Get the balance: equations - unknowns.
///
/// Positive means over-determined, negative means under-determined.
pub fn balance(dae_model: &dae::Dae) -> i64 {
    // Count scalar unknowns (arrays expand to size() elements)
    let state_unknowns: usize = dae_model.states.values().map(|v| v.size()).sum();
    let alg_unknowns: usize = dae_model.algebraics.values().map(|v| v.size()).sum();
    let output_unknowns: usize = dae_model.outputs.values().map(|v| v.size()).sum();
    let unknowns = (state_unknowns + alg_unknowns + output_unknowns) as i64;

    // f_x: unified continuous equations (MLS B.1a).
    // Only equations that constrain at least one continuous unknown belong
    // to local continuous balance accounting.
    let f_x_scalar = count_f_x_scalars_with_continuous_unknowns(dae_model);
    let algorithm_outputs = 0usize;
    let when_eq_scalar = 0usize;

    // Per MLS §4.7: interface flow variables count as equations
    // Per MLS §4.8/§9.4: overconstrained correction
    //
    // OC interface correction is applied only to close an existing deficit
    // (oc_needed), so it cannot over-correct. Break-edge correction is then
    // applied at the end only when the system is over-determined.
    let brk = dae_model.oc_break_edge_scalar_count as i64;
    let available_oc_interface = dae_model.overconstrained_interface_count.max(0);
    let base_without_iflow = (f_x_scalar + algorithm_outputs + when_eq_scalar) as i64;
    // Interface-flow equations should only compensate a remaining local deficit.
    // This prevents double-counting when explicit unconnected-flow equations
    // already close top-level connector flows in standalone models.
    let iflow_needed = (unknowns - base_without_iflow).max(0);
    let effective_iflow = (dae_model.interface_flow_count as i64).min(iflow_needed);
    let base_equations = base_without_iflow + effective_iflow;
    // OC interface correction must never over-correct a model that is already
    // balanced (or over-determined) before OC terms are applied.
    let oc_needed = (unknowns - base_equations).max(0);
    let effective_oc_interface = available_oc_interface.min(oc_needed);

    let raw_equations = base_equations + effective_oc_interface;
    let raw_balance = raw_equations - unknowns;

    // Per MLS §9.4: subtract break edge excess (cycles in OC graph).
    // Cap the correction so it only reduces positive balance toward zero.
    let effective_brk = brk.min(raw_balance.max(0));
    raw_balance - effective_brk
}

/// Return detailed breakdown of the balance calculation components.
pub fn balance_detail(dae_model: &dae::Dae) -> dae::BalanceDetail {
    let state_unknowns: usize = dae_model.states.values().map(|v| v.size()).sum();
    let alg_unknowns: usize = dae_model.algebraics.values().map(|v| v.size()).sum();
    let output_unknowns: usize = dae_model.outputs.values().map(|v| v.size()).sum();
    let algorithm_outputs = 0usize;
    let when_eq_scalar = 0usize;
    let f_x_scalar = count_f_x_scalars_with_continuous_unknowns(dae_model);
    dae::BalanceDetail {
        state_unknowns,
        alg_unknowns,
        output_unknowns,
        f_x_scalar,
        algorithm_outputs,
        when_eq_scalar,
        interface_flow_count: dae_model.interface_flow_count,
        overconstrained_interface_count: dae_model.overconstrained_interface_count,
        oc_break_edge_scalar_count: dae_model.oc_break_edge_scalar_count,
    }
}

/// Names of unknowns defined at runtime by event/clock evaluation.
///
/// Includes direct targets and expanded record fields.
pub fn runtime_defined_unknown_names(dae_model: &dae::Dae) -> HashSet<String> {
    runtime_defined_unknown_names_impl(dae_model, true)
}

/// Names of continuous unknowns defined at runtime by event/clock evaluation.
///
/// Includes direct targets and expanded record fields in `algebraics`/`outputs`.
pub fn runtime_defined_continuous_unknown_names(dae_model: &dae::Dae) -> HashSet<String> {
    runtime_defined_unknown_names_impl(dae_model, false)
}

fn runtime_defined_unknown_names_impl(
    dae_model: &dae::Dae,
    include_discrete: bool,
) -> HashSet<String> {
    let mut defined = HashSet::new();

    // Discrete partitions (f_z/f_m/f_c + relation) can reference unknowns
    // that must remain available at runtime for event and clocked evaluation.
    for eq in dae_model.f_z.iter().chain(dae_model.f_m.iter()) {
        if let Some(lhs) = eq.lhs.as_ref() {
            extend_runtime_defined_target(dae_model, &mut defined, lhs, include_discrete);
        }
        for target in runtime_assignment_target_names(&eq.rhs) {
            extend_runtime_defined_target(dae_model, &mut defined, target, include_discrete);
        }
    }

    for expr in dae_model
        .f_z
        .iter()
        .map(|eq| &eq.rhs)
        .chain(dae_model.f_m.iter().map(|eq| &eq.rhs))
        .chain(dae_model.f_c.iter().map(|eq| &eq.rhs))
        .chain(dae_model.relation.iter())
    {
        extend_runtime_defined_refs_from_expr(dae_model, &mut defined, expr, include_discrete);
    }

    // Continuous equations that use event/clock operators define values that
    // are produced by runtime semantics (MLS Appendix B pre()/sample()/clocked
    // evaluation). Keep their assignment targets available at runtime.
    for eq in &dae_model.f_x {
        let Some(target) = runtime_assignment_target_name(&eq.rhs) else {
            continue;
        };
        let Some(solution) = runtime_assignment_solution_expr(&eq.rhs) else {
            continue;
        };
        if expression_contains_clocked_or_event_operators(solution) {
            extend_runtime_defined_target(dae_model, &mut defined, target, include_discrete);
        }
    }

    defined
}

fn extend_runtime_defined_refs_from_expr(
    dae_model: &dae::Dae,
    defined: &mut HashSet<String>,
    expr: &dae::Expression,
    include_discrete: bool,
) {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    for name in refs {
        extend_runtime_defined_target(dae_model, defined, &name, include_discrete);
    }
}

fn extend_runtime_defined_target(
    dae_model: &dae::Dae,
    defined: &mut HashSet<String>,
    target: &dae::VarName,
    include_discrete: bool,
) {
    if !include_discrete
        && (dae_model.discrete_reals.contains_key(target)
            || dae_model.discrete_valued.contains_key(target))
    {
        return;
    }

    let raw_target = target.as_str();
    let mut candidates = VecDeque::from([raw_target.to_string()]);
    if let Some(base) = dae::component_base_name(raw_target)
        && base != raw_target
    {
        candidates.push_back(base);
    }

    while let Some(candidate) = candidates.pop_front() {
        let prefix = format!("{candidate}.");
        if include_discrete {
            insert_matching_runtime_targets(
                defined,
                &candidate,
                &prefix,
                dae_model
                    .states
                    .keys()
                    .chain(dae_model.algebraics.keys())
                    .chain(dae_model.outputs.keys())
                    .chain(dae_model.discrete_reals.keys())
                    .chain(dae_model.discrete_valued.keys()),
            );
        } else {
            insert_matching_runtime_targets(
                defined,
                &candidate,
                &prefix,
                dae_model.algebraics.keys().chain(dae_model.outputs.keys()),
            );
        }
    }
}

fn count_f_x_scalars_with_continuous_unknowns(dae_model: &dae::Dae) -> usize {
    let continuous_unknowns = collect_continuous_unknown_names(dae_model);
    let input_names = collect_input_names(dae_model);
    let component_defined_targets = collect_component_defined_targets_for_balance(dae_model);
    dae_model
        .f_x
        .iter()
        .filter(|eq| {
            equation_counts_for_balance(
                dae_model,
                eq,
                &continuous_unknowns,
                &input_names,
                &component_defined_targets,
            )
        })
        .map(|eq| eq.scalar_count)
        .sum()
}

fn equation_counts_for_balance(
    dae_model: &dae::Dae,
    eq: &dae::Equation,
    continuous_unknowns: &HashSet<dae::VarName>,
    input_names: &HashSet<dae::VarName>,
    component_defined_targets: &HashSet<dae::VarName>,
) -> bool {
    if is_connection_origin(eq.origin.as_str())
        && is_redundant_connection_alias(
            dae_model,
            eq,
            continuous_unknowns,
            component_defined_targets,
        )
    {
        return false;
    }
    if equation_references_continuous_unknown(eq, continuous_unknowns) {
        return true;
    }
    // Connection aliases that do not constrain any continuous unknown should
    // not contribute to local continuous balance.
    if is_connection_origin(eq.origin.as_str()) {
        return false;
    }
    // Binding equations for internal promoted inputs/discrete partitions can
    // be input-only aliases and should not inflate continuous balance.
    if eq.origin.starts_with("binding equation for") {
        return false;
    }
    // Preserve explicit user equations constraining interface inputs.
    equation_references_input(eq, input_names)
}

fn is_redundant_connection_alias(
    _dae_model: &dae::Dae,
    eq: &dae::Equation,
    continuous_unknowns: &HashSet<dae::VarName>,
    component_defined_targets: &HashSet<dae::VarName>,
) -> bool {
    let names = runtime_assignment_target_names(&eq.rhs);
    if names.len() != 2 {
        return false;
    }
    let lhs = names[0];
    let rhs = names[1];

    let lhs_component_defined = name_matches_set(lhs, component_defined_targets);
    let rhs_component_defined = name_matches_set(rhs, component_defined_targets);
    let lhs_is_continuous_unknown = name_matches_set(lhs, continuous_unknowns);
    let rhs_is_continuous_unknown = name_matches_set(rhs, continuous_unknowns);

    (lhs_component_defined && !rhs_is_continuous_unknown)
        || (rhs_component_defined && !lhs_is_continuous_unknown)
}

fn collect_component_defined_targets_for_balance(dae_model: &dae::Dae) -> HashSet<dae::VarName> {
    let mut targets = HashSet::new();
    for eq in &dae_model.f_x {
        if is_connection_origin(eq.origin.as_str()) {
            continue;
        }
        if let Some(target) = runtime_assignment_target_name(&eq.rhs) {
            targets.insert(target.clone());
        }
    }
    targets
}

fn collect_continuous_unknown_names(dae_model: &dae::Dae) -> HashSet<dae::VarName> {
    dae_model
        .states
        .keys()
        .chain(dae_model.algebraics.keys())
        .chain(dae_model.outputs.keys())
        .cloned()
        .collect()
}

fn collect_input_names(dae_model: &dae::Dae) -> HashSet<dae::VarName> {
    dae_model.inputs.keys().cloned().collect()
}

fn equation_references_continuous_unknown(
    eq: &dae::Equation,
    continuous_unknowns: &HashSet<dae::VarName>,
) -> bool {
    if eq
        .lhs
        .as_ref()
        .is_some_and(|name| name_matches_set(name, continuous_unknowns))
    {
        return true;
    }

    let mut refs = HashSet::new();
    eq.rhs.collect_var_refs(&mut refs);
    refs.into_iter()
        .any(|name| name_matches_set(&name, continuous_unknowns))
}

fn equation_references_input(eq: &dae::Equation, input_names: &HashSet<dae::VarName>) -> bool {
    if eq
        .lhs
        .as_ref()
        .is_some_and(|name| name_matches_set(name, input_names))
    {
        return true;
    }

    let mut refs = HashSet::new();
    eq.rhs.collect_var_refs(&mut refs);
    refs.into_iter()
        .any(|name| name_matches_set(&name, input_names))
}

fn name_matches_set(name: &dae::VarName, names: &HashSet<dae::VarName>) -> bool {
    if names.contains(name) {
        return true;
    }
    if let Some(base_name) = dae::component_base_name(name.as_str())
        && base_name != name.as_str()
    {
        let base = dae::VarName::new(base_name.clone());
        if names.contains(&base) {
            return true;
        }
        let base_prefix = format!("{base_name}.");
        if names
            .iter()
            .any(|candidate| candidate.as_str().starts_with(&base_prefix))
        {
            return true;
        }
    }
    let prefix = format!("{}.", name.as_str());
    names
        .iter()
        .any(|candidate| candidate.as_str().starts_with(&prefix))
}

fn is_connection_origin(origin: &str) -> bool {
    origin.starts_with("connect(") || origin.starts_with("connection equation:")
}

fn insert_matching_runtime_targets<'a, I>(
    defined: &mut HashSet<String>,
    candidate: &str,
    prefix: &str,
    names: I,
) where
    I: Iterator<Item = &'a dae::VarName>,
{
    for text in names
        .map(dae::VarName::as_str)
        .filter(|text| *text == candidate || text.starts_with(prefix))
    {
        defined.insert(text.to_string());
    }
}

fn runtime_assignment_target_names(expr: &dae::Expression) -> Vec<&dae::VarName> {
    let dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(_),
        lhs,
        rhs,
    } = expr
    else {
        return Vec::new();
    };

    let mut names = Vec::with_capacity(2);
    if let dae::Expression::VarRef { name, .. } = lhs.as_ref() {
        names.push(name);
    }
    if let dae::Expression::VarRef { name, .. } = rhs.as_ref() {
        names.push(name);
    }
    names
}

fn runtime_assignment_target_name(expr: &dae::Expression) -> Option<&dae::VarName> {
    runtime_assignment_target_names(expr).into_iter().next()
}

fn runtime_assignment_solution_expr(expr: &dae::Expression) -> Option<&dae::Expression> {
    let dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(_),
        lhs,
        rhs,
    } = expr
    else {
        return None;
    };

    if matches!(lhs.as_ref(), dae::Expression::VarRef { .. }) {
        return Some(rhs.as_ref());
    }
    if matches!(rhs.as_ref(), dae::Expression::VarRef { .. }) {
        return Some(lhs.as_ref());
    }
    None
}

fn expression_contains_clocked_or_event_operators(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            if matches!(
                function,
                dae::BuiltinFunction::Pre
                    | dae::BuiltinFunction::Sample
                    | dae::BuiltinFunction::Edge
                    | dae::BuiltinFunction::Change
            ) {
                return true;
            }
            args.iter()
                .any(expression_contains_clocked_or_event_operators)
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short_name = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            if matches!(
                short_name,
                "previous"
                    | "Clock"
                    | "hold"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "noClock"
                    | "firstTick"
                    | "interval"
            ) {
                return true;
            }
            args.iter()
                .any(expression_contains_clocked_or_event_operators)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expression_contains_clocked_or_event_operators(lhs)
                || expression_contains_clocked_or_event_operators(rhs)
        }
        dae::Expression::Unary { rhs, .. } => expression_contains_clocked_or_event_operators(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expression_contains_clocked_or_event_operators(cond)
                    || expression_contains_clocked_or_event_operators(value)
            }) || expression_contains_clocked_or_event_operators(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(expression_contains_clocked_or_event_operators),
        dae::Expression::Range { start, step, end } => {
            expression_contains_clocked_or_event_operators(start)
                || step
                    .as_deref()
                    .is_some_and(expression_contains_clocked_or_event_operators)
                || expression_contains_clocked_or_event_operators(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expression_contains_clocked_or_event_operators(expr)
                || indices
                    .iter()
                    .any(|idx| expression_contains_clocked_or_event_operators(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(expression_contains_clocked_or_event_operators)
        }
        dae::Expression::Index { base, subscripts } => {
            expression_contains_clocked_or_event_operators(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => {
                        expression_contains_clocked_or_event_operators(expr)
                    }
                    dae::Subscript::Index(_) | dae::Subscript::Colon => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => {
            expression_contains_clocked_or_event_operators(base)
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn scalar_eq(count: usize) -> dae::Equation {
        dae::Equation {
            lhs: Some(dae::VarName::new("x")),
            rhs: dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("x"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Integer(0))),
            },
            span: Span::DUMMY,
            origin: "test".to_string(),
            scalar_count: count,
        }
    }

    fn dae_with_unknown_scalars(unknown_scalars: i64) -> dae::Dae {
        let mut dae = dae::Dae::default();
        dae.algebraics.insert(
            dae::VarName::new("x"),
            dae::Variable {
                name: dae::VarName::new("x"),
                dims: vec![unknown_scalars],
                ..Default::default()
            },
        );
        dae
    }

    #[test]
    fn test_runtime_partition_scalar_counts() {
        let mut dae = dae::Dae::default();
        dae.parameters.insert(
            dae::VarName::new("p"),
            dae::Variable {
                name: dae::VarName::new("p"),
                ..Default::default()
            },
        );
        dae.constants.insert(
            dae::VarName::new("c"),
            dae::Variable {
                name: dae::VarName::new("c"),
                ..Default::default()
            },
        );
        dae.states.insert(
            dae::VarName::new("x"),
            dae::Variable {
                name: dae::VarName::new("x"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae.algebraics.insert(
            dae::VarName::new("y"),
            dae::Variable {
                name: dae::VarName::new("y"),
                ..Default::default()
            },
        );
        dae.outputs.insert(
            dae::VarName::new("w"),
            dae::Variable {
                name: dae::VarName::new("w"),
                ..Default::default()
            },
        );
        dae.discrete_reals.insert(
            dae::VarName::new("z"),
            dae::Variable {
                name: dae::VarName::new("z"),
                dims: vec![3],
                ..Default::default()
            },
        );
        dae.discrete_valued.insert(
            dae::VarName::new("m"),
            dae::Variable {
                name: dae::VarName::new("m"),
                ..Default::default()
            },
        );

        let counts = runtime_partition_scalar_counts(&dae);
        assert_eq!(counts.p, 2);
        assert_eq!(counts.t, 1);
        assert_eq!(counts.x, 2);
        assert_eq!(counts.y, 2);
        assert_eq!(counts.z, 3);
        assert_eq!(counts.m, 1);
    }

    #[test]
    fn test_balance_clamps_overconstrained_interface_to_deficit() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.f_x.push(scalar_eq(4));
        dae.overconstrained_interface_count = 9;

        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_uses_only_needed_overconstrained_interface() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.f_x.push(scalar_eq(3));
        dae.overconstrained_interface_count = 9;

        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_applies_oc_interface_even_with_break_edges() {
        let mut dae = dae_with_unknown_scalars(10);
        dae.f_x.push(scalar_eq(1));
        dae.overconstrained_interface_count = 9;
        dae.oc_break_edge_scalar_count = 12;

        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_clamps_interface_flow_to_remaining_deficit() {
        let mut dae = dae_with_unknown_scalars(4);
        dae.f_x.push(scalar_eq(4));
        dae.interface_flow_count = 3;

        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_balance_uses_interface_flow_to_close_deficit_only() {
        let mut dae = dae_with_unknown_scalars(5);
        dae.f_x.push(scalar_eq(3));
        dae.interface_flow_count = 9;

        assert_eq!(balance(&dae), 0);
    }

    #[test]
    fn test_runtime_defined_unknown_names_include_discrete_targets() {
        let mut dae = dae::Dae::default();
        dae.algebraics.insert(
            dae::VarName::new("a"),
            dae::Variable {
                name: dae::VarName::new("a"),
                ..Default::default()
            },
        );
        dae.discrete_valued.insert(
            dae::VarName::new("enable"),
            dae::Variable {
                name: dae::VarName::new("enable"),
                ..Default::default()
            },
        );
        dae.f_m.push(dae::Equation::explicit(
            dae::VarName::new("a"),
            dae::Expression::Literal(dae::Literal::Real(1.0)),
            Span::DUMMY,
            "runtime-defined-a",
        ));
        dae.f_m.push(dae::Equation::explicit(
            dae::VarName::new("enable"),
            dae::Expression::Literal(dae::Literal::Boolean(true)),
            Span::DUMMY,
            "runtime-defined-enable",
        ));

        let all = runtime_defined_unknown_names(&dae);
        assert!(all.contains("a"));
        assert!(all.contains("enable"));
    }

    #[test]
    fn test_runtime_defined_continuous_unknown_names_exclude_discrete_targets() {
        let mut dae = dae::Dae::default();
        dae.algebraics.insert(
            dae::VarName::new("a"),
            dae::Variable {
                name: dae::VarName::new("a"),
                ..Default::default()
            },
        );
        dae.discrete_valued.insert(
            dae::VarName::new("enable"),
            dae::Variable {
                name: dae::VarName::new("enable"),
                ..Default::default()
            },
        );
        dae.f_m.push(dae::Equation::explicit(
            dae::VarName::new("a"),
            dae::Expression::Literal(dae::Literal::Real(1.0)),
            Span::DUMMY,
            "runtime-defined-a",
        ));
        dae.f_m.push(dae::Equation::explicit(
            dae::VarName::new("enable"),
            dae::Expression::Literal(dae::Literal::Boolean(true)),
            Span::DUMMY,
            "runtime-defined-enable",
        ));

        let continuous = runtime_defined_continuous_unknown_names(&dae);
        assert!(continuous.contains("a"));
        assert!(!continuous.contains("enable"));
    }

    #[test]
    fn test_runtime_defined_continuous_unknown_names_include_fx_pre_assignment_targets() {
        let mut dae = dae::Dae::default();
        dae.algebraics.insert(
            dae::VarName::new("gate.y"),
            dae::Variable {
                name: dae::VarName::new("gate.y"),
                ..Default::default()
            },
        );
        dae.algebraics.insert(
            dae::VarName::new("gate.aux"),
            dae::Variable {
                name: dae::VarName::new("gate.aux"),
                ..Default::default()
            },
        );
        dae.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("gate.y"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("gate.aux"),
                        subscripts: vec![],
                    }],
                }),
            },
            Span::DUMMY,
            "equation from gate",
        ));

        let continuous = runtime_defined_continuous_unknown_names(&dae);
        assert!(
            continuous.contains("gate.y"),
            "f_x assignment targets using pre() must remain runtime-defined"
        );
    }
}
