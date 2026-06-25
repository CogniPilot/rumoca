use super::*;

fn log_direct_demotion_scan_summary(
    trace: bool,
    state_count: usize,
    substitutions: &HashMap<String, DirectStateDemotionPlan>,
    counters: &DirectDemotionCounters,
) {
    if !trace {
        return;
    }
    crate::structural_trace!(
        "[sim-trace] direct-assignment-demotion scan: states={} candidates={} accepted={} skip_flow_sum_origin={} skip_unsafe_non_state_alias={} skip_when={} skip_self_der={} skip_der_in_defining_expr={} skip_unsliced_vector_ref={} skip_extra_state_refs={} skip_no_der={} skip_non_state_der={}",
        state_count,
        counters.n_candidates,
        substitutions.len(),
        counters.n_skip_flow_sum_origin,
        counters.n_skip_unsafe_non_state_alias,
        counters.n_skip_when_assigned,
        counters.n_skip_self_der,
        counters.n_skip_der_in_defining_expr,
        counters.n_skip_unsliced_vector_ref,
        counters.n_skip_extra_state_refs,
        counters.n_skip_no_der_expr,
        counters.n_skip_non_state_der
    );
}

pub(super) fn collect_non_state_continuous_unknown_names(dae: &Dae) -> HashSet<String> {
    dae.variables
        .algebraics
        .keys()
        .chain(dae.variables.outputs.keys())
        .map(|name| name.as_str().to_string())
        .collect()
}

pub(super) fn is_connection_equation_origin(origin: &str) -> bool {
    origin.starts_with("connection equation:")
}

pub(super) fn expr_refs_only_parameters_constants_or_time(dae: &Dae, expr: &Expression) -> bool {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().all(|name| {
        name.as_str() == "time"
            || dae.variables.parameters.contains_key(&name)
            || dae.variables.constants.contains_key(&name)
    })
}

pub(super) fn expression_contains_any_der_call(expr: &Expression) -> bool {
    dae::ContainsDerChecker::check(expr)
}

pub(super) fn equation_defining_expr_for_unknown(
    eq: &Equation,
    unknown_name: &VarName,
) -> Option<Expression> {
    if let Some(lhs) = eq.lhs.as_ref()
        && lhs.var_name() == unknown_name
    {
        if expression_contains_any_der_call(&eq.rhs) {
            return None;
        }
        return Some(eq.rhs.clone());
    }
    if let Some((coef, remainder)) = split_linear_target(&eq.rhs, unknown_name, eq.span) {
        let defining_expr = match coef {
            1 => sub_expr(zero_expr(eq.span), remainder, eq.span),
            -1 => remainder,
            _ => return None,
        };
        if expression_contains_any_der_call(&defining_expr) {
            return None;
        }
        return Some(defining_expr);
    }
    None
}

fn unique_non_state_defining_expr_excluding(
    definitions: &DefiningExprIndex,
    unknown_name: &VarName,
    excluded_eq_index: Option<usize>,
) -> Option<Expression> {
    let mut defining_exprs = definitions
        .get(unknown_name.as_str())?
        .iter()
        .filter(|candidate| excluded_eq_index != Some(candidate.equation_index))
        .map(|candidate| candidate.expr.clone());
    let defining_expr = defining_exprs.next()?;
    defining_exprs.next().is_none().then_some(defining_expr)
}

fn expr_depends_on_state_or_unsafe_non_state_alias(
    definitions: &DefiningExprIndex,
    expr: &Expression,
    state_name_set: &HashSet<String>,
    non_state_unknown_names: &HashSet<String>,
    excluded_eq_index: Option<usize>,
    visiting: &mut HashSet<String>,
    alias_safety_cache: &mut AliasSafetyCache,
) -> bool {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().any(|ref_name| {
        if state_name_set.contains(ref_name.as_str()) {
            return true;
        }
        if !non_state_unknown_names.contains(ref_name.as_str()) {
            return false;
        }
        !non_state_alias_closure_is_state_free(
            definitions,
            &ref_name,
            state_name_set,
            non_state_unknown_names,
            excluded_eq_index,
            visiting,
            alias_safety_cache,
        )
    })
}

fn non_state_alias_closure_is_state_free(
    definitions: &DefiningExprIndex,
    unknown_name: &VarName,
    state_name_set: &HashSet<String>,
    non_state_unknown_names: &HashSet<String>,
    excluded_eq_index: Option<usize>,
    visiting: &mut HashSet<String>,
    alias_safety_cache: &mut AliasSafetyCache,
) -> bool {
    let cache_key = (unknown_name.as_str().to_string(), excluded_eq_index);
    if let Some(is_safe) = alias_safety_cache.get(&cache_key) {
        return *is_safe;
    }
    if !visiting.insert(unknown_name.as_str().to_string()) {
        alias_safety_cache.insert(cache_key, false);
        return false;
    }

    let is_safe =
        unique_non_state_defining_expr_excluding(definitions, unknown_name, excluded_eq_index)
            .is_some_and(|defining_expr| {
                // MLS Appendix B / SPEC_0003: variables appearing differentiated remain
                // states. Alias-driven direct demotion is only sound when every
                // referenced non-state unknown resolves through a unique, state-free
                // closure.
                !expr_depends_on_state_or_unsafe_non_state_alias(
                    definitions,
                    &defining_expr,
                    state_name_set,
                    non_state_unknown_names,
                    excluded_eq_index,
                    visiting,
                    alias_safety_cache,
                )
            });

    visiting.remove(unknown_name.as_str());
    alias_safety_cache.insert(cache_key, is_safe);
    is_safe
}

fn defining_expr_references_unsafe_non_state_alias_closure(
    definitions: &DefiningExprIndex,
    defining_expr: &Expression,
    state_name_set: &HashSet<String>,
    non_state_unknown_names: &HashSet<String>,
    excluded_eq_index: usize,
    alias_safety_cache: &mut AliasSafetyCache,
) -> bool {
    let mut visiting = HashSet::new();
    expr_depends_on_state_or_unsafe_non_state_alias(
        definitions,
        defining_expr,
        state_name_set,
        non_state_unknown_names,
        Some(excluded_eq_index),
        &mut visiting,
        alias_safety_cache,
    )
}

fn apply_direct_demotion_plans(
    dae: &mut Dae,
    substitutions: &HashMap<String, DirectStateDemotionPlan>,
) -> usize {
    let mut demoted_this_round = 0usize;
    let mut plans: Vec<&DirectStateDemotionPlan> = substitutions.values().collect();
    plans.sort_by(|a, b| a.state_name.as_str().cmp(b.state_name.as_str()));
    for plan in plans {
        demoted_this_round += apply_direct_demotion_plan(dae, plan);
    }
    demoted_this_round
}

pub(super) fn apply_direct_demotion_plan(dae: &mut Dae, plan: &DirectStateDemotionPlan) -> usize {
    let state_dims = variable_dims_for_direct_demotion(dae, &plan.state_name);
    for eq in &mut dae.continuous.equations {
        eq.rhs = substitute_der_of_state(&eq.rhs, &plan.state_name, &plan.der_expr, &state_dims);
    }
    if let Some(mut var) = dae.variables.states.shift_remove(&plan.state_name) {
        var.fixed = Some(false);
        dae.variables
            .algebraics
            .insert(plan.state_name.clone(), var);
        return 1;
    }
    0
}

fn direct_demotion_plan_for_equation(
    round: &DirectDemotionRound<'_>,
    eq_index: usize,
    eq: &Equation,
    counters: &mut DirectDemotionCounters,
    alias_safety_cache: &mut AliasSafetyCache,
) -> Result<Option<DirectStateDemotionPlan>, StructuralError> {
    let (state_name, defining_expr) = match direct_assignment_candidate(round, eq) {
        Some(candidate) => candidate,
        None => return Ok(None),
    };
    counters.n_candidates += 1;
    if eq.origin.starts_with("flow sum equation:") {
        counters.n_skip_flow_sum_origin += 1;
        return Ok(None);
    }
    log_direct_assignment_candidate(round.trace, counters, round.dae, eq, &state_name);
    if round.when_assigned_states.contains(state_name.as_str()) {
        counters.n_skip_when_assigned += 1;
        return Ok(None);
    }
    if expr_contains_der_of_state_or_indexed(&defining_expr, &state_name) {
        counters.n_skip_self_der += 1;
        return Ok(None);
    }
    if !defining_expr_state_derivatives_are_demotable(round, &state_name, &defining_expr)? {
        counters.n_skip_der_in_defining_expr += 1;
        return Ok(None);
    }
    // `der(state)` links are substituted symbolically on demotion (gated by
    // `state_ders_in_expr_independently_defined` above and validated again in
    // `choose_derivative_replacement`), so mask them before scanning for value
    // dependencies on states or unsafe alias closures.
    let alias_scan_expr = mask_state_der_calls(&defining_expr, &round.state_name_set);
    if defining_expr_references_unsafe_non_state_alias_closure(
        &round.non_state_defining_exprs,
        &alias_scan_expr,
        &round.state_name_set,
        &round.non_state_unknown_names,
        eq_index,
        alias_safety_cache,
    ) {
        counters.n_skip_unsafe_non_state_alias += 1;
        return Ok(None);
    }
    if !direct_assignment_shape_is_demotable(round.dae, &state_name, &defining_expr) {
        // MLS §10.1: array state shape is semantic IR. This path substitutes
        // whole `der(state)` calls only when the defining expression has the
        // same aggregate shape. Scalar states still reject unsliced vector refs.
        counters.n_skip_unsliced_vector_ref += 1;
        return Ok(None);
    }
    if state_non_derivative_reference_row_count(round.dae, &state_name) > 1
        && !expr_refs_only_parameters_constants_or_time(round.dae, &defining_expr)
    {
        counters.n_skip_extra_state_refs += 1;
        return Ok(None);
    }
    let der_map =
        build_relaxed_derivative_map_for_exprs(round.dae, std::slice::from_ref(&defining_expr))?;
    let der_expr = choose_derivative_replacement(
        &defining_expr,
        &round.state_name_set,
        round.dae,
        &der_map,
        counters,
    );
    let Some(der_expr) = der_expr else {
        return Ok(None);
    };
    if expr_contains_der_of_state_or_indexed(&der_expr, &state_name) {
        counters.n_skip_self_der += 1;
        return Ok(None);
    }
    if expr_contains_der_of_non_state(&der_expr, &round.state_name_set) {
        counters.n_skip_non_state_der += 1;
        return Ok(None);
    }
    if round.trace && counters.n_trace_logged_candidates < 16 {
        crate::structural_trace!(
            "[sim-trace] direct-assignment accepted state={} der_expr={}",
            state_name.as_str(),
            truncate_debug(&format!("{:?}", der_expr), 1200)
        );
        counters.n_trace_logged_candidates += 1;
    }
    Ok(Some(DirectStateDemotionPlan {
        state_name,
        der_expr,
    }))
}

fn direct_assignment_candidate(
    round: &DirectDemotionRound<'_>,
    eq: &Equation,
) -> Option<(VarName, Expression)> {
    let (state_name, defining_expr) =
        extract_state_direct_assignment_equation(eq, &round.state_names, &round.state_name_set)?;
    if !is_connection_equation_origin(&eq.origin) {
        return Some((state_name, defining_expr));
    }
    let defining_expr =
        connection_component_fixed_defining_expr(round.dae, &state_name, &round.state_name_set)
            .unwrap_or(defining_expr);
    Some((state_name, defining_expr))
}

fn defining_expr_state_derivatives_are_demotable(
    round: &DirectDemotionRound<'_>,
    state_name: &VarName,
    defining_expr: &Expression,
) -> Result<bool, StructuralError> {
    if derivative_states_in_eq(defining_expr, &round.state_names).is_empty() {
        return Ok(true);
    }
    let der_map =
        build_relaxed_derivative_map_for_exprs(round.dae, std::slice::from_ref(defining_expr))?;
    Ok(state_ders_in_expr_independently_defined(
        defining_expr,
        state_name,
        round,
        &der_map,
    ))
}

fn state_non_derivative_reference_row_count(dae: &Dae, state_name: &VarName) -> usize {
    dae.continuous
        .equations
        .iter()
        .filter(|row| {
            expr_contains_var(&row.rhs, state_name) && !expr_contains_der_of(&row.rhs, state_name)
        })
        .count()
}

/// `der(z)` links inside a defining expression are demotable only when `z`'s
/// own derivative definition is closed-form (not the symbolic `der(z)`
/// fallback) and does not feed back through the candidate state. This admits
/// differentiator chains (`y = der(x)` reading a state with its own ODE row)
/// while keeping kinematic aliases as states: in `v = der(s)` the alias row
/// itself defines `der(s) = v`, so `der(s)`'s value contains the candidate.
fn state_ders_in_expr_independently_defined(
    defining_expr: &Expression,
    candidate: &VarName,
    round: &DirectDemotionRound<'_>,
    der_map: &HashMap<String, Expression>,
) -> bool {
    derivative_states_in_eq(defining_expr, &round.state_names)
        .iter()
        .all(|inner_state| {
            der_map.get(inner_state.as_str()).is_some_and(|value| {
                !expr_contains_der_of(value, inner_state) && !expr_contains_var(value, candidate)
            })
        })
}

fn expr_contains_der_of_state_or_indexed(expr: &Expression, state_name: &VarName) -> bool {
    expr_contains_der_of(expr, state_name)
}

fn collect_direct_demotion_plans(
    dae: &Dae,
    trace: bool,
) -> Result<HashMap<String, DirectStateDemotionPlan>, StructuralError> {
    let timer = structural_timing_start("direct_demotion.collect_round");
    let Some(round) = DirectDemotionRound::new(dae, trace)? else {
        return Ok(HashMap::new());
    };
    structural_timing_done("direct_demotion.collect_round", timer);
    let mut alias_safety_cache = AliasSafetyCache::new();
    let mut substitutions = HashMap::new();
    let mut counters = DirectDemotionCounters::default();

    for plan in collect_componentwise_direct_demotion_plans(&round, &mut counters)? {
        substitutions
            .entry(plan.state_name.as_str().to_string())
            .or_insert(plan);
    }

    let timer = structural_timing_start("direct_demotion.scan_equations");
    for (eq_index, eq) in round.dae.continuous.equations.iter().enumerate() {
        let Some(plan) = direct_demotion_plan_for_equation(
            &round,
            eq_index,
            eq,
            &mut counters,
            &mut alias_safety_cache,
        )?
        else {
            continue;
        };
        substitutions
            .entry(plan.state_name.as_str().to_string())
            .or_insert(plan);
    }
    structural_timing_done("direct_demotion.scan_equations", timer);

    log_direct_demotion_scan_summary(trace, round.state_count(), &substitutions, &counters);
    Ok(substitutions)
}

fn collect_componentwise_direct_demotion_plans(
    round: &DirectDemotionRound<'_>,
    counters: &mut DirectDemotionCounters,
) -> Result<Vec<DirectStateDemotionPlan>, StructuralError> {
    let mut by_state: IndexMap<String, Vec<Option<Expression>>> = IndexMap::new();
    let mut alias_safety_cache = AliasSafetyCache::new();

    for (eq_index, eq) in round.dae.continuous.equations.iter().enumerate() {
        let Some((state_name, flat_index, defining_expr)) =
            extract_state_component_direct_assignment_equation(
                round.dae,
                eq,
                &round.state_name_set,
            )
        else {
            continue;
        };
        if round.when_assigned_states.contains(state_name.as_str())
            || expr_contains_der_of_state_or_indexed(&defining_expr, &state_name)
        {
            continue;
        }
        let alias_scan_expr = mask_state_der_calls(&defining_expr, &round.state_name_set);
        if defining_expr_references_unsafe_non_state_alias_closure(
            &round.non_state_defining_exprs,
            &alias_scan_expr,
            &round.state_name_set,
            &round.non_state_unknown_names,
            eq_index,
            &mut alias_safety_cache,
        ) {
            counters.n_skip_unsafe_non_state_alias += 1;
            continue;
        }
        if !derivative_states_in_eq(&defining_expr, &round.state_names).is_empty() {
            let der_map = build_relaxed_derivative_map_for_exprs(
                round.dae,
                std::slice::from_ref(&defining_expr),
            )?;
            if !state_ders_in_expr_independently_defined(
                &defining_expr,
                &state_name,
                round,
                &der_map,
            ) {
                continue;
            }
        }
        let Some(size) = round
            .dae
            .variables
            .states
            .get(&state_name)
            .map(Variable::size)
            .filter(|size| *size > 1)
        else {
            continue;
        };
        let slots = by_state
            .entry(state_name.as_str().to_string())
            .or_insert_with(|| vec![None; size]);
        if flat_index >= slots.len() || slots[flat_index].is_some() {
            continue;
        }
        slots[flat_index] = Some(defining_expr);
    }

    let mut plans = Vec::new();
    for (state_name_string, slots) in by_state {
        let state_name = VarName::new(state_name_string.as_str());
        let Some(dims) = variable_dims_for_direct_demotion(round.dae, &state_name) else {
            continue;
        };
        let Some(component_exprs) = slots.into_iter().collect::<Option<Vec<_>>>() else {
            continue;
        };
        let Some(defining_expr) = array_expr_from_flat_values(component_exprs, &dims) else {
            continue;
        };
        let der_map = build_relaxed_derivative_map_for_exprs(
            round.dae,
            std::slice::from_ref(&defining_expr),
        )?;
        let Some(der_expr) = choose_derivative_replacement(
            &defining_expr,
            &round.state_name_set,
            round.dae,
            &der_map,
            counters,
        ) else {
            continue;
        };
        if expr_contains_der_of_state_or_indexed(&der_expr, &state_name)
            || expr_contains_der_of_non_state(&der_expr, &round.state_name_set)
        {
            continue;
        }
        plans.push(DirectStateDemotionPlan {
            state_name,
            der_expr,
        });
    }

    Ok(plans)
}

fn extract_state_component_direct_assignment_equation(
    dae: &Dae,
    eq: &Equation,
    state_name_set: &HashSet<String>,
) -> Option<(VarName, usize, Expression)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &eq.rhs
    else {
        return None;
    };
    if let Some((state_name, flat_index)) = state_component_ref_flat_index(dae, lhs, state_name_set)
        && !expr_contains_var(rhs, &state_name)
    {
        return Some((state_name, flat_index, *rhs.clone()));
    }
    if let Some((state_name, flat_index)) = state_component_ref_flat_index(dae, rhs, state_name_set)
        && !expr_contains_var(lhs, &state_name)
    {
        return Some((state_name, flat_index, *lhs.clone()));
    }
    None
}

fn state_component_ref_flat_index(
    dae: &Dae,
    expr: &Expression,
    state_name_set: &HashSet<String>,
) -> Option<(VarName, usize)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if subscripts.is_empty() || !state_name_set.contains(name.as_str()) {
        return None;
    }
    let state_name = name.var_name().clone();
    let dims = variable_dims_for_direct_demotion(dae, &state_name)?;
    let indices = static_subscript_indices(subscripts)?;
    let flat_index = flat_index_from_indices(&dims, &indices)?;
    Some((state_name, flat_index))
}

/// Demote states that are explicitly defined by direct assignment equations
/// (`state = expr`) and substitute `der(state)` with `d/dt(expr)` throughout
/// the system.
///
/// This removes structurally over-constrained "dummy/trajectory" states from
/// the differential set and keeps derivative chains algebraically consistent.
/// The defining expression need not reference `time` directly; if `d/dt(expr)`
/// can be resolved without introducing derivatives of non-state variables, the
/// state is demoted. States assigned in `when` clauses are preserved, since
/// they participate in event/reinit updates and must remain in the state vector.
pub fn demote_direct_assigned_states(dae: &mut Dae) -> Result<usize, StructuralError> {
    let max_rounds = dae.variables.states.len().clamp(1, 8);
    let mut total_demoted = 0usize;

    for round_index in 0..max_rounds {
        let trace = sim_trace_enabled();
        let label = format!("direct_demotion.round[{round_index}].collect_plans");
        let timer = structural_timing_start(&label);
        let substitutions = collect_direct_demotion_plans(dae, trace)?;
        structural_timing_done(&label, timer);

        if substitutions.is_empty() {
            break;
        }

        let label = format!("direct_demotion.round[{round_index}].apply_plans");
        let timer = structural_timing_start(&label);
        let demoted_this_round = apply_direct_demotion_plans(dae, &substitutions);
        structural_timing_done(&label, timer);

        if demoted_this_round == 0 {
            break;
        }
        total_demoted += demoted_this_round;
    }

    Ok(total_demoted)
}

fn direct_assignment_shape_is_demotable(
    dae: &Dae,
    state_name: &VarName,
    defining_expr: &Expression,
) -> bool {
    let Some(state) = dae.variables.states.get(state_name) else {
        return false;
    };
    if state.size() <= 1 {
        return !expr_contains_unsliced_vector_ref(defining_expr, dae);
    }
    let Some(state_dims) = variable_dims_for_direct_demotion(dae, state_name) else {
        return false;
    };
    expression_dims(defining_expr, dae).is_some_and(|expr_dims| expr_dims == state_dims)
}
