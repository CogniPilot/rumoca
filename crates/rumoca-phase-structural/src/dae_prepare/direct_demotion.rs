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
    if let Some(defining_expr) = extract_unknown_defining_expr(&eq.rhs, unknown_name, eq.span) {
        if expression_contains_any_der_call(&defining_expr) {
            return None;
        }
        return Some(defining_expr);
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
    excluded_eq_index: Option<usize>,
    alias_safety_cache: &mut AliasSafetyCache,
) -> bool {
    let mut visiting = HashSet::new();
    expr_depends_on_state_or_unsafe_non_state_alias(
        definitions,
        defining_expr,
        state_name_set,
        non_state_unknown_names,
        excluded_eq_index,
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
    promote_plan_derivative_algebraics(dae, &plan.promote_der_algebraics);
    ensure_scalar_state_partition_for_plan(dae, &plan.state_name);
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

fn promote_plan_derivative_algebraics(dae: &mut Dae, names: &[VarName]) {
    for name in names {
        if dae.variables.states.contains_key(name) {
            continue;
        }
        if let Some(var) = dae.variables.algebraics.shift_remove(name) {
            dae.variables.states.insert(name.clone(), var);
        }
    }
}

fn ensure_scalar_state_partition_for_plan(dae: &mut Dae, state_name: &VarName) {
    if dae.variables.states.contains_key(state_name) {
        return;
    }
    let Some(scalar) = rumoca_core::parse_scalar_name(state_name.as_str()) else {
        return;
    };
    let base_name = VarName::new(scalar.base);
    let Some(base_var) = dae.variables.states.shift_remove(&base_name) else {
        return;
    };
    let size = base_var.size();
    if size <= 1 {
        dae.variables.states.insert(base_name, base_var);
        return;
    }
    for flat_index in 0..size {
        let scalar_name = VarName::new(dae::scalar_name_text_for_flat_index(
            base_name.as_str(),
            &base_var.dims,
            flat_index,
        ));
        let mut scalar_var = base_var.clone();
        scalar_var.name = scalar_name.clone();
        scalar_var.dims.clear();
        dae.variables.states.insert(scalar_name, scalar_var);
    }
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
        Some(eq_index),
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
        promote_der_algebraics: Vec::new(),
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

fn collect_direct_demotion_plans_with_boundary_substitutions(
    dae: &Dae,
    trace: bool,
    boundary_substitutions: &[crate::eliminate::Substitution],
) -> Result<HashMap<String, DirectStateDemotionPlan>, StructuralError> {
    let timer = structural_timing_start("direct_demotion.collect_round");
    let Some(round) = DirectDemotionRound::new(dae, trace)? else {
        return Ok(HashMap::new());
    };
    structural_timing_done("direct_demotion.collect_round", timer);
    let mut alias_safety_cache = AliasSafetyCache::new();
    let mut substitutions = HashMap::new();
    let mut counters = DirectDemotionCounters::default();
    for plan in collect_componentwise_direct_demotion_plans(
        &round,
        &mut counters,
        boundary_substitutions,
        &mut alias_safety_cache,
    )? {
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
    boundary_substitutions: &[crate::eliminate::Substitution],
    alias_safety_cache: &mut AliasSafetyCache,
) -> Result<Vec<DirectStateDemotionPlan>, StructuralError> {
    let by_state = collect_componentwise_direct_demotion_slots(round, boundary_substitutions)?;

    let mut plans = Vec::new();
    for (state_name_string, slots) in by_state {
        let state_name = VarName::new(state_name_string.as_str());
        let Some(dims) = variable_dims_for_direct_demotion(round.dae, &state_name) else {
            continue;
        };
        let Some(component_exprs) = slots.iter().cloned().collect::<Option<Vec<_>>>() else {
            plans.extend(componentwise_scalar_demotion_plans(
                round,
                counters,
                &state_name,
                &dims,
                &slots,
                alias_safety_cache,
            )?);
            continue;
        };
        let Some(defining_expr) = array_expr_from_flat_values(component_exprs, &dims) else {
            plans.extend(componentwise_scalar_demotion_plans(
                round,
                counters,
                &state_name,
                &dims,
                &slots,
                alias_safety_cache,
            )?);
            continue;
        };
        let alias_scan_expr = mask_state_der_calls(&defining_expr, &round.state_name_set);
        if defining_expr_references_unsafe_non_state_alias_closure(
            &round.non_state_defining_exprs,
            &alias_scan_expr,
            &round.state_name_set,
            &round.non_state_unknown_names,
            None,
            alias_safety_cache,
        ) {
            counters.n_skip_unsafe_non_state_alias += 1;
            plans.extend(componentwise_scalar_demotion_plans(
                round,
                counters,
                &state_name,
                &dims,
                &slots,
                alias_safety_cache,
            )?);
            continue;
        }
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
            plans.extend(componentwise_scalar_demotion_plans(
                round,
                counters,
                &state_name,
                &dims,
                &slots,
                alias_safety_cache,
            )?);
            continue;
        };
        if expr_contains_der_of_state_or_indexed(&der_expr, &state_name)
            || expr_contains_der_of_non_state(&der_expr, &round.state_name_set)
        {
            plans.extend(componentwise_scalar_demotion_plans(
                round,
                counters,
                &state_name,
                &dims,
                &slots,
                alias_safety_cache,
            )?);
            continue;
        }
        plans.push(DirectStateDemotionPlan {
            state_name,
            der_expr,
            promote_der_algebraics: Vec::new(),
        });
    }

    Ok(plans)
}

fn collect_componentwise_direct_demotion_slots(
    round: &DirectDemotionRound<'_>,
    boundary_substitutions: &[crate::eliminate::Substitution],
) -> Result<IndexMap<String, Vec<Option<Expression>>>, StructuralError> {
    let mut by_state: IndexMap<String, Vec<Option<Expression>>> = IndexMap::new();
    for (eq_index, eq) in round.dae.continuous.equations.iter().enumerate() {
        let component_assignment = extract_state_component_direct_assignment_equation(
            round.dae,
            eq_index,
            eq,
            &round.state_name_set,
        );
        let aggregate_assignment = component_assignment
            .is_none()
            .then(|| aggregate_direct_assignment_component_slots(round, eq));
        let Some((state_name, slot_exprs)) = component_assignment
            .map(|(state_name, flat_index, defining_expr)| {
                (state_name, vec![(flat_index, defining_expr)])
            })
            .or_else(|| aggregate_assignment.flatten())
        else {
            continue;
        };
        if round.when_assigned_states.contains(state_name.as_str())
            || slot_exprs
                .iter()
                .any(|(_, expr)| expr_contains_der_of_state_or_indexed(expr, &state_name))
        {
            continue;
        }
        if slot_exprs
            .iter()
            .any(|(_, expr)| !derivative_states_in_eq(expr, &round.state_names).is_empty())
        {
            let seed_exprs = slot_exprs
                .iter()
                .map(|(_, expr)| expr.clone())
                .collect::<Vec<_>>();
            let der_map = build_relaxed_derivative_map_for_exprs(round.dae, &seed_exprs)?;
            if slot_exprs.iter().any(|(_, expr)| {
                !state_ders_in_expr_independently_defined(expr, &state_name, round, &der_map)
            }) {
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
        for (flat_index, defining_expr) in slot_exprs {
            if flat_index >= slots.len() || slots[flat_index].is_some() {
                continue;
            }
            slots[flat_index] = Some(defining_expr);
        }
    }
    collect_boundary_substitution_component_slots(round, &mut by_state, boundary_substitutions);
    Ok(by_state)
}

fn aggregate_direct_assignment_component_slots(
    round: &DirectDemotionRound<'_>,
    eq: &Equation,
) -> Option<(VarName, Vec<(usize, Expression)>)> {
    let (state_name, defining_expr, dims) = direct_assignment_candidate(round, eq)
        .and_then(|(state_name, defining_expr)| {
            let dims = variable_dims_for_direct_demotion(round.dae, &state_name)?;
            Some((state_name, defining_expr, dims))
        })
        .or_else(|| aggregate_scalar_state_family_assignment(round, eq))?;
    if dims.is_empty() {
        return None;
    }
    let size = dims.iter().try_fold(1usize, |acc, dim| {
        (*dim > 0).then(|| acc.checked_mul(*dim as usize)).flatten()
    })?;
    let mut slots = Vec::with_capacity(size);
    for flat_index in 0..size {
        let expr = project_flat_index_with_span(&defining_expr, &dims, flat_index, None)?;
        slots.push((flat_index, expr));
    }
    Some((state_name, slots))
}

fn aggregate_scalar_state_family_assignment(
    round: &DirectDemotionRound<'_>,
    eq: &Equation,
) -> Option<(VarName, Expression, Vec<i64>)> {
    if let Some(lhs) = &eq.lhs {
        let (state_name, dims) = aggregate_scalar_state_family_dims(round.dae, lhs.var_name())?;
        return Some((state_name, eq.rhs.clone(), dims));
    }
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &eq.rhs
    else {
        return None;
    };
    if let Some((state_name, dims)) =
        aggregate_scalar_state_family_expr_dims(round.dae, lhs, &round.state_name_set)
        && !expr_contains_var(rhs, &state_name)
    {
        return Some((state_name, *rhs.clone(), dims));
    }
    if let Some((state_name, dims)) =
        aggregate_scalar_state_family_expr_dims(round.dae, rhs, &round.state_name_set)
        && !expr_contains_var(lhs, &state_name)
    {
        return Some((state_name, *lhs.clone(), dims));
    }
    None
}

fn aggregate_scalar_state_family_expr_dims(
    dae: &Dae,
    expr: &Expression,
    state_name_set: &HashSet<String>,
) -> Option<(VarName, Vec<i64>)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() || state_name_set.contains(name.as_str()) {
        return None;
    }
    aggregate_scalar_state_family_dims(dae, name.var_name())
}

fn aggregate_scalar_state_family_dims(
    dae: &Dae,
    state_name: &VarName,
) -> Option<(VarName, Vec<i64>)> {
    if dae.variables.states.contains_key(state_name) {
        return None;
    }
    let mut parsed_scalars = dae
        .variables
        .states
        .keys()
        .filter_map(|candidate| {
            let scalar = rumoca_core::parse_scalar_name(candidate.as_str())?;
            (scalar.base == state_name.as_str()).then_some(scalar)
        })
        .collect::<Vec<_>>();
    if parsed_scalars.is_empty() {
        return None;
    }
    parsed_scalars.sort_by(|a, b| a.indices.cmp(&b.indices));
    let rank = parsed_scalars.first()?.indices.len();
    if rank == 0
        || parsed_scalars
            .iter()
            .any(|scalar| scalar.indices.len() != rank)
    {
        return None;
    }
    let mut dims = vec![0_i64; rank];
    for scalar in &parsed_scalars {
        for (axis, index) in scalar.indices.iter().enumerate() {
            dims[axis] = dims[axis].max(*index);
        }
    }
    let size = dims.iter().try_fold(1usize, |acc, dim| {
        (*dim > 0).then(|| acc.checked_mul(*dim as usize)).flatten()
    })?;
    if size != parsed_scalars.len() {
        return None;
    }
    for flat_index in 0..size {
        let scalar_name = VarName::new(dae::scalar_name_text_for_flat_index(
            state_name.as_str(),
            &dims,
            flat_index,
        ));
        if !dae.variables.states.contains_key(&scalar_name) {
            return None;
        }
    }
    Some((state_name.clone(), dims))
}

fn componentwise_scalar_demotion_plans(
    round: &DirectDemotionRound<'_>,
    counters: &mut DirectDemotionCounters,
    state_name: &VarName,
    dims: &[i64],
    slots: &[Option<Expression>],
    alias_safety_cache: &mut AliasSafetyCache,
) -> Result<Vec<DirectStateDemotionPlan>, StructuralError> {
    let mut plans = Vec::new();
    for (flat_index, defining_expr) in slots.iter().enumerate() {
        let Some(defining_expr) = defining_expr else {
            continue;
        };
        let scalar_state_name = VarName::new(dae::scalar_name_text_for_flat_index(
            state_name.as_str(),
            dims,
            flat_index,
        ));
        let alias_scan_expr = mask_state_der_calls(defining_expr, &round.state_name_set);
        if defining_expr_references_unsafe_non_state_alias_closure(
            &round.non_state_defining_exprs,
            &alias_scan_expr,
            &round.state_name_set,
            &round.non_state_unknown_names,
            None,
            alias_safety_cache,
        ) {
            counters.n_skip_unsafe_non_state_alias += 1;
            continue;
        }
        let der_map =
            build_relaxed_derivative_map_for_exprs(round.dae, std::slice::from_ref(defining_expr))?;
        let Some(der_expr) = choose_derivative_replacement_allowing_preferred_promotions(
            defining_expr,
            &round.state_name_set,
            round.dae,
            &der_map,
            counters,
        )?
        else {
            continue;
        };
        if expr_contains_der_of_state_or_indexed(&der_expr, &scalar_state_name) {
            continue;
        }
        let promote_der_algebraics = preferred_derivative_algebraics(round.dae, &der_expr);
        if expr_contains_der_of_unpromoted_non_state(
            &der_expr,
            &round.state_name_set,
            &promote_der_algebraics,
        ) {
            continue;
        }
        plans.push(DirectStateDemotionPlan {
            state_name: scalar_state_name,
            der_expr,
            promote_der_algebraics,
        });
    }
    Ok(plans)
}

fn choose_derivative_replacement_allowing_preferred_promotions(
    defining_expr: &Expression,
    state_name_set: &HashSet<String>,
    dae: &Dae,
    der_map: &HashMap<String, Expression>,
    counters: &mut DirectDemotionCounters,
) -> Result<Option<Expression>, StructuralError> {
    let Some(symbolic) = symbolic_time_derivative(defining_expr, dae, der_map) else {
        counters.n_skip_no_der_expr += 1;
        return Ok(None);
    };
    let promote_der_algebraics = preferred_derivative_algebraics(dae, &symbolic);
    if expr_contains_der_of_unpromoted_non_state(&symbolic, state_name_set, &promote_der_algebraics)
    {
        counters.n_skip_non_state_der += 1;
        return Ok(None);
    }
    Ok(Some(symbolic))
}

fn preferred_derivative_algebraics(dae: &Dae, expr: &Expression) -> Vec<VarName> {
    let mut names = Vec::new();
    collect_der_of_algebraics(expr, dae, &mut names);
    let mut seen = HashSet::new();
    names.retain(|name| {
        seen.insert(name.as_str().to_string()) && derivative_algebraic_is_preferred_state(dae, name)
    });
    names
}

fn derivative_algebraic_is_preferred_state(dae: &Dae, name: &VarName) -> bool {
    dae.variables.algebraics.get(name).is_some_and(|var| {
        state_select_rank(var.state_select) >= state_select_rank(rumoca_core::StateSelect::Prefer)
    })
}

fn expr_contains_der_of_unpromoted_non_state(
    expr: &Expression,
    state_name_set: &HashSet<String>,
    promoted: &[VarName],
) -> bool {
    let promoted = promoted
        .iter()
        .map(|name| name.as_str().to_string())
        .collect::<HashSet<_>>();
    let mut checker = UnpromotedNonStateDerivativeChecker {
        state_name_set,
        promoted: &promoted,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct UnpromotedNonStateDerivativeChecker<'a> {
    state_name_set: &'a HashSet<String>,
    promoted: &'a HashSet<String>,
    found: bool,
}

impl ExpressionVisitor for UnpromotedNonStateDerivativeChecker<'_> {
    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der {
            self.found =
                der_arg_is_not_plain_state_or_promoted(args, self.state_name_set, self.promoted);
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

fn der_arg_is_not_plain_state_or_promoted(
    args: &[Expression],
    state_name_set: &HashSet<String>,
    promoted: &HashSet<String>,
) -> bool {
    if args.len() != 1 {
        return true;
    }
    match &args[0] {
        Expression::VarRef { name, .. } => {
            !state_name_set.contains(name.as_str()) && !promoted.contains(name.as_str())
        }
        _ => true,
    }
}

fn collect_boundary_substitution_component_slots(
    round: &DirectDemotionRound<'_>,
    by_state: &mut IndexMap<String, Vec<Option<Expression>>>,
    boundary_substitutions: &[crate::eliminate::Substitution],
) {
    for substitution in boundary_substitutions {
        let Some((state_name, flat_index)) =
            boundary_substitution_state_component(round.dae, substitution, &round.state_name_set)
        else {
            continue;
        };
        if round.when_assigned_states.contains(state_name.as_str())
            || expr_contains_der_of_state_or_indexed(&substitution.expr, &state_name)
        {
            continue;
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
        if flat_index < slots.len() && slots[flat_index].is_none() {
            slots[flat_index] = Some(substitution.expr.clone());
        }
    }
}

fn boundary_substitution_state_component(
    dae: &Dae,
    substitution: &crate::eliminate::Substitution,
    state_name_set: &HashSet<String>,
) -> Option<(VarName, usize)> {
    let scalar = rumoca_core::parse_scalar_name(substitution.var_name.as_str())?;
    if !state_name_set.contains(scalar.base) {
        return None;
    }
    let state_name = VarName::new(scalar.base);
    let dims = variable_dims_for_direct_demotion(dae, &state_name)?;
    let flat_index = flat_index_from_indices(&dims, &scalar.indices)?;
    Some((state_name, flat_index))
}

fn extract_state_component_direct_assignment_equation(
    dae: &Dae,
    eq_index: usize,
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
    if let Some((state_name, flat_index)) =
        structured_scalar_slot_state_component(dae, eq_index, lhs, state_name_set)
        && !expr_contains_var(rhs, &state_name)
    {
        return Some((state_name, flat_index, *rhs.clone()));
    }
    if let Some((state_name, flat_index)) =
        structured_scalar_slot_state_component(dae, eq_index, rhs, state_name_set)
        && !expr_contains_var(lhs, &state_name)
    {
        return Some((state_name, flat_index, *lhs.clone()));
    }
    None
}

fn structured_scalar_slot_state_component(
    dae: &Dae,
    eq_index: usize,
    expr: &Expression,
    state_name_set: &HashSet<String>,
) -> Option<(VarName, usize)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() || !state_name_set.contains(name.as_str()) {
        return None;
    }
    let state_name = name.var_name().clone();
    let state_size = dae.variables.states.get(&state_name)?.size();
    if state_size <= 1 {
        return None;
    }
    let slot = dae::structured_equation_slot(&dae.continuous.structured_equations, eq_index)?;
    if slot.equation_count != 1 || slot.equation_position != 0 {
        return None;
    }
    let family = dae.continuous.structured_equations.get(slot.family_index)?;
    if family.equation_counts.len() != state_size || slot.iteration_index >= state_size {
        return None;
    }
    Some((state_name, slot.iteration_index))
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
    demote_direct_assigned_states_with_boundary_substitutions(dae, &[])
}

pub fn demote_direct_assigned_states_with_boundary_substitutions(
    dae: &mut Dae,
    boundary_substitutions: &[crate::eliminate::Substitution],
) -> Result<usize, StructuralError> {
    let max_rounds = dae.variables.states.len().clamp(1, 8);
    let mut total_demoted = 0usize;

    for round_index in 0..max_rounds {
        let trace = sim_trace_enabled();
        let label = format!("direct_demotion.round[{round_index}].collect_plans");
        let timer = structural_timing_start(&label);
        let substitutions = collect_direct_demotion_plans_with_boundary_substitutions(
            dae,
            trace,
            boundary_substitutions,
        )?;
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
