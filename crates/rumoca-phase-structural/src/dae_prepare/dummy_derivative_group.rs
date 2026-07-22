use super::*;

struct DummyDerivativeGroupPlan {
    aggregate_name: VarName,
    constraint_index: usize,
    differentiated_constraint: Expression,
    states: Vec<(VarName, Expression)>,
}

struct DummyDerivativeGroupCandidate {
    aggregate_name: VarName,
    constraint_index: usize,
    states: Vec<(VarName, Expression)>,
}

pub(crate) struct SingularHolonomicCandidate {
    pub(crate) dae: Dae,
    pub(crate) constraint_index: usize,
    pub(crate) demoted_states: Vec<(u8, VarName)>,
}

type HolonomicDifferentiation = (Option<VarName>, Expression, Vec<Expression>);

struct ConstraintDifferentiationCtx<'a> {
    dae: &'a Dae,
    current_state: &'a VarName,
    defining_expr_index: &'a DefiningExprIndex,
    structural_bindings: &'a HashMap<String, f64>,
    selected_derivatives: &'a HashMap<String, Expression>,
    derivative_value: &'a Expression,
    future_states: &'a HashSet<String>,
}

struct DerivativeResolutionCtx<'a> {
    dae: &'a Dae,
    current_state: &'a VarName,
    constraint_indices: &'a [usize],
    defining_expr_index: &'a DefiningExprIndex,
    structural_bindings: &'a HashMap<String, f64>,
}

/// Enumerate structurally valid prefixes of smooth holonomic state chains.
///
/// This pass deliberately does not choose a candidate. The caller evaluates
/// every prefix with the DAE maximum matching and may commit only a strict
/// reduction in matching deficiency. Keeping selection separate from symbolic
/// differentiation prevents equation order from becoming a semantic choice.
#[cfg(test)]
pub(crate) fn singular_holonomic_state_candidates(
    dae: &Dae,
) -> Result<Vec<SingularHolonomicCandidate>, StructuralError> {
    let derivative_values = isolated_state_derivative_values(dae);
    singular_holonomic_state_candidates_with_derivative_values(dae, &derivative_values)
}

pub(crate) fn singular_holonomic_state_candidates_with_derivative_values(
    dae: &Dae,
    retained_derivative_values: &HashMap<String, Expression>,
) -> Result<Vec<SingularHolonomicCandidate>, StructuralError> {
    let defining_expr_index = collect_residual_defining_expr_index(dae);
    let structural_bindings = crate::static_eval::structural_scalar_bindings(dae);
    let derivative_values = build_der_value_map(dae);
    let chain_successors = dae
        .variables
        .states
        .iter()
        .filter(|(_, variable)| variable.state_select != rumoca_core::StateSelect::Always)
        .filter_map(|(name, _)| derivative_values.get(name.as_str()))
        .filter_map(|value| plain_continuous_derivative_value(value, dae))
        .filter(|name| dae.variables.states.contains_key(name))
        .collect::<HashSet<_>>();
    let mut state_candidates = dae
        .variables
        .states
        .iter()
        .filter(|(_, variable)| variable.state_select != rumoca_core::StateSelect::Always)
        .filter(|(name, _)| !state_has_overlapping_event_update(dae, name))
        // A holonomic chain starts at a position-level state. States reached as
        // exact ODE successors (for example `q' = v`) are prolonged from that
        // root and must not launch duplicate velocity-level searches.
        .filter(|(name, _)| !chain_successors.contains(*name))
        .map(|(name, variable)| (state_select_rank(variable.state_select), name.clone()))
        .collect::<Vec<_>>();
    state_candidates.sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0).then_with(|| lhs.1.cmp(&rhs.1)));

    trace_vector_holonomic_seeds(dae, &chain_successors, &structural_bindings);

    let mut candidates = Vec::new();
    for (_, state_name) in state_candidates {
        for constraint_indices in
            holonomic_constraint_groups(dae, &state_name, &structural_bindings)?
        {
            candidates.extend(build_singular_holonomic_chain_prefixes(
                dae,
                &state_name,
                &constraint_indices,
                &defining_expr_index,
                &structural_bindings,
                retained_derivative_values,
            )?);
        }
    }
    Ok(candidates)
}

fn trace_vector_holonomic_seeds(
    dae: &Dae,
    chain_successors: &HashSet<VarName>,
    structural_bindings: &HashMap<String, f64>,
) {
    for (state_name, variable) in &dae.variables.states {
        if variable.dims.is_empty()
            || variable.state_select == rumoca_core::StateSelect::Always
            || chain_successors.contains(state_name)
            || state_has_overlapping_event_update(dae, state_name)
        {
            continue;
        }
        let constraints = dae
            .continuous
            .equations
            .iter()
            .enumerate()
            .filter(|(_, equation)| {
                expr_contains_var(&equation.rhs, state_name)
                    && !expression_contains_any_der_call(&equation.rhs)
                    && state_row_reduction::expression_is_smooth_for_index_reduction(
                        &equation.rhs,
                        dae,
                        structural_bindings,
                    )
            })
            .map(|(index, equation)| (index, equation.scalar_count, equation.origin.as_str()))
            .take(12)
            .collect::<Vec<_>>();
        if !constraints.is_empty() {
            crate::structural_trace!(
                "[sim-trace] vector holonomic seed state={} dims={:?} constraints={:?}",
                state_name.as_str(),
                variable.dims,
                constraints
            );
        }
    }
}

pub(super) fn holonomic_constraint_groups(
    dae: &Dae,
    state_name: &VarName,
    structural_bindings: &HashMap<String, f64>,
) -> Result<Vec<Vec<usize>>, StructuralError> {
    let Some(state) = dae.variables.states.get(state_name) else {
        return Ok(Vec::new());
    };
    let eligible = dae
        .continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(_, equation)| {
            expression_contains_state_component(&equation.rhs, state_name, &state.dims)
                && !expression_contains_any_der_call(&equation.rhs)
                && state_row_reduction::expression_is_smooth_for_index_reduction(
                    &equation.rhs,
                    dae,
                    structural_bindings,
                )
        })
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    if state.dims.is_empty() {
        return Ok(eligible
            .into_iter()
            .filter(|index| dae.continuous.equations[*index].scalar_count == 1)
            .map(|index| vec![index])
            .collect());
    }

    let mut groups = Vec::new();
    for start in 0..eligible.len() {
        if let Some(group) = complete_constraint_group_from_start(
            dae,
            &eligible,
            start,
            state_name,
            &state.dims,
            state.size(),
        ) {
            groups.push(group);
        }
    }
    if !groups.is_empty() {
        crate::structural_trace!(
            "[sim-trace] vector holonomic groups state={} groups={:?}",
            state_name.as_str(),
            groups
        );
    }
    Ok(groups)
}

fn complete_constraint_group_from_start(
    dae: &Dae,
    eligible: &[usize],
    start: usize,
    state_name: &VarName,
    state_dims: &[i64],
    state_size: usize,
) -> Option<Vec<usize>> {
    let mut group = Vec::new();
    let mut scalar_width = 0;
    let origin = &dae.continuous.equations[eligible[start]].origin;
    for index in eligible.iter().copied().skip(start) {
        if group.last().is_some_and(|previous| index != previous + 1)
            || dae.continuous.equations[index].origin != *origin
        {
            return None;
        }
        scalar_width += dae.continuous.equations[index].scalar_count;
        group.push(index);
        if scalar_width == state_size {
            return constraint_group_contains_complete_state(dae, &group, state_name, state_dims)
                .then_some(group);
        }
        if scalar_width > state_size {
            return None;
        }
    }
    None
}

fn expression_contains_state_component(
    expression: &Expression,
    state_name: &VarName,
    dims: &[i64],
) -> bool {
    contains_exact_reference(expression, state_name.as_str())
        || (0..dims.iter().product::<i64>() as usize).any(|flat_index| {
            let component = dae::scalar_name_for_flat_index(state_name, dims, flat_index);
            contains_exact_reference(expression, component.as_str())
        })
}

fn constraint_group_contains_complete_state(
    dae: &Dae,
    group: &[usize],
    state_name: &VarName,
    dims: &[i64],
) -> bool {
    let expressions = group
        .iter()
        .map(|index| &dae.continuous.equations[*index].rhs)
        .collect::<Vec<_>>();
    expressions
        .iter()
        .any(|expr| contains_exact_reference(expr, state_name.as_str()))
        || (0..dims.iter().product::<i64>() as usize).all(|flat_index| {
            let component = dae::scalar_name_for_flat_index(state_name, dims, flat_index);
            expressions
                .iter()
                .any(|expr| contains_exact_reference(expr, component.as_str()))
        })
}

fn build_singular_holonomic_chain_prefixes(
    dae: &Dae,
    seed_state: &VarName,
    constraint_indices: &[usize],
    defining_expr_index: &DefiningExprIndex,
    structural_bindings: &HashMap<String, f64>,
    retained_derivative_values: &HashMap<String, Expression>,
) -> Result<Vec<SingularHolonomicCandidate>, StructuralError> {
    let mut staged = dae.clone();
    let mut current_state = seed_state.clone();
    let mut current_constraint_indices = constraint_indices.to_vec();
    let mut selected_derivatives = HashMap::<String, Expression>::new();
    let mut demoted_states = Vec::new();
    let mut candidates = Vec::new();
    let constraint_index = constraint_indices[0];

    while let Some(variable) = staged.variables.states.get(&current_state) {
        if variable.state_select == rumoca_core::StateSelect::Always
            || state_has_overlapping_event_update(&staged, &current_state)
        {
            break;
        }
        let Some((derivative_name, derivative_value, differentiated)) =
            differentiate_holonomic_constraint_group(
                &staged,
                &current_state,
                &current_constraint_indices,
                defining_expr_index,
                structural_bindings,
                &selected_derivatives,
                retained_derivative_values,
            )?
        else {
            break;
        };
        let new_constraint_indices = append_differentiated_constraints(
            &mut staged,
            &current_constraint_indices,
            differentiated,
        );
        rewrite_derivative_value_everywhere(&mut staged, &current_state, &derivative_value)?;
        let Some(demoted) = staged.variables.states.shift_remove(&current_state) else {
            break;
        };
        let rank = state_select_rank(demoted.state_select);
        staged
            .variables
            .algebraics
            .insert(current_state.clone(), demoted);
        selected_derivatives.insert(current_state.as_str().to_string(), derivative_value);
        demoted_states.push((rank, current_state.clone()));
        candidates.push(SingularHolonomicCandidate {
            dae: staged.clone(),
            constraint_index,
            demoted_states: demoted_states.clone(),
        });

        let Some(derivative_name) = derivative_name else {
            break;
        };
        if !staged.variables.states.contains_key(&derivative_name) {
            break;
        }
        current_state = derivative_name;
        current_constraint_indices = new_constraint_indices;
    }
    Ok(candidates)
}

pub(super) fn differentiate_holonomic_constraint_group(
    dae: &Dae,
    current_state: &VarName,
    constraint_indices: &[usize],
    defining_expr_index: &DefiningExprIndex,
    structural_bindings: &HashMap<String, f64>,
    selected_derivatives: &HashMap<String, Expression>,
    retained_derivative_values: &HashMap<String, Expression>,
) -> Result<Option<HolonomicDifferentiation>, StructuralError> {
    let Some(derivative_value) = resolve_holonomic_derivative_value(
        dae,
        current_state,
        constraint_indices,
        defining_expr_index,
        structural_bindings,
        retained_derivative_values,
    )?
    else {
        crate::structural_trace!(
            "[sim-trace] holonomic group rejected state={} reason=missing_derivative_value",
            current_state.as_str()
        );
        return Ok(None);
    };
    let state_dims = &dae.variables.states[current_state].dims;
    let derivative_dims = row_shape::expression_dims_for_row_count(dae, &derivative_value)?;
    let shape_matches = if state_dims.is_empty() {
        row_shape::residual_scalar_width(dae, &derivative_value)? == 1
    } else {
        derivative_dims.as_ref() == Some(state_dims)
    };
    if !shape_matches {
        crate::structural_trace!(
            "[sim-trace] holonomic group rejected state={} reason=shape_mismatch state_dims={:?} derivative_dims={:?}",
            current_state.as_str(),
            state_dims,
            derivative_dims
        );
        return Ok(None);
    }
    let derivative_name = plain_continuous_derivative_value(&derivative_value, dae);
    let future_states = dae
        .variables
        .states
        .keys()
        .filter(|name| *name != current_state)
        .map(|name| name.as_str().to_string())
        .collect::<HashSet<_>>();
    let context = ConstraintDifferentiationCtx {
        dae,
        current_state,
        defining_expr_index,
        structural_bindings,
        selected_derivatives,
        derivative_value: &derivative_value,
        future_states: &future_states,
    };
    let mut differentiated = Vec::with_capacity(constraint_indices.len());
    for constraint_index in constraint_indices {
        let Some(expression) = differentiate_one_constraint(&context, *constraint_index)? else {
            return Ok(None);
        };
        differentiated.push(expression);
    }
    if derivative_name.as_ref().is_some_and(|name| {
        !differentiated
            .iter()
            .any(|expression| expr_contains_var(expression, name))
    }) {
        crate::structural_trace!(
            "[sim-trace] holonomic group rejected state={} derivative={} reason=derivative_target_absent",
            current_state.as_str(),
            derivative_name
                .as_ref()
                .map_or("<expression>", VarName::as_str)
        );
        return Ok(None);
    }
    Ok(Some((derivative_name, derivative_value, differentiated)))
}

fn differentiate_one_constraint(
    context: &ConstraintDifferentiationCtx<'_>,
    constraint_index: usize,
) -> Result<Option<Expression>, StructuralError> {
    let constraint = &context.dae.continuous.equations[constraint_index];
    let mut derivative_map = build_relaxed_derivative_map_for_exprs_with_index(
        context.dae,
        context.defining_expr_index,
        std::slice::from_ref(&constraint.rhs),
        RelaxedDerivativeMapOptions {
            excluded_equation: Some(constraint_index),
            ..RelaxedDerivativeMapOptions::default()
        },
    )?;
    propagate_exact_alias_derivatives(
        context.dae,
        &precise_alias_adjacency(context.dae),
        &mut derivative_map,
    );
    derivative_map.extend(context.selected_derivatives.clone());
    derivative_map.insert(
        context.current_state.as_str().to_string(),
        context.derivative_value.clone(),
    );
    let Some(expression) = symbolic_time_derivative(&constraint.rhs, context.dae, &derivative_map)
    else {
        crate::structural_trace!(
            "[sim-trace] holonomic group rejected state={} constraint={} reason=symbolic_derivative_failed expression={}",
            context.current_state.as_str(),
            constraint_index,
            truncate_debug(&format!("{:?}", constraint.rhs), 1_600)
        );
        return Ok(None);
    };
    let expression = substitute_derivative_value_in_expression(
        context.dae,
        &expression,
        context.current_state,
        context.derivative_value,
    )?;
    let expression = crate::eliminate::simplify_arithmetic_identities(expression);
    let retains_state_derivative = expr_contains_der_of(&expression, context.current_state);
    let has_unclosed_derivative =
        expr_contains_der_of_non_state(&expression, context.future_states);
    let differentiated_width = row_shape::residual_scalar_width(context.dae, &expression)?;
    let shape_mismatch = differentiated_width != constraint.scalar_count;
    let nonsmooth = !state_row_reduction::expression_is_smooth_for_index_reduction(
        &expression,
        context.dae,
        context.structural_bindings,
    );
    if retains_state_derivative || has_unclosed_derivative || shape_mismatch || nonsmooth {
        crate::structural_trace!(
            "[sim-trace] holonomic group rejected state={} constraint={} reason=invalid_derivative retains_state_derivative={} unclosed_derivative={} shape_mismatch={} differentiated_width={} source_width={} nonsmooth={}",
            context.current_state.as_str(),
            constraint_index,
            retains_state_derivative,
            has_unclosed_derivative,
            shape_mismatch,
            differentiated_width,
            constraint.scalar_count,
            nonsmooth
        );
        return Ok(None);
    }
    Ok(Some(expression))
}

pub(crate) fn isolated_state_derivative_values(dae: &Dae) -> HashMap<String, Expression> {
    let mut derivative_values = build_der_value_map(dae);
    for (state_name, state) in &dae.variables.states {
        if derivative_values.contains_key(state_name.as_str()) {
            continue;
        }
        let mut isolated_values = Vec::new();
        for value in dae
            .continuous
            .equations
            .iter()
            .filter(|equation| equation.scalar_count == state.size())
            .filter_map(|equation| try_extract_der_value(&equation.rhs, state_name))
            .filter(|value| !expr_contains_der_of(value, state_name))
        {
            if !isolated_values.contains(&value) {
                isolated_values.push(value);
            }
        }
        if let [value] = isolated_values.as_slice() {
            derivative_values.insert(state_name.as_str().to_string(), value.clone());
        }
    }
    derivative_values
}

fn resolve_holonomic_derivative_value(
    dae: &Dae,
    current_state: &VarName,
    constraint_indices: &[usize],
    defining_expr_index: &DefiningExprIndex,
    structural_bindings: &HashMap<String, f64>,
    retained_derivative_values: &HashMap<String, Expression>,
) -> Result<Option<Expression>, StructuralError> {
    if let Some(value) =
        direct_holonomic_derivative_value(dae, current_state, retained_derivative_values)
    {
        return Ok(Some(value));
    }
    let context = DerivativeResolutionCtx {
        dae,
        current_state,
        constraint_indices,
        defining_expr_index,
        structural_bindings,
    };
    if let Some(value) = resolve_derivative_from_definitions(&context)? {
        return Ok(Some(value));
    }
    trace_unavailable_holonomic_derivative(&context);
    Ok(None)
}

fn direct_holonomic_derivative_value(
    dae: &Dae,
    current_state: &VarName,
    retained_derivative_values: &HashMap<String, Expression>,
) -> Option<Expression> {
    if let Some(value) = build_der_value_map(dae).get(current_state.as_str()) {
        return Some(value.clone());
    }
    if let Some(value) = retained_derivative_values.get(current_state.as_str()) {
        crate::structural_trace!(
            "[sim-trace] holonomic derivative retained state={} value={}",
            current_state.as_str(),
            truncate_debug(&format!("{value:?}"), 240)
        );
        return Some(value.clone());
    }
    let state_size = dae.variables.states[current_state].size();
    let isolated_values = dae
        .continuous
        .equations
        .iter()
        .filter(|equation| equation.scalar_count == state_size)
        .filter_map(|equation| try_extract_der_value(&equation.rhs, current_state))
        .filter(|value| !expr_contains_der_of(value, current_state))
        .fold(Vec::new(), |mut values, value| {
            if !values.contains(&value) {
                values.push(value);
            }
            values
        });
    let [value] = isolated_values.as_slice() else {
        return None;
    };
    crate::structural_trace!(
        "[sim-trace] holonomic derivative isolated state={} value={}",
        current_state.as_str(),
        truncate_debug(&format!("{value:?}"), 240)
    );
    Some(value.clone())
}

fn resolve_derivative_from_definitions(
    context: &DerivativeResolutionCtx<'_>,
) -> Result<Option<Expression>, StructuralError> {
    let future_states = context
        .dae
        .variables
        .states
        .keys()
        .filter(|name| *name != context.current_state)
        .map(|name| name.as_str().to_string())
        .collect::<HashSet<_>>();
    for definition in context
        .defining_expr_index
        .get(context.current_state.as_str())
        .into_iter()
        .flatten()
        .filter(|definition| {
            !context
                .constraint_indices
                .contains(&definition.equation_index)
        })
    {
        let mut derivative_map = build_relaxed_derivative_map_for_exprs_with_index(
            context.dae,
            context.defining_expr_index,
            std::slice::from_ref(&definition.expr),
            RelaxedDerivativeMapOptions {
                canonical_state_derivative: Some(context.current_state),
                rejected_state_derivative: Some(context.current_state),
                excluded_equation: Some(definition.equation_index),
            },
        )?;
        propagate_exact_alias_derivatives(
            context.dae,
            &precise_alias_adjacency(context.dae),
            &mut derivative_map,
        );
        let Some(derivative) =
            symbolic_time_derivative(&definition.expr, context.dae, &derivative_map)
        else {
            continue;
        };
        let derivative = crate::eliminate::simplify_arithmetic_identities(derivative);
        if !expr_contains_der_of(&derivative, context.current_state)
            && !expr_contains_der_of_non_state(&derivative, &future_states)
            && state_row_reduction::expression_is_smooth_for_index_reduction(
                &derivative,
                context.dae,
                context.structural_bindings,
            )
        {
            crate::structural_trace!(
                "[sim-trace] holonomic derivative resolved state={} definition={} value={}",
                context.current_state.as_str(),
                definition.equation_index,
                truncate_debug(&format!("{derivative:?}"), 240)
            );
            return Ok(Some(derivative));
        }
    }
    Ok(None)
}

fn trace_unavailable_holonomic_derivative(context: &DerivativeResolutionCtx<'_>) {
    let derivative_rows = context
        .dae
        .continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(_, equation)| expr_contains_der_of(&equation.rhs, context.current_state))
        .map(|(index, equation)| {
            (
                index,
                equation.scalar_count,
                equation.origin.as_str(),
                truncate_debug(&format!("{:?}", equation.rhs), 1_600),
            )
        })
        .collect::<Vec<_>>();
    crate::structural_trace!(
        "[sim-trace] holonomic derivative unavailable state={} derivative_rows={:?} defining_candidates={}",
        context.current_state.as_str(),
        derivative_rows,
        context
            .defining_expr_index
            .get(context.current_state.as_str())
            .map_or(0, Vec::len)
    );
}

fn append_differentiated_constraints(
    dae: &mut Dae,
    constraint_indices: &[usize],
    differentiated: Vec<Expression>,
) -> Vec<usize> {
    constraint_indices
        .iter()
        .copied()
        .zip(differentiated)
        .map(|(constraint_index, rhs)| {
            let source = &dae.continuous.equations[constraint_index];
            let span = source.span;
            let scalar_count = source.scalar_count;
            let origin = if source.origin.is_empty() {
                "index_reduction:d_dt_holonomic_constraint".to_string()
            } else {
                format!(
                    "{}|index_reduction:d_dt_holonomic_constraint",
                    source.origin
                )
            };
            dae.continuous.equations.push(Equation {
                lhs: None,
                rhs,
                span,
                origin,
                scalar_count,
            });
            dae.continuous.equations.len() - 1
        })
        .collect()
}

fn plain_continuous_derivative_value(expr: &Expression, dae: &Dae) -> Option<VarName> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let name = name.var_name();
    (dae.variables.states.contains_key(name)
        || dae.variables.algebraics.contains_key(name)
        || dae.variables.outputs.contains_key(name))
    .then(|| name.clone())
}

pub(super) fn reduce_one_complete_dummy_derivative_group(
    dae: &mut Dae,
) -> Result<usize, StructuralError> {
    let Some(plan) = find_complete_dummy_derivative_group(dae)? else {
        return Ok(0);
    };
    apply_complete_dummy_derivative_group(dae, plan)
}

pub(super) fn planned_complete_dummy_derivative_group_states(
    dae: &Dae,
) -> Result<IndexSet<VarName>, StructuralError> {
    Ok(find_complete_dummy_derivative_group(dae)?
        .map(|plan| {
            plan.states
                .into_iter()
                .map(|(state_name, _)| state_name)
                .collect()
        })
        .unwrap_or_default())
}

fn find_complete_dummy_derivative_group(
    dae: &Dae,
) -> Result<Option<DummyDerivativeGroupPlan>, StructuralError> {
    let candidates = complete_dummy_derivative_group_candidates(dae);
    if candidates.is_empty() {
        return Ok(None);
    }
    let adjacency = precise_alias_adjacency(dae);
    let defining_expr_index = collect_residual_defining_expr_index(dae);
    let structural_bindings = crate::static_eval::structural_scalar_bindings(dae);
    for candidate in candidates {
        let constraint = &dae.continuous.equations[candidate.constraint_index];
        let mut derivative_map = build_relaxed_derivative_map_for_exprs_with_index(
            dae,
            &defining_expr_index,
            std::slice::from_ref(&constraint.rhs),
            RelaxedDerivativeMapOptions {
                excluded_equation: Some(candidate.constraint_index),
                ..RelaxedDerivativeMapOptions::default()
            },
        )?;
        propagate_exact_alias_derivatives(dae, &adjacency, &mut derivative_map);
        derivative_map.insert(
            candidate.aggregate_name.as_str().to_string(),
            Expression::Array {
                elements: candidate
                    .states
                    .iter()
                    .map(|(_, value)| value.clone())
                    .collect(),
                is_matrix: false,
                span: constraint.span,
            },
        );
        let Some(differentiated) = symbolic_time_derivative(&constraint.rhs, dae, &derivative_map)
        else {
            continue;
        };
        let differentiated = crate::eliminate::simplify_arithmetic_identities(differentiated);
        if !differentiated_group_is_closed(dae, &differentiated, &candidate.states)?
            || !state_row_reduction::expression_is_smooth_for_index_reduction(
                &differentiated,
                dae,
                &structural_bindings,
            )
        {
            continue;
        }
        return Ok(Some(DummyDerivativeGroupPlan {
            aggregate_name: candidate.aggregate_name,
            constraint_index: candidate.constraint_index,
            differentiated_constraint: differentiated,
            states: candidate.states,
        }));
    }
    Ok(None)
}

fn complete_dummy_derivative_group_candidates(dae: &Dae) -> Vec<DummyDerivativeGroupCandidate> {
    let derivative_values = dummy_derivative_values(dae);
    if derivative_values.is_empty() {
        return Vec::new();
    }
    let adjacency = precise_alias_adjacency(dae);
    let structural_bindings = crate::static_eval::structural_scalar_bindings(dae);
    let mut candidates = Vec::new();
    for (aggregate_name, aggregate) in dae
        .variables
        .algebraics
        .iter()
        .chain(&dae.variables.outputs)
        .chain(&dae.variables.inputs)
    {
        if aggregate.dims.len() != 1 || aggregate.size() < 2 {
            continue;
        }
        let Some(states) = complete_component_state_group(
            dae,
            aggregate_name,
            aggregate,
            &adjacency,
            &derivative_values,
        ) else {
            continue;
        };
        for (constraint_index, constraint) in dae.continuous.equations.iter().enumerate() {
            if constraint.scalar_count == aggregate.size()
                && !is_connection_equation_origin(&constraint.origin)
                && precise_alias_pair(&constraint.rhs).is_none()
                && !expression_contains_any_der_call(&constraint.rhs)
                && contains_complete_aggregate_reference(
                    &constraint.rhs,
                    aggregate_name,
                    &aggregate.dims,
                )
                && state_row_reduction::expression_is_smooth_for_index_reduction(
                    &constraint.rhs,
                    dae,
                    &structural_bindings,
                )
            {
                candidates.push(DummyDerivativeGroupCandidate {
                    aggregate_name: aggregate_name.clone(),
                    constraint_index,
                    states: states.clone(),
                });
            }
        }
    }
    candidates
}

fn propagate_exact_alias_derivatives(
    dae: &Dae,
    adjacency: &HashMap<String, HashSet<String>>,
    derivative_map: &mut HashMap<String, Expression>,
) {
    let known_derivatives = build_der_value_map(dae);
    let mut seeds = known_derivatives.into_iter().collect::<Vec<_>>();
    seeds.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
    for (seed_name, derivative) in seeds {
        let Some(seed_variable) = exact_continuous_variable(dae, &seed_name) else {
            continue;
        };
        for member in alias_component(&seed_name, adjacency) {
            if exact_continuous_variable(dae, &member)
                .is_some_and(|variable| variable.dims == seed_variable.dims)
            {
                derivative_map.insert(member, derivative.clone());
            }
        }
    }
}

fn dummy_derivative_values(dae: &Dae) -> IndexMap<VarName, Expression> {
    let derivative_values = build_der_value_map(dae);
    dae.variables
        .states
        .iter()
        .filter(|(_, variable)| {
            variable.dims.is_empty()
                && matches!(
                    variable.state_select,
                    rumoca_core::StateSelect::Never
                        | rumoca_core::StateSelect::Avoid
                        | rumoca_core::StateSelect::Default
                )
        })
        .filter(|(name, _)| !state_has_overlapping_event_update(dae, name))
        .filter_map(|(name, _)| {
            let value = derivative_values.get(name.as_str())?;
            plain_non_state_unknown(value, dae)?;
            Some((name.clone(), value.clone()))
        })
        .collect()
}

fn plain_non_state_unknown(expr: &Expression, dae: &Dae) -> Option<VarName> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let name = name.var_name();
    (dae.variables.algebraics.contains_key(name) || dae.variables.outputs.contains_key(name))
        .then(|| name.clone())
}

fn precise_alias_adjacency(dae: &Dae) -> HashMap<String, HashSet<String>> {
    let mut adjacency = HashMap::new();
    for (lhs, rhs) in dae
        .continuous
        .equations
        .iter()
        .filter_map(|equation| precise_alias_pair(&equation.rhs))
        .filter(|(lhs, rhs)| lhs != rhs)
    {
        adjacency
            .entry(lhs.clone())
            .or_insert_with(HashSet::new)
            .insert(rhs.clone());
        adjacency
            .entry(rhs)
            .or_insert_with(HashSet::new)
            .insert(lhs);
    }
    adjacency
}

fn precise_alias_pair(expr: &Expression) -> Option<(String, String)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expr
    else {
        return None;
    };
    Some((exact_scalar_reference(lhs)?, exact_scalar_reference(rhs)?))
}

fn exact_scalar_reference(expr: &Expression) -> Option<String> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }
    crate::scalarize::scalarization_var_ref_name(name, subscripts)
}

fn alias_component(root: &str, adjacency: &HashMap<String, HashSet<String>>) -> HashSet<String> {
    let mut component = HashSet::from([root.to_string()]);
    let mut stack = vec![root.to_string()];
    while let Some(node) = stack.pop() {
        let Some(neighbors) = adjacency.get(&node) else {
            continue;
        };
        for neighbor in neighbors {
            if component.insert(neighbor.clone()) {
                stack.push(neighbor.clone());
            }
        }
    }
    component
}

fn complete_component_state_group(
    dae: &Dae,
    aggregate_name: &VarName,
    aggregate: &Variable,
    adjacency: &HashMap<String, HashSet<String>>,
    derivative_values: &IndexMap<VarName, Expression>,
) -> Option<Vec<(VarName, Expression)>> {
    let mut states = Vec::with_capacity(aggregate.size());
    let mut used_states = HashSet::new();
    let mut used_values = HashSet::new();
    for flat_index in 0..aggregate.size() {
        let component_name =
            dae::scalar_name_for_flat_index(aggregate_name, &aggregate.dims, flat_index);
        let component = alias_component(component_name.as_str(), adjacency);
        if !component
            .iter()
            .any(|member| exact_member_requests_dummy_state(dae, member))
        {
            return None;
        }
        let mut candidates = derivative_values
            .iter()
            .filter(|(state_name, _)| component.contains(state_name.as_str()));
        let (state_name, derivative_value) = candidates.next()?;
        if candidates.next().is_some()
            || !used_states.insert(state_name.as_str().to_string())
            || !used_values.insert(plain_reference_name(derivative_value)?.to_string())
        {
            return None;
        }
        states.push((state_name.clone(), derivative_value.clone()));
    }
    Some(states)
}

fn exact_member_requests_dummy_state(dae: &Dae, member: &str) -> bool {
    exact_continuous_variable(dae, member)
        .is_some_and(|variable| variable.state_select == rumoca_core::StateSelect::Never)
}

fn exact_continuous_variable<'a>(dae: &'a Dae, exact_name: &str) -> Option<&'a Variable> {
    let lookup = |name: &VarName| {
        dae.variables
            .states
            .get(name)
            .or_else(|| dae.variables.algebraics.get(name))
            .or_else(|| dae.variables.outputs.get(name))
            .or_else(|| dae.variables.inputs.get(name))
    };
    let exact = VarName::new(exact_name);
    lookup(&exact).or_else(|| {
        let scalar = rumoca_core::parse_scalar_name(exact_name)?;
        lookup(&VarName::new(scalar.base))
    })
}

fn plain_reference_name(expr: &Expression) -> Option<&str> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.as_str()),
        _ => None,
    }
}

fn contains_complete_aggregate_reference(
    expr: &Expression,
    target: &VarName,
    dims: &[i64],
) -> bool {
    contains_exact_reference(expr, target.as_str())
        || (0..dims.iter().product::<i64>() as usize).all(|flat_index| {
            let component = dae::scalar_name_for_flat_index(target, dims, flat_index);
            contains_exact_reference(expr, component.as_str())
        })
}

fn contains_exact_reference(expr: &Expression, target: &str) -> bool {
    struct Checker<'a> {
        target: &'a str,
        found: bool,
    }
    impl ExpressionVisitor for Checker<'_> {
        fn visit_expression(&mut self, expr: &Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
            let exact_name = if subscripts.is_empty() {
                Some(name.as_str().to_string())
            } else {
                crate::scalarize::scalarization_var_ref_name(name, subscripts)
            };
            if exact_name.as_deref() == Some(self.target) {
                self.found = true;
                return;
            }
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }
    }
    let mut checker = Checker {
        target,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

fn differentiated_group_is_closed(
    dae: &Dae,
    differentiated: &Expression,
    states: &[(VarName, Expression)],
) -> Result<bool, StructuralError> {
    if row_shape::residual_scalar_width(dae, differentiated)? != states.len()
        || states
            .iter()
            .any(|(state_name, _)| contains_exact_derivative(differentiated, state_name))
        || states.iter().any(|(_, derivative_value)| {
            plain_non_state_unknown(derivative_value, dae)
                .is_none_or(|name| !expr_contains_var(differentiated, &name))
        })
    {
        return Ok(false);
    }
    let future_states = dae
        .variables
        .states
        .keys()
        .filter(|name| !states.iter().any(|(state_name, _)| state_name == *name))
        .map(|name| name.as_str().to_string())
        .collect::<HashSet<_>>();
    Ok(!expr_contains_der_of_non_state(
        differentiated,
        &future_states,
    ))
}

fn contains_exact_derivative(expr: &Expression, target: &VarName) -> bool {
    struct Checker<'a> {
        target: &'a VarName,
        found: bool,
    }
    impl ExpressionVisitor for Checker<'_> {
        fn visit_expression(&mut self, expr: &Expression) {
            if matches!(
                expr,
                Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args,
                    ..
                } if args.len() == 1
                    && exact_scalar_reference(&args[0]).as_deref()
                        == Some(self.target.as_str())
            ) {
                self.found = true;
            } else if !self.found {
                self.walk_expression(expr);
            }
        }
    }
    let mut checker = Checker {
        target,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

fn substitute_exact_derivative(
    expr: &Expression,
    target: &VarName,
    replacement: &Expression,
) -> Expression {
    struct Rewriter<'a> {
        target: &'a VarName,
        replacement: &'a Expression,
    }
    impl ExpressionRewriter for Rewriter<'_> {
        fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
            if matches!(
                expr,
                Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args,
                    ..
                } if args.len() == 1
                    && exact_scalar_reference(&args[0]).as_deref()
                        == Some(self.target.as_str())
            ) {
                self.replacement.clone()
            } else {
                self.walk_expression(expr)
            }
        }
    }
    Rewriter {
        target,
        replacement,
    }
    .rewrite_expression(expr)
}

fn rewrite_exact_derivative_everywhere(dae: &mut Dae, target: &VarName, replacement: &Expression) {
    for equation in dae
        .continuous
        .equations
        .iter_mut()
        .chain(&mut dae.initialization.equations)
        .chain(&mut dae.discrete.real_updates)
        .chain(&mut dae.discrete.valued_updates)
        .chain(&mut dae.conditions.equations)
    {
        equation.rhs = substitute_exact_derivative(&equation.rhs, target, replacement);
    }
    for expression in dae
        .conditions
        .relations
        .iter_mut()
        .chain(&mut dae.events.synthetic_root_conditions)
        .chain(&mut dae.clocks.triggered_conditions)
        .chain(&mut dae.clocks.constructor_exprs)
    {
        *expression = substitute_exact_derivative(expression, target, replacement);
    }
    for action in &mut dae.events.event_actions {
        action.condition = substitute_exact_derivative(&action.condition, target, replacement);
        let message = match &mut action.kind {
            rumoca_ir_dae::DaeEventActionKind::Assert { message }
            | rumoca_ir_dae::DaeEventActionKind::Terminate { message } => message,
        };
        *message = substitute_exact_derivative(message, target, replacement);
    }
}

fn rewrite_derivative_value_everywhere(
    dae: &mut Dae,
    target: &VarName,
    replacement: &Expression,
) -> Result<(), StructuralError> {
    let component_replacements = derivative_component_replacements(dae, target, replacement)?;
    for (component, replacement_row) in &component_replacements {
        rewrite_exact_derivative_everywhere(dae, component, replacement_row);
    }
    rewrite_exact_derivative_everywhere(dae, target, replacement);
    Ok(())
}

fn substitute_derivative_value_in_expression(
    dae: &Dae,
    expression: &Expression,
    target: &VarName,
    replacement: &Expression,
) -> Result<Expression, StructuralError> {
    let mut rewritten = expression.clone();
    for (component, replacement_row) in derivative_component_replacements(dae, target, replacement)?
    {
        rewritten = substitute_exact_derivative(&rewritten, &component, &replacement_row);
    }
    Ok(substitute_exact_derivative(&rewritten, target, replacement))
}

fn derivative_component_replacements(
    dae: &Dae,
    target: &VarName,
    replacement: &Expression,
) -> Result<Vec<(VarName, Expression)>, StructuralError> {
    let Some(variable) = dae.variables.states.get(target) else {
        return Ok(Vec::new());
    };
    if variable.dims.is_empty() {
        return Ok(Vec::new());
    }
    let dims = variable.dims.clone();
    let size = variable.size();
    let scalarization = crate::scalarize::build_expression_scalarization_context(dae)?;
    let replacement_rows =
        crate::scalarize::scalarize_expression_rows(replacement, size, &scalarization)?;
    if replacement_rows.len() != size {
        return Err(StructuralError::UnspannedContractViolation {
            reason: format!(
                "derivative replacement for '{}' has {} rows, expected {}",
                target.as_str(),
                replacement_rows.len(),
                size
            ),
        });
    }
    Ok(replacement_rows
        .into_iter()
        .enumerate()
        .map(|(flat_index, replacement_row)| {
            (
                dae::scalar_name_for_flat_index(target, &dims, flat_index),
                replacement_row,
            )
        })
        .collect())
}

fn apply_complete_dummy_derivative_group(
    dae: &mut Dae,
    plan: DummyDerivativeGroupPlan,
) -> Result<usize, StructuralError> {
    crate::structural_trace!(
        "[sim-trace] complete dummy-derivative group aggregate={} states={:?}",
        plan.aggregate_name.as_str(),
        plan.states
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
    );
    let mut staged = dae.clone();
    let original_constraint = staged.continuous.equations[plan.constraint_index].clone();
    let differentiated_origin = if original_constraint.origin.is_empty() {
        "index_reduction:d_dt_complete_dummy_derivative_group".to_string()
    } else {
        format!(
            "{}|index_reduction:d_dt_complete_dummy_derivative_group",
            original_constraint.origin
        )
    };
    staged.continuous.equations.push(Equation {
        lhs: None,
        rhs: plan.differentiated_constraint,
        span: original_constraint.span,
        origin: differentiated_origin,
        scalar_count: original_constraint.scalar_count,
    });

    for (state_name, derivative_value) in &plan.states {
        rewrite_exact_derivative_everywhere(&mut staged, state_name, derivative_value);
    }
    if staged.continuous.equations.iter().any(|equation| {
        plan.states
            .iter()
            .any(|(state_name, _)| contains_exact_derivative(&equation.rhs, state_name))
    }) {
        return Ok(0);
    }
    for (state_name, _) in &plan.states {
        let Some(variable) = staged.variables.states.shift_remove(state_name) else {
            return Ok(0);
        };
        staged
            .variables
            .algebraics
            .insert(state_name.clone(), variable);
    }
    let demoted = plan.states.len();
    *dae = staged;
    Ok(demoted)
}
