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

/// One differentiated holonomic constraint group, ready to commit.
pub(super) struct HolonomicDifferentiation {
    /// Retained state reached by an exact ODE successor (`q' = v`), when the
    /// derivative value is a plain reference to one. Prolongation continues
    /// from it.
    derivative_name: Option<VarName>,
    /// Value that replaces every `der(state)` reference once the state is
    /// demoted.
    derivative_value: Expression,
    /// Time derivative of each constraint row in the group.
    differentiated: Vec<Expression>,
    /// Generated dummy-derivative unknown the caller must declare before the
    /// demotion is committed. `None` when the derivative value is built from
    /// variables that already exist.
    dummy_derivative: Option<VarName>,
}

/// Derivative value for one state plus the dummy unknown it introduces.
struct HolonomicDerivativeValue {
    value: Expression,
    dummy: Option<VarName>,
}

struct ConstraintDifferentiationCtx<'a> {
    dae: &'a Dae,
    current_state: &'a VarName,
    defining_expr_index: &'a DefiningExprIndex,
    structural_bindings: &'a HashMap<String, f64>,
    selected_derivatives: &'a HashMap<String, Expression>,
    derivative_value: &'a Expression,
    future_states: &'a HashSet<String>,
}

/// The inputs one holonomic constraint group is differentiated against, kept
/// together so the resolution and differentiation halves can be separate
/// functions without threading the same six borrows through both.
struct HolonomicGroupRequest<'a> {
    current_state: &'a VarName,
    constraint_indices: &'a [usize],
    defining_expr_index: &'a DefiningExprIndex,
    structural_bindings: &'a HashMap<String, f64>,
    selected_derivatives: &'a HashMap<String, Expression>,
}

struct DerivativeResolutionCtx<'a> {
    dae: &'a Dae,
    current_state: &'a VarName,
    constraint_indices: &'a [usize],
    defining_expr_index: &'a DefiningExprIndex,
    structural_bindings: &'a HashMap<String, f64>,
    selected_derivatives: &'a HashMap<String, Expression>,
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
    extend_with_indirect_constraint_candidates(
        dae,
        &defining_expr_index,
        &structural_bindings,
        &chain_successors,
        retained_derivative_values,
        &mut candidates,
    )?;
    Ok(candidates)
}

/// Add candidates for constraint rows that name no state at all.
///
/// Index-3 kinematic constraints (multibody cut joints, rolling constraints,
/// the MLS 9.4 orientation residual) are written against intermediate algebraic
/// quantities, so the textual scan above never pairs them with the states they
/// constrain. See [`super::indirect_constraint_seed`].
fn extend_with_indirect_constraint_candidates(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    structural_bindings: &HashMap<String, f64>,
    chain_successors: &HashSet<VarName>,
    retained_derivative_values: &HashMap<String, Expression>,
    candidates: &mut Vec<SingularHolonomicCandidate>,
) -> Result<(), StructuralError> {
    let demoted_derivatives = demoted_state_derivative_values(dae, retained_derivative_values);
    let seeds = super::indirect_constraint_seed::indirect_constraint_seeds(
        dae,
        defining_expr_index,
        structural_bindings,
        &demoted_derivatives,
    )?;
    for seed in seeds {
        if chain_successors.contains(&seed.state_name) {
            continue;
        }
        candidates.extend(build_singular_holonomic_chain_prefixes(
            dae,
            &seed.state_name,
            &[seed.equation_index],
            defining_expr_index,
            structural_bindings,
            retained_derivative_values,
        )?);
    }
    Ok(())
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
    let state_size = crate::variable_scope::scalar_count_from_dims(state_name, &state.dims)?;
    let mut eligible = Vec::new();
    for (index, equation) in dae.continuous.equations.iter().enumerate() {
        if expression_contains_state_component(&equation.rhs, state_name, &state.dims)?
            && !expression_contains_any_der_call(&equation.rhs)
            && state_row_reduction::expression_is_smooth_for_index_reduction(
                &equation.rhs,
                dae,
                structural_bindings,
            )
        {
            eligible.push(index);
        }
    }
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
            state_size,
        )? {
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
) -> Result<Option<Vec<usize>>, StructuralError> {
    let mut group = Vec::new();
    let mut scalar_width = 0usize;
    let origin = &dae.continuous.equations[eligible[start]].origin;
    for index in eligible.iter().copied().skip(start) {
        if group.last().is_some_and(|previous| index != previous + 1)
            || dae.continuous.equations[index].origin != *origin
        {
            return Ok(None);
        }
        scalar_width = scalar_width
            .checked_add(dae.continuous.equations[index].scalar_count)
            .ok_or_else(|| {
                holonomic_group_width_overflow(state_name, dae.continuous.equations[index].span)
            })?;
        group.push(index);
        if scalar_width == state_size {
            return Ok(constraint_group_contains_complete_state(
                dae, &group, state_name, state_dims,
            )?
            .then_some(group));
        }
        if scalar_width > state_size {
            return Ok(None);
        }
    }
    Ok(None)
}

fn holonomic_group_width_overflow(
    state_name: &VarName,
    span: rumoca_core::Span,
) -> StructuralError {
    let reason = format!(
        "holonomic constraint group scalar width overflows for `{}`",
        state_name.as_str()
    );
    if span.is_dummy() {
        StructuralError::UnspannedContractViolation { reason }
    } else {
        StructuralError::ContractViolation { reason, span }
    }
}

fn expression_contains_state_component(
    expression: &Expression,
    state_name: &VarName,
    dims: &[i64],
) -> Result<bool, StructuralError> {
    let scalar_count = crate::variable_scope::scalar_count_from_dims(state_name, dims)?;
    Ok(contains_exact_reference(expression, state_name.as_str())
        || (0..scalar_count).any(|flat_index| {
            let component = dae::scalar_name_for_flat_index(state_name, dims, flat_index);
            contains_exact_reference(expression, component.as_str())
        }))
}

fn constraint_group_contains_complete_state(
    dae: &Dae,
    group: &[usize],
    state_name: &VarName,
    dims: &[i64],
) -> Result<bool, StructuralError> {
    let scalar_count = crate::variable_scope::scalar_count_from_dims(state_name, dims)?;
    let expressions = group
        .iter()
        .map(|index| &dae.continuous.equations[*index].rhs)
        .collect::<Vec<_>>();
    Ok(expressions
        .iter()
        .any(|expr| contains_exact_reference(expr, state_name.as_str()))
        || (0..scalar_count).all(|flat_index| {
            let component = dae::scalar_name_for_flat_index(state_name, dims, flat_index);
            expressions
                .iter()
                .any(|expr| contains_exact_reference(expr, component.as_str()))
        }))
}

/// Known time-derivative values of states an earlier round already demoted.
///
/// Committing a dummy-derivative demotion rewrites every `der(x)` to the chosen
/// value and drops the now-trivial `der(x) = v` row, so the DAE no longer
/// records that `x` differentiates to `v`. The value survives in the retained
/// map the caller carries across rounds. Without replaying it, the next round's
/// derivative closure blocks on the first algebraic that reads `x` — a
/// multibody orientation matrix, in practice — and no further constraint in the
/// same loop can be reduced.
fn demoted_state_derivative_values(
    dae: &Dae,
    retained_derivative_values: &HashMap<String, Expression>,
) -> HashMap<String, Expression> {
    let states = dae
        .variables
        .states
        .keys()
        .map(|name| name.as_str())
        .collect::<HashSet<_>>();
    retained_derivative_values
        .iter()
        .filter(|(name, _)| !states.contains(name.as_str()))
        .map(|(name, value)| (name.clone(), value.clone()))
        .collect()
}

fn build_singular_holonomic_chain_prefixes(
    dae: &Dae,
    seed_state: &VarName,
    constraint_indices: &[usize],
    defining_expr_index: &DefiningExprIndex,
    structural_bindings: &HashMap<String, f64>,
    retained_derivative_values: &HashMap<String, Expression>,
) -> Result<Vec<SingularHolonomicCandidate>, StructuralError> {
    let mut staged = super::copy_accounting::clone_dae(dae);
    let mut current_state = seed_state.clone();
    let mut current_constraint_indices = constraint_indices.to_vec();
    let mut selected_derivatives = demoted_state_derivative_values(dae, retained_derivative_values);
    let mut demoted_states = Vec::new();
    let mut candidates = Vec::new();
    let constraint_index = constraint_indices[0];

    while let Some(variable) = staged.variables.states.get(&current_state) {
        if variable.state_select == rumoca_core::StateSelect::Always
            || state_has_overlapping_event_update(&staged, &current_state)
        {
            break;
        }
        let Some(differentiation) = differentiate_holonomic_constraint_group(
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
        let HolonomicDifferentiation {
            derivative_name,
            derivative_value,
            differentiated,
            dummy_derivative,
        } = differentiation;
        if let Some(dummy_name) = &dummy_derivative {
            declare_dummy_derivative(&mut staged, &current_state, dummy_name)?;
        }
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
            dae: super::copy_accounting::clone_dae(&staged),
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
    let request = HolonomicGroupRequest {
        current_state,
        constraint_indices,
        defining_expr_index,
        structural_bindings,
        selected_derivatives,
    };
    let Some(resolved) =
        resolve_holonomic_derivative_value(dae, &request, retained_derivative_values)?
    else {
        crate::structural_trace!(
            "[sim-trace] holonomic group rejected state={} reason=missing_derivative_value",
            current_state.as_str()
        );
        return Ok(None);
    };
    let Some(dummy_name) = resolved.dummy.clone() else {
        return differentiate_resolved_group(dae, &request, resolved);
    };
    // Row-shape analysis sizes every reference against the variable partitions,
    // so the generated unknown must exist before the differentiated rows that
    // read it are measured. Stage a copy that declares it; the caller declares
    // it for real only if this plan is returned.
    let mut staged = super::copy_accounting::clone_dae(dae);
    declare_dummy_derivative(&mut staged, current_state, &dummy_name)?;
    differentiate_resolved_group(&staged, &request, resolved)
}

fn differentiate_resolved_group(
    dae: &Dae,
    request: &HolonomicGroupRequest<'_>,
    resolved: HolonomicDerivativeValue,
) -> Result<Option<HolonomicDifferentiation>, StructuralError> {
    let HolonomicDerivativeValue {
        value: derivative_value,
        dummy: dummy_derivative,
    } = resolved;
    let current_state = request.current_state;
    let constraint_indices = request.constraint_indices;
    let defining_expr_index = request.defining_expr_index;
    let structural_bindings = request.structural_bindings;
    let selected_derivatives = request.selected_derivatives;
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
    // A generated dummy derivative is a brand-new unknown: the differentiated
    // constraint group is the only row that can determine it. Reject the plan
    // when differentiation dropped it, or the demotion would leave the system
    // one equation short of its unknowns.
    let target_name = derivative_name.as_ref().or(dummy_derivative.as_ref());
    if target_name.is_some_and(|name| {
        !differentiated
            .iter()
            .any(|expression| expr_contains_var(expression, name))
    }) {
        crate::structural_trace!(
            "[sim-trace] holonomic group rejected state={} derivative={} reason=derivative_target_absent",
            current_state.as_str(),
            target_name.map_or("<expression>", VarName::as_str)
        );
        return Ok(None);
    }
    Ok(Some(HolonomicDifferentiation {
        derivative_name,
        derivative_value,
        differentiated,
        dummy_derivative,
    }))
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
            excluded_equations: &[constraint_index],
            selected_derivatives: Some(context.selected_derivatives),
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
    request: &HolonomicGroupRequest<'_>,
    retained_derivative_values: &HashMap<String, Expression>,
) -> Result<Option<HolonomicDerivativeValue>, StructuralError> {
    let current_state = request.current_state;
    if let Some(value) =
        direct_holonomic_derivative_value(dae, current_state, retained_derivative_values)
    {
        return Ok(Some(HolonomicDerivativeValue { value, dummy: None }));
    }
    let context = DerivativeResolutionCtx {
        dae,
        current_state,
        constraint_indices: request.constraint_indices,
        defining_expr_index: request.defining_expr_index,
        structural_bindings: request.structural_bindings,
        selected_derivatives: request.selected_derivatives,
    };
    if let Some(value) = resolve_derivative_from_definitions(&context)? {
        return Ok(Some(HolonomicDerivativeValue { value, dummy: None }));
    }
    if let Some(value) = coupled_dummy_derivative_value(dae, current_state) {
        return Ok(Some(value));
    }
    trace_unavailable_holonomic_derivative(&context);
    Ok(None)
}

/// Namespace of the generated dummy-derivative unknowns introduced by
/// [`coupled_dummy_derivative_value`].
const DUMMY_DERIVATIVE_NAMESPACE: &str = "__dummyder__";

/// Rendered name of the dummy derivative that stands for `der(state_name)`.
pub(super) fn dummy_derivative_name(state_name: &VarName) -> VarName {
    VarName::new(format!(
        "{DUMMY_DERIVATIVE_NAMESPACE}.{}",
        state_name.as_str()
    ))
}

/// Time-derivative values already supplied by generated dummy derivatives.
///
/// `__dummyder__.<x>` is, by construction, the value of `der(x)` for a state an
/// earlier index-reduction round demoted: the differentiated constraint
/// appended alongside it is its defining row. A later round that differentiates
/// a constraint reading `x` needs that value, and without it the derivative
/// closure rediscovers it the only other way it can — by differentiating `x`'s
/// own defining constraint all over again, rebuilding the whole tree inside the
/// next round's tree, and the next round's inside the one after that. Along a
/// constraint chain that compounds. The dummy is the same value as a single
/// leaf, so resolving through it keeps each differentiated constraint the size
/// of one constraint.
pub(super) fn generated_dummy_derivative_values(dae: &Dae) -> HashMap<String, Expression> {
    let prefix = format!("{DUMMY_DERIVATIVE_NAMESPACE}.");
    dae.variables
        .algebraics
        .iter()
        .filter_map(|(name, variable)| {
            let state = name.as_str().strip_prefix(prefix.as_str())?;
            Some((
                state.to_string(),
                Expression::VarRef {
                    name: Reference::new(name.as_str()),
                    subscripts: Vec::new(),
                    span: variable.source_span,
                },
            ))
        })
        .collect()
}

/// True when any partition already owns `name`, so a generated name would
/// shadow a real variable.
pub(super) fn variable_name_is_taken(dae: &Dae, name: &VarName) -> bool {
    let variables = &dae.variables;
    variables.states.contains_key(name)
        || variables.algebraics.contains_key(name)
        || variables.outputs.contains_key(name)
        || variables.inputs.contains_key(name)
        || variables.parameters.contains_key(name)
        || variables.constants.contains_key(name)
        || variables.discrete_reals.contains_key(name)
        || variables.discrete_valued.contains_key(name)
}

/// True when `der(state_name)` is read outside the continuous residual rows.
///
/// Discrete updates, event conditions and clock expressions are not part of the
/// continuous system the differentiated constraint balances, so a derivative
/// read from one of them must keep its state rather than become a dummy.
pub(super) fn state_derivative_escapes_continuous_rows(dae: &Dae, state_name: &VarName) -> bool {
    dae.discrete
        .real_updates
        .iter()
        .chain(&dae.discrete.valued_updates)
        .chain(&dae.conditions.equations)
        .any(|equation| contains_exact_derivative(&equation.rhs, state_name))
        || dae
            .conditions
            .relations
            .iter()
            .chain(&dae.events.synthetic_root_conditions)
            .chain(&dae.clocks.triggered_conditions)
            .chain(&dae.clocks.constructor_exprs)
            .any(|expression| contains_exact_derivative(expression, state_name))
        || dae
            .events
            .event_actions
            .iter()
            .any(|action| contains_exact_derivative(&action.condition, state_name))
}

/// Introduce a Mattsson-Söderlind dummy derivative for a state whose derivative
/// only ever appears in rows that couple several state derivatives at once.
///
/// MLS 3.7 leaves the choice of index-reduction method to the tool but requires
/// the reduced system to have the same solution manifold. Pantelides needs one
/// differentiated constraint per dependent state; the dummy-derivative method
/// supplies it by demoting the state to an algebraic determined by the original
/// constraint `g = 0`, and turning `der(state)` into a plain algebraic unknown
/// determined by the differentiated constraint `dg/dt = 0`. Both constraints are
/// retained, so the reduced system stays on the original manifold instead of
/// drifting off it.
///
/// The other resolution strategies solve some row for `der(state)` and
/// substitute the result. Neither can act on the linear-implicit (mass-matrix)
/// form the fundamental-wave converter writes,
/// `-v = N.re*der(Phi.re) + N.im*der(Phi.im)`: no row isolates either
/// derivative, so the coupled group loses one differentiated constraint per
/// dependent state and the system stays structurally singular.
///
/// The construction is deliberately restricted to that shape. A scalar state
/// whose derivative is read only from continuous rows that each mention a second
/// retained state derivative cannot have its derivative solved for by any
/// substitution, so introducing the unknown cannot displace a strategy that
/// would otherwise have succeeded. Rows that isolate the derivative keep the
/// substituting strategies, which add no unknown.
fn coupled_dummy_derivative_value(
    dae: &Dae,
    current_state: &VarName,
) -> Option<HolonomicDerivativeValue> {
    let state = dae.variables.states.get(current_state)?;
    if !state.dims.is_empty() || state_derivative_escapes_continuous_rows(dae, current_state) {
        return None;
    }
    let state_names = dae.variables.states.keys().cloned().collect::<Vec<_>>();
    let derivative_rows = dae
        .continuous
        .equations
        .iter()
        .filter(|equation| contains_exact_derivative(&equation.rhs, current_state))
        .collect::<Vec<_>>();
    if derivative_rows.is_empty() {
        return None;
    }
    let every_row_is_coupled = derivative_rows.iter().all(|equation| {
        super::derivative_states_in_eq(&equation.rhs, &state_names)
            .iter()
            .any(|name| name != current_state)
    });
    if !every_row_is_coupled {
        return None;
    }
    let dummy_name = dummy_derivative_name(current_state);
    if variable_name_is_taken(dae, &dummy_name) {
        return None;
    }
    crate::structural_trace!(
        "[sim-trace] holonomic derivative dummy state={} unknown={} coupled_rows={}",
        current_state.as_str(),
        dummy_name.as_str(),
        derivative_rows.len()
    );
    Some(HolonomicDerivativeValue {
        value: Expression::VarRef {
            name: Reference::new(dummy_name.as_str()),
            subscripts: Vec::new(),
            span: state.source_span,
        },
        dummy: Some(dummy_name),
    })
}

/// Declare the generated dummy-derivative unknown for `state_name`.
///
/// The variable is a plain continuous algebraic: the differentiated constraint
/// appended alongside it is its defining row. It carries no start value or unit
/// because it is a time derivative of the demoted state, not a copy of it.
///
/// MLS 10.1 makes array dimensions part of a variable's type, and the
/// derivative of an array-valued state has that state's shape, so the generated
/// unknown copies `dims`. Row-shape analysis sizes every reference against the
/// variable partitions, so a shapeless unknown standing for `der(v[3])` would
/// measure one scalar where the differentiated row supplies three.
pub(super) fn declare_dummy_derivative(
    dae: &mut Dae,
    state_name: &VarName,
    dummy_name: &VarName,
) -> Result<(), StructuralError> {
    let Some(state) = dae.variables.states.get(state_name) else {
        return Err(StructuralError::UnspannedContractViolation {
            reason: format!(
                "dummy derivative `{}` requested for `{}`, which is no longer a state",
                dummy_name.as_str(),
                state_name.as_str()
            ),
        });
    };
    let mut variable = Variable::empty_with_span(state.source_span);
    variable.name = dummy_name.clone();
    variable.dims = state.dims.clone();
    variable.description = Some(format!(
        "dummy derivative of {} (index reduction)",
        state_name.as_str()
    ));
    variable.origin = dae::VariableOrigin::Generated;
    dae.variables
        .algebraics
        .insert(dummy_name.clone(), variable);
    Ok(())
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
                excluded_equations: &[definition.equation_index],
                selected_derivatives: Some(context.selected_derivatives),
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
    Ok(
        find_complete_dummy_derivative_group(dae)?.map_or_else(IndexSet::new, |plan| {
            plan.states
                .into_iter()
                .map(|(state_name, _)| state_name)
                .collect()
        }),
    )
}

fn find_complete_dummy_derivative_group(
    dae: &Dae,
) -> Result<Option<DummyDerivativeGroupPlan>, StructuralError> {
    let candidates = complete_dummy_derivative_group_candidates(dae)?;
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
                excluded_equations: &[candidate.constraint_index],
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

fn complete_dummy_derivative_group_candidates(
    dae: &Dae,
) -> Result<Vec<DummyDerivativeGroupCandidate>, StructuralError> {
    let derivative_values = dummy_derivative_values(dae);
    if derivative_values.is_empty() {
        return Ok(Vec::new());
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
        let aggregate_size =
            crate::variable_scope::scalar_count_from_dims(aggregate_name, &aggregate.dims)?;
        if aggregate.dims.len() != 1 || aggregate_size < 2 {
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
            if constraint.scalar_count == aggregate_size
                && !is_connection_equation_origin(&constraint.origin)
                && precise_alias_pair(&constraint.rhs).is_none()
                && !expression_contains_any_der_call(&constraint.rhs)
                && contains_complete_aggregate_reference(
                    &constraint.rhs,
                    aggregate_name,
                    &aggregate.dims,
                )?
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
    Ok(candidates)
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
) -> Result<bool, StructuralError> {
    let scalar_count = crate::variable_scope::scalar_count_from_dims(target, dims)?;
    Ok(contains_exact_reference(expr, target.as_str())
        || (0..scalar_count).all(|flat_index| {
            let component = dae::scalar_name_for_flat_index(target, dims, flat_index);
            contains_exact_reference(expr, component.as_str())
        }))
}

pub(super) fn contains_exact_reference(expr: &Expression, target: &str) -> bool {
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
    let state_names = plan
        .states
        .iter()
        .map(|(state_name, _)| state_name.clone())
        .collect::<Vec<_>>();
    let scalar_size = state_names.iter().try_fold(0usize, |total, state_name| {
        let state = dae.variables.states.get(state_name)?;
        total.checked_add(state.size())
    });
    let Some(scalar_size) = scalar_size else {
        return Ok(0);
    };
    let dummy_names = state_names
        .iter()
        .map(dummy_derivative_name)
        .collect::<Vec<_>>();
    if dummy_names
        .iter()
        .any(|dummy_name| variable_name_is_taken(dae, dummy_name))
    {
        return Ok(0);
    }

    let mut staged = super::copy_accounting::clone_dae(dae);
    for (state_name, dummy_name) in state_names.iter().zip(&dummy_names) {
        declare_dummy_derivative(&mut staged, state_name, dummy_name)?;
    }
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

    for (state_name, dummy_name) in state_names.iter().zip(&dummy_names) {
        let state_span = staged.variables.states[state_name].source_span;
        let dummy_reference = Expression::VarRef {
            name: Reference::new(dummy_name.as_str()),
            subscripts: Vec::new(),
            span: state_span,
        };
        rewrite_exact_derivative_everywhere(&mut staged, state_name, &dummy_reference);
    }
    if staged.continuous.equations.iter().any(|equation| {
        plan.states
            .iter()
            .any(|(state_name, _)| contains_exact_derivative(&equation.rhs, state_name))
    }) {
        return Ok(0);
    }
    for state_name in &state_names {
        let Some(variable) = staged.variables.states.shift_remove(state_name) else {
            return Ok(0);
        };
        staged
            .variables
            .algebraics
            .insert(state_name.clone(), variable);
    }
    super::constrained_dummy_derivative::verify_group_demotion_preserves_balance(
        dae,
        &staged,
        &state_names,
        scalar_size,
        original_constraint.span,
    )?;
    let demoted = state_names.len();
    *dae = staged;
    Ok(demoted)
}
