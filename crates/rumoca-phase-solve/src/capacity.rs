use super::*;

pub(crate) fn lower_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_lower_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

pub(crate) fn lower_vec_with_optional_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_lower_optional_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

pub(crate) fn reserve_lower_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        lower_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

pub(crate) fn reserve_lower_optional_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        lower_optional_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

pub(crate) fn lower_contract_violation(reason: String, span: rumoca_core::Span) -> LowerError {
    if span.is_dummy() {
        LowerError::UnspannedContractViolation { reason }
    } else {
        LowerError::ContractViolation { reason, span }
    }
}

pub(crate) fn lower_optional_contract_violation(
    reason: String,
    span: Option<rumoca_core::Span>,
) -> LowerError {
    match span.filter(|span| !span.is_dummy()) {
        Some(span) => LowerError::ContractViolation { reason, span },
        None => LowerError::UnspannedContractViolation { reason },
    }
}

pub(crate) fn lower_hash_set_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<HashSet<T>, LowerError>
where
    T: std::hash::Hash + Eq,
{
    let mut values = HashSet::new();
    values.try_reserve(capacity).map_err(|_| {
        lower_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

pub(crate) fn lower_bool_slice_copy(
    values: &[bool],
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<bool>, LowerError> {
    let mut copied = lower_vec_with_capacity(values.len(), context, span)?;
    copied.extend(values.iter().copied());
    Ok(copied)
}

pub(crate) fn reserve_lower_index_map_capacity<K, V>(
    values: &mut IndexMap<K, V>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError>
where
    K: std::hash::Hash + Eq,
{
    values.try_reserve(additional).map_err(|_| {
        lower_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

pub(crate) fn reserve_lower_hash_set_capacity<T>(
    values: &mut HashSet<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError>
where
    T: std::hash::Hash + Eq,
{
    values.try_reserve(additional).map_err(|_| {
        lower_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

pub(crate) fn dae_model_span(dae_model: &dae::Dae) -> Result<rumoca_core::Span, LowerError> {
    dae_model_variable_span(dae_model)
        .or_else(|| dae_model_equation_span(dae_model))
        .or_else(|| dae_model_expression_span(dae_model))
        .or_else(|| dae_model_event_action_span(dae_model))
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: "DAE model has no source provenance".to_string(),
        })
}

fn dae_model_variable_span(dae_model: &dae::Dae) -> Option<rumoca_core::Span> {
    dae_model
        .variables
        .states
        .values()
        .chain(dae_model.variables.algebraics.values())
        .chain(dae_model.variables.outputs.values())
        .chain(dae_model.variables.inputs.values())
        .chain(dae_model.variables.discrete_reals.values())
        .chain(dae_model.variables.discrete_valued.values())
        .chain(dae_model.variables.parameters.values())
        .chain(dae_model.variables.constants.values())
        .find_map(|var| (!var.source_span.is_dummy()).then_some(var.source_span))
}

fn dae_model_equation_span(dae_model: &dae::Dae) -> Option<rumoca_core::Span> {
    dae_model
        .continuous
        .equations
        .iter()
        .chain(dae_model.initialization.equations.iter())
        .chain(dae_model.discrete.real_updates.iter())
        .chain(dae_model.discrete.valued_updates.iter())
        .chain(dae_model.conditions.equations.iter())
        .find_map(|equation| (!equation.span.is_dummy()).then_some(equation.span))
}

fn dae_model_expression_span(dae_model: &dae::Dae) -> Option<rumoca_core::Span> {
    dae_model
        .conditions
        .relations
        .iter()
        .chain(dae_model.events.synthetic_root_conditions.iter())
        .chain(dae_model.clocks.constructor_exprs.iter())
        .chain(dae_model.clocks.triggered_conditions.iter())
        .find_map(|expression| expression.span().filter(|span| !span.is_dummy()))
}

fn dae_model_event_action_span(dae_model: &dae::Dae) -> Option<rumoca_core::Span> {
    dae_model
        .events
        .event_actions
        .iter()
        .find_map(|action| (!action.span.is_dummy()).then_some(action.span))
}

pub(crate) fn dae_model_has_no_solve_lowering_inputs(dae_model: &dae::Dae) -> bool {
    dae_model.variables.states.is_empty()
        && dae_model.variables.algebraics.is_empty()
        && dae_model.variables.outputs.is_empty()
        && dae_model.variables.inputs.is_empty()
        && dae_model.variables.discrete_reals.is_empty()
        && dae_model.variables.discrete_valued.is_empty()
        && dae_model.variables.parameters.is_empty()
        && dae_model.variables.constants.is_empty()
        && dae_model.continuous.equations.is_empty()
        && dae_model.initialization.equations.is_empty()
        && dae_model.discrete.real_updates.is_empty()
        && dae_model.discrete.valued_updates.is_empty()
        && dae_model.conditions.equations.is_empty()
        && dae_model.conditions.relations.is_empty()
        && dae_model.events.synthetic_root_conditions.is_empty()
        && dae_model.events.event_actions.is_empty()
        && dae_model.clocks.constructor_exprs.is_empty()
        && dae_model.clocks.triggered_conditions.is_empty()
}

pub fn lower_solve_layout(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Result<solve::SolveLayout, LowerError> {
    let layout = build_var_layout_with_solver_len(dae_model, solver_len)?;
    lower_solve_layout_with_var_layout(dae_model, solver_len, &layout)
}
