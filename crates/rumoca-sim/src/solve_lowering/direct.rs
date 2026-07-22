//! Guarded direct DAE -> Solve IR lowering for models that are already in an
//! explicit runtime shape. Anything outside this narrow proof falls back to the
//! structural Modelica path in [`super::entry`].

use std::collections::{BTreeSet, HashMap};

use rumoca_core::{Expression, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::{SimOptions, SimSolverMode};

use super::structural_lowering::metadata_attachment_lower_error;

pub(super) fn lower_direct_dae_for_simulation(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    param_overrides: &HashMap<String, f64>,
) -> Result<Option<solve::SolveModel>, rumoca_phase_solve::SolveModelLowerError> {
    if opts.solver_mode != SimSolverMode::RkLike {
        return Ok(None);
    }

    let metadata_dae = attach_reference_metadata(dae_model)?;
    if let Some(reason) = projected_slot_rejection(&metadata_dae) {
        trace_direct_rejection(reason);
        return Ok(None);
    }
    if let Some(reason) = direct_state_value_assignment_rejection(&metadata_dae) {
        trace_direct_rejection(reason);
        return Ok(None);
    }
    let visible_expressions = match rumoca_phase_solve::visible_expressions_for_dae(&metadata_dae) {
        Ok(expressions) => expressions,
        Err(err) => {
            trace_direct_rejection(format!("visible expression lowering failed: {err}"));
            return Ok(None);
        }
    };
    let lowered = metadata_dae.clone();
    let solve_model =
        match rumoca_phase_solve::lower_dae_to_solve_model_owned_value_only_with_visible_expressions_and_metadata_and_overrides(
            lowered,
            visible_expressions,
            &metadata_dae,
            param_overrides,
        ) {
            Ok(model) => model,
            Err(err) => {
                trace_direct_rejection(format!("direct solve lowering failed: {err}"));
                return Ok(None);
            }
        };

    match validate_direct_runtime_model(&solve_model) {
        Ok(()) => Ok(Some(solve_model)),
        Err(reason) => {
            trace_direct_rejection(reason);
            Ok(None)
        }
    }
}

pub(super) fn lower_direct_dae_for_gpu_preparation(
    dae_model: &dae::Dae,
) -> Result<solve::SolveModel, rumoca_phase_solve::SolveModelLowerError> {
    validate_gpu_dae_admission(dae_model)?;
    let metadata_dae = attach_reference_metadata(dae_model)?;
    let lowered = metadata_dae.clone();
    let solve_model =
        rumoca_phase_solve::lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata(
            lowered,
            &metadata_dae,
        )?;
    Ok(solve_model)
}

/// GPU preparation has no event/discrete runtime payload.  Reject these DAE
/// forms before the direct path attempts lowering; falling through to the
/// structural path would otherwise hide a semantic admission failure.
fn validate_gpu_dae_admission(
    dae_model: &dae::Dae,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    let rejection = |reason: &str, span: Option<rumoca_core::Span>| {
        let Some(span) = span else {
            return rumoca_phase_solve::SolveModelLowerError::Lower(
                rumoca_phase_solve::LowerError::Unsupported {
                    reason: format!("GPU preparation rejects {reason} without source provenance"),
                },
            );
        };
        rumoca_phase_solve::SolveModelLowerError::Lower(
            rumoca_phase_solve::LowerError::UnsupportedAt {
                reason: format!("GPU preparation rejects {reason}"),
                contexts: vec!["GPU DAE admission".to_string()],
                span,
            },
        )
    };
    if let Some(equation) = dae_model.discrete.real_updates.first() {
        return Err(rejection("discrete real updates", Some(equation.span)));
    }
    if let Some(equation) = dae_model.discrete.valued_updates.first() {
        return Err(rejection("discrete-valued updates", Some(equation.span)));
    }
    if let Some(variable) = dae_model.variables.discrete_reals.values().next() {
        return Err(rejection(
            "discrete Real variables",
            Some(variable.source_span),
        ));
    }
    if let Some(variable) = dae_model.variables.discrete_valued.values().next() {
        return Err(rejection(
            "discrete-valued variables",
            Some(variable.source_span),
        ));
    }
    if let Some(equation) = dae_model.conditions.equations.first() {
        return Err(rejection("condition equations", Some(equation.span)));
    }
    if let Some(relation) = dae_model.conditions.relations.first() {
        return Err(rejection("relation memory", relation.span()));
    }
    if let Some(expression) = dae_model.events.synthetic_root_conditions.first() {
        return Err(rejection("root conditions", expression.span()));
    }
    if let Some(event) = dae_model.events.scheduled_time_events.first() {
        return Err(rejection("scheduled time events", event.source_span));
    }
    if let Some(event) = dae_model.events.scheduled_root_conditions.first() {
        let span = dae_model
            .conditions
            .relations
            .get(event.root_index)
            .and_then(rumoca_core::Expression::span);
        return Err(rejection("scheduled root conditions", span));
    }
    if let Some(action) = dae_model.events.event_actions.first() {
        return Err(rejection("event actions", Some(action.span)));
    }
    if let Some(schedule) = dae_model.clocks.schedules.first() {
        return Err(rejection("clock schedules", Some(schedule.source_span)));
    }
    if let Some(expression) = dae_model.clocks.constructor_exprs.first() {
        return Err(rejection("clock constructors", expression.span()));
    }
    if let Some(expression) = dae_model.clocks.triggered_conditions.first() {
        return Err(rejection("triggered clock conditions", expression.span()));
    }
    if let Some(variable) = dae_model
        .variables
        .parameters
        .values()
        .find(|variable| rumoca_core::pre_slot_base(variable.name.as_str()).is_some())
    {
        return Err(rejection("pre-state memory", Some(variable.source_span)));
    }
    if let Some(equation) = dae_model.initialization.equations.iter().find(|equation| {
        equation.lhs.as_ref().is_some_and(|lhs| {
            let name = lhs.var_name();
            dae_model.variables.parameters.contains_key(name)
                || dae_model.variables.inputs.contains_key(name)
                || dae_model.variables.discrete_reals.contains_key(name)
                || dae_model.variables.discrete_valued.contains_key(name)
        })
    }) {
        return Err(rejection("initial P-slot target", Some(equation.span)));
    }
    Ok(())
}

fn attach_reference_metadata(
    dae_model: &dae::Dae,
) -> Result<dae::Dae, rumoca_phase_solve::SolveModelLowerError> {
    let mut metadata_dae = dae_model.clone();
    rumoca_phase_dae::attach_dae_reference_metadata(&mut metadata_dae)
        .map_err(metadata_attachment_lower_error)?;
    Ok(metadata_dae)
}

fn projected_slot_rejection(dae_model: &dae::Dae) -> Option<String> {
    if !dae_model.variables.algebraics.is_empty() {
        return Some(format!(
            "model has {} algebraic variables",
            dae_model.variables.algebraics.len()
        ));
    }
    if !dae_model.variables.outputs.is_empty() {
        return Some(format!(
            "model has {} output variables",
            dae_model.variables.outputs.len()
        ));
    }
    None
}

fn direct_state_value_assignment_rejection(dae_model: &dae::Dae) -> Option<String> {
    let state_names = dae_model
        .variables
        .states
        .keys()
        .cloned()
        .collect::<Vec<_>>();
    if state_names.is_empty() {
        return None;
    }
    for (eq_idx, eq) in dae_model.continuous.equations.iter().enumerate() {
        if let Some(lhs) = &eq.lhs
            && state_names
                .iter()
                .any(|state_name| lhs.var_name() == state_name)
        {
            return Some(format!(
                "continuous equation {eq_idx} directly assigns state `{}`",
                lhs.as_str()
            ));
        }
        if let Some(state_name) = residual_direct_state_assignment(&eq.rhs, &state_names) {
            return Some(format!(
                "continuous residual equation {eq_idx} directly assigns state `{}`",
                state_name.as_str()
            ));
        }
    }
    None
}

fn residual_direct_state_assignment<'a>(
    expr: &'a Expression,
    state_names: &'a [VarName],
) -> Option<&'a VarName> {
    match expr {
        Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            if let Some(state_name) = direct_state_ref(lhs, state_names)
                && !dae::expr_contains_der_of(rhs, state_name)
            {
                return Some(state_name);
            }
            if let Some(state_name) = direct_state_ref(rhs, state_names)
                && !dae::expr_contains_der_of(lhs, state_name)
            {
                return Some(state_name);
            }
            None
        }
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs,
            ..
        } => residual_direct_state_assignment(rhs, state_names),
        _ => None,
    }
}

fn direct_state_ref<'a>(expr: &'a Expression, state_names: &'a [VarName]) -> Option<&'a VarName> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    state_names
        .iter()
        .find(|state_name| dae::var_ref_matches_unknown(name, subscripts, state_name))
}

fn validate_direct_runtime_model(model: &solve::SolveModel) -> Result<(), String> {
    let state_count = model.state_scalar_count();
    if state_count == 0 {
        return Err("model has no states".to_string());
    }

    let derivative_rhs_len = model
        .problem
        .continuous
        .derivative_rhs
        .len()
        .map_err(|err| err.to_string())?;
    if derivative_rhs_len != state_count {
        return Err(format!(
            "derivative RHS has {derivative_rhs_len} rows for {state_count} state scalars"
        ));
    }

    validate_tail_residual_targets(model)?;
    validate_no_projected_derivative_dependencies(model)
}

fn validate_tail_residual_targets(model: &solve::SolveModel) -> Result<(), String> {
    let state_count = model.state_scalar_count();
    let implicit_rhs_len = model
        .problem
        .continuous
        .implicit_rhs
        .len()
        .map_err(|err| err.to_string())?;
    if model.problem.continuous.implicit_row_targets.len() != implicit_rhs_len {
        return Err(format!(
            "implicit row target count {} does not match implicit RHS row count {implicit_rhs_len}",
            model.problem.continuous.implicit_row_targets.len()
        ));
    }
    if implicit_rhs_len < state_count {
        return Err(format!(
            "implicit RHS has {implicit_rhs_len} rows for {state_count} state scalars"
        ));
    }

    for (row_idx, target) in model
        .problem
        .continuous
        .implicit_row_targets
        .iter()
        .enumerate()
        .skip(state_count)
    {
        match target {
            Some(solve::ScalarSlot::Y { index, .. }) if *index < state_count => {
                return Err(format!(
                    "implicit residual row {row_idx} targets state Y[{index}]"
                ));
            }
            Some(solve::ScalarSlot::Y { .. }) => {}
            Some(other) => {
                return Err(format!(
                    "implicit residual row {row_idx} targets non-solver slot {other:?}"
                ));
            }
            None => {
                return Err(format!("implicit residual row {row_idx} has no target"));
            }
        }
    }
    Ok(())
}

fn validate_no_projected_derivative_dependencies(model: &solve::SolveModel) -> Result<(), String> {
    let derivative_rows =
        rumoca_eval_solve::to_scalar_program_block(&model.problem.continuous.derivative_rhs)
            .map_err(|err| err.to_string())?;
    let direct_deps = derivative_non_state_loads(model, &derivative_rows);
    if !direct_deps.is_empty() {
        return Err(format!(
            "derivative RHS reads projected non-state Y slots {:?}",
            direct_deps.into_iter().collect::<Vec<_>>()
        ));
    }
    Ok(())
}

fn derivative_non_state_loads(
    model: &solve::SolveModel,
    derivative_rows: &solve::ScalarProgramBlock,
) -> BTreeSet<usize> {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    derivative_rows
        .programs
        .iter()
        .take(state_count)
        .flat_map(|row| non_state_y_loads(row, state_count, solver_count))
        .collect()
}

fn non_state_y_loads(
    row: &[solve::LinearOp],
    state_count: usize,
    solver_count: usize,
) -> Vec<usize> {
    let mut loads = row
        .iter()
        .filter_map(|op| match *op {
            solve::LinearOp::LoadY { index, .. }
                if index >= state_count && index < solver_count =>
            {
                Some(index)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    loads.sort_unstable();
    loads.dedup();
    loads
}

fn trace_direct_rejection(reason: impl AsRef<str>) {
    tracing::debug!(
        target: "rumoca_sim::solve_lowering",
        "direct simulation lowering rejected: {}",
        reason.as_ref()
    );
}

#[cfg(test)]
mod tests {
    use super::validate_gpu_dae_admission;
    use rumoca_core::{Expression, Literal, SourceId, Span, VarName};
    use rumoca_ir_dae as dae;

    fn span() -> Span {
        Span::from_offsets(SourceId::from_source_name("gpu_admission.mo"), 1, 2)
    }

    fn zero() -> Expression {
        Expression::Literal {
            value: Literal::Real(0.0),
            span: span(),
        }
    }

    fn equation() -> dae::Equation {
        dae::Equation::residual(zero(), span(), "GPU admission fixture")
    }

    #[test]
    fn gpu_admission_rejects_discrete_and_condition_partitions_with_source_span() {
        let mut discrete = dae::Dae::default();
        discrete.discrete.real_updates.push(equation());
        let error = validate_gpu_dae_admission(&discrete).expect_err("discrete must reject");
        assert!(error.to_string().contains("discrete real updates"));

        let mut conditions = dae::Dae::default();
        conditions.conditions.equations.push(equation());
        let error = validate_gpu_dae_admission(&conditions).expect_err("conditions must reject");
        assert!(error.to_string().contains("condition equations"));

        let mut bare_discrete = dae::Dae::default();
        let name = VarName::new("z");
        bare_discrete
            .variables
            .discrete_reals
            .insert(name.clone(), dae::Variable::empty_with_span(span()));
        let error = validate_gpu_dae_admission(&bare_discrete)
            .expect_err("a discrete variable without updates must reject");
        assert!(error.to_string().contains("discrete Real variables"));
    }

    #[test]
    fn gpu_admission_rejects_event_and_clock_metadata_with_source_span() {
        let mut events = dae::Dae::default();
        events.events.synthetic_root_conditions.push(zero());
        let error = validate_gpu_dae_admission(&events).expect_err("events must reject");
        assert!(error.to_string().contains("root conditions"));

        let mut scheduled = dae::Dae::default();
        let event_span = Span::from_offsets(SourceId::from_source_name("event-only.mo"), 40, 55);
        scheduled
            .events
            .scheduled_time_events
            .push(dae::DaeScheduledTimeEvent {
                time: 1.0,
                source_span: Some(event_span),
            });
        let error = validate_gpu_dae_admission(&scheduled)
            .expect_err("scheduled time events must reject before fast lowering");
        assert!(error.to_string().contains("scheduled time events"));
        assert_eq!(error.source_span(), Some(event_span));

        let mut clocks = dae::Dae::default();
        clocks.clocks.constructor_exprs.push(zero());
        let error = validate_gpu_dae_admission(&clocks).expect_err("clocks must reject");
        assert!(error.to_string().contains("clock constructors"));
    }

    #[test]
    fn gpu_admission_rejects_typed_pre_slot_memory() {
        let mut dae_model = dae::Dae::default();
        let name = rumoca_core::pre_slot_name("x");
        let mut variable = dae::Variable::empty_with_span(span());
        variable.name = name.clone();
        dae_model.variables.parameters.insert(name, variable);
        let error = validate_gpu_dae_admission(&dae_model).expect_err("pre slot must reject");
        assert!(error.to_string().contains("pre-state memory"));
    }

    #[test]
    fn gpu_admission_rejects_initial_parameter_target_before_fast_lowering() {
        let mut dae_model = dae::Dae::default();
        let name = VarName::new("p");
        dae_model
            .variables
            .parameters
            .insert(name.clone(), dae::Variable::empty_with_span(span()));
        dae_model
            .initialization
            .equations
            .push(dae::Equation::explicit(name, zero(), span(), "P target"));

        let error = validate_gpu_dae_admission(&dae_model)
            .expect_err("initial parameter target must reject");
        assert!(error.to_string().contains("initial P-slot target"));
    }
}
