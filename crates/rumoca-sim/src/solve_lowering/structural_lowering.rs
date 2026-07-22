//! The shared structural-preparation and elimination funnel that both the
//! simulation lowering and the `--inspect structure` report run through, so the
//! report and the simulator always agree on the matched system.

use rumoca_ir_dae as dae;
use rumoca_solver::SimOptions;

use super::causal_reconstruction::restore_shared_causal_assignments;
use super::expr_util::{
    debug_render_expr, equation_lhs_prefix, remove_duplicate_continuous_equations,
};
use super::timing::{log_solve_lowering_done, log_solve_lowering_start, stage_timer_start};

/// Shared structural rewrites run before both simulation lowering and the
/// `--inspect structure` report: demote pseudo-states and reduce index,
/// eliminate derivative aliases, and rewrite standalone
/// `der(state)` references in non-ODE rows (`y = der(x)` → `y = <x's ODE rhs>`).
///
/// Keeping this in one place ensures the structural report and the simulator
/// agree on the matched system, and that fixes apply to both paths at once.
fn rewrite_dae_for_structural_analysis(
    lowered: &mut dae::Dae,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("prepare.demote_exact_alias_component_states");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_exact_alias_component_states(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.demote_exact_alias_component_states", timer);
    log_solve_lowering_start("prepare.demote_direct_assigned_states");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_direct_assigned_states(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.demote_direct_assigned_states", timer);
    log_solve_lowering_start("prepare.reduce_constrained_dummy_derivatives");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::reduce_constrained_dummy_derivatives(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.reduce_constrained_dummy_derivatives", timer);
    log_solve_lowering_start("prepare.index_reduce_missing_state_derivatives");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::index_reduce_missing_state_derivatives(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.index_reduce_missing_state_derivatives", timer);
    log_solve_lowering_start("prepare.demote_states_without_assignable_derivative_rows");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_assignable_derivative_rows(lowered);
    log_solve_lowering_done(
        "prepare.demote_states_without_assignable_derivative_rows",
        timer,
    );
    log_solve_lowering_start("prepare.eliminate_derivative_aliases");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::eliminate_derivative_aliases(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.eliminate_derivative_aliases", timer);
    log_solve_lowering_start("prepare.demote_states_without_retained_derivative_rows");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_retained_derivative_rows(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done(
        "prepare.demote_states_without_retained_derivative_rows",
        timer,
    );
    // After demotion, any `der(<algebraic>)` (a differentiated algebraic such as
    // `a_rel = der(w_rel)`, or successive `Der` blocks) is expanded symbolically
    // via the chain rule, leaving only `der(state)`. Running this after demotion
    // is essential: a `der`'d algebraic with its own defining equation is first
    // demoted from a spurious state, then its derivative is expanded here rather
    // than left as an orphan column (which the matcher reports as singular).
    log_solve_lowering_start("prepare.expand_compound_derivatives");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::expand_compound_derivatives(lowered);
    log_solve_lowering_done("prepare.expand_compound_derivatives", timer);
    // Rewrite `y = der(x)` (e.g. a `Modelica.Blocks.Continuous.Der` block reading
    // a state derivative) into `y = <x's ODE rhs>` so `y` is matchable. Without
    // this, the standalone `der(x)` reference in a non-ODE row has no column to
    // match and the system reports a spurious structural singularity.
    log_solve_lowering_start("prepare.substitute_standalone_state_derivatives_in_non_ode_rows");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::substitute_standalone_state_derivatives_in_non_ode_rows(
        lowered,
    );
    log_solve_lowering_done(
        "prepare.substitute_standalone_state_derivatives_in_non_ode_rows",
        timer,
    );
    if tracing::enabled!(target: "rumoca_phase_structural", tracing::Level::DEBUG) {
        for (index, eq) in lowered.continuous.equations.iter().enumerate() {
            let summary = format!("{}{}", equation_lhs_prefix(eq), debug_render_expr(&eq.rhs));
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] prepared f_x[{index}] origin='{}' {}",
                eq.origin,
                summary
            );
        }
    }
    Ok(())
}

pub(super) fn prepare_dae_for_structural_analysis(
    lowered: &mut dae::Dae,
    opts: &SimOptions,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    rewrite_dae_for_structural_analysis(lowered)?;
    scalarize_solver_view(lowered, opts, "prepare.scalarize_equations")
}

pub(super) struct StructurallyLoweredDae {
    pub(super) dae: dae::Dae,
    pub(super) metadata_dae: dae::Dae,
    pub(super) visible_expressions: Vec<rumoca_phase_solve::VisibleExpression>,
}

struct PreparedStructuralDaes {
    source_dae: dae::Dae,
    lowered: dae::Dae,
    metadata_dae: dae::Dae,
}

pub(super) fn structurally_lower_dae_for_simulation(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<StructurallyLoweredDae, rumoca_phase_solve::SolveModelLowerError> {
    let PreparedStructuralDaes {
        source_dae,
        mut lowered,
        mut metadata_dae,
    } = prepare_structural_daes(dae_model)?;

    log_solve_lowering_start("structural.eliminate_trivial");
    let timer = stage_timer_start();
    let elimination = eliminate_with_singular_state_selection(&mut lowered)?;
    log_solve_lowering_done("structural.eliminate_trivial", timer);
    if let Some(source) = elimination.blt_error {
        if dae_model.variables.states.is_empty() {
            validate_residual_shapes_for_simulation(dae_model)?;
        }
        return Err(rumoca_phase_solve::SolveModelLowerError::Structural { source });
    }

    let causal_plan = rumoca_phase_structural::eliminate::factor_causal_substitutions(
        &source_dae,
        &elimination.substitutions,
    )
    .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    apply_simulation_elimination(&mut lowered, &elimination.substitutions)?;
    trace_simulation_elimination(&lowered, &elimination.substitutions);
    log_solve_lowering_start("structural.restore_shared_causal_assignments");
    let timer = stage_timer_start();
    let restored_causal_targets =
        restore_shared_causal_assignments(&mut lowered, &source_dae, &causal_plan);
    log_solve_lowering_done("structural.restore_shared_causal_assignments", timer);
    tracing::debug!(
        target: "rumoca_sim::solve_lowering",
        candidates = causal_plan.retained_targets.len(),
        restored = restored_causal_targets.len(),
        "restored shared causal assignments"
    );
    mark_state_selection_metadata(&mut metadata_dae, &elimination.substitutions)?;
    let visible_expressions = visible_expressions_after_elimination(
        &source_dae,
        &causal_plan.substitutions,
        &restored_causal_targets,
        opts,
    )?;
    scalarize_solver_view(&mut lowered, opts, "structural.scalarize_solve_dae")?;
    log_solve_lowering_start("structural.condense_scalar_algebraic_loops");
    let timer = stage_timer_start();
    let condensation =
        rumoca_phase_structural::eliminate::condense_scalar_algebraic_loops(&mut lowered)
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.condense_scalar_algebraic_loops", timer);
    tracing::debug!(
        target: "rumoca_sim::solve_lowering",
        blocks = condensation.blocks.len(),
        causal_variables = condensation.causal_variable_count(),
        "condensed exact algebraic BLT loops"
    );

    Ok(StructurallyLoweredDae {
        dae: lowered,
        metadata_dae,
        visible_expressions,
    })
}

fn eliminate_with_singular_state_selection(
    dae: &mut dae::Dae,
) -> Result<
    rumoca_phase_structural::eliminate::EliminationResult,
    rumoca_phase_solve::SolveModelLowerError,
> {
    rumoca_phase_structural::eliminate::eliminate_trivial_with_state_selection(dae)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })
}

fn prepare_structural_daes(
    dae_model: &dae::Dae,
) -> Result<PreparedStructuralDaes, rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.attach_dae_reference_metadata");
    let timer = stage_timer_start();
    let mut source_dae = dae_model.clone();
    rumoca_phase_dae::attach_dae_reference_metadata(&mut source_dae)
        .map_err(metadata_attachment_lower_error)?;
    log_solve_lowering_done("structural.attach_dae_reference_metadata", timer);
    log_solve_lowering_start("structural.clone_source_for_lowered");
    let timer = stage_timer_start();
    let mut lowered = source_dae.clone();
    log_solve_lowering_done("structural.clone_source_for_lowered", timer);
    rewrite_dae_for_structural_analysis(&mut lowered)?;
    log_solve_lowering_start("structural.remove_duplicate_continuous_equations");
    let timer = stage_timer_start();
    remove_duplicate_continuous_equations(&mut lowered);
    log_solve_lowering_done("structural.remove_duplicate_continuous_equations", timer);
    log_solve_lowering_start("structural.clone_metadata_dae");
    let timer = stage_timer_start();
    let metadata_dae = lowered.clone();
    log_solve_lowering_done("structural.clone_metadata_dae", timer);

    Ok(PreparedStructuralDaes {
        source_dae,
        lowered,
        metadata_dae,
    })
}

fn scalarize_solver_view(
    dae_model: &mut dae::Dae,
    opts: &SimOptions,
    stage: &'static str,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    if !opts.scalarize {
        return Ok(());
    }
    log_solve_lowering_start(stage);
    let timer = stage_timer_start();
    rumoca_phase_structural::scalarize::scalarize_equations(dae_model)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    rumoca_phase_structural::dae_prepare::simplify_scalarized_continuous_equations(dae_model);
    log_solve_lowering_done(stage, timer);
    Ok(())
}

fn apply_simulation_elimination(
    lowered: &mut dae::Dae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.demote_states_without_retained_derivative_rows");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_retained_derivative_rows(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done(
        "structural.demote_states_without_retained_derivative_rows",
        timer,
    );
    log_solve_lowering_start("structural.apply_elimination_substitutions_to_dae");
    let timer = stage_timer_start();
    rumoca_phase_structural::eliminate::apply_elimination_substitutions_to_dae(
        lowered,
        substitutions,
    )
    .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.apply_elimination_substitutions_to_dae", timer);
    Ok(())
}

fn trace_simulation_elimination(
    lowered: &dae::Dae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
) {
    if tracing::enabled!(target: "rumoca_phase_structural", tracing::Level::DEBUG) {
        for sub in substitutions {
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] substitution {} := {}",
                sub.var_name.as_str(),
                debug_render_expr(&sub.expr)
            );
        }
        for (index, eq) in lowered.continuous.equations.iter().enumerate() {
            tracing::debug!(
                target: "rumoca_phase_structural",
                "[sim-trace] post-elim f_x[{index}] origin='{}' {}{}",
                eq.origin,
                equation_lhs_prefix(eq),
                debug_render_expr(&eq.rhs)
            );
        }
    }
}

fn mark_state_selection_metadata(
    metadata_dae: &mut dae::Dae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.clone_state_selection_dae");
    let timer = stage_timer_start();
    let mut state_selection_dae = metadata_dae.clone();
    log_solve_lowering_done("structural.clone_state_selection_dae", timer);
    log_solve_lowering_start("structural.apply_state_selection_substitutions");
    let timer = stage_timer_start();
    rumoca_phase_structural::eliminate::apply_elimination_substitutions_to_dae(
        &mut state_selection_dae,
        substitutions,
    )
    .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.apply_state_selection_substitutions", timer);
    log_solve_lowering_start("structural.demote_state_selection_dae");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_retained_derivative_rows(
        &mut state_selection_dae,
    )
    .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.demote_state_selection_dae", timer);
    log_solve_lowering_start("structural.mark_constrained_dummy_states_in_metadata");
    let timer = stage_timer_start();
    mark_constrained_dummy_states_in_metadata(&state_selection_dae, metadata_dae)?;
    log_solve_lowering_done(
        "structural.mark_constrained_dummy_states_in_metadata",
        timer,
    );
    // State selection can demote a source state only after the first compound
    // derivative pass. Re-run the same chain-rule normalization on the final
    // metadata partition so initialization equations cannot retain an orphan
    // `der(algebraic)` merely because classification changed late.
    log_solve_lowering_start("structural.expand_state_selection_metadata_derivatives");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::expand_compound_derivatives(metadata_dae);
    log_solve_lowering_done(
        "structural.expand_state_selection_metadata_derivatives",
        timer,
    );
    Ok(())
}

fn visible_expressions_after_elimination(
    source_dae: &dae::Dae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
    restored_causal_targets: &indexmap::IndexSet<rumoca_core::VarName>,
    opts: &SimOptions,
) -> Result<Vec<rumoca_phase_solve::VisibleExpression>, rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.clone_observation_dae");
    let timer = stage_timer_start();
    let mut observation_dae = source_dae.clone();
    log_solve_lowering_done("structural.clone_observation_dae", timer);
    if opts.scalarize {
        log_solve_lowering_start("structural.scalarize_observation_dae");
        let timer = stage_timer_start();
        rumoca_phase_structural::scalarize::scalarize_equations(&mut observation_dae)
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
        rumoca_phase_structural::dae_prepare::simplify_scalarized_continuous_equations(
            &mut observation_dae,
        );
        log_solve_lowering_done("structural.scalarize_observation_dae", timer);
    }
    // The visible inventory is declaration-based; rewriting every source
    // equation cannot change it and duplicates large eliminated expression
    // trees. Resolve only the selected visible expressions below.
    log_solve_lowering_start("structural.visible_expressions_for_dae");
    let timer = stage_timer_start();
    let mut visible_expressions = rumoca_phase_solve::visible_expressions_for_dae(&observation_dae)
        .map_err(rumoca_phase_solve::SolveModelLowerError::Lower)?;
    log_solve_lowering_done("structural.visible_expressions_for_dae", timer);
    if !substitutions.is_empty() {
        log_solve_lowering_start("structural.resolve_visible_expression_substitutions");
        let timer = stage_timer_start();
        let mut expressions = visible_expressions
            .iter()
            .map(|visible| visible.expr.clone())
            .collect::<Vec<_>>();
        let observation_substitutions = substitutions
            .iter()
            .filter(|substitution| !restored_causal_targets.contains(&substitution.var_name))
            .cloned()
            .collect::<Vec<_>>();
        rumoca_phase_structural::eliminate::resolve_substitutions_in_exprs(
            &mut expressions,
            &observation_substitutions,
        )
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
        for (visible, expression) in visible_expressions.iter_mut().zip(expressions) {
            visible.expr = expression;
        }
        log_solve_lowering_done("structural.resolve_visible_expression_substitutions", timer);
    }
    Ok(visible_expressions)
}

pub(super) fn metadata_attachment_lower_error(
    err: rumoca_phase_dae::ToDaeError,
) -> rumoca_phase_solve::SolveModelLowerError {
    let reason = format!("DAE reference metadata attachment failed: {err}");
    rumoca_phase_solve::SolveModelLowerError::Lower(lower_contract_error_from_optional_span(
        reason,
        err.source_span(),
    ))
}

fn lower_contract_error_from_optional_span(
    reason: String,
    span: Option<rumoca_core::Span>,
) -> rumoca_phase_solve::lower::LowerError {
    match span {
        Some(span) if !span.is_dummy() => {
            rumoca_phase_solve::lower::LowerError::ContractViolation { reason, span }
        }
        Some(_) | None => {
            rumoca_phase_solve::lower::LowerError::UnspannedContractViolation { reason }
        }
    }
}

fn validate_residual_shapes_for_simulation(
    dae_model: &dae::Dae,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    let layout = rumoca_phase_solve::build_var_layout(dae_model)?;
    rumoca_phase_solve::lower::lower_residual(dae_model, &layout)?;
    Ok(())
}

fn mark_constrained_dummy_states_in_metadata(
    structural_dae: &dae::Dae,
    metadata_dae: &mut dae::Dae,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    let dummy_states =
        rumoca_phase_structural::dae_prepare::constrained_dummy_state_names(structural_dae)
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    for state_name in dummy_states {
        let name = rumoca_core::VarName::new(state_name);
        if let Some(var) = metadata_dae.variables.states.shift_remove(&name) {
            metadata_dae.variables.algebraics.insert(name, var);
        }
    }
    Ok(())
}
