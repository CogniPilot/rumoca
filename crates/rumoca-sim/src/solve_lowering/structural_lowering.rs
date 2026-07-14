//! The shared structural-preparation and elimination funnel that both the
//! simulation lowering and the `--inspect structure` report run through, so the
//! report and the simulator always agree on the matched system.

use rumoca_ir_dae as dae;
use rumoca_solver::SimOptions;

use super::expr_util::{
    debug_render_expr, equation_lhs_prefix, remove_duplicate_continuous_equations,
};
use super::timing::{log_solve_lowering_done, log_solve_lowering_start, stage_timer_start};

/// Shared structural preparation run before both the simulation lowering and the
/// `--inspect structure` report: scalarize (when requested), demote pseudo-states
/// and reduce index, eliminate derivative aliases, and rewrite standalone
/// `der(state)` references in non-ODE rows (`y = der(x)` → `y = <x's ODE rhs>`).
///
/// Keeping this in one place ensures the structural report and the simulator
/// agree on the matched system, and that fixes apply to both paths at once.
pub(super) fn prepare_dae_for_structural_analysis(
    lowered: &mut dae::Dae,
    opts: &SimOptions,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("prepare.scalarize_vector_member_slices");
    let timer = stage_timer_start();
    rumoca_phase_dae::scalarize_phantom_vector_equations(lowered)
        .map_err(vector_scalarization_lower_error)?;
    log_solve_lowering_done("prepare.scalarize_vector_member_slices", timer);
    if opts.scalarize {
        log_solve_lowering_start("prepare.scalarize_equations");
        let timer = stage_timer_start();
        rumoca_phase_structural::scalarize::scalarize_equations(lowered)
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
        log_solve_lowering_done("prepare.scalarize_equations", timer);
    }
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
    log_solve_lowering_start("prepare.demote_states_after_standalone_derivative_substitution");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_retained_derivative_rows(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done(
        "prepare.demote_states_after_standalone_derivative_substitution",
        timer,
    );
    log_solve_lowering_start("prepare.remove_nonnumeric_continuous_equations");
    let timer = stage_timer_start();
    remove_nonnumeric_continuous_equations(lowered);
    log_solve_lowering_done("prepare.remove_nonnumeric_continuous_equations", timer);
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

/// Boundary elimination can expose state constraints that were hidden behind
/// aliases or simple connector equalities. Re-run the state/dummy preparation
/// steps that depend on those exposed equations before the final BLT match.
pub(super) fn prepare_dae_after_boundary_elimination(
    lowered: &mut dae::Dae,
    boundary_substitutions: &[rumoca_phase_structural::eliminate::Substitution],
) -> Result<bool, rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.post_boundary.demote_exact_alias_component_states");
    let timer = stage_timer_start();
    let exact_alias_demoted =
        rumoca_phase_structural::dae_prepare::demote_exact_alias_component_states(lowered)
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done(
        "structural.post_boundary.demote_exact_alias_component_states",
        timer,
    );

    log_solve_lowering_start("structural.post_boundary.demote_direct_assigned_states");
    let timer = stage_timer_start();
    let direct_demoted =
        rumoca_phase_structural::dae_prepare::demote_direct_assigned_states_with_boundary_substitutions(
            lowered,
            boundary_substitutions,
        )
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done(
        "structural.post_boundary.demote_direct_assigned_states",
        timer,
    );

    log_solve_lowering_start("structural.post_boundary.reduce_constrained_dummy_derivatives");
    let timer = stage_timer_start();
    let dummy_reduced =
        rumoca_phase_structural::dae_prepare::reduce_constrained_dummy_derivatives(lowered)
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done(
        "structural.post_boundary.reduce_constrained_dummy_derivatives",
        timer,
    );

    log_solve_lowering_start(
        "structural.post_boundary.demote_states_without_retained_derivative_rows",
    );
    let timer = stage_timer_start();
    let (states_demoted, unassignable_demoted) =
        rumoca_phase_structural::dae_prepare::demote_states_without_retained_derivative_rows(
            lowered,
        )
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done(
        "structural.post_boundary.demote_states_without_retained_derivative_rows",
        timer,
    );

    Ok(exact_alias_demoted > 0
        || direct_demoted > 0
        || dummy_reduced > 0
        || states_demoted > 0
        || unassignable_demoted > 0)
}

fn remove_nonnumeric_continuous_equations(dae: &mut dae::Dae) {
    let removed = dae
        .continuous
        .equations
        .iter()
        .enumerate()
        .filter_map(|(idx, equation)| {
            expression_contains_nonnumeric_metadata(&equation.rhs).then_some(idx)
        })
        .collect::<Vec<_>>();
    if removed.is_empty() {
        return;
    }
    let mut next_removed = 0usize;
    let mut next_idx = 0usize;
    dae.continuous.equations.retain(|_| {
        let remove = removed
            .get(next_removed)
            .is_some_and(|idx| *idx == next_idx);
        next_idx += 1;
        if remove {
            next_removed += 1;
        }
        !remove
    });
    shift_structured_families_after_equation_removal(
        &mut dae.continuous.structured_equations,
        &removed,
    );
}

fn expression_contains_nonnumeric_metadata(expr: &rumoca_core::Expression) -> bool {
    struct Visitor {
        found: bool,
    }

    impl rumoca_core::ExpressionVisitor for Visitor {
        fn visit_function_call(
            &mut self,
            name: &rumoca_core::Reference,
            args: &[rumoca_core::Expression],
            _is_constructor: bool,
        ) {
            // Table intrinsics are numeric even though their constructor
            // metadata contains String fields. Skip only this call's argument
            // subtree; nonnumeric siblings and wrappers must still be seen.
            if external_table_numeric_intrinsic_reference(name) {
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }

        fn visit_literal(&mut self, value: &rumoca_core::Literal) {
            if matches!(value, rumoca_core::Literal::String(_)) {
                self.found = true;
            }
        }
    }

    let mut visitor = Visitor { found: false };
    rumoca_core::ExpressionVisitor::visit_expression(&mut visitor, expr);
    visitor.found
}

fn external_table_numeric_intrinsic_reference(name: &rumoca_core::Reference) -> bool {
    if external_table_numeric_intrinsic(name.last_segment()) {
        return true;
    }

    // Scalarization represents a scalar function result as an exact output
    // projection (`function.y`). Match that structured shape explicitly; a
    // generic suffix match would accidentally preserve unrelated String-valued
    // calls whose names merely contain an intrinsic name.
    let segments = name.segments();
    segments.last() == Some(&"y")
        && segments
            .get(segments.len().saturating_sub(2))
            .is_some_and(|function| external_table_numeric_intrinsic(function))
}

fn external_table_numeric_intrinsic(short_name: &str) -> bool {
    matches!(
        short_name,
        "getTimeTableTmin"
            | "getTimeTableTmax"
            | "getNextTimeEvent"
            | "getTimeTableValueNoDer"
            | "getTimeTableValueNoDer2"
            | "getTimeTableValue"
            | "getTable1DAbscissaUmin"
            | "getTable1DAbscissaUmax"
            | "getTable1DValueNoDer"
            | "getTable1DValueNoDer2"
            | "getTable1DValue"
    )
}

#[cfg(test)]
fn projected_external_table_test_call() -> rumoca_core::Expression {
    let span = rumoca_core::Span::DUMMY;
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(
            "Modelica.Blocks.Tables.Internal.getTimeTableValueNoDer.y",
        ),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("NoName".to_string()),
            span,
        }],
        is_constructor: false,
        span,
    }
}

#[cfg(test)]
#[test]
fn projected_external_table_numeric_intrinsic_survives_metadata_pruning() {
    let span = rumoca_core::Span::DUMMY;
    let mut model = dae::Dae::default();
    model.continuous.equations.push(dae::Equation {
        lhs: None,
        rhs: projected_external_table_test_call(),
        span,
        origin: "scalarized external table lookup".to_string(),
        scalar_count: 1,
    });

    remove_nonnumeric_continuous_equations(&mut model);

    assert_eq!(model.continuous.equations.len(), 1);
}

#[cfg(test)]
#[test]
fn projected_external_table_call_does_not_hide_string_sibling_metadata() {
    let span = rumoca_core::Span::DUMMY;
    let expr = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Add,
        lhs: Box::new(projected_external_table_test_call()),
        rhs: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("metadata".to_string()),
            span,
        }),
        span,
    };

    assert!(expression_contains_nonnumeric_metadata(&expr));
}

#[cfg(test)]
#[test]
fn numeric_named_argument_wrapper_is_not_nonnumeric_metadata() {
    let span = rumoca_core::Span::DUMMY;
    let named = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(format!("{}A", rumoca_core::NAMED_FUNCTION_ARG_PREFIX)),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span,
        }],
        is_constructor: false,
        span,
    };
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.numericFunction"),
        args: vec![named],
        is_constructor: false,
        span,
    };

    assert!(!expression_contains_nonnumeric_metadata(&expr));
}

#[cfg(test)]
#[test]
fn named_argument_wrapper_does_not_hide_nonnumeric_actual() {
    let span = rumoca_core::Span::DUMMY;
    let named = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(format!(
            "{}fileName",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("table.csv".to_string()),
            span,
        }],
        is_constructor: false,
        span,
    };
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.numericFunction"),
        args: vec![named],
        is_constructor: false,
        span,
    };

    assert!(expression_contains_nonnumeric_metadata(&expr));
}

#[cfg(test)]
#[test]
fn projected_external_table_call_remains_numeric_inside_named_argument() {
    let span = rumoca_core::Span::DUMMY;
    let named = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(format!(
            "{}tableValue",
            rumoca_core::NAMED_FUNCTION_ARG_PREFIX
        )),
        args: vec![projected_external_table_test_call()],
        is_constructor: false,
        span,
    };
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new("Pkg.numericFunction"),
        args: vec![named],
        is_constructor: false,
        span,
    };

    assert!(!expression_contains_nonnumeric_metadata(&expr));
}

fn shift_structured_families_after_equation_removal(
    families: &mut Vec<dae::StructuredEquationFamily>,
    removed_sorted: &[usize],
) {
    families.retain_mut(|family| {
        let total: usize = family.equation_counts.iter().sum();
        let block_end = family.first_equation_index + total;
        if removed_sorted
            .iter()
            .any(|&idx| idx >= family.first_equation_index && idx < block_end)
        {
            return false;
        }
        let shift = removed_sorted
            .iter()
            .filter(|&&idx| idx < family.first_equation_index)
            .count();
        family.first_equation_index -= shift;
        true
    });
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
        metadata_dae,
    } = prepare_structural_daes(dae_model, opts)?;
    if dae_model.variables.states.is_empty() {
        let mut residual_shape_dae = source_dae.clone();
        remove_nonnumeric_continuous_equations(&mut residual_shape_dae);
        validate_residual_shapes_for_simulation(&residual_shape_dae)?;
    }
    log_solve_lowering_start("structural.eliminate_trivial");
    let timer = stage_timer_start();
    let mut elimination = rumoca_phase_structural::eliminate::eliminate_trivial(&mut lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.eliminate_trivial", timer);
    let first_blt_error = elimination.blt_error.take();
    if first_blt_error.is_some()
        && prepare_dae_after_boundary_elimination(&mut lowered, &elimination.substitutions)?
    {
        let mut substitutions = elimination.substitutions;
        log_solve_lowering_start("structural.eliminate_trivial_after_boundary_state_prep");
        let timer = stage_timer_start();
        elimination = rumoca_phase_structural::eliminate::eliminate_trivial(&mut lowered)
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
        log_solve_lowering_done(
            "structural.eliminate_trivial_after_boundary_state_prep",
            timer,
        );
        substitutions.extend(elimination.substitutions);
        if elimination.blt_error.is_some() {
            reprepare_dae_after_boundary_elimination(&mut lowered, opts)?;
            log_solve_lowering_start("structural.eliminate_trivial_after_boundary_reprepare");
            let timer = stage_timer_start();
            elimination = rumoca_phase_structural::eliminate::eliminate_trivial(&mut lowered)
                .map_err(
                    |source| rumoca_phase_solve::SolveModelLowerError::Structural { source },
                )?;
            log_solve_lowering_done(
                "structural.eliminate_trivial_after_boundary_reprepare",
                timer,
            );
            substitutions.extend(elimination.substitutions);
        }
        elimination.substitutions = substitutions;
    } else if let Some(source) = first_blt_error {
        if dae_model.variables.states.is_empty() {
            validate_residual_shapes_for_simulation(dae_model)?;
        }
        return Err(rumoca_phase_solve::SolveModelLowerError::Structural { source });
    }
    if let Some(source) = elimination.blt_error {
        if dae_model.variables.states.is_empty() {
            validate_residual_shapes_for_simulation(dae_model)?;
        }
        return Err(rumoca_phase_solve::SolveModelLowerError::Structural { source });
    }
    apply_simulation_elimination(&mut lowered, &elimination.substitutions)?;
    trace_simulation_elimination(&lowered, &elimination.substitutions);
    let visible_expressions =
        visible_expressions_after_elimination(&source_dae, &elimination.substitutions, opts)?;

    Ok(StructurallyLoweredDae {
        dae: lowered,
        metadata_dae,
        visible_expressions,
    })
}

fn prepare_structural_daes(
    dae_model: &dae::Dae,
    opts: &SimOptions,
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
    prepare_dae_for_structural_analysis(&mut lowered, opts)?;
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

fn reprepare_dae_after_boundary_elimination(
    lowered: &mut dae::Dae,
    opts: &SimOptions,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.reprepare_after_boundary_elimination");
    let timer = stage_timer_start();
    prepare_dae_for_structural_analysis(lowered, opts)?;
    remove_duplicate_continuous_equations(lowered);
    log_solve_lowering_done("structural.reprepare_after_boundary_elimination", timer);
    Ok(())
}

pub(super) fn prepare_structural_dae_for_simulation_artifact(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<dae::Dae, rumoca_phase_solve::SolveModelLowerError> {
    prepare_structural_daes(dae_model, opts).map(|prepared| prepared.lowered)
}

pub(super) fn boundary_reduced_dae_for_simulation_artifact(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<dae::Dae, rumoca_phase_solve::SolveModelLowerError> {
    let mut lowered = prepare_structural_daes(dae_model, opts)?.lowered;
    let mut elimination = rumoca_phase_structural::eliminate::eliminate_trivial(&mut lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    if elimination.blt_error.is_some()
        && prepare_dae_after_boundary_elimination(&mut lowered, &elimination.substitutions)?
    {
        elimination = rumoca_phase_structural::eliminate::eliminate_trivial(&mut lowered)
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
        if elimination.blt_error.is_some() {
            reprepare_dae_after_boundary_elimination(&mut lowered, opts)?;
            rumoca_phase_structural::eliminate::eliminate_trivial(&mut lowered).map_err(
                |source| rumoca_phase_solve::SolveModelLowerError::Structural { source },
            )?;
        }
    }
    Ok(lowered)
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

fn visible_expressions_after_elimination(
    source_dae: &dae::Dae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
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
        log_solve_lowering_done("structural.scalarize_observation_dae", timer);
    }
    if !substitutions.is_empty() {
        log_solve_lowering_start("structural.resolve_observation_substitutions");
        let timer = stage_timer_start();
        for eq in &mut observation_dae.continuous.equations {
            eq.rhs = rumoca_phase_structural::eliminate::resolve_substitutions_in_expr(
                &eq.rhs,
                substitutions,
            )
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
        }
        log_solve_lowering_done("structural.resolve_observation_substitutions", timer);
    }
    log_solve_lowering_start("structural.visible_expressions_for_dae");
    let timer = stage_timer_start();
    let mut visible_expressions = rumoca_phase_solve::visible_expressions_for_dae(&observation_dae)
        .map_err(rumoca_phase_solve::SolveModelLowerError::Lower)?;
    log_solve_lowering_done("structural.visible_expressions_for_dae", timer);
    if !substitutions.is_empty() {
        log_solve_lowering_start("structural.resolve_visible_expression_substitutions");
        let timer = stage_timer_start();
        for visible in &mut visible_expressions {
            visible.expr = rumoca_phase_structural::eliminate::resolve_substitutions_in_expr(
                &visible.expr,
                substitutions,
            )
            .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
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

fn vector_scalarization_lower_error(
    err: rumoca_phase_dae::ToDaeError,
) -> rumoca_phase_solve::SolveModelLowerError {
    let reason = format!("DAE vector scalarization failed: {err}");
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
