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
) -> Result<
    Vec<rumoca_phase_structural::dae_prepare::IndexReducedConstraint>,
    rumoca_phase_solve::SolveModelLowerError,
> {
    // Capture source-level `dummy = der(state)` ownership before any state
    // demotion can move the derivative target out of the state partition.
    // Index reduction may introduce another alias later, so the same
    // idempotent rewrite runs once more after that transformation.
    log_solve_lowering_start("prepare.eliminate_source_dummy_derivative_aliases");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::eliminate_dummy_derivative_aliases_in_place(lowered);
    log_solve_lowering_done("prepare.eliminate_source_dummy_derivative_aliases", timer);
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
    // Equation-driven (Pantelides) index reduction. The two passes above
    // nominate by state; this one nominates by equation, which is the only way
    // a closed kinematic loop's constraint rows — which assign no state at all
    // — are ever reached. It runs here, before scalarization, because the
    // symbolic derivative closure keys defining expressions on aggregate
    // variable names: after scalarization no row assigns `b0.frame_b.r_0` any
    // more, so every nomination would be rejected as an unresolved `der` leaf.
    log_solve_lowering_start("prepare.index_reduce_deficient_constraint_rows");
    let timer = stage_timer_start();
    let reduced_constraints =
        rumoca_phase_structural::dae_prepare::index_reduce_deficient_constraint_rows_with_metadata(
            lowered,
        )
        .constraints;
    log_solve_lowering_done("prepare.index_reduce_deficient_constraint_rows", timer);
    log_solve_lowering_start("prepare.eliminate_index_reduced_dummy_derivative_aliases");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::eliminate_dummy_derivative_aliases_in_place(lowered);
    log_solve_lowering_done(
        "prepare.eliminate_index_reduced_dummy_derivative_aliases",
        timer,
    );
    log_solve_lowering_start("prepare.eliminate_derivative_aliases");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::eliminate_derivative_aliases(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("prepare.eliminate_derivative_aliases", timer);
    // At prepare time the global structural matching has not selected an
    // independent state basis yet. Remove only genuine pseudo-states with no
    // derivative references here; row-assignability cleanup belongs after
    // structural state-selection substitutions have been applied.
    log_solve_lowering_start("prepare.demote_states_without_derivative_refs");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_derivative_refs(lowered);
    log_solve_lowering_done("prepare.demote_states_without_derivative_refs", timer);
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
    Ok(reduced_constraints)
}

pub(super) fn prepare_dae_for_structural_analysis(
    lowered: &mut dae::Dae,
    opts: &SimOptions,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    let _ = rewrite_dae_for_structural_analysis(lowered)?;
    scalarize_solver_view(lowered, opts, "prepare.scalarize_equations")
}

/// A whole-DAE value owned by the structural simulation funnel.
///
/// [`dae::Dae`] is `Clone` with no `Arc` sharing and [`rumoca_core::Expression`]
/// is boxed per node, so every copy reallocates the entire expression graph.
/// Deep-copying one of these is therefore a cost the funnel must not pay by
/// accident, and a reintroduced copy is invisible: nothing observable changes
/// except wall time.
///
/// So the funnel does not hold bare `dae::Dae` values. `Clone` is implemented
/// by hand here and records the copy in
/// [`rumoca_phase_structural::dae_prepare::copy_accounting`]; there is no way
/// to write `something.clone()` in this module and have the copy escape the
/// accounting.
struct FunnelDae(dae::Dae);

impl FunnelDae {
    /// Copy an externally owned DAE into the funnel, recording the copy.
    fn copied_from(dae: &dae::Dae) -> Self {
        Self(rumoca_phase_structural::dae_prepare::copy_accounting::clone_dae(dae))
    }

    fn into_inner(self) -> dae::Dae {
        self.0
    }
}

impl Clone for FunnelDae {
    fn clone(&self) -> Self {
        Self::copied_from(&self.0)
    }
}

impl std::ops::Deref for FunnelDae {
    type Target = dae::Dae;

    fn deref(&self) -> &dae::Dae {
        &self.0
    }
}

impl std::ops::DerefMut for FunnelDae {
    fn deref_mut(&mut self) -> &mut dae::Dae {
        &mut self.0
    }
}

/// Whole-DAE deep copies and full scalarization passes performed while lowering
/// one model through [`structurally_lower_dae_for_simulation`].
///
/// This is a *measured* total, not a hand-maintained list of the copy sites
/// this module happens to know about: every copy site reached by the funnel
/// records itself in
/// [`rumoca_phase_structural::dae_prepare::copy_accounting`], and the funnel
/// reports the difference between the thread's counters on entry and on exit.
/// The nested structural passes copy far more than the funnel's own four
/// staging copies do — index reduction, unassignable-derivative-row demotion,
/// dummy-derivative alias rewriting, state-selection candidate staging and BLT
/// sort-input preparation all stage their rewrites on a copy — so a budget that
/// counted only this module's copies would under-report the funnel by a factor
/// of two or more.
///
/// One copy is not instrumented at its site: the BLT sort input that
/// `eliminate::block_condensation` builds. The funnel records it against that
/// module's own acceptance condition — see [`condense_scalar_algebraic_loops`]
/// — so the reported total is still the funnel's real copy count rather than a
/// count that is knowingly one short.
///
/// A regression test pins the totals for two fixtures. Without it a future
/// refactor can silently reintroduce a discarded copy or a discarded
/// scalarization pass and nothing fails.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub(super) struct FunnelCopyBudget {
    /// Whole-`Dae` deep copies made while lowering one model, including those
    /// made inside nested structural passes.
    pub(super) dae_clones: u32,
    /// Full `scalarize_equations` passes run over a whole `Dae`, including the
    /// one BLT preparation runs on its own sort input.
    pub(super) scalarizations: u32,
}

impl FunnelCopyBudget {
    fn observed_since(
        baseline: rumoca_phase_structural::dae_prepare::copy_accounting::DaeCopyCounts,
    ) -> Self {
        let observed =
            rumoca_phase_structural::dae_prepare::copy_accounting::counts().since(baseline);
        Self {
            dae_clones: observed.dae_clones,
            scalarizations: observed.scalarizations,
        }
    }
}

pub(super) struct StructurallyLoweredDae {
    pub(super) dae: dae::Dae,
    pub(super) metadata_dae: dae::Dae,
    pub(super) reduced_constraints:
        Vec<rumoca_phase_structural::dae_prepare::IndexReducedConstraint>,
    pub(super) visible_expressions: Vec<rumoca_phase_solve::VisibleExpression>,
    pub(super) copy_budget: FunnelCopyBudget,
}

impl StructurallyLoweredDae {
    fn trace_copy_budget(&self) {
        tracing::debug!(
            target: "rumoca_sim::solve_lowering",
            dae_clones = self.copy_budget.dae_clones,
            scalarizations = self.copy_budget.scalarizations,
            "structural funnel copy budget"
        );
    }
}

struct PreparedStructuralDaes {
    source_dae: FunnelDae,
    lowered: FunnelDae,
    metadata_dae: FunnelDae,
    reduced_constraints: Vec<rumoca_phase_structural::dae_prepare::IndexReducedConstraint>,
}

pub(super) fn structurally_lower_dae_for_simulation(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<StructurallyLoweredDae, rumoca_phase_solve::SolveModelLowerError> {
    let copy_baseline = rumoca_phase_structural::dae_prepare::copy_accounting::counts();
    let PreparedStructuralDaes {
        source_dae,
        mut lowered,
        mut metadata_dae,
        mut reduced_constraints,
    } = prepare_structural_daes(dae_model)?;

    let elimination = eliminate_for_simulation(&mut lowered, dae_model)?;
    apply_simulation_elimination(&mut lowered, &elimination.substitutions)?;
    let causal_plan =
        rumoca_phase_structural::eliminate::factor_causal_substitutions_with_consumers(
            &source_dae,
            &lowered,
            &elimination.substitutions,
        )
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    resolve_reduced_constraint_substitutions(&mut reduced_constraints, &elimination.substitutions)?;
    rumoca_phase_structural::eliminate::factor_retained_computations_in_dae(
        &mut lowered,
        &elimination.substitutions,
        &causal_plan.retained_targets,
    )
    .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    trace_simulation_elimination(&lowered, &elimination.substitutions);
    log_solve_lowering_start("structural.restore_shared_causal_assignments");
    let timer = stage_timer_start();
    let restored_causal_targets =
        restore_shared_causal_assignments(&mut lowered, &source_dae, &metadata_dae, &causal_plan);
    log_solve_lowering_done("structural.restore_shared_causal_assignments", timer);
    tracing::debug!(
        target: "rumoca_sim::solve_lowering",
        candidates = causal_plan.retained_targets.len(),
        restored = restored_causal_targets.len(),
        "restored shared causal assignments"
    );
    // Resolve the observable inventory while `source_dae` is still live, then
    // release it together with the causal plan. Everything below reads only
    // `lowered` and `metadata_dae`, so the funnel never has to keep three
    // whole-DAE copies plus a scratch copy alive at the same time.
    //
    // This runs the observable inventory *before* the state-selection metadata
    // pass, where it used to run after. Both are fallible, so the swap is not
    // behaviour-preserving in general: for a model where both would fail, the
    // inventory's `Lower` error now surfaces where the metadata pass's
    // `Structural` error used to. The precedence is deliberate — an inventory
    // failure is a property of the source model's own declarations, so it names
    // something the author wrote, while a state-selection metadata failure
    // reports derived structural bookkeeping the author never sees.
    //
    // Only the inventory-fails half is pinned by a test
    // (`observable_inventory_falls_back_to_equation_spans`). Every failure the
    // metadata pass can raise — substitution application, unassignable
    // derivative-row demotion, constrained-dummy naming — is raised earlier in
    // this same funnel by `apply_simulation_elimination` on the solver view, so
    // reaching the metadata pass's copy of it needs a model whose
    // pre-elimination row set breaks where its post-elimination row set does
    // not. No such fixture is known.
    let visible_expressions = visible_expressions_after_elimination(
        &source_dae,
        &causal_plan.substitutions,
        &restored_causal_targets,
    )?;
    drop(causal_plan);
    drop(source_dae);

    mark_state_selection_metadata(&mut metadata_dae, &elimination.substitutions)?;
    scalarize_solver_view(&mut lowered, opts, "structural.scalarize_solve_dae")?;
    condense_scalar_algebraic_loops(&mut lowered)?;

    let structurally_lowered = StructurallyLoweredDae {
        dae: lowered.into_inner(),
        metadata_dae: metadata_dae.into_inner(),
        reduced_constraints,
        visible_expressions,
        copy_budget: FunnelCopyBudget::observed_since(copy_baseline),
    };
    structurally_lowered.trace_copy_budget();
    Ok(structurally_lowered)
}

fn eliminate_for_simulation(
    lowered: &mut dae::Dae,
    dae_model: &dae::Dae,
) -> Result<
    rumoca_phase_structural::eliminate::EliminationResult,
    rumoca_phase_solve::SolveModelLowerError,
> {
    log_solve_lowering_start("structural.eliminate_trivial");
    let timer = stage_timer_start();
    let elimination = eliminate_with_singular_state_selection(lowered)?;
    log_solve_lowering_done("structural.eliminate_trivial", timer);
    if let Some(source) = elimination.blt_error {
        if dae_model.variables.states.is_empty() {
            validate_residual_shapes_for_simulation(dae_model)?;
        }
        return Err(rumoca_phase_solve::SolveModelLowerError::Structural { source });
    }
    Ok(elimination)
}

/// Condense exact scalar algebraic BLT loops in the solver view.
///
/// `condense_scalar_algebraic_loops` copies the whole DAE once to build its own
/// BLT sort input, but only after it accepts the view: it declines — returning
/// an empty result, having copied nothing — as soon as any continuous row is
/// still array-shaped. That copy is made in a module the copy accounting does
/// not instrument, so it is recorded here against the same acceptance
/// condition. Reporting a funnel copy budget that is knowingly one short would
/// be worse than mirroring one predicate, and
/// `array_shaped_solver_view_declines_loop_condensation` pins the mirror
/// against the behaviour it mirrors.
fn condense_scalar_algebraic_loops(
    lowered: &mut dae::Dae,
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.condense_scalar_algebraic_loops");
    let timer = stage_timer_start();
    if solver_view_is_fully_scalar(lowered) {
        rumoca_phase_structural::dae_prepare::copy_accounting::record_dae_clone();
    }
    let condensation = rumoca_phase_structural::eliminate::condense_scalar_algebraic_loops(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.condense_scalar_algebraic_loops", timer);
    tracing::debug!(
        target: "rumoca_sim::solve_lowering",
        blocks = condensation.blocks.len(),
        causal_variables = condensation.causal_variable_count(),
        "condensed exact algebraic BLT loops"
    );
    Ok(())
}

/// The acceptance condition of
/// `rumoca_phase_structural::eliminate::condense_scalar_algebraic_loops`.
pub(super) fn solver_view_is_fully_scalar(dae_model: &dae::Dae) -> bool {
    dae_model
        .continuous
        .equations
        .iter()
        .all(|equation| equation.scalar_count == 1)
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
    let mut source_dae = FunnelDae::copied_from(dae_model);
    rumoca_phase_dae::attach_dae_reference_metadata(&mut source_dae)
        .map_err(metadata_attachment_lower_error)?;
    log_solve_lowering_done("structural.attach_dae_reference_metadata", timer);
    log_solve_lowering_start("structural.clone_source_for_lowered");
    let timer = stage_timer_start();
    let mut lowered = source_dae.clone();
    log_solve_lowering_done("structural.clone_source_for_lowered", timer);
    let reduced_constraints = rewrite_dae_for_structural_analysis(&mut lowered)?;
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
        reduced_constraints,
    })
}

fn resolve_reduced_constraint_substitutions(
    constraints: &mut [rumoca_phase_structural::dae_prepare::IndexReducedConstraint],
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    if constraints.is_empty() || substitutions.is_empty() {
        return Ok(());
    }
    let mut expressions = Vec::new();
    for constraint in constraints.iter() {
        expressions.push(constraint.holonomic.rhs.clone());
        if let Some(velocity) = &constraint.velocity {
            expressions.push(velocity.rhs.clone());
        }
    }
    rumoca_phase_structural::eliminate::resolve_substitutions_in_exprs(
        &mut expressions,
        substitutions,
    )
    .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    let mut expressions = expressions.into_iter();
    for constraint in constraints {
        let Some(holonomic) = expressions.next() else {
            return Err(rumoca_phase_solve::SolveModelLowerError::Lower(
                rumoca_phase_solve::LowerError::UnspannedContractViolation {
                    reason: "retained holonomic constraint inventory changed while applying \
                             structural substitutions"
                        .to_string(),
                },
            ));
        };
        constraint.holonomic.rhs = holonomic;
        if let Some(velocity) = &mut constraint.velocity {
            let Some(resolved_velocity) = expressions.next() else {
                return Err(rumoca_phase_solve::SolveModelLowerError::Lower(
                    rumoca_phase_solve::LowerError::UnspannedContractViolation {
                        reason: "retained velocity constraint inventory changed while applying \
                                 structural substitutions"
                            .to_string(),
                    },
                ));
            };
            velocity.rhs = resolved_velocity;
        }
    }
    if expressions.next().is_some() {
        return Err(rumoca_phase_solve::SolveModelLowerError::Lower(
            rumoca_phase_solve::LowerError::UnspannedContractViolation {
                reason: "retained constraint substitution produced unclaimed expressions"
                    .to_string(),
            },
        ));
    }
    Ok(())
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
    rumoca_phase_structural::dae_prepare::copy_accounting::record_scalarization();
    rumoca_phase_structural::dae_prepare::simplify_scalarized_continuous_equations(dae_model);
    log_solve_lowering_done(stage, timer);
    Ok(())
}

fn apply_simulation_elimination(
    lowered: &mut dae::Dae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
) -> Result<(), rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.apply_elimination_substitutions_to_dae");
    let timer = stage_timer_start();
    rumoca_phase_structural::eliminate::apply_elimination_substitutions_to_dae(
        lowered,
        substitutions,
    )
    .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done("structural.apply_elimination_substitutions_to_dae", timer);
    // State selection and algebraic elimination establish the independent
    // basis. Only now is derivative-row assignability meaningful; normalize
    // any derivative of a demoted target in the same structural stage.
    log_solve_lowering_start("structural.demote_states_without_retained_derivative_rows");
    let timer = stage_timer_start();
    rumoca_phase_structural::dae_prepare::demote_states_without_retained_derivative_rows(lowered)
        .map_err(|source| rumoca_phase_solve::SolveModelLowerError::Structural { source })?;
    log_solve_lowering_done(
        "structural.demote_states_without_retained_derivative_rows",
        timer,
    );
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
    metadata_dae: &mut FunnelDae,
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
    // The state-selection scratch copy has served its only purpose (naming the
    // constrained dummy states); release it before the metadata partition is
    // rewritten so the funnel does not carry a fourth live whole-DAE copy.
    drop(state_selection_dae);
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

/// Resolve the observable expression inventory directly against the borrowed
/// source DAE.
///
/// This deliberately does **not** copy and scalarize the source DAE first. The
/// visible inventory `rumoca_phase_solve::visible_expressions_for_dae` produces
/// is declaration-based: it walks `dae.variables` (states, algebraics and
/// outputs for the solver block, then inputs and discretes for the runtime
/// block) and expands each entry from that variable's own `dims`. It never
/// reads an equation to decide what is observable. The single equation-derived
/// quantity is the `solver_len` bound
/// (`solver_visible_scalar_count(dae).max(dae.continuous.equations.len())`),
/// which is used only as a `Vec::truncate` limit; because the produced solver
/// list is built from exactly those states/algebraics/outputs minus the
/// runtime-parameter-tail filter, its length is always
/// `<= solver_visible_scalar_count <= solver_len`, so the truncate is a no-op.
/// Neither `scalarize_equations` (which rewrites `continuous.equations`,
/// `continuous.structured_equations` and the event/clock expression slots) nor
/// `simplify_scalarized_continuous_equations` (which rewrites
/// `continuous.equations` only) touches `dae.variables`. Scalarizing a copy of
/// the source DAE therefore cannot change a single visible expression — it only
/// duplicates the whole expression graph and re-expands every array row.
///
/// The parameter is a [`FunnelDae`] rather than a plain `&dae::Dae` on purpose:
/// re-adding the copy this function used to make is then a `FunnelDae::clone`,
/// which the copy accounting sees and the funnel copy-budget ratchet rejects.
fn visible_expressions_after_elimination(
    source_dae: &FunnelDae,
    substitutions: &[rumoca_phase_structural::eliminate::Substitution],
    restored_causal_targets: &indexmap::IndexSet<rumoca_core::VarName>,
) -> Result<Vec<rumoca_phase_solve::VisibleExpression>, rumoca_phase_solve::SolveModelLowerError> {
    log_solve_lowering_start("structural.visible_expressions_for_dae");
    let timer = stage_timer_start();
    let mut visible_expressions = rumoca_phase_solve::visible_expressions_for_dae(source_dae)
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
