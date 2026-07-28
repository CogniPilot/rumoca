//! Symbolic elimination of trivially solvable equations.
//!
//! Two-phase pipeline:
//! 1. **Boundary resolution** — removes redundant equations (0 unknowns) and
//!    resolves trivial single-unknown equations, making structurally singular
//!    systems (from unconnected ports) amenable to BLT.
//! 2. **BLT scalar-block elimination** — uses the structural BLT decomposition
//!    to identify and eliminate scalar blocks in topological order.
//!
//! Solutions are substituted into remaining equations and the eliminated
//! equations/variables are removed from the DAE, producing a smaller,
//! better-conditioned system for the numerical solver.

use std::collections::HashSet;

use indexmap::{IndexMap, IndexSet};

use crate::static_eval::{eval_static_number, structural_scalar_bindings};
use rumoca_core::{
    ExpressionRewriter, FallibleExpressionRewriter, maybe_elapsed_seconds, maybe_start_timer_if,
};
use rumoca_ir_dae as dae;

mod aggregate_alias;
mod block_condensation;
mod boundary_scan;
mod causal_factor;
mod compact_family_blocks;
mod connection_policy;
mod diagnostics;
mod direct_definition_index;
mod flow_policy;
mod orphan_unknowns;
mod profiling;
mod runtime_known;
mod runtime_protection;
mod scalar_shape;
mod solve_for_unknown;
mod substitution_application;
mod substitution_target;
mod tearing_elimination;
mod unknown_index;

use aggregate_alias::{
    aggregate_definition_for_elimination, aggregate_variable_fully_resolved,
    is_scalarized_element_of_aggregate,
};
pub use block_condensation::{
    CondensedAlgebraicBlock, ScalarBlockCondensationResult, condense_scalar_algebraic_loops,
};
use boundary_scan::{BoundaryScanCtx, BoundaryScanState, scan_boundary_equations};
pub use causal_factor::{
    CausalSubstitutionPlan, factor_causal_substitutions,
    factor_causal_substitutions_with_consumers, factor_retained_computations_in_dae,
};
use compact_family_blocks::expand_compact_family_blocks;
use connection_policy::should_skip_connection_equation;
use diagnostics::trace_singular_reduced_rows;
use direct_definition_index::DirectDefinitionIndex;
use flow_policy::{expr_contains_indexed_multiscalar_ref, is_flow_equation_origin};
use orphan_unknowns::{drop_unreferenced_continuous_unknowns, output_partition_contains_unknown};
use profiling::{eliminate_profile_enabled, log_blt_profile, log_eliminate_profile};
use runtime_known::singular_rows_are_runtime_known_assignments;
use runtime_protection::{
    expr_references_any_discrete_name, expr_references_any_runtime_discrete_target,
    is_runtime_protected_unknown, runtime_defined_discrete_target_names,
    runtime_partition_or_event_refs_var, runtime_protected_unknown_names,
    should_preserve_runtime_known_assignment,
};
use scalar_shape::expression_is_scalar_after_subscripts;
pub use solve_for_unknown::try_solve_for_unknown;
pub(crate) use substitution_application::simplify_arithmetic_identities;
use substitution_application::{
    aggregate_constructor_reference, apply_aggregate_substitutions_to_dae_partitions,
    apply_substitutions_in_order_with_plan, apply_substitutions_to_dae_partitions,
    apply_substitutions_to_expressions_in_order, apply_substitutions_to_remaining_once,
    equation_analysis_expr,
};
use substitution_target::{
    expr_contains_derivative_substitution_target, expr_contains_substitution_target,
};
use tearing_elimination::{EliminationOutputs, tear_and_eliminate_loop_block};
use unknown_index::{
    BoundaryUnknownIndex, checked_count_live_unknowns, expression_references_boundary_unknown,
    find_live_scalar_unknowns, has_any_live_unknown,
};

use crate::variable_scope::{DaeVariableScope, DaeVariableShape, scalar_count_from_dims};
use crate::{BltBlock, EquationRef, StructuralError, UnknownId, sort_dae};

use rumoca_core::ExpressionVisitor;
#[cfg(test)]
use rumoca_ir_dae::expr_contains_der_of;
use rumoca_ir_dae::{
    DerivativeNameMatcher, expr_contains_der_of_any, expr_contains_var, split_complex_field_suffix,
    subscripts_all_one, var_ref_matches_unknown,
};

type Dae = dae::Dae;
type BuiltinFunction = rumoca_core::BuiltinFunction;
type Expression = rumoca_core::Expression;
type OpBinary = rumoca_core::OpBinary;
type OpUnary = rumoca_core::OpUnary;
type Reference = rumoca_core::Reference;
type VarName = rumoca_core::VarName;

/// A single symbolic substitution: `var_name = expr`.
#[derive(Debug, Clone)]
pub struct Substitution {
    /// The variable being eliminated.
    pub var_name: VarName,
    /// Structured component reference for the eliminated variable when it
    /// corresponds to a Modelica component.
    pub var_ref: Option<Reference>,
    /// The expression it equals (all prior substitutions already applied).
    pub expr: Expression,
    /// Dimensions of the eliminated variable, if known.
    pub var_dims: Vec<i64>,
    /// Dimensions of the replacement expression, if known.
    pub replacement_dims: Vec<i64>,
    /// Environment keys for this variable (e.g., `["z"]` or `["z[1]", "z[2]"]`).
    pub env_keys: Vec<String>,
}

/// Result of the symbolic elimination pass.
#[derive(Debug, Clone, Default)]
pub struct EliminationResult {
    /// Substitutions in evaluation order.
    pub substitutions: Vec<Substitution>,
    /// Number of equations/variables eliminated.
    pub n_eliminated: usize,
    /// BLT structural error that prevented Phase B from running.
    pub blt_error: Option<StructuralError>,
}

struct ZeroUnknownEliminationCtx<'a> {
    dae: &'a Dae,
    state_names: &'a [VarName],
    unknown_index: &'a BoundaryUnknownIndex<'a>,
    resolved: &'a HashSet<VarName>,
    runtime_protected_unknowns: &'a IndexSet<String>,
    runtime_defined_discrete_targets: &'a HashSet<String>,
    substitutions: &'a mut PlannedSubstitutions,
    eliminated_eq_indices: &'a mut Vec<usize>,
    eliminated_eq_flags: &'a mut [bool],
}

struct BltPreparation {
    /// BLT blocks the caller can eliminate through, or `None` when there is
    /// nothing to eliminate. This is deliberately `None` for array-shaped
    /// systems: those are sorted only to surface a structural singularity as
    /// `error`, and the caller eliminates through scalar blocks only, so
    /// handing them back would move a block decomposition nobody reads.
    blocks: Option<Vec<BltBlock>>,
    /// Blocks the sort produced, whether or not they were handed back.
    ///
    /// Profiling reports the size of the decomposition the sort actually
    /// computed. Reporting `blocks.len()` instead would silently report `0`
    /// for every array-shaped system, hiding exactly the sorts whose cost the
    /// profile exists to expose.
    sorted_block_count: usize,
    error: Option<StructuralError>,
}

/// Eliminate trivially solvable equations from the DAE.
///
/// Pipeline:
/// 1. `resolve_boundary_equations` — remove zero-unknown constraints and
///    solve single-unknown equations (ascending unknown-count order).
/// 2. `eliminate_via_blt` — BLT scalar-block elimination on the reduced system.
///
/// Mutates `dae` in place (removes equations and variables).
/// Returns substitution map for output reconstruction.
///
/// Must be called BEFORE scalarization, since `sort_dae` works with
/// base variable names (not expanded scalar names).
pub fn eliminate_trivial(dae: &mut Dae) -> Result<EliminationResult, StructuralError> {
    let trace = eliminate_trace_enabled();
    let profile = eliminate_profile_enabled();
    let t_total = maybe_start_timer_if(trace);
    let p_total = maybe_start_timer_if(profile);

    // Phase A: resolve boundary equations to make the system non-singular.
    let t_boundary = maybe_start_timer_if(trace);
    let p_boundary = maybe_start_timer_if(profile);
    let (mut result, direct_demoted) = resolve_boundary_and_direct_demotions_to_fixpoint(dae)?;
    log_eliminate_profile(
        profile,
        "boundary_fixpoint",
        p_boundary,
        result.n_eliminated,
    );
    if trace {
        crate::structural_trace!(
            "[sim-trace] eliminate_trivial boundary elapsed={:.3}s eliminated_eqs={} demoted_states={}",
            maybe_elapsed_seconds(t_boundary),
            result.n_eliminated,
            direct_demoted
        );
    }

    // Boundary substitutions can expose additional standalone `der(state)`
    // rows after the structural-preparation pass has already canonicalized
    // them. Keep one defining ODE row and rewrite the newly exposed copies to
    // its exact right-hand side before BLT matching.
    crate::dae_prepare::substitute_standalone_state_derivatives_in_non_ode_rows(dae);
    // Phase B: BLT scalar-block elimination on the reduced system.
    let prepared = prepare_blt_elimination(dae, trace, profile)?;
    if trace {
        crate::structural_trace!(
            "[sim-trace] eliminate_trivial blt preparation sorted_blocks={} eliminable={}",
            prepared.sorted_block_count,
            prepared.blocks.is_some()
        );
    }
    if let Some(blocks) = prepared.blocks {
        let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
        let t_blt = maybe_start_timer_if(trace);
        let p_blt = maybe_start_timer_if(profile);
        let blt_result = eliminate_via_blt(dae, &blocks, &state_names)?;
        log_eliminate_profile(profile, "eliminate_via_blt", p_blt, blt_result.n_eliminated);
        if trace {
            crate::structural_trace!(
                "[sim-trace] eliminate_trivial blt elapsed={:.3}s eliminated_eqs={}",
                maybe_elapsed_seconds(t_blt),
                blt_result.n_eliminated
            );
        }
        result.substitutions.extend(blt_result.substitutions);
        result.n_eliminated += blt_result.n_eliminated;
    }
    result.blt_error = prepared.error;
    let p_apply = maybe_start_timer_if(profile);
    apply_substitutions_to_dae_partitions(dae, &result.substitutions)?;
    log_eliminate_profile(
        profile,
        "apply_substitutions_to_partitions",
        p_apply,
        result.substitutions.len(),
    );
    log_eliminate_profile(profile, "total", p_total, result.n_eliminated);
    if trace {
        crate::structural_trace!(
            "[sim-trace] eliminate_trivial total elapsed={:.3}s eliminated_eqs={}",
            maybe_elapsed_seconds(t_total),
            result.n_eliminated
        );
    }

    Ok(result)
}

fn prepare_blt_elimination(
    dae: &Dae,
    trace: bool,
    profile: bool,
) -> Result<BltPreparation, StructuralError> {
    // Extract blocks from a clone before mutating the source DAE.
    let p_clone = maybe_start_timer_if(profile);
    let mut sort_input = crate::dae_prepare::copy_accounting::clone_dae(dae);
    log_eliminate_profile(
        profile,
        "clone_sort_input",
        p_clone,
        sort_input.continuous.equations.len(),
    );
    let p_drop = maybe_start_timer_if(profile);
    drop_unreferenced_continuous_unknowns(&mut sort_input)?;
    log_eliminate_profile(
        profile,
        "drop_unreferenced_unknowns",
        p_drop,
        sort_input.continuous.equations.len(),
    );
    let uses_scalar_view = sort_input
        .continuous
        .equations
        .iter()
        .any(|equation| equation.scalar_count != 1);
    if uses_scalar_view {
        crate::scalarize::scalarize_equations(&mut sort_input)?;
        crate::dae_prepare::copy_accounting::record_scalarization();
    }
    let p_sort = maybe_start_timer_if(profile);
    let mut error = None;
    let mut sorted_block_count = 0usize;
    // Array-shaped systems are sorted purely for the singularity check: the
    // caller eliminates through scalar blocks only, so hand the decomposition
    // straight back to the allocator instead of moving it out of `sorted`.
    // Profiling still sees the real decomposition first — the sort cost is the
    // same either way, and dropping the blocks must not drop their profile.
    let blocks = match sort_dae(&sort_input) {
        Ok(sorted) => {
            sorted_block_count = sorted.blocks.len();
            if profile {
                log_blt_block_profile(&sorted.blocks);
            }
            match (!uses_scalar_view).then_some(sorted.blocks) {
                Some(blocks) => Some(expand_compact_family_blocks(blocks, &sorted.matching)?),
                None => None,
            }
        }
        Err(StructuralError::EmptySystem) => None,
        Err(err) if singular_rows_are_runtime_known_assignments(&sort_input, &err) => None,
        Err(err) => {
            trace_singular_reduced_rows(trace, &sort_input, &err);
            error = Some(err);
            None
        }
    };
    log_eliminate_profile(profile, "sort_dae", p_sort, sorted_block_count);
    Ok(BltPreparation {
        blocks,
        sorted_block_count,
        error,
    })
}

fn log_blt_block_profile(blocks: &[BltBlock]) {
    // Compact family blocks stand in for many scalar blocks; report the scalar
    // count they represent so the profile stays comparable across the compact
    // and expanded representations.
    let n_scalar: usize = blocks.iter().map(BltBlock::scalar_block_count).sum();
    let n_structured = blocks
        .iter()
        .filter(|block| matches!(block, BltBlock::StructuredScalar(..)))
        .count();
    let mut n_loops = 0usize;
    let mut max_loop = 0usize;
    for size in blocks.iter().filter_map(BltBlock::loop_size) {
        n_loops += 1;
        max_loop = max_loop.max(size);
    }
    crate::structural_trace!(
        "[sim-profile] blt structured_family_blocks={n_structured} blocks={}",
        blocks.len()
    );
    log_blt_profile(true, n_scalar, n_loops, max_loop);
}

/// Eliminate a DAE and, only when maximum matching proves it singular, select
/// a minimal smooth holonomic state chain that strictly reduces the global
/// matching deficiency.
///
/// Candidate construction is Pantelides-style symbolic prolongation. The
/// matching proof is the selection rule: `StateSelect`, names, and equation
/// indices are used only to order equally regular candidates.
pub fn eliminate_trivial_with_state_selection(
    dae: &mut Dae,
) -> Result<EliminationResult, StructuralError> {
    let mut retained_derivative_values = crate::dae_prepare::isolated_state_derivative_values(dae);
    let mut cumulative = eliminate_trivial(dae)?;
    let Some(mut current_error) = cumulative.blt_error.take() else {
        return Ok(cumulative);
    };

    loop {
        let Some(baseline_defect) = matching_defect(&current_error) else {
            cumulative.blt_error = Some(current_error);
            return Ok(cumulative);
        };
        let profile = eliminate_profile_enabled();
        let p_candidates = maybe_start_timer_if(profile);
        drop_unresolvable_derivative_values(dae, &mut retained_derivative_values);
        let candidates =
            crate::dae_prepare::singular_holonomic_state_candidates_with_derivative_values(
                dae,
                &retained_derivative_values,
            )?;
        log_eliminate_profile(
            profile,
            "state_selection_candidate_generation",
            p_candidates,
            candidates.len(),
        );
        crate::structural_trace!(
            "[sim-trace] state selection candidates={} baseline_defect={:?}",
            candidates.len(),
            baseline_defect
        );
        let mut best: Option<StateSelectionTrial> = None;
        for candidate in candidates {
            let p_trial = maybe_start_timer_if(profile);
            let mut trial_dae = candidate.dae;
            let elimination = eliminate_trivial(&mut trial_dae)?;
            log_eliminate_profile(
                profile,
                "state_selection_candidate_trial",
                p_trial,
                candidate.demoted_states.len(),
            );
            let defect = elimination
                .blt_error
                .as_ref()
                .and_then(matching_defect)
                .unwrap_or(MatchingDefect::REGULAR);
            // State selection is a rank-improving equivalence transform, not a
            // way to trade a square singular system for a rectangular one.
            // Require a strict reduction in total unmatched rows/columns while
            // preserving (or reducing) the equation/unknown count mismatch.
            if !defect.strictly_improves_without_rectangularity_regression(baseline_defect) {
                continue;
            }
            let order = StateSelectionOrder {
                defect,
                n_demoted: candidate.demoted_states.len(),
                state_select: candidate
                    .demoted_states
                    .iter()
                    .map(|(rank, _)| *rank)
                    .collect(),
                state_names: candidate
                    .demoted_states
                    .iter()
                    .map(|(_, name)| name.as_str().to_string())
                    .collect(),
                constraint_index: candidate.constraint_index,
            };
            if best.as_ref().is_none_or(|selected| order < selected.order) {
                best = Some(StateSelectionTrial {
                    dae: trial_dae,
                    elimination,
                    order,
                });
            }
        }

        let Some(mut selected) = best else {
            cumulative.blt_error = Some(current_error);
            return Ok(cumulative);
        };
        crate::structural_trace!(
            "[sim-trace] state selection accepted states={:?} constraint={} defect={:?}->{:?}",
            selected.order.state_names,
            selected.order.constraint_index,
            baseline_defect,
            selected.order.defect
        );
        *dae = selected.dae;
        cumulative.n_eliminated += selected.elimination.n_eliminated;
        cumulative
            .substitutions
            .append(&mut selected.elimination.substitutions);
        let Some(next_error) = selected.elimination.blt_error.take() else {
            return Ok(cumulative);
        };
        current_error = next_error;
    }
}

/// Drop retained state-derivative values the current DAE can no longer resolve.
///
/// The values are read from the `der(x) = v` rows BEFORE elimination, because
/// elimination deletes those rows. When `v` was itself an alias of that
/// derivative (`v = der(x)`, e.g. a model output publishing a joint velocity),
/// elimination deletes `v` as well, and the retained value then names a variable
/// this DAE no longer has: it asserts `der(x) = der(x)` and carries no
/// information. Holonomic candidate construction reads these values against the
/// current DAE, so a dangling one must not survive into it.
fn drop_unresolvable_derivative_values(
    dae: &dae::Dae,
    values: &mut std::collections::HashMap<String, rumoca_core::Expression>,
) {
    values.retain(|_, value| {
        crate::dae_prepare::row_shape::expression_dims_for_row_count(dae, value).is_ok()
    });
}

#[derive(Debug, Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
struct MatchingDefect {
    unmatched_total: usize,
    rectangularity: usize,
}

impl MatchingDefect {
    const REGULAR: Self = Self {
        unmatched_total: 0,
        rectangularity: 0,
    };

    fn strictly_improves_without_rectangularity_regression(self, baseline: Self) -> bool {
        self.unmatched_total < baseline.unmatched_total
            && self.rectangularity <= baseline.rectangularity
    }
}

fn matching_defect(error: &StructuralError) -> Option<MatchingDefect> {
    let StructuralError::Singular {
        n_equations,
        n_unknowns,
        n_matched,
        ..
    } = error
    else {
        return None;
    };
    Some(MatchingDefect {
        unmatched_total: n_equations.saturating_sub(*n_matched)
            + n_unknowns.saturating_sub(*n_matched),
        rectangularity: n_equations.abs_diff(*n_unknowns),
    })
}

#[derive(Eq, Ord, PartialEq, PartialOrd)]
struct StateSelectionOrder {
    defect: MatchingDefect,
    n_demoted: usize,
    state_select: Vec<u8>,
    state_names: Vec<String>,
    constraint_index: usize,
}

struct StateSelectionTrial {
    dae: Dae,
    elimination: EliminationResult,
    order: StateSelectionOrder,
}

fn resolve_boundary_equations_to_fixpoint(
    dae: &mut Dae,
) -> Result<EliminationResult, StructuralError> {
    let mut result = EliminationResult::default();
    loop {
        let pass = resolve_boundary_equations(dae)?;
        if pass.n_eliminated == 0 {
            return Ok(result);
        }
        result.n_eliminated += pass.n_eliminated;
        result.substitutions.extend(pass.substitutions);
        // Aggregate values can be defined by scalar field substitutions found
        // in different fixed-point passes. Reapply the cumulative set so a
        // complete record value (for example Complex(re, im)) is reconstructed
        // before a later pass examines an equation that references its parent.
        apply_aggregate_substitutions_to_dae_partitions(dae, &result.substitutions)?;
    }
}

fn resolve_boundary_and_direct_demotions_to_fixpoint(
    dae: &mut Dae,
) -> Result<(EliminationResult, usize), StructuralError> {
    let mut result = EliminationResult::default();
    let mut total_demoted = 0usize;
    let profile = eliminate_profile_enabled();

    loop {
        let p_boundary = maybe_start_timer_if(profile);
        let pass = resolve_boundary_equations_to_fixpoint(dae)?;
        log_eliminate_profile(
            profile,
            "boundary_equations_to_fixpoint",
            p_boundary,
            pass.n_eliminated,
        );
        let eliminated = pass.n_eliminated;
        result.n_eliminated += pass.n_eliminated;
        result.substitutions.extend(pass.substitutions);

        let p_demote = maybe_start_timer_if(profile);
        let direct_demoted = crate::dae_prepare::demote_direct_assigned_states(dae)?;
        log_eliminate_profile(
            profile,
            "boundary_direct_demotion",
            p_demote,
            direct_demoted,
        );
        let demoted = direct_demoted;
        total_demoted += demoted;
        if eliminated == 0 && demoted == 0 {
            return Ok((result, total_demoted));
        }
    }
}

pub fn apply_elimination_substitutions_to_dae(
    dae: &mut Dae,
    substitutions: &[Substitution],
) -> Result<(), StructuralError> {
    apply_substitutions_to_dae_partitions(dae, substitutions)
}

fn eliminate_trace_enabled() -> bool {
    crate::structural_trace_enabled()
}

// ── Phase A: Boundary Resolution ────────────────────────────────────────

/// Remove redundant equations and resolve trivial single-unknown equations.
///
/// Processes equations in ascending order of unknown count:
/// - **0 unknowns**: removed (parameter-only constraint or redundant).
/// - **1 unknown**: solved symbolically via `try_solve_for_unknown` and
///   substituted into all remaining equations (cascade).
/// - **2+ unknowns**: left for BLT.
///
/// ODE equations (containing `der(state)`) are always skipped.
fn resolve_boundary_equations(dae: &mut Dae) -> Result<EliminationResult, StructuralError> {
    let profile = eliminate_profile_enabled();
    let p_unknowns = maybe_start_timer_if(profile);
    let all_unknowns = collect_boundary_unknowns(dae)?;
    log_eliminate_profile(
        profile,
        "boundary_collect_unknowns",
        p_unknowns,
        all_unknowns.len(),
    );
    let p_index = maybe_start_timer_if(profile);
    let unknown_index = BoundaryUnknownIndex::build(dae, &all_unknowns)?;
    log_eliminate_profile(
        profile,
        "boundary_build_unknown_index",
        p_index,
        all_unknowns.len(),
    );
    let p_runtime = maybe_start_timer_if(profile);
    let runtime_protected_unknowns = runtime_protected_unknown_names(dae)?;
    let runtime_defined_discrete_targets = runtime_defined_discrete_target_names(dae);
    log_eliminate_profile(
        profile,
        "boundary_runtime_protection_sets",
        p_runtime,
        runtime_protected_unknowns.len() + runtime_defined_discrete_targets.len(),
    );

    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    let state_derivative_matcher = DerivativeNameMatcher::from_var_names(&state_names);
    let p_direct = maybe_start_timer_if(profile);
    let direct_definitions = DirectDefinitionIndex::build(dae);
    log_eliminate_profile(
        profile,
        "boundary_direct_definition_index",
        p_direct,
        direct_definitions.len(),
    );
    // The operator-record constructor is a property of the symbol table, not of
    // the equations, so it is resolved once for the whole scan instead of once
    // per equation.
    let aggregate_constructor = aggregate_constructor_reference(dae);
    let mut scan_state = BoundaryScanState::new(
        dae.continuous.equations.len(),
        aggregate_constructor.as_ref(),
    );

    let p_order = maybe_start_timer_if(profile);
    let eq_order = boundary_equation_order(dae, &unknown_index, &scan_state.resolved)?;
    log_eliminate_profile(profile, "boundary_equation_order", p_order, eq_order.len());

    let p_loop = maybe_start_timer_if(profile);
    {
        let scan_ctx = BoundaryScanCtx {
            dae,
            state_names: &state_names,
            unknown_index: &unknown_index,
            state_derivative_matcher: &state_derivative_matcher,
            runtime_protected_unknowns: &runtime_protected_unknowns,
            runtime_defined_discrete_targets: &runtime_defined_discrete_targets,
            direct_definitions: &direct_definitions,
        };
        scan_boundary_equations(eq_order, &scan_ctx, &mut scan_state)?;
    }
    log_eliminate_profile(
        profile,
        "boundary_scan_equations",
        p_loop,
        scan_state.substitutions.len(),
    );

    let p_finish = maybe_start_timer_if(profile);
    let result = finish_boundary_elimination(
        dae,
        scan_state.substitutions.into_vec(),
        scan_state.eliminated_eq_flags,
        scan_state.eliminated_eq_indices,
        &scan_state.resolved,
    )?;
    log_eliminate_profile(profile, "boundary_finish", p_finish, result.n_eliminated);
    Ok(result)
}

fn boundary_equation_order(
    dae: &Dae,
    unknown_index: &BoundaryUnknownIndex<'_>,
    resolved: &HashSet<VarName>,
) -> Result<Vec<(usize, usize)>, StructuralError> {
    let mut eq_order: Vec<(usize, usize)> = (0..dae.continuous.equations.len())
        .map(|eq_idx| {
            let expr = equation_analysis_expr(&dae.continuous.equations[eq_idx]);
            checked_count_live_unknowns(&expr, unknown_index, resolved).map(|count| (eq_idx, count))
        })
        .collect::<Result<_, StructuralError>>()?;
    eq_order.sort_by_key(|&(_, count)| count);
    Ok(eq_order)
}

fn collect_boundary_unknowns(dae: &Dae) -> Result<Vec<VarName>, StructuralError> {
    let mut unknowns = Vec::new();
    for (name, var) in dae
        .variables
        .algebraics
        .iter()
        .chain(dae.variables.outputs.iter())
    {
        let scalar_count = scalar_count_from_dims(name, &var.dims)?;
        if scalar_count <= 1 {
            unknowns.push(name.clone());
            continue;
        }
        for flat_index in 0..scalar_count {
            unknowns.push(VarName::new(dae::scalar_name_text_for_flat_index(
                name.as_str(),
                &var.dims,
                flat_index,
            )));
        }
    }
    Ok(unknowns)
}

/// Keep `continuous.structured_equations` pointing at their (now-compacted) equation
/// blocks after `removed_sorted` equations were removed from `continuous.equations`.
///
/// A family whose block stays intact is shifted down by the number of removed
/// equations positioned strictly before it. Most trivial/boundary eliminations are
/// scalar (`x = const`, aliases) sitting outside any family block, so only the start
/// index moves; without this a method-of-lines interior `der` family would silently
/// absorb the adjacent boundary `der` row and compute it with the wrong body.
///
/// A family one of whose own rows is removed (e.g. a constant `for k loop a[k]=k*c`
/// family folded away) can no longer describe a contiguous array block, so it is
/// dropped: the surviving rows lower as plain scalars rather than indexing a hole.
fn shift_structured_families_after_equation_removal(
    dae: &mut Dae,
    removed_sorted: &[usize],
) -> Result<(), StructuralError> {
    if removed_sorted.is_empty() {
        return Ok(());
    }
    for family in &dae.continuous.structured_equations {
        if !family.interiors_materialized && family_touches_equations(family, removed_sorted)? {
            return Err(structured_family_scalarization_error(
                family,
                "equation removal",
            ));
        }
    }
    let mut remapped = Vec::with_capacity(dae.continuous.structured_equations.len());
    for mut family in dae.continuous.structured_equations.iter().cloned() {
        let block_end = structured_family_block_end(&family)?;
        let removed_inside_block = removed_sorted
            .iter()
            .any(|&idx| idx >= family.first_equation_index && idx < block_end);
        if removed_inside_block {
            continue;
        }
        let shift = removed_sorted
            .iter()
            .filter(|&&idx| idx < family.first_equation_index)
            .count();
        family.first_equation_index =
            family
                .first_equation_index
                .checked_sub(shift)
                .ok_or_else(|| {
                    structured_family_metadata_error(
                        &family,
                        "equation-removal remap underflowed the family start index",
                    )
                })?;
        remapped.push(family);
    }
    dae.continuous.structured_equations = remapped;
    Ok(())
}

/// Reconcile structured families after symbolic rewrites.
///
/// Materialized family rows are authoritative, so rewriting one invalidates
/// that family's compact proof and drops its metadata. A non-materialized
/// family's scalar rows are only a derived view: arithmetic normalization of
/// those rows does not invalidate the canonical template. Such a family is
/// rejected only when substitution changes that authoritative template, since
/// placeholder interiors cannot become a scalar fallback.
fn drop_structured_families_touching_equations(
    dae: &mut Dae,
    touched_sorted: &[usize],
    canonical_touched_families: &[usize],
) -> Result<(), StructuralError> {
    if let Some(retained) =
        structured_families_surviving_substitution(dae, touched_sorted, canonical_touched_families)?
    {
        dae.continuous.structured_equations = retained;
    }
    Ok(())
}

/// Decide [`drop_structured_families_touching_equations`] without mutating.
///
/// Returns the replacement family list, or `None` when nothing was touched and
/// the list stands. Split out so a caller that must not leave a half-rewritten
/// DAE behind can raise the family-scalarization error while the DAE is still
/// pristine, then commit the surviving list together with its equations.
fn structured_families_surviving_substitution(
    dae: &Dae,
    touched_sorted: &[usize],
    canonical_touched_families: &[usize],
) -> Result<Option<Vec<dae::StructuredEquationFamily>>, StructuralError> {
    if touched_sorted.is_empty() {
        return Ok(None);
    }
    for (family_index, family) in dae.continuous.structured_equations.iter().enumerate() {
        if !family.interiors_materialized
            && canonical_touched_families.contains(&family_index)
            && family_touches_equations(family, touched_sorted)?
        {
            return Err(structured_family_scalarization_error(
                family,
                "symbolic substitution",
            ));
        }
    }
    let mut retained = Vec::with_capacity(dae.continuous.structured_equations.len());
    for family in dae.continuous.structured_equations.iter().cloned() {
        if !family_touches_equations(&family, touched_sorted)? || !family.interiors_materialized {
            retained.push(family);
        }
    }
    Ok(Some(retained))
}

fn family_touches_equations(
    family: &dae::StructuredEquationFamily,
    equation_indices: &[usize],
) -> Result<bool, StructuralError> {
    let block_end = structured_family_block_end(family)?;
    Ok(equation_indices
        .iter()
        .any(|&idx| idx >= family.first_equation_index && idx < block_end))
}

fn structured_family_block_end(
    family: &dae::StructuredEquationFamily,
) -> Result<usize, StructuralError> {
    let row_count = family.scalar_view_row_count().map_err(|source| {
        structured_family_metadata_error(
            family,
            &format!("invalid structured domain while deriving row count: {source}"),
        )
    })?;
    family
        .first_equation_index
        .checked_add(row_count)
        .ok_or_else(|| {
            structured_family_metadata_error(family, "structured family row range overflows")
        })
}

fn structured_family_metadata_error(
    family: &dae::StructuredEquationFamily,
    detail: &str,
) -> StructuralError {
    let reason = format!(
        "invalid structured family metadata for `{}`: {detail}",
        family.origin
    );
    if family.span.is_dummy() {
        StructuralError::UnspannedContractViolation { reason }
    } else {
        StructuralError::ContractViolation {
            reason,
            span: family.span,
        }
    }
}

fn structured_family_scalarization_error(
    family: &dae::StructuredEquationFamily,
    operation: &str,
) -> StructuralError {
    let reason = format!(
        "{operation} touched cheapened structured family `{}`; placeholder interior rows cannot \
         become authoritative scalar equations",
        family.origin
    );
    if family.span.is_dummy() {
        StructuralError::UnspannedContractViolation { reason }
    } else {
        StructuralError::ContractViolation {
            reason,
            span: family.span,
        }
    }
}

fn finish_boundary_elimination(
    dae: &mut Dae,
    substitutions: Vec<Substitution>,
    eliminated_eq_flags: Vec<bool>,
    mut eliminated_eq_indices: Vec<usize>,
    resolved: &HashSet<VarName>,
) -> Result<EliminationResult, StructuralError> {
    apply_substitutions_to_remaining_once(dae, &eliminated_eq_flags, &substitutions)?;
    let n_eliminated = eliminated_eq_indices.len();
    eliminated_eq_indices.sort_unstable();
    shift_structured_families_after_equation_removal(dae, &eliminated_eq_indices)?;
    for &idx in eliminated_eq_indices.iter().rev() {
        dae.continuous.equations.remove(idx);
    }
    for name in fully_resolved_continuous_unknowns(dae, resolved)? {
        dae.variables.algebraics.shift_remove(&name);
        dae.variables.outputs.shift_remove(&name);
    }
    Ok(EliminationResult {
        substitutions,
        n_eliminated,
        blt_error: None,
    })
}

fn fully_resolved_continuous_unknowns(
    dae: &Dae,
    resolved: &HashSet<VarName>,
) -> Result<IndexSet<VarName>, StructuralError> {
    let mut removable = IndexSet::new();
    for (name, var) in dae
        .variables
        .algebraics
        .iter()
        .chain(dae.variables.outputs.iter())
    {
        if resolved.contains(name) || aggregate_variable_fully_resolved(name, var, resolved)? {
            removable.insert(name.clone());
        }
    }
    Ok(removable)
}

pub(super) fn full_var_ref(expr: &Expression) -> Option<&Reference> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name),
        _ => None,
    }
}

fn same_aggregate_shape(dae: &Dae, lhs: &VarName, rhs: &VarName) -> Result<bool, StructuralError> {
    let Some(lhs_var) = dae_var(dae, lhs) else {
        return Ok(false);
    };
    let Some(rhs_var) = dae_var(dae, rhs) else {
        return Ok(false);
    };
    let lhs_dims = &lhs_var.dims;
    Ok(!lhs_dims.is_empty() && lhs_dims == &rhs_var.dims)
}

fn aggregate_alias_candidate(
    dae: &Dae,
    eliminated: &Reference,
    replacement: &Reference,
    replacement_span: Option<rumoca_core::Span>,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> Result<Option<(VarName, Expression)>, StructuralError> {
    let var_name = eliminated.var_name();
    if !can_eliminate_aggregate_alias_var(
        dae,
        var_name,
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    ) {
        return Ok(None);
    }
    Ok(Some((
        var_name.clone(),
        Expression::VarRef {
            name: replacement.clone(),
            subscripts: Vec::new(),
            span: aggregate_alias_span(replacement, replacement_span)?,
        },
    )))
}

fn aggregate_alias_span(
    replacement: &Reference,
    span: Option<rumoca_core::Span>,
) -> Result<rumoca_core::Span, StructuralError> {
    span.filter(|span| !span.is_dummy()).ok_or_else(|| {
        StructuralError::UnspannedContractViolation {
            reason: format!(
                "cannot eliminate aggregate alias without source provenance for replacement `{}`",
                replacement.as_str()
            ),
        }
    })
}

fn can_eliminate_aggregate_alias_var(
    dae: &Dae,
    var_name: &VarName,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    !unknown_is_fixed(dae, var_name)
        && !dae.variables.states.contains_key(var_name)
        && !is_runtime_protected_unknown(var_name, runtime_protected_unknowns)
        && !runtime_defined_discrete_targets.contains(var_name.as_str())
        && !runtime_partition_or_event_refs_var(dae, var_name)
        && (dae.variables.algebraics.contains_key(var_name)
            || dae.variables.outputs.contains_key(var_name))
}

fn preferred_aggregate_alias_candidate(
    lhs: (VarName, Expression),
    rhs: (VarName, Expression),
) -> (VarName, Expression) {
    let lhs_rank = aggregate_alias_rank(&lhs.0);
    let rhs_rank = aggregate_alias_rank(&rhs.0);
    if lhs_rank >= rhs_rank { lhs } else { rhs }
}

fn aggregate_alias_rank(name: &VarName) -> (usize, usize) {
    let path = rumoca_core::ComponentPath::from_flat_path(name.as_str());
    (path.len(), name.as_str().len())
}

fn try_eliminate_zero_unknown_equation(
    eq_idx: usize,
    eq_rhs: &Expression,
    has_state_derivative: bool,
    ctx: &mut ZeroUnknownEliminationCtx<'_>,
) -> Result<(), StructuralError> {
    let references_state_value = ctx
        .state_names
        .iter()
        .any(|sn| expr_contains_var(eq_rhs, sn));
    if has_state_derivative
        || references_state_value
        || has_any_live_unknown(eq_rhs, ctx.unknown_index, ctx.resolved)?
        || expr_contains_indexed_multiscalar_ref(eq_rhs, ctx.dae)?
    {
        return Ok(());
    }
    // MLS Appendix B / §8.3 / §16.5.1: a zero-unknown equation may still
    // define a live runtime discrete/event value. Do not drop those rows
    // unless they can be substituted safely through every runtime consumer.
    if should_preserve_runtime_known_assignment(ctx.dae, eq_rhs) {
        return Ok(());
    }
    let n_subs_before = ctx.substitutions.len();
    maybe_push_non_unknown_alias_substitution(
        ctx.dae,
        eq_rhs,
        ctx.runtime_protected_unknowns,
        ctx.runtime_defined_discrete_targets,
        ctx.substitutions,
    )?;
    if ctx.substitutions.len() == n_subs_before {
        let bindings = structural_scalar_bindings(ctx.dae);
        let simplified = simplify_arithmetic_identities(eq_rhs.clone());
        match eval_static_number(&simplified, &bindings) {
            Some(value) if value.is_finite() && value == 0.0 => {}
            Some(value) if value.is_finite() => {
                let equation = &ctx.dae.continuous.equations[eq_idx];
                return Err(StructuralError::InconsistentEquation {
                    residual: value,
                    origin: equation.origin.clone(),
                    span: equation.span,
                });
            }
            Some(_) | None => return Ok(()),
        }
    }
    ctx.eliminated_eq_indices.push(eq_idx);
    ctx.eliminated_eq_flags[eq_idx] = true;
    Ok(())
}

fn choose_solvable_unknown_for_elimination(
    dae: &Dae,
    eq_idx: usize,
    rhs: &Expression,
    live: &[VarName],
    has_state_derivative: bool,
    runtime_protected_unknowns: &IndexSet<String>,
    direct_definitions: &DirectDefinitionIndex,
) -> Result<Option<(VarName, Expression)>, StructuralError> {
    let mut candidates: Vec<&VarName> = live.iter().collect();
    candidates.sort_by(|a, b| {
        let a_has_definition = direct_definitions.has_other_direct_definition(eq_idx, a);
        let b_has_definition = direct_definitions.has_other_direct_definition(eq_idx, b);
        let a_is_output = output_partition_contains_unknown(dae, a);
        let b_is_output = output_partition_contains_unknown(dae, b);
        a_has_definition
            .cmp(&b_has_definition)
            .then_with(|| b_is_output.cmp(&a_is_output))
            .then_with(|| a.as_str().cmp(b.as_str()))
    });

    for candidate in candidates {
        // `fixed=true` introduces a hard initialization constraint. Eliminating
        // that unknown can erase user intent (especially through alias chains)
        // and alter the selected initialization branch.
        if unknown_is_fixed(dae, candidate)
            && !fixed_alias_constraint_is_duplicated_by_peer(dae, rhs, candidate)
        {
            continue;
        }
        if dae.variables.states.contains_key(candidate) {
            continue;
        }
        if is_scalarized_element_of_aggregate(dae, candidate)? {
            continue;
        }
        // MLS §10.6: an array or record equation denotes one scalar equation
        // per element. Eliminating its whole row is sound only when the solved
        // target has the same scalar cardinality. In particular, do not solve
        // one record field and discard the other component equations.
        if dae.continuous.equations[eq_idx].scalar_count
            != DaeVariableScope::new(dae).size(candidate)?
        {
            continue;
        }
        if is_runtime_protected_unknown(candidate, runtime_protected_unknowns) {
            continue;
        }
        let is_output = output_partition_contains_unknown(dae, candidate);
        // Skip equations with state derivatives — unless the candidate is an
        // output that forms a direct alias (e.g. `output y = der(x)`), which
        // can be safely eliminated.
        if has_state_derivative && !is_output {
            continue;
        }
        // Try the simple top-level Sub pattern first; fall back to the additive
        // solver so substitution residues like `x - (y - 0)` (which the simple
        // pattern can't see through) still resolve. The additive solver is gated
        // by `live` to avoid accidentally solving a multi-unknown equation.
        let Some(solution) = try_solve_for_unknown(rhs, candidate) else {
            continue;
        };
        if expr_contains_var(&solution, candidate) {
            continue;
        }
        let direct_assignment_solution = has_direct_assignment_form(rhs, candidate);
        // Output variables exist for external callers — only eliminate them
        // when the solution is a trivial alias (a single variable reference or
        // its negation), since keeping non-trivial outputs enlarges the DAE and
        // can hurt solver performance.
        if is_output && !is_trivial_alias(&solution) {
            continue;
        }
        if !direct_assignment_solution && !is_symbolically_stable_solution(&solution) {
            continue;
        }
        if expr_contains_unsliced_multiscalar_ref(&solution, dae)? {
            continue;
        }
        if expr_contains_indexed_multiscalar_ref(&solution, dae)?
            && !(is_trivial_alias(&solution)
                && expression_is_scalar_after_subscripts(&solution, dae)?)
        {
            continue;
        }
        if live.len() > 1 && !direct_assignment_solution {
            continue;
        }
        return Ok(Some((candidate.clone(), solution)));
    }
    Ok(None)
}

fn choose_solvable_non_unknown_alias_for_elimination(
    dae: &Dae,
    rhs: &Expression,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> Result<Option<(VarName, Expression)>, StructuralError> {
    let Expression::Binary {
        op, lhs, rhs: r, ..
    } = rhs
    else {
        return Ok(None);
    };
    if !matches!(op, OpBinary::Sub) {
        return Ok(None);
    }

    let mut candidates: Vec<Reference> = Vec::with_capacity(2);
    if let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
        && subscripts.is_empty()
    {
        candidates.push(name.clone());
    }
    if let Expression::VarRef {
        name, subscripts, ..
    } = r.as_ref()
        && subscripts.is_empty()
        && !candidates
            .iter()
            .any(|existing| existing.var_name() == name.var_name())
    {
        candidates.push(name.clone());
    }

    let scope = DaeVariableScope::new(dae);
    for candidate_ref in candidates {
        let candidate = candidate_ref.var_name().clone();
        if candidate.as_str() == "time" {
            continue;
        }
        if is_runtime_protected_unknown(&candidate, runtime_protected_unknowns) {
            continue;
        }
        if dae.variables.parameters.contains_key(&candidate)
            || dae.variables.constants.contains_key(&candidate)
        {
            continue;
        }
        if dae.variables.states.contains_key(&candidate) {
            continue;
        }
        if runtime_defined_discrete_targets.contains(candidate.as_str()) {
            continue;
        }
        match scope.size_for_reference(&candidate_ref)? {
            Some(size) if size > 1 => continue,
            Some(_) => {}
            None => continue,
        }

        let Some(solution) = try_solve_for_unknown(rhs, &candidate) else {
            continue;
        };
        if expr_contains_var(&solution, &candidate) {
            continue;
        }
        if expr_contains_unsliced_multiscalar_ref(&solution, dae)? {
            continue;
        }
        if !is_symbolically_stable_solution(&solution) {
            continue;
        }
        return Ok(Some((candidate, solution)));
    }

    Ok(None)
}

fn maybe_push_non_unknown_alias_substitution(
    dae: &Dae,
    eq_rhs: &Expression,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
    substitutions: &mut PlannedSubstitutions,
) -> Result<(), StructuralError> {
    let Some((var_name, solution)) = choose_solvable_non_unknown_alias_for_elimination(
        dae,
        eq_rhs,
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    )?
    else {
        return Ok(());
    };
    substitutions.push(substitution_for_var(dae, var_name.clone(), solution)?);
    Ok(())
}

fn unknown_is_fixed(dae: &Dae, name: &VarName) -> bool {
    dae.variables
        .states
        .get(name)
        .or_else(|| dae.variables.algebraics.get(name))
        .or_else(|| dae.variables.outputs.get(name))
        .and_then(|var| var.fixed)
        .unwrap_or(false)
}

fn fixed_alias_constraint_is_duplicated_by_peer(
    dae: &Dae,
    rhs: &Expression,
    candidate: &VarName,
) -> bool {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = rhs
    else {
        return false;
    };
    let Some(lhs_ref) = full_var_ref(lhs) else {
        return false;
    };
    let Some(rhs_ref) = full_var_ref(rhs) else {
        return false;
    };
    let peer = if lhs_ref.var_name() == candidate {
        rhs_ref.var_name()
    } else if rhs_ref.var_name() == candidate {
        lhs_ref.var_name()
    } else {
        return false;
    };
    let Some(candidate_var) = continuous_variable(dae, candidate) else {
        return false;
    };
    let Some(peer_var) = continuous_variable(dae, peer) else {
        return false;
    };
    if candidate_var.fixed != Some(true) || peer_var.fixed != Some(true) {
        return false;
    }
    match (&candidate_var.start, &peer_var.start) {
        (Some(candidate_start), Some(peer_start)) => {
            rumoca_core::expressions_semantically_equal(candidate_start, peer_start)
        }
        (None, None) => true,
        (Some(_), None) | (None, Some(_)) => false,
    }
}

fn continuous_variable<'a>(dae: &'a Dae, name: &VarName) -> Option<&'a dae::Variable> {
    dae.variables
        .states
        .get(name)
        .or_else(|| dae.variables.algebraics.get(name))
        .or_else(|| dae.variables.outputs.get(name))
}

fn has_direct_assignment_form(rhs: &Expression, candidate: &VarName) -> bool {
    match rhs {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => is_assignment_target(lhs, candidate) || is_assignment_target(rhs, candidate),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => has_direct_assignment_form(rhs, candidate),
        _ => false,
    }
}

fn is_assignment_target(expr: &Expression, candidate: &VarName) -> bool {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => var_ref_matches_unknown(name, subscripts, candidate),
        _ => false,
    }
}

/// Returns true if the expression is a single variable reference or its
/// negation — i.e., a trivial alias like `x` or `-x`.
fn is_trivial_alias(expr: &Expression) -> bool {
    match expr {
        Expression::VarRef { .. } => true,
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => is_trivial_alias(rhs),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } => args.len() == 1 && matches!(&args[0], Expression::VarRef { .. }),
        _ => false,
    }
}

fn is_symbolically_stable_solution(expr: &Expression) -> bool {
    match expr {
        Expression::If { .. } => false,
        Expression::BuiltinCall { function, args, .. } => {
            !matches!(
                function,
                rumoca_core::BuiltinFunction::Smooth
                    | rumoca_core::BuiltinFunction::NoEvent
                    | rumoca_core::BuiltinFunction::Homotopy
            ) && args.iter().all(is_symbolically_stable_solution)
        }
        Expression::Binary { lhs, rhs, .. } => {
            is_symbolically_stable_solution(lhs) && is_symbolically_stable_solution(rhs)
        }
        Expression::Unary { rhs, .. } => is_symbolically_stable_solution(rhs),
        Expression::FunctionCall { args, .. } => args.iter().all(is_symbolically_stable_solution),
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            elements.iter().all(is_symbolically_stable_solution)
        }
        Expression::Range {
            start, step, end, ..
        } => {
            is_symbolically_stable_solution(start)
                && step.as_deref().is_none_or(is_symbolically_stable_solution)
                && is_symbolically_stable_solution(end)
        }
        Expression::ArrayComprehension { expr, filter, .. } => {
            is_symbolically_stable_solution(expr)
                && filter
                    .as_deref()
                    .is_none_or(is_symbolically_stable_solution)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            is_symbolically_stable_solution(base)
                && subscripts.iter().all(|sub| match sub {
                    rumoca_core::Subscript::Expr { expr, .. } => {
                        is_symbolically_stable_solution(expr)
                    }
                    _ => true,
                })
        }
        Expression::FieldAccess { base, .. } => is_symbolically_stable_solution(base),
        Expression::VarRef { .. }
        | Expression::Literal { value: _, .. }
        | Expression::Empty { .. } => true,
    }
}

pub(super) fn collect_var_ref_nodes(
    expr: &Expression,
    out: &mut Vec<(Reference, Vec<rumoca_core::Subscript>)>,
) {
    struct Collector<'out> {
        out: &'out mut Vec<(Reference, Vec<rumoca_core::Subscript>)>,
    }

    impl ExpressionVisitor for Collector<'_> {
        fn visit_var_ref(&mut self, name: &Reference, subscripts: &[rumoca_core::Subscript]) {
            self.out.push((name.clone(), subscripts.to_vec()));
            self.walk_var_ref(name, subscripts);
        }
    }

    Collector { out }.visit_expression(expr);
}

fn dae_var_size(dae: &Dae, name: &VarName) -> Result<usize, StructuralError> {
    DaeVariableScope::new(dae).size(name)
}

fn dae_var<'a>(dae: &'a Dae, name: &VarName) -> Option<&'a dae::Variable> {
    DaeVariableScope::new(dae).exact(name)
}

pub(super) fn substitution_for_var(
    dae: &Dae,
    var_name: VarName,
    expr: Expression,
) -> Result<Substitution, StructuralError> {
    let scope = DaeVariableScope::new(dae);
    let var_dims = scope.dims(&var_name)?;
    let replacement_dims = replacement_expr_dims(dae, &expr, &var_dims)?;
    Ok(Substitution {
        replacement_dims,
        var_dims,
        env_keys: vec![var_name.as_str().to_string()],
        var_ref: scope
            .exact(&var_name)
            .and_then(|var| var.component_ref.clone())
            .map(Reference::from_component_reference),
        var_name,
        expr,
    })
}

fn replacement_expr_dims(
    dae: &Dae,
    expr: &Expression,
    expected_dims: &[i64],
) -> Result<Vec<i64>, StructuralError> {
    Ok(match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => DaeVariableScope::new(dae)
            .dims_for_reference(name)?
            .unwrap_or_else(|| expected_dims.to_vec()),
        Expression::VarRef { .. } | Expression::Index { .. } => Vec::new(),
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => array_expr_dims(elements, *is_matrix),
        _ => Vec::new(),
    })
}

fn array_expr_dims(elements: &[Expression], is_matrix: bool) -> Vec<i64> {
    if !is_matrix {
        return vec![elements.len() as i64];
    }
    let cols = match elements.first() {
        Some(Expression::Array { elements, .. }) => elements.len(),
        _ => return vec![elements.len() as i64],
    };
    vec![elements.len() as i64, cols as i64]
}

// ── Phase B: BLT Scalar-Block Elimination ───────────────────────────────

/// Eliminate scalar blocks identified by BLT analysis.
///
/// Walks the BLT blocks in topological order. For each scalar block
/// with an algebraic/output unknown, tries to solve the equation
/// symbolically and substitutes the solution into remaining equations.
fn eliminate_via_blt(
    dae: &mut Dae,
    blocks: &[BltBlock],
    state_names: &[VarName],
) -> Result<EliminationResult, StructuralError> {
    let state_derivative_matcher = DerivativeNameMatcher::from_var_names(state_names);
    let runtime_protected_unknowns = runtime_protected_unknown_names(dae)?;
    let runtime_defined_discrete_targets = runtime_defined_discrete_target_names(dae);
    let aggregate_constructor = aggregate_constructor_reference(dae);
    let mut substitutions = PlannedSubstitutions::new(aggregate_constructor.as_ref());
    let mut eliminated_eq_indices: Vec<usize> = Vec::new();
    let mut eliminated_eq_flags = vec![false; dae.continuous.equations.len()];
    let mut eliminated_var_names: Vec<VarName> = Vec::new();

    for block in blocks {
        match block {
            BltBlock::Scalar { equation, unknown } => eliminate_scalar_blt_block(
                dae,
                equation,
                unknown,
                &runtime_protected_unknowns,
                &runtime_defined_discrete_targets,
                &state_derivative_matcher,
                EliminationOutputs {
                    substitutions: &mut substitutions,
                    eliminated_eq_indices: &mut eliminated_eq_indices,
                    eliminated_eq_flags: &mut eliminated_eq_flags,
                    eliminated_var_names: &mut eliminated_var_names,
                },
            )?,
            BltBlock::AlgebraicLoop {
                equations,
                unknowns,
            } => tear_and_eliminate_loop_block(
                dae,
                equations,
                unknowns,
                &runtime_protected_unknowns,
                &state_derivative_matcher,
                EliminationOutputs {
                    substitutions: &mut substitutions,
                    eliminated_eq_indices: &mut eliminated_eq_indices,
                    eliminated_eq_flags: &mut eliminated_eq_flags,
                    eliminated_var_names: &mut eliminated_var_names,
                },
            )?,
            // `prepare_blt_elimination` expands every compact family block into
            // its scalar rows before handing the decomposition over, so a
            // compact block reaching here means the caller skipped that step.
            // Skipping the block would silently leave the family's rows in the
            // DAE; report instead.
            // TODO(P3): family-level elimination, substituting one compact
            // solution for the whole family instead of expanding it first.
            BltBlock::StructuredScalar(family) => {
                return Err(StructuralError::ContractViolation {
                    reason: family.unsupported_by("BLT scalar-block elimination"),
                    span: family.span,
                });
            }
        }
    }

    // Apply BLT substitutions once to the remaining equations.
    apply_substitutions_to_remaining_once(dae, &eliminated_eq_flags, substitutions.as_slice())?;

    let n_eliminated = eliminated_eq_indices.len();

    // Remove eliminated equations (in reverse order to preserve indices).
    eliminated_eq_indices.sort_unstable();
    shift_structured_families_after_equation_removal(dae, &eliminated_eq_indices)?;
    for &idx in eliminated_eq_indices.iter().rev() {
        dae.continuous.equations.remove(idx);
    }

    // Remove eliminated variables from algebraics and outputs.
    for name in &eliminated_var_names {
        dae.variables.algebraics.shift_remove(name);
        dae.variables.outputs.shift_remove(name);
    }

    Ok(EliminationResult {
        substitutions: substitutions.into_vec(),
        n_eliminated,
        blt_error: None,
    })
}

fn eliminate_scalar_blt_block(
    dae: &Dae,
    equation: &EquationRef,
    unknown: &UnknownId,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
    state_derivative_matcher: &DerivativeNameMatcher,
    outputs: EliminationOutputs<'_>,
) -> Result<(), StructuralError> {
    let EliminationOutputs {
        substitutions,
        eliminated_eq_indices,
        eliminated_eq_flags,
        eliminated_var_names,
    } = outputs;
    let Some((eq_idx, var_name, solution)) = scalar_blt_solution(
        dae,
        equation,
        unknown,
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
        state_derivative_matcher,
        substitutions,
    )?
    else {
        return Ok(());
    };
    substitutions.push(substitution_for_var(dae, var_name.clone(), solution)?);
    eliminated_eq_indices.push(eq_idx);
    eliminated_eq_flags[eq_idx] = true;
    eliminated_var_names.push(var_name);
    Ok(())
}

fn scalar_blt_solution(
    dae: &Dae,
    equation: &EquationRef,
    unknown: &UnknownId,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
    state_derivative_matcher: &DerivativeNameMatcher,
    substitutions: &PlannedSubstitutions,
) -> Result<Option<(usize, VarName, Expression)>, StructuralError> {
    let Some(raw_var_name) = algebraic_or_output_unknown(unknown) else {
        return Ok(None);
    };
    let var_name = raw_var_name.clone();
    if !can_eliminate_scalar_unknown(dae, &var_name, runtime_protected_unknowns)? {
        return Ok(None);
    }

    let eq_idx = equation.0;
    let is_output = output_partition_contains_unknown(dae, &var_name);
    let has_state_derivative = equation_has_state_derivative(dae, eq_idx, state_derivative_matcher);
    if has_state_derivative && !is_output {
        return Ok(None);
    }

    let eq_rhs = apply_substitutions_in_order_with_plan(
        &dae.continuous.equations[eq_idx].rhs,
        substitutions.as_slice(),
        substitutions.plan(),
    )?;
    if is_flow_equation_origin(&dae.continuous.equations[eq_idx].origin)
        && expr_contains_indexed_multiscalar_ref(&eq_rhs, dae)?
    {
        return Ok(None);
    }
    if !can_use_scalar_equation_for_elimination(
        dae,
        eq_idx,
        &eq_rhs,
        &var_name,
        runtime_defined_discrete_targets,
    ) {
        return Ok(None);
    }
    let Some(solution) = stable_solution_for_unknown(dae, &eq_rhs, &var_name)? else {
        return Ok(None);
    };
    if is_output && !is_trivial_alias(&solution) {
        return Ok(None);
    }
    Ok(Some((eq_idx, var_name, solution)))
}

fn algebraic_or_output_unknown(unknown: &UnknownId) -> Option<&VarName> {
    match unknown {
        UnknownId::Variable(name) => Some(name),
        UnknownId::DerState(_) | UnknownId::SolverY(_) | UnknownId::Unmatched { .. } => None,
    }
}

fn can_eliminate_scalar_unknown(
    dae: &Dae,
    var_name: &VarName,
    runtime_protected_unknowns: &IndexSet<String>,
) -> Result<bool, StructuralError> {
    Ok(
        !is_runtime_protected_unknown(var_name, runtime_protected_unknowns)
            && !unknown_is_fixed(dae, var_name)
            && !dae.variables.states.contains_key(var_name)
            && !is_scalarized_element_of_aggregate(dae, var_name)?
            && dae_var_size(dae, var_name)? == 1,
    )
}

fn can_use_equation_for_elimination(dae: &Dae, eq_idx: usize) -> bool {
    dae.continuous
        .equations
        .get(eq_idx)
        .is_some_and(|eq| !eq.origin.starts_with("connection equation:"))
}

fn can_use_scalar_equation_for_elimination(
    dae: &Dae,
    eq_idx: usize,
    rhs: &Expression,
    var_name: &VarName,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    let Some(eq) = dae.continuous.equations.get(eq_idx) else {
        return false;
    };
    !should_skip_connection_equation(
        dae,
        rhs,
        eq.origin.starts_with("connection equation:"),
        std::slice::from_ref(var_name),
        runtime_defined_discrete_targets,
        false,
    )
}

fn equation_has_state_derivative(
    dae: &Dae,
    eq_idx: usize,
    state_derivative_matcher: &DerivativeNameMatcher,
) -> bool {
    dae.continuous
        .equations
        .get(eq_idx)
        .is_some_and(|eq| expr_contains_der_of_any(&eq.rhs, state_derivative_matcher))
}

fn stable_solution_for_unknown(
    dae: &Dae,
    rhs: &Expression,
    var_name: &VarName,
) -> Result<Option<Expression>, StructuralError> {
    let Some(solution) = try_solve_for_unknown(rhs, var_name) else {
        return Ok(None);
    };
    if expr_contains_var(&solution, var_name)
        || expr_contains_unsliced_multiscalar_ref(&solution, dae)?
        || !is_symbolically_stable_solution(&solution)
    {
        return Ok(None);
    }
    Ok(Some(solution))
}

mod expression_substitution;
#[cfg(test)]
use expression_substitution::SubstituteVarRewriter;
#[cfg(test)]
use expression_substitution::apply_substitutions_to_expr_with_derivatives;
use expression_substitution::{
    SubstitutionApplicationPlan, aggregate_subscript_ref_matches_var,
    apply_substitutions_to_expr_with_plan, der_call_matches_scalar_substitution,
    embedded_alias_indices_for_substitution, expr_contains_unsliced_multiscalar_ref,
    scalar_var_ref_key_from_reference, var_ref_matches_unknown_for_substitution,
};
mod substitution_index;
pub use expression_substitution::{
    apply_substitutions_to_expr, resolve_substitutions_in_expr, resolve_substitutions_in_exprs,
};
use substitution_index::{PlannedSubstitutions, SubstitutionIndex};

#[cfg(test)]
mod blt_preparation_tests;
#[cfg(test)]
mod tests;
