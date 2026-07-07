//! Symbolic elimination of trivially solvable equations.
//!
//! SPEC_0021 file-size exception: elimination still hosts boundary resolution,
//! substitution grouping, and provenance-preserving replacement helpers. split plan:
//! move substitution groups and scalar alias projection into submodules.
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

use rumoca_core::{
    ExpressionRewriter, FallibleExpressionRewriter, maybe_elapsed_seconds, maybe_start_timer_if,
};
use rumoca_ir_dae as dae;

mod aggregate_alias;
mod boundary_scan;
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
    aggregate_alias_for_elimination, aggregate_variable_fully_resolved,
    is_scalarized_element_of_aggregate,
};
use boundary_scan::{BoundaryScanCtx, BoundaryScanState, scan_boundary_equations};
use connection_policy::should_skip_connection_equation;
use diagnostics::trace_singular_reduced_rows;
use direct_definition_index::DirectDefinitionIndex;
use flow_policy::{
    expr_contains_indexed_multiscalar_ref, expr_contains_indexed_multiscalar_slice_ref,
    is_flow_equation_origin,
};
use orphan_unknowns::{drop_unreferenced_continuous_unknowns, output_partition_contains_unknown};
use profiling::{eliminate_profile_enabled, log_eliminate_profile};
use runtime_known::singular_rows_are_runtime_known_assignments;
use runtime_protection::{
    assignment_target_name_in_dae, expr_references_any_discrete_name,
    expr_references_any_runtime_discrete_target, is_runtime_protected_unknown,
    runtime_defined_discrete_target_names, runtime_partition_or_event_refs_var,
    runtime_protected_unknown_names, should_preserve_runtime_known_assignment,
};
use scalar_shape::expression_is_scalar_after_subscripts;
pub use solve_for_unknown::try_solve_for_unknown;
use solve_for_unknown::{expr_contains_unknown_in_dae, try_solve_for_unknown_in_dae};
use substitution_application::{
    apply_substitutions_to_dae_partitions, apply_substitutions_to_remaining_once,
    equation_analysis_expr,
};
use substitution_target::{
    expr_contains_derivative_substitution_target, expr_contains_substitution_target,
};
use tearing_elimination::tear_and_eliminate_loop_block;
use unknown_index::{
    BoundaryUnknownIndex, checked_count_live_unknowns, find_live_scalar_unknowns,
    has_any_live_unknown,
};

use crate::variable_scope::{DaeVariableScope, DaeVariableShape, scalar_count_from_dims};
use crate::{
    BltBlock, EquationRef, StructuralError, UnknownId, build_blt_from_incidence,
    maximum_regular_subsystem, sort_dae,
};

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
    substitutions: &'a mut Vec<Substitution>,
    eliminated_eq_indices: &'a mut Vec<usize>,
    eliminated_eq_flags: &'a mut [bool],
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

    // Phase B: BLT scalar-block elimination on the (now hopefully non-singular) system.
    // Extract blocks before mutating dae (SortedDae borrows dae immutably).
    let mut blt_error = None;
    let p_clone = maybe_start_timer_if(profile);
    let mut sort_input = dae.clone();
    log_eliminate_profile(
        profile,
        "clone_sort_input",
        p_clone,
        sort_input.continuous.equations.len(),
    );
    let p_drop = maybe_start_timer_if(profile);
    drop_unreferenced_continuous_unknowns(&mut sort_input);
    log_eliminate_profile(
        profile,
        "drop_unreferenced_unknowns",
        p_drop,
        sort_input.continuous.equations.len(),
    );
    let p_sort = maybe_start_timer_if(profile);
    let blocks = match sort_dae(&sort_input) {
        Ok(sorted) => Some(sorted.blocks.clone()),
        Err(StructuralError::EmptySystem) => None,
        Err(err) if singular_rows_are_runtime_known_assignments(&sort_input, &err) => None,
        Err(err) => match regular_blt_blocks_for_fully_matched_rows(&sort_input, &err)? {
            Some(blocks) => Some(blocks),
            None => {
                trace_singular_reduced_rows(trace, &sort_input, &err);
                blt_error = Some(err);
                None
            }
        },
    };
    log_eliminate_profile(
        profile,
        "sort_dae",
        p_sort,
        blocks.as_ref().map_or(0, Vec::len),
    );
    if let Some(blocks) = blocks {
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
    result.blt_error = blt_error;
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

fn regular_blt_blocks_for_fully_matched_rows(
    dae: &Dae,
    error: &StructuralError,
) -> Result<Option<Vec<BltBlock>>, StructuralError> {
    let StructuralError::Singular {
        n_equations,
        n_unknowns,
        n_matched,
        unmatched_equations,
        unmatched_unknowns,
        ..
    } = error
    else {
        return Ok(None);
    };
    if unmatched_unknowns.is_empty() && n_matched == n_unknowns && n_unknowns < n_equations {
        let incidence = crate::incidence::build_incidence(dae);
        let regular = maximum_regular_subsystem(&incidence)?;
        if regular.dropped_unknowns.is_empty()
            && regular.dropped_equations.iter().all(|equation| {
                dropped_equation_is_evaluation_assignment_row(dae, &incidence, equation)
            })
        {
            return build_blt_from_incidence(&regular.incidence).map(Some);
        }
        return Ok(None);
    }
    if n_matched != n_equations || !unmatched_equations.is_empty() || n_unknowns <= n_equations {
        return Ok(None);
    }
    for unknown in unmatched_unknowns {
        if !regular_subsystem_extra_unknown_is_direct_component_helper(dae, unknown)? {
            return Ok(None);
        }
    }
    let incidence = crate::incidence::build_incidence(dae);
    let regular = maximum_regular_subsystem(&incidence)?;
    if !regular.dropped_equations.is_empty() || regular.dropped_unknowns.is_empty() {
        return Ok(None);
    }
    build_blt_from_incidence(&regular.incidence).map(Some)
}

fn dropped_equation_is_evaluation_assignment_row(
    dae: &Dae,
    incidence: &crate::incidence::Incidence,
    equation: &EquationRef,
) -> bool {
    let Some(eq) = dae.continuous.equations.get(equation.0) else {
        return false;
    };
    let analysis_expr = equation_analysis_expr(eq);
    let Some(target) = assignment_target_name_in_dae(dae, &analysis_expr) else {
        return false;
    };
    let Some(row_unknowns) = incidence
        .equation_refs
        .iter()
        .position(|candidate| candidate == equation)
        .and_then(|idx| incidence.eq_unknowns.get(idx))
    else {
        return false;
    };
    !row_unknowns
        .iter()
        .filter_map(|idx| incidence.unknown_names.get(*idx))
        .any(|unknown| unknown_matches_var_name(unknown, &target))
}

fn unknown_matches_var_name(unknown: &UnknownId, target: &VarName) -> bool {
    matches!(unknown, UnknownId::Variable(name) if name == target)
}

fn regular_subsystem_extra_unknown_is_direct_component_helper(
    dae: &Dae,
    unknown: &str,
) -> Result<bool, StructuralError> {
    let var_name = VarName::new(unknown);
    let is_scalarized_element = is_scalarized_element_of_aggregate(dae, &var_name)?;
    let component_path_len = rumoca_core::ComponentPath::from_flat_path(unknown).len();
    let is_nested_component = component_path_len > 1;
    if output_partition_contains_unknown(dae, &var_name) {
        return Ok(true);
    }
    if !is_scalarized_element && component_path_len > 2 {
        return Ok(true);
    }
    if !is_scalarized_element && !is_nested_component {
        return Ok(false);
    }
    if is_scalarized_element {
        return Ok(true);
    }
    direct_non_connection_definition_for_unknown(dae, &var_name)
}

fn direct_non_connection_definition_for_unknown(
    dae: &Dae,
    var_name: &VarName,
) -> Result<bool, StructuralError> {
    for (eq_idx, equation) in dae.continuous.equations.iter().enumerate() {
        if equation.origin.starts_with("connection equation:") {
            continue;
        }
        if !has_direct_assignment_form(dae, &equation.rhs, var_name) {
            continue;
        }
        if !can_use_equation_for_elimination(dae, eq_idx) {
            continue;
        }
        return Ok(true);
    }
    Ok(false)
}

pub fn resolve_boundary_equations_to_fixpoint(
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
        let demoted =
            crate::dae_prepare::demote_direct_assigned_states_with_boundary_substitutions(
                dae,
                &result.substitutions,
            )?;
        log_eliminate_profile(profile, "boundary_direct_demotion", p_demote, demoted);
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
    let runtime_protected_unknowns = runtime_protected_unknown_names(dae);
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
    let mut scan_state = BoundaryScanState::new(dae.continuous.equations.len());
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
        scan_state.substitutions,
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
fn shift_structured_families_after_equation_removal(dae: &mut Dae, removed_sorted: &[usize]) {
    if removed_sorted.is_empty() {
        return;
    }
    dae.continuous.structured_equations.retain_mut(|family| {
        let total: usize = family.equation_counts.iter().sum();
        let block_end = family.first_equation_index + total;
        let removed_inside_block = removed_sorted
            .iter()
            .any(|&idx| idx >= family.first_equation_index && idx < block_end);
        if removed_inside_block {
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

/// Drop structured families whose row bodies were symbolically rewritten.
///
/// Substitutions can change a family row's LHS/RHS without changing row count.
/// The old compact family metadata was proven for the pre-substitution body, so
/// keeping it would let downstream Solve-IR rebuild a tensor node from stale row
/// provenance. Regenerating a family proof belongs in a dedicated pass; the safe
/// structural-elimination behavior is to scalarize rewritten families.
fn drop_structured_families_touching_equations(dae: &mut Dae, touched_sorted: &[usize]) {
    if touched_sorted.is_empty() {
        return;
    }
    dae.continuous.structured_equations.retain(|family| {
        let total: usize = family.equation_counts.iter().sum();
        let block_end = family.first_equation_index + total;
        !touched_sorted
            .iter()
            .any(|&idx| idx >= family.first_equation_index && idx < block_end)
    });
}

fn finish_boundary_elimination(
    dae: &mut Dae,
    substitutions: Vec<Substitution>,
    mut eliminated_eq_flags: Vec<bool>,
    mut eliminated_eq_indices: Vec<usize>,
    resolved: &HashSet<VarName>,
) -> Result<EliminationResult, StructuralError> {
    eliminated_eq_indices.retain(|&idx| {
        let preserve = dae.continuous.equations.get(idx).is_some_and(|eq| {
            should_preserve_runtime_sensitive_continuous_assignment(
                dae,
                &equation_analysis_expr(eq),
            )
        });
        if preserve && let Some(flag) = eliminated_eq_flags.get_mut(idx) {
            *flag = false;
        }
        !preserve
    });
    apply_substitutions_to_remaining_once(dae, &eliminated_eq_flags, &substitutions)?;
    let n_eliminated = eliminated_eq_indices.len();
    eliminated_eq_indices.sort_unstable();
    for &idx in eliminated_eq_indices.iter().rev() {
        dae.continuous.equations.remove(idx);
    }
    shift_structured_families_after_equation_removal(dae, &eliminated_eq_indices);
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

pub(super) fn scalar_connection_alias_for_elimination(
    dae: &Dae,
    rhs: &Expression,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> Result<Option<(VarName, Expression)>, StructuralError> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs: rhs_expr,
        ..
    } = rhs
    else {
        return Ok(None);
    };
    let Some(lhs_name) =
        exact_reference_expr_name_in_dae(dae, lhs).or_else(|| exact_reference_expr_name(lhs))
    else {
        return Ok(None);
    };
    let Some(rhs_name) = exact_reference_expr_name_in_dae(dae, rhs_expr)
        .or_else(|| exact_reference_expr_name(rhs_expr))
    else {
        return Ok(None);
    };
    let lhs_rank = scalar_connection_alias_candidate_rank(
        dae,
        &lhs_name,
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    )?;
    let rhs_rank = scalar_connection_alias_candidate_rank(
        dae,
        &rhs_name,
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    )?;
    match (lhs_rank, rhs_rank) {
        (Some(lhs_rank), Some(rhs_rank)) if lhs_rank <= rhs_rank => {
            Ok(Some((lhs_name, rhs_expr.as_ref().clone())))
        }
        (Some(_), Some(_)) => Ok(Some((rhs_name, lhs.as_ref().clone()))),
        (Some(_), None) => Ok(Some((lhs_name, rhs_expr.as_ref().clone()))),
        (None, Some(_)) => Ok(Some((rhs_name, lhs.as_ref().clone()))),
        (None, None) => Ok(None),
    }
}

fn scalar_connection_alias_candidate_rank(
    dae: &Dae,
    var_name: &VarName,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> Result<Option<u8>, StructuralError> {
    if !can_eliminate_aggregate_alias_var(
        dae,
        var_name,
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    ) || DaeVariableScope::new(dae).size(var_name)? != 1
    {
        return Ok(None);
    }
    if dae.variables.algebraics.contains_key(var_name) {
        return Ok(Some(0));
    }
    if dae.variables.outputs.contains_key(var_name) {
        return Ok(Some(1));
    }
    Ok(None)
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
    let assignment_target = assignment_target_name_in_dae(ctx.dae, eq_rhs);
    let assignment_target_is_dae_var = assignment_target
        .as_ref()
        .is_some_and(|target| dae_var(ctx.dae, target).is_some());
    if has_state_derivative
        || has_any_live_unknown(eq_rhs, ctx.unknown_index, ctx.resolved)?
        || (references_state_value && assignment_target_is_dae_var)
    {
        return Ok(());
    }
    // MLS Appendix B / §8.3 / §16.5.1: a zero-unknown equation may still
    // define a live runtime discrete/event value. Do not drop those rows
    // unless they can be substituted safely through every runtime consumer.
    if should_preserve_runtime_known_assignment(ctx.dae, eq_rhs) {
        return Ok(());
    }
    if should_preserve_runtime_sensitive_continuous_assignment(ctx.dae, eq_rhs) {
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
    if assignment_target_is_dae_var && ctx.substitutions.len() == n_subs_before {
        return Ok(());
    }
    ctx.eliminated_eq_indices.push(eq_idx);
    ctx.eliminated_eq_flags[eq_idx] = true;
    Ok(())
}

fn should_preserve_runtime_sensitive_continuous_assignment(dae: &Dae, eq_rhs: &Expression) -> bool {
    if !expr_contains_runtime_sensitive_operator(eq_rhs) {
        return false;
    }
    let Some(target) = assignment_target_name_in_dae(dae, eq_rhs) else {
        return false;
    };
    dae.variables.algebraics.contains_key(&target)
        || dae.variables.outputs.contains_key(&target)
        || rumoca_ir_dae::component_base_name(target.as_str()).is_some_and(|base| {
            let base = VarName::new(base);
            dae.variables.algebraics.contains_key(&base)
                || dae.variables.outputs.contains_key(&base)
        })
}

pub(in crate::eliminate) struct EliminationChoiceContext<'a> {
    pub(in crate::eliminate) dae: &'a Dae,
    pub(in crate::eliminate) eq_idx: usize,
    pub(in crate::eliminate) has_state_derivative: bool,
    pub(in crate::eliminate) runtime_protected_unknowns: &'a IndexSet<String>,
    pub(in crate::eliminate) direct_definitions: &'a DirectDefinitionIndex,
    pub(in crate::eliminate) allow_multi_live_trivial_alias: bool,
}

fn choose_solvable_unknown_for_elimination(
    ctx: &EliminationChoiceContext<'_>,
    rhs: &Expression,
    live: &[VarName],
) -> Result<Option<(VarName, Expression)>, StructuralError> {
    let mut candidates: Vec<&VarName> = live.iter().collect();
    let dae = ctx.dae;
    let connection_rhs = connection_rhs_assignment_target(dae, ctx.eq_idx, rhs);
    candidates.sort_by(|a, b| {
        let a_is_connection_rhs =
            connection_rhs.is_some_and(|target| is_assignment_target(dae, target, a));
        let b_is_connection_rhs =
            connection_rhs.is_some_and(|target| is_assignment_target(dae, target, b));
        let a_has_definition = ctx
            .direct_definitions
            .has_other_direct_definition(ctx.eq_idx, a);
        let b_has_definition = ctx
            .direct_definitions
            .has_other_direct_definition(ctx.eq_idx, b);
        let a_is_output = output_partition_contains_unknown(dae, a);
        let b_is_output = output_partition_contains_unknown(dae, b);
        b_is_connection_rhs
            .cmp(&a_is_connection_rhs)
            .then_with(|| a_has_definition.cmp(&b_has_definition))
            .then_with(|| b_is_output.cmp(&a_is_output))
            .then_with(|| a.as_str().cmp(b.as_str()))
    });

    for candidate in candidates {
        // `fixed=true` introduces a hard initialization constraint. Eliminating
        // that unknown can erase user intent (especially through alias chains)
        // and alter the selected initialization branch.
        if unknown_is_fixed(dae, candidate) {
            continue;
        }
        if dae.variables.states.contains_key(candidate) {
            continue;
        }
        let is_output = output_partition_contains_unknown(dae, candidate);
        // Try the simple top-level Sub pattern first; fall back to the additive
        // solver so substitution residues like `x - (y - 0)` (which the simple
        // pattern can't see through) still resolve. The additive solver is gated
        // by `live` to avoid accidentally solving a multi-unknown equation.
        let Some(solution) = try_solve_for_unknown_in_dae(dae, rhs, candidate) else {
            continue;
        };
        let direct_assignment_solution = has_direct_assignment_form(dae, rhs, candidate);
        if !elimination_solution_is_valid(
            ctx,
            candidate,
            &solution,
            is_output,
            direct_assignment_solution,
            live.len(),
        )? {
            continue;
        }
        return Ok(Some((candidate.clone(), solution)));
    }
    Ok(None)
}

fn elimination_solution_is_valid(
    ctx: &EliminationChoiceContext<'_>,
    candidate: &VarName,
    solution: &Expression,
    is_output: bool,
    direct_assignment_solution: bool,
    live_len: usize,
) -> Result<bool, StructuralError> {
    let dae = ctx.dae;
    if expr_contains_unknown_in_dae(dae, solution, candidate) {
        return Ok(false);
    }
    if !is_trivial_alias_in_dae(dae, solution)
        && !solution_is_cheap_for_symbolic_substitution(solution)
    {
        return Ok(false);
    }
    if !runtime_protected_elimination_is_valid(ctx, candidate, solution, direct_assignment_solution)
    {
        return Ok(false);
    }
    if !scalarized_candidate_elimination_is_valid(
        ctx,
        candidate,
        solution,
        direct_assignment_solution,
    )? {
        return Ok(false);
    }
    if !state_derivative_elimination_is_valid(
        ctx,
        candidate,
        solution,
        is_output,
        direct_assignment_solution,
    ) {
        return Ok(false);
    }
    if is_output
        && !is_trivial_alias_in_dae(dae, solution)
        && !is_internal_component_output(dae, candidate)
    {
        return Ok(false);
    }
    if !direct_assignment_solution && !is_symbolically_stable_solution(solution) {
        return Ok(false);
    }
    if solution_has_blocking_unsliced_multiscalar_ref(solution, dae)? {
        return Ok(false);
    }
    if indexed_multiscalar_slice_solution_is_blocked(dae, solution)? {
        return Ok(false);
    }
    Ok(live_len <= 1
        || direct_assignment_solution
        || (ctx.allow_multi_live_trivial_alias && is_trivial_alias_in_dae(dae, solution)))
}

fn runtime_protected_elimination_is_valid(
    ctx: &EliminationChoiceContext<'_>,
    candidate: &VarName,
    solution: &Expression,
    direct_assignment_solution: bool,
) -> bool {
    !is_runtime_protected_unknown(candidate, ctx.runtime_protected_unknowns)
        || (direct_assignment_solution
            && !solution_references_fixed_unknown(ctx.dae, solution)
            && !expr_contains_runtime_sensitive_operator(solution))
}

fn scalarized_candidate_elimination_is_valid(
    ctx: &EliminationChoiceContext<'_>,
    candidate: &VarName,
    solution: &Expression,
    direct_assignment_solution: bool,
) -> Result<bool, StructuralError> {
    let dae = ctx.dae;
    let is_scalarized_element = is_scalarized_element_of_aggregate(dae, candidate)?;
    if !is_scalarized_element {
        return Ok(true);
    }
    let is_trivial_alias = is_trivial_alias_in_dae(dae, solution);
    if direct_assignment_solution
        && !is_trivial_alias
        && expr_contains_runtime_sensitive_operator(solution)
        && scalarized_element_has_non_connection_use(dae, candidate)
    {
        return Ok(false);
    }
    if ctx.has_state_derivative {
        return Ok(false);
    }
    if scalarized_element_has_coupled_derivative_use(dae, candidate) && !is_trivial_alias {
        return Ok(false);
    }
    Ok(scalarized_element_has_non_connection_use(dae, candidate))
}

fn state_derivative_elimination_is_valid(
    ctx: &EliminationChoiceContext<'_>,
    candidate: &VarName,
    solution: &Expression,
    is_output: bool,
    direct_assignment_solution: bool,
) -> bool {
    !ctx.has_state_derivative
        || is_output
        || (direct_assignment_solution
            && ctx
                .direct_definitions
                .has_other_direct_definition(ctx.eq_idx, candidate)
            && is_derivative_alias_expr(solution))
}

fn indexed_multiscalar_slice_solution_is_blocked(
    dae: &Dae,
    solution: &Expression,
) -> Result<bool, StructuralError> {
    if !expr_contains_indexed_multiscalar_slice_ref(solution, dae)? {
        return Ok(false);
    }
    Ok(!(is_scalar_reduction_solution_tree(solution)
        || is_trivial_alias_in_dae(dae, solution)
            && expression_is_scalar_after_subscripts(solution, dae)?))
}

fn connection_rhs_assignment_target<'a>(
    dae: &'a Dae,
    eq_idx: usize,
    rhs: &'a Expression,
) -> Option<&'a Expression> {
    let eq = dae.continuous.equations.get(eq_idx)?;
    if !eq.origin.starts_with("connection equation:") {
        return None;
    }
    let Expression::Binary {
        op: OpBinary::Sub,
        rhs: target,
        ..
    } = rhs
    else {
        return None;
    };
    Some(target.as_ref())
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

        let Some(solution) = try_solve_for_unknown_in_dae(dae, rhs, &candidate) else {
            continue;
        };
        if expr_contains_unknown_in_dae(dae, &solution, &candidate) {
            continue;
        }
        if solution_has_blocking_unsliced_multiscalar_ref(&solution, dae)? {
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
    substitutions: &mut Vec<Substitution>,
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

fn has_direct_assignment_form(dae: &Dae, rhs: &Expression, candidate: &VarName) -> bool {
    match rhs {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => is_assignment_target(dae, lhs, candidate) || is_assignment_target(dae, rhs, candidate),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => has_direct_assignment_form(dae, rhs, candidate),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches
                .iter()
                .all(|(_, branch)| has_direct_assignment_form(dae, branch, candidate))
                && has_direct_assignment_form(dae, else_branch, candidate)
        }
        _ => false,
    }
}

fn is_assignment_target(dae: &Dae, expr: &Expression, candidate: &VarName) -> bool {
    if exact_reference_expr_name_in_dae(dae, expr).as_ref() == Some(candidate) {
        return true;
    }
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            var_ref_matches_unknown(name, subscripts, candidate)
                || assignment_slice_target_contains_scalarized_candidate(
                    dae, name, subscripts, candidate,
                )
                || assignment_target_is_singleton_projection(dae, name, subscripts, candidate)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            if let Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            {
                let mut combined = Vec::with_capacity(base_subscripts.len() + subscripts.len());
                combined.extend_from_slice(base_subscripts);
                combined.extend_from_slice(subscripts);
                var_ref_matches_unknown(name, &combined, candidate)
                    || assignment_slice_target_contains_scalarized_candidate(
                        dae, name, &combined, candidate,
                    )
                    || assignment_target_is_singleton_projection(dae, name, &combined, candidate)
            } else {
                false
            }
        }
        _ => false,
    }
}

fn assignment_slice_target_contains_scalarized_candidate(
    dae: &Dae,
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    candidate: &VarName,
) -> bool {
    let Some(scalar) = rumoca_core::parse_scalar_name(candidate.as_str()) else {
        return false;
    };
    if name.as_str() != scalar.base || subscripts.len() != scalar.indices.len() {
        return false;
    }
    subscripts
        .iter()
        .zip(scalar.indices.iter())
        .all(|(subscript, candidate_index)| match subscript {
            rumoca_core::Subscript::Index { value, .. } => value == candidate_index,
            rumoca_core::Subscript::Colon { .. } => true,
            rumoca_core::Subscript::Expr { .. } => {
                exact_subscript_index_in_dae(dae, subscript) == Some(*candidate_index)
            }
        })
}

fn assignment_target_is_singleton_projection(
    dae: &Dae,
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    candidate: &VarName,
) -> bool {
    if let Some((base, indices)) = scalar_var_ref_key_from_reference(name)
        && base.as_str() == candidate.as_str()
        && indices.iter().all(|index| *index == 1)
        && DaeVariableScope::new(dae)
            .exact(candidate)
            .is_some_and(|var| var.dims.iter().all(|dim| *dim == 1))
    {
        return true;
    }
    if subscripts.is_empty() || name.as_str() != candidate.as_str() {
        return false;
    }
    let Some(var) = DaeVariableScope::new(dae).exact(candidate) else {
        return false;
    };
    var.dims.iter().all(|dim| *dim == 1)
        && subscripts
            .iter()
            .all(|subscript| matches!(positive_usize_subscript(subscript), Some(1)))
}

fn scalarized_element_has_non_connection_use(dae: &Dae, candidate: &VarName) -> bool {
    dae.continuous.equations.iter().any(|eq| {
        !eq.origin.starts_with("connection equation:")
            && expr_references_var_for_presence(&eq.rhs, candidate)
    })
}

fn scalarized_element_has_coupled_derivative_use(dae: &Dae, candidate: &VarName) -> bool {
    let mut exact_derivative_use_count = 0usize;
    let has_aggregate_base_use = dae.continuous.equations.iter().any(|eq| {
        if !expression_contains_der_call(&eq.rhs) {
            return false;
        }
        let mut refs = Vec::new();
        collect_var_ref_nodes(&eq.rhs, &mut refs);
        if refs
            .iter()
            .any(|(name, subscripts)| var_ref_matches_unknown(name, subscripts, candidate))
        {
            exact_derivative_use_count += 1;
        }
        refs.iter().any(|(name, subscripts)| {
            subscripts.is_empty()
                && aggregate_ref_matches_scalarized_candidate(name, subscripts, candidate)
        })
    });
    has_aggregate_base_use || exact_derivative_use_count > 1
}

fn solution_references_fixed_unknown(dae: &Dae, expr: &Expression) -> bool {
    let mut refs = Vec::new();
    collect_exact_reference_expr_names_in_dae(dae, expr, &mut refs);
    refs.into_iter().any(|name| {
        unknown_is_fixed(dae, &name)
            || rumoca_ir_dae::component_base_name(name.as_str())
                .is_some_and(|base| unknown_is_fixed(dae, &VarName::new(base)))
    })
}

fn expr_contains_runtime_sensitive_operator(expr: &Expression) -> bool {
    struct Checker {
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &Expression) {
            if self.found {
                return;
            }
            if let Expression::VarRef { name, .. } = expr
                && name.as_str() == "time"
            {
                self.found = true;
                return;
            }
            self.walk_expression(expr);
        }

        fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
            if matches!(
                function,
                BuiltinFunction::Pre
                    | BuiltinFunction::Sample
                    | BuiltinFunction::Initial
                    | BuiltinFunction::Terminal
                    | BuiltinFunction::Edge
                    | BuiltinFunction::Change
                    | BuiltinFunction::Reinit
            ) {
                self.found = true;
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }
    }

    let mut checker = Checker { found: false };
    checker.visit_expression(expr);
    checker.found
}

fn expression_contains_der_call(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            ..
        } => true,
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(expression_contains_der_call)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expression_contains_der_call(lhs) || expression_contains_der_call(rhs)
        }
        Expression::Unary { rhs, .. } => expression_contains_der_call(rhs),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, branch)| {
                expression_contains_der_call(condition) || expression_contains_der_call(branch)
            }) || expression_contains_der_call(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            elements.iter().any(expression_contains_der_call)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            expression_contains_der_call(base)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_core::Subscript::Expr { expr, .. } => expression_contains_der_call(expr),
                    _ => false,
                })
        }
        Expression::FieldAccess { base, .. } => expression_contains_der_call(base),
        Expression::ArrayComprehension { expr, filter, .. } => {
            expression_contains_der_call(expr)
                || filter
                    .as_ref()
                    .is_some_and(|filter| expression_contains_der_call(filter))
        }
        Expression::Range {
            start, step, end, ..
        } => {
            expression_contains_der_call(start)
                || step
                    .as_ref()
                    .is_some_and(|step| expression_contains_der_call(step))
                || expression_contains_der_call(end)
        }
        Expression::VarRef { .. } | Expression::Literal { .. } | Expression::Empty { .. } => false,
    }
}

fn expr_references_var_for_presence(expr: &Expression, candidate: &VarName) -> bool {
    let mut refs = Vec::new();
    collect_var_ref_nodes(expr, &mut refs);
    refs.iter().any(|(name, subscripts)| {
        var_ref_matches_unknown(name, subscripts, candidate)
            || aggregate_ref_matches_scalarized_candidate(name, subscripts, candidate)
    })
}

fn aggregate_ref_matches_scalarized_candidate(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    candidate: &VarName,
) -> bool {
    if !subscripts.is_empty() {
        return false;
    }
    rumoca_core::parse_scalar_name(candidate.as_str())
        .is_some_and(|scalar| name.var_name().as_str() == scalar.base)
}

/// Returns true if the expression is a single variable reference or its
/// negation — i.e., a trivial alias like `x` or `-x`.
fn is_trivial_alias(expr: &Expression) -> bool {
    if exact_reference_expr_name(expr).is_some() {
        return true;
    }
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

pub(super) fn is_trivial_alias_in_dae(dae: &Dae, expr: &Expression) -> bool {
    if exact_reference_expr_name_in_dae(dae, expr).is_some() {
        return true;
    }
    match expr {
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => is_trivial_alias_in_dae(dae, rhs),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } => args.len() == 1 && exact_reference_expr_name_in_dae(dae, &args[0]).is_some(),
        _ => is_trivial_alias(expr),
    }
}

fn is_symbolically_stable_solution(expr: &Expression) -> bool {
    match expr {
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().all(|(condition, branch)| {
                is_symbolically_stable_solution(condition)
                    && is_symbolically_stable_solution(branch)
            }) && is_symbolically_stable_solution(else_branch)
        }
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

fn solution_has_blocking_unsliced_multiscalar_ref(
    expr: &Expression,
    dae: &Dae,
) -> Result<bool, StructuralError> {
    let scope = DaeVariableScope::new(dae);
    solution_has_blocking_unsliced_multiscalar_ref_inner(expr, &scope)
}

const MAX_SYMBOLIC_SUBSTITUTION_NODES: usize = 64;
const MAX_BLT_SYMBOLIC_CANDIDATE_NODES: usize = 256;

pub(super) fn solution_is_cheap_for_symbolic_substitution(expr: &Expression) -> bool {
    expression_node_count_exceeds(expr, MAX_SYMBOLIC_SUBSTITUTION_NODES)
        .is_some_and(|exceeds| !exceeds)
}

fn expression_is_within_symbolic_candidate_budget(expr: &Expression) -> bool {
    expression_node_count_exceeds(expr, MAX_BLT_SYMBOLIC_CANDIDATE_NODES)
        .is_some_and(|exceeds| !exceeds)
}

fn expression_node_count_exceeds(expr: &Expression, limit: usize) -> Option<bool> {
    let mut count = 0usize;
    expression_node_count_visit(expr, limit, &mut count)
}

fn expression_node_count_visit_all<'a>(
    exprs: impl IntoIterator<Item = &'a Expression>,
    limit: usize,
    count: &mut usize,
) -> Option<bool> {
    for expr in exprs {
        if expression_node_count_visit(expr, limit, count)? {
            return Some(true);
        }
    }
    Some(false)
}

fn expression_node_count_visit_branches(
    branches: &[(Expression, Expression)],
    limit: usize,
    count: &mut usize,
) -> Option<bool> {
    for (condition, branch) in branches {
        if expression_node_count_visit_all([condition, branch], limit, count)? {
            return Some(true);
        }
    }
    Some(false)
}

fn expression_node_count_visit_subscripts(
    subscripts: &[rumoca_core::Subscript],
    limit: usize,
    count: &mut usize,
) -> Option<bool> {
    expression_node_count_visit_all(
        subscripts.iter().filter_map(|subscript| match subscript {
            rumoca_core::Subscript::Expr { expr, .. } => Some(expr.as_ref()),
            _ => None,
        }),
        limit,
        count,
    )
}

fn expression_node_count_visit(expr: &Expression, limit: usize, count: &mut usize) -> Option<bool> {
    *count = count.checked_add(1)?;
    if *count > limit {
        return Some(true);
    }
    match expr {
        Expression::Binary { lhs, rhs, .. } => {
            expression_node_count_visit_binary(lhs, rhs, limit, count)
        }
        Expression::Unary { rhs, .. } => expression_node_count_visit(rhs, limit, count),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            expression_node_count_visit_all(args, limit, count)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => expression_node_count_visit_if(branches, else_branch, limit, count),
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            expression_node_count_visit_all(elements, limit, count)
        }
        Expression::Range {
            start, step, end, ..
        } => expression_node_count_visit_range(start, step.as_deref(), end, limit, count),
        Expression::Index {
            base, subscripts, ..
        } => expression_node_count_visit_index(base, subscripts, limit, count),
        Expression::FieldAccess { base, .. } => expression_node_count_visit(base, limit, count),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => expression_node_count_visit_comprehension(
            expr,
            indices,
            filter.as_deref(),
            limit,
            count,
        ),
        _ => Some(false),
    }
}

fn expression_node_count_visit_binary(
    lhs: &Expression,
    rhs: &Expression,
    limit: usize,
    count: &mut usize,
) -> Option<bool> {
    if expression_node_count_visit(lhs, limit, count)? {
        return Some(true);
    }
    expression_node_count_visit(rhs, limit, count)
}

fn expression_node_count_visit_if(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    limit: usize,
    count: &mut usize,
) -> Option<bool> {
    if expression_node_count_visit_branches(branches, limit, count)? {
        return Some(true);
    }
    expression_node_count_visit(else_branch, limit, count)
}

fn expression_node_count_visit_range(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    limit: usize,
    count: &mut usize,
) -> Option<bool> {
    if expression_node_count_visit(start, limit, count)? {
        return Some(true);
    }
    if let Some(step) = step
        && expression_node_count_visit(step, limit, count)?
    {
        return Some(true);
    }
    expression_node_count_visit(end, limit, count)
}

fn expression_node_count_visit_index(
    base: &Expression,
    subscripts: &[rumoca_core::Subscript],
    limit: usize,
    count: &mut usize,
) -> Option<bool> {
    if expression_node_count_visit(base, limit, count)? {
        return Some(true);
    }
    expression_node_count_visit_subscripts(subscripts, limit, count)
}

fn expression_node_count_visit_comprehension(
    expr: &Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&Expression>,
    limit: usize,
    count: &mut usize,
) -> Option<bool> {
    if expression_node_count_visit(expr, limit, count)? {
        return Some(true);
    }
    if expression_node_count_visit_all(indices.iter().map(|index| &index.range), limit, count)? {
        return Some(true);
    }
    filter.map_or(Some(false), |filter| {
        expression_node_count_visit(filter, limit, count)
    })
}

fn is_scalar_reduction_solution_tree(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Sum | BuiltinFunction::Product,
            args,
            ..
        } => args.len() == 1,
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches
                .iter()
                .all(|(_, branch)| is_scalar_reduction_solution_tree(branch))
                && is_scalar_reduction_solution_tree(else_branch)
        }
        Expression::Literal { .. } => true,
        _ => false,
    }
}

fn solution_has_blocking_unsliced_multiscalar_ref_inner(
    expr: &Expression,
    scope: &DaeVariableScope<'_>,
) -> Result<bool, StructuralError> {
    Ok(match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            if !subscripts.is_empty() || name.as_str() == "time" {
                false
            } else {
                match scope.shape_for_reference(name) {
                    Ok(DaeVariableShape::Dimensions(dims)) => {
                        scalar_count_from_dims(name.var_name(), &dims)? > 1
                    }
                    Ok(DaeVariableShape::StructuredAggregate) => true,
                    Err(StructuralError::ContractViolation { reason, .. })
                    | Err(StructuralError::UnspannedContractViolation { reason })
                        if reason.contains("missing DAE variable metadata")
                            && reference_has_scalar_indices(name) =>
                    {
                        true
                    }
                    Err(err) => return Err(err),
                }
            }
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Sum | BuiltinFunction::Product,
            args,
            ..
        } if args.len() == 1 => false,
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            exprs_have_blocking_unsliced_multiscalar_ref(args, scope)?
        }
        Expression::Binary { lhs, rhs, .. } => {
            solution_has_blocking_unsliced_multiscalar_ref_inner(lhs, scope)?
                || solution_has_blocking_unsliced_multiscalar_ref_inner(rhs, scope)?
        }
        Expression::Unary { rhs, .. } => {
            solution_has_blocking_unsliced_multiscalar_ref_inner(rhs, scope)?
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let mut blocked = false;
            for (condition, branch) in branches {
                blocked |= solution_has_blocking_unsliced_multiscalar_ref_inner(condition, scope)?;
                blocked |= solution_has_blocking_unsliced_multiscalar_ref_inner(branch, scope)?;
            }
            blocked || solution_has_blocking_unsliced_multiscalar_ref_inner(else_branch, scope)?
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            exprs_have_blocking_unsliced_multiscalar_ref(elements, scope)?
        }
        Expression::Range {
            start, step, end, ..
        } => {
            let step_blocked = match step.as_deref() {
                Some(step) => solution_has_blocking_unsliced_multiscalar_ref_inner(step, scope)?,
                None => false,
            };
            solution_has_blocking_unsliced_multiscalar_ref_inner(start, scope)?
                || step_blocked
                || solution_has_blocking_unsliced_multiscalar_ref_inner(end, scope)?
        }
        Expression::ArrayComprehension { expr, filter, .. } => {
            let filter_blocked = match filter.as_deref() {
                Some(filter) => {
                    solution_has_blocking_unsliced_multiscalar_ref_inner(filter, scope)?
                }
                None => false,
            };
            solution_has_blocking_unsliced_multiscalar_ref_inner(expr, scope)? || filter_blocked
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            if solution_has_blocking_unsliced_multiscalar_ref_inner(base, scope)? {
                true
            } else {
                subscripts_have_blocking_unsliced_multiscalar_ref(subscripts, scope)?
            }
        }
        Expression::FieldAccess { base, .. } => {
            solution_has_blocking_unsliced_multiscalar_ref_inner(base, scope)?
        }
        Expression::Literal { .. } | Expression::Empty { .. } => false,
    })
}

fn subscripts_have_blocking_unsliced_multiscalar_ref(
    subscripts: &[rumoca_core::Subscript],
    scope: &DaeVariableScope<'_>,
) -> Result<bool, StructuralError> {
    let mut blocked = false;
    for sub in subscripts {
        if let rumoca_core::Subscript::Expr { expr, .. } = sub {
            blocked |= solution_has_blocking_unsliced_multiscalar_ref_inner(expr, scope)?;
        }
    }
    Ok(blocked)
}

fn exprs_have_blocking_unsliced_multiscalar_ref(
    exprs: &[Expression],
    scope: &DaeVariableScope<'_>,
) -> Result<bool, StructuralError> {
    for expr in exprs {
        if solution_has_blocking_unsliced_multiscalar_ref_inner(expr, scope)? {
            return Ok(true);
        }
    }
    Ok(false)
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

pub(super) fn collect_exact_reference_expr_names_in_dae(
    dae: &Dae,
    expr: &Expression,
    out: &mut Vec<VarName>,
) {
    match expr {
        Expression::VarRef { .. } => {
            if let Some(name) = exact_reference_expr_name_in_dae(dae, expr) {
                out.push(name);
            }
        }
        Expression::Index { base, .. } | Expression::FieldAccess { base, .. } => {
            if let Some(name) = exact_reference_expr_name_in_dae(dae, expr) {
                out.push(name);
            } else {
                collect_exact_reference_expr_names_in_dae(dae, base, out);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_exact_reference_expr_names_in_dae(dae, lhs, out);
            collect_exact_reference_expr_names_in_dae(dae, rhs, out);
        }
        Expression::Unary { rhs, .. } => collect_exact_reference_expr_names_in_dae(dae, rhs, out),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_exact_reference_expr_names_in_dae(dae, arg, out);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                collect_exact_reference_expr_names_in_dae(dae, condition, out);
                collect_exact_reference_expr_names_in_dae(dae, value, out);
            }
            collect_exact_reference_expr_names_in_dae(dae, else_branch, out);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            for element in elements {
                collect_exact_reference_expr_names_in_dae(dae, element, out);
            }
        }
        Expression::Range {
            start, step, end, ..
        } => {
            collect_exact_reference_expr_names_in_dae(dae, start, out);
            if let Some(step) = step {
                collect_exact_reference_expr_names_in_dae(dae, step, out);
            }
            collect_exact_reference_expr_names_in_dae(dae, end, out);
        }
        Expression::ArrayComprehension { expr, filter, .. } => {
            collect_exact_reference_expr_names_in_dae(dae, expr, out);
            if let Some(filter) = filter {
                collect_exact_reference_expr_names_in_dae(dae, filter, out);
            }
        }
        Expression::Literal { .. } | Expression::Empty { .. } => {}
    }
}

pub(super) fn exact_reference_expr_name(expr: &Expression) -> Option<VarName> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => exact_name_with_subscripts(name.var_name().as_str(), subscripts).map(VarName::new),
        Expression::Index {
            base, subscripts, ..
        } => {
            let base_name = exact_reference_expr_name(base)?;
            exact_name_with_subscripts(base_name.as_str(), subscripts).map(VarName::new)
        }
        Expression::FieldAccess { base, field, .. } => {
            let base_name = exact_reference_expr_name(base)?;
            Some(VarName::new(format!("{}.{field}", base_name.as_str())))
        }
        _ => None,
    }
}

pub(super) fn exact_reference_expr_name_in_dae(dae: &Dae, expr: &Expression) -> Option<VarName> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => exact_name_with_subscripts_in_dae(dae, name.var_name().as_str(), subscripts)
            .map(VarName::new),
        Expression::Index {
            base, subscripts, ..
        } => {
            let base_name = exact_reference_expr_name_in_dae(dae, base)?;
            exact_name_with_subscripts_in_dae(dae, base_name.as_str(), subscripts).map(VarName::new)
        }
        Expression::FieldAccess { base, field, .. } => {
            let base_name = exact_reference_expr_name_in_dae(dae, base)?;
            Some(VarName::new(format!("{}.{field}", base_name.as_str())))
        }
        _ => None,
    }
}

fn exact_name_with_subscripts(base: &str, subscripts: &[rumoca_core::Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(base.to_string());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        indices.push(exact_subscript_index(subscript)?.to_string());
    }
    Some(format!("{base}[{}]", indices.join(",")))
}

fn exact_name_with_subscripts_in_dae(
    dae: &Dae,
    base: &str,
    subscripts: &[rumoca_core::Subscript],
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(base.to_string());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        indices.push(exact_subscript_index_in_dae(dae, subscript)?.to_string());
    }
    Some(format!("{base}[{}]", indices.join(",")))
}

fn exact_subscript_index(subscript: &rumoca_core::Subscript) -> Option<i64> {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => Some(*value),
        rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } => Some(*value),
            Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } if value.is_finite() && value.fract() == 0.0 => Some(*value as i64),
            _ => None,
        },
        rumoca_core::Subscript::Colon { .. } => None,
    }
}

fn exact_subscript_index_in_dae(dae: &Dae, subscript: &rumoca_core::Subscript) -> Option<i64> {
    match subscript {
        rumoca_core::Subscript::Index { value, .. } => Some(*value),
        rumoca_core::Subscript::Expr { expr, .. } => exact_index_expr_in_dae(dae, expr),
        rumoca_core::Subscript::Colon { .. } => None,
    }
}

fn exact_index_expr_in_dae(dae: &Dae, expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } if value.is_finite() && value.fract() == 0.0 => Some(*value as i64),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => fixed_integer_parameter_start(dae, name.var_name()),
        Expression::Unary { op, rhs, .. } => match op {
            OpUnary::Plus => exact_index_expr_in_dae(dae, rhs),
            OpUnary::Minus => {
                exact_index_expr_in_dae(dae, rhs).and_then(|value| value.checked_neg())
            }
            _ => None,
        },
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = exact_index_expr_in_dae(dae, lhs)?;
            let rhs = exact_index_expr_in_dae(dae, rhs)?;
            match op {
                OpBinary::Add | OpBinary::AddElem => lhs.checked_add(rhs),
                OpBinary::Sub | OpBinary::SubElem => lhs.checked_sub(rhs),
                OpBinary::Mul | OpBinary::MulElem => lhs.checked_mul(rhs),
                OpBinary::Div | OpBinary::DivElem if rhs != 0 && lhs % rhs == 0 => Some(lhs / rhs),
                _ => None,
            }
        }
        _ => None,
    }
}

fn fixed_integer_parameter_start(dae: &Dae, name: &VarName) -> Option<i64> {
    let var = dae
        .variables
        .parameters
        .get(name)
        .filter(|var| !var.is_tunable)
        .or_else(|| dae.variables.constants.get(name))?;
    let start = var.start.as_ref()?;
    exact_index_expr_in_dae(dae, start)
}

fn dae_var_size(dae: &Dae, name: &VarName) -> Result<usize, StructuralError> {
    DaeVariableScope::new(dae).size(name)
}

fn dae_var_dims(dae: &Dae, name: &VarName) -> Result<Vec<i64>, StructuralError> {
    DaeVariableScope::new(dae).dims(name)
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
    let expr = project_scalarized_unknown_solution(&var_name, expr);
    Ok(Substitution {
        var_dims: scope.dims(&var_name)?,
        replacement_dims: replacement_expr_dims(dae, &expr)?,
        env_keys: vec![var_name.as_str().to_string()],
        var_ref: scope
            .exact(&var_name)
            .and_then(|var| var.component_ref.clone())
            .map(Reference::from_component_reference),
        var_name,
        expr,
    })
}

fn project_scalarized_unknown_solution(var_name: &VarName, expr: Expression) -> Expression {
    let Some(scalar_name) = rumoca_core::parse_scalar_name(var_name.as_str()) else {
        return expr;
    };
    let Some(span) = expr.span() else {
        return expr;
    };
    let Ok(subscripts) = scalar_name
        .indices
        .iter()
        .map(|index| {
            rumoca_core::Subscript::try_generated_index(
                *index,
                span,
                "scalarized unknown solution projection",
            )
        })
        .collect::<Result<Vec<_>, _>>()
    else {
        return expr;
    };
    project_replacement_expr_with_subscripts(&expr, &subscripts, span).unwrap_or(expr)
}

fn replacement_expr_dims(dae: &Dae, expr: &Expression) -> Result<Vec<i64>, StructuralError> {
    Ok(match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => dae_var_dims(dae, name.var_name())?,
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
    let runtime_protected_unknowns = runtime_protected_unknown_names(dae);
    let runtime_defined_discrete_targets = runtime_defined_discrete_target_names(dae);
    let mut substitutions: Vec<Substitution> = Vec::new();
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
                &mut substitutions,
                &mut eliminated_eq_indices,
                &mut eliminated_eq_flags,
                &mut eliminated_var_names,
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
                &mut substitutions,
                &mut eliminated_eq_indices,
                &mut eliminated_eq_flags,
                &mut eliminated_var_names,
            )?,
        }
    }

    // Apply BLT substitutions once to the remaining equations.
    apply_substitutions_to_remaining_once(dae, &eliminated_eq_flags, &substitutions)?;

    let n_eliminated = eliminated_eq_indices.len();

    // Remove eliminated equations (in reverse order to preserve indices).
    eliminated_eq_indices.sort_unstable();
    for &idx in eliminated_eq_indices.iter().rev() {
        dae.continuous.equations.remove(idx);
    }
    shift_structured_families_after_equation_removal(dae, &eliminated_eq_indices);

    // Remove eliminated variables from algebraics and outputs.
    for name in &eliminated_var_names {
        dae.variables.algebraics.shift_remove(name);
        dae.variables.outputs.shift_remove(name);
    }

    Ok(EliminationResult {
        substitutions,
        n_eliminated,
        blt_error: None,
    })
}

#[allow(clippy::too_many_arguments)]
fn eliminate_scalar_blt_block(
    dae: &Dae,
    equation: &EquationRef,
    unknown: &UnknownId,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
    state_derivative_matcher: &DerivativeNameMatcher,
    substitutions: &mut Vec<Substitution>,
    eliminated_eq_indices: &mut Vec<usize>,
    eliminated_eq_flags: &mut [bool],
    eliminated_var_names: &mut Vec<VarName>,
) -> Result<(), StructuralError> {
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
    substitutions: &[Substitution],
) -> Result<Option<(usize, VarName, Expression)>, StructuralError> {
    let Some(raw_var_name) = algebraic_or_output_unknown(unknown) else {
        return Ok(None);
    };
    let var_name = raw_var_name.clone();
    let eq_idx = equation.0;
    let is_output = output_partition_contains_unknown(dae, &var_name);
    let has_state_derivative = equation_has_state_derivative(dae, eq_idx, state_derivative_matcher);
    if !can_eliminate_scalar_unknown(
        dae,
        &var_name,
        runtime_protected_unknowns,
        has_state_derivative,
    )? {
        return Ok(None);
    }
    if has_state_derivative && !is_output {
        return Ok(None);
    }

    let Some(eq_rhs) = apply_substitutions_for_symbolic_candidate(
        &dae.continuous.equations[eq_idx].rhs,
        substitutions,
    )?
    else {
        return Ok(None);
    };
    if should_preserve_runtime_sensitive_continuous_assignment(dae, &eq_rhs) {
        return Ok(None);
    }
    if expr_contains_indexed_multiscalar_slice_ref(&eq_rhs, dae)? {
        return Ok(None);
    }
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
    if scalar_blt_solution_would_break_aggregate_element(dae, &var_name, &solution)? {
        return Ok(None);
    }
    if is_output
        && !is_trivial_alias_in_dae(dae, &solution)
        && !is_internal_component_output(dae, &var_name)
    {
        return Ok(None);
    }
    if !is_trivial_alias_in_dae(dae, &solution)
        && !solution_is_cheap_for_symbolic_substitution(&solution)
    {
        return Ok(None);
    }
    Ok(Some((eq_idx, var_name, solution)))
}

pub(super) fn apply_substitutions_for_symbolic_candidate(
    expr: &Expression,
    substitutions: &[Substitution],
) -> Result<Option<Expression>, StructuralError> {
    let mut out = apply_record_field_aggregate_substitutions(expr, substitutions, None);
    for sub in substitutions {
        if !expr_contains_substitution_target(&out, sub) {
            continue;
        }
        if !is_trivial_alias(&sub.expr) && !solution_is_cheap_for_symbolic_substitution(&sub.expr) {
            return Ok(None);
        }
        out = SubstituteVarRewriter {
            substitution: sub,
            replacement: &sub.expr,
            replacement_dims: &sub.replacement_dims,
            derivative_replacement: None,
        }
        .rewrite_expression(&out)?;
        if !expression_is_within_symbolic_candidate_budget(&out) {
            return Ok(None);
        }
    }
    Ok(Some(out))
}

fn is_internal_component_output(dae: &Dae, var_name: &VarName) -> bool {
    let Some(var) = dae.variables.outputs.get(var_name).or_else(|| {
        rumoca_ir_dae::component_base_name(var_name.as_str())
            .and_then(|base| dae.variables.outputs.get(&VarName::new(base)))
    }) else {
        return false;
    };
    var.component_ref
        .as_ref()
        .is_some_and(|component_ref| component_ref.parts.len() > 1)
        || rumoca_core::component_reference_from_flat_name(var_name, var.source_span)
            .is_some_and(|component_ref| component_ref.parts.len() > 1)
}

fn algebraic_or_output_unknown(unknown: &UnknownId) -> Option<&VarName> {
    match unknown {
        UnknownId::Variable(name) => Some(name),
        UnknownId::DerState(_) | UnknownId::SolverY(_) => None,
    }
}

fn is_derivative_alias_expr(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } if args.len() == 1
    )
}

fn can_eliminate_scalar_unknown(
    dae: &Dae,
    var_name: &VarName,
    runtime_protected_unknowns: &IndexSet<String>,
    has_state_derivative: bool,
) -> Result<bool, StructuralError> {
    if is_runtime_protected_unknown(var_name, runtime_protected_unknowns)
        || unknown_is_fixed(dae, var_name)
        || dae.variables.states.contains_key(var_name)
        || dae_var_size(dae, var_name)? != 1
    {
        return Ok(false);
    }
    if !is_scalarized_element_of_aggregate(dae, var_name)? {
        return Ok(true);
    }
    Ok(!has_state_derivative && scalarized_element_has_non_connection_use(dae, var_name))
}

pub(super) fn scalar_blt_solution_would_break_aggregate_element(
    dae: &Dae,
    var_name: &VarName,
    solution: &Expression,
) -> Result<bool, StructuralError> {
    if !is_scalarized_element_of_aggregate(dae, var_name)? {
        return Ok(false);
    }
    if is_trivial_alias_in_dae(dae, solution) {
        return Ok(false);
    }
    Ok(scalarized_element_has_coupled_derivative_use(dae, var_name)
        || (expr_contains_runtime_sensitive_operator(solution)
            && scalarized_element_has_non_connection_use(dae, var_name)))
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
    let Some(solution) = try_solve_for_unknown_in_dae(dae, rhs, var_name) else {
        return Ok(None);
    };
    if expr_contains_var(&solution, var_name)
        || solution_has_blocking_unsliced_multiscalar_ref(&solution, dae)?
        || !is_symbolically_stable_solution(&solution)
    {
        return Ok(None);
    }
    Ok(Some(solution))
}

/// Apply substitutions in-order to an expression.
pub fn apply_substitutions_to_expr(
    expr: &Expression,
    substitutions: &[Substitution],
) -> Result<Expression, StructuralError> {
    apply_substitutions_to_expr_with_derivatives(expr, substitutions, |_| Ok(None))
}

pub(crate) fn apply_substitutions_to_expr_with_derivatives(
    expr: &Expression,
    substitutions: &[Substitution],
    derivative_replacement_for: impl FnMut(&Substitution) -> Result<Option<Expression>, StructuralError>,
) -> Result<Expression, StructuralError> {
    apply_substitutions_to_expr_with_derivatives_and_dae(
        expr,
        substitutions,
        None,
        derivative_replacement_for,
    )
}

pub(crate) fn apply_substitutions_to_expr_with_derivatives_and_dae(
    expr: &Expression,
    substitutions: &[Substitution],
    dae_context: Option<&Dae>,
    mut derivative_replacement_for: impl FnMut(
        &Substitution,
    ) -> Result<Option<Expression>, StructuralError>,
) -> Result<Expression, StructuralError> {
    let mut out = apply_record_field_aggregate_substitutions(expr, substitutions, dae_context);
    for sub in substitutions {
        if expr_contains_substitution_target(&out, sub) {
            let derivative_replacement = if expr_contains_derivative_substitution_target(&out, sub)
            {
                derivative_replacement_for(sub)?
            } else {
                None
            };
            out = SubstituteVarRewriter {
                substitution: sub,
                replacement: &sub.expr,
                replacement_dims: &sub.replacement_dims,
                derivative_replacement: derivative_replacement.as_ref(),
            }
            .rewrite_expression(&out)?;
        }
    }
    Ok(out)
}

fn apply_record_field_aggregate_substitutions(
    expr: &Expression,
    substitutions: &[Substitution],
    dae_context: Option<&Dae>,
) -> Expression {
    let aggregate_alias_groups = aggregate_alias_substitution_groups(substitutions);
    let complex_groups = complex_field_substitution_groups(substitutions);
    if aggregate_alias_groups.is_empty() && complex_groups.is_empty() {
        return expr.clone();
    }
    RecordFieldAggregateRewriter {
        aggregate_alias_groups,
        complex_groups,
        dae_scope: dae_context.map(DaeVariableScope::new),
    }
    .rewrite_expression(expr)
}

#[derive(Debug, Clone, Default)]
struct AggregateAliasSubstitutionGroup {
    dims: Vec<usize>,
    replacement_base: Option<Reference>,
    values: IndexMap<Vec<usize>, Expression>,
}

impl AggregateAliasSubstitutionGroup {
    fn insert(&mut self, indices: Vec<usize>, expr: Expression, replacement_dims: &[i64]) {
        if indices.len() > self.dims.len() {
            self.dims.resize(indices.len(), 0);
        }
        for (idx, value) in indices.iter().enumerate() {
            self.dims[idx] = self.dims[idx].max(*value);
        }
        self.replacement_base = replacement_aggregate_base(&expr, &indices, &self.replacement_base);
        self.values.insert(
            indices.clone(),
            project_scalar_alias_replacement(expr, &indices, replacement_dims),
        );
    }

    fn to_replacement_expr(&self, span: rumoca_core::Span) -> Option<Expression> {
        let expected_len = self.expected_len();
        if self.dims.is_empty() || expected_len <= 1 || self.values.len() != expected_len {
            return None;
        }
        if let Some(base) = &self.replacement_base {
            return Some(Expression::VarRef {
                name: base.clone(),
                subscripts: Vec::new(),
                span,
            });
        }
        self.array_expr_at_depth(0, &mut Vec::new(), span)
    }

    fn to_indexed_replacement_expr(
        &self,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Option<Expression> {
        let expected_len = self.expected_len();
        if self.dims.is_empty() || expected_len <= 1 || self.values.len() != expected_len {
            return None;
        }
        Some(Expression::Index {
            base: Box::new(self.array_expr_at_depth(0, &mut Vec::new(), span)?),
            subscripts: subscripts.to_vec(),
            span,
        })
    }

    fn covers_dims(&self, dims: &[usize]) -> bool {
        !dims.is_empty() && self.dims == dims && self.values.len() == dims.iter().product::<usize>()
    }

    fn to_partial_replacement_expr(
        &self,
        name: &Reference,
        dims: &[usize],
        span: rumoca_core::Span,
    ) -> Option<Expression> {
        if dims.is_empty() || dims.iter().product::<usize>() <= 1 || self.values.is_empty() {
            return None;
        }
        self.partial_array_expr_at_depth(name, dims, 0, &mut Vec::new(), span)
    }

    fn expected_len(&self) -> usize {
        self.dims.iter().product()
    }

    fn array_expr_at_depth(
        &self,
        depth: usize,
        current: &mut Vec<usize>,
        span: rumoca_core::Span,
    ) -> Option<Expression> {
        if depth >= self.dims.len() {
            return self.values.get(current).cloned();
        }
        let mut elements = Vec::with_capacity(self.dims[depth]);
        for index in 1..=self.dims[depth] {
            current.push(index);
            elements.push(self.array_expr_at_depth(depth + 1, current, span)?);
            current.pop();
        }
        Some(Expression::Array {
            elements,
            is_matrix: depth == 0 && self.dims.len() == 2,
            span,
        })
    }

    fn partial_array_expr_at_depth(
        &self,
        name: &Reference,
        dims: &[usize],
        depth: usize,
        current: &mut Vec<usize>,
        span: rumoca_core::Span,
    ) -> Option<Expression> {
        if depth >= dims.len() {
            return self
                .values
                .get(current)
                .cloned()
                .or_else(|| scalar_ref_for_indices(name, current, span));
        }
        let mut elements = Vec::with_capacity(dims[depth]);
        for index in 1..=dims[depth] {
            current.push(index);
            elements.push(self.partial_array_expr_at_depth(
                name,
                dims,
                depth + 1,
                current,
                span,
            )?);
            current.pop();
        }
        Some(Expression::Array {
            elements,
            is_matrix: depth == 0 && dims.len() == 2,
            span,
        })
    }
}

fn project_scalar_alias_replacement(
    expr: Expression,
    indices: &[usize],
    replacement_dims: &[i64],
) -> Expression {
    if replacement_dims.is_empty() || indices.is_empty() {
        return expr;
    }
    let replacement_rank = replacement_dims.len();
    let start = indices.len().saturating_sub(replacement_rank);
    let projected_indices = &indices[start..];
    let Some(span) = expr.span() else {
        return expr;
    };
    let Ok(owner) = span.require_provenance("scalar alias replacement projection") else {
        return expr;
    };
    let projected_subscripts = projected_indices
        .iter()
        .map(|index| rumoca_core::Subscript::generated_index_with_provenance(*index as i64, owner))
        .collect::<Vec<_>>();
    match expr {
        Expression::VarRef {
            name,
            mut subscripts,
            span,
        } => {
            subscripts.extend(projected_subscripts);
            Expression::VarRef {
                name,
                subscripts,
                span,
            }
        }
        _ => Expression::Index {
            base: Box::new(expr),
            subscripts: projected_subscripts,
            span,
        },
    }
}

fn scalar_ref_for_indices(
    name: &Reference,
    indices: &[usize],
    span: rumoca_core::Span,
) -> Option<Expression> {
    let Ok(owner) = span.require_provenance("scalar alias replacement reference") else {
        return Some(Expression::VarRef {
            name: name.clone(),
            subscripts: Vec::new(),
            span,
        });
    };
    Some(Expression::VarRef {
        name: name.clone(),
        subscripts: indices
            .iter()
            .map(|index| {
                rumoca_core::Subscript::generated_index_with_provenance(*index as i64, owner)
            })
            .collect(),
        span,
    })
}

fn replacement_aggregate_base(
    expr: &Expression,
    expected_indices: &[usize],
    existing_base: &Option<Reference>,
) -> Option<Reference> {
    let (base, indices) = scalar_var_ref_key(expr)?;
    (indices == expected_indices
        && existing_base
            .as_ref()
            .is_none_or(|existing| references_same_base(existing, &base)))
    .then_some(base)
}

fn aggregate_alias_substitution_groups(
    substitutions: &[Substitution],
) -> IndexMap<VarName, AggregateAliasSubstitutionGroup> {
    let mut groups = IndexMap::new();
    for substitution in substitutions {
        let Some((base, indices)) = scalar_substitution_target_key(substitution) else {
            continue;
        };
        groups
            .entry(base.into_var_name())
            .or_insert_with(AggregateAliasSubstitutionGroup::default)
            .insert(
                indices,
                substitution.expr.clone(),
                &substitution.replacement_dims,
            );
    }
    groups
}

fn scalar_substitution_target_key(substitution: &Substitution) -> Option<(Reference, Vec<usize>)> {
    if let Some(var_ref) = &substitution.var_ref
        && let Some(key) = scalar_var_ref_key_from_reference(var_ref)
    {
        return Some(key);
    }
    if let Some(key) = embedded_indexed_path_key(substitution.var_name.as_str()) {
        return Some(key);
    }
    let scalar = rumoca_core::parse_scalar_name(substitution.var_name.as_str())?;
    let indices = scalar
        .indices
        .iter()
        .copied()
        .map(usize::try_from)
        .collect::<Result<Vec<_>, _>>()
        .ok()?;
    if indices.iter().all(|index| *index > 0) {
        Some((Reference::new(scalar.base), indices))
    } else {
        None
    }
}

fn embedded_indexed_path_key(raw: &str) -> Option<(Reference, Vec<usize>)> {
    let mut base = String::with_capacity(raw.len());
    let mut indices = Vec::new();
    let mut chars = raw.char_indices().peekable();
    while let Some((_, ch)) = chars.next() {
        if ch != '[' {
            base.push(ch);
            continue;
        }
        let mut value = String::new();
        let mut closed = false;
        for (_, inner) in chars.by_ref() {
            if inner == ']' {
                closed = true;
                break;
            }
            value.push(inner);
        }
        if !closed || value.is_empty() || !value.chars().all(|c| c.is_ascii_digit()) {
            return None;
        }
        let index = value.parse::<usize>().ok()?;
        if index == 0 {
            return None;
        }
        indices.push(index);
    }
    (!indices.is_empty() && base != raw).then_some((Reference::new(base), indices))
}

fn scalar_var_ref_key(expr: &Expression) -> Option<(Reference, Vec<usize>)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    scalar_var_ref_key_from_reference(name)
}

fn scalar_var_ref_key_from_reference(reference: &Reference) -> Option<(Reference, Vec<usize>)> {
    let component_ref = reference.component_ref()?;
    let mut base = component_ref.clone();
    let mut indices = Vec::new();
    for part in &mut base.parts {
        if part.subs.is_empty() {
            continue;
        }
        indices.extend(positive_usize_subscripts(&part.subs)?);
        part.subs.clear();
    }
    (!indices.is_empty()).then_some((Reference::from_component_reference(base), indices))
}

fn positive_usize_subscripts(subscripts: &[rumoca_core::Subscript]) -> Option<Vec<usize>> {
    subscripts
        .iter()
        .map(positive_usize_subscript)
        .collect::<Option<Vec<_>>>()
}

fn positive_usize_subscript(subscript: &rumoca_core::Subscript) -> Option<usize> {
    let index = match subscript {
        rumoca_core::Subscript::Index { value, .. } => *value,
        rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                ..
            } => *value,
            Expression::Literal {
                value: rumoca_core::Literal::Real(value),
                ..
            } if value.is_finite() && value.fract() == 0.0 => *value as i64,
            _ => return None,
        },
        rumoca_core::Subscript::Colon { .. } => return None,
    };
    usize::try_from(index).ok().filter(|value| *value > 0)
}

fn subscripts_are_static_scalar_indices(subscripts: &[rumoca_core::Subscript]) -> bool {
    !subscripts.is_empty()
        && subscripts
            .iter()
            .all(|subscript| positive_usize_subscript(subscript).is_some())
}

fn references_same_base(lhs: &Reference, rhs: &Reference) -> bool {
    lhs.var_name().id() == rhs.var_name().id()
}

fn reference_has_scalar_indices(reference: &Reference) -> bool {
    reference
        .component_ref()
        .is_some_and(component_ref_has_scalar_indices)
}

fn component_ref_has_scalar_indices(component_ref: &rumoca_core::ComponentReference) -> bool {
    component_ref
        .parts
        .iter()
        .flat_map(|part| &part.subs)
        .any(|subscript| positive_usize_subscript(subscript).is_some())
}

fn substitution_has_scalar_indices(substitution: &Substitution) -> bool {
    substitution
        .var_ref
        .as_ref()
        .is_some_and(reference_has_scalar_indices)
        || rumoca_core::parse_scalar_name(substitution.var_name.as_str()).is_some()
}

fn reference_complex_field(reference: &Reference) -> Option<&str> {
    reference
        .component_ref()?
        .last_ident()
        .filter(|field| matches!(*field, "re" | "im"))
}

fn substitution_complex_field(substitution: &Substitution) -> Option<&str> {
    substitution
        .var_ref
        .as_ref()
        .and_then(reference_complex_field)
}

fn substitution_indexed_base_matches(name: &Reference, substitution: &Substitution) -> bool {
    let Some(var_ref) = &substitution.var_ref else {
        return false;
    };
    let Some((base, _)) = scalar_var_ref_key_from_reference(var_ref) else {
        return false;
    };
    references_same_base(name, &base)
}

#[derive(Debug, Clone, Default)]
struct ComplexFieldSubstitutionGroup {
    re: Option<Expression>,
    im: Option<Expression>,
}

impl ComplexFieldSubstitutionGroup {
    fn insert(&mut self, field: &str, expr: Expression) {
        match field {
            "re" => self.re = Some(expr),
            "im" => self.im = Some(expr),
            _ => {}
        }
    }

    fn to_constructor_expr(&self, span: rumoca_core::Span) -> Option<Expression> {
        Some(Expression::FunctionCall {
            name: Reference::new("Complex"),
            args: vec![self.re.clone()?, self.im.clone()?],
            is_constructor: true,
            span,
        })
    }
}

fn complex_field_substitution_groups(
    substitutions: &[Substitution],
) -> IndexMap<String, ComplexFieldSubstitutionGroup> {
    let mut groups = IndexMap::new();
    for substitution in substitutions {
        let Some((base, field)) = split_complex_field_suffix(substitution.var_name.as_str()) else {
            continue;
        };
        groups
            .entry(base.to_string())
            .or_insert_with(ComplexFieldSubstitutionGroup::default)
            .insert(field, substitution.expr.clone());
    }
    groups
}

struct RecordFieldAggregateRewriter<'a> {
    aggregate_alias_groups: IndexMap<VarName, AggregateAliasSubstitutionGroup>,
    complex_groups: IndexMap<String, ComplexFieldSubstitutionGroup>,
    dae_scope: Option<DaeVariableScope<'a>>,
}

impl ExpressionRewriter for RecordFieldAggregateRewriter<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Expression {
        if !subscripts.is_empty()
            && !subscripts_are_static_scalar_indices(subscripts)
            && let Some(group) = self.aggregate_alias_groups.get(name.var_name())
            && self.group_covers_reference_dims(name, group)
            && let Some(replacement) = group.to_indexed_replacement_expr(subscripts, span)
        {
            return replacement;
        }
        if !subscripts.is_empty() {
            return self.walk_var_ref_expression(name, subscripts, span);
        }
        if let Some(group) = self.aggregate_alias_groups.get(name.var_name())
            && self.group_covers_reference_dims(name, group)
            && let Some(replacement) = group.to_replacement_expr(span)
        {
            return replacement;
        }
        if let Some(replacement) =
            self.aggregate_alias_groups
                .get(name.var_name())
                .and_then(|group| {
                    self.aggregate_dims_for_reference(name)
                        .and_then(|dims| group.to_partial_replacement_expr(name, &dims, span))
                })
        {
            return replacement;
        }
        self.complex_groups
            .get(name.as_str())
            .and_then(|group| group.to_constructor_expr(span))
            .unwrap_or_else(|| self.walk_var_ref_expression(name, subscripts, span))
    }
}

impl RecordFieldAggregateRewriter<'_> {
    fn group_covers_reference_dims(
        &self,
        name: &Reference,
        group: &AggregateAliasSubstitutionGroup,
    ) -> bool {
        self.aggregate_dims_for_reference(name)
            .is_none_or(|dims| group.covers_dims(&dims))
    }

    fn aggregate_dims_for_reference(&self, name: &Reference) -> Option<Vec<usize>> {
        let dims = self.dae_scope.as_ref()?.dims(name.var_name()).ok()?;
        dims.into_iter()
            .map(usize::try_from)
            .collect::<Result<Vec<_>, _>>()
            .ok()
            .filter(|dims| dims.iter().all(|dim| *dim > 0))
    }
}

pub fn resolve_substitutions_in_expr(
    expr: &Expression,
    substitutions: &[Substitution],
) -> Result<Expression, StructuralError> {
    let mut out = expr.clone();
    for _ in 0..substitutions.len() {
        let next = apply_substitutions_to_expr(&out, substitutions)?;
        if next == out {
            return Ok(out);
        }
        out = next;
    }
    Ok(out)
}

pub(super) fn embedded_alias_indices_for_substitution(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    substitution: &Substitution,
) -> Option<Vec<i64>> {
    if !subscripts.is_empty() || name.var_name().id() == substitution.var_name.id() {
        return None;
    }
    if reference_complex_field(name).is_some() || substitution_complex_field(substitution).is_some()
    {
        return None;
    }
    if substitution_has_scalar_indices(substitution) {
        return None;
    }
    let var_ref = substitution.var_ref.as_ref()?;
    let (name_base, indices) = scalar_var_ref_key_from_reference(name)?;
    if !references_same_base(&name_base, var_ref) {
        return None;
    }
    Some(indices.into_iter().map(|index| index as i64).collect())
}

fn index_replacement_expr(
    replacement: &Expression,
    indices: &[i64],
    fallback_span: rumoca_core::Span,
) -> Result<Expression, StructuralError> {
    let provenance = projection_owner_span(replacement, fallback_span)?;
    let span = provenance.span();
    if indices.is_empty() {
        return Ok(replacement.clone().with_span(span));
    }
    let extra_subscripts = indices
        .iter()
        .copied()
        .map(|index| rumoca_core::Subscript::generated_index_with_provenance(index, provenance))
        .collect::<Vec<_>>();
    Ok(match replacement {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut projected_subscripts = subscripts.clone();
            projected_subscripts.extend(extra_subscripts);
            Expression::VarRef {
                name: name.clone(),
                subscripts: projected_subscripts,
                span,
            }
        }
        _ => Expression::Index {
            base: Box::new(replacement.clone()),
            subscripts: extra_subscripts,
            span,
        },
    })
}

fn index_replacement_expr_with_subscripts(
    replacement: &Expression,
    subscripts: &[rumoca_core::Subscript],
    fallback_span: rumoca_core::Span,
) -> Result<Expression, StructuralError> {
    let span = projection_owner_span(replacement, fallback_span)?.span();
    if subscripts.is_empty() {
        return Ok(replacement.clone().with_span(span));
    }
    Ok(match replacement {
        Expression::VarRef {
            name,
            subscripts: replacement_subscripts,
            ..
        } => {
            let mut projected_subscripts = replacement_subscripts.clone();
            projected_subscripts.extend(subscripts.iter().cloned());
            Expression::VarRef {
                name: name.clone(),
                subscripts: projected_subscripts,
                span,
            }
        }
        _ => project_replacement_expr_with_subscripts(replacement, subscripts, span)
            .unwrap_or_else(|| Expression::Index {
                base: Box::new(replacement.clone()),
                subscripts: subscripts.to_vec(),
                span,
            }),
    })
}

fn project_replacement_expr_with_subscripts(
    replacement: &Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
) -> Option<Expression> {
    let (first, rest) = subscripts.split_first()?;
    match first {
        rumoca_core::Subscript::Index { value, .. } => {
            project_replacement_expr_index(replacement, *value, rest, span)
        }
        rumoca_core::Subscript::Expr { expr, .. } => {
            if let Some(value) = literal_integer_value(expr) {
                project_replacement_expr_index(replacement, value, rest, span)
            } else {
                project_replacement_expr_symbolic_index(replacement, expr, rest, span)
            }
        }
        rumoca_core::Subscript::Colon { .. } => None,
    }
}

fn project_replacement_expr_index(
    replacement: &Expression,
    one_based_index: i64,
    rest: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
) -> Option<Expression> {
    match replacement {
        Expression::Array { elements, .. } => {
            let zero_based = usize::try_from(one_based_index.checked_sub(1)?).ok()?;
            let element = elements.get(zero_based)?.clone().with_span(span);
            if rest.is_empty() {
                Some(element)
            } else {
                project_replacement_expr_with_subscripts(&element, rest, span)
            }
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            if filter.is_some() || indices.len() != 1 {
                return None;
            }
            let value = comprehension_index_value(&indices[0].range, one_based_index)?;
            let selected =
                substitute_comprehension_index_literal(expr, &indices[0].name, value, span);
            if rest.is_empty() {
                Some(selected)
            } else {
                project_replacement_expr_with_subscripts(&selected, rest, span)
            }
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs_selected = project_replacement_expr_index(lhs, one_based_index, rest, span);
            let rhs_selected = project_replacement_expr_index(rhs, one_based_index, rest, span);
            project_binary_selection(op.clone(), lhs, rhs, lhs_selected, rhs_selected, span)
        }
        _ => None,
    }
}

fn project_replacement_expr_symbolic_index(
    replacement: &Expression,
    index: &Expression,
    rest: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
) -> Option<Expression> {
    match replacement {
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            if filter.is_some() || indices.len() != 1 {
                return None;
            }
            let selected = substitute_comprehension_index_expr(expr, &indices[0].name, index, span);
            if rest.is_empty() {
                Some(selected)
            } else {
                project_replacement_expr_with_subscripts(&selected, rest, span)
            }
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs_selected = project_replacement_expr_symbolic_index(lhs, index, rest, span);
            let rhs_selected = project_replacement_expr_symbolic_index(rhs, index, rest, span);
            project_binary_selection(op.clone(), lhs, rhs, lhs_selected, rhs_selected, span)
        }
        _ => None,
    }
}

fn project_binary_selection(
    op: OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    lhs_selected: Option<Expression>,
    rhs_selected: Option<Expression>,
    span: rumoca_core::Span,
) -> Option<Expression> {
    match (lhs_selected, rhs_selected) {
        (Some(lhs), Some(rhs)) => Some(Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        }),
        (Some(lhs), None) => Some(Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs.clone().with_span(span)),
            span,
        }),
        (None, Some(rhs)) => Some(Expression::Binary {
            op,
            lhs: Box::new(lhs.clone().with_span(span)),
            rhs: Box::new(rhs),
            span,
        }),
        (None, None) => None,
    }
}

fn comprehension_index_value(range: &Expression, one_based_index: i64) -> Option<i64> {
    let Expression::Range {
        start, step, end, ..
    } = range
    else {
        return None;
    };
    let start = literal_integer_value(start)?;
    let step = match step.as_deref() {
        Some(step) => literal_integer_value(step)?,
        None => 1,
    };
    let value = start + (one_based_index.checked_sub(1)?) * step;
    let end = literal_integer_value(end)?;
    ((step > 0 && value <= end) || (step < 0 && value >= end) || (step == 0 && value == start))
        .then_some(value)
}

fn literal_integer_value(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        _ => None,
    }
}

fn substitute_comprehension_index_literal(
    expr: &Expression,
    name: &str,
    value: i64,
    span: rumoca_core::Span,
) -> Expression {
    substitute_comprehension_index_expr(
        expr,
        name,
        &Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span,
        },
        span,
    )
}

fn substitute_comprehension_index_expr(
    expr: &Expression,
    name: &str,
    replacement: &Expression,
    span: rumoca_core::Span,
) -> Expression {
    struct Substituter<'a> {
        name: &'a str,
        replacement: &'a Expression,
        span: rumoca_core::Span,
    }

    impl rumoca_core::ExpressionRewriter for Substituter<'_> {
        fn walk_var_ref_expression(
            &mut self,
            name: &Reference,
            subscripts: &[rumoca_core::Subscript],
            span: rumoca_core::Span,
        ) -> Expression {
            if name.as_str() == self.name && subscripts.is_empty() {
                return self.replacement.clone().with_span(self.span);
            }
            Expression::VarRef {
                name: name.clone(),
                subscripts: self.rewrite_subscripts(subscripts),
                span,
            }
        }

        fn walk_array_comprehension_expression(
            &mut self,
            expr: &Expression,
            indices: &[rumoca_core::ComprehensionIndex],
            filter: Option<&Expression>,
            span: rumoca_core::Span,
        ) -> Expression {
            if indices.iter().any(|index| index.name == self.name) {
                return Expression::ArrayComprehension {
                    expr: Box::new(expr.clone()),
                    indices: indices.to_vec(),
                    filter: filter.cloned().map(Box::new),
                    span,
                };
            }
            rumoca_core::ExpressionRewriter::walk_array_comprehension_expression(
                self, expr, indices, filter, span,
            )
        }
    }

    let mut substituter = Substituter {
        name,
        replacement,
        span,
    };
    substituter.rewrite_expression(expr)
}

fn projection_owner_span(
    replacement: &Expression,
    fallback_span: rumoca_core::Span,
) -> Result<rumoca_core::ProvenanceSpan, StructuralError> {
    let span = if fallback_span.is_dummy() {
        replacement.span().unwrap_or(fallback_span)
    } else {
        fallback_span
    };
    span.require_provenance("structural substitution projection")
        .map_err(|err| StructuralError::UnspannedContractViolation {
            reason: err.to_string(),
        })
}

fn replacement_indices_for_alias(
    indices: &[i64],
    var_dims: &[i64],
    replacement_dims: &[i64],
) -> Vec<i64> {
    if indices.is_empty() {
        return Vec::new();
    }
    if replacement_dims.is_empty() {
        return if var_dims.is_empty() {
            indices.to_vec()
        } else {
            Vec::new()
        };
    }
    if replacement_dims.len() >= indices.len() {
        return indices.to_vec();
    }
    if var_dims.len() == indices.len() && replacement_dims.len() < var_dims.len() {
        let start = indices.len() - replacement_dims.len();
        return indices[start..].to_vec();
    }
    indices.to_vec()
}

pub(super) fn var_ref_matches_unknown_for_substitution(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    substitution: &Substitution,
) -> bool {
    if indexed_component_ref_mismatch_for_substitution(name, subscripts, substitution) {
        return false;
    }
    if name.var_name().id() == substitution.var_name.id() {
        return subscripts.is_empty() || subscripts_all_one(subscripts);
    }

    if subscripts.is_empty() && substitution_indexed_base_matches(name, substitution) {
        return false;
    }

    // Substitution must preserve complex field semantics: do not allow
    // base<->field alias matching here, otherwise `.re/.im` projections can be
    // applied to already-scalar replacement expressions.
    let name_field = reference_complex_field(name);
    let unknown_field = substitution_complex_field(substitution);
    if name_field.is_some() || unknown_field.is_some() {
        return name.var_name().id() == substitution.var_name.id()
            && (subscripts.is_empty() || subscripts_all_one(subscripts));
    }

    if subscripts.is_empty()
        && !reference_has_scalar_indices(name)
        && substitution_has_scalar_indices(substitution)
    {
        return false;
    }

    var_ref_matches_unknown(name, subscripts, &substitution.var_name)
}

pub(super) fn aggregate_subscript_ref_matches_var(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    substitution: &Substitution,
) -> bool {
    if indexed_component_ref_mismatch_for_substitution(name, subscripts, substitution) {
        return false;
    }
    !substitution.var_dims.is_empty()
        && !subscripts.is_empty()
        && name.var_name().id() == substitution.var_name.id()
}

fn indexed_component_ref_mismatch_for_substitution(
    name: &Reference,
    _subscripts: &[rumoca_core::Subscript],
    substitution: &Substitution,
) -> bool {
    if !reference_has_scalar_indices(name) || !substitution_has_scalar_indices(substitution) {
        return false;
    }
    substitution_exact_reference_name(substitution)
        .is_some_and(|substitution_name| name.as_str() != substitution_name)
}

fn substitution_exact_reference_name(substitution: &Substitution) -> Option<&str> {
    substitution
        .var_ref
        .as_ref()
        .map(Reference::as_str)
        .or(Some(substitution.var_name.as_str()))
}

struct SubstituteVarRewriter<'a> {
    substitution: &'a Substitution,
    replacement: &'a Expression,
    replacement_dims: &'a [i64],
    derivative_replacement: Option<&'a Expression>,
}

impl FallibleExpressionRewriter for SubstituteVarRewriter<'_> {
    type Error = StructuralError;

    fn rewrite_expression(&mut self, expr: &Expression) -> Result<Expression, Self::Error> {
        if exact_reference_expr_name(expr).as_ref() == Some(&self.substitution.var_name) {
            return Ok(expr.span().map_or_else(
                || self.replacement.clone(),
                |span| replacement_with_owner_span(self.replacement, span),
            ));
        }
        match expr {
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args,
                ..
            } if self.der_call_matches_scalar_substitution(args) => self
                .derivative_replacement
                .cloned()
                .map_or_else(|| self.walk_expression(expr), Ok),
            Expression::BuiltinCall {
                function: BuiltinFunction::Pre | BuiltinFunction::Edge | BuiltinFunction::Change,
                ..
            } => {
                // Preserve event-operator arguments to maintain MLS Appendix B
                // pre/change/edge semantics during symbolic substitution.
                Ok(expr.clone())
            }
            Expression::ArrayComprehension {
                expr: inner,
                indices,
                filter,
                span,
            } => Ok(Expression::ArrayComprehension {
                expr: Box::new(self.rewrite_expression(inner)?),
                indices: indices.clone(),
                filter: filter
                    .as_ref()
                    .map(|filter| self.rewrite_expression(filter).map(Box::new))
                    .transpose()?,
                span: *span,
            }),
            Expression::Index {
                base,
                subscripts,
                span,
            } => {
                if let Expression::VarRef {
                    name,
                    subscripts: base_subscripts,
                    ..
                } = base.as_ref()
                    && self.indexed_var_ref_matches_substitution(name, base_subscripts, subscripts)
                {
                    let mut combined_subscripts =
                        Vec::with_capacity(base_subscripts.len() + subscripts.len());
                    combined_subscripts.extend_from_slice(base_subscripts);
                    combined_subscripts.extend_from_slice(subscripts);
                    return self.rewrite_var_ref_expression(name, &combined_subscripts, *span);
                }
                self.walk_expression(expr)
            }
            _ => self.walk_expression(expr),
        }
    }

    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Result<Expression, Self::Error> {
        if let Some(indices) =
            embedded_alias_indices_for_substitution(name, subscripts, self.substitution)
        {
            // MLS §10.6: if a vector alias is eliminated before scalarization,
            // scalarized references to that alias must map to the same scalar
            // component of the replacement expression.
            let replacement_indices = replacement_indices_for_alias(
                &indices,
                &self.substitution.var_dims,
                self.replacement_dims,
            );
            index_replacement_expr(self.replacement, &replacement_indices, span)
        } else if aggregate_subscript_ref_matches_var(name, subscripts, self.substitution) {
            index_replacement_expr_with_subscripts(self.replacement, subscripts, span)
        } else if var_ref_matches_unknown_for_substitution(name, subscripts, self.substitution) {
            if !subscripts.is_empty() && !self.substitution.var_dims.is_empty() {
                return index_replacement_expr_with_subscripts(self.replacement, subscripts, span);
            }
            Ok(replacement_with_owner_span(self.replacement, span))
        } else {
            self.walk_var_ref_expression(name, subscripts, span)
        }
    }
}

impl SubstituteVarRewriter<'_> {
    fn indexed_var_ref_matches_substitution(
        &self,
        name: &Reference,
        base_subscripts: &[rumoca_core::Subscript],
        subscripts: &[rumoca_core::Subscript],
    ) -> bool {
        let mut combined_subscripts = Vec::with_capacity(base_subscripts.len() + subscripts.len());
        combined_subscripts.extend_from_slice(base_subscripts);
        combined_subscripts.extend_from_slice(subscripts);
        aggregate_subscript_ref_matches_var(name, &combined_subscripts, self.substitution)
            || var_ref_matches_unknown_for_substitution(
                name,
                &combined_subscripts,
                self.substitution,
            )
            || embedded_alias_indices_for_substitution(
                name,
                &combined_subscripts,
                self.substitution,
            )
            .is_some()
    }
}

fn replacement_with_owner_span(
    replacement: &Expression,
    owner_span: rumoca_core::Span,
) -> Expression {
    if replacement.span().is_some() || owner_span.is_dummy() {
        replacement.clone()
    } else {
        replacement.clone().with_span(owner_span)
    }
}

impl SubstituteVarRewriter<'_> {
    fn der_call_matches_scalar_substitution(&self, args: &[Expression]) -> bool {
        der_call_matches_scalar_substitution(args, self.substitution)
    }
}

pub(super) fn der_call_matches_scalar_substitution(
    args: &[Expression],
    substitution: &Substitution,
) -> bool {
    if !substitution.var_dims.is_empty() {
        return false;
    }
    let [
        Expression::VarRef {
            name, subscripts, ..
        },
    ] = args
    else {
        return false;
    };
    subscripts.is_empty()
        && var_ref_matches_unknown_for_substitution(name, subscripts, substitution)
}

#[cfg(test)]
mod tests;
