//! BLT-based initial condition solving plan.
//!
//! Decomposes the algebraic subsystem (equations `n_x..n_eq`) into small
//! sequential blocks using BLT, applies tearing to algebraic loops, and
//! produces an `IcBlock` plan that a runtime solver can execute.

use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use crate::eliminate::{expr_contains_var, try_solve_for_unknown};
use crate::incidence::{Incidence, ScalarUnknownResolver, collect_expression_unknowns};
use crate::tearing::tear_algebraic_loop;
use crate::types::{BltBlock, EquationRef, UnknownId};
use crate::{StructuralError, build_blt_from_incidence};

type Dae = dae::Dae;
type VarName = flat::VarName;

/// A block in the IC solving plan, produced by BLT + tearing at compile time.
#[derive(Debug, Clone)]
pub enum IcBlock {
    /// Symbolically solved: `var = eval(solution_expr)`.
    ScalarDirect {
        /// Index of this variable in the solver y-vector.
        var_idx: usize,
        /// Environment key for updating the variable.
        var_name: String,
        /// Symbolic solution expression.
        solution_expr: flat::Expression,
    },
    /// Single-variable Newton: solve `f_x[eq_idx]` for `y[var_idx]`.
    ScalarNewton {
        /// Index of the equation in `dae.f_x`.
        eq_idx: usize,
        /// Index of this variable in the solver y-vector.
        var_idx: usize,
        /// Environment key for updating the variable.
        var_name: String,
    },
    /// Torn algebraic loop: LM on tear variables + causal sequence.
    TornBlock {
        /// Indices in the solver y-vector for tear (iteration) variables.
        tear_var_indices: Vec<usize>,
        /// Environment keys for tear variables.
        tear_var_names: Vec<String>,
        /// Causal steps solved sequentially given current tear_var values.
        causal_sequence: Vec<CausalStep>,
        /// dae::Equation indices whose residuals drive LM (same count as tear_vars).
        residual_eq_indices: Vec<usize>,
    },
    /// Untearable coupled block: full small Newton/LM.
    CoupledLM {
        /// dae::Equation indices in `dae.f_x`.
        eq_indices: Vec<usize>,
        /// dae::Variable indices in the solver y-vector.
        var_indices: Vec<usize>,
        /// Environment keys for the variables.
        var_names: Vec<String>,
    },
}

/// Structural reduction hint for singular algebraic IC subsystems.
///
/// When the algebraic incidence is square but structurally singular, we can
/// select a balanced subset by dropping one or more redundant equations and the
/// same number of unmatched unknowns.
#[derive(Debug, Clone)]
pub struct IcRelaxationHint {
    /// Global equation indices in `dae.f_x` to drop.
    pub dropped_eq_global: Vec<usize>,
    /// Scalar unknown names to pin after dropping equations.
    pub dropped_unknown_names: Vec<String>,
}

/// One step in a torn block's causal sequence.
#[derive(Debug, Clone)]
pub struct CausalStep {
    /// Index in the solver y-vector.
    pub var_idx: usize,
    /// Environment key for updating the variable.
    pub var_name: String,
    /// `Some` = direct symbolic eval, `None` = scalar Newton.
    pub solution_expr: Option<flat::Expression>,
    /// dae::Equation used for Newton if `solution_expr` is `None`.
    pub eq_idx: usize,
}

/// Build the IC solving plan for the algebraic subsystem.
///
/// Takes a DAE (already reordered so ODE rows are first) and the number
/// of state variables `n_x`. Returns a sequence of `IcBlock`s that a
/// runtime solver should execute in order to find consistent initial
/// conditions for the algebraic variables.
///
/// Returns `Ok(vec![])` if there are no algebraic equations.
/// Returns `Err` if the algebraic subsystem is structurally singular.
pub fn build_ic_plan(dae: &Dae, n_x: usize) -> Result<Vec<IcBlock>, StructuralError> {
    let n_eq = dae.f_x.len();
    if n_eq <= n_x {
        return Ok(Vec::new());
    }

    // Build variable name → solver y-vector index mapping
    let (var_name_to_idx, _var_idx_to_name) = build_var_index_maps(dae);

    // Build incidence for algebraic equations only (indices n_x..n_eq),
    // with only algebraic+output variables as unknowns (states are known).
    let (incidence, alg_eq_offset, alg_var_indices, alg_var_names) =
        build_algebraic_incidence(dae, n_x, &var_name_to_idx);

    let (match_eq, match_var) =
        crate::matching::maximum_matching(incidence.n_eq, incidence.n_var, &incidence.eq_unknowns);
    let matching_size = match_eq.iter().filter(|m| m.is_some()).count();
    if matching_size < incidence.n_eq || matching_size < incidence.n_var {
        let unmatched_equation_indices: Vec<usize> = match_eq
            .iter()
            .enumerate()
            .filter_map(|(local_idx, matched)| matched.is_none().then_some(local_idx))
            .collect();
        let unmatched_unknown_indices: Vec<usize> = match_var
            .iter()
            .enumerate()
            .filter_map(|(local_idx, matched)| matched.is_none().then_some(local_idx))
            .collect();
        trace_ic_plan_singularity(
            dae,
            &incidence,
            alg_eq_offset,
            &alg_var_names,
            &unmatched_equation_indices,
            &unmatched_unknown_indices,
        );
        let relaxed_ctx = RelaxedIcPlanContext {
            dae,
            alg_eq_offset,
            var_name_to_idx: &var_name_to_idx,
            alg_var_indices: &alg_var_indices,
            alg_var_names: &alg_var_names,
        };
        if let Some(relaxed_ic_blocks) = try_build_relaxed_ic_plan_for_singular(
            &relaxed_ctx,
            &incidence,
            &unmatched_equation_indices,
            &unmatched_unknown_indices,
        ) {
            return Ok(relaxed_ic_blocks);
        }
        let unmatched_equations: Vec<String> = unmatched_equation_indices
            .iter()
            .map(|local_idx| {
                let global_eq_idx = alg_eq_offset + local_idx;
                dae.f_x
                    .get(global_eq_idx)
                    .map(|eq| eq.origin.clone())
                    .unwrap_or_else(|| format!("f_x[{global_eq_idx}]"))
            })
            .collect();
        let unmatched_unknowns: Vec<String> = unmatched_unknown_indices
            .iter()
            .map(|local_idx| {
                alg_var_names
                    .get(*local_idx)
                    .cloned()
                    .unwrap_or_else(|| format!("z[{local_idx}]"))
            })
            .collect();
        return Err(StructuralError::Singular {
            n_equations: incidence.n_eq,
            n_unknowns: incidence.n_var,
            n_matched: matching_size,
            unmatched_equations,
            unmatched_unknowns,
        });
    }

    // BLT decomposition
    let blt_blocks = build_blt_from_incidence(&incidence)?;
    Ok(convert_blt_blocks_to_ic(
        dae,
        &blt_blocks,
        alg_eq_offset,
        &var_name_to_idx,
        &alg_var_indices,
        &alg_var_names,
    ))
}

/// Build a structural relaxation hint for singular algebraic IC subsystems.
///
/// Returns `Some` only when the algebraic subsystem is square and singular in a
/// way that can be reduced by dropping a balanced subset of rows/unknowns.
pub fn build_ic_relaxation_hint(dae: &Dae, n_x: usize) -> Option<IcRelaxationHint> {
    let n_eq = dae.f_x.len();
    if n_eq <= n_x {
        return None;
    }

    let (var_name_to_idx, _) = build_var_index_maps(dae);
    let (incidence, alg_eq_offset, _alg_var_indices, alg_var_names) =
        build_algebraic_incidence(dae, n_x, &var_name_to_idx);
    let (match_eq, match_var) =
        crate::matching::maximum_matching(incidence.n_eq, incidence.n_var, &incidence.eq_unknowns);
    let matching_size = match_eq.iter().filter(|m| m.is_some()).count();
    if matching_size >= incidence.n_eq && matching_size >= incidence.n_var {
        return None;
    }

    let unmatched_equation_indices: Vec<usize> = match_eq
        .iter()
        .enumerate()
        .filter_map(|(local_idx, matched)| matched.is_none().then_some(local_idx))
        .collect();
    let unmatched_unknown_indices: Vec<usize> = match_var
        .iter()
        .enumerate()
        .filter_map(|(local_idx, matched)| matched.is_none().then_some(local_idx))
        .collect();
    let reduction = reduce_incidence_for_relaxed_ic(
        dae,
        &incidence,
        alg_eq_offset,
        &alg_var_names,
        &unmatched_equation_indices,
        &unmatched_unknown_indices,
    )?;

    let dropped_eq_global: Vec<usize> = reduction
        .dropped_eq_local
        .iter()
        .map(|local_idx| alg_eq_offset + *local_idx)
        .collect();
    let dropped_unknown_names: Vec<String> = reduction
        .dropped_unknown_local
        .iter()
        .map(|local_idx| {
            alg_var_names
                .get(*local_idx)
                .cloned()
                .unwrap_or_else(|| format!("z[{local_idx}]"))
        })
        .collect();
    Some(IcRelaxationHint {
        dropped_eq_global,
        dropped_unknown_names,
    })
}

fn ic_plan_trace_enabled() -> bool {
    std::env::var("RUMOCA_SIM_TRACE").is_ok() || std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
}

fn convert_blt_blocks_to_ic(
    dae: &Dae,
    blt_blocks: &[BltBlock],
    alg_eq_offset: usize,
    var_name_to_idx: &HashMap<String, usize>,
    alg_var_indices: &[usize],
    alg_var_names: &[String],
) -> Vec<IcBlock> {
    let mut ic_blocks = Vec::new();
    for block in blt_blocks {
        match block {
            BltBlock::Scalar { equation, unknown } => {
                let eq_idx = equation_ref_to_global(equation, alg_eq_offset);
                let (var_idx, var_name) =
                    resolve_unknown(unknown, var_name_to_idx, alg_var_indices, alg_var_names);

                let var_vn = VarName::new(&var_name);
                match try_solve_for_unknown(&dae.f_x[eq_idx].rhs, &var_vn) {
                    Some(solution) if !expr_contains_var(&solution, &var_vn) => {
                        ic_blocks.push(IcBlock::ScalarDirect {
                            var_idx,
                            var_name,
                            solution_expr: solution,
                        });
                    }
                    _ => {
                        ic_blocks.push(IcBlock::ScalarNewton {
                            eq_idx,
                            var_idx,
                            var_name,
                        });
                    }
                }
            }
            BltBlock::AlgebraicLoop {
                equations,
                unknowns,
            } => {
                let eq_indices: Vec<usize> = equations
                    .iter()
                    .map(|e| equation_ref_to_global(e, alg_eq_offset))
                    .collect();
                let var_info: Vec<(usize, String)> = unknowns
                    .iter()
                    .map(|u| resolve_unknown(u, var_name_to_idx, alg_var_indices, alg_var_names))
                    .collect();

                let block = build_loop_block(dae, &eq_indices, &var_info, var_name_to_idx);
                ic_blocks.push(block);
            }
        }
    }
    ic_blocks
}

struct RelaxedIcReduction {
    incidence: Incidence,
    dropped_eq_local: Vec<usize>,
    dropped_unknown_local: Vec<usize>,
}

struct RelaxedIcPlanContext<'a> {
    dae: &'a Dae,
    alg_eq_offset: usize,
    var_name_to_idx: &'a HashMap<String, usize>,
    alg_var_indices: &'a [usize],
    alg_var_names: &'a [String],
}

fn validate_relaxed_drop_inputs(
    incidence: &Incidence,
    unmatched_unknown_indices: &[usize],
) -> Option<(usize, HashSet<usize>)> {
    let target_drop_rows = unmatched_unknown_indices.len();
    let dropped_var: HashSet<usize> = unmatched_unknown_indices.iter().copied().collect();
    if target_drop_rows == 0
        || target_drop_rows > incidence.n_eq
        || target_drop_rows > incidence.n_var
        || dropped_var.len() != target_drop_rows
    {
        return None;
    }
    Some((target_drop_rows, dropped_var))
}

fn build_relaxed_candidate_rows(
    incidence: &Incidence,
    unmatched_equation_indices: &[usize],
    dropped_var: &HashSet<usize>,
) -> Vec<usize> {
    let mut candidate_rows = Vec::with_capacity(incidence.n_eq);
    let mut seen_rows = HashSet::with_capacity(incidence.n_eq);

    for &row_idx in unmatched_equation_indices {
        if row_idx < incidence.n_eq && seen_rows.insert(row_idx) {
            candidate_rows.push(row_idx);
        }
    }
    for (row_idx, cols) in incidence.eq_unknowns.iter().enumerate() {
        if cols.iter().any(|col| dropped_var.contains(col)) && seen_rows.insert(row_idx) {
            candidate_rows.push(row_idx);
        }
    }
    for row_idx in 0..incidence.n_eq {
        if seen_rows.insert(row_idx) {
            candidate_rows.push(row_idx);
        }
    }

    candidate_rows
}

#[derive(Debug, Clone, Copy)]
struct RelaxedDropCandidateScore {
    matched: usize,
    full_match: bool,
    touches_dropped_unknown: bool,
    single_drop_realign_full_match: bool,
    drop_priority: u8,
}

fn relaxed_drop_priority_for_origin(origin: &str) -> u8 {
    if origin == "orphaned_variable_pin" {
        return 5;
    }
    if origin.starts_with("flow sum equation:") {
        return 4;
    }
    if origin.starts_with("connection equation:") {
        return 3;
    }
    2
}

fn score_relaxed_drop_candidate(
    dae: &Dae,
    alg_eq_offset: usize,
    incidence: &Incidence,
    dropped_eq: &HashSet<usize>,
    dropped_var: &HashSet<usize>,
    row_idx: usize,
) -> Option<RelaxedDropCandidateScore> {
    if dropped_eq.contains(&row_idx) {
        return None;
    }
    let mut test_drop = dropped_eq.clone();
    test_drop.insert(row_idx);
    let reduced = project_relaxed_ic_incidence(incidence, &test_drop, dropped_var)?;
    let (match_eq, match_var) =
        crate::matching::maximum_matching(reduced.n_eq, reduced.n_var, &reduced.eq_unknowns);
    let mut matched = match_eq.iter().filter(|m| m.is_some()).count();
    let full_match =
        matched >= reduced.n_eq && matched >= reduced.n_var && matched >= match_var.len();
    let mut single_drop_realign_full_match = false;

    // For single-row relaxations, also score rows by whether they can become
    // fully matched after realigning the dropped unknown to one of the row's
    // own unknowns. This avoids selecting unrelated rows that only match under
    // a mismatched (row-disjoint) dropped-unknown set.
    if dropped_var.len() == 1 {
        let row_unknowns = incidence.eq_unknowns.get(row_idx)?;
        for &candidate_unknown in row_unknowns {
            let candidate_set = HashSet::from([candidate_unknown]);
            let Some(candidate_reduced) =
                project_relaxed_ic_incidence(incidence, &test_drop, &candidate_set)
            else {
                continue;
            };
            let (cand_eq, cand_var) = crate::matching::maximum_matching(
                candidate_reduced.n_eq,
                candidate_reduced.n_var,
                &candidate_reduced.eq_unknowns,
            );
            let cand_matched = cand_eq.iter().filter(|m| m.is_some()).count();
            let cand_full = cand_matched >= candidate_reduced.n_eq
                && cand_matched >= candidate_reduced.n_var
                && cand_matched >= cand_var.len();
            if cand_matched > matched {
                matched = cand_matched;
            }
            if cand_full {
                single_drop_realign_full_match = true;
            }
        }
    }
    let touches_dropped_unknown = incidence
        .eq_unknowns
        .get(row_idx)
        .is_some_and(|cols| cols.iter().any(|col| dropped_var.contains(col)));
    let global_idx = alg_eq_offset + row_idx;
    let origin = dae
        .f_x
        .get(global_idx)
        .map(|eq| eq.origin.as_str())
        .unwrap_or("");
    Some(RelaxedDropCandidateScore {
        matched,
        full_match,
        touches_dropped_unknown,
        single_drop_realign_full_match,
        drop_priority: relaxed_drop_priority_for_origin(origin),
    })
}

#[derive(Debug, Clone, Copy)]
struct RelaxedDropSelectionContext {
    single_drop: bool,
    has_full_touch: bool,
    has_preferred_full: bool,
    has_full: bool,
    has_touch: bool,
    has_touch_realign: bool,
}

fn is_preferred_full_candidate(score: &RelaxedDropCandidateScore, single_drop: bool) -> bool {
    score.full_match
        && (!single_drop || score.touches_dropped_unknown || score.single_drop_realign_full_match)
}

fn build_relaxed_drop_selection_context(
    candidates: &[(usize, RelaxedDropCandidateScore)],
    single_drop: bool,
) -> RelaxedDropSelectionContext {
    let has_full_touch = candidates
        .iter()
        .any(|(_, score)| score.full_match && score.touches_dropped_unknown);
    let has_preferred_full = candidates
        .iter()
        .any(|(_, score)| is_preferred_full_candidate(score, single_drop));
    let has_full = candidates.iter().any(|(_, score)| score.full_match);
    let has_touch = candidates
        .iter()
        .any(|(_, score)| score.touches_dropped_unknown);
    let has_touch_realign = single_drop
        && candidates.iter().any(|(_, score)| {
            score.touches_dropped_unknown && score.single_drop_realign_full_match
        });
    RelaxedDropSelectionContext {
        single_drop,
        has_full_touch,
        has_preferred_full,
        has_full,
        has_touch,
        has_touch_realign,
    }
}

fn selection_tier_for_relaxed_drop(
    score: &RelaxedDropCandidateScore,
    ctx: RelaxedDropSelectionContext,
) -> u8 {
    if ctx.has_full_touch {
        if score.full_match && score.touches_dropped_unknown {
            return 3;
        }
        return 0;
    }

    if ctx.has_preferred_full {
        if is_preferred_full_candidate(score, ctx.single_drop) {
            return if score.touches_dropped_unknown { 4 } else { 3 };
        }
        return if score.touches_dropped_unknown { 2 } else { 1 };
    }

    if ctx.has_full {
        if ctx.has_touch_realign {
            if score.touches_dropped_unknown && score.single_drop_realign_full_match {
                return 3;
            }
            if score.full_match {
                return 2;
            }
            return if score.touches_dropped_unknown { 1 } else { 0 };
        }
        if score.full_match {
            return 3;
        }
        return if score.touches_dropped_unknown { 2 } else { 1 };
    }

    if ctx.has_touch || score.touches_dropped_unknown {
        return if score.touches_dropped_unknown { 2 } else { 1 };
    }
    1
}

fn should_replace_best_relaxed_drop(
    best_score: RelaxedDropCandidateScore,
    best_tier: u8,
    candidate_score: RelaxedDropCandidateScore,
    candidate_tier: u8,
) -> bool {
    (candidate_tier > best_tier)
        || (candidate_tier == best_tier
            && (candidate_score.matched > best_score.matched
                || (candidate_score.matched == best_score.matched
                    && candidate_score.drop_priority > best_score.drop_priority)))
}

fn choose_best_relaxed_drop_candidate(
    candidates: &[(usize, RelaxedDropCandidateScore)],
    ctx: RelaxedDropSelectionContext,
) -> Option<(usize, RelaxedDropCandidateScore, u8)> {
    let mut best: Option<(usize, RelaxedDropCandidateScore, u8)> = None;
    for &(row_idx, score) in candidates {
        let selection_tier = selection_tier_for_relaxed_drop(&score, ctx);
        if selection_tier == 0 {
            continue;
        }
        match best {
            None => best = Some((row_idx, score, selection_tier)),
            Some((_, best_score, best_tier))
                if should_replace_best_relaxed_drop(
                    best_score,
                    best_tier,
                    score,
                    selection_tier,
                ) =>
            {
                best = Some((row_idx, score, selection_tier));
            }
            Some(_) => {}
        }
    }
    best
}

fn select_relaxed_drop_rows(
    dae: &Dae,
    alg_eq_offset: usize,
    incidence: &Incidence,
    candidate_rows: &[usize],
    dropped_var: &HashSet<usize>,
    target_drop_rows: usize,
) -> Option<HashSet<usize>> {
    let mut dropped_eq = HashSet::with_capacity(target_drop_rows);
    while dropped_eq.len() < target_drop_rows {
        let mut candidates: Vec<(usize, RelaxedDropCandidateScore)> = Vec::new();
        for &row_idx in candidate_rows {
            let Some(score) = score_relaxed_drop_candidate(
                dae,
                alg_eq_offset,
                incidence,
                &dropped_eq,
                dropped_var,
                row_idx,
            ) else {
                continue;
            };
            candidates.push((row_idx, score));
        }
        if candidates.is_empty() {
            return None;
        }

        let single_drop = dropped_var.len() == 1;
        let ctx = build_relaxed_drop_selection_context(&candidates, single_drop);
        let (row_idx, best_score, selected_tier) =
            choose_best_relaxed_drop_candidate(&candidates, ctx)?;
        if ic_plan_trace_enabled() {
            let global_idx = alg_eq_offset + row_idx;
            let origin = dae
                .f_x
                .get(global_idx)
                .map(|eq| eq.origin.as_str())
                .unwrap_or("<missing-eq>");
            eprintln!(
                "[sim-trace] IC relaxed drop select: row_local={} row_global=f_x[{}] tier={} matched={} full={} touches_dropped_unknown={} priority={} origin='{}'",
                row_idx,
                global_idx,
                selected_tier,
                best_score.matched,
                best_score.full_match,
                best_score.touches_dropped_unknown,
                best_score.drop_priority,
                origin
            );
            if single_drop && best_score.full_match && !best_score.touches_dropped_unknown {
                eprintln!(
                    "[sim-trace] IC relaxed drop note: selected disjoint full-match row; realign_full_candidate={}",
                    best_score.single_drop_realign_full_match
                );
            }
        }
        dropped_eq.insert(row_idx);
    }
    Some(dropped_eq)
}

fn summarize_relaxed_drops(
    dropped_eq: &HashSet<usize>,
    dropped_unknowns: &HashSet<usize>,
) -> (Vec<usize>, Vec<usize>) {
    let mut dropped_eq_local: Vec<usize> = dropped_eq.iter().copied().collect();
    dropped_eq_local.sort_unstable();
    let mut dropped_unknown_local: Vec<usize> = dropped_unknowns.iter().copied().collect();
    dropped_unknown_local.sort_unstable();
    (dropped_eq_local, dropped_unknown_local)
}

fn collect_unknown_ref_counts(incidence: &Incidence) -> Vec<usize> {
    let mut counts = vec![0usize; incidence.n_var];
    for cols in &incidence.eq_unknowns {
        for &col in cols {
            if let Some(slot) = counts.get_mut(col) {
                *slot += 1;
            }
        }
    }
    counts
}

fn maybe_realign_single_relaxed_drop_unknown(
    incidence: &Incidence,
    dropped_eq: &HashSet<usize>,
    dropped_var: &HashSet<usize>,
    preferred_unknowns: &HashSet<usize>,
) -> HashSet<usize> {
    if dropped_eq.len() != 1 || dropped_var.len() != 1 {
        return dropped_var.clone();
    }
    let row_idx = dropped_eq.iter().copied().next().unwrap_or(0);
    let current_unknown = dropped_var.iter().copied().next().unwrap_or(0);
    let row_unknowns = match incidence.eq_unknowns.get(row_idx) {
        Some(cols) => cols,
        None => return dropped_var.clone(),
    };
    if row_unknowns.contains(&current_unknown) {
        return dropped_var.clone();
    }

    let ref_counts = collect_unknown_ref_counts(incidence);
    let mut best: Option<(usize, bool, usize)> = None;
    for &candidate in row_unknowns {
        let candidate_set = HashSet::from([candidate]);
        let Some(reduced) = project_relaxed_ic_incidence(incidence, dropped_eq, &candidate_set)
        else {
            if ic_plan_trace_enabled() {
                eprintln!(
                    "[sim-trace] IC relaxed drop unknown candidate={} reduced=none dropped_eq_local={:?}",
                    candidate, dropped_eq
                );
            }
            continue;
        };
        let full_match = reduction_is_fully_matched(&reduced);
        if !full_match {
            if ic_plan_trace_enabled() {
                eprintln!(
                    "[sim-trace] IC relaxed drop unknown candidate={} full_match=false dropped_eq_local={:?}",
                    candidate, dropped_eq
                );
            }
            continue;
        }

        let preferred = preferred_unknowns.contains(&candidate);
        let refs = ref_counts.get(candidate).copied().unwrap_or(usize::MAX);
        if ic_plan_trace_enabled() {
            eprintln!(
                "[sim-trace] IC relaxed drop unknown candidate={} full_match=true preferred={} refs={}",
                candidate, preferred, refs
            );
        }
        let replace = match best {
            None => true,
            Some((best_candidate, best_preferred, best_refs)) => {
                (preferred && !best_preferred)
                    || (preferred == best_preferred
                        && (refs < best_refs || (refs == best_refs && candidate < best_candidate)))
            }
        };
        if replace {
            best = Some((candidate, preferred, refs));
        }
    }

    let Some((candidate, _, _)) = best else {
        return dropped_var.clone();
    };

    if ic_plan_trace_enabled() {
        eprintln!(
            "[sim-trace] IC relaxed drop unknown realign: {} -> {} for dropped_eq_local={:?}",
            current_unknown, candidate, dropped_eq
        );
    }
    HashSet::from([candidate])
}

fn trace_relaxed_drop_selection(
    dae: &Dae,
    incidence: &Incidence,
    alg_eq_offset: usize,
    alg_var_names: &[String],
    dropped_eq_local: &[usize],
    dropped_unknown_local: &[usize],
) {
    if !ic_plan_trace_enabled() {
        return;
    }

    eprintln!(
        "[sim-trace] IC relaxed drop detail: dropped_eq_local={:?} dropped_unknown_local={:?}",
        dropped_eq_local, dropped_unknown_local
    );
    for local_idx in dropped_eq_local {
        let global_idx = alg_eq_offset + *local_idx;
        let origin = dae
            .f_x
            .get(global_idx)
            .map(|eq| eq.origin.as_str())
            .unwrap_or("<missing-eq>");
        let mut row_unknowns: Vec<String> = incidence
            .eq_unknowns
            .get(*local_idx)
            .into_iter()
            .flat_map(|cols| cols.iter().copied())
            .map(|local_col| {
                alg_var_names
                    .get(local_col)
                    .cloned()
                    .unwrap_or_else(|| format!("z[{local_col}]"))
            })
            .collect();
        row_unknowns.sort();
        if row_unknowns.len() > 6 {
            row_unknowns.truncate(6);
            row_unknowns.push("...".to_string());
        }
        eprintln!(
            "[sim-trace]   dropped_eq f_x[{global_idx}] origin='{}' unknowns={}",
            origin,
            row_unknowns.join(", ")
        );
    }
    for local_idx in dropped_unknown_local {
        let name = alg_var_names
            .get(*local_idx)
            .cloned()
            .unwrap_or_else(|| format!("z[{local_idx}]"));
        eprintln!("[sim-trace]   dropped_unknown '{}'", name);
    }
}

fn reduction_is_fully_matched(reduced: &Incidence) -> bool {
    let (match_eq, match_var) =
        crate::matching::maximum_matching(reduced.n_eq, reduced.n_var, &reduced.eq_unknowns);
    let matched = match_eq.iter().filter(|m| m.is_some()).count();
    matched >= reduced.n_eq && matched >= reduced.n_var && matched >= match_var.len()
}

fn reduce_incidence_for_relaxed_ic(
    dae: &Dae,
    incidence: &Incidence,
    alg_eq_offset: usize,
    alg_var_names: &[String],
    unmatched_equation_indices: &[usize],
    unmatched_unknown_indices: &[usize],
) -> Option<RelaxedIcReduction> {
    let (target_drop_rows, dropped_var_initial) =
        validate_relaxed_drop_inputs(incidence, unmatched_unknown_indices)?;
    let preferred_unknowns: HashSet<usize> = unmatched_unknown_indices.iter().copied().collect();
    let candidate_rows =
        build_relaxed_candidate_rows(incidence, unmatched_equation_indices, &dropped_var_initial);
    let dropped_eq = select_relaxed_drop_rows(
        dae,
        alg_eq_offset,
        incidence,
        &candidate_rows,
        &dropped_var_initial,
        target_drop_rows,
    )?;
    let dropped_var = maybe_realign_single_relaxed_drop_unknown(
        incidence,
        &dropped_eq,
        &dropped_var_initial,
        &preferred_unknowns,
    );
    let (dropped_eq_local, dropped_unknown_local) =
        summarize_relaxed_drops(&dropped_eq, &dropped_var);
    trace_relaxed_drop_selection(
        dae,
        incidence,
        alg_eq_offset,
        alg_var_names,
        &dropped_eq_local,
        &dropped_unknown_local,
    );
    let reduced = project_relaxed_ic_incidence(incidence, &dropped_eq, &dropped_var)?;
    if !reduction_is_fully_matched(&reduced) {
        return None;
    }
    Some(RelaxedIcReduction {
        incidence: reduced,
        dropped_eq_local,
        dropped_unknown_local,
    })
}

fn project_relaxed_ic_incidence(
    incidence: &Incidence,
    dropped_eq: &HashSet<usize>,
    dropped_var: &HashSet<usize>,
) -> Option<Incidence> {
    let kept_vars: Vec<usize> = (0..incidence.n_var)
        .filter(|idx| !dropped_var.contains(idx))
        .collect();
    let kept_eqs: Vec<usize> = (0..incidence.n_eq)
        .filter(|idx| !dropped_eq.contains(idx))
        .collect();
    if kept_vars.is_empty() || kept_eqs.is_empty() || kept_vars.len() != kept_eqs.len() {
        return None;
    }

    let mut old_to_new_var = vec![None; incidence.n_var];
    for (new_idx, old_idx) in kept_vars.iter().copied().enumerate() {
        old_to_new_var[old_idx] = Some(new_idx);
    }
    let unknown_names: Vec<UnknownId> = kept_vars
        .iter()
        .map(|old_idx| incidence.unknown_names[*old_idx].clone())
        .collect();
    let equation_refs: Vec<EquationRef> = kept_eqs
        .iter()
        .map(|old_idx| incidence.equation_refs[*old_idx].clone())
        .collect();

    let mut eq_unknowns = Vec::with_capacity(kept_eqs.len());
    for old_eq_idx in kept_eqs {
        let mut cols = HashSet::new();
        for old_col in incidence.eq_unknowns[old_eq_idx].iter().copied() {
            if let Some(new_col) = old_to_new_var[old_col] {
                cols.insert(new_col);
            }
        }
        eq_unknowns.push(cols);
    }

    Some(Incidence::new(eq_unknowns, equation_refs, unknown_names))
}

fn try_build_relaxed_ic_plan_for_singular(
    ctx: &RelaxedIcPlanContext<'_>,
    incidence: &Incidence,
    unmatched_equation_indices: &[usize],
    unmatched_unknown_indices: &[usize],
) -> Option<Vec<IcBlock>> {
    // Relax only square systems with balanced structural deficiency.
    if incidence.n_eq != incidence.n_var
        || unmatched_equation_indices.is_empty()
        || unmatched_equation_indices.len() != unmatched_unknown_indices.len()
        || unmatched_equation_indices.len() > 32
    {
        return None;
    }

    let reduction = reduce_incidence_for_relaxed_ic(
        ctx.dae,
        incidence,
        ctx.alg_eq_offset,
        ctx.alg_var_names,
        unmatched_equation_indices,
        unmatched_unknown_indices,
    )?;
    let blt_blocks = build_blt_from_incidence(&reduction.incidence).ok()?;
    let ic_blocks = convert_blt_blocks_to_ic(
        ctx.dae,
        &blt_blocks,
        ctx.alg_eq_offset,
        ctx.var_name_to_idx,
        ctx.alg_var_indices,
        ctx.alg_var_names,
    );
    if ic_plan_trace_enabled() {
        eprintln!(
            "[sim-trace] IC plan relaxed fallback applied: dropped_eq={} dropped_unknowns={} kept_eq={} kept_unknowns={} blocks={}",
            reduction.dropped_eq_local.len(),
            reduction.dropped_unknown_local.len(),
            reduction.incidence.n_eq,
            reduction.incidence.n_var,
            ic_blocks.len()
        );
    }
    Some(ic_blocks)
}

fn trace_ic_plan_singularity(
    dae: &Dae,
    incidence: &Incidence,
    alg_eq_offset: usize,
    alg_var_names: &[String],
    unmatched_equation_indices: &[usize],
    unmatched_unknown_indices: &[usize],
) {
    if !ic_plan_trace_enabled() {
        return;
    }

    eprintln!(
        "[sim-trace] IC plan singular detail: unmatched_eq={} unmatched_unknowns={}",
        unmatched_equation_indices.len(),
        unmatched_unknown_indices.len()
    );

    for local_var_idx in unmatched_unknown_indices.iter().copied().take(8) {
        let name = alg_var_names
            .get(local_var_idx)
            .cloned()
            .unwrap_or_else(|| format!("z[{local_var_idx}]"));
        let mut referencing_rows: Vec<usize> = incidence
            .eq_unknowns
            .iter()
            .enumerate()
            .filter_map(|(row, cols)| cols.contains(&local_var_idx).then_some(row))
            .collect();
        referencing_rows.sort_unstable();

        if referencing_rows.is_empty() {
            eprintln!(
                "[sim-trace]   unmatched_unknown '{}' has no referencing algebraic rows",
                name
            );
            continue;
        }

        let mut samples = Vec::new();
        for local_row in referencing_rows.iter().copied().take(4) {
            let global_row = alg_eq_offset + local_row;
            let origin = dae
                .f_x
                .get(global_row)
                .map(|eq| eq.origin.as_str())
                .unwrap_or("<missing-eq>");
            let is_pin = origin == "orphaned_variable_pin";
            let mut row_unknowns: Vec<String> = incidence
                .eq_unknowns
                .get(local_row)
                .into_iter()
                .flat_map(|cols| cols.iter().copied())
                .map(|local_col| {
                    alg_var_names
                        .get(local_col)
                        .cloned()
                        .unwrap_or_else(|| format!("z[{local_col}]"))
                })
                .collect();
            row_unknowns.sort();
            if row_unknowns.len() > 4 {
                row_unknowns.truncate(4);
                row_unknowns.push("...".to_string());
            }
            samples.push(format!(
                "f_x[{global_row}] origin='{origin}' pin={is_pin} row_unknowns={}",
                row_unknowns.join(", ")
            ));
        }
        eprintln!(
            "[sim-trace]   unmatched_unknown '{}' referenced_by={} sample={}",
            name,
            referencing_rows.len(),
            samples.join(" | ")
        );
    }

    for local_eq_idx in unmatched_equation_indices.iter().copied().take(8) {
        let global_eq_idx = alg_eq_offset + local_eq_idx;
        let origin = dae
            .f_x
            .get(global_eq_idx)
            .map(|eq| eq.origin.as_str())
            .unwrap_or("<missing-eq>");

        let mut unknown_names: Vec<String> = incidence
            .eq_unknowns
            .get(local_eq_idx)
            .into_iter()
            .flat_map(|cols| cols.iter().copied())
            .map(|local_var_idx| {
                alg_var_names
                    .get(local_var_idx)
                    .cloned()
                    .unwrap_or_else(|| format!("z[{local_var_idx}]"))
            })
            .collect();
        unknown_names.sort();
        if unknown_names.len() > 6 {
            unknown_names.truncate(6);
            unknown_names.push("...".to_string());
        }
        eprintln!(
            "[sim-trace]   unmatched_eq f_x[{global_eq_idx}] origin='{}' unknowns={}",
            origin,
            unknown_names.join(", ")
        );
    }
}

/// Map variable names to solver y-vector indices and back.
fn build_var_index_maps(dae: &Dae) -> (std::collections::HashMap<String, usize>, Vec<String>) {
    let mut name_to_idx = std::collections::HashMap::new();
    let mut idx_to_name = Vec::new();
    let mut idx = 0;
    for (name, var) in dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
    {
        let sz = var.size();
        if sz <= 1 {
            name_to_idx.insert(name.as_str().to_string(), idx);
            idx_to_name.push(name.as_str().to_string());
            idx += 1;
        } else {
            for i in 0..sz {
                let key = format!("{}[{}]", name.as_str(), i + 1);
                name_to_idx.insert(key.clone(), idx);
                idx_to_name.push(key);
                idx += 1;
            }
        }
    }
    (name_to_idx, idx_to_name)
}

/// Build incidence matrix for the algebraic subsystem only.
///
/// Returns (incidence, alg_eq_offset, alg_var_indices, alg_var_names) where:
/// - `alg_eq_offset` maps local eq index → global `dae.f_x` index
/// - `alg_var_indices[local]` is the global y-vector index
/// - `alg_var_names[local]` is the variable name string
fn build_algebraic_incidence(
    dae: &Dae,
    n_x: usize,
    var_name_to_idx: &std::collections::HashMap<String, usize>,
) -> (Incidence, usize, Vec<usize>, Vec<String>) {
    // Algebraic + output variable names (in order), with y-vector indices
    let mut alg_var_names: Vec<String> = Vec::new();
    let mut alg_var_indices: Vec<usize> = Vec::new();
    let mut unknown_names: Vec<UnknownId> = Vec::new();
    for (name, var) in dae.algebraics.iter().chain(dae.outputs.iter()) {
        let sz = var.size();
        let keys: Vec<(String, UnknownId)> = if sz <= 1 {
            vec![(name.as_str().to_string(), UnknownId::Variable(name.clone()))]
        } else {
            (0..sz)
                .map(|i| {
                    let key = format!("{}[{}]", name.as_str(), i + 1);
                    let uid = UnknownId::Variable(VarName::new(&key));
                    (key, uid)
                })
                .collect()
        };
        for (key, uid) in keys {
            let global_idx = var_name_to_idx.get(&key).copied().unwrap_or(0);
            alg_var_indices.push(global_idx);
            alg_var_names.push(key);
            unknown_names.push(uid);
        }
    }
    let local_resolver = ScalarUnknownResolver::from_entries(
        alg_var_names
            .iter()
            .enumerate()
            .map(|(local_idx, name)| (name.clone(), local_idx)),
    );

    // Build incidence for algebraic equations (n_x..n_eq)
    let alg_eq_offset = n_x;
    let mut equation_refs = Vec::new();
    let mut eq_unknowns_list: Vec<HashSet<usize>> = Vec::new();

    // Collect algebraic/output unknowns referenced by each algebraic equation.
    for (local_eq, eq) in dae.f_x[n_x..].iter().enumerate() {
        equation_refs.push(EquationRef::Continuous(n_x + local_eq));

        let mut unknown_indices = HashSet::new();
        collect_expression_unknowns(&eq.rhs, &local_resolver, &mut unknown_indices);
        eq_unknowns_list.push(unknown_indices);
    }

    let incidence = Incidence::new(eq_unknowns_list, equation_refs, unknown_names);

    (incidence, alg_eq_offset, alg_var_indices, alg_var_names)
}

/// Convert an `EquationRef` back to its global `dae.f_x` index.
fn equation_ref_to_global(eq_ref: &EquationRef, _alg_eq_offset: usize) -> usize {
    match eq_ref {
        EquationRef::Continuous(i) => *i,
    }
}

/// Resolve an `UnknownId` to (y-vector index, env name).
fn resolve_unknown(
    unknown: &UnknownId,
    var_name_to_idx: &std::collections::HashMap<String, usize>,
    alg_var_indices: &[usize],
    alg_var_names: &[String],
) -> (usize, String) {
    let name = match unknown {
        UnknownId::Variable(vn) => vn.as_str().to_string(),
        UnknownId::DerState(vn) => format!("der({})", vn.as_str()),
    };

    // Try direct lookup first
    if let Some(&idx) = var_name_to_idx.get(&name) {
        return (idx, name);
    }

    // Fall back to searching alg_var_names
    for (i, n) in alg_var_names.iter().enumerate() {
        if *n == name {
            return (alg_var_indices[i], name);
        }
    }

    // Last resort
    (0, name)
}

/// Build an IcBlock for an algebraic loop: attempt tearing, fall back to CoupledLM.
fn build_loop_block(
    dae: &Dae,
    eq_indices: &[usize],
    var_info: &[(usize, String)],
    var_name_to_idx: &std::collections::HashMap<String, usize>,
) -> IcBlock {
    let n = eq_indices.len();

    // Build local incidence for tearing (only among the loop's own unknowns)
    let local_var_names: Vec<&str> = var_info.iter().map(|(_, name)| name.as_str()).collect();
    let local_resolver = ScalarUnknownResolver::from_entries(
        local_var_names
            .iter()
            .enumerate()
            .map(|(local_idx, name)| (name.to_string(), local_idx)),
    );

    let local_eq_unknowns: Vec<HashSet<usize>> = eq_indices
        .iter()
        .map(|&eq_idx| {
            let mut local_unknowns = HashSet::new();
            collect_expression_unknowns(&dae.f_x[eq_idx].rhs, &local_resolver, &mut local_unknowns);
            local_unknowns
        })
        .collect();

    // Attempt tearing
    if let Some(tearing) = tear_algebraic_loop(n, &local_eq_unknowns) {
        let tear_var_indices: Vec<usize> = tearing
            .tear_var_local_indices
            .iter()
            .map(|&li| var_info[li].0)
            .collect();
        let tear_var_names: Vec<String> = tearing
            .tear_var_local_indices
            .iter()
            .map(|&li| var_info[li].1.clone())
            .collect();
        let mut residual_eq_indices: Vec<usize> = tearing
            .residual_eq_local_indices
            .iter()
            .map(|&li| eq_indices[li])
            .collect();

        let mut causal_sequence: Vec<CausalStep> = tearing
            .causal_sequence
            .iter()
            .map(|&(local_eq, local_var)| {
                let eq_idx = eq_indices[local_eq];
                let (var_idx, var_name) = (var_info[local_var].0, var_info[local_var].1.clone());

                // Try symbolic solve for the causal step
                let var_vn = VarName::new(&var_name);
                let solution = try_solve_for_unknown(&dae.f_x[eq_idx].rhs, &var_vn)
                    .filter(|s| !expr_contains_var(s, &var_vn));

                CausalStep {
                    var_idx,
                    var_name,
                    solution_expr: solution,
                    eq_idx,
                }
            })
            .collect();

        // Post-processing: for causal steps that can't be solved symbolically,
        // check if swapping with a residual equation would help.
        // This handles cases where the tearing assigned a bilinear equation
        // (e.g. v = R*i) to a variable that could be solved from a simpler
        // equation (e.g. R = R0*(1+alpha*(T-Tref))).
        improve_causal_assignment(dae, &mut causal_sequence, &mut residual_eq_indices);

        return IcBlock::TornBlock {
            tear_var_indices,
            tear_var_names,
            causal_sequence,
            residual_eq_indices,
        };
    }

    // Fall back to coupled LM
    let _ = var_name_to_idx; // suppress unused warning
    IcBlock::CoupledLM {
        eq_indices: eq_indices.to_vec(),
        var_indices: var_info.iter().map(|(idx, _)| *idx).collect(),
        var_names: var_info.iter().map(|(_, name)| name.clone()).collect(),
    }
}

/// Post-process the causal assignment: swap equations between causal steps and
/// residuals when a residual equation can solve symbolically for a causal variable.
///
/// This handles cases where tearing (which only uses structural incidence) assigns
/// a bilinear equation like `v = R*i` to solve for R, when a simpler equation like
/// `R = R0*(1+alpha*(T-Tref))` is available but was placed as a residual.
fn improve_causal_assignment(
    dae: &Dae,
    causal_sequence: &mut [CausalStep],
    residual_eq_indices: &mut [usize],
) {
    for step in causal_sequence.iter_mut() {
        if step.solution_expr.is_some() {
            continue; // Already symbolically solvable, no need to improve
        }

        // This causal step can't be solved symbolically from its current equation.
        // Check if any residual equation can solve for this variable.
        let var_vn = VarName::new(&step.var_name);
        let mut best_swap = None;

        for (res_idx, &res_eq_idx) in residual_eq_indices.iter().enumerate() {
            // Check if the residual equation references this variable
            if !expr_contains_var(&dae.f_x[res_eq_idx].rhs, &var_vn) {
                continue;
            }
            // Check if we can solve the residual equation for this variable
            if let Some(solution) = try_solve_for_unknown(&dae.f_x[res_eq_idx].rhs, &var_vn)
                && !expr_contains_var(&solution, &var_vn)
            {
                best_swap = Some((res_idx, res_eq_idx, solution));
                break;
            }
        }

        if let Some((res_idx, res_eq_idx, solution)) = best_swap {
            // Swap: the residual equation becomes causal, the old causal becomes residual
            let old_eq_idx = step.eq_idx;
            step.eq_idx = res_eq_idx;
            step.solution_expr = Some(solution);
            residual_eq_indices[res_idx] = old_eq_idx;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;
    use rumoca_ir_dae as dae;

    fn var_ref(name: &str) -> flat::Expression {
        flat::Expression::VarRef {
            name: VarName::new(name),
            subscripts: vec![],
        }
    }

    fn lit(v: f64) -> flat::Expression {
        flat::Expression::Literal(flat::Literal::Real(v))
    }

    fn sub(l: flat::Expression, r: flat::Expression) -> flat::Expression {
        flat::Expression::Binary {
            op: flat::OpBinary::Sub(Default::default()),
            lhs: Box::new(l),
            rhs: Box::new(r),
        }
    }

    fn mul(l: flat::Expression, r: flat::Expression) -> flat::Expression {
        flat::Expression::Binary {
            op: flat::OpBinary::Mul(Default::default()),
            lhs: Box::new(l),
            rhs: Box::new(r),
        }
    }

    fn index(base: &str, idx: i64) -> flat::Expression {
        flat::Expression::Index {
            base: Box::new(var_ref(base)),
            subscripts: vec![flat::Subscript::Index(idx)],
        }
    }

    fn eq_from(rhs: flat::Expression) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs,
            span: Span::DUMMY,
            origin: String::new(),
            scalar_count: 1,
        }
    }

    #[test]
    fn test_build_ic_plan_no_algebraics() {
        let dae = Dae::new();
        let plan = build_ic_plan(&dae, 0).unwrap();
        assert!(plan.is_empty());
    }

    #[test]
    fn test_build_ic_plan_scalar_chain() {
        // R_actual = R * factor  (scalar direct)
        // v = R_actual * i       (scalar — depends on R_actual)
        // Two algebraic variables, two algebraic equations, zero states.
        let mut dae = Dae::new();

        dae.algebraics.insert(
            VarName::new("R_actual"),
            dae::Variable::new(VarName::new("R_actual")),
        );
        dae.algebraics
            .insert(VarName::new("v"), dae::Variable::new(VarName::new("v")));
        dae.parameters
            .insert(VarName::new("R"), dae::Variable::new(VarName::new("R")));
        dae.parameters.insert(
            VarName::new("factor"),
            dae::Variable::new(VarName::new("factor")),
        );

        // 0 = R_actual - R * factor
        dae.f_x.push(eq_from(sub(
            var_ref("R_actual"),
            mul(var_ref("R"), var_ref("factor")),
        )));
        // 0 = v - R_actual * i  (but i is a parameter for simplicity)
        dae.f_x.push(eq_from(sub(
            var_ref("v"),
            mul(var_ref("R_actual"), var_ref("i")),
        )));

        let plan = build_ic_plan(&dae, 0).unwrap();
        assert_eq!(plan.len(), 2);
        // First block should solve R_actual directly
        assert!(
            matches!(&plan[0], IcBlock::ScalarDirect { var_name, .. } if var_name == "R_actual")
        );
        // Second block should solve v directly (R_actual is now known)
        assert!(matches!(&plan[1], IcBlock::ScalarDirect { var_name, .. } if var_name == "v"));
    }

    #[test]
    fn test_build_ic_plan_algebraic_loop() {
        // Two coupled equations: 0 = y - 2*z, 0 = z - 3*y
        let mut dae = Dae::new();

        dae.algebraics
            .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
        dae.algebraics
            .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));

        // 0 = y - 2*z
        dae.f_x
            .push(eq_from(sub(var_ref("y"), mul(lit(2.0), var_ref("z")))));
        // 0 = z - 3*y
        dae.f_x
            .push(eq_from(sub(var_ref("z"), mul(lit(3.0), var_ref("y")))));

        let plan = build_ic_plan(&dae, 0).unwrap();
        assert_eq!(plan.len(), 1);
        // Should be a TornBlock or CoupledLM (2x2 loop)
        match &plan[0] {
            IcBlock::TornBlock {
                tear_var_indices,
                causal_sequence,
                residual_eq_indices,
                ..
            } => {
                assert_eq!(tear_var_indices.len(), 1);
                assert_eq!(causal_sequence.len(), 1);
                assert_eq!(residual_eq_indices.len(), 1);
            }
            IcBlock::CoupledLM {
                eq_indices,
                var_indices,
                ..
            } => {
                assert_eq!(eq_indices.len(), 2);
                assert_eq!(var_indices.len(), 2);
            }
            other => panic!("expected TornBlock or CoupledLM, got {other:?}"),
        }
    }

    #[test]
    fn test_build_ic_plan_handles_scalarized_array_reference_forms() {
        let mut dae = Dae::new();

        let mut u = dae::Variable::new(VarName::new("u"));
        u.dims = vec![2];
        dae.algebraics.insert(VarName::new("u"), u);
        dae.algebraics
            .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));

        dae.f_x.push(eq_from(sub(index("u", 1), lit(2.0))));
        dae.f_x.push(eq_from(sub(index("u", 2), lit(3.0))));
        dae.f_x.push(eq_from(sub(
            var_ref("y"),
            flat::Expression::BuiltinCall {
                function: flat::BuiltinFunction::Product,
                args: vec![var_ref("u")],
            },
        )));

        let plan = build_ic_plan(&dae, 0).expect(
            "IC plan should not be structurally singular for index/reduction refs on scalarized arrays",
        );
        assert!(!plan.is_empty());

        let mut referenced = std::collections::BTreeSet::new();
        for block in &plan {
            match block {
                IcBlock::ScalarDirect { var_name, .. } | IcBlock::ScalarNewton { var_name, .. } => {
                    referenced.insert(var_name.clone());
                }
                IcBlock::TornBlock {
                    tear_var_names,
                    causal_sequence,
                    ..
                } => {
                    referenced.extend(tear_var_names.iter().cloned());
                    referenced.extend(causal_sequence.iter().map(|step| step.var_name.clone()));
                }
                IcBlock::CoupledLM { var_names, .. } => {
                    referenced.extend(var_names.iter().cloned());
                }
            }
        }

        assert!(referenced.contains("u[1]"));
        assert!(referenced.contains("u[2]"));
        assert!(referenced.contains("y"));
    }

    #[test]
    fn test_build_ic_plan_flags_rectangular_algebraic_subsystem() {
        let mut dae = Dae::new();
        dae.algebraics
            .insert(VarName::new("y"), dae::Variable::new(VarName::new("y")));
        dae.algebraics
            .insert(VarName::new("z"), dae::Variable::new(VarName::new("z")));
        dae.f_x.push(eq_from(sub(var_ref("y"), lit(1.0))));

        let err =
            build_ic_plan(&dae, 0).expect_err("rectangular algebraic subsystem must be singular");
        match err {
            StructuralError::Singular {
                n_equations,
                n_unknowns,
                n_matched,
                unmatched_unknowns,
                ..
            } => {
                assert_eq!(n_equations, 1);
                assert_eq!(n_unknowns, 2);
                assert_eq!(n_matched, 1);
                assert!(
                    unmatched_unknowns.iter().any(|name| name == "z"),
                    "expected unmatched unknown list to include z, got: {unmatched_unknowns:?}"
                );
            }
            other => panic!("expected singular error, got {other:?}"),
        }
    }

    #[test]
    fn test_build_ic_plan_relaxes_square_singular_subsystem() {
        let mut dae = Dae::new();
        dae.algebraics
            .insert(VarName::new("v1"), dae::Variable::new(VarName::new("v1")));
        dae.algebraics
            .insert(VarName::new("v2"), dae::Variable::new(VarName::new("v2")));
        dae.algebraics
            .insert(VarName::new("i"), dae::Variable::new(VarName::new("i")));

        // One voltage equality plus two conflicting current equations creates a
        // square but structurally deficient system (1 unmatched eq + 1 unmatched unknown).
        dae.f_x.push(eq_from(sub(var_ref("v1"), var_ref("v2"))));
        dae.f_x.push(eq_from(sub(var_ref("i"), lit(1.0))));
        dae.f_x.push(eq_from(sub(var_ref("i"), lit(2.0))));

        let plan =
            build_ic_plan(&dae, 0).expect("relaxed IC fallback should keep the matched subsystem");
        assert!(!plan.is_empty());

        let mut referenced = std::collections::BTreeSet::new();
        for block in &plan {
            match block {
                IcBlock::ScalarDirect { var_name, .. } | IcBlock::ScalarNewton { var_name, .. } => {
                    referenced.insert(var_name.clone());
                }
                IcBlock::TornBlock {
                    tear_var_names,
                    causal_sequence,
                    ..
                } => {
                    referenced.extend(tear_var_names.iter().cloned());
                    referenced.extend(causal_sequence.iter().map(|step| step.var_name.clone()));
                }
                IcBlock::CoupledLM { var_names, .. } => {
                    referenced.extend(var_names.iter().cloned());
                }
            }
        }

        assert!(referenced.contains("i"));
        assert!(referenced.contains("v1") || referenced.contains("v2"));
    }

    #[test]
    fn test_build_ic_relaxation_hint_reports_drop_set_for_square_singular_subsystem() {
        let mut dae = Dae::new();
        dae.algebraics
            .insert(VarName::new("v1"), dae::Variable::new(VarName::new("v1")));
        dae.algebraics
            .insert(VarName::new("v2"), dae::Variable::new(VarName::new("v2")));
        dae.algebraics
            .insert(VarName::new("i"), dae::Variable::new(VarName::new("i")));
        dae.f_x.push(eq_from(sub(var_ref("v1"), var_ref("v2"))));
        dae.f_x.push(eq_from(sub(var_ref("i"), lit(1.0))));
        dae.f_x.push(eq_from(sub(var_ref("i"), lit(2.0))));

        let hint = build_ic_relaxation_hint(&dae, 0)
            .expect("square singular subsystem should produce hint");
        assert_eq!(hint.dropped_eq_global.len(), 1);
        assert_eq!(hint.dropped_unknown_names.len(), 1);
        assert!(
            hint.dropped_unknown_names
                .iter()
                .all(|name| name == "v1" || name == "v2" || name == "i"),
            "unexpected dropped unknown names: {:?}",
            hint.dropped_unknown_names
        );
    }
}
