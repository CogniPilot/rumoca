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

use rumoca_core::{ExpressionRewriter, maybe_elapsed_seconds, maybe_start_timer_if};
use rumoca_ir_dae as dae;

mod orphan_unknowns;
mod runtime_known;
mod runtime_protection;
mod solve_for_unknown;
mod substitution_application;
mod tearing_elimination;

use orphan_unknowns::{drop_unreferenced_continuous_unknowns, output_partition_contains_unknown};
use runtime_known::singular_rows_are_runtime_known_assignments;
use runtime_protection::{
    assignment_target_name, expr_references_any_discrete_name,
    expr_references_any_runtime_discrete_target, is_runtime_protected_unknown,
    runtime_defined_discrete_target_names, runtime_partition_or_event_refs_var,
    runtime_protected_unknown_names, should_preserve_runtime_known_assignment,
};
pub use solve_for_unknown::try_solve_for_unknown;
use substitution_application::{
    apply_substitutions_in_order, apply_substitutions_to_dae_partitions,
    apply_substitutions_to_remaining_once, equation_analysis_expr,
};
use tearing_elimination::tear_and_eliminate_loop_block;

use crate::{BltBlock, EquationRef, StructuralError, UnknownId, sort_dae};

use rumoca_core::ExpressionVisitor;
#[cfg(test)]
use rumoca_ir_dae::expr_contains_der_of;
use rumoca_ir_dae::{
    DerivativeNameMatcher, embedded_subscripts_all_one, expr_contains_der_of_any,
    expr_contains_var, parse_embedded_subscripts, split_complex_field_suffix, subscripts_all_one,
    var_ref_matches_unknown,
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
    /// The expression it equals (all prior substitutions already applied).
    pub expr: Expression,
    /// Dimensions of the eliminated variable, if known.
    pub var_dims: Vec<i64>,
    /// Dimensions of the replacement expression, if known.
    pub replacement_dims: Vec<i64>,
    /// Declared dimensions of the aggregate that owns this scalar substitution.
    pub aggregate_dims: Vec<i64>,
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
    all_unknowns: &'a [VarName],
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
pub fn eliminate_trivial(dae: &mut Dae) -> EliminationResult {
    let trace = eliminate_trace_enabled();
    let t_total = maybe_start_timer_if(trace);

    // Phase A: resolve boundary equations to make the system non-singular.
    let t_boundary = maybe_start_timer_if(trace);
    let mut result = resolve_boundary_equations(dae);
    if trace {
        crate::structural_trace!(
            "[sim-trace] eliminate_trivial boundary elapsed={:.3}s eliminated_eqs={}",
            maybe_elapsed_seconds(t_boundary),
            result.n_eliminated
        );
    }

    // Phase B: BLT scalar-block elimination on the (now hopefully non-singular) system.
    // Extract blocks before mutating dae (SortedDae borrows dae immutably).
    let mut blt_error = None;
    let mut sort_input = dae.clone();
    drop_unreferenced_continuous_unknowns(&mut sort_input);
    let blocks = match sort_dae(&sort_input) {
        Ok(sorted) => Some(sorted.blocks.clone()),
        Err(StructuralError::EmptySystem) => None,
        Err(err) if singular_rows_are_runtime_known_assignments(&sort_input, &err) => None,
        Err(err) => {
            blt_error = Some(err);
            None
        }
    };
    if let Some(blocks) = blocks {
        let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
        let t_blt = maybe_start_timer_if(trace);
        let blt_result = eliminate_via_blt(dae, &blocks, &state_names);
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
    apply_substitutions_to_dae_partitions(dae, &result.substitutions);
    if trace {
        crate::structural_trace!(
            "[sim-trace] eliminate_trivial total elapsed={:.3}s eliminated_eqs={}",
            maybe_elapsed_seconds(t_total),
            result.n_eliminated
        );
    }

    result
}

pub fn apply_elimination_substitutions_to_dae(dae: &mut Dae, substitutions: &[Substitution]) {
    apply_substitutions_to_dae_partitions(dae, substitutions);
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
fn resolve_boundary_equations(dae: &mut Dae) -> EliminationResult {
    let all_unknowns: Vec<VarName> = dae
        .variables
        .algebraics
        .keys()
        .chain(dae.variables.outputs.keys())
        .cloned()
        .collect();
    let runtime_protected_unknowns = runtime_protected_unknown_names(dae);
    let runtime_defined_discrete_targets = runtime_defined_discrete_target_names(dae);

    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    let state_derivative_matcher = DerivativeNameMatcher::from_var_names(&state_names);
    // Track which unknowns have been resolved (removed from the live set).
    let mut resolved: HashSet<VarName> = HashSet::new();
    let mut substitutions: Vec<Substitution> = Vec::new();
    let mut eliminated_eq_indices: Vec<usize> = Vec::new();
    let mut eliminated_eq_flags = vec![false; dae.continuous.equations.len()];

    // Build (eq_idx, unknown_count) pairs, sort by ascending unknown count.
    let mut eq_order: Vec<(usize, usize)> = (0..dae.continuous.equations.len())
        .map(|eq_idx| {
            let expr = equation_analysis_expr(&dae.continuous.equations[eq_idx]);
            let count = count_live_unknowns(&expr, &all_unknowns, &resolved, dae);
            (eq_idx, count)
        })
        .collect();

    eq_order.sort_by_key(|&(_, count)| count);

    for (eq_idx, _) in eq_order {
        if eliminated_eq_flags[eq_idx] {
            continue;
        }

        let expr = equation_analysis_expr(&dae.continuous.equations[eq_idx]);
        let eq_rhs = apply_substitutions_in_order(&expr, &substitutions);
        let is_connection_eq = dae.continuous.equations[eq_idx]
            .origin
            .starts_with("connection equation:");

        // Re-count live unknowns (may have decreased due to prior resolutions).
        let live: Vec<VarName> = find_live_scalar_unknowns(&eq_rhs, &all_unknowns, &resolved, dae);

        if should_skip_connection_equation(
            dae,
            &eq_rhs,
            is_connection_eq,
            live.len(),
            &runtime_defined_discrete_targets,
        ) {
            continue;
        }
        let has_state_derivative = expr_contains_der_of_any(&eq_rhs, &state_derivative_matcher);
        if let Some((var_name, solution)) = aggregate_alias_for_elimination(
            dae,
            &eq_rhs,
            &runtime_protected_unknowns,
            &runtime_defined_discrete_targets,
        ) {
            substitutions.push(substitution_for_var(dae, var_name.clone(), solution));
            eliminated_eq_indices.push(eq_idx);
            eliminated_eq_flags[eq_idx] = true;
            resolved.insert(var_name);
            continue;
        }
        if live.is_empty() {
            let mut zero_unknown_ctx = ZeroUnknownEliminationCtx {
                dae,
                state_names: &state_names,
                all_unknowns: &all_unknowns,
                resolved: &resolved,
                runtime_protected_unknowns: &runtime_protected_unknowns,
                runtime_defined_discrete_targets: &runtime_defined_discrete_targets,
                substitutions: &mut substitutions,
                eliminated_eq_indices: &mut eliminated_eq_indices,
                eliminated_eq_flags: &mut eliminated_eq_flags,
            };
            try_eliminate_zero_unknown_equation(
                eq_idx,
                &eq_rhs,
                has_state_derivative,
                &mut zero_unknown_ctx,
            );
            continue;
        }

        let Some((var_name, solution)) = choose_solvable_unknown_for_elimination(
            dae,
            &eq_rhs,
            &live,
            has_state_derivative,
            &runtime_protected_unknowns,
        ) else {
            continue;
        };
        substitutions.push(substitution_for_var(
            dae,
            var_name.clone(),
            solution.clone(),
        ));
        eliminated_eq_indices.push(eq_idx);
        eliminated_eq_flags[eq_idx] = true;
        resolved.insert(var_name.clone());
    }

    finish_boundary_elimination(
        dae,
        substitutions,
        eliminated_eq_flags,
        eliminated_eq_indices,
        &resolved,
    )
}

fn finish_boundary_elimination(
    dae: &mut Dae,
    substitutions: Vec<Substitution>,
    eliminated_eq_flags: Vec<bool>,
    mut eliminated_eq_indices: Vec<usize>,
    resolved: &HashSet<VarName>,
) -> EliminationResult {
    apply_substitutions_to_remaining_once(dae, &eliminated_eq_flags, &substitutions);
    let n_eliminated = eliminated_eq_indices.len();
    eliminated_eq_indices.sort_unstable();
    for &idx in eliminated_eq_indices.iter().rev() {
        dae.continuous.equations.remove(idx);
    }
    for name in resolved {
        dae.variables.algebraics.shift_remove(name);
        dae.variables.outputs.shift_remove(name);
    }
    EliminationResult {
        substitutions,
        n_eliminated,
        blt_error: None,
    }
}

fn aggregate_alias_for_elimination(
    dae: &Dae,
    rhs: &Expression,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> Option<(VarName, Expression)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = rhs
    else {
        return None;
    };
    let lhs_ref = full_var_ref(lhs)?;
    let rhs_ref = full_var_ref(rhs)?;
    if lhs_ref.var_name() == rhs_ref.var_name() {
        return None;
    }
    if !same_aggregate_shape(dae, lhs_ref.var_name(), rhs_ref.var_name()) {
        return None;
    }

    let lhs_candidate = aggregate_alias_candidate(
        dae,
        lhs_ref,
        rhs_ref,
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    );
    let rhs_candidate = aggregate_alias_candidate(
        dae,
        rhs_ref,
        lhs_ref,
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    );
    match (lhs_candidate, rhs_candidate) {
        (Some(lhs), Some(rhs)) => Some(preferred_aggregate_alias_candidate(lhs, rhs)),
        (Some(candidate), None) | (None, Some(candidate)) => Some(candidate),
        (None, None) => None,
    }
}

fn full_var_ref(expr: &Expression) -> Option<&Reference> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name),
        _ => None,
    }
}

fn same_aggregate_shape(dae: &Dae, lhs: &VarName, rhs: &VarName) -> bool {
    let lhs_dims = dae_var_dims(dae, lhs);
    !lhs_dims.is_empty() && lhs_dims == dae_var_dims(dae, rhs)
}

fn aggregate_alias_candidate(
    dae: &Dae,
    eliminated: &Reference,
    replacement: &Reference,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> Option<(VarName, Expression)> {
    let var_name = eliminated.var_name();
    if !can_eliminate_aggregate_alias_var(
        dae,
        var_name,
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    ) {
        return None;
    }
    Some((
        var_name.clone(),
        Expression::VarRef {
            name: replacement.clone(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
    ))
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

fn should_skip_connection_equation(
    dae: &Dae,
    eq_rhs: &Expression,
    is_connection_eq: bool,
    live_count: usize,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    if !is_connection_eq {
        return false;
    }
    // MLS Appendix B B.1b/B.1c: discrete signal paths remain event-discrete
    // constraints at runtime. Boundary elimination only substitutes through
    // f_x; dropping connection aliases on any discrete path can disconnect
    // internal connector chains and freeze downstream values at defaults.
    let touches_runtime_discrete_path =
        expr_references_any_runtime_discrete_target(eq_rhs, runtime_defined_discrete_targets)
            || expr_references_any_discrete_name(dae, eq_rhs);
    if touches_runtime_discrete_path {
        return true;
    }
    // Preserve multi-unknown connection equations through structural solving.
    // After prior substitutions reduce a connection equation to a single
    // unknown assignment, allow elimination.
    live_count > 1
}

fn try_eliminate_zero_unknown_equation(
    eq_idx: usize,
    eq_rhs: &Expression,
    has_state_derivative: bool,
    ctx: &mut ZeroUnknownEliminationCtx<'_>,
) {
    let references_state_value = ctx
        .state_names
        .iter()
        .any(|sn| expr_contains_var(eq_rhs, sn));
    if has_state_derivative
        || references_state_value
        || has_any_live_unknown(eq_rhs, ctx.all_unknowns, ctx.resolved, ctx.dae)
    {
        return;
    }
    // MLS Appendix B / §8.3 / §16.5.1: a zero-unknown equation may still
    // define a live runtime discrete/event value. Do not drop those rows
    // unless they can be substituted safely through every runtime consumer.
    if should_preserve_runtime_known_assignment(ctx.dae, eq_rhs) {
        return;
    }
    let n_subs_before = ctx.substitutions.len();
    maybe_push_non_unknown_alias_substitution(
        ctx.dae,
        eq_rhs,
        ctx.runtime_protected_unknowns,
        ctx.runtime_defined_discrete_targets,
        ctx.substitutions,
    );
    if assignment_target_name(eq_rhs).is_some_and(|target| dae_var(ctx.dae, &target).is_some())
        && ctx.substitutions.len() == n_subs_before
    {
        return;
    }
    ctx.eliminated_eq_indices.push(eq_idx);
    ctx.eliminated_eq_flags[eq_idx] = true;
}

fn choose_solvable_unknown_for_elimination(
    dae: &Dae,
    rhs: &Expression,
    live: &[VarName],
    has_state_derivative: bool,
    runtime_protected_unknowns: &IndexSet<String>,
) -> Option<(VarName, Expression)> {
    let mut candidates: Vec<&VarName> = live.iter().collect();
    candidates.sort_by(|a, b| {
        let a_is_output = output_partition_contains_unknown(dae, a);
        let b_is_output = output_partition_contains_unknown(dae, b);
        b_is_output
            .cmp(&a_is_output)
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
        if expr_contains_unsliced_multiscalar_ref(&solution, dae) {
            continue;
        }
        if live.len() > 1
            && !is_alias_solution_for_other_live_unknown(&solution, candidate, live)
            && !direct_assignment_solution
        {
            continue;
        }
        return Some((candidate.clone(), solution));
    }
    None
}

fn choose_solvable_non_unknown_alias_for_elimination(
    dae: &Dae,
    rhs: &Expression,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> Option<(VarName, Expression)> {
    let Expression::Binary {
        op,
        lhs,
        rhs: r,
        span: rumoca_core::Span::DUMMY,
    } = rhs
    else {
        return None;
    };
    if !matches!(op, OpBinary::Sub) {
        return None;
    }

    let mut candidates: Vec<VarName> = Vec::with_capacity(2);
    if let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
        && subscripts.is_empty()
    {
        candidates.push(name.var_name().clone());
    }
    if let Expression::VarRef {
        name, subscripts, ..
    } = r.as_ref()
        && subscripts.is_empty()
        && !candidates
            .iter()
            .any(|existing| existing == name.var_name())
    {
        candidates.push(name.var_name().clone());
    }

    for candidate in candidates {
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
        if dae_var_size(dae, &candidate) > 1 {
            continue;
        }

        let Some(solution) = try_solve_for_unknown(rhs, &candidate) else {
            continue;
        };
        if expr_contains_var(&solution, &candidate) {
            continue;
        }
        if expr_contains_unsliced_multiscalar_ref(&solution, dae) {
            continue;
        }
        if !is_symbolically_stable_solution(&solution) {
            continue;
        }
        return Some((candidate, solution));
    }

    None
}

fn maybe_push_non_unknown_alias_substitution(
    dae: &Dae,
    eq_rhs: &Expression,
    runtime_protected_unknowns: &IndexSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
    substitutions: &mut Vec<Substitution>,
) {
    let Some((var_name, solution)) = choose_solvable_non_unknown_alias_for_elimination(
        dae,
        eq_rhs,
        runtime_protected_unknowns,
        runtime_defined_discrete_targets,
    ) else {
        return;
    };
    substitutions.push(substitution_for_var(dae, var_name.clone(), solution));
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

fn is_alias_solution_for_other_live_unknown(
    solution: &Expression,
    candidate: &VarName,
    live: &[VarName],
) -> bool {
    let others: Vec<&VarName> = live
        .iter()
        .filter(|name| *name != candidate && expr_contains_var(solution, name))
        .collect();
    if others.len() != 1 {
        return false;
    }
    is_alias_expression_of(solution, others[0])
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

fn is_alias_expression_of(expr: &Expression, target: &VarName) -> bool {
    match expr {
        Expression::VarRef { .. } => expr_contains_var(expr, target),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => is_alias_expression_of(rhs, target),
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

/// Count how many live (non-resolved) scalar unknowns appear in an expression.
fn count_live_unknowns(
    expr: &Expression,
    all_unknowns: &[VarName],
    resolved: &HashSet<VarName>,
    dae: &Dae,
) -> usize {
    let mut var_refs = Vec::new();
    collect_var_ref_nodes(expr, &mut var_refs);
    all_unknowns
        .iter()
        .filter(|v| !resolved.contains(*v) && refs_contain_unknown(&var_refs, v, dae))
        .count()
}

fn has_any_live_unknown(
    expr: &Expression,
    all_unknowns: &[VarName],
    resolved: &HashSet<VarName>,
    dae: &Dae,
) -> bool {
    let mut var_refs = Vec::new();
    collect_var_ref_nodes(expr, &mut var_refs);
    all_unknowns
        .iter()
        .any(|v| !resolved.contains(v) && refs_contain_unknown(&var_refs, v, dae))
}

/// Find the live scalar unknowns referenced by an expression.
fn find_live_scalar_unknowns(
    expr: &Expression,
    all_unknowns: &[VarName],
    resolved: &HashSet<VarName>,
    dae: &Dae,
) -> Vec<VarName> {
    let mut var_refs = Vec::new();
    collect_var_ref_nodes(expr, &mut var_refs);
    all_unknowns
        .iter()
        .filter(|v| {
            !resolved.contains(*v)
                && refs_contain_unknown(&var_refs, v, dae)
                && dae
                    .variables
                    .algebraics
                    .get(*v)
                    .or_else(|| dae.variables.outputs.get(*v))
                    .map(|var| var.size() == 1)
                    .unwrap_or(false)
        })
        .cloned()
        .collect()
}

fn collect_var_ref_nodes(
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

fn refs_contain_unknown(
    refs: &[(Reference, Vec<rumoca_core::Subscript>)],
    unknown: &VarName,
    dae: &Dae,
) -> bool {
    refs.iter().any(|(name, subscripts)| {
        var_ref_mentions_unknown_for_presence(name, subscripts.as_slice(), unknown, dae)
    })
}

fn unknown_scalar_size(dae: &Dae, unknown: &VarName) -> usize {
    dae_var_size(dae, unknown)
}

fn dae_var_size(dae: &Dae, name: &VarName) -> usize {
    dae_var(dae, name).map(|v| v.size()).unwrap_or(1)
}

fn dae_var_dims(dae: &Dae, name: &VarName) -> Vec<i64> {
    dae_var(dae, name)
        .map(|var| var.dims.clone())
        .unwrap_or_default()
}

fn dae_var<'a>(dae: &'a Dae, name: &VarName) -> Option<&'a dae::Variable> {
    dae.variables
        .algebraics
        .get(name)
        .or_else(|| dae.variables.outputs.get(name))
        .or_else(|| dae.variables.states.get(name))
        .or_else(|| dae.variables.inputs.get(name))
        .or_else(|| dae.variables.discrete_reals.get(name))
        .or_else(|| dae.variables.discrete_valued.get(name))
        .or_else(|| dae.variables.parameters.get(name))
        .or_else(|| dae.variables.constants.get(name))
}

pub(super) fn substitution_for_var(dae: &Dae, var_name: VarName, expr: Expression) -> Substitution {
    Substitution {
        var_dims: dae_var_dims(dae, &var_name),
        replacement_dims: replacement_expr_dims(dae, &expr),
        aggregate_dims: aggregate_dims_for_substitution(dae, &var_name),
        env_keys: vec![var_name.as_str().to_string()],
        var_name,
        expr,
    }
}

fn aggregate_dims_for_substitution(dae: &Dae, var_name: &VarName) -> Vec<i64> {
    let Some((base, _)) = scalar_var_name_key(var_name) else {
        return dae_var_dims(dae, var_name);
    };
    dae_var_dims(dae, &VarName::new(base))
}

fn replacement_expr_dims(dae: &Dae, expr: &Expression) -> Vec<i64> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => dae_var_dims(dae, name.var_name()),
        Expression::VarRef { .. } | Expression::Index { .. } => Vec::new(),
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => array_expr_dims(elements, *is_matrix),
        _ => Vec::new(),
    }
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

fn var_ref_mentions_unknown_for_presence(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    unknown: &VarName,
    dae: &Dae,
) -> bool {
    if var_ref_matches_unknown(name, subscripts, unknown) {
        return true;
    }

    // MLS §10.6 / SPEC_0019: array equations stay aggregate before
    // scalarization. Any indexed or sliced reference to an aggregate unknown is
    // still a live reference to that unknown at this phase.
    if unknown_scalar_size(dae, unknown) <= 1 {
        return false;
    }
    if path_has_scalar_indices(unknown.as_str()) {
        return false;
    }

    let Some(name_base) = dae::component_base_name(name.as_str()) else {
        return false;
    };
    let Some(unknown_base) = dae::component_base_name(unknown.as_str()) else {
        return false;
    };
    name_base == unknown_base
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
) -> EliminationResult {
    let state_derivative_matcher = DerivativeNameMatcher::from_var_names(state_names);
    let runtime_protected_unknowns = runtime_protected_unknown_names(dae);
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
                &state_derivative_matcher,
                &mut substitutions,
                &mut eliminated_eq_indices,
                &mut eliminated_eq_flags,
                &mut eliminated_var_names,
            ),
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
            ),
        }
    }

    // Apply BLT substitutions once to the remaining equations.
    apply_substitutions_to_remaining_once(dae, &eliminated_eq_flags, &substitutions);

    let n_eliminated = eliminated_eq_indices.len();

    // Remove eliminated equations (in reverse order to preserve indices).
    eliminated_eq_indices.sort_unstable();
    for &idx in eliminated_eq_indices.iter().rev() {
        dae.continuous.equations.remove(idx);
    }

    // Remove eliminated variables from algebraics and outputs.
    for name in &eliminated_var_names {
        dae.variables.algebraics.shift_remove(name);
        dae.variables.outputs.shift_remove(name);
    }

    EliminationResult {
        substitutions,
        n_eliminated,
        blt_error: None,
    }
}

#[allow(clippy::too_many_arguments)]
fn eliminate_scalar_blt_block(
    dae: &Dae,
    equation: &EquationRef,
    unknown: &UnknownId,
    runtime_protected_unknowns: &IndexSet<String>,
    state_derivative_matcher: &DerivativeNameMatcher,
    substitutions: &mut Vec<Substitution>,
    eliminated_eq_indices: &mut Vec<usize>,
    eliminated_eq_flags: &mut [bool],
    eliminated_var_names: &mut Vec<VarName>,
) {
    let Some((eq_idx, var_name, solution)) = scalar_blt_solution(
        dae,
        equation,
        unknown,
        runtime_protected_unknowns,
        state_derivative_matcher,
        substitutions,
    ) else {
        return;
    };
    substitutions.push(substitution_for_var(dae, var_name.clone(), solution));
    eliminated_eq_indices.push(eq_idx);
    eliminated_eq_flags[eq_idx] = true;
    eliminated_var_names.push(var_name);
}

fn scalar_blt_solution(
    dae: &Dae,
    equation: &EquationRef,
    unknown: &UnknownId,
    runtime_protected_unknowns: &IndexSet<String>,
    state_derivative_matcher: &DerivativeNameMatcher,
    substitutions: &[Substitution],
) -> Option<(usize, VarName, Expression)> {
    let raw_var_name = algebraic_or_output_unknown(unknown)?;
    let var_name = normalize_unknown_for_dae(dae, raw_var_name);
    if !can_eliminate_scalar_unknown(dae, &var_name, runtime_protected_unknowns) {
        return None;
    }

    let eq_idx = equation.0;
    if !can_use_equation_for_elimination(dae, eq_idx) {
        return None;
    }

    let is_output = dae.variables.outputs.contains_key(&var_name);
    let has_state_derivative = equation_has_state_derivative(dae, eq_idx, state_derivative_matcher);
    if has_state_derivative && !is_output {
        return None;
    }

    let eq_rhs = apply_substitutions_in_order(&dae.continuous.equations[eq_idx].rhs, substitutions);
    stable_solution_for_unknown(dae, &eq_rhs, &var_name)
        .map(|solution| (eq_idx, var_name, solution))
}

fn algebraic_or_output_unknown(unknown: &UnknownId) -> Option<&VarName> {
    match unknown {
        UnknownId::Variable(name) => Some(name),
        UnknownId::DerState(_) | UnknownId::SolverY(_) => None,
    }
}

fn can_eliminate_scalar_unknown(
    dae: &Dae,
    var_name: &VarName,
    runtime_protected_unknowns: &IndexSet<String>,
) -> bool {
    !is_runtime_protected_unknown(var_name, runtime_protected_unknowns)
        && !unknown_is_fixed(dae, var_name)
        && !dae.variables.states.contains_key(var_name)
        && dae_var_size(dae, var_name) == 1
}

fn can_use_equation_for_elimination(dae: &Dae, eq_idx: usize) -> bool {
    dae.continuous
        .equations
        .get(eq_idx)
        .is_some_and(|eq| !eq.origin.starts_with("connection equation:"))
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
) -> Option<Expression> {
    let solution = try_solve_for_unknown(rhs, var_name)?;
    if expr_contains_var(&solution, var_name)
        || expr_contains_unsliced_multiscalar_ref(&solution, dae)
        || !is_symbolically_stable_solution(&solution)
    {
        return None;
    }
    Some(solution)
}

/// Apply substitutions in-order to an expression.
pub fn apply_substitutions_to_expr(
    expr: &Expression,
    substitutions: &[Substitution],
) -> Expression {
    let mut out = apply_record_field_aggregate_substitutions(expr, substitutions);
    for sub in substitutions {
        if expr_contains_substitution_target(&out, &sub.var_name, &sub.var_dims) {
            out = SubstituteVarRewriter {
                var: &sub.var_name,
                replacement: &sub.expr,
                var_dims: &sub.var_dims,
                replacement_dims: &sub.replacement_dims,
            }
            .rewrite_expression(&out);
        }
    }
    out
}

fn expr_contains_substitution_target(expr: &Expression, var: &VarName, var_dims: &[i64]) -> bool {
    let mut checker = SubstitutionTargetChecker {
        var,
        var_dims,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct SubstitutionTargetChecker<'a> {
    var: &'a VarName,
    var_dims: &'a [i64],
    found: bool,
}

impl ExpressionVisitor for SubstitutionTargetChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[rumoca_core::Subscript]) {
        if aggregate_subscript_ref_matches_var(name, subscripts, self.var, self.var_dims)
            || var_ref_matches_unknown_for_substitution(name, subscripts, self.var)
            || embedded_alias_indices_for_substitution(name, subscripts, self.var).is_some()
        {
            self.found = true;
            return;
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

fn apply_record_field_aggregate_substitutions(
    expr: &Expression,
    substitutions: &[Substitution],
) -> Expression {
    let aggregate_alias_groups = aggregate_alias_substitution_groups(substitutions);
    let indexed_groups = indexed_record_field_substitution_groups(substitutions);
    let complex_groups = complex_field_substitution_groups(substitutions);
    if aggregate_alias_groups.is_empty() && indexed_groups.is_empty() && complex_groups.is_empty() {
        return expr.clone();
    }
    RecordFieldAggregateRewriter {
        aggregate_alias_groups,
        indexed_groups,
        complex_groups,
    }
    .rewrite_expression(expr)
}

#[derive(Debug, Clone, Default)]
struct AggregateAliasSubstitutionGroup {
    dims: Option<Vec<usize>>,
    replacement_base: Option<String>,
    values: IndexMap<Vec<usize>, Expression>,
    invalid: bool,
}

impl AggregateAliasSubstitutionGroup {
    fn insert(&mut self, indices: Vec<usize>, dims: Vec<usize>, expr: Expression) {
        if indices.len() != dims.len()
            || indices
                .iter()
                .zip(dims.iter())
                .any(|(index, dim)| *index == 0 || *index > *dim)
        {
            self.invalid = true;
            return;
        }
        match &self.dims {
            Some(existing) if existing != &dims => {
                self.invalid = true;
                return;
            }
            None => self.dims = Some(dims),
            Some(_) => {}
        }
        self.replacement_base =
            replacement_aggregate_base(&expr, &indices, self.replacement_base.as_deref());
        self.values.insert(indices, expr);
    }

    fn to_replacement_expr(&self, span: rumoca_core::Span) -> Option<Expression> {
        if self.invalid {
            return None;
        }
        let expected_len = self.expected_len()?;
        if expected_len <= 1 || self.values.len() != expected_len {
            return None;
        }
        if let Some(base) = &self.replacement_base {
            return Some(Expression::VarRef {
                name: Reference::new(base),
                subscripts: Vec::new(),
                span,
            });
        }
        self.array_expr_at_depth(0, &mut Vec::new())
    }

    fn expected_len(&self) -> Option<usize> {
        Some(self.dims.as_ref()?.iter().copied().product())
    }

    fn array_expr_at_depth(&self, depth: usize, current: &mut Vec<usize>) -> Option<Expression> {
        let dims = self.dims.as_ref()?;
        if depth >= dims.len() {
            return self.values.get(current).cloned();
        }
        let mut elements = Vec::with_capacity(dims[depth]);
        for index in 1..=dims[depth] {
            current.push(index);
            elements.push(self.array_expr_at_depth(depth + 1, current)?);
            current.pop();
        }
        Some(Expression::Array {
            elements,
            is_matrix: depth == 0 && dims.len() == 2,
            span: rumoca_core::Span::DUMMY,
        })
    }
}

fn replacement_aggregate_base(
    expr: &Expression,
    expected_indices: &[usize],
    existing_base: Option<&str>,
) -> Option<String> {
    let (base, indices) = scalar_var_ref_key(expr)?;
    (indices == expected_indices && existing_base.is_none_or(|existing| existing == base))
        .then_some(base.to_string())
}

fn aggregate_alias_substitution_groups(
    substitutions: &[Substitution],
) -> IndexMap<String, AggregateAliasSubstitutionGroup> {
    let mut groups = IndexMap::new();
    for substitution in substitutions {
        let Some((base, indices)) = scalar_var_name_key(&substitution.var_name) else {
            continue;
        };
        let Some(dims) = positive_usize_dims(&substitution.aggregate_dims) else {
            continue;
        };
        groups
            .entry(base.to_string())
            .or_insert_with(AggregateAliasSubstitutionGroup::default)
            .insert(indices, dims, substitution.expr.clone());
    }
    groups
}

fn positive_usize_dims(dims: &[i64]) -> Option<Vec<usize>> {
    let dims = dims
        .iter()
        .copied()
        .map(|dim| usize::try_from(dim).ok().filter(|value| *value > 0))
        .collect::<Option<Vec<_>>>()?;
    (!dims.is_empty()).then_some(dims)
}

fn scalar_var_name_key(var_name: &VarName) -> Option<(&str, Vec<usize>)> {
    let scalar = rumoca_core::parse_scalar_name(var_name.as_str())?;
    let indices = positive_usize_indices(&scalar.indices)?;
    (!indices.is_empty()).then_some((scalar.base, indices))
}

fn scalar_var_ref_key(expr: &Expression) -> Option<(&str, Vec<usize>)> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let scalar = rumoca_core::parse_scalar_name(name.as_str())?;
    let indices = positive_usize_indices(&scalar.indices)?;
    (!indices.is_empty()).then_some((scalar.base, indices))
}

fn positive_usize_indices(indices: &[i64]) -> Option<Vec<usize>> {
    indices
        .iter()
        .copied()
        .map(|index| usize::try_from(index).ok().filter(|value| *value > 0))
        .collect()
}

#[derive(Debug, Clone, Default)]
struct IndexedRecordFieldSubstitutionGroup {
    dims: Vec<usize>,
    values: IndexMap<Vec<usize>, Expression>,
}

impl IndexedRecordFieldSubstitutionGroup {
    fn insert(&mut self, indices: Vec<usize>, expr: Expression) {
        if indices.len() > self.dims.len() {
            self.dims.resize(indices.len(), 0);
        }
        for (idx, value) in indices.iter().enumerate() {
            self.dims[idx] = self.dims[idx].max(*value);
        }
        self.values.insert(indices, expr);
    }

    fn to_array_expr(&self) -> Option<Expression> {
        if self.dims.is_empty() || self.values.len() != self.expected_len() {
            return None;
        }
        self.array_expr_at_depth(0, &mut Vec::new())
    }

    fn expected_len(&self) -> usize {
        self.dims.iter().product()
    }

    fn array_expr_at_depth(&self, depth: usize, current: &mut Vec<usize>) -> Option<Expression> {
        if depth >= self.dims.len() {
            return self.values.get(current).cloned();
        }
        let mut elements = Vec::with_capacity(self.dims[depth]);
        for index in 1..=self.dims[depth] {
            current.push(index);
            elements.push(self.array_expr_at_depth(depth + 1, current)?);
            current.pop();
        }
        Some(Expression::Array {
            elements,
            is_matrix: depth == 0 && self.dims.len() == 2,
            span: rumoca_core::Span::DUMMY,
        })
    }
}

fn indexed_record_field_substitution_groups(
    substitutions: &[Substitution],
) -> IndexMap<String, IndexedRecordFieldSubstitutionGroup> {
    let mut groups = IndexMap::new();
    for substitution in substitutions {
        let Some((aggregate_key, indices)) =
            indexed_record_field_substitution_key(&substitution.var_name)
        else {
            continue;
        };
        groups
            .entry(aggregate_key)
            .or_insert_with(IndexedRecordFieldSubstitutionGroup::default)
            .insert(indices, substitution.expr.clone());
    }
    groups
}

fn indexed_record_field_substitution_key(var_name: &VarName) -> Option<(String, Vec<usize>)> {
    let (indexed_base, field) = rumoca_core::split_last_top_level(var_name.as_str())?;
    let scalar = rumoca_core::parse_scalar_name(indexed_base)?;
    let indices = scalar
        .indices
        .iter()
        .copied()
        .map(usize::try_from)
        .collect::<Result<Vec<_>, _>>()
        .ok()?;
    if indices.is_empty() || indices.contains(&0) {
        return None;
    }
    Some((format!("{}.{}", scalar.base, field), indices))
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

struct RecordFieldAggregateRewriter {
    aggregate_alias_groups: IndexMap<String, AggregateAliasSubstitutionGroup>,
    indexed_groups: IndexMap<String, IndexedRecordFieldSubstitutionGroup>,
    complex_groups: IndexMap<String, ComplexFieldSubstitutionGroup>,
}

impl ExpressionRewriter for RecordFieldAggregateRewriter {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Expression {
        let rewritten_subscripts = self.rewrite_subscripts(subscripts);
        if let Some(replacement) = self
            .aggregate_alias_groups
            .get(name.as_str())
            .and_then(|group| group.to_replacement_expr(span))
        {
            return apply_aggregate_replacement_subscripts(replacement, rewritten_subscripts, span);
        }
        if !rewritten_subscripts.is_empty() {
            return Expression::VarRef {
                name: name.clone(),
                subscripts: rewritten_subscripts,
                span,
            };
        }
        if let Some(array_expr) = self
            .indexed_groups
            .get(name.as_str())
            .and_then(IndexedRecordFieldSubstitutionGroup::to_array_expr)
        {
            return array_expr;
        }
        self.complex_groups
            .get(name.as_str())
            .and_then(|group| group.to_constructor_expr(span))
            .unwrap_or_else(|| self.walk_var_ref_expression(name, subscripts, span))
    }
}

fn apply_aggregate_replacement_subscripts(
    replacement: Expression,
    subscripts: Vec<rumoca_core::Subscript>,
    span: rumoca_core::Span,
) -> Expression {
    if subscripts.is_empty() {
        return replacement;
    }
    match replacement {
        Expression::VarRef {
            name,
            subscripts: replacement_subscripts,
            span: replacement_span,
        } if replacement_subscripts.is_empty() => Expression::VarRef {
            name,
            subscripts,
            span: replacement_span,
        },
        other => Expression::Index {
            base: Box::new(other),
            subscripts,
            span,
        },
    }
}

pub fn resolve_substitutions_in_expr(
    expr: &Expression,
    substitutions: &[Substitution],
) -> Expression {
    let mut out = expr.clone();
    for _ in 0..substitutions.len() {
        let next = apply_substitutions_to_expr(&out, substitutions);
        if next == out {
            return out;
        }
        out = next;
    }
    out
}

fn normalize_unknown_for_dae(dae: &Dae, unknown: &VarName) -> VarName {
    if dae.variables.algebraics.contains_key(unknown) || dae.variables.outputs.contains_key(unknown)
    {
        return unknown.clone();
    }
    let raw = unknown.as_str();
    let Some(base) = dae::component_base_name(raw) else {
        return unknown.clone();
    };
    if base == raw || !embedded_subscripts_all_one(raw) {
        return unknown.clone();
    }
    let base_name = VarName::new(base.as_str());
    let is_singleton = dae
        .variables
        .algebraics
        .get(&base_name)
        .or_else(|| dae.variables.outputs.get(&base_name))
        .is_some_and(|var| var.size() == 1);
    if is_singleton {
        base_name
    } else {
        unknown.clone()
    }
}

fn expr_contains_unsliced_multiscalar_ref(expr: &Expression, dae: &Dae) -> bool {
    let mut checker = UnslicedMultiscalarRefChecker { dae, found: false };
    checker.visit_expression(expr);
    checker.found
}

struct UnslicedMultiscalarRefChecker<'a> {
    dae: &'a Dae,
    found: bool,
}

impl ExpressionVisitor for UnslicedMultiscalarRefChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if !subscripts.is_empty() {
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
            return;
        }

        let size = self
            .dae
            .variables
            .states
            .get(name.var_name())
            .or_else(|| self.dae.variables.algebraics.get(name.var_name()))
            .or_else(|| self.dae.variables.outputs.get(name.var_name()))
            .map(|v| v.size())
            .unwrap_or(0);
        self.found = size > 1;
    }
}

fn embedded_alias_indices_for_substitution(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    var: &VarName,
) -> Option<Vec<i64>> {
    if !subscripts.is_empty() || name.as_str() == var.as_str() {
        return None;
    }
    let name_field = split_complex_field_suffix(name.as_str());
    let var_field = split_complex_field_suffix(var.as_str());
    if name_field.is_some() || var_field.is_some() {
        return None;
    }
    if path_has_scalar_indices(var.as_str()) {
        return None;
    }
    let name_base = dae::component_base_name(name.as_str())?;
    let var_base = dae::component_base_name(var.as_str())?;
    if name_base != var_base {
        return None;
    }
    parse_embedded_subscripts(name.as_str())
        .or_else(|| path_segment_scalar_indices(name.as_str()))
        .filter(|indices| !indices.is_empty())
}

fn path_has_scalar_indices(path: &str) -> bool {
    parse_embedded_subscripts(path)
        .or_else(|| path_segment_scalar_indices(path))
        .is_some_and(|indices| !indices.is_empty())
}

fn path_segment_scalar_indices(path: &str) -> Option<Vec<i64>> {
    let mut indices = Vec::new();
    for segment in rumoca_core::split_path_with_indices(path) {
        if let Some(scalar) = rumoca_core::parse_scalar_name(segment) {
            indices.extend(scalar.indices);
        }
    }
    (!indices.is_empty()).then_some(indices)
}

fn index_replacement_expr(replacement: &Expression, indices: &[i64]) -> Expression {
    let span = replacement.span().unwrap_or(rumoca_core::Span::DUMMY);
    if indices.is_empty() {
        return replacement.clone();
    }
    let extra_subscripts = indices
        .iter()
        .copied()
        .map(|index| rumoca_core::Subscript::generated_index(index, span))
        .collect::<Vec<_>>();
    match replacement {
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
    }
}

fn index_replacement_expr_with_subscripts(
    replacement: &Expression,
    subscripts: &[rumoca_core::Subscript],
) -> Expression {
    let span = replacement.span().unwrap_or(rumoca_core::Span::DUMMY);
    if subscripts.is_empty() {
        return replacement.clone();
    }
    match replacement {
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
        _ => Expression::Index {
            base: Box::new(replacement.clone()),
            subscripts: subscripts.to_vec(),
            span,
        },
    }
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

fn var_ref_matches_unknown_for_substitution(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    unknown: &VarName,
) -> bool {
    if subscripts.is_empty()
        && indexed_record_field_substitution_key(unknown)
            .is_some_and(|(aggregate_key, _)| aggregate_key == name.as_str())
    {
        return false;
    }

    let name_field = split_complex_field_suffix(name.as_str());
    let unknown_field = split_complex_field_suffix(unknown.as_str());

    // Substitution must preserve complex field semantics: do not allow
    // base<->field alias matching here, otherwise `.re/.im` projections can be
    // applied to already-scalar replacement expressions.
    if name_field.is_some() || unknown_field.is_some() {
        return name.as_str() == unknown.as_str()
            && (subscripts.is_empty() || subscripts_all_one(subscripts));
    }

    if subscripts.is_empty()
        && !path_has_scalar_indices(name.as_str())
        && path_has_scalar_indices(unknown.as_str())
    {
        return false;
    }

    var_ref_matches_unknown(name, subscripts, unknown)
}

fn aggregate_subscript_ref_matches_var(
    name: &Reference,
    subscripts: &[rumoca_core::Subscript],
    unknown: &VarName,
    unknown_dims: &[i64],
) -> bool {
    !unknown_dims.is_empty() && !subscripts.is_empty() && name.var_name().id() == unknown.id()
}

/// Check if an expression is a simple VarRef to the given variable.
fn is_var_ref(expr: &Expression, var: &VarName) -> bool {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => var_ref_matches_unknown_for_substitution(name, subscripts, var),
        _ => false,
    }
}

struct SubstituteVarRewriter<'a> {
    var: &'a VarName,
    replacement: &'a Expression,
    var_dims: &'a [i64],
    replacement_dims: &'a [i64],
}

impl ExpressionRewriter for SubstituteVarRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::BuiltinCall {
                function: BuiltinFunction::Pre | BuiltinFunction::Edge | BuiltinFunction::Change,
                ..
            } => {
                // Preserve event-operator arguments to maintain MLS Appendix B
                // pre/change/edge semantics during symbolic substitution.
                expr.clone()
            }
            Expression::ArrayComprehension {
                expr: inner,
                indices,
                filter,
                span,
            } => Expression::ArrayComprehension {
                expr: Box::new(self.rewrite_expression(inner)),
                indices: indices.clone(),
                filter: filter
                    .as_ref()
                    .map(|filter| Box::new(self.rewrite_expression(filter))),
                span: *span,
            },
            _ => self.walk_expression(expr),
        }
    }

    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Expression {
        if let Some(indices) = embedded_alias_indices_for_substitution(name, subscripts, self.var) {
            // MLS §10.6: if a vector alias is eliminated before scalarization,
            // scalarized references to that alias must map to the same scalar
            // component of the replacement expression.
            let replacement_indices =
                replacement_indices_for_alias(&indices, self.var_dims, self.replacement_dims);
            index_replacement_expr(self.replacement, &replacement_indices)
        } else if aggregate_subscript_ref_matches_var(name, subscripts, self.var, self.var_dims) {
            index_replacement_expr_with_subscripts(self.replacement, subscripts)
        } else if var_ref_matches_unknown_for_substitution(name, subscripts, self.var) {
            if !subscripts.is_empty() && !self.var_dims.is_empty() {
                return index_replacement_expr_with_subscripts(self.replacement, subscripts);
            }
            self.replacement.clone()
        } else {
            self.walk_var_ref_expression(name, subscripts, span)
        }
    }
}

#[cfg(test)]
mod tests;
