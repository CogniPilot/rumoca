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

use rumoca_core::{maybe_elapsed_seconds, maybe_start_timer_if};
use rumoca_ir_dae as dae;

use crate::{BltBlock, EquationRef, UnknownId, sort_dae};

type Dae = dae::Dae;
type BuiltinFunction = dae::BuiltinFunction;
type Expression = dae::Expression;
type OpBinary = rumoca_ir_core::OpBinary;
type OpUnary = rumoca_ir_core::OpUnary;
type VarName = dae::VarName;

/// A single symbolic substitution: `var_name = expr`.
#[derive(Debug, Clone)]
pub struct Substitution {
    /// The variable being eliminated.
    pub var_name: VarName,
    /// The expression it equals (all prior substitutions already applied).
    pub expr: Expression,
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
}

struct ZeroUnknownEliminationCtx<'a> {
    dae: &'a Dae,
    state_names: &'a [VarName],
    all_unknowns: &'a [VarName],
    resolved: &'a HashSet<VarName>,
    runtime_protected_unknowns: &'a HashSet<String>,
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
        eprintln!(
            "[sim-trace] eliminate_trivial boundary elapsed={:.3}s eliminated_eqs={}",
            maybe_elapsed_seconds(t_boundary),
            result.n_eliminated
        );
    }

    // Phase B: BLT scalar-block elimination on the (now hopefully non-singular) system.
    // Extract blocks before mutating dae (SortedDae borrows dae immutably).
    let blocks = match sort_dae(dae) {
        Ok(sorted) => Some(sorted.blocks.clone()),
        Err(_) => None,
    };
    if let Some(blocks) = blocks {
        let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
        let t_blt = maybe_start_timer_if(trace);
        let blt_result = eliminate_via_blt(dae, &blocks, &state_names);
        if trace {
            eprintln!(
                "[sim-trace] eliminate_trivial blt elapsed={:.3}s eliminated_eqs={}",
                maybe_elapsed_seconds(t_blt),
                blt_result.n_eliminated
            );
        }
        result.substitutions.extend(blt_result.substitutions);
        result.n_eliminated += blt_result.n_eliminated;
    }
    if trace {
        eprintln!(
            "[sim-trace] eliminate_trivial total elapsed={:.3}s eliminated_eqs={}",
            maybe_elapsed_seconds(t_total),
            result.n_eliminated
        );
    }

    result
}

fn eliminate_trace_enabled() -> bool {
    std::env::var("RUMOCA_SIM_TRACE").is_ok() || std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
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
        .algebraics
        .keys()
        .chain(dae.outputs.keys())
        .cloned()
        .collect();
    let runtime_protected_unknowns = runtime_protected_unknown_names(dae);
    let runtime_defined_discrete_targets = runtime_defined_discrete_target_names(dae);

    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    // Track which unknowns have been resolved (removed from the live set).
    let mut resolved: HashSet<VarName> = HashSet::new();
    let mut substitutions: Vec<Substitution> = Vec::new();
    let mut eliminated_eq_indices: Vec<usize> = Vec::new();
    let mut eliminated_eq_flags = vec![false; dae.f_x.len()];

    // Build (eq_idx, unknown_count) pairs, sort by ascending unknown count.
    let mut eq_order: Vec<(usize, usize)> = (0..dae.f_x.len())
        .map(|eq_idx| {
            let rhs = &dae.f_x[eq_idx].rhs;
            let count = count_live_unknowns(rhs, &all_unknowns, &resolved, dae);
            (eq_idx, count)
        })
        .collect();

    eq_order.sort_by_key(|&(_, count)| count);

    for (eq_idx, _) in eq_order {
        if eliminated_eq_flags[eq_idx] {
            continue;
        }

        let rhs = &dae.f_x[eq_idx].rhs;
        let eq_rhs = apply_substitutions_in_order(rhs, &substitutions);
        let is_connection_eq = dae.f_x[eq_idx].origin.starts_with("connection equation:");

        // Re-count live unknowns (may have decreased due to prior resolutions).
        let live: Vec<VarName> = find_live_scalar_unknowns(&eq_rhs, &all_unknowns, &resolved, dae);

        let has_state_derivative = state_names
            .iter()
            .any(|sn| expr_contains_der_of(&eq_rhs, sn));
        if should_skip_connection_equation(
            dae,
            &eq_rhs,
            is_connection_eq,
            live.len(),
            &runtime_defined_discrete_targets,
        ) {
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
            // Not directly solvable for any live scalar unknown.
            continue;
        };
        substitutions.push(Substitution {
            var_name: var_name.clone(),
            expr: solution.clone(),
            env_keys: vec![var_name.as_str().to_string()],
        });
        eliminated_eq_indices.push(eq_idx);
        eliminated_eq_flags[eq_idx] = true;
        resolved.insert(var_name.clone());
    }

    // Apply boundary substitutions once to the remaining equations.
    apply_substitutions_to_remaining_once(dae, &eliminated_eq_flags, &substitutions);

    let n_eliminated = eliminated_eq_indices.len();

    // Remove eliminated equations (reverse order to preserve indices).
    eliminated_eq_indices.sort_unstable();
    for &idx in eliminated_eq_indices.iter().rev() {
        dae.f_x.remove(idx);
    }

    // Remove resolved variables.
    for name in &resolved {
        dae.algebraics.shift_remove(name);
        dae.outputs.shift_remove(name);
    }

    EliminationResult {
        substitutions,
        n_eliminated,
    }
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
    if assignment_target_name(eq_rhs).is_some() && ctx.substitutions.len() == n_subs_before {
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
    runtime_protected_unknowns: &HashSet<String>,
) -> Option<(VarName, Expression)> {
    let mut candidates: Vec<&VarName> = live.iter().collect();
    candidates.sort_by(|a, b| {
        let a_is_output = dae.outputs.contains_key(*a);
        let b_is_output = dae.outputs.contains_key(*b);
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
        if is_runtime_protected_unknown(candidate, runtime_protected_unknowns) {
            continue;
        }
        let is_output = dae.outputs.contains_key(candidate);
        // Skip equations with state derivatives — unless the candidate is an
        // output that forms a direct alias (e.g. `output y = der(x)`), which
        // can be safely eliminated.
        if has_state_derivative && !is_output {
            continue;
        }
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
    runtime_protected_unknowns: &HashSet<String>,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> Option<(VarName, Expression)> {
    let Expression::Binary { op, lhs, rhs: r } = rhs else {
        return None;
    };
    if !matches!(op, OpBinary::Sub(_)) {
        return None;
    }

    let mut candidates: Vec<VarName> = Vec::with_capacity(2);
    if let Expression::VarRef { name, subscripts } = lhs.as_ref()
        && subscripts.is_empty()
    {
        candidates.push(name.clone());
    }
    if let Expression::VarRef { name, subscripts } = r.as_ref()
        && subscripts.is_empty()
        && !candidates.iter().any(|existing| existing == name)
    {
        candidates.push(name.clone());
    }

    for candidate in candidates {
        if candidate.as_str() == "time" {
            continue;
        }
        if is_runtime_protected_unknown(&candidate, runtime_protected_unknowns) {
            continue;
        }
        if dae.parameters.contains_key(&candidate) || dae.constants.contains_key(&candidate) {
            continue;
        }
        if dae.states.contains_key(&candidate) {
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
    runtime_protected_unknowns: &HashSet<String>,
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
    substitutions.push(Substitution {
        var_name: var_name.clone(),
        expr: solution,
        env_keys: vec![var_name.as_str().to_string()],
    });
}

fn unknown_is_fixed(dae: &Dae, name: &VarName) -> bool {
    dae.states
        .get(name)
        .or_else(|| dae.algebraics.get(name))
        .or_else(|| dae.outputs.get(name))
        .and_then(|var| var.fixed)
        .unwrap_or(false)
}

fn has_direct_assignment_form(rhs: &Expression, candidate: &VarName) -> bool {
    match rhs {
        Expression::Binary {
            op: OpBinary::Sub(_),
            lhs,
            rhs,
        } => is_assignment_target(lhs, candidate) || is_assignment_target(rhs, candidate),
        Expression::Unary {
            op: OpUnary::Minus(_),
            rhs,
        } => has_direct_assignment_form(rhs, candidate),
        _ => false,
    }
}

fn is_assignment_target(expr: &Expression, candidate: &VarName) -> bool {
    match expr {
        Expression::VarRef { name, subscripts } => {
            var_ref_matches_unknown(name, subscripts, candidate)
        }
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
            op: OpUnary::Minus(_),
            rhs,
        } => is_trivial_alias(rhs),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
        } => args.len() == 1 && matches!(&args[0], Expression::VarRef { .. }),
        _ => false,
    }
}

fn is_alias_expression_of(expr: &Expression, target: &VarName) -> bool {
    match expr {
        Expression::VarRef { .. } => expr_contains_var(expr, target),
        Expression::Unary {
            op: OpUnary::Minus(_),
            rhs,
        } => is_alias_expression_of(rhs, target),
        _ => false,
    }
}

fn is_symbolically_stable_solution(expr: &Expression) -> bool {
    match expr {
        Expression::If { .. } => false,
        Expression::BuiltinCall { function, args } => {
            !matches!(
                function,
                rumoca_ir_dae::BuiltinFunction::Smooth
                    | rumoca_ir_dae::BuiltinFunction::NoEvent
                    | rumoca_ir_dae::BuiltinFunction::Homotopy
            ) && args.iter().all(is_symbolically_stable_solution)
        }
        Expression::Binary { lhs, rhs, .. } => {
            is_symbolically_stable_solution(lhs) && is_symbolically_stable_solution(rhs)
        }
        Expression::Unary { rhs, .. } => is_symbolically_stable_solution(rhs),
        Expression::FunctionCall { args, .. } => args.iter().all(is_symbolically_stable_solution),
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().all(is_symbolically_stable_solution)
        }
        Expression::Range { start, step, end } => {
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
        Expression::Index { base, subscripts } => {
            is_symbolically_stable_solution(base)
                && subscripts.iter().all(|sub| match sub {
                    rumoca_ir_dae::Subscript::Expr(expr) => is_symbolically_stable_solution(expr),
                    _ => true,
                })
        }
        Expression::FieldAccess { base, .. } => is_symbolically_stable_solution(base),
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => true,
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
                    .algebraics
                    .get(*v)
                    .or_else(|| dae.outputs.get(*v))
                    .map(|var| var.size() == 1)
                    .unwrap_or(false)
        })
        .cloned()
        .collect()
}

fn collect_var_ref_nodes<'a>(
    expr: &'a Expression,
    out: &mut Vec<(&'a VarName, &'a [rumoca_ir_dae::Subscript])>,
) {
    match expr {
        Expression::VarRef { name, subscripts } => {
            out.push((name, subscripts.as_slice()));
            for subscript in subscripts {
                if let rumoca_ir_dae::Subscript::Expr(inner) = subscript {
                    collect_var_ref_nodes(inner, out);
                }
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_var_ref_nodes(lhs, out);
            collect_var_ref_nodes(rhs, out);
        }
        Expression::Unary { rhs, .. } => collect_var_ref_nodes(rhs, out),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_var_ref_nodes(arg, out);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                collect_var_ref_nodes(condition, out);
                collect_var_ref_nodes(value, out);
            }
            collect_var_ref_nodes(else_branch, out);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for element in elements {
                collect_var_ref_nodes(element, out);
            }
        }
        Expression::Range { start, step, end } => {
            collect_var_ref_nodes(start, out);
            if let Some(step) = step.as_deref() {
                collect_var_ref_nodes(step, out);
            }
            collect_var_ref_nodes(end, out);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_var_ref_nodes(expr, out);
            for idx in indices {
                collect_var_ref_nodes(&idx.range, out);
            }
            if let Some(pred) = filter.as_deref() {
                collect_var_ref_nodes(pred, out);
            }
        }
        Expression::Index { base, subscripts } => {
            collect_var_ref_nodes(base, out);
            for subscript in subscripts {
                if let rumoca_ir_dae::Subscript::Expr(inner) = subscript {
                    collect_var_ref_nodes(inner, out);
                }
            }
        }
        Expression::FieldAccess { base, .. } => collect_var_ref_nodes(base, out),
        Expression::Literal(_) | Expression::Empty => {}
    }
}

fn refs_contain_unknown(
    refs: &[(&VarName, &[rumoca_ir_dae::Subscript])],
    unknown: &VarName,
    dae: &Dae,
) -> bool {
    refs.iter().any(|(name, subscripts)| {
        var_ref_mentions_unknown_for_presence(name, subscripts, unknown, dae)
    })
}

fn unknown_scalar_size(dae: &Dae, unknown: &VarName) -> usize {
    dae_var_size(dae, unknown)
}

fn dae_var_size(dae: &Dae, name: &VarName) -> usize {
    dae.algebraics
        .get(name)
        .or_else(|| dae.outputs.get(name))
        .or_else(|| dae.states.get(name))
        .or_else(|| dae.inputs.get(name))
        .or_else(|| dae.discrete_reals.get(name))
        .or_else(|| dae.discrete_valued.get(name))
        .or_else(|| dae.parameters.get(name))
        .or_else(|| dae.constants.get(name))
        .map(|v| v.size())
        .unwrap_or(1)
}

fn var_ref_mentions_unknown_for_presence(
    name: &VarName,
    subscripts: &[rumoca_ir_dae::Subscript],
    unknown: &VarName,
    dae: &Dae,
) -> bool {
    if var_ref_matches_unknown(name, subscripts, unknown) {
        return true;
    }

    // Base-array unknowns are tracked as aggregate names before scalarization
    // (e.g. `add.u`). Indexed references (`add.u[2]`) must still count as
    // live references so boundary elimination does not drop those equations.
    if unknown_scalar_size(dae, unknown) <= 1 {
        return false;
    }
    if unknown.as_str().contains('[') || !subscripts.is_empty() {
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
    let runtime_protected_unknowns = runtime_protected_unknown_names(dae);
    let mut substitutions: Vec<Substitution> = Vec::new();
    let mut eliminated_eq_indices: Vec<usize> = Vec::new();
    let mut eliminated_eq_flags = vec![false; dae.f_x.len()];
    let mut eliminated_var_names: Vec<VarName> = Vec::new();

    for block in blocks {
        let BltBlock::Scalar {
            equation: EquationRef::Continuous(eq_idx),
            unknown,
        } = block
        else {
            continue;
        };

        // Only eliminate algebraic/output variables, not DerState (ODE equations).
        let raw_var_name = match unknown {
            UnknownId::DerState(_) => continue,
            UnknownId::Variable(name) => name,
        };
        let var_name = normalize_unknown_for_dae(dae, raw_var_name);
        if is_runtime_protected_unknown(&var_name, &runtime_protected_unknowns) {
            continue;
        }
        // Preserve hard Modelica initialization constraints from `fixed=true`
        // aliases. Eliminating them in BLT can silently change IC branches.
        if unknown_is_fixed(dae, &var_name) {
            continue;
        }

        // Only eliminate scalar variables (size == 1).
        let var_size = dae.algebraics.get(&var_name).map(|v| v.size()).unwrap_or(1);
        if var_size != 1 {
            continue;
        }

        let eq_idx = *eq_idx;
        if eq_idx >= dae.f_x.len() {
            continue;
        }
        if dae.f_x[eq_idx].origin.starts_with("connection equation:") {
            continue;
        }

        // Skip equations containing der(state) — unless the candidate is an
        // output that forms a direct alias, which can be safely eliminated.
        let is_output = dae.outputs.contains_key(&var_name);
        let has_state_derivative = state_names
            .iter()
            .any(|sn| expr_contains_der_of(&dae.f_x[eq_idx].rhs, sn));
        if has_state_derivative && !is_output {
            continue;
        }

        let eq_rhs = apply_substitutions_in_order(&dae.f_x[eq_idx].rhs, &substitutions);

        // Try to solve 0 = rhs for var_name.
        let solution = match try_solve_for_unknown(&eq_rhs, &var_name) {
            Some(expr) => expr,
            None => continue,
        };

        // Verify the solution doesn't reference the variable being eliminated.
        if expr_contains_var(&solution, &var_name) {
            continue;
        }
        if expr_contains_unsliced_multiscalar_ref(&solution, dae) {
            continue;
        }
        if !is_symbolically_stable_solution(&solution) {
            continue;
        }
        // Output variables exist for external callers — only eliminate them
        // when the solution is a trivial alias (a single variable reference or
        // its negation). Eliminating non-trivial outputs in the BLT pass
        // alters the DAE structure enough to cause measurable trace-accuracy
        // regressions in MSL parity tests, so prefer keeping them in the DAE.
        if is_output && !is_trivial_alias(&solution) {
            continue;
        }

        // Record substitution.
        substitutions.push(Substitution {
            var_name: var_name.clone(),
            expr: solution.clone(),
            env_keys: vec![var_name.as_str().to_string()],
        });
        eliminated_eq_indices.push(eq_idx);
        eliminated_eq_flags[eq_idx] = true;
        eliminated_var_names.push(var_name.clone());
    }

    // Apply BLT substitutions once to the remaining equations.
    apply_substitutions_to_remaining_once(dae, &eliminated_eq_flags, &substitutions);

    let n_eliminated = eliminated_eq_indices.len();

    // Remove eliminated equations (in reverse order to preserve indices).
    eliminated_eq_indices.sort_unstable();
    for &idx in eliminated_eq_indices.iter().rev() {
        dae.f_x.remove(idx);
    }

    // Remove eliminated variables from algebraics and outputs.
    for name in &eliminated_var_names {
        dae.algebraics.shift_remove(name);
        dae.outputs.shift_remove(name);
    }

    EliminationResult {
        substitutions,
        n_eliminated,
    }
}

fn runtime_protected_unknown_names(dae: &Dae) -> HashSet<String> {
    let mut protected = rumoca_eval_dae::analysis::runtime_defined_continuous_unknown_names(dae);
    protected.extend(branch_local_analog_protected_unknown_names(dae));
    protected.extend(clocked_value_source_protected_unknown_names(dae));
    protected
}

fn runtime_defined_discrete_target_names(dae: &Dae) -> HashSet<String> {
    let mut targets = HashSet::default();
    for lhs in dae
        .f_m
        .iter()
        .chain(dae.f_z.iter())
        .filter_map(|eq| eq.lhs.as_ref())
    {
        targets.insert(lhs.as_str().to_string());
        if let Some(base) = dae::component_base_name(lhs.as_str()) {
            targets.insert(base);
        }
    }
    targets
}

fn is_runtime_protected_unknown(name: &VarName, protected: &HashSet<String>) -> bool {
    protected.contains(name.as_str())
}

fn branch_local_analog_protected_unknown_names(dae: &Dae) -> HashSet<String> {
    let mut protected = HashSet::new();
    for eq in &dae.f_x {
        if !expr_contains_branch_local_analog_operator(&eq.rhs) {
            continue;
        }

        // MLS §3.3 / §3.7.5: noEvent/smooth preserve the value semantics of
        // the enclosed expression while only changing event generation or
        // differentiability treatment. Keep continuous helper unknowns that
        // appear inside those branch-local analog rows so structural
        // elimination does not rewrite them away into a numerically wider
        // solve than the original Modelica equation system.
        let mut refs = HashSet::new();
        eq.rhs.collect_var_refs(&mut refs);
        for name in refs {
            maybe_protect_branch_local_unknown(dae, &mut protected, &name);
        }
        if let Some(target) = assignment_target_name(&eq.rhs) {
            maybe_protect_branch_local_unknown(dae, &mut protected, &target);
        }
    }
    protected
}

fn clocked_value_source_protected_unknown_names(dae: &Dae) -> HashSet<String> {
    let mut protected = HashSet::new();
    for eq in dae.f_z.iter().chain(dae.f_m.iter()) {
        collect_clocked_value_source_unknowns(dae, &eq.rhs, &mut protected);
    }
    protected
}

fn collect_clocked_value_source_unknowns(
    dae: &Dae,
    expr: &Expression,
    protected: &mut HashSet<String>,
) {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
        } => {
            if let Some(source) = args.first() {
                // MLS §16.5.1: sampled-value equations read the source signal
                // at clock ticks. Keep continuous helper unknowns that feed the
                // sampled value alive so structural elimination does not leave
                // a dangling sampled source in f_z/f_m.
                let mut refs = HashSet::new();
                source.collect_var_refs(&mut refs);
                for name in refs {
                    maybe_protect_branch_local_unknown(dae, protected, &name);
                }
            }
            for arg in args {
                collect_clocked_value_source_unknowns(dae, arg, protected);
            }
        }
        Expression::BuiltinCall { args, .. } => {
            for arg in args {
                collect_clocked_value_source_unknowns(dae, arg, protected);
            }
        }
        Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            if matches!(
                short,
                "hold"
                    | "previous"
                    | "noClock"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
            ) && let Some(source) = args.first()
            {
                let mut refs = HashSet::new();
                source.collect_var_refs(&mut refs);
                for name in refs {
                    maybe_protect_branch_local_unknown(dae, protected, &name);
                }
            }
            for arg in args {
                collect_clocked_value_source_unknowns(dae, arg, protected);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_clocked_value_source_unknowns(dae, lhs, protected);
            collect_clocked_value_source_unknowns(dae, rhs, protected);
        }
        Expression::Unary { rhs, .. } | Expression::FieldAccess { base: rhs, .. } => {
            collect_clocked_value_source_unknowns(dae, rhs, protected);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (condition, value) in branches {
                collect_clocked_value_source_unknowns(dae, condition, protected);
                collect_clocked_value_source_unknowns(dae, value, protected);
            }
            collect_clocked_value_source_unknowns(dae, else_branch, protected);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for element in elements {
                collect_clocked_value_source_unknowns(dae, element, protected);
            }
        }
        Expression::Range { start, step, end } => {
            collect_clocked_value_source_unknowns(dae, start, protected);
            if let Some(step) = step.as_deref() {
                collect_clocked_value_source_unknowns(dae, step, protected);
            }
            collect_clocked_value_source_unknowns(dae, end, protected);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_clocked_value_source_unknowns(dae, expr, protected);
            for index in indices {
                collect_clocked_value_source_unknowns(dae, &index.range, protected);
            }
            if let Some(filter) = filter.as_deref() {
                collect_clocked_value_source_unknowns(dae, filter, protected);
            }
        }
        Expression::Index { base, subscripts } => {
            collect_clocked_value_source_unknowns(dae, base, protected);
            for subscript in subscripts {
                if let dae::Subscript::Expr(expr) = subscript {
                    collect_clocked_value_source_unknowns(dae, expr, protected);
                }
            }
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => {}
    }
}

fn maybe_protect_branch_local_unknown(dae: &Dae, protected: &mut HashSet<String>, name: &VarName) {
    if dae.algebraics.contains_key(name) || dae.outputs.contains_key(name) {
        protected.insert(name.as_str().to_string());
    }

    let Some(base) = dae::component_base_name(name.as_str()) else {
        return;
    };
    let base = VarName::new(base);
    if dae.algebraics.contains_key(&base) || dae.outputs.contains_key(&base) {
        protected.insert(base.as_str().to_string());
    }
}

fn expr_contains_branch_local_analog_operator(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall { function, args } => {
            matches!(function, BuiltinFunction::Smooth | BuiltinFunction::NoEvent)
                || args.iter().any(expr_contains_branch_local_analog_operator)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_branch_local_analog_operator(lhs)
                || expr_contains_branch_local_analog_operator(rhs)
        }
        Expression::Unary { rhs, .. } | Expression::FieldAccess { base: rhs, .. } => {
            expr_contains_branch_local_analog_operator(rhs)
        }
        Expression::FunctionCall { args, .. } => {
            args.iter().any(expr_contains_branch_local_analog_operator)
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_branch_local_analog_operator(condition)
                    || expr_contains_branch_local_analog_operator(value)
            }) || expr_contains_branch_local_analog_operator(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => elements
            .iter()
            .any(expr_contains_branch_local_analog_operator),
        Expression::Range { start, step, end } => {
            expr_contains_branch_local_analog_operator(start)
                || step
                    .as_deref()
                    .is_some_and(expr_contains_branch_local_analog_operator)
                || expr_contains_branch_local_analog_operator(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_branch_local_analog_operator(expr)
                || indices
                    .iter()
                    .any(|idx| expr_contains_branch_local_analog_operator(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(expr_contains_branch_local_analog_operator)
        }
        Expression::Index { base, subscripts } => {
            expr_contains_branch_local_analog_operator(base)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_ir_dae::Subscript::Expr(expr) => {
                        expr_contains_branch_local_analog_operator(expr)
                    }
                    rumoca_ir_dae::Subscript::Index(_) | rumoca_ir_dae::Subscript::Colon => false,
                })
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

fn assignment_target_name(expr: &Expression) -> Option<VarName> {
    let Expression::Binary { op, lhs, rhs } = expr else {
        return None;
    };
    if !matches!(op, OpBinary::Sub(_)) {
        return None;
    }
    if let Expression::VarRef { name, subscripts } = lhs.as_ref()
        && subscripts.is_empty()
    {
        return Some(name.clone());
    }
    if let Expression::VarRef { name, subscripts } = rhs.as_ref()
        && subscripts.is_empty()
    {
        return Some(name.clone());
    }
    None
}

fn runtime_partition_or_event_refs_var(dae: &Dae, var_name: &VarName) -> bool {
    dae.f_z
        .iter()
        .any(|eq| expr_contains_var(&eq.rhs, var_name))
        || dae
            .f_m
            .iter()
            .any(|eq| expr_contains_var(&eq.rhs, var_name))
        || dae
            .f_c
            .iter()
            .any(|eq| expr_contains_var(&eq.rhs, var_name))
        || dae
            .relation
            .iter()
            .any(|expr| expr_contains_var(expr, var_name))
        || dae
            .synthetic_root_conditions
            .iter()
            .any(|expr| expr_contains_var(expr, var_name))
        || dae
            .clock_constructor_exprs
            .iter()
            .any(|expr| expr_contains_var(expr, var_name))
}

fn should_preserve_runtime_known_assignment(dae: &Dae, eq_rhs: &Expression) -> bool {
    let Some(target) = assignment_target_name(eq_rhs) else {
        return false;
    };
    dae.discrete_reals.contains_key(&target)
        || dae.discrete_valued.contains_key(&target)
        || runtime_partition_or_event_refs_var(dae, &target)
}

fn expr_references_any_runtime_discrete_target(
    expr: &Expression,
    runtime_defined_discrete_targets: &HashSet<String>,
) -> bool {
    if runtime_defined_discrete_targets.is_empty() {
        return false;
    }

    let mut refs: HashSet<VarName> = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.iter().any(|name| {
        let raw = name.as_str();
        runtime_defined_discrete_targets.contains(raw)
            || dae::component_base_name(raw)
                .is_some_and(|base| runtime_defined_discrete_targets.contains(base.as_str()))
    })
}

fn expr_references_any_discrete_name(dae: &Dae, expr: &Expression) -> bool {
    let mut refs: HashSet<VarName> = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.iter().any(|name| {
        dae.discrete_reals.contains_key(name)
            || dae.discrete_valued.contains_key(name)
            || dae::component_base_name(name.as_str()).is_some_and(|base| {
                let base = VarName::new(base.as_str());
                dae.discrete_reals.contains_key(&base) || dae.discrete_valued.contains_key(&base)
            })
    })
}

// ── Expression Helpers ──────────────────────────────────────────────────

/// Apply substitutions in-order to an expression.
fn apply_substitutions_in_order(expr: &Expression, substitutions: &[Substitution]) -> Expression {
    let mut out = expr.clone();
    for sub in substitutions {
        if expr_contains_var(&out, &sub.var_name) {
            out = substitute_var(&out, &sub.var_name, &sub.expr);
        }
    }
    out
}

/// Apply all substitutions to non-eliminated equations in one sweep.
fn apply_substitutions_to_remaining_once(
    dae: &mut Dae,
    eliminated_eq_flags: &[bool],
    substitutions: &[Substitution],
) {
    if substitutions.is_empty() {
        return;
    }
    for (i, eq) in dae.f_x.iter_mut().enumerate() {
        if eliminated_eq_flags.get(i).copied().unwrap_or(false) {
            continue;
        }
        eq.rhs = apply_substitutions_in_order(&eq.rhs, substitutions);
    }
}

fn normalize_unknown_for_dae(dae: &Dae, unknown: &VarName) -> VarName {
    if dae.algebraics.contains_key(unknown) || dae.outputs.contains_key(unknown) {
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
        .algebraics
        .get(&base_name)
        .or_else(|| dae.outputs.get(&base_name))
        .is_some_and(|var| var.size() == 1);
    if is_singleton {
        base_name
    } else {
        unknown.clone()
    }
}

fn expr_contains_unsliced_multiscalar_ref(expr: &Expression, dae: &Dae) -> bool {
    match expr {
        Expression::VarRef { name, subscripts } => {
            if !subscripts.is_empty() {
                return subscripts.iter().any(|subscript| match subscript {
                    rumoca_ir_dae::Subscript::Expr(expr) => {
                        expr_contains_unsliced_multiscalar_ref(expr, dae)
                    }
                    _ => false,
                });
            }
            let size = dae
                .states
                .get(name)
                .or_else(|| dae.algebraics.get(name))
                .or_else(|| dae.outputs.get(name))
                .map(|v| v.size())
                .unwrap_or(0);
            size > 1
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_unsliced_multiscalar_ref(lhs, dae)
                || expr_contains_unsliced_multiscalar_ref(rhs, dae)
        }
        Expression::Unary { rhs, .. } => expr_contains_unsliced_multiscalar_ref(rhs, dae),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| expr_contains_unsliced_multiscalar_ref(arg, dae)),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_unsliced_multiscalar_ref(condition, dae)
                    || expr_contains_unsliced_multiscalar_ref(value, dae)
            }) || expr_contains_unsliced_multiscalar_ref(else_branch, dae)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => elements
            .iter()
            .any(|element| expr_contains_unsliced_multiscalar_ref(element, dae)),
        Expression::Range { start, step, end } => {
            expr_contains_unsliced_multiscalar_ref(start, dae)
                || step
                    .as_deref()
                    .is_some_and(|step| expr_contains_unsliced_multiscalar_ref(step, dae))
                || expr_contains_unsliced_multiscalar_ref(end, dae)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            indices
                .iter()
                .any(|idx| expr_contains_unsliced_multiscalar_ref(&idx.range, dae))
                || expr_contains_unsliced_multiscalar_ref(expr, dae)
                || filter
                    .as_deref()
                    .is_some_and(|pred| expr_contains_unsliced_multiscalar_ref(pred, dae))
        }
        Expression::Index { base, subscripts } => {
            expr_contains_unsliced_multiscalar_ref(base, dae)
                || subscripts.iter().any(|subscript| match subscript {
                    rumoca_ir_dae::Subscript::Expr(expr) => {
                        expr_contains_unsliced_multiscalar_ref(expr, dae)
                    }
                    _ => false,
                })
        }
        Expression::FieldAccess { base, .. } => expr_contains_unsliced_multiscalar_ref(base, dae),
        Expression::Literal(_) | Expression::Empty => false,
    }
}

/// Try to solve `0 = rhs` for `unknown` symbolically.
///
/// Handles the common residual patterns produced by todae:
/// - `0 = z`              -> `z = 0`
/// - `0 = z - expr`       -> `z = expr`
/// - `0 = expr - z`       -> `z = expr`
/// - `0 = -(z - expr)`    -> `z = expr`
/// - `0 = -(expr - z)`    -> `z = expr`
pub fn try_solve_for_unknown(rhs: &Expression, unknown: &VarName) -> Option<Expression> {
    match rhs {
        // Pattern: 0 = z  ->  z = 0
        Expression::VarRef { name, subscripts } if name == unknown && subscripts.is_empty() => {
            Some(Expression::Literal(rumoca_ir_dae::Literal::Real(0.0)))
        }
        // Pattern: 0 = lhs - rhs_inner (Binary Sub)
        Expression::Binary {
            op: OpBinary::Sub(_),
            lhs,
            rhs: rhs_inner,
        } => {
            // 0 = z - expr -> z = expr
            if is_var_ref(lhs, unknown) && !expr_contains_var(rhs_inner, unknown) {
                return Some(*rhs_inner.clone());
            }
            // 0 = expr - z -> z = expr
            if is_var_ref(rhs_inner, unknown) && !expr_contains_var(lhs, unknown) {
                return Some(*lhs.clone());
            }
            None
        }
        // Pattern: 0 = -(something) (Unary Minus)
        Expression::Unary {
            op: OpUnary::Minus(_),
            rhs: inner,
        } => {
            // Recurse into the negated expression.
            // -(z - expr) has the same solutions as (z - expr).
            try_solve_for_unknown(inner, unknown)
        }
        // Pattern: 0 = a + b + c + ... (additive form, e.g. connection equations)
        // Handled by try_solve_additive_for_unknown() which requires the live
        // unknown set to avoid solving 2-unknown equations incorrectly.
        // This base function does NOT handle additive forms — callers that want
        // additive solving should use try_solve_additive_for_unknown() directly.
        _ => None,
    }
}

/// Try to solve an additive equation `0 = a + b + c` for `unknown`, but only
/// when exactly one term contains the unknown AND no other term contains a
/// different live unknown (to avoid solving 2-unknown equations).
pub fn try_solve_additive_for_unknown(
    rhs: &Expression,
    unknown: &VarName,
    live_unknowns: &[VarName],
) -> Option<Expression> {
    let terms = flatten_additive_terms(rhs);
    if terms.len() < 2 {
        return None;
    }

    // Find which term(s) contain the target unknown
    let mut unknown_idx = None;
    for (i, (_, term)) in terms.iter().enumerate() {
        if expr_contains_var(term, unknown) {
            if unknown_idx.is_some() {
                return None; // multiple terms contain the unknown
            }
            unknown_idx = Some(i);
        }
    }
    let unknown_idx = unknown_idx?;

    // The unknown term must be a bare VarRef (linear, coefficient = 1)
    let (unknown_positive, unknown_term) = terms[unknown_idx];
    if !is_var_ref(unknown_term, unknown) {
        return None;
    }

    // Check that no OTHER term contains a DIFFERENT live unknown
    for (i, (_, term)) in terms.iter().enumerate() {
        if i == unknown_idx {
            continue;
        }
        for other_unknown in live_unknowns {
            if other_unknown != unknown && expr_contains_var(term, other_unknown) {
                return None; // another term has a different live unknown
            }
        }
    }

    // Safe to solve: build -(other_terms) or other_terms
    let other_terms: Vec<(bool, &Expression)> = terms
        .iter()
        .enumerate()
        .filter(|(i, _)| *i != unknown_idx)
        .map(|(_, &(sign, term))| {
            if unknown_positive {
                (!sign, term)
            } else {
                (sign, term)
            }
        })
        .collect();

    build_sum_expr(&other_terms)
}

/// Flatten a tree of Add/Sub operations into signed terms.
/// E.g., `a + b - c + d` → [(+, a), (+, b), (-, c), (+, d)]
fn flatten_additive_terms(expr: &Expression) -> Vec<(bool, &Expression)> {
    match expr {
        Expression::Binary {
            op: OpBinary::Add(_),
            lhs,
            rhs,
        } => {
            let mut terms = flatten_additive_terms(lhs);
            terms.extend(flatten_additive_terms(rhs));
            terms
        }
        Expression::Binary {
            op: OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            let mut terms = flatten_additive_terms(lhs);
            // Negate all terms from the RHS
            for (sign, term) in flatten_additive_terms(rhs) {
                terms.push((!sign, term));
            }
            terms
        }
        Expression::Unary {
            op: OpUnary::Minus(_),
            rhs: inner,
        } => flatten_additive_terms(inner)
            .into_iter()
            .map(|(sign, term)| (!sign, term))
            .collect(),
        _ => vec![(true, expr)],
    }
}

/// Build an Expression from a list of signed terms.
fn build_sum_expr(terms: &[(bool, &Expression)]) -> Option<Expression> {
    if terms.is_empty() {
        return Some(Expression::Literal(rumoca_ir_dae::Literal::Real(0.0)));
    }
    if terms.len() == 1 {
        let (positive, expr) = terms[0];
        return if positive {
            Some(expr.clone())
        } else {
            Some(Expression::Unary {
                op: OpUnary::Minus(Default::default()),
                rhs: Box::new(expr.clone()),
            })
        };
    }

    // Start with the first term
    let (first_positive, first_expr) = terms[0];
    let mut result = if first_positive {
        first_expr.clone()
    } else {
        Expression::Unary {
            op: OpUnary::Minus(Default::default()),
            rhs: Box::new(first_expr.clone()),
        }
    };

    // Add remaining terms
    for &(positive, term) in &terms[1..] {
        if positive {
            result = Expression::Binary {
                op: OpBinary::Add(Default::default()),
                lhs: Box::new(result),
                rhs: Box::new(term.clone()),
            };
        } else {
            result = Expression::Binary {
                op: OpBinary::Sub(Default::default()),
                lhs: Box::new(result),
                rhs: Box::new(term.clone()),
            };
        }
    }

    Some(result)
}

/// Check if an expression contains `der(var_name)`.
pub(crate) fn expr_contains_der_of(expr: &Expression, var_name: &VarName) -> bool {
    match expr {
        Expression::BuiltinCall {
            function: rumoca_ir_dae::BuiltinFunction::Der,
            args,
        } => {
            if args
                .first()
                .is_some_and(|arg| expr_refers_to_var_base(arg, var_name))
            {
                return true;
            }
            args.iter().any(|a| expr_contains_der_of(a, var_name))
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_der_of(lhs, var_name) || expr_contains_der_of(rhs, var_name)
        }
        Expression::Unary { rhs, .. } => expr_contains_der_of(rhs, var_name),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(|a| expr_contains_der_of(a, var_name))
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(c, v)| {
                expr_contains_der_of(c, var_name) || expr_contains_der_of(v, var_name)
            }) || expr_contains_der_of(else_branch, var_name)
        }
        _ => false,
    }
}

fn expr_refers_to_var_base(expr: &Expression, var_name: &VarName) -> bool {
    match expr {
        Expression::VarRef { name, .. } => {
            let Some(name_base) = dae::component_base_name(name.as_str()) else {
                return false;
            };
            let Some(var_base) = dae::component_base_name(var_name.as_str()) else {
                return false;
            };
            name_base == var_base
        }
        Expression::Index { base, .. } => expr_refers_to_var_base(base, var_name),
        _ => false,
    }
}

fn parse_embedded_subscripts(name: &str) -> Option<Vec<i64>> {
    let mut indices = Vec::new();
    let mut depth = 0usize;
    let mut current = String::new();
    let mut saw_subscript = false;

    for ch in name.chars() {
        match ch {
            '[' => {
                depth += 1;
                if depth == 1 {
                    current.clear();
                    saw_subscript = true;
                } else {
                    current.push(ch);
                }
            }
            ']' => {
                if depth == 1 {
                    let trimmed = current.trim();
                    let idx = parse_subscript_index(trimmed)?;
                    indices.push(idx);
                    current.clear();
                } else if depth > 1 {
                    current.push(ch);
                }
                depth = depth.saturating_sub(1);
            }
            _ if depth >= 1 => current.push(ch),
            _ => {}
        }
    }

    (saw_subscript && depth == 0).then_some(indices)
}

fn parse_subscript_index(text: &str) -> Option<i64> {
    text.parse::<i64>().ok().or_else(|| {
        text.parse::<f64>()
            .ok()
            .filter(|v| v.is_finite() && v.fract() == 0.0)
            .map(|v| v as i64)
    })
}

fn subscripts_all_one(subscripts: &[rumoca_ir_dae::Subscript]) -> bool {
    !subscripts.is_empty()
        && subscripts.iter().all(|sub| match sub {
            rumoca_ir_dae::Subscript::Index(i) => *i == 1,
            rumoca_ir_dae::Subscript::Expr(expr) => match expr.as_ref() {
                Expression::Literal(rumoca_ir_dae::Literal::Integer(i)) => *i == 1,
                Expression::Literal(rumoca_ir_dae::Literal::Real(v))
                    if v.is_finite() && v.fract() == 0.0 =>
                {
                    (*v as i64) == 1
                }
                _ => false,
            },
            rumoca_ir_dae::Subscript::Colon => false,
        })
}

fn embedded_subscripts_all_one(name: &str) -> bool {
    parse_embedded_subscripts(name)
        .is_some_and(|indices| !indices.is_empty() && indices.iter().all(|i| *i == 1))
}

fn subscripts_match_indices(subscripts: &[rumoca_ir_dae::Subscript], expected: &[i64]) -> bool {
    if subscripts.len() != expected.len() || subscripts.is_empty() {
        return false;
    }
    subscripts
        .iter()
        .zip(expected.iter())
        .all(|(sub, expected_idx)| match sub {
            rumoca_ir_dae::Subscript::Index(i) => *i == *expected_idx,
            rumoca_ir_dae::Subscript::Expr(expr) => match expr.as_ref() {
                Expression::Literal(rumoca_ir_dae::Literal::Integer(i)) => *i == *expected_idx,
                Expression::Literal(rumoca_ir_dae::Literal::Real(v))
                    if v.is_finite() && v.fract() == 0.0 =>
                {
                    (*v as i64) == *expected_idx
                }
                _ => false,
            },
            rumoca_ir_dae::Subscript::Colon => false,
        })
}

fn split_complex_field_suffix(name: &str) -> Option<(&str, &str)> {
    let (base, field) = name.rsplit_once('.')?;
    matches!(field, "re" | "im").then_some((base, field))
}

fn complex_base_alias_match(base_or_field: &str, other: &str) -> bool {
    split_complex_field_suffix(base_or_field).is_some_and(|(base, _)| base == other)
        || split_complex_field_suffix(other).is_some_and(|(base, _)| base == base_or_field)
}

fn var_ref_matches_unknown(
    name: &VarName,
    subscripts: &[rumoca_ir_dae::Subscript],
    unknown: &VarName,
) -> bool {
    if name == unknown {
        return subscripts.is_empty() || subscripts_all_one(subscripts);
    }
    if subscripts.is_empty() && complex_base_alias_match(name.as_str(), unknown.as_str()) {
        return true;
    }
    let Some(name_base) = dae::component_base_name(name.as_str()) else {
        return false;
    };
    let Some(unknown_base) = dae::component_base_name(unknown.as_str()) else {
        return false;
    };
    if complex_base_alias_match(&name_base, &unknown_base) {
        return true;
    }
    if name_base != unknown_base {
        return false;
    }
    if !subscripts.is_empty() {
        if let Some(indices) = parse_embedded_subscripts(unknown.as_str())
            && subscripts_match_indices(subscripts, &indices)
        {
            return true;
        }
        return false;
    }

    let name_has_embedded = name.as_str().contains('[');
    let unknown_has_embedded = unknown.as_str().contains('[');
    if name_has_embedded != unknown_has_embedded {
        let embedded_name = if name_has_embedded {
            name.as_str()
        } else {
            unknown.as_str()
        };
        if embedded_subscripts_all_one(embedded_name) {
            return true;
        }
        return false;
    }
    if name_has_embedded {
        return name.as_str() == unknown.as_str();
    }
    true
}

fn var_ref_matches_unknown_for_substitution(
    name: &VarName,
    subscripts: &[rumoca_ir_dae::Subscript],
    unknown: &VarName,
) -> bool {
    let name_field = split_complex_field_suffix(name.as_str());
    let unknown_field = split_complex_field_suffix(unknown.as_str());

    // Substitution must preserve complex field semantics: do not allow
    // base<->field alias matching here, otherwise `.re/.im` projections can be
    // applied to already-scalar replacement expressions.
    if name_field.is_some() || unknown_field.is_some() {
        return name == unknown && (subscripts.is_empty() || subscripts_all_one(subscripts));
    }

    var_ref_matches_unknown(name, subscripts, unknown)
}

/// Check if an expression is a simple VarRef to the given variable.
fn is_var_ref(expr: &Expression, var: &VarName) -> bool {
    match expr {
        Expression::VarRef { name, subscripts } => {
            var_ref_matches_unknown_for_substitution(name, subscripts, var)
        }
        _ => false,
    }
}

/// Check if an expression references a variable (by base name).
pub fn expr_contains_var(expr: &Expression, var: &VarName) -> bool {
    match expr {
        Expression::VarRef { name, subscripts } => {
            if var_ref_matches_unknown(name, subscripts, var) {
                return true;
            }
            subscripts.iter().any(|s| match s {
                rumoca_ir_dae::Subscript::Expr(e) => expr_contains_var(e, var),
                _ => false,
            })
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_var(lhs, var) || expr_contains_var(rhs, var)
        }
        Expression::Unary { rhs, .. } => expr_contains_var(rhs, var),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(|a| expr_contains_var(a, var))
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches
                .iter()
                .any(|(c, v)| expr_contains_var(c, var) || expr_contains_var(v, var))
                || expr_contains_var(else_branch, var)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(|e| expr_contains_var(e, var))
        }
        Expression::Range { start, step, end } => {
            expr_contains_var(start, var)
                || step.as_ref().is_some_and(|s| expr_contains_var(s, var))
                || expr_contains_var(end, var)
        }
        Expression::Index { base, subscripts } => {
            expr_contains_var(base, var)
                || subscripts.iter().any(|s| match s {
                    rumoca_ir_dae::Subscript::Expr(e) => expr_contains_var(e, var),
                    _ => false,
                })
        }
        Expression::ArrayComprehension { expr, filter, .. } => {
            expr_contains_var(expr, var)
                || filter.as_ref().is_some_and(|f| expr_contains_var(f, var))
        }
        Expression::FieldAccess { base, .. } => expr_contains_var(base, var),
        Expression::Literal(_) | Expression::Empty => false,
    }
}

fn substitute_expr_list(
    exprs: &[Expression],
    var: &VarName,
    replacement: &Expression,
) -> Vec<Expression> {
    exprs
        .iter()
        .map(|expr| substitute_var(expr, var, replacement))
        .collect()
}

fn substitute_subscripts(
    subscripts: &[rumoca_ir_dae::Subscript],
    var: &VarName,
    replacement: &Expression,
) -> Vec<rumoca_ir_dae::Subscript> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_ir_dae::Subscript::Expr(expr) => {
                rumoca_ir_dae::Subscript::Expr(Box::new(substitute_var(expr, var, replacement)))
            }
            other => other.clone(),
        })
        .collect()
}

/// Replace all occurrences of `var` in `expr` with `replacement`.
pub(crate) fn substitute_var(
    expr: &Expression,
    var: &VarName,
    replacement: &Expression,
) -> Expression {
    match expr {
        Expression::VarRef { name, subscripts }
            if var_ref_matches_unknown_for_substitution(name, subscripts, var) =>
        {
            replacement.clone()
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => expr.clone(),
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(substitute_var(lhs, var, replacement)),
            rhs: Box::new(substitute_var(rhs, var, replacement)),
        },
        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(substitute_var(rhs, var, replacement)),
        },
        Expression::BuiltinCall { function, args } => {
            if matches!(
                function,
                BuiltinFunction::Pre | BuiltinFunction::Edge | BuiltinFunction::Change
            ) {
                // Preserve event-operator arguments to maintain MLS Appendix B
                // pre/change/edge semantics during symbolic substitution.
                Expression::BuiltinCall {
                    function: *function,
                    args: args.clone(),
                }
            } else {
                Expression::BuiltinCall {
                    function: *function,
                    args: substitute_expr_list(args, var, replacement),
                }
            }
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => Expression::FunctionCall {
            name: name.clone(),
            args: substitute_expr_list(args, var, replacement),
            is_constructor: *is_constructor,
        },
        Expression::If {
            branches,
            else_branch,
        } => Expression::If {
            branches: branches
                .iter()
                .map(|(c, v)| {
                    (
                        substitute_var(c, var, replacement),
                        substitute_var(v, var, replacement),
                    )
                })
                .collect(),
            else_branch: Box::new(substitute_var(else_branch, var, replacement)),
        },
        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: substitute_expr_list(elements, var, replacement),
            is_matrix: *is_matrix,
        },
        Expression::Tuple { elements } => Expression::Tuple {
            elements: substitute_expr_list(elements, var, replacement),
        },
        Expression::Range { start, step, end } => Expression::Range {
            start: Box::new(substitute_var(start, var, replacement)),
            step: step
                .as_ref()
                .map(|s| Box::new(substitute_var(s, var, replacement))),
            end: Box::new(substitute_var(end, var, replacement)),
        },
        Expression::Index { base, subscripts } => Expression::Index {
            base: Box::new(substitute_var(base, var, replacement)),
            subscripts: substitute_subscripts(subscripts, var, replacement),
        },
        Expression::ArrayComprehension {
            expr: inner,
            indices,
            filter,
        } => Expression::ArrayComprehension {
            expr: Box::new(substitute_var(inner, var, replacement)),
            indices: indices.clone(),
            filter: filter
                .as_ref()
                .map(|f| Box::new(substitute_var(f, var, replacement))),
        },
        Expression::FieldAccess { base, field } => Expression::FieldAccess {
            base: Box::new(substitute_var(base, var, replacement)),
            field: field.clone(),
        },
    }
}

#[cfg(test)]
mod tests;
