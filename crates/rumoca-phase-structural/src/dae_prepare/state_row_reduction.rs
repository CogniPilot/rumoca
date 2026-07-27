use super::*;
use crate::static_eval::{eval_static_bool, structural_scalar_bindings};

fn state_has_any_equation_reference(dae: &Dae, state_name: &VarName) -> bool {
    dae.continuous
        .equations
        .iter()
        .any(|eq| expr_contains_var(&eq.rhs, state_name))
}

fn state_has_any_derivative_reference(dae: &Dae, state_name: &VarName) -> bool {
    let bindings = structural_scalar_bindings(dae);
    let matcher = DerivativeNameMatcher::from_var_names([state_name]);
    dae.continuous
        .equations
        .iter()
        .any(|eq| expr_contains_active_derivative(&eq.rhs, &matcher, &bindings))
}

fn try_match_state_to_row(
    state_idx: usize,
    state_to_rows: &[Vec<usize>],
    row_to_state: &mut [Option<usize>],
    seen_rows: &mut [bool],
) -> bool {
    for &row_idx in &state_to_rows[state_idx] {
        if seen_rows[row_idx] {
            continue;
        }
        seen_rows[row_idx] = true;
        if let Some(other_state_idx) = row_to_state[row_idx] {
            if try_match_state_to_row(other_state_idx, state_to_rows, row_to_state, seen_rows) {
                row_to_state[row_idx] = Some(state_idx);
                return true;
            }
            continue;
        }
        row_to_state[row_idx] = Some(state_idx);
        return true;
    }
    false
}

fn states_with_assignable_derivative_rows(dae: &Dae, state_names: &[VarName]) -> HashSet<usize> {
    let bindings = structural_scalar_bindings(dae);
    let all_state_matcher = DerivativeNameMatcher::from_var_names(state_names);
    let state_to_rows: Vec<Vec<usize>> = state_names
        .iter()
        .map(|state_name| {
            let state_matcher = DerivativeNameMatcher::from_var_names([state_name]);
            dae.continuous
                .equations
                .iter()
                .enumerate()
                .filter_map(|(row_idx, eq)| {
                    if !state_derivative_row_is_assignable(
                        eq,
                        state_name,
                        &state_matcher,
                        &all_state_matcher,
                        &bindings,
                    ) {
                        return None;
                    }
                    if let Some(alias) = try_extract_derivative_alias(eq, state_name)
                        && dae.variables.states.contains_key(&alias)
                    {
                        return Some(row_idx);
                    }
                    Some(row_idx)
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut state_order: Vec<usize> = (0..state_names.len()).collect();
    state_order.sort_by_key(|idx| state_to_rows[*idx].len());

    let mut row_to_state: Vec<Option<usize>> = vec![None; dae.continuous.equations.len()];
    for state_idx in state_order {
        if state_to_rows[state_idx].is_empty() {
            continue;
        }
        let mut seen_rows = vec![false; dae.continuous.equations.len()];
        let _ =
            try_match_state_to_row(state_idx, &state_to_rows, &mut row_to_state, &mut seen_rows);
    }

    row_to_state.into_iter().flatten().collect()
}

fn state_derivative_row_is_assignable(
    eq: &Equation,
    state_name: &VarName,
    state_matcher: &DerivativeNameMatcher,
    all_state_matcher: &DerivativeNameMatcher,
    bindings: &HashMap<String, f64>,
) -> bool {
    if !all_active_derivative_args_are_states(&eq.rhs, all_state_matcher, bindings)
        || !expr_contains_active_derivative(&eq.rhs, state_matcher, bindings)
    {
        return false;
    }
    if let Some(value) = try_extract_der_value(&eq.rhs, state_name)
        && expr_contains_active_derivative(&value, state_matcher, bindings)
    {
        return false;
    }
    true
}

pub(super) fn expression_is_smooth_for_index_reduction(
    expr: &Expression,
    dae: &Dae,
    bindings: &HashMap<String, f64>,
) -> bool {
    let mut checker = IndexReductionSmoothness {
        dae,
        bindings,
        smooth: true,
    };
    checker.visit_expression(expr);
    checker.smooth
}

struct IndexReductionSmoothness<'a> {
    dae: &'a Dae,
    bindings: &'a HashMap<String, f64>,
    smooth: bool,
}

impl ExpressionVisitor for IndexReductionSmoothness<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if self.smooth {
            self.walk_expression(expr);
        }
    }

    fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
        let name = name.var_name();
        if self.dae.variables.discrete_reals.contains_key(name)
            || self.dae.variables.discrete_valued.contains_key(name)
        {
            self.smooth = false;
            return;
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
        for (condition, value) in branches {
            match eval_static_bool(condition, self.bindings) {
                Some(true) => {
                    self.visit_expression(value);
                    return;
                }
                Some(false) => continue,
                None if condition_is_time_invariant(condition, self.dae) => {
                    self.visit_expression(value);
                }
                None => {
                    self.smooth = false;
                    return;
                }
            }
        }
        self.visit_expression(else_branch);
    }
}

fn condition_is_time_invariant(condition: &Expression, dae: &Dae) -> bool {
    let mut references = Vec::new();
    condition.collect_var_refs(&mut references);
    references.into_iter().all(|name| {
        dae.variables.parameters.contains_key(&name) || dae.variables.constants.contains_key(&name)
    })
}

fn expr_contains_active_derivative(
    expr: &Expression,
    matcher: &DerivativeNameMatcher,
    bindings: &HashMap<String, f64>,
) -> bool {
    let mut checker = ActiveDerivativeChecker {
        matcher,
        bindings,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct ActiveDerivativeChecker<'a> {
    matcher: &'a DerivativeNameMatcher,
    bindings: &'a HashMap<String, f64>,
    found: bool,
}

impl ExpressionVisitor for ActiveDerivativeChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der
            && args
                .first()
                .is_some_and(|arg| self.matcher.expression_refers_to_match(arg))
        {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
        for (condition, value) in branches {
            match eval_static_bool(condition, self.bindings) {
                Some(true) => {
                    self.visit_expression(value);
                    return;
                }
                Some(false) => continue,
                None => {
                    self.visit_expression(condition);
                    self.visit_expression(value);
                }
            }
        }
        self.visit_expression(else_branch);
    }
}

fn all_active_derivative_args_are_states(
    expr: &Expression,
    state_matcher: &DerivativeNameMatcher,
    bindings: &HashMap<String, f64>,
) -> bool {
    let mut checker = DerivativeArgsAreStatesChecker {
        state_matcher,
        bindings,
        all_are_states: true,
    };
    checker.visit_expression(expr);
    checker.all_are_states
}

struct DerivativeArgsAreStatesChecker<'a> {
    state_matcher: &'a DerivativeNameMatcher,
    bindings: &'a HashMap<String, f64>,
    all_are_states: bool,
}

impl ExpressionVisitor for DerivativeArgsAreStatesChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if self.all_are_states {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der {
            let is_state_derivative = args
                .first()
                .is_some_and(|arg| self.state_matcher.expression_refers_to_match(arg));
            if !is_state_derivative {
                self.all_are_states = false;
            }
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
        for (condition, value) in branches {
            match eval_static_bool(condition, self.bindings) {
                Some(true) => {
                    self.visit_expression(value);
                    return;
                }
                Some(false) => continue,
                None => {
                    self.visit_expression(condition);
                    self.visit_expression(value);
                }
            }
        }
        self.visit_expression(else_branch);
    }
}

pub(super) fn expression_exact_name(expr: &Expression) -> Option<String> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => append_subscripts(name.as_str().to_string(), subscripts),
        Expression::Index {
            base, subscripts, ..
        } => {
            let base_name = expression_exact_name(base)?;
            append_subscripts(base_name, subscripts)
        }
        Expression::FieldAccess { base, field, .. } => {
            let base_name = expression_exact_name(base)?;
            Some(format!("{base_name}.{field}"))
        }
        _ => None,
    }
}

fn append_subscripts(base: String, subscripts: &[Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(base);
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        indices.push(subscript_index_text(subscript)?);
    }
    Some(format!("{base}[{}]", indices.join(",")))
}

fn subscript_index_text(subscript: &Subscript) -> Option<String> {
    match subscript {
        Subscript::Index { value, .. } => Some(value.to_string()),
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: Literal::Integer(value),
                ..
            } => Some(value.to_string()),
            Expression::Literal {
                value: Literal::Real(value),
                ..
            } if value.is_finite() && value.fract() == 0.0 => Some((*value as i64).to_string()),
            _ => None,
        },
        Subscript::Colon { .. } => None,
    }
}

/// Demote states that are no longer referenced by any continuous equation.
///
/// Trivial elimination may remove an alias/binding equation that was the only
/// remaining reference to a misclassified state-like variable. Such orphan
/// states cannot have valid ODE rows and should be treated as algebraics.
pub fn demote_orphan_states_without_equation_refs(dae: &mut Dae) -> usize {
    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    let mut demoted = 0usize;
    for name in state_names {
        if state_has_any_equation_reference(dae, &name) {
            continue;
        }
        if let Some(var) = dae.variables.states.shift_remove(&name) {
            dae.variables.algebraics.insert(name, var);
            demoted += 1;
        }
    }
    demoted
}

/// Demote state variables that have no `der(state)` occurrence in any equation.
///
/// Promotion of algebraics used in `der(...)` expressions can temporarily mark
/// variables as states even if later structural passes remove all derivative
/// occurrences for that variable. Such variables cannot be solved as states and
/// must remain algebraic.
pub fn demote_states_without_derivative_refs(dae: &mut Dae) -> usize {
    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    let mut demoted = 0usize;
    for name in state_names {
        if state_has_any_derivative_reference(dae, &name) {
            continue;
        }
        if sim_trace_enabled() {
            crate::structural_trace!(
                "[sim-trace] demoting state without derivative refs: {}",
                name.as_str()
            );
        }
        if let Some(var) = dae.variables.states.shift_remove(&name) {
            dae.variables.algebraics.insert(name, var);
            demoted += 1;
        }
    }
    demoted
}

/// Demote states that cannot be assigned a unique derivative row.
///
/// The simulator's ODE row ordering needs at least one assignable derivative
/// equation per retained state. We compute a maximum bipartite matching between
/// states and derivative-bearing rows; unmatched states are demoted to
/// algebraics. Each demotion round immediately normalizes derivatives of those
/// newly algebraic targets before computing another matching: otherwise one
/// legitimate dummy-state demotion can make a coupled derivative row appear
/// unusable and incorrectly cascade into demoting every state in that row.
pub fn demote_states_without_assignable_derivative_rows(
    dae: &mut Dae,
) -> Result<usize, StructuralError> {
    let mut reduced = super::copy_accounting::clone_dae(dae);
    let total_demoted = demote_states_without_assignable_derivative_rows_in_place(&mut reduced)?;
    *dae = reduced;
    Ok(total_demoted)
}

fn demote_states_without_assignable_derivative_rows_in_place(
    dae: &mut Dae,
) -> Result<usize, StructuralError> {
    let mut total_demoted = 0usize;

    loop {
        let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
        if state_names.is_empty() {
            break;
        }

        let matched_states = states_with_assignable_derivative_rows(dae, &state_names);
        let to_demote: Vec<VarName> = state_names
            .iter()
            .enumerate()
            .filter_map(|(idx, name)| (!matched_states.contains(&idx)).then_some(name.clone()))
            .collect();

        if to_demote.is_empty() {
            break;
        }

        let mut demoted_this_round = 0usize;
        for name in to_demote {
            if sim_trace_enabled() {
                crate::structural_trace!(
                    "[sim-trace] demoting state without assignable derivative row: {}",
                    name.as_str()
                );
            }
            if let Some(var) = dae.variables.states.shift_remove(&name) {
                dae.variables.algebraics.insert(name, var);
                demoted_this_round += 1;
            }
        }
        if demoted_this_round == 0 {
            break;
        }
        total_demoted += demoted_this_round;

        // Reclassifying a state changes the meaning of every derivative of that
        // target. Resolve differentiable algebraic definitions now, before the
        // next matching round examines coupled derivative rows. If no smooth
        // definition exists, report the invalid IR contract instead of
        // cascading the partition damage to otherwise assignable states.
        expand_compound_derivatives(dae);
        validate_derivatives_reference_retained_states(dae)?;
    }

    Ok(total_demoted)
}

/// Final state cleanup after late prepare passes that can remove continuous rows.
///
/// MLS Appendix B / SPEC_0003: retained states require retained derivative
/// rows. This combines the existing no-derivative and no-assignable-row
/// demotions without adding logging, timeout, or backend policy.
pub fn demote_states_without_retained_derivative_rows(
    dae: &mut Dae,
) -> Result<(usize, usize), StructuralError> {
    let n_no_derivative_refs = demote_states_without_derivative_refs(dae);
    let n_unassignable_derivative_rows = demote_states_without_assignable_derivative_rows(dae)?;
    // Any earlier prepare rewrite can also have changed the state partition
    // before reaching this final cleanup. Normalize every `der(target)` against
    // the current partition here even when this invocation did not itself
    // demote a state. Algebraics with a smooth retained definition expand by
    // the chain rule; genuinely unresolved derivatives are rejected by the
    // validation below instead of crossing into matching or Solve lowering.
    expand_compound_derivatives(dae);
    validate_derivatives_reference_retained_states(dae)?;
    Ok((n_no_derivative_refs, n_unassignable_derivative_rows))
}

fn validate_derivatives_reference_retained_states(dae: &Dae) -> Result<(), StructuralError> {
    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    let state_matcher = DerivativeNameMatcher::from_var_names(&state_names);
    let mut checker = NonStateDerivativeChecker {
        state_matcher: &state_matcher,
        found: None,
    };
    rumoca_ir_dae::DaeVisitor::visit_dae(&mut checker, dae);
    let Some((name, span)) = checker.found else {
        return Ok(());
    };
    let reason = format!(
        "derivative reference `der({name})` survived after its target was removed from the state \
         partition"
    );
    match span {
        Some(span) if !span.is_dummy() => Err(StructuralError::ContractViolation { reason, span }),
        Some(_) | None => Err(StructuralError::UnspannedContractViolation { reason }),
    }
}

struct NonStateDerivativeChecker<'a> {
    state_matcher: &'a DerivativeNameMatcher,
    found: Option<(String, Option<rumoca_core::Span>)>,
}

impl rumoca_ir_dae::DaeVisitor for NonStateDerivativeChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        ExpressionVisitor::visit_expression(self, expr);
    }
}

impl ExpressionVisitor for NonStateDerivativeChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if self.found.is_some() {
            return;
        }
        if let Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } = expr
        {
            let target = args.first();
            let retained =
                target.is_some_and(|target| self.state_matcher.expression_refers_to_match(target));
            if !retained {
                self.found = Some((
                    target
                        .and_then(expression_exact_name)
                        .unwrap_or_else(|| "<non-state expression>".to_string()),
                    expr.span().or_else(|| target.and_then(Expression::span)),
                ));
            }
            return;
        }
        self.walk_expression(expr);
    }
}

/// Phase-1 structural index reduction.
///
/// For each state without a `der(state)` equation, find a non-ODE constraint
/// referencing that state and differentiate it once with symbolic chain-rule.
/// The differentiated equation must explicitly contain `der(state)` to be
/// accepted; otherwise it is discarded.
///
/// # Two conventions, and when each one is available
///
/// [`super::constrained_dummy_derivative`] retains the original constraint in
/// the continuous partition and funds the differentiated one with a generated
/// `__dummyder__` unknown. This pass uses that same *naming* form wherever the
/// exchange can be funded, and falls back to *consumption* — the original row
/// is copied into `initialization` and the continuous row becomes its
/// derivative — where it cannot.
///
/// The naming form pays for each appended row with one new unknown per demoted
/// scalar state, so it is available exactly when the group of rows being
/// differentiated is as wide as the state being reduced.
/// [`super::dummy_row_group`] carries that arithmetic and the commit. It is not
/// always fundable: a state of scalar width one reduced from a three-wide
/// multibody residual would have to buy three rows with one unknown, and that
/// reduction is declined by the group construction rather than mis-funded.
///
/// Consumption remains for those rows, and it keeps one known cost: `g = 0`
/// holds only where initialization enforces it, and drifts afterwards.
///
/// # Rows that define a generated dummy derivative are never consumed
///
/// A row [`super::constrained_dummy_derivative`] wrote is the only continuous
/// definition of its `__dummyder__` unknown. Consuming it leaves that unknown
/// undetermined while the row and column counts stay exactly balanced — the
/// moved row's width and the new derivative column cancel — so only a matching
/// sees the loss. The equation-driven pass already declines such rows
/// (`reason=defines_a_dummy_derivative`); this pass declines them too and
/// reaches them through the naming form instead.
///
/// That gate is also what makes the leaf resolution below sound. Resolving
/// `der(x)` through the `__dummyder__.x` leaf keeps each differentiated
/// constraint the size of one constraint instead of re-expanding the whole
/// chain (`MultiBody.Examples.Loops.Fourbar1`: 14978 continuous expression
/// nodes down to ~4000), but a leaf only means what it says while the row
/// defining it stays in the continuous partition. It now always does.
pub fn index_reduce_missing_state_derivatives_once(
    dae: &mut Dae,
) -> Result<usize, StructuralError> {
    let mut reduced = super::copy_accounting::clone_dae(dae);
    let changed =
        index_reduce_missing_state_derivatives_once_in_place(&mut reduced, &mut HashSet::new())?;
    *dae = reduced;
    Ok(changed)
}

/// Everything one round differentiates against, built once per round.
struct RoundContext<'a> {
    state_names: &'a [VarName],
    state_name_set: &'a HashSet<String>,
    state_derivative_matcher: &'a DerivativeNameMatcher,
    defining_expr_index: &'a DefiningExprIndex,
    structural_bindings: &'a HashMap<String, f64>,
    /// `der(x)` values already named by a generated dummy derivative.
    dummy_derivative_values: &'a HashMap<String, Expression>,
}

fn index_reduce_missing_state_derivatives_once_in_place(
    dae: &mut Dae,
    funded_rows: &mut HashSet<usize>,
) -> Result<usize, StructuralError> {
    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    if state_names.is_empty() {
        return Ok(0);
    }
    let state_name_set: HashSet<String> = state_names
        .iter()
        .map(|name| name.as_str().to_string())
        .collect();
    let state_derivative_matcher = DerivativeNameMatcher::from_var_names(&state_names);
    let defining_expr_index = collect_residual_defining_expr_index(dae);
    let structural_bindings = structural_scalar_bindings(dae);
    let dummy_derivative_values =
        super::dummy_derivative_group::generated_dummy_derivative_values(dae);
    let context = RoundContext {
        state_names: &state_names,
        state_name_set: &state_name_set,
        state_derivative_matcher: &state_derivative_matcher,
        defining_expr_index: &defining_expr_index,
        structural_bindings: &structural_bindings,
        dummy_derivative_values: &dummy_derivative_values,
    };
    let mut changed = 0usize;
    let mut used_eq = HashSet::new();

    for state_name in &state_names {
        if state_has_standalone_der_equation(dae, state_name, &state_names)? {
            continue;
        }
        if reduce_one_missing_state_derivative(dae, state_name, &context, funded_rows)? {
            // A naming-form commit rewrote the variable partitions and appended
            // rows, so this round's cached indices no longer describe the DAE.
            changed += 1;
            return Ok(changed);
        }
        if consume_one_missing_state_derivative(dae, state_name, &context, &mut used_eq)? {
            changed += 1;
        }
    }

    Ok(changed)
}

/// Rows this pass may differentiate for `state_name`, in equation order.
fn candidate_rows_for_state(
    dae: &Dae,
    state_name: &VarName,
    context: &RoundContext<'_>,
    used_eq: &HashSet<usize>,
) -> Vec<usize> {
    dae.continuous
        .equations
        .iter()
        .enumerate()
        .filter_map(|(idx, eq)| {
            if used_eq.contains(&idx) {
                return None;
            }
            if eq_contains_any_state_der_with_matcher(&eq.rhs, context.state_derivative_matcher) {
                return None;
            }
            if dae.variables.algebraics.keys().any(|alg_name| {
                is_unsliced_algebraic_definition(eq, alg_name)
                    && !has_independent_exact_alias_equation(dae, idx, alg_name)
            }) {
                return None;
            }
            if is_indexed_state_component_alias_definition(eq, state_name) {
                return None;
            }
            Some(idx)
        })
        .collect()
}

/// The time derivative of row `idx`, or `None` when it is inadmissible for
/// reducing `state_name`.
fn differentiated_candidate_row(
    dae: &Dae,
    idx: usize,
    state_name: &VarName,
    context: &RoundContext<'_>,
) -> Result<Option<Expression>, StructuralError> {
    differentiate_candidate_row_through(dae, idx, state_name, context, LeafResolution::Named)
}

/// Whether the derivative closure may stop at a generated `__dummyder__` leaf.
///
/// Stopping there is what keeps each differentiated constraint the size of one
/// constraint instead of re-expanding the whole chain, and it is why the
/// state-driven pass is affordable on `MultiBody` at all. But a leaf is opaque:
/// the row that defines it still depends on the coordinates the inlined
/// expression named, and the differentiated row no longer mentions them. When
/// the *only* thing that matters about a row is whether its derivative reaches
/// `der(state)`, a leaf can hide the answer.
#[derive(Clone, Copy, PartialEq, Eq)]
enum LeafResolution {
    /// Resolve `der(x)` to `__dummyder__.x` wherever an earlier round named it.
    Named,
    /// Expand every derivative through its defining row, as if nothing had been
    /// named. Reserved for a second look at a row the leaf form already
    /// rejected, so the compact form stays the one that is paid for.
    Expanded,
}

fn differentiate_candidate_row_through(
    dae: &Dae,
    idx: usize,
    state_name: &VarName,
    context: &RoundContext<'_>,
    leaves: LeafResolution,
) -> Result<Option<Expression>, StructuralError> {
    let seed_exprs = vec![dae.continuous.equations[idx].rhs.clone()];
    let der_map = build_relaxed_derivative_map_for_exprs_with_index(
        dae,
        context.defining_expr_index,
        &seed_exprs,
        RelaxedDerivativeMapOptions {
            canonical_state_derivative: Some(state_name),
            rejected_state_derivative: None,
            excluded_equations: &[idx],
            selected_derivatives: match leaves {
                LeafResolution::Named => Some(context.dummy_derivative_values),
                LeafResolution::Expanded => None,
            },
        },
    )?;
    let Some(differentiated) =
        symbolic_time_derivative(&dae.continuous.equations[idx].rhs, dae, &der_map)
    else {
        return Ok(trace_row_declined(idx, state_name, "not_differentiable"));
    };
    let new_rhs = crate::eliminate::simplify_arithmetic_identities(differentiated);
    let der_states = derivative_states_in_eq(&new_rhs, context.state_names);
    if !der_states.iter().any(|der_state| der_state == state_name) {
        return Ok(trace_row_declined(
            idx,
            state_name,
            "state_derivative_absent",
        ));
    }
    if expr_contains_der_of_non_state(&new_rhs, context.state_name_set) {
        return Ok(trace_row_declined(
            idx,
            state_name,
            "derivative_of_non_state",
        ));
    }
    if !expression_is_smooth_for_index_reduction(
        &dae.continuous.equations[idx].rhs,
        dae,
        context.structural_bindings,
    ) || !expression_is_smooth_for_index_reduction(&new_rhs, dae, context.structural_bindings)
    {
        return Ok(None);
    }
    Ok(Some(new_rhs))
}

/// Record why a candidate row cannot reduce `state_name`, and report "no row".
///
/// A state that stays a state with no derivative row is the shape a matching
/// later reports as an unmatched column, and the reason is always here: the row
/// did not differentiate, or its derivative did not reach `der(state)`. Naming
/// it is the difference between an operator seeing "the reduction was attempted
/// and every row was rejected for this reason" and seeing nothing at all.
fn trace_row_declined(idx: usize, state_name: &VarName, reason: &str) -> Option<Expression> {
    crate::structural_trace!(
        "[sim-trace] state-row reduction declined row={idx} state={} reason={reason}",
        state_name.as_str()
    );
    None
}

/// The derivative of a candidate row for the *naming* form, looking past a
/// `__dummyder__` leaf when the leaf hid `der(state_name)`.
///
/// The leaf form is tried first and is what nearly every row is accepted on, so
/// the compact differentiated constraint stays the one that is paid for. It can
/// answer "this row does not reach `der(state)`" for the wrong reason, though:
/// the row's dependence on the state may run through a coordinate an earlier
/// round named, and `__dummyder__.q` mentions none of it. Re-differentiating
/// that row with every derivative expanded answers the question the leaf could
/// not, and leaves the state reducible instead of stranding it as a column with
/// no row (`Engine1a`: `crank2..4.body.v_0`, `connectingRod.body.v_0`).
///
/// The second look is bounded: the caller only offers rows that define a
/// generated dummy derivative and fit inside the state's width, and only the
/// rows the leaf form already rejected reach it.
fn fundable_row_derivative(
    dae: &Dae,
    idx: usize,
    state_name: &VarName,
    context: &RoundContext<'_>,
) -> Result<Option<Expression>, StructuralError> {
    if let Some(new_rhs) =
        differentiate_candidate_row_through(dae, idx, state_name, context, LeafResolution::Named)?
    {
        return Ok(Some(new_rhs));
    }
    differentiate_candidate_row_through(dae, idx, state_name, context, LeafResolution::Expanded)
}

/// Try the naming form for `state_name`: differentiate a group of
/// dummy-defining rows whose combined width funds the state, append the
/// derivatives, and demote the state.
///
/// Returns `true` when a group was committed.
fn reduce_one_missing_state_derivative(
    dae: &mut Dae,
    state_name: &VarName,
    context: &RoundContext<'_>,
    funded_rows: &mut HashSet<usize>,
) -> Result<bool, StructuralError> {
    let Some(state_size) = dae
        .variables
        .states
        .get(state_name)
        .map(Variable::size)
        .filter(|size| *size > 0)
    else {
        return Ok(false);
    };
    if !super::dummy_row_group::state_is_demotable(dae, state_name) {
        return Ok(false);
    }
    let mut group: Vec<(usize, Expression)> = Vec::new();
    let mut group_width = 0usize;
    for idx in candidate_rows_for_state(dae, state_name, context, funded_rows) {
        let width = dae.continuous.equations[idx].scalar_count;
        if !super::dummy_row_group::row_defines_a_dummy_derivative(&dae.continuous.equations[idx])
            || width == 0
            || group_width + width > state_size
        {
            continue;
        }
        let Some(new_rhs) = fundable_row_derivative(dae, idx, state_name, context)? else {
            continue;
        };
        group.push((idx, new_rhs));
        group_width += width;
        if group_width == state_size
            && super::dummy_row_group::reduce_state_by_row_group(dae, state_name, &group)?
        {
            funded_rows.extend(group.iter().map(|(index, _)| *index));
            return Ok(true);
        }
    }
    crate::structural_trace!(
        "[sim-trace] dummy row group not formed state={} width={state_size} \
         funded={group_width} rows={:?}",
        state_name.as_str(),
        group.iter().map(|(index, _)| *index).collect::<Vec<_>>()
    );
    Ok(false)
}

/// Fall back to consumption: replace the source row with its derivative and
/// retain the original in `initialization`.
///
/// Rows that define a generated dummy derivative are never consumed; they are
/// only ever reached through [`reduce_one_missing_state_derivative`].
///
/// Every candidate that differentiates is staged in place and judged by
/// [`super::demotion_rank_check::consumption_is_rank_justified`] before it is
/// kept: consumption is the one step in this pass that *removes* a row's ability
/// to determine anything, and a row that was some algebraic's only row must not
/// be spent here. The reading it is judged against is taken lazily, once per
/// call, so a state whose every candidate is rejected earlier — the common case
/// — pays for no matching at all.
fn consume_one_missing_state_derivative(
    dae: &mut Dae,
    state_name: &VarName,
    context: &RoundContext<'_>,
    used_eq: &mut HashSet<usize>,
) -> Result<bool, StructuralError> {
    let mut rank_before: Option<Option<usize>> = None;
    for idx in candidate_rows_for_state(dae, state_name, context, used_eq) {
        if super::dummy_row_group::row_defines_a_dummy_derivative(&dae.continuous.equations[idx]) {
            crate::structural_trace!(
                "[sim-trace] state-row reduction declined row={idx} state={} \
                 reason=defines_a_dummy_derivative",
                state_name.as_str()
            );
            continue;
        }
        let Some(new_rhs) = differentiated_candidate_row(dae, idx, state_name, context)? else {
            continue;
        };
        let before =
            *rank_before.get_or_insert_with(|| super::demotion_rank_check::row_rank_reading(dae));
        let original_rhs = std::mem::replace(&mut dae.continuous.equations[idx].rhs, new_rhs);
        if !super::demotion_rank_check::consumption_is_rank_justified(before, dae, state_name, idx)
        {
            dae.continuous.equations[idx].rhs = original_rhs;
            continue;
        }
        // The differentiated equation preserves the constraint only after t=0.
        // Retain the original equation to initialize on the same solution manifold.
        let mut original = dae.continuous.equations[idx].clone();
        original.rhs = original_rhs;
        dae.initialization.equations.push(original);
        let old_origin = dae.continuous.equations[idx].origin.clone();
        dae.continuous.equations[idx].origin = if old_origin.is_empty() {
            format!("index_reduction:d_dt_for_{}", state_name.as_str())
        } else {
            format!(
                "{}|index_reduction:d_dt_for_{}",
                old_origin,
                state_name.as_str()
            )
        };
        used_eq.insert(idx);
        return Ok(true);
    }
    Ok(false)
}

fn is_unsliced_algebraic_definition(eq: &Equation, alg_name: &VarName) -> bool {
    let Expression::Binary { op, lhs, rhs, .. } = &eq.rhs else {
        return false;
    };
    if !matches!(op, OpBinary::Sub) {
        return false;
    }
    [lhs.as_ref(), rhs.as_ref()].into_iter().any(|expr| {
        matches!(
            expr,
            Expression::VarRef { name, subscripts, .. }
                if name.var_name() == alg_name && subscripts.is_empty()
        )
    })
}

/// True when some row other than `defining_equation` still determines `name`
/// through an exact alias `name = partner`.
///
/// An alias row only *supplies* a value when its partner has one. When the
/// partner is a state the integrator supplies it, and the alias determines
/// `name` outright. When the partner is an algebraic, the alias relates two
/// unknowns and determines neither on its own: it can be assigned to `name`
/// only if the partner is determined somewhere else again.
///
/// Ignoring that turns consumption into a rank loss the counting invariant
/// cannot see. In `a = -x2 - x3; x1 = a`, once `x1` has been demoted to an
/// algebraic the pair `{x1, a}` is carried by exactly those two rows; consuming
/// the first — replacing it with a derivative that mentions neither name —
/// leaves one row for two unknowns while rows and columns stay balanced.
fn has_independent_exact_alias_equation(
    dae: &Dae,
    defining_equation: usize,
    name: &VarName,
) -> bool {
    dae.continuous
        .equations
        .iter()
        .enumerate()
        .filter(|(index, _)| *index != defining_equation)
        .filter_map(|(index, equation)| {
            let (lhs, rhs) = try_extract_state_alias_pair(&equation.rhs)?;
            let partner = if lhs == *name {
                rhs
            } else if rhs == *name {
                lhs
            } else {
                return None;
            };
            Some((index, partner))
        })
        .any(|(alias_row, partner)| {
            alias_partner_is_determined_elsewhere(dae, alias_row, defining_equation, &partner)
        })
}

/// True when `partner` has a value that does not come from `alias_row`.
///
/// A state is supplied by the integrator. An algebraic needs a continuous row of
/// its own, and neither the alias row nor the row being consumed can be it.
///
/// # What the row test does and does not establish
///
/// For an algebraic this looks for a *witness row*: some other continuous row
/// that reads `partner`. Reading a name is not the same as determining it, and
/// the gap is reachable — `0 = y - 2*a` reads `a` while determining neither `y`
/// nor `a`, so a witness that is itself two undetermined algebraics wide is no
/// witness at all. [`witness_row_can_determine`] rejects that shape; anything
/// wider is accepted, because deciding it properly is a matching question over
/// the rows excluding `consumed_row`, not a predicate over one row.
///
/// So this remains a necessary condition rather than a sufficient one. It is
/// where the cheap tests stop being able to tell the difference, and the
/// counting invariant this gate protects is re-checked by the real matching
/// downstream in any case.
fn alias_partner_is_determined_elsewhere(
    dae: &Dae,
    alias_row: usize,
    consumed_row: usize,
    partner: &VarName,
) -> bool {
    if dae.variables.states.contains_key(partner) {
        return true;
    }
    dae.continuous
        .equations
        .iter()
        .enumerate()
        .any(|(index, equation)| {
            index != alias_row
                && index != consumed_row
                && expr_contains_var(&equation.rhs, partner)
                && witness_row_can_determine(dae, equation)
        })
}

/// Can this row plausibly be the row that determines `partner`?
///
/// It cannot when the row's only two continuous unknowns are `partner` and one
/// other algebraic that nothing has determined either: such a row relates two
/// free names and pins neither. `0 = y - 2*a` is the shape, and it is exactly
/// the shape that makes "appears in another row" too weak to stand in for
/// "determined elsewhere".
///
/// Anything grounded in the row — a state, a state derivative, a parameter, a
/// constant, an input or a discrete — is enough for the row to be a candidate
/// definition, and so is any third unknown, because a wider row is a matching
/// question this predicate deliberately does not try to answer.
///
/// `partner` is not a parameter here: it is one of the free names by
/// construction, so the count is what decides.
fn witness_row_can_determine(dae: &Dae, equation: &Equation) -> bool {
    let mut free = Vec::new();
    for name in collect_rhs_var_refs(&equation.rhs) {
        if dae.variables.algebraics.contains_key(&name) || dae.variables.outputs.contains_key(&name)
        {
            free.push(name);
        } else {
            // A state value, parameter, constant, input or discrete is known
            // when the row is evaluated, so the row grounds `partner`.
            return true;
        }
    }
    free.len() != 2
}

fn is_indexed_state_component_alias_definition(eq: &Equation, state_name: &VarName) -> bool {
    let Expression::Binary { op, lhs, rhs, .. } = &eq.rhs else {
        return false;
    };
    if !matches!(op, OpBinary::Sub) {
        return false;
    }
    let lhs_is_state_component = is_indexed_component_of_state(lhs, state_name);
    let rhs_is_state_component = is_indexed_component_of_state(rhs, state_name);
    if lhs_is_state_component == rhs_is_state_component {
        return false;
    }
    let other = if lhs_is_state_component { rhs } else { lhs };
    !expr_contains_var(other, state_name)
}

fn is_indexed_component_of_state(expr: &Expression, state_name: &VarName) -> bool {
    let Some(exact_name) = expression_exact_name(expr) else {
        return false;
    };
    exact_name != state_name.as_str()
        && rumoca_core::parse_scalar_name(&exact_name)
            .is_some_and(|scalar| scalar.base == state_name.as_str())
}

/// Termination measure of the state-driven pass, ordered lexicographically.
///
/// Consumption leaves the state partition alone and turns one derivative-free
/// row into a row carrying `der(state)`. The naming form does the opposite: it
/// removes a state and *appends* rows that carry no state derivative at all,
/// because `der(state)` has become a reference to the state's generated dummy.
/// So neither component decreases on its own, and a single counter cannot bound
/// the loop.
///
/// The pair does. States never come back — no pass here promotes an algebraic —
/// so `(states, derivative_free_rows)` in lexicographic order is well founded
/// on `ℕ × ℕ`, and every round must lower it.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ReductionMeasure {
    states: usize,
    derivative_free_rows: usize,
}

impl ReductionMeasure {
    fn of(dae: &Dae) -> Self {
        let state_names = dae.variables.states.keys().cloned().collect::<Vec<_>>();
        let matcher = DerivativeNameMatcher::from_var_names(&state_names);
        Self {
            states: state_names.len(),
            derivative_free_rows: dae
                .continuous
                .equations
                .iter()
                .filter(|equation| !eq_contains_any_state_der_with_matcher(&equation.rhs, &matcher))
                .count(),
        }
    }
}

pub fn index_reduce_missing_state_derivatives(dae: &mut Dae) -> Result<usize, StructuralError> {
    let mut reduced = super::copy_accounting::clone_dae(dae);
    let deficiency_before = super::demotion_rank_check::scalar_rank_deficiency(&reduced);
    let mut measure = ReductionMeasure::of(&reduced);
    let mut funded_rows = HashSet::new();
    let mut total_changed = 0usize;
    loop {
        let changed =
            index_reduce_missing_state_derivatives_once_in_place(&mut reduced, &mut funded_rows)?;
        if changed == 0 {
            break;
        }
        let next = ReductionMeasure::of(&reduced);
        if next >= measure {
            return Err(StructuralError::UnspannedContractViolation {
                reason: "index reduction changed equations without lowering the number of states or the finite set of derivative-free continuous rows".to_string(),
            });
        }
        measure = next;
        total_changed += changed;
    }
    super::demotion_rank_check::trace_pass_rank_transition(
        "index_reduce_missing_state_derivatives",
        deficiency_before,
        super::demotion_rank_check::scalar_rank_deficiency(&reduced),
    );
    *dae = reduced;
    Ok(total_changed)
}

/// Regularisation epsilon levels to try, from most accurate to least.
///
/// The larger fallback values help stiff, switch-heavy MSL examples that can
/// otherwise fail early with very small accepted timesteps.
pub const REGULARIZATION_LEVELS: &[f64] = &[1e-8, 1e-6, 1e-4, 1e-3, 1e-2, 1e-1];

/// Determine the sign of `der(state)` in an expression by tracking negations.
///
/// Returns +1 if der(state) appears with positive coefficient, -1 if negative, 0 if absent.
/// Tracks sign flips through subtraction (RHS negated) and unary minus.
pub fn der_sign_in_expr(expr: &Expression, state_name: &VarName, current_sign: i32) -> i32 {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } if args.len() == 1 && expr_refers_to_var(&args[0], state_name) => current_sign,
        Expression::Binary { op, lhs, rhs, .. } => match op {
            OpBinary::Add | OpBinary::AddElem => {
                let l = der_sign_in_expr(lhs, state_name, current_sign);
                if l != 0 {
                    return l;
                }
                der_sign_in_expr(rhs, state_name, current_sign)
            }
            OpBinary::Sub | OpBinary::SubElem => {
                let l = der_sign_in_expr(lhs, state_name, current_sign);
                if l != 0 {
                    return l;
                }
                der_sign_in_expr(rhs, state_name, -current_sign)
            }
            OpBinary::Mul | OpBinary::MulElem => {
                let l = der_sign_in_expr(lhs, state_name, current_sign);
                if l != 0 {
                    return l;
                }
                der_sign_in_expr(rhs, state_name, current_sign)
            }
            _ => 0,
        },
        Expression::Unary { op, rhs, .. } => match op {
            OpUnary::Minus | OpUnary::DotMinus => der_sign_in_expr(rhs, state_name, -current_sign),
            _ => der_sign_in_expr(rhs, state_name, current_sign),
        },
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (_, v) in branches {
                let s = der_sign_in_expr(v, state_name, current_sign);
                if s != 0 {
                    return s;
                }
            }
            der_sign_in_expr(else_branch, state_name, current_sign)
        }
        _ => 0,
    }
}

/// Normalize ODE equation signs so that `der(state)` has positive coefficient.
///
/// The mass-matrix formulation `M * y' = f` with `f = -eval(equation)` for ODE
/// rows requires `der(state)` to appear with coefficient +1 in the residual.
/// Equations like `0 = v - der(s)` (from `v = der(s)` in Modelica) have
/// coefficient -1 and produce the wrong sign.
///
/// This pass negates equations where `der(state)` has negative coefficient.
pub fn normalize_ode_equation_signs(dae: &mut Dae) {
    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    for (i, state_name) in state_names.iter().enumerate() {
        if i >= dae.continuous.equations.len() {
            break;
        }
        let sign = der_sign_in_expr(&dae.continuous.equations[i].rhs, state_name, 1);
        if sign < 0 {
            let old_rhs = dae.continuous.equations[i].rhs.clone();
            let span = dae.continuous.equations[i].span;
            dae.continuous.equations[i].rhs = Expression::Unary {
                op: OpUnary::Minus,
                rhs: Box::new(old_rhs),
                span,
            };
        }
    }
}

/// After ODE row selection, non-ODE residual rows must not keep standalone
/// `der(state)` calls because compiled residual evaluation lowers `der(...)`
/// to zero outside the mass-matrix rows. Substitute any duplicate standalone
/// state derivative that can be resolved from the selected ODE rows.
pub fn substitute_standalone_state_derivatives_in_non_ode_rows(dae: &mut Dae) -> usize {
    let n_x: usize = dae.variables.states.values().map(Variable::size).sum();
    if n_x == 0 {
        return 0;
    }

    let mut der_map = build_der_value_map(dae);
    // Multiple scalar equations may constrain the same derivative after
    // boundary alias elimination (`der(x)=a`, `der(x)=b`). They are exactly
    // equivalent to one defining derivative row plus algebraic equalities, so
    // choose the first source-ordered assignment as the canonical definition.
    // Ranked states remain on the stricter component-aware map path above.
    for (state_name, variable) in &dae.variables.states {
        if !variable.dims.is_empty() || der_map.contains_key(state_name.as_str()) {
            continue;
        }
        let replacement = dae.continuous.equations.iter().find_map(|equation| {
            let value = try_extract_der_value(&equation.rhs, state_name)?;
            (!expr_contains_der_of(&value, state_name)).then_some(value)
        });
        if let Some(replacement) = replacement {
            der_map.insert(state_name.as_str().to_string(), replacement);
        }
    }
    if der_map.is_empty() {
        return 0;
    }

    // Scalar states only. This pass substitutes a single `der_map[state]` value
    // for every `der(state)` occurrence, which is well-defined only for scalar
    // states. An array/matrix state `R[m,n]` has one ODE row *per component*
    // (`der(R[i,j]) = ...`); `der_map[R]` is the whole-array der value, and
    // `der(R[i,j])` matches state `R` by base name, so substituting here would
    // (a) overwrite component derivatives with the entire array and (b) rewrite
    // all-but-one component ODE row (only one "defining row" per state name is
    // protected), corrupting them into unmatchable residuals. The matcher
    // handles component ODE rows directly, so array states are left untouched.
    let state_names: Vec<VarName> = dae
        .variables
        .states
        .iter()
        .filter(|(_, var)| var.size() <= 1)
        .map(|(name, _)| name.clone())
        .collect();

    // For each state, locate the equation that *defines* its derivative — the
    // first row whose rhs is `der(state)` with an extractable value (the same
    // row `build_der_value_map` uses). `der(state)` must never be substituted
    // inside its own defining row: that collapses the row to `value = value`
    // and orphans `der(state)`. (Keying off equation order — "skip the first
    // n_x rows" — is unsafe: the ODE rows are not guaranteed to come first.)
    let mut defining_row: std::collections::HashMap<&str, usize> = std::collections::HashMap::new();
    for state_name in &state_names {
        if !der_map.contains_key(state_name.as_str()) {
            continue;
        }
        for (index, eq) in dae.continuous.equations.iter().enumerate() {
            if expr_contains_der_of(&eq.rhs, state_name)
                && try_extract_der_value(&eq.rhs, state_name).is_some()
            {
                defining_row.insert(state_name.as_str(), index);
                break;
            }
        }
    }

    let mut rewritten_rows = 0usize;
    for (index, eq) in dae.continuous.equations.iter_mut().enumerate() {
        rewritten_rows += usize::from(rewrite_known_state_derivatives(
            eq,
            &state_names,
            &der_map,
            Some((&defining_row, index)),
        ));
    }
    rewritten_rows += rewrite_state_derivatives_in_non_continuous_partitions(dae, &der_map);

    rewritten_rows
}

fn rewrite_known_state_derivatives(
    equation: &mut Equation,
    state_names: &[VarName],
    derivative_values: &std::collections::HashMap<String, Expression>,
    protected_rows: Option<(&std::collections::HashMap<&str, usize>, usize)>,
) -> bool {
    let mut rewritten = false;
    for state_name in state_names {
        if protected_rows.is_some_and(|(rows, index)| {
            rows.get(state_name.as_str())
                .is_some_and(|row| *row == index)
        }) {
            continue;
        }
        let Some(replacement) = derivative_values.get(state_name.as_str()) else {
            continue;
        };
        if expression_contains_any_der_call(replacement)
            || !expr_contains_der_of(&equation.rhs, state_name)
        {
            continue;
        }
        equation.rhs = substitute_der_of_state(&equation.rhs, state_name, replacement);
        rewritten = true;
    }
    rewritten
}

/// Resolve state-derivative reads outside the continuous owner rows.
///
/// MLS permits `der(state)` in initialization constraints and in values
/// captured by a `when` equation. Those partitions consume the derivative
/// selected by the continuous system; they do not own another derivative row.
/// Ranked state components are projected from the aggregate derivative value
/// here so `der(v[2])` never reaches generic Solve-expression lowering.
fn rewrite_state_derivatives_in_non_continuous_partitions(
    dae: &mut Dae,
    derivative_values: &std::collections::HashMap<String, Expression>,
) -> usize {
    let mut rewriter = NonOwnerStateDerivativeRewriter {
        derivative_values,
        replacements: 0,
    };
    rewriter.rewrite_equations(&mut dae.initialization.equations);
    rewriter.rewrite_equations(&mut dae.discrete.real_updates);
    rewriter.rewrite_equations(&mut dae.discrete.valued_updates);
    rewriter.rewrite_equations(&mut dae.conditions.equations);
    rewriter.rewrite_expression_slots(&mut dae.conditions.relations);
    rewriter.rewrite_expression_slots(&mut dae.events.synthetic_root_conditions);
    rewriter.rewrite_event_actions(&mut dae.events.event_actions);
    rewriter.rewrite_delay_channels(&mut dae.events.delay_channels);
    rewriter.rewrite_expression_slots(&mut dae.clocks.constructor_exprs);
    rewriter.rewrite_expression_slots(&mut dae.clocks.triggered_conditions);
    rewriter.replacements
}

struct NonOwnerStateDerivativeRewriter<'a> {
    derivative_values: &'a std::collections::HashMap<String, Expression>,
    replacements: usize,
}

impl ExpressionRewriter for NonOwnerStateDerivativeRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        if let Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } = expr
            && let [target] = args.as_slice()
            && let Some(replacement) =
                derivative_replacement_for_target(target, self.derivative_values)
            && !expression_contains_any_der_call(&replacement)
        {
            self.replacements += 1;
            return replacement;
        }
        self.walk_expression(expr)
    }
}

impl DaeExpressionRewriter for NonOwnerStateDerivativeRewriter<'_> {}

fn derivative_replacement_for_target(
    target: &Expression,
    derivative_values: &std::collections::HashMap<String, Expression>,
) -> Option<Expression> {
    if let Some(exact_name) = expression_exact_name(target)
        && let Some(value) = derivative_values.get(&exact_name)
    {
        return Some(value.clone());
    }

    let (base_name, subscripts, span) = derivative_target_projection(target)?;
    if let Some(value) = derivative_values.get(base_name.as_str()) {
        return Some(project_derivative_value(value, &subscripts, span));
    }

    let scalar = rumoca_core::parse_scalar_name(base_name.as_str())?;
    let value = derivative_values.get(scalar.base)?;
    let provenance = span
        .require_provenance("scalar derivative replacement projection")
        .ok()?;
    let subscripts = scalar
        .indices
        .iter()
        .map(|index| Subscript::generated_index_with_provenance(*index, provenance))
        .collect::<Vec<_>>();
    Some(project_derivative_value(value, &subscripts, span))
}

fn derivative_target_projection(target: &Expression) -> Option<(VarName, Vec<Subscript>, Span)> {
    match target {
        Expression::VarRef {
            name,
            subscripts,
            span,
        } => Some((name.var_name().clone(), subscripts.clone(), *span)),
        Expression::Index {
            base,
            subscripts,
            span,
        } => {
            let Expression::VarRef {
                name,
                subscripts: base_subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            let mut combined = base_subscripts.clone();
            combined.extend_from_slice(subscripts);
            Some((name.var_name().clone(), combined, *span))
        }
        _ => None,
    }
}

fn project_derivative_value(
    value: &Expression,
    subscripts: &[Subscript],
    span: Span,
) -> Expression {
    if subscripts.is_empty() {
        return value.clone();
    }
    if let Some(projected) = project_static_array_value(value, subscripts) {
        return projected;
    }
    Expression::Index {
        base: Box::new(value.clone()),
        subscripts: subscripts.to_vec(),
        span,
    }
}

fn project_static_array_value(value: &Expression, subscripts: &[Subscript]) -> Option<Expression> {
    let mut selected = value;
    for subscript in subscripts {
        let index = static_one_based_subscript(subscript)?;
        let Expression::Array { elements, .. } = selected else {
            return None;
        };
        selected = elements.get(index.checked_sub(1)?)?;
    }
    Some(selected.clone())
}

fn static_one_based_subscript(subscript: &Subscript) -> Option<usize> {
    let value = match subscript {
        Subscript::Index { value, .. } => *value,
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: Literal::Integer(value),
                ..
            } => *value,
            Expression::Literal {
                value: Literal::Real(value),
                ..
            } if value.is_finite() && value.fract() == 0.0 => *value as i64,
            _ => return None,
        },
        Subscript::Colon { .. } => return None,
    };
    usize::try_from(value).ok().filter(|value| *value > 0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{BuiltinFunction, Reference, SourceId};

    fn test_span() -> Span {
        Span::from_offsets(
            SourceId::from_source_name("state_row_reduction_test.mo"),
            12,
            31,
        )
    }

    fn var_ref(name: &str, span: Span) -> Expression {
        Expression::VarRef {
            name: Reference::from_var_name(VarName::new(name)),
            subscripts: Vec::new(),
            span,
        }
    }

    fn der_call(name: &str, span: Span) -> Expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![var_ref(name, span)],
            span,
        }
    }

    #[test]
    fn normalize_ode_equation_sign_uses_equation_span() {
        let span = test_span();
        let mut dae = Dae::new();
        dae.variables.states.insert(
            VarName::new("s"),
            Variable::new(
                VarName::new("s"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        dae.continuous.equations.push(Equation::residual(
            Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(var_ref("v", span)),
                rhs: Box::new(der_call("s", span)),
                span,
            },
            span,
            "test",
        ));

        normalize_ode_equation_signs(&mut dae);

        let Expression::Unary {
            op: OpUnary::Minus,
            span: actual,
            ..
        } = dae.continuous.equations[0].rhs
        else {
            panic!("expected normalized unary minus");
        };
        assert_eq!(actual, span);
    }

    #[test]
    fn substitutes_duplicate_scalar_derivative_assignments_with_first_value() {
        let span = test_span();
        let mut dae = Dae::new();
        dae.variables
            .states
            .insert(VarName::new("s"), Variable::new(VarName::new("s"), span));
        for value in ["v1", "v2", "v3"] {
            dae.variables.algebraics.insert(
                VarName::new(value),
                Variable::new(VarName::new(value), span),
            );
            dae.continuous.equations.push(Equation::residual(
                Expression::Binary {
                    op: OpBinary::Sub,
                    lhs: Box::new(der_call("s", span)),
                    rhs: Box::new(var_ref(value, span)),
                    span,
                },
                span,
                "duplicate derivative assignment",
            ));
        }

        let rewritten = substitute_standalone_state_derivatives_in_non_ode_rows(&mut dae);

        assert_eq!(rewritten, 2);
        assert!(expr_contains_der_of(
            &dae.continuous.equations[0].rhs,
            &VarName::new("s")
        ));
        for equation in &dae.continuous.equations[1..] {
            assert!(!expr_contains_der_of(&equation.rhs, &VarName::new("s")));
            assert!(expr_contains_var(&equation.rhs, &VarName::new("v1")));
        }
    }

    #[test]
    fn substitutes_selected_state_derivative_in_initialization_equation() {
        let span = test_span();
        let mut dae = Dae::new();
        dae.variables
            .states
            .insert(VarName::new("s"), Variable::new(VarName::new("s"), span));
        dae.variables
            .algebraics
            .insert(VarName::new("v"), Variable::new(VarName::new("v"), span));
        dae.continuous.equations.push(Equation::residual(
            Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(der_call("s", span)),
                rhs: Box::new(var_ref("v", span)),
                span,
            },
            span,
            "selected ODE row",
        ));
        dae.initialization.equations.push(Equation::residual(
            Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(der_call("s", span)),
                rhs: Box::new(Expression::Literal {
                    value: Literal::Integer(0),
                    span,
                }),
                span,
            },
            span,
            "steady-state initialization",
        ));

        let rewritten = substitute_standalone_state_derivatives_in_non_ode_rows(&mut dae);

        assert_eq!(rewritten, 1);
        assert!(expr_contains_der_of(
            &dae.continuous.equations[0].rhs,
            &VarName::new("s")
        ));
        assert!(!expr_contains_der_of(
            &dae.initialization.equations[0].rhs,
            &VarName::new("s")
        ));
        assert!(expr_contains_var(
            &dae.initialization.equations[0].rhs,
            &VarName::new("v")
        ));
    }

    #[test]
    fn substitutes_ranked_state_component_derivative_in_discrete_update() {
        let span = test_span();
        let mut dae = Dae::new();
        let mut velocity = Variable::new(VarName::new("velocity"), span);
        velocity.dims = vec![2];
        dae.variables
            .states
            .insert(VarName::new("velocity"), velocity);
        for (index, acceleration) in [(1, "a1"), (2, "a2")] {
            dae.continuous.equations.push(Equation::residual(
                Expression::Binary {
                    op: OpBinary::Sub,
                    lhs: Box::new(der_call(&format!("velocity[{index}]"), span)),
                    rhs: Box::new(var_ref(acceleration, span)),
                    span,
                },
                span,
                "ranked ODE row",
            ));
        }
        dae.discrete.real_updates.push(Equation::explicit(
            Reference::new("captured_acceleration"),
            der_call("velocity[1]", span),
            span,
            "when capture",
        ));

        let rewritten = substitute_standalone_state_derivatives_in_non_ode_rows(&mut dae);

        assert_eq!(rewritten, 1);
        assert_eq!(
            dae.discrete.real_updates[0].rhs,
            var_ref("a1", span),
            "the discrete update should read the selected component ODE value"
        );
    }
}
