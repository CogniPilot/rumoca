// SPEC_0021 file-size exception: DAE preparation still coordinates alias
// demotion, dummy-state reduction, and structural preprocessing in one module.
// split plan: move direct state demotion and constrained dummy state reduction
// into focused submodules with imports at the top of each file.
use std::cmp::Reverse;
use std::collections::{HashMap, HashSet, hash_map::Entry};

use indexmap::{IndexMap, IndexSet};
use rumoca_core::timing::{OptionalTimer, maybe_start_timer};
use rumoca_core::{ExpressionRewriter, ExpressionVisitor};
use rumoca_ir_dae as dae;
use rumoca_ir_dae::{
    DerivativeNameMatcher, expr_contains_der_of, expr_contains_der_of_any, expr_contains_var,
    expr_refers_to_var,
};

use crate::StructuralError;

type BuiltinFunction = rumoca_core::BuiltinFunction;
type Dae = dae::Dae;
type Equation = dae::Equation;
type Expression = rumoca_core::Expression;
type Literal = rumoca_core::Literal;
type OpBinary = rumoca_core::OpBinary;
type OpUnary = rumoca_core::OpUnary;
type Span = rumoca_core::Span;
type Subscript = rumoca_core::Subscript;
type VarName = rumoca_core::VarName;
type Variable = dae::Variable;
type DefiningExprIndex = IndexMap<String, Vec<IndexedDefiningExpr>>;
type AliasSafetyCache = IndexMap<(String, Option<usize>), bool>;

#[derive(Clone)]
struct IndexedDefiningExpr {
    equation_index: usize,
    expr: Expression,
}

mod connection_alias;
use connection_alias::connection_component_fixed_defining_expr;
mod symbolic;
use symbolic::{
    array_expr_from_flat_values, build_der_value_map, expand_der_in_expr_full, expression_dims,
    flat_index_from_indices, project_flat_index_with_span, static_subscript_indices,
    symbolic_time_derivative, truncate_debug, try_extract_der_value,
};
mod row_shape;
use row_shape::{dae_variable_size, required_dae_variable_size, residual_scalar_width};
mod dummy_state_metadata;
pub use dummy_state_metadata::{
    ConstrainedDummyDefinition, constrained_dummy_state_defining_exprs,
    constrained_dummy_state_names,
};
mod direct_demotion;
mod state_row_reduction;
pub use direct_demotion::demote_direct_assigned_states;
use direct_demotion::{
    collect_non_state_continuous_unknown_names, equation_defining_expr_for_unknown,
    expr_refs_only_parameters_constants_or_time, expression_contains_any_der_call,
    is_connection_equation_origin,
};
pub use state_row_reduction::{
    REGULARIZATION_LEVELS, demote_orphan_states_without_equation_refs,
    demote_states_without_assignable_derivative_rows, demote_states_without_derivative_refs,
    demote_states_without_retained_derivative_rows, der_sign_in_expr,
    index_reduce_missing_state_derivatives, index_reduce_missing_state_derivatives_once,
    normalize_ode_equation_signs, substitute_standalone_state_derivatives_in_non_ode_rows,
};

fn sim_trace_enabled() -> bool {
    crate::structural_trace_enabled()
}

fn structural_timing_start(_label: &str) -> OptionalTimer {
    maybe_start_timer()
}

fn structural_timing_done(_label: &str, _start: OptionalTimer) {}

/// Try to extract the defining expression for an algebraic variable.
///
/// Looks for equations of the form `0 = var - expr` or `0 = expr - var`
/// and returns `expr` (the value that `var` equals).
fn zero_expr(span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(0.0),
        span,
    }
}

fn add_expr(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn sub_expr(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn div_expr(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn extract_scaled_target(expr: &Expression, target: &VarName) -> Option<Expression> {
    let Expression::Binary { op, lhs, rhs, .. } = expr else {
        return None;
    };
    if !matches!(op, OpBinary::Mul | OpBinary::MulElem) {
        return None;
    }
    let lhs_is_target = matches!(lhs.as_ref(), Expression::VarRef { name, subscripts, .. } if name.var_name() == target && subscripts.is_empty());
    let rhs_is_target = matches!(rhs.as_ref(), Expression::VarRef { name, subscripts, .. } if name.var_name() == target && subscripts.is_empty());
    if lhs_is_target && !expr_contains_var(rhs, target) {
        return Some(*rhs.clone());
    }
    if rhs_is_target && !expr_contains_var(lhs, target) {
        return Some(*lhs.clone());
    }
    None
}

/// If `expr` is affine in `target` with coefficient ±1 and a target-free
/// remainder, return `(coef, remainder)` where `expr = coef*target + remainder`.
fn split_linear_target(
    expr: &Expression,
    target: &VarName,
    context_span: Span,
) -> Option<(i32, Expression)> {
    let span = expr.span().unwrap_or(context_span);
    if expr_refers_to_var(expr, target) {
        return Some((1, zero_expr(span)));
    }

    let Expression::Binary { op, lhs, rhs, .. } = expr else {
        return None;
    };
    match op {
        OpBinary::Add | OpBinary::AddElem => {
            if let Some((coef, rem)) = split_linear_target(lhs, target, span)
                && !expr_contains_var(rhs, target)
            {
                return Some((coef, add_expr(rem, *rhs.clone(), span)));
            }
            if let Some((coef, rem)) = split_linear_target(rhs, target, span)
                && !expr_contains_var(lhs, target)
            {
                return Some((coef, add_expr(*lhs.clone(), rem, span)));
            }
            None
        }
        OpBinary::Sub | OpBinary::SubElem => {
            if let Some((coef, rem)) = split_linear_target(lhs, target, span)
                && !expr_contains_var(rhs, target)
            {
                return Some((coef, sub_expr(rem, *rhs.clone(), span)));
            }
            if let Some((coef, rem)) = split_linear_target(rhs, target, span)
                && !expr_contains_var(lhs, target)
            {
                return Some((-coef, sub_expr(*lhs.clone(), rem, span)));
            }
            None
        }
        _ => None,
    }
}

fn extract_defining_expr(eq: &Equation, alg_name: &VarName) -> Option<Expression> {
    let Expression::Binary { op, lhs, rhs, .. } = &eq.rhs else {
        return None;
    };
    if !matches!(op, OpBinary::Sub) {
        return None;
    }

    let is_var = |e: &Expression| -> bool {
        matches!(e, Expression::VarRef { name, subscripts, .. }
            if name.var_name() == alg_name && subscripts.is_empty())
    };

    // 0 = var - expr → var = expr → return expr
    if is_var(lhs) {
        return Some(*rhs.clone());
    }
    // 0 = expr - var → var = expr → return lhs
    if is_var(rhs) {
        return Some(*lhs.clone());
    }

    let lhs_has = expr_contains_var(lhs, alg_name);
    let rhs_has = expr_contains_var(rhs, alg_name);
    if lhs_has == rhs_has {
        return None;
    }
    if lhs_has && let Some(coeff) = extract_scaled_target(lhs, alg_name) {
        // (coeff*x) - rhs = 0  =>  x = rhs/coeff
        return Some(div_expr(*rhs.clone(), coeff, eq.span));
    }
    if rhs_has && let Some(coeff) = extract_scaled_target(rhs, alg_name) {
        // lhs - (coeff*x) = 0  =>  x = lhs/coeff
        return Some(div_expr(*lhs.clone(), coeff, eq.span));
    }
    if lhs_has && let Some((coef, lhs_rem)) = split_linear_target(lhs, alg_name, eq.span) {
        // (coef*x + lhs_rem) - rhs = 0  =>  x = (rhs - lhs_rem)/coef
        return Some(match coef {
            1 => sub_expr(*rhs.clone(), lhs_rem, eq.span),
            -1 => sub_expr(lhs_rem, *rhs.clone(), eq.span),
            _ => return None,
        });
    }
    if rhs_has && let Some((coef, rhs_rem)) = split_linear_target(rhs, alg_name, eq.span) {
        // lhs - (coef*x + rhs_rem) = 0  =>  x = (lhs - rhs_rem)/coef
        return Some(match coef {
            1 => sub_expr(*lhs.clone(), rhs_rem, eq.span),
            -1 => sub_expr(rhs_rem, *lhs.clone(), eq.span),
            _ => return None,
        });
    }
    None
}

fn find_defining_expr_candidates(dae: &Dae, alg_name: &VarName) -> Vec<Expression> {
    let candidates = collect_residual_defining_expr_index(dae);
    defining_expr_candidates(&candidates, alg_name)
        .cloned()
        .collect()
}

fn push_indexed_defining_expr(
    index: &mut DefiningExprIndex,
    name: &VarName,
    equation_index: usize,
    expr: Expression,
) {
    index
        .entry(name.as_str().to_string())
        .or_default()
        .push(IndexedDefiningExpr {
            equation_index,
            expr,
        });
}

fn collect_rhs_var_refs(expr: &Expression) -> IndexSet<VarName> {
    let mut refs = IndexSet::new();
    expr.collect_var_refs(&mut refs);
    refs
}

fn collect_residual_defining_expr_index(dae: &Dae) -> DefiningExprIndex {
    let mut index = DefiningExprIndex::new();
    for (equation_index, eq) in dae.continuous.equations.iter().enumerate() {
        for ref_name in collect_rhs_var_refs(&eq.rhs) {
            if let Some(expr) = extract_defining_expr(eq, &ref_name) {
                push_indexed_defining_expr(&mut index, &ref_name, equation_index, expr);
            }
        }
    }
    index
}

fn collect_non_derivative_defining_expr_index(dae: &Dae) -> DefiningExprIndex {
    let mut index = DefiningExprIndex::new();
    for (equation_index, eq) in dae.continuous.equations.iter().enumerate() {
        let lhs_name = eq.lhs.as_ref().map(|lhs| lhs.var_name().clone());
        if let Some(name) = &lhs_name
            && !expression_contains_any_der_call(&eq.rhs)
        {
            push_indexed_defining_expr(&mut index, name, equation_index, eq.rhs.clone());
        }

        for ref_name in collect_rhs_var_refs(&eq.rhs) {
            if lhs_name.as_ref().is_some_and(|lhs| lhs == &ref_name) {
                continue;
            }
            if let Some(expr) = equation_defining_expr_for_unknown(eq, &ref_name) {
                push_indexed_defining_expr(&mut index, &ref_name, equation_index, expr);
            }
        }
    }
    index
}

fn defining_expr_candidates<'a>(
    index: &'a DefiningExprIndex,
    name: &VarName,
) -> impl Iterator<Item = &'a Expression> {
    index
        .get(name.as_str())
        .into_iter()
        .flat_map(|candidates| candidates.iter().map(|candidate| &candidate.expr))
}

fn continuous_variable<'a>(dae: &'a Dae, name: &VarName) -> Option<&'a Variable> {
    dae.variables
        .states
        .get(name)
        .or_else(|| dae.variables.algebraics.get(name))
        .or_else(|| dae.variables.outputs.get(name))
        .or_else(|| dae.variables.inputs.get(name))
}

fn insert_symbolic_derivative_fallback(
    dae: &Dae,
    map: &mut HashMap<String, Expression>,
    name: &VarName,
) -> Result<(), StructuralError> {
    let Some(variable) = continuous_variable(dae, name) else {
        return Ok(());
    };
    match map.entry(name.as_str().to_string()) {
        Entry::Occupied(_) => {}
        Entry::Vacant(entry) => {
            entry.insert(symbolic_der_var_ref_for_variable(variable)?);
        }
    }
    Ok(())
}

fn resolve_derivatives_for_expr(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    map: &mut HashMap<String, Expression>,
    expr: &Expression,
    visiting: &mut HashSet<String>,
) -> Result<(), StructuralError> {
    for ref_name in collect_rhs_var_refs(expr) {
        resolve_derivative_for_var(dae, defining_expr_index, map, &ref_name, visiting)?;
    }
    Ok(())
}

fn resolve_derivative_for_var(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    map: &mut HashMap<String, Expression>,
    name: &VarName,
    visiting: &mut HashSet<String>,
) -> Result<(), StructuralError> {
    if name.as_str() == "time"
        || dae.variables.parameters.contains_key(name)
        || dae.variables.constants.contains_key(name)
        || map.contains_key(name.as_str())
    {
        return Ok(());
    }

    if dae.variables.states.contains_key(name) {
        return insert_symbolic_derivative_fallback(dae, map, name);
    }

    if !visiting.insert(name.as_str().to_string()) {
        return insert_symbolic_derivative_fallback(dae, map, name);
    }

    for defining_expr in defining_expr_candidates(defining_expr_index, name) {
        resolve_derivatives_for_expr(dae, defining_expr_index, map, defining_expr, visiting)?;
        let derivative = symbolic_time_derivative(defining_expr, dae, map);
        if let Some(derivative) = derivative
            && !expr_contains_der_of(&derivative, name)
        {
            map.insert(name.as_str().to_string(), derivative);
            visiting.remove(name.as_str());
            return Ok(());
        }
    }

    visiting.remove(name.as_str());
    insert_symbolic_derivative_fallback(dae, map, name)
}

fn build_relaxed_derivative_map_for_exprs(
    dae: &Dae,
    seed_exprs: &[Expression],
) -> Result<HashMap<String, Expression>, StructuralError> {
    let defining_expr_index = collect_residual_defining_expr_index(dae);
    build_relaxed_derivative_map_for_exprs_with_index(dae, &defining_expr_index, seed_exprs)
}

fn build_relaxed_derivative_map_for_exprs_with_index(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    seed_exprs: &[Expression],
) -> Result<HashMap<String, Expression>, StructuralError> {
    let mut map = build_der_value_map(dae);
    let mut visiting = HashSet::new();
    for expr in seed_exprs {
        resolve_derivatives_for_expr(dae, defining_expr_index, &mut map, expr, &mut visiting)?;
    }
    let candidate_names =
        collect_seeded_relaxation_candidates(dae, defining_expr_index, seed_exprs);
    relax_algebraic_derivative_map_to_fixed_point(
        dae,
        defining_expr_index,
        &mut map,
        &candidate_names,
        false,
    );
    Ok(map)
}

fn collect_seeded_relaxation_candidates(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    seed_exprs: &[Expression],
) -> IndexSet<VarName> {
    let mut candidates = IndexSet::new();
    let mut stack: Vec<VarName> = seed_exprs
        .iter()
        .flat_map(|expr| collect_rhs_var_refs(expr).into_iter())
        .collect();

    while let Some(name) = stack.pop() {
        if !(dae.variables.algebraics.contains_key(&name)
            || dae.variables.outputs.contains_key(&name))
        {
            continue;
        }
        if !candidates.insert(name.clone()) {
            continue;
        }
        for defining_expr in defining_expr_candidates(defining_expr_index, &name) {
            stack.extend(collect_rhs_var_refs(defining_expr));
        }
    }

    candidates
}

fn collect_all_relaxation_candidates(dae: &Dae) -> IndexSet<VarName> {
    dae.variables
        .algebraics
        .keys()
        .chain(dae.variables.outputs.keys())
        .cloned()
        .collect()
}

fn relax_algebraic_derivative_map_to_fixed_point(
    dae: &Dae,
    defining_expr_index: &DefiningExprIndex,
    der_map: &mut HashMap<String, Expression>,
    candidate_names: &IndexSet<VarName>,
    replace_existing: bool,
) {
    let resolvable = candidate_names.len();
    for _ in 0..resolvable.max(1) {
        let mut changed = false;
        for alg_name in candidate_names {
            if !replace_existing
                && der_map
                    .get(alg_name.as_str())
                    .is_some_and(|existing| !is_symbolic_derivative_of_var(existing, alg_name))
            {
                continue;
            }
            let derivative = defining_expr_candidates(defining_expr_index, alg_name)
                .find_map(|expr| symbolic_time_derivative(expr, dae, der_map));
            let Some(derivative) = derivative else {
                continue;
            };
            if expr_contains_der_of(&derivative, alg_name) {
                continue;
            }
            if der_map.get(alg_name.as_str()) == Some(&derivative) {
                continue;
            }
            der_map.insert(alg_name.as_str().to_string(), derivative);
            changed = true;
        }
        if !changed {
            break;
        }
    }
}

fn is_symbolic_derivative_of_var(expr: &Expression, name: &VarName) -> bool {
    let Expression::BuiltinCall { function, args, .. } = expr else {
        return false;
    };
    if *function != BuiltinFunction::Der || args.len() != 1 {
        return false;
    }
    matches!(
        &args[0],
        Expression::VarRef {
            name: ref_name,
            subscripts,
            ..
        } if ref_name.var_name() == name && subscripts.is_empty()
    )
}

/// Iteratively resolve time derivatives for algebraic variables.
///
/// Starting from known state derivatives (from `build_der_value_map`), this
/// function iteratively resolves derivatives for algebraic variables by:
/// 1. Finding the algebraic equation that defines each variable: `z = expr`
/// 2. Differentiating `expr` using the chain rule with known derivatives
/// 3. Adding the resolved derivative to the map and repeating
///
/// This avoids promoting algebraic variables to states, which would create
/// redundant degrees of freedom and conflicting ODE/algebraic constraints.
pub fn compute_full_derivative_map(dae: &Dae) -> HashMap<String, Expression> {
    let mut der_map = build_der_value_map(dae);
    let defining_expr_index = collect_residual_defining_expr_index(dae);

    // Iteratively resolve algebraic variable derivatives
    // Each pass may resolve new variables that enable further resolution
    let max_iters = 20; // prevent infinite loops
    for _ in 0..max_iters {
        let mut new_entries = Vec::new();

        // Outputs are causal algebraics defined by their own block equations, so
        // their time derivatives are differentiable just like algebraics. They
        // must be resolved too: a `Modelica.Blocks.Continuous.Der` chain reads
        // `der(output)` (e.g. `der1.y = der(der1.u)` with `der1.u = Bessel.y`),
        // which only expands once `der(Bessel.y)` is in the map.
        for alg_name in dae
            .variables
            .algebraics
            .keys()
            .chain(dae.variables.outputs.keys())
        {
            if der_map.contains_key(alg_name.as_str()) {
                continue; // Already resolved
            }
            let derivative = defining_expr_candidates(&defining_expr_index, alg_name)
                .find_map(|expr| symbolic_time_derivative(expr, dae, &der_map));
            if let Some(d) = derivative {
                new_entries.push((alg_name.as_str().to_string(), d));
            }
        }

        if new_entries.is_empty() {
            break; // Fixed point reached
        }

        for (name, deriv) in new_entries {
            der_map.insert(name, deriv);
        }
    }

    der_map
}

/// Expand all `der()` calls in the DAE equations using chain-rule derivatives.
///
/// This pass:
/// 1. Builds a full derivative map (states + resolved algebraics)
/// 2. Substitutes `der(algebraic_var)` with its chain-rule derivative
/// 3. Expands compound `der(non-VarRef)` using the chain rule
///
/// After this pass, only `der(state)` calls remain (needed for mass matrix).
/// All `der(algebraic)` and `der(compound)` calls are replaced with algebraic
/// expressions. This prevents spurious state promotion.
pub fn expand_compound_derivatives(dae: &mut Dae) {
    let der_map = compute_full_derivative_map(dae);
    if der_map.is_empty() {
        return;
    }

    // Build set of state names — we keep der(state) intact
    let state_names: HashSet<String> = dae
        .variables
        .states
        .keys()
        .map(|n| n.as_str().to_string())
        .collect();

    let expanded: Vec<Expression> = dae
        .continuous
        .equations
        .iter()
        .map(|eq| expand_der_in_expr_full(&eq.rhs, dae, &der_map, &state_names))
        .collect();
    for (eq, new_rhs) in dae.continuous.equations.iter_mut().zip(expanded) {
        eq.rhs = new_rhs;
    }
}

/// Recursively collect names of algebraic variables that appear inside `der()`.
///
/// When `der(x)` appears in an equation but `x` is classified as algebraic,
/// the evaluator returns 0 for `der(x)` (derivatives are only populated for
/// states). This helper finds such variables so they can be promoted to states.
pub fn collect_der_of_algebraics(expr: &Expression, dae: &Dae, out: &mut Vec<VarName>) {
    DerOfAlgebraicCollector { dae, out }.visit_expression(expr);
}

struct DerOfAlgebraicCollector<'a> {
    dae: &'a Dae,
    out: &'a mut Vec<VarName>,
}

impl ExpressionVisitor for DerOfAlgebraicCollector<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if let Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } = expr
            && let Some(arg) = args.first()
        {
            let matches = self
                .dae
                .variables
                .algebraics
                .keys()
                .filter(|alg_name| expr_refers_to_var(arg, alg_name))
                .cloned();
            self.out.extend(matches);
        }
        self.walk_expression(expr);
    }
}

/// Promote algebraic variables whose derivatives appear in equations to states.
///
/// When `der(x)` appears in an equation but `x` is an algebraic variable,
/// the evaluator looks up `"der(x)"` in the environment and finds nothing,
/// returning 0.0. This makes equations like `v_rel = der(s_rel)` evaluate
/// to `v_rel = 0`, zeroing all velocity/damping terms.
///
/// After promotion, `reorder_equations_for_solver` will find the equation
/// containing `der(promoted_var)` and place it as an ODE row. The BDF solver
/// then correctly computes the derivative.
pub fn promote_der_algebraics_to_states(dae: &mut Dae) {
    let mut to_promote: Vec<VarName> = Vec::new();
    for eq in &dae.continuous.equations {
        collect_der_of_algebraics(&eq.rhs, dae, &mut to_promote);
    }

    // Deduplicate using a set (VarName doesn't impl Ord)
    let mut seen = HashSet::new();
    to_promote.retain(|n| seen.insert(n.as_str().to_string()));

    for name in &to_promote {
        if let Some(var) = dae.variables.algebraics.shift_remove(name) {
            dae.variables.states.insert(name.clone(), var);
        }
    }
}

/// Check if an equation is a derivative alias: `0 = alias_var - der(state)` or
/// `0 = der(state) - alias_var`. Returns the alias variable name if so.
pub fn try_extract_derivative_alias(eq: &Equation, state_name: &VarName) -> Option<VarName> {
    // Pattern: Binary { op: Sub, lhs, rhs } where one side is der(state)
    // and the other is a plain VarRef (the alias variable)
    let Expression::Binary { op, lhs, rhs, .. } = &eq.rhs else {
        return None;
    };
    if !matches!(op, OpBinary::Sub) {
        return None;
    }

    let is_der_of_state = |expr: &Expression| -> bool {
        matches!(
            expr,
            Expression::BuiltinCall { function: BuiltinFunction::Der, args, .. }
            if args.len() == 1 && expr_refers_to_var(&args[0], state_name)
        )
    };

    let plain_var_name = |expr: &Expression| -> Option<VarName> {
        match expr {
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => Some(name.var_name().clone()),
            _ => None,
        }
    };

    // 0 = alias - der(state)
    if is_der_of_state(rhs)
        && let Some(alias) = plain_var_name(lhs)
    {
        return Some(alias);
    }
    // 0 = der(state) - alias
    if is_der_of_state(lhs)
        && let Some(alias) = plain_var_name(rhs)
    {
        return Some(alias);
    }

    // Also handle negated forms: 0 = -(alias - der(state)) which shows up as
    // 0 = der(state) - alias (already covered above) or via Unary::Neg wrapping
    None
}

/// Recursively substitute all occurrences of `VarRef(old_name)` with `replacement`.
pub fn substitute_var_in_expr(
    expr: &Expression,
    old_name: &VarName,
    replacement: &Expression,
) -> Expression {
    VarSubstitutionRewriter {
        old_name,
        replacement,
    }
    .rewrite_expression(expr)
}

struct VarSubstitutionRewriter<'a> {
    old_name: &'a VarName,
    replacement: &'a Expression,
}

impl ExpressionRewriter for VarSubstitutionRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::VarRef {
                name, subscripts, ..
            } if name.var_name() == self.old_name && subscripts.is_empty() => {
                self.replacement.clone()
            }
            _ => self.walk_expression(expr),
        }
    }
}

/// Eliminate derivative-alias equations from the DAE.
///
/// Some flattened models produce equations like `0 = mass1.der_T - der(mass1.T)`
/// which alias an algebraic variable to a state derivative. When
/// `reorder_equations_for_solver` picks ONE equation per state as the ODE row,
/// the derivative-alias can end up as an algebraic equation. During residual
/// evaluation, `der(state)` evaluates to 0 (not populated in `build_env`),
/// creating false constraints.
///
/// This function:
/// 1. For each state, finds all equations containing `der(state)`
/// 2. If there are exactly 2 and one is a simple alias, substitutes the alias
///    variable with `der(state)` in all other equations
/// 3. Removes the alias equation and the alias variable from `algebraics`
pub fn eliminate_derivative_aliases(dae: &mut Dae) -> Result<(), StructuralError> {
    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    let mut alias_eqs_to_remove: Vec<usize> = Vec::new();
    let mut alias_vars_to_remove: Vec<VarName> = Vec::new();
    let mut substitutions: Vec<(VarName, Expression)> = Vec::new();

    for state_name in &state_names {
        // Find all equation indices containing der(state)
        let der_eq_indices: Vec<usize> = dae
            .continuous
            .equations
            .iter()
            .enumerate()
            .filter(|(_, eq)| expr_contains_der_of(&eq.rhs, state_name))
            .map(|(i, _)| i)
            .collect();

        let mut alias_candidates = Vec::new();
        let mut non_alias_derivative_rows = 0usize;
        for &idx in &der_eq_indices {
            let Some(var) =
                try_extract_derivative_alias(&dae.continuous.equations[idx], state_name)
            else {
                non_alias_derivative_rows += 1;
                continue;
            };
            if !dae.variables.algebraics.contains_key(&var) {
                non_alias_derivative_rows += 1;
                continue;
            }
            alias_candidates.push((idx, var));
        }

        if alias_candidates.is_empty() || non_alias_derivative_rows == 0 {
            continue;
        }

        let state_var = dae.variables.states.get(state_name).ok_or_else(|| {
            StructuralError::UnspannedContractViolation {
                reason: format!(
                    "state metadata missing while eliminating derivative alias for `{}`",
                    state_name.as_str()
                ),
            }
        })?;
        let der_expr = symbolic_der_var_ref_for_variable(state_var)?;

        for (alias_idx, alias_var) in alias_candidates {
            alias_eqs_to_remove.push(alias_idx);
            alias_vars_to_remove.push(alias_var.clone());
            substitutions.push((alias_var, der_expr.clone()));
        }
    }

    // MLS Appendix B / §16.5.1: eliminating a continuous derivative helper
    // must rewrite every runtime/event surface that can still read that helper.
    // Otherwise later sampled/event partitions can retain dangling sources such
    // as `sample(sample1.u)` after `sample1.u = der(x)` has been removed.
    for (old_name, replacement) in &substitutions {
        for eq in &mut dae.continuous.equations {
            eq.rhs = substitute_var_in_expr(&eq.rhs, old_name, replacement);
        }
        for eq in &mut dae.discrete.real_updates {
            eq.rhs = substitute_var_in_expr(&eq.rhs, old_name, replacement);
        }
        for eq in &mut dae.discrete.valued_updates {
            eq.rhs = substitute_var_in_expr(&eq.rhs, old_name, replacement);
        }
        for eq in &mut dae.conditions.equations {
            eq.rhs = substitute_var_in_expr(&eq.rhs, old_name, replacement);
        }
        for expr in &mut dae.conditions.relations {
            *expr = substitute_var_in_expr(expr, old_name, replacement);
        }
        for expr in &mut dae.events.synthetic_root_conditions {
            *expr = substitute_var_in_expr(expr, old_name, replacement);
        }
        for expr in &mut dae.clocks.constructor_exprs {
            *expr = substitute_var_in_expr(expr, old_name, replacement);
        }
    }

    // Remove alias equations (in reverse order to preserve indices)
    alias_eqs_to_remove.sort_unstable();
    alias_eqs_to_remove.dedup();
    for &idx in alias_eqs_to_remove.iter().rev() {
        dae.continuous.equations.remove(idx);
    }

    // Remove alias variables from algebraics
    for var_name in &alias_vars_to_remove {
        dae.variables.algebraics.shift_remove(var_name);
    }
    Ok(())
}

fn symbolic_der_var_ref(name: &VarName, span: Span) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![Expression::VarRef {
            name: rumoca_core::Reference::from_var_name(name.clone()),
            subscripts: vec![],
            span,
        }],
        span,
    }
}

fn symbolic_der_var_ref_for_variable(variable: &Variable) -> Result<Expression, StructuralError> {
    let span = required_variable_span(variable, "symbolic derivative reference")?;
    Ok(symbolic_der_var_ref(&variable.name, span))
}

fn required_variable_span(variable: &Variable, context: &str) -> Result<Span, StructuralError> {
    let span = variable.source_span;
    if span.is_dummy() {
        return Err(StructuralError::UnspannedContractViolation {
            reason: format!(
                "{context} for `{}` is missing source provenance",
                variable.name
            ),
        });
    }
    Ok(span)
}

pub fn build_relaxed_derivative_map(
    dae: &Dae,
) -> Result<HashMap<String, Expression>, StructuralError> {
    let mut map = build_der_value_map(dae);
    let defining_expr_index = collect_residual_defining_expr_index(dae);

    // For index-reduction differentiation, keep unknown derivatives symbolic
    // instead of failing the whole derivative expansion.
    for variable in dae
        .variables
        .states
        .values()
        .chain(dae.variables.algebraics.values())
        .chain(dae.variables.outputs.values())
        .chain(dae.variables.inputs.values())
    {
        match map.entry(variable.name.as_str().to_string()) {
            Entry::Occupied(_) => {}
            Entry::Vacant(entry) => {
                entry.insert(symbolic_der_var_ref_for_variable(variable)?);
            }
        }
    }

    let candidate_names = collect_all_relaxation_candidates(dae);
    relax_algebraic_derivative_map_to_fixed_point(
        dae,
        &defining_expr_index,
        &mut map,
        &candidate_names,
        true,
    );
    Ok(map)
}

pub fn symbolic_time_derivative_for_expr(
    dae: &Dae,
    expr: &Expression,
) -> Result<Option<Expression>, StructuralError> {
    let der_map = build_relaxed_derivative_map(dae)?;
    Ok(symbolic_time_derivative(expr, dae, &der_map))
}

fn derivative_states_in_eq(rhs: &Expression, state_names: &[VarName]) -> Vec<VarName> {
    state_names
        .iter()
        .filter(|state| expr_contains_der_of(rhs, state))
        .cloned()
        .collect()
}

fn state_has_standalone_der_equation(
    dae: &Dae,
    state_name: &VarName,
    state_names: &[VarName],
) -> Result<bool, StructuralError> {
    let required_rows = required_dae_variable_size(dae, state_name)?;
    let mut matched_rows = 0usize;
    for eq in &dae.continuous.equations {
        if let Some(alias) = try_extract_derivative_alias(eq, state_name)
            && !state_names.contains(&alias)
        {
            continue;
        }
        let der_states = derivative_states_in_eq(&eq.rhs, state_names);
        if der_states.len() == 1
            && der_states[0] == *state_name
            && try_extract_der_value(&eq.rhs, state_name)
                .is_some_and(|value| !expr_contains_der_of(&value, state_name))
        {
            matched_rows += residual_scalar_width(dae, &eq.rhs)?;
        }
    }
    Ok(matched_rows >= required_rows)
}

pub fn eq_contains_any_state_der(rhs: &Expression, state_names: &[VarName]) -> bool {
    let matcher = DerivativeNameMatcher::from_var_names(state_names);
    eq_contains_any_state_der_with_matcher(rhs, &matcher)
}

fn eq_contains_any_state_der_with_matcher(
    rhs: &Expression,
    matcher: &DerivativeNameMatcher,
) -> bool {
    expr_contains_der_of_any(rhs, matcher)
}

fn expr_contains_der_of_non_state(expr: &Expression, state_name_set: &HashSet<String>) -> bool {
    let mut checker = NonStateDerivativeChecker {
        state_name_set,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct NonStateDerivativeChecker<'a> {
    state_name_set: &'a HashSet<String>,
    found: bool,
}

impl ExpressionVisitor for NonStateDerivativeChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der {
            self.found = der_arg_is_not_plain_state(args, self.state_name_set);
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

fn der_arg_is_not_plain_state(args: &[Expression], state_name_set: &HashSet<String>) -> bool {
    if args.len() != 1 {
        return true;
    }
    match &args[0] {
        Expression::VarRef {
            name,
            subscripts: _,
            ..
        } => !state_name_set.contains(name.as_str()),
        _ => true,
    }
}

/// Direct-assignment demotion runs before scalarization. If a scalar state is
/// defined using an unsliced vector reference (e.g. `x = -i` where `i` is
/// array-valued), demotion is ambiguous and can corrupt index/alias structure.
fn expr_contains_unsliced_vector_ref(expr: &Expression, dae: &Dae) -> bool {
    let mut checker = UnslicedVectorRefChecker { dae, found: false };
    checker.visit_expression(expr);
    checker.found
}

struct UnslicedVectorRefChecker<'a> {
    dae: &'a Dae,
    found: bool,
}

impl ExpressionVisitor for UnslicedVectorRefChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
        if subscripts.is_empty()
            && dae_variable_size(self.dae, name.var_name())
                .is_ok_and(|size| size.is_some_and(|size| size > 1))
        {
            self.found = true;
            return;
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

pub fn try_extract_state_alias_pair(rhs: &Expression) -> Option<(VarName, VarName)> {
    let Expression::Binary { op, lhs, rhs, .. } = rhs else {
        return None;
    };
    if !matches!(op, OpBinary::Sub) {
        return None;
    }
    let Expression::VarRef {
        name: lhs_name,
        subscripts: lhs_subscripts,
        span: _,
    } = lhs.as_ref()
    else {
        return None;
    };
    let Expression::VarRef {
        name: rhs_name,
        subscripts: rhs_subscripts,
        span: _,
    } = rhs.as_ref()
    else {
        return None;
    };
    if !lhs_subscripts.is_empty() || !rhs_subscripts.is_empty() {
        return None;
    }
    Some((lhs_name.var_name().clone(), rhs_name.var_name().clone()))
}

fn state_select_rank(state_select: rumoca_core::StateSelect) -> u8 {
    match state_select {
        rumoca_core::StateSelect::Never => 0,
        rumoca_core::StateSelect::Avoid => 1,
        rumoca_core::StateSelect::Default => 2,
        rumoca_core::StateSelect::Prefer => 3,
        rumoca_core::StateSelect::Always => 4,
    }
}

fn choose_exact_alias_state_representative<'a>(
    dae: &'a Dae,
    component_states: &'a [VarName],
) -> Option<&'a VarName> {
    component_states
        .iter()
        .filter_map(|name| dae.variables.states.get(name).map(|var| (name, var)))
        .min_by_key(|(name, var)| {
            (
                Reverse(state_select_rank(var.state_select)),
                Reverse(u8::from(var.fixed == Some(true))),
                Reverse(u8::from(var.start.is_some())),
                name.as_str().to_string(),
            )
        })
        .map(|(name, _)| name)
}

fn exact_alias_member_variable<'a>(dae: &'a Dae, name: &VarName) -> Option<&'a Variable> {
    dae.variables
        .states
        .get(name)
        .or_else(|| dae.variables.algebraics.get(name))
        .or_else(|| dae.variables.outputs.get(name))
}

fn propagate_exact_alias_member_metadata_to_canonical_state(
    dae: &mut Dae,
    component_members: &[VarName],
    canonical_state: &VarName,
) {
    let donor = component_members
        .iter()
        .filter(|name| *name != canonical_state)
        .filter_map(|name| exact_alias_member_variable(dae, name).map(|var| (name, var)))
        .filter(|(_, var)| var.fixed == Some(true) || var.start.is_some())
        .min_by_key(|(name, var)| {
            (
                Reverse(u8::from(var.fixed == Some(true))),
                Reverse(u8::from(var.start.is_some())),
                name.as_str().to_string(),
            )
        })
        .map(|(_, var)| (var.fixed, var.start.clone()));

    let Some(canonical_var) = dae.variables.states.get_mut(canonical_state) else {
        return;
    };
    let Some((donor_fixed, donor_start)) = donor else {
        return;
    };

    if canonical_var.fixed.is_none() && donor_fixed == Some(true) {
        canonical_var.fixed = donor_fixed;
    }
    if canonical_var.start.is_none() && donor_start.is_some() {
        canonical_var.start = donor_start;
    }
}

fn rewrite_component_member_derivatives_in_equations(
    equations: &mut [Equation],
    member_name: &VarName,
    replacement: &Expression,
) {
    let state_dims = None;
    for eq in equations {
        eq.rhs = substitute_der_of_state(&eq.rhs, member_name, replacement, &state_dims);
    }
}

fn rewrite_component_member_derivatives_in_exprs(
    exprs: &mut [Expression],
    member_name: &VarName,
    replacement: &Expression,
) {
    let state_dims = None;
    for expr in exprs {
        *expr = substitute_der_of_state(expr, member_name, replacement, &state_dims);
    }
}

/// Demote duplicate states connected only through exact alias equalities.
///
/// MLS simple equality equations and generated connection equations express
/// exact value equality. If a component of exact `a = b` aliases contains a
/// state, all `der(member)` references in that component must observe the same
/// trajectory. Rumoca therefore rewrites `der(alias_member)` to the canonical
/// state early, and if the component contains multiple states it demotes the
/// duplicates before derivative-alias cleanup runs.
fn push_component_neighbor_if_unvisited(
    visited: &mut HashSet<String>,
    stack: &mut Vec<String>,
    component: &mut Vec<String>,
    neighbor: &str,
) {
    let neighbor = neighbor.to_string();
    if !visited.insert(neighbor.clone()) {
        return;
    }
    stack.push(neighbor.clone());
    component.push(neighbor);
}

fn rewrite_exact_alias_component_member_derivatives(
    dae: &mut Dae,
    component_members: &[VarName],
    canonical_state: &VarName,
) -> Result<(), StructuralError> {
    let canonical_var = dae.variables.states.get(canonical_state).ok_or_else(|| {
        StructuralError::UnspannedContractViolation {
            reason: format!(
                "canonical state metadata missing while rewriting derivative aliases for `{}`",
                canonical_state.as_str()
            ),
        }
    })?;
    let replacement = symbolic_der_var_ref_for_variable(canonical_var)?;
    for member_name in component_members {
        if *member_name == *canonical_state {
            continue;
        }
        rewrite_component_member_derivatives_in_equations(
            &mut dae.continuous.equations,
            member_name,
            &replacement,
        );
        rewrite_component_member_derivatives_in_equations(
            &mut dae.discrete.real_updates,
            member_name,
            &replacement,
        );
        rewrite_component_member_derivatives_in_equations(
            &mut dae.discrete.valued_updates,
            member_name,
            &replacement,
        );
        rewrite_component_member_derivatives_in_equations(
            &mut dae.initialization.equations,
            member_name,
            &replacement,
        );
        rewrite_component_member_derivatives_in_exprs(
            &mut dae.conditions.relations,
            member_name,
            &replacement,
        );
        rewrite_component_member_derivatives_in_exprs(
            &mut dae.events.synthetic_root_conditions,
            member_name,
            &replacement,
        );
        rewrite_component_member_derivatives_in_exprs(
            &mut dae.clocks.triggered_conditions,
            member_name,
            &replacement,
        );
        rewrite_component_member_derivatives_in_exprs(
            &mut dae.clocks.constructor_exprs,
            member_name,
            &replacement,
        );
    }
    Ok(())
}

pub fn demote_exact_alias_component_states(dae: &mut Dae) -> Result<usize, StructuralError> {
    let alias_pairs: Vec<(VarName, VarName)> = dae
        .continuous
        .equations
        .iter()
        .filter_map(|eq| try_extract_state_alias_pair(&eq.rhs))
        .filter(|(a, b)| a != b)
        .collect();
    if alias_pairs.is_empty() {
        return Ok(0);
    }

    let mut adjacency: HashMap<String, HashSet<String>> = HashMap::new();
    for (a, b) in &alias_pairs {
        adjacency
            .entry(a.as_str().to_string())
            .or_default()
            .insert(b.as_str().to_string());
        adjacency
            .entry(b.as_str().to_string())
            .or_default()
            .insert(a.as_str().to_string());
    }

    let mut nodes: Vec<String> = adjacency.keys().cloned().collect();
    nodes.sort();
    let mut visited = HashSet::new();
    let mut demotions = Vec::new();

    for root in nodes {
        if !visited.insert(root.clone()) {
            continue;
        }

        let mut stack = vec![root.clone()];
        let mut component = vec![root];
        while let Some(node) = stack.pop() {
            let Some(neighbors) = adjacency.get(&node) else {
                continue;
            };
            for neighbor in neighbors {
                push_component_neighbor_if_unvisited(
                    &mut visited,
                    &mut stack,
                    &mut component,
                    neighbor,
                );
            }
        }

        let mut component_members: Vec<VarName> = component
            .iter()
            .map(|name| VarName::new(name.clone()))
            .collect();
        component_members.sort_by(|a, b| a.as_str().cmp(b.as_str()));

        let mut component_states: Vec<VarName> = component_members
            .iter()
            .filter_map(|name| dae.variables.states.get_key_value(name))
            .map(|(name, _)| name.clone())
            .collect();
        if component_states.is_empty() {
            continue;
        }
        component_states.sort_by(|a, b| a.as_str().cmp(b.as_str()));

        let Some(canonical_state) = choose_exact_alias_state_representative(dae, &component_states)
        else {
            continue;
        };
        let canonical_state = canonical_state.clone();
        propagate_exact_alias_member_metadata_to_canonical_state(
            dae,
            &component_members,
            &canonical_state,
        );

        rewrite_exact_alias_component_member_derivatives(
            dae,
            &component_members,
            &canonical_state,
        )?;

        for state_name in component_states {
            if state_name != canonical_state {
                demotions.push((state_name, canonical_state.clone()));
            }
        }
    }

    let mut demoted = 0usize;
    for (state_name, _canonical_state) in demotions {
        if let Some(var) = dae.variables.states.shift_remove(&state_name) {
            dae.variables.algebraics.insert(state_name, var);
            demoted += 1;
        }
    }

    Ok(demoted)
}

/// Demote remaining no-der states that are still exact aliases of non-state
/// unknowns after exact alias components have been collapsed.
///
/// MLS §8 simple equalities define exact alias relations, but
/// [`demote_exact_alias_component_states`] already chooses one state
/// representative per multi-state alias component earlier in prepare. The only
/// remaining structural case here is `state = non_state` with no standalone
/// `der(state)` row.
pub fn demote_alias_states_without_der(dae: &mut Dae) -> Result<usize, StructuralError> {
    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    if state_names.is_empty() {
        return Ok(0);
    }

    let state_name_set: HashSet<String> = state_names
        .iter()
        .map(|name| name.as_str().to_string())
        .collect();
    let mut has_der: HashMap<String, bool> = HashMap::new();
    for name in &state_names {
        has_der.insert(
            name.as_str().to_string(),
            state_has_standalone_der_equation(dae, name, &state_names)?,
        );
    }

    let mut adjacency: HashMap<String, HashSet<String>> = HashMap::new();
    for (a, b) in dae
        .continuous
        .equations
        .iter()
        .filter_map(|eq| try_extract_state_alias_pair(&eq.rhs))
    {
        if !(state_name_set.contains(a.as_str()) || state_name_set.contains(b.as_str())) {
            continue;
        }
        adjacency
            .entry(a.as_str().to_string())
            .or_default()
            .insert(b.as_str().to_string());
        adjacency
            .entry(b.as_str().to_string())
            .or_default()
            .insert(a.as_str().to_string());
    }
    if adjacency.is_empty() {
        return Ok(0);
    }

    let mut visited = HashSet::new();
    let mut to_demote = HashSet::new();
    for state_name in &state_names {
        let start = state_name.as_str().to_string();
        if visited.contains(&start) || !adjacency.contains_key(&start) {
            continue;
        }
        let component = collect_alias_connected_names(&adjacency, &start);
        visited.extend(component.iter().cloned());
        let mut component_has_der = false;
        for name in &component {
            if state_name_set.contains(name.as_str()) && has_der_for_state_name(&has_der, name)? {
                component_has_der = true;
                break;
            }
        }
        for name in component {
            if !state_name_set.contains(name.as_str()) {
                continue;
            }
            if !component_has_der || !has_der_for_state_name(&has_der, name.as_str())? {
                to_demote.insert(name);
            }
        }
    }

    let mut demoted = 0usize;
    let mut names_to_demote: Vec<String> = to_demote.into_iter().collect();
    names_to_demote.sort();
    for name in names_to_demote.into_iter().map(VarName::new) {
        if let Some(var) = dae.variables.states.shift_remove(&name) {
            dae.variables.algebraics.insert(name.clone(), var);
            demoted += 1;
        }
    }
    Ok(demoted)
}

fn has_der_for_state_name(
    has_der: &HashMap<String, bool>,
    name: &str,
) -> Result<bool, StructuralError> {
    has_der
        .get(name)
        .copied()
        .ok_or_else(|| StructuralError::UnspannedContractViolation {
            reason: format!("state derivative metadata missing for `{name}`"),
        })
}

fn collect_alias_connected_names(
    adjacency: &HashMap<String, HashSet<String>>,
    start: &str,
) -> HashSet<String> {
    let mut component = HashSet::from([start.to_string()]);
    let mut stack = vec![start.to_string()];
    while let Some(name) = stack.pop() {
        for neighbor in adjacency.get(&name).into_iter().flatten() {
            if component.insert(neighbor.clone()) {
                stack.push(neighbor.clone());
            }
        }
    }
    component
}

/// Demote states that appear only in coupled-derivative equations (rows with
/// derivatives of multiple states) and have no standalone derivative row.
///
/// Coupled derivative rows are now supported through the dense ODE-block mass
/// matrix, so this pass intentionally keeps states intact.
pub fn demote_coupled_derivative_states(dae: &mut Dae) -> usize {
    let _ = dae;
    0
}

fn extract_state_direct_assignment(
    rhs: &Expression,
    state_name_set: &HashSet<String>,
) -> Option<(VarName, Expression)> {
    match rhs {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            if let Expression::VarRef {
                name, subscripts, ..
            } = lhs.as_ref()
                && subscripts.is_empty()
                && state_name_set.contains(name.as_str())
            {
                return Some((name.var_name().clone(), *rhs.clone()));
            }
            if let Expression::VarRef {
                name, subscripts, ..
            } = rhs.as_ref()
                && subscripts.is_empty()
                && state_name_set.contains(name.as_str())
            {
                return Some((name.var_name().clone(), *lhs.clone()));
            }
            None
        }
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => extract_state_direct_assignment(rhs, state_name_set),
        _ => None,
    }
}

fn extract_state_direct_assignment_equation(
    eq: &Equation,
    state_names: &[VarName],
    state_name_set: &HashSet<String>,
) -> Option<(VarName, Expression)> {
    if let Some(lhs) = &eq.lhs {
        return state_name_set
            .contains(lhs.as_str())
            .then(|| (lhs.var_name().clone(), eq.rhs.clone()));
    }
    // Defining expressions may read `der(<other state>)` (differentiator
    // chains such as `y = der(x)` behind a closed-form ODE for `x`); the
    // per-candidate gates below and in `direct_demotion_plan_for_equation`
    // reject the unsafe cases (self-derivatives, derivative definitions that
    // feed back through the candidate).
    if let Some(pair) = extract_state_direct_assignment(&eq.rhs, state_name_set)
        && !expr_contains_der_of_state_or_component(&pair.1, &pair.0)
    {
        return Some(pair);
    }

    // Residual form: 0 = expr. If expr is affine in exactly one state with
    // coefficient ±1, solve for that state.
    let mut solved: Option<(VarName, Expression)> = None;
    for state_name in state_names {
        if !expr_contains_var(&eq.rhs, state_name) {
            continue;
        }
        if expr_contains_der_of_state_or_component(&eq.rhs, state_name) {
            continue;
        }
        let Some((coef, remainder)) = split_linear_target(&eq.rhs, state_name, eq.span) else {
            continue;
        };
        let defining_expr = match coef {
            1 => sub_expr(zero_expr(eq.span), remainder, eq.span),
            -1 => remainder,
            _ => continue,
        };
        if solved.is_some() {
            return None;
        }
        solved = Some((state_name.clone(), defining_expr));
    }
    solved
}

fn expr_contains_der_of_state_or_component(expr: &Expression, state_name: &VarName) -> bool {
    let matcher = DerivativeNameMatcher::from_var_names(std::slice::from_ref(state_name));
    expr_contains_der_of_any(expr, &matcher)
}

fn variable_dims_for_direct_demotion(dae: &Dae, state_name: &VarName) -> Option<Vec<i64>> {
    dae.variables
        .states
        .get(state_name)
        .map(|state| state.dims.clone())
        .filter(|dims| !dims.is_empty())
}

fn der_call_target_subscripts<'a>(
    expr: &'a Expression,
    state_name: &VarName,
) -> Option<Option<&'a [Subscript]>> {
    let Expression::BuiltinCall { function, args, .. } = expr else {
        return None;
    };
    if *function != BuiltinFunction::Der || args.len() != 1 {
        return None;
    }
    let Expression::VarRef {
        name, subscripts, ..
    } = &args[0]
    else {
        return expr_refers_to_var(&args[0], state_name).then_some(None);
    };
    if name.var_name() != state_name {
        return None;
    }
    if subscripts.is_empty() {
        Some(None)
    } else {
        Some(Some(subscripts.as_slice()))
    }
}

fn substitute_der_of_state(
    expr: &Expression,
    state_name: &VarName,
    replacement: &Expression,
    state_dims: &Option<Vec<i64>>,
) -> Expression {
    DerSubstitutionRewriter {
        state_name,
        replacement,
        state_dims,
    }
    .rewrite_expression(expr)
}

/// Replace every `der(<state>)` sub-expression with a zero literal.
///
/// Used to scan a defining expression for unsafe dependencies *outside* its
/// derivative-reader links: a `der(state)` link is substituted symbolically on
/// demotion (validated separately), so its state reference must not count as
/// a value dependence on that state.
fn mask_state_der_calls(expr: &Expression, state_name_set: &HashSet<String>) -> Expression {
    struct StateDerMasker<'a> {
        state_name_set: &'a HashSet<String>,
    }
    impl ExpressionRewriter for StateDerMasker<'_> {
        fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
            if let Expression::BuiltinCall {
                function,
                args,
                span,
            } = expr
                && *function == BuiltinFunction::Der
                && args.len() == 1
                && let Expression::VarRef {
                    name, subscripts, ..
                } = &args[0]
                && subscripts.is_empty()
                && self.state_name_set.contains(name.as_str())
            {
                return zero_expr(*span);
            }
            self.walk_expression(expr)
        }
    }
    StateDerMasker { state_name_set }.rewrite_expression(expr)
}

struct DerSubstitutionRewriter<'a> {
    state_name: &'a VarName,
    replacement: &'a Expression,
    state_dims: &'a Option<Vec<i64>>,
}

impl ExpressionRewriter for DerSubstitutionRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match der_call_target_subscripts(expr, self.state_name) {
            Some(None) => self.replacement.clone(),
            Some(Some(subscripts)) => {
                let projected = self
                    .state_dims
                    .as_deref()
                    .and_then(|dims| static_subscript_indices(subscripts).zip(Some(dims)))
                    .and_then(|(indices, dims)| {
                        flat_index_from_indices(dims, &indices).zip(Some(dims))
                    })
                    .and_then(|(flat_index, dims)| {
                        project_flat_index_with_span(
                            self.replacement,
                            dims,
                            flat_index,
                            expr.span(),
                        )
                    });
                projected.unwrap_or_else(|| self.walk_expression(expr))
            }
            None => self.walk_expression(expr),
        }
    }
}

#[derive(Clone)]
struct DirectStateDemotionPlan {
    state_name: VarName,
    der_expr: Expression,
}

#[derive(Default)]
struct DirectDemotionCounters {
    n_candidates: usize,
    n_skip_flow_sum_origin: usize,
    n_skip_unsafe_non_state_alias: usize,
    n_skip_when_assigned: usize,
    n_skip_self_der: usize,
    n_skip_der_in_defining_expr: usize,
    n_skip_unsliced_vector_ref: usize,
    n_skip_extra_state_refs: usize,
    n_skip_non_state_der: usize,
    n_skip_no_der_expr: usize,
    n_trace_logged_candidates: usize,
}

struct DirectDemotionRound<'a> {
    dae: &'a Dae,
    state_names: Vec<VarName>,
    state_name_set: HashSet<String>,
    when_assigned_states: HashSet<String>,
    non_state_unknown_names: HashSet<String>,
    non_state_defining_exprs: DefiningExprIndex,
    trace: bool,
}

impl<'a> DirectDemotionRound<'a> {
    fn new(dae: &'a Dae, trace: bool) -> Result<Option<Self>, StructuralError> {
        let timer = structural_timing_start("direct_demotion.round_context");
        let (state_names, state_name_set, when_assigned_states) =
            match direct_demotion_round_context(dae) {
                Some(context) => context,
                None => return Ok(None),
            };
        structural_timing_done("direct_demotion.round_context", timer);
        let timer = structural_timing_start("direct_demotion.non_state_unknown_names");
        let non_state_unknown_names = collect_non_state_continuous_unknown_names(dae);
        let non_state_defining_exprs = collect_non_derivative_defining_expr_index(dae);
        structural_timing_done("direct_demotion.non_state_unknown_names", timer);
        Ok(Some(Self {
            dae,
            state_names,
            state_name_set,
            when_assigned_states,
            non_state_unknown_names,
            non_state_defining_exprs,
            trace,
        }))
    }

    fn state_count(&self) -> usize {
        self.state_name_set.len()
    }
}

fn log_direct_assignment_candidate(
    trace: bool,
    counters: &mut DirectDemotionCounters,
    dae: &Dae,
    eq: &Equation,
    state_name: &VarName,
) {
    if !trace || counters.n_trace_logged_candidates >= 8 {
        return;
    }
    let state_select = dae
        .variables
        .states
        .get(state_name)
        .map(|var| format!("{:?}", var.state_select))
        .unwrap_or_else(|| "Unknown".to_string());
    crate::structural_trace!(
        "[sim-trace] direct-assignment candidate state={} state_select={} origin='{}' rhs={}",
        state_name.as_str(),
        state_select,
        eq.origin,
        truncate_debug(&format!("{:?}", eq.rhs), 180)
    );
    counters.n_trace_logged_candidates += 1;
}

fn choose_derivative_replacement(
    defining_expr: &Expression,
    state_name_set: &HashSet<String>,
    dae: &Dae,
    der_map: &HashMap<String, Expression>,
    counters: &mut DirectDemotionCounters,
) -> Option<Expression> {
    let Some(symbolic) = symbolic_time_derivative(defining_expr, dae, der_map) else {
        counters.n_skip_no_der_expr += 1;
        return None;
    };

    if expr_contains_der_of_non_state(&symbolic, state_name_set) {
        counters.n_skip_non_state_der += 1;
        return None;
    }

    Some(symbolic)
}

fn direct_demotion_round_context(
    dae: &Dae,
) -> Option<(Vec<VarName>, HashSet<String>, HashSet<String>)> {
    let state_names: Vec<VarName> = dae.variables.states.keys().cloned().collect();
    let state_name_set: HashSet<String> = dae
        .variables
        .states
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    if state_name_set.is_empty() {
        return None;
    }
    let when_assigned_states: HashSet<String> = dae
        .discrete
        .real_updates
        .iter()
        .chain(dae.discrete.valued_updates.iter())
        .filter_map(|eq| eq.lhs.as_ref())
        .map(|name| name.as_str().to_string())
        .filter(|name| state_name_set.contains(name))
        .collect();
    Some((state_names, state_name_set, when_assigned_states))
}

/// Apply structural dummy-derivative reduction for constrained states.
///
/// The source DAE initially marks every variable that appears under `der()` as
/// a state. Models with position constraints can therefore contain dependent
/// states. This pass selects states already identified by constrained-dummy
/// analysis, differentiates the defining constraint, substitutes `der(dummy)`,
/// and moves the dummy variable to the algebraic partition before BLT.
pub fn reduce_constrained_dummy_derivatives(dae: &mut Dae) -> Result<usize, StructuralError> {
    let mut total_demoted = 0usize;

    loop {
        let definitions = constrained_dummy_state_defining_exprs(dae);
        crate::structural_trace!(
            "[sim-trace] constrained-dummy scan: candidates={:?}",
            definitions.keys().collect::<Vec<_>>()
        );
        if definitions.is_empty() {
            break;
        }

        let mut demoted_this_round = false;
        for (candidate, definition) in definitions {
            let Some((_state_names, state_name_set, _when_assigned_states)) =
                direct_demotion_round_context(dae)
            else {
                return Ok(total_demoted);
            };
            let state_name = VarName::new(candidate);
            if !dae.variables.states.contains_key(&state_name) {
                continue;
            }
            let seed_exprs = vec![definition.defining_expr.clone()];
            let der_map = build_relaxed_derivative_map_for_exprs(dae, &seed_exprs)?;
            let Some(plan) = constrained_dummy_derivative_plan(
                dae,
                &state_name,
                &definition.defining_expr,
                &state_name_set,
                &der_map,
            ) else {
                crate::structural_trace!(
                    "[sim-trace] constrained-dummy plan rejected state={}",
                    state_name.as_str()
                );
                continue;
            };
            crate::structural_trace!(
                "[sim-trace] constrained-dummy demoting state={} structural_params={:?} defining={} der_expr={}",
                state_name.as_str(),
                definition.structural_params,
                truncate_debug(&format!("{:?}", definition.defining_expr), 400),
                truncate_debug(&format!("{:?}", plan.der_expr), 400)
            );
            total_demoted += direct_demotion::apply_direct_demotion_plan(dae, &plan);
            pin_structural_params(dae, &definition.structural_params);
            demoted_this_round = true;
            break;
        }
        if !demoted_this_round {
            break;
        }
    }

    Ok(total_demoted)
}

fn constrained_dummy_derivative_plan(
    dae: &Dae,
    state_name: &VarName,
    defining_expr: &Expression,
    state_name_set: &HashSet<String>,
    der_map: &HashMap<String, Expression>,
) -> Option<DirectStateDemotionPlan> {
    let der_expr = symbolic_time_derivative(defining_expr, dae, der_map)?;
    if expr_contains_der_of(&der_expr, state_name)
        || expr_contains_der_of_non_state(&der_expr, state_name_set)
    {
        return None;
    }
    Some(DirectStateDemotionPlan {
        state_name: state_name.clone(),
        der_expr,
    })
}

#[cfg(test)]
mod dae_prepare_demotion_tests;

/// Pin parameters whose compile-time values the constrained-dummy reduction
/// baked into substituted derivative expressions: runtime tuning of them
/// would silently disagree with the reduction.
fn pin_structural_params(dae: &mut rumoca_ir_dae::Dae, params: &[String]) {
    for param in params {
        if let Some(var) = dae
            .variables
            .parameters
            .get_mut(&VarName::new(param.as_str()))
        {
            var.is_tunable = false;
        }
    }
}
