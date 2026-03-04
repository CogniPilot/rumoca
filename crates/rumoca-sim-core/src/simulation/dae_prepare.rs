use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;

type BuiltinFunction = dae::BuiltinFunction;
type Dae = dae::Dae;
type Equation = dae::Equation;
type Expression = dae::Expression;
type Literal = dae::Literal;
type OpBinary = dae::OpBinary;
type OpUnary = dae::OpUnary;
type Subscript = dae::Subscript;
type VarName = dae::VarName;
type Variable = dae::Variable;

mod symbolic;
use symbolic::{
    build_der_value_map, expand_der_in_expr_full, symbolic_time_derivative, truncate_debug,
};

fn scalar_subscript_string(sub: &dae::Subscript) -> Option<String> {
    match sub {
        dae::Subscript::Index(i) => Some(i.to_string()),
        dae::Subscript::Expr(expr) => match expr.as_ref() {
            dae::Expression::Literal(dae::Literal::Integer(i)) => Some(i.to_string()),
            dae::Expression::Literal(dae::Literal::Real(v))
                if v.is_finite() && v.fract() == 0.0 =>
            {
                Some((*v as i64).to_string())
            }
            _ => None,
        },
        _ => None,
    }
}

fn append_subscripts(base: String, subscripts: &[dae::Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(base);
    }
    let mut idx = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        idx.push(scalar_subscript_string(sub)?);
    }
    Some(format!("{base}[{}]", idx.join(",")))
}

fn expr_exact_name(expr: &dae::Expression) -> Option<String> {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            append_subscripts(name.as_str().to_string(), subscripts)
        }
        dae::Expression::Index { base, subscripts } => {
            let base_name = expr_exact_name(base)?;
            append_subscripts(base_name, subscripts)
        }
        dae::Expression::FieldAccess { base, field } => {
            let base_name = expr_exact_name(base)?;
            Some(format!("{base_name}.{field}"))
        }
        _ => None,
    }
}

fn expr_base_name(expr: &dae::Expression) -> Option<String> {
    match expr {
        dae::Expression::VarRef { name, .. } => dae::component_base_name(name.as_str()),
        dae::Expression::Index { base, .. } => expr_base_name(base),
        dae::Expression::FieldAccess { base, field } => {
            let base_name = expr_base_name(base)?;
            Some(format!("{base_name}.{field}"))
        }
        _ => None,
    }
}

pub fn expr_refers_to_var(expr: &dae::Expression, var_name: &dae::VarName) -> bool {
    if let Some(expr_exact) = expr_exact_name(expr)
        && expr_exact == var_name.as_str()
    {
        return true;
    }

    // For indexed targets, require exact index/path match. This avoids
    // cross-index collisions (e.g. x[1] incorrectly matching x[2]).
    if var_name.as_str().contains('[') {
        return false;
    }

    let Some(expr_base) = expr_base_name(expr) else {
        return false;
    };
    let Some(var_base) = dae::component_base_name(var_name.as_str()) else {
        return false;
    };
    expr_base == var_base
}

pub fn expr_contains_der_of(expr: &dae::Expression, var_name: &dae::VarName) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            if *function == dae::BuiltinFunction::Der
                && args
                    .first()
                    .is_some_and(|a| expr_refers_to_var(a, var_name))
            {
                return true;
            }
            args.iter().any(|a| expr_contains_der_of(a, var_name))
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_contains_der_of(lhs, var_name) || expr_contains_der_of(rhs, var_name)
        }
        dae::Expression::Unary { rhs, .. } => expr_contains_der_of(rhs, var_name),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(c, e)| {
                expr_contains_der_of(c, var_name) || expr_contains_der_of(e, var_name)
            }) || expr_contains_der_of(else_branch, var_name)
        }
        dae::Expression::FunctionCall { args, .. } => {
            args.iter().any(|a| expr_contains_der_of(a, var_name))
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(|e| expr_contains_der_of(e, var_name))
        }
        dae::Expression::FieldAccess { base, .. } => expr_contains_der_of(base, var_name),
        dae::Expression::Index { base, .. } => expr_contains_der_of(base, var_name),
        _ => false,
    }
}

fn sim_trace_enabled() -> bool {
    std::env::var("RUMOCA_SIM_TRACE").is_ok() || std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
}

/// Try to extract the defining expression for an algebraic variable.
///
/// Looks for equations of the form `0 = var - expr` or `0 = expr - var`
/// and returns `expr` (the value that `var` equals).
pub fn zero_expr() -> Expression {
    Expression::Literal(Literal::Real(0.0))
}

pub fn add_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Add(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

pub fn sub_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn div_expr(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Div(Default::default()),
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

fn extract_scaled_target(expr: &Expression, target: &VarName) -> Option<Expression> {
    let Expression::Binary { op, lhs, rhs } = expr else {
        return None;
    };
    if !matches!(op, OpBinary::Mul(_) | OpBinary::MulElem(_)) {
        return None;
    }
    let lhs_is_target = matches!(lhs.as_ref(), Expression::VarRef { name, subscripts } if name == target && subscripts.is_empty());
    let rhs_is_target = matches!(rhs.as_ref(), Expression::VarRef { name, subscripts } if name == target && subscripts.is_empty());
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
pub fn split_linear_target(expr: &Expression, target: &VarName) -> Option<(i32, Expression)> {
    if expr_refers_to_var(expr, target) {
        return Some((1, zero_expr()));
    }

    let Expression::Binary { op, lhs, rhs } = expr else {
        return None;
    };
    match op {
        OpBinary::Add(_) | OpBinary::AddElem(_) => {
            if let Some((coef, rem)) = split_linear_target(lhs, target)
                && !expr_contains_var(rhs, target)
            {
                return Some((coef, add_expr(rem, *rhs.clone())));
            }
            if let Some((coef, rem)) = split_linear_target(rhs, target)
                && !expr_contains_var(lhs, target)
            {
                return Some((coef, add_expr(*lhs.clone(), rem)));
            }
            None
        }
        OpBinary::Sub(_) | OpBinary::SubElem(_) => {
            if let Some((coef, rem)) = split_linear_target(lhs, target)
                && !expr_contains_var(rhs, target)
            {
                return Some((coef, sub_expr(rem, *rhs.clone())));
            }
            if let Some((coef, rem)) = split_linear_target(rhs, target)
                && !expr_contains_var(lhs, target)
            {
                return Some((-coef, sub_expr(*lhs.clone(), rem)));
            }
            None
        }
        _ => None,
    }
}

fn extract_defining_expr(eq: &Equation, alg_name: &VarName) -> Option<Expression> {
    let Expression::Binary { op, lhs, rhs } = &eq.rhs else {
        return None;
    };
    if !matches!(op, OpBinary::Sub(_)) {
        return None;
    }

    let is_var = |e: &Expression| -> bool {
        matches!(e, Expression::VarRef { name, subscripts, .. }
            if name == alg_name && subscripts.is_empty())
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
        return Some(div_expr(*rhs.clone(), coeff));
    }
    if rhs_has && let Some(coeff) = extract_scaled_target(rhs, alg_name) {
        // lhs - (coeff*x) = 0  =>  x = lhs/coeff
        return Some(div_expr(*lhs.clone(), coeff));
    }
    if lhs_has && let Some((coef, lhs_rem)) = split_linear_target(lhs, alg_name) {
        // (coef*x + lhs_rem) - rhs = 0  =>  x = (rhs - lhs_rem)/coef
        return Some(match coef {
            1 => sub_expr(*rhs.clone(), lhs_rem),
            -1 => sub_expr(lhs_rem, *rhs.clone()),
            _ => return None,
        });
    }
    if rhs_has && let Some((coef, rhs_rem)) = split_linear_target(rhs, alg_name) {
        // lhs - (coef*x + rhs_rem) = 0  =>  x = (lhs - rhs_rem)/coef
        return Some(match coef {
            1 => sub_expr(*lhs.clone(), rhs_rem),
            -1 => sub_expr(rhs_rem, *lhs.clone()),
            _ => return None,
        });
    }
    None
}

fn find_defining_expr_candidates(dae: &Dae, alg_name: &VarName) -> Vec<Expression> {
    dae.f_x
        .iter()
        .filter_map(|eq| extract_defining_expr(eq, alg_name))
        .collect()
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

    // Iteratively resolve algebraic variable derivatives
    // Each pass may resolve new variables that enable further resolution
    let max_iters = 20; // prevent infinite loops
    for _ in 0..max_iters {
        let mut new_entries = Vec::new();

        for alg_name in dae.algebraics.keys() {
            if der_map.contains_key(alg_name.as_str()) {
                continue; // Already resolved
            }
            let derivative = find_defining_expr_candidates(dae, alg_name)
                .into_iter()
                .find_map(|expr| symbolic_time_derivative(&expr, dae, &der_map));
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
    let state_names: HashSet<String> = dae.states.keys().map(|n| n.as_str().to_string()).collect();

    let expanded: Vec<Expression> = dae
        .f_x
        .iter()
        .map(|eq| expand_der_in_expr_full(&eq.rhs, dae, &der_map, &state_names))
        .collect();
    for (eq, new_rhs) in dae.f_x.iter_mut().zip(expanded) {
        eq.rhs = new_rhs;
    }
}

/// Recursively collect names of algebraic variables that appear inside `der()`.
///
/// When `der(x)` appears in an equation but `x` is classified as algebraic,
/// the evaluator returns 0 for `der(x)` (derivatives are only populated for
/// states). This helper finds such variables so they can be promoted to states.
pub fn collect_der_of_algebraics(expr: &Expression, dae: &Dae, out: &mut Vec<VarName>) {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
        } => {
            // Check if the argument refers to an algebraic variable
            if let Some(arg) = args.first() {
                let matches: Vec<_> = dae
                    .algebraics
                    .keys()
                    .filter(|alg_name| expr_refers_to_var(arg, alg_name))
                    .cloned()
                    .collect();
                out.extend(matches);
            }
            // Also recurse into args (der could be nested)
            for a in args {
                collect_der_of_algebraics(a, dae, out);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_der_of_algebraics(lhs, dae, out);
            collect_der_of_algebraics(rhs, dae, out);
        }
        Expression::Unary { rhs, .. } => {
            collect_der_of_algebraics(rhs, dae, out);
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            for a in args {
                collect_der_of_algebraics(a, dae, out);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (c, v) in branches {
                collect_der_of_algebraics(c, dae, out);
                collect_der_of_algebraics(v, dae, out);
            }
            collect_der_of_algebraics(else_branch, dae, out);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for e in elements {
                collect_der_of_algebraics(e, dae, out);
            }
        }
        Expression::Index { base, .. } => {
            collect_der_of_algebraics(base, dae, out);
        }
        _ => {}
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
    for eq in &dae.f_x {
        collect_der_of_algebraics(&eq.rhs, dae, &mut to_promote);
    }

    // Deduplicate using a set (VarName doesn't impl Ord)
    let mut seen = HashSet::new();
    to_promote.retain(|n| seen.insert(n.as_str().to_string()));

    for name in &to_promote {
        if let Some(var) = dae.algebraics.shift_remove(name) {
            dae.states.insert(name.clone(), var);
        }
    }
}

/// Check if an equation is a derivative alias: `0 = alias_var - der(state)` or
/// `0 = der(state) - alias_var`. Returns the alias variable name if so.
pub fn try_extract_derivative_alias(eq: &Equation, state_name: &VarName) -> Option<VarName> {
    // Pattern: Binary { op: Sub, lhs, rhs } where one side is der(state)
    // and the other is a plain VarRef (the alias variable)
    let Expression::Binary { op, lhs, rhs } = &eq.rhs else {
        return None;
    };
    if !matches!(op, OpBinary::Sub(_)) {
        return None;
    }

    let is_der_of_state = |expr: &Expression| -> bool {
        matches!(
            expr,
            Expression::BuiltinCall { function: BuiltinFunction::Der, args }
            if args.len() == 1 && expr_refers_to_var(&args[0], state_name)
        )
    };

    let plain_var_name = |expr: &Expression| -> Option<VarName> {
        match expr {
            Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => Some(name.clone()),
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
    match expr {
        Expression::VarRef { name, subscripts } if name == old_name && subscripts.is_empty() => {
            replacement.clone()
        }
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(substitute_var_in_expr(lhs, old_name, replacement)),
            rhs: Box::new(substitute_var_in_expr(rhs, old_name, replacement)),
        },
        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(substitute_var_in_expr(rhs, old_name, replacement)),
        },
        Expression::BuiltinCall { function, args } => Expression::BuiltinCall {
            function: *function,
            args: args
                .iter()
                .map(|a| substitute_var_in_expr(a, old_name, replacement))
                .collect(),
        },
        Expression::If {
            branches,
            else_branch,
        } => Expression::If {
            branches: branches
                .iter()
                .map(|(c, v)| {
                    (
                        substitute_var_in_expr(c, old_name, replacement),
                        substitute_var_in_expr(v, old_name, replacement),
                    )
                })
                .collect(),
            else_branch: Box::new(substitute_var_in_expr(else_branch, old_name, replacement)),
        },
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => Expression::FunctionCall {
            name: name.clone(),
            args: args
                .iter()
                .map(|a| substitute_var_in_expr(a, old_name, replacement))
                .collect(),
            is_constructor: *is_constructor,
        },
        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: elements
                .iter()
                .map(|e| substitute_var_in_expr(e, old_name, replacement))
                .collect(),
            is_matrix: *is_matrix,
        },
        Expression::Index { base, subscripts } => Expression::Index {
            base: Box::new(substitute_var_in_expr(base, old_name, replacement)),
            subscripts: subscripts.clone(),
        },
        _ => expr.clone(),
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
pub fn eliminate_derivative_aliases(dae: &mut Dae) {
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    let mut alias_eqs_to_remove: Vec<usize> = Vec::new();
    let mut alias_vars_to_remove: Vec<VarName> = Vec::new();
    let mut substitutions: Vec<(VarName, Expression)> = Vec::new();

    for state_name in &state_names {
        // Find all equation indices containing der(state)
        let der_eq_indices: Vec<usize> = dae
            .f_x
            .iter()
            .enumerate()
            .filter(|(_, eq)| expr_contains_der_of(&eq.rhs, state_name))
            .map(|(i, _)| i)
            .collect();

        if der_eq_indices.len() != 2 {
            continue;
        }

        // Try to identify which of the two is the alias
        let mut alias_idx = None;
        let mut alias_var = None;
        for &idx in &der_eq_indices {
            if let Some(var) = try_extract_derivative_alias(&dae.f_x[idx], state_name) {
                alias_idx = Some(idx);
                alias_var = Some(var);
                break;
            }
        }

        let Some(alias_idx) = alias_idx else {
            continue;
        };
        let alias_var = alias_var.unwrap();

        // Build the replacement: der(state)
        let der_expr = Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![Expression::VarRef {
                name: state_name.clone(),
                subscripts: vec![],
            }],
        };

        alias_eqs_to_remove.push(alias_idx);
        alias_vars_to_remove.push(alias_var.clone());
        substitutions.push((alias_var, der_expr));
    }

    // Apply substitutions to all equations
    for (old_name, replacement) in &substitutions {
        for eq in &mut dae.f_x {
            eq.rhs = substitute_var_in_expr(&eq.rhs, old_name, replacement);
        }
    }

    // Remove alias equations (in reverse order to preserve indices)
    alias_eqs_to_remove.sort_unstable();
    alias_eqs_to_remove.dedup();
    for &idx in alias_eqs_to_remove.iter().rev() {
        dae.f_x.remove(idx);
    }

    // Remove alias variables from algebraics
    for var_name in &alias_vars_to_remove {
        dae.algebraics.shift_remove(var_name);
    }
}

pub fn symbolic_der_var_ref(name: &VarName) -> Expression {
    Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![Expression::VarRef {
            name: name.clone(),
            subscripts: vec![],
        }],
    }
}

pub fn build_relaxed_derivative_map(dae: &Dae) -> HashMap<String, Expression> {
    let mut map = compute_full_derivative_map(dae);

    // For index-reduction differentiation, keep unknown derivatives symbolic
    // instead of failing the whole derivative expansion.
    for name in dae
        .states
        .keys()
        .chain(dae.algebraics.keys())
        .chain(dae.outputs.keys())
        .chain(dae.inputs.keys())
    {
        map.entry(name.as_str().to_string())
            .or_insert_with(|| symbolic_der_var_ref(name));
    }
    map
}

pub fn expr_contains_var(expr: &Expression, var_name: &VarName) -> bool {
    match expr {
        Expression::VarRef { .. } | Expression::Index { .. } => expr_refers_to_var(expr, var_name),
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_var(lhs, var_name) || expr_contains_var(rhs, var_name)
        }
        Expression::Unary { rhs, .. } => expr_contains_var(rhs, var_name),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(|a| expr_contains_var(a, var_name))
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches
                .iter()
                .any(|(c, v)| expr_contains_var(c, var_name) || expr_contains_var(v, var_name))
                || expr_contains_var(else_branch, var_name)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(|e| expr_contains_var(e, var_name))
        }
        _ => false,
    }
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
) -> bool {
    dae.f_x.iter().any(|eq| {
        let der_states = derivative_states_in_eq(&eq.rhs, state_names);
        der_states.len() == 1 && der_states[0] == *state_name
    })
}

pub fn eq_contains_any_state_der(rhs: &Expression, state_names: &[VarName]) -> bool {
    state_names
        .iter()
        .any(|state| expr_contains_der_of(rhs, state))
}

fn expr_contains_der_of_non_state(expr: &Expression, state_name_set: &HashSet<String>) -> bool {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
        } => {
            if args.len() != 1 {
                return true;
            }
            match &args[0] {
                Expression::VarRef { name, subscripts } => {
                    !subscripts.is_empty() || !state_name_set.contains(name.as_str())
                }
                _ => true,
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_der_of_non_state(lhs, state_name_set)
                || expr_contains_der_of_non_state(rhs, state_name_set)
        }
        Expression::Unary { rhs, .. } => expr_contains_der_of_non_state(rhs, state_name_set),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| expr_contains_der_of_non_state(arg, state_name_set)),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(c, v)| {
                expr_contains_der_of_non_state(c, state_name_set)
                    || expr_contains_der_of_non_state(v, state_name_set)
            }) || expr_contains_der_of_non_state(else_branch, state_name_set)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => elements
            .iter()
            .any(|e| expr_contains_der_of_non_state(e, state_name_set)),
        Expression::Index { base, .. } => expr_contains_der_of_non_state(base, state_name_set),
        _ => false,
    }
}

fn dae_variable_size(dae: &Dae, name: &VarName) -> Option<usize> {
    dae.states
        .get(name)
        .or_else(|| dae.algebraics.get(name))
        .or_else(|| dae.outputs.get(name))
        .or_else(|| dae.inputs.get(name))
        .or_else(|| dae.parameters.get(name))
        .or_else(|| dae.constants.get(name))
        .or_else(|| dae.discrete_reals.get(name))
        .or_else(|| dae.discrete_valued.get(name))
        .map(Variable::size)
}

/// Direct-assignment demotion runs before scalarization. If a scalar state is
/// defined using an unsliced vector reference (e.g. `x = -i` where `i` is
/// array-valued), demotion is ambiguous and can corrupt index/alias structure.
fn expr_contains_unsliced_vector_ref(expr: &Expression, dae: &Dae) -> bool {
    match expr {
        Expression::VarRef { name, subscripts } => {
            subscripts.is_empty() && dae_variable_size(dae, name).is_some_and(|size| size > 1)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_unsliced_vector_ref(lhs, dae)
                || expr_contains_unsliced_vector_ref(rhs, dae)
        }
        Expression::Unary { rhs, .. } => expr_contains_unsliced_vector_ref(rhs, dae),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| expr_contains_unsliced_vector_ref(arg, dae)),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expr_contains_unsliced_vector_ref(condition, dae)
                    || expr_contains_unsliced_vector_ref(value, dae)
            }) || expr_contains_unsliced_vector_ref(else_branch, dae)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => elements
            .iter()
            .any(|element| expr_contains_unsliced_vector_ref(element, dae)),
        Expression::Range { start, step, end } => {
            expr_contains_unsliced_vector_ref(start, dae)
                || step
                    .as_ref()
                    .is_some_and(|step| expr_contains_unsliced_vector_ref(step, dae))
                || expr_contains_unsliced_vector_ref(end, dae)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_contains_unsliced_vector_ref(expr, dae)
                || indices
                    .iter()
                    .any(|idx| expr_contains_unsliced_vector_ref(&idx.range, dae))
                || filter
                    .as_ref()
                    .is_some_and(|pred| expr_contains_unsliced_vector_ref(pred, dae))
        }
        Expression::Index { base, subscripts } => {
            expr_contains_unsliced_vector_ref(base, dae)
                || subscripts.iter().any(|sub| match sub {
                    Subscript::Expr(expr) => expr_contains_unsliced_vector_ref(expr, dae),
                    _ => false,
                })
        }
        Expression::FieldAccess { base, .. } => expr_contains_unsliced_vector_ref(base, dae),
        Expression::Literal(_) | Expression::Empty => false,
    }
}

pub fn try_extract_state_alias_pair(rhs: &Expression) -> Option<(VarName, VarName)> {
    let Expression::Binary { op, lhs, rhs } = rhs else {
        return None;
    };
    if !matches!(op, OpBinary::Sub(_)) {
        return None;
    }
    let Expression::VarRef {
        name: lhs_name,
        subscripts: lhs_subscripts,
    } = lhs.as_ref()
    else {
        return None;
    };
    let Expression::VarRef {
        name: rhs_name,
        subscripts: rhs_subscripts,
    } = rhs.as_ref()
    else {
        return None;
    };
    if !lhs_subscripts.is_empty() || !rhs_subscripts.is_empty() {
        return None;
    }
    Some((lhs_name.clone(), rhs_name.clone()))
}

fn grow_alias_demotion_set(
    to_demote: &mut HashSet<String>,
    candidate: &VarName,
    candidate_has_der: bool,
    linked: &VarName,
) -> bool {
    if candidate_has_der
        || !to_demote.contains(linked.as_str())
        || to_demote.contains(candidate.as_str())
    {
        return false;
    }
    to_demote.insert(candidate.as_str().to_string());
    true
}

/// Conservative state-selection helper for alias-constrained states.
///
/// Demotion rules:
/// - state <-> state alias: if one side has a standalone der-row and the other
///   does not, demote the no-der side (and propagate across chains).
/// - state <-> non-state alias: if the state has no standalone der-row, demote it.
pub fn demote_alias_states_without_der(dae: &mut Dae) -> usize {
    let max_rounds = dae.states.len().clamp(1, 8);
    let mut total_demoted = 0usize;

    for _ in 0..max_rounds {
        let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
        if state_names.is_empty() {
            break;
        }
        let state_name_set: HashSet<String> = state_names
            .iter()
            .map(|name| name.as_str().to_string())
            .collect();
        let has_der: HashMap<String, bool> = state_names
            .iter()
            .map(|name| {
                (
                    name.as_str().to_string(),
                    state_has_standalone_der_equation(dae, name, &state_names),
                )
            })
            .collect();

        let alias_pairs: Vec<(VarName, VarName)> = dae
            .f_x
            .iter()
            .filter_map(|eq| try_extract_state_alias_pair(&eq.rhs))
            .filter(|(a, b)| {
                state_name_set.contains(a.as_str()) || state_name_set.contains(b.as_str())
            })
            .collect();
        if alias_pairs.is_empty() {
            break;
        }

        let mut to_demote: HashSet<String> = HashSet::new();
        for (a, b) in &alias_pairs {
            let a_is_state = state_name_set.contains(a.as_str());
            let b_is_state = state_name_set.contains(b.as_str());
            let a_has_der = a_is_state && has_der.get(a.as_str()).copied().unwrap_or(false);
            let b_has_der = b_is_state && has_der.get(b.as_str()).copied().unwrap_or(false);

            if a_is_state && b_is_state && a_has_der != b_has_der {
                let demoted = match a_has_der {
                    true => b,
                    false => a,
                };
                to_demote.insert(demoted.as_str().to_string());
            }
            if a_is_state && b_is_state {
                continue;
            }

            if a_is_state && !b_is_state && !a_has_der {
                to_demote.insert(a.as_str().to_string());
            }
            if b_is_state && !a_is_state && !b_has_der {
                to_demote.insert(b.as_str().to_string());
            }
        }

        // Propagate through alias chains: if a no-der state aliases a state
        // selected for demotion, demote it as well.
        let mut grown = true;
        while grown {
            grown = false;
            for (a, b) in &alias_pairs {
                let a_has_der = has_der.get(a.as_str()).copied().unwrap_or(false);
                let b_has_der = has_der.get(b.as_str()).copied().unwrap_or(false);
                grown |= grow_alias_demotion_set(&mut to_demote, a, a_has_der, b);
                grown |= grow_alias_demotion_set(&mut to_demote, b, b_has_der, a);
            }
        }

        if to_demote.is_empty() {
            break;
        }

        let mut demoted_this_round = 0usize;
        let to_demote_names: Vec<VarName> = to_demote.into_iter().map(VarName::new).collect();
        for name in to_demote_names {
            if let Some(var) = dae.states.shift_remove(&name) {
                dae.algebraics.insert(name.clone(), var);
                demoted_this_round += 1;
            }
        }
        if demoted_this_round == 0 {
            break;
        }
        total_demoted += demoted_this_round;
    }

    total_demoted
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
            op: OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            if let Expression::VarRef { name, subscripts } = lhs.as_ref()
                && subscripts.is_empty()
                && state_name_set.contains(name.as_str())
            {
                return Some((name.clone(), *rhs.clone()));
            }
            if let Expression::VarRef { name, subscripts } = rhs.as_ref()
                && subscripts.is_empty()
                && state_name_set.contains(name.as_str())
            {
                return Some((name.clone(), *lhs.clone()));
            }
            None
        }
        Expression::Unary {
            op: OpUnary::Minus(_),
            rhs,
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
            .then(|| (lhs.clone(), eq.rhs.clone()));
    }
    if let Some(pair) = extract_state_direct_assignment(&eq.rhs, state_name_set) {
        return Some(pair);
    }

    // Residual form: 0 = expr. If expr is affine in exactly one state with
    // coefficient ±1, solve for that state.
    let mut solved: Option<(VarName, Expression)> = None;
    for state_name in state_names {
        if !expr_contains_var(&eq.rhs, state_name) {
            continue;
        }
        let Some((coef, remainder)) = split_linear_target(&eq.rhs, state_name) else {
            continue;
        };
        let defining_expr = match coef {
            1 => sub_expr(zero_expr(), remainder),
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

fn rewrite_subscripts_with_der_substitution(
    subscripts: &[Subscript],
    state_name: &VarName,
    replacement: &Expression,
) -> Vec<Subscript> {
    subscripts
        .iter()
        .map(|sub| match sub {
            Subscript::Expr(expr) => Subscript::Expr(Box::new(substitute_der_of_state(
                expr,
                state_name,
                replacement,
            ))),
            _ => sub.clone(),
        })
        .collect()
}

fn rewrite_exprs_with_der_substitution(
    exprs: &[Expression],
    state_name: &VarName,
    replacement: &Expression,
) -> Vec<Expression> {
    exprs
        .iter()
        .map(|expr| substitute_der_of_state(expr, state_name, replacement))
        .collect()
}

fn rewrite_if_branches_with_der_substitution(
    branches: &[(Expression, Expression)],
    state_name: &VarName,
    replacement: &Expression,
) -> Vec<(Expression, Expression)> {
    branches
        .iter()
        .map(|(cond, value)| {
            (
                substitute_der_of_state(cond, state_name, replacement),
                substitute_der_of_state(value, state_name, replacement),
            )
        })
        .collect()
}

fn rewrite_comprehension_indices_with_der_substitution(
    indices: &[rumoca_ir_dae::ComprehensionIndex],
    state_name: &VarName,
    replacement: &Expression,
) -> Vec<rumoca_ir_dae::ComprehensionIndex> {
    indices
        .iter()
        .map(|idx| rumoca_ir_dae::ComprehensionIndex {
            name: idx.name.clone(),
            range: substitute_der_of_state(&idx.range, state_name, replacement),
        })
        .collect()
}

fn der_call_targets_state(expr: &Expression, state_name: &VarName) -> bool {
    if let Expression::BuiltinCall { function, args } = expr
        && *function == BuiltinFunction::Der
        && args.len() == 1
        && let Expression::VarRef { name, subscripts } = &args[0]
    {
        return name == state_name && subscripts.is_empty();
    }
    false
}

fn substitute_der_of_state(
    expr: &Expression,
    state_name: &VarName,
    replacement: &Expression,
) -> Expression {
    if der_call_targets_state(expr, state_name) {
        return replacement.clone();
    }

    match expr {
        Expression::Literal(_) | Expression::Empty => expr.clone(),
        Expression::VarRef { name, subscripts } => Expression::VarRef {
            name: name.clone(),
            subscripts: rewrite_subscripts_with_der_substitution(
                subscripts,
                state_name,
                replacement,
            ),
        },
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(substitute_der_of_state(lhs, state_name, replacement)),
            rhs: Box::new(substitute_der_of_state(rhs, state_name, replacement)),
        },
        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(substitute_der_of_state(rhs, state_name, replacement)),
        },
        Expression::If {
            branches,
            else_branch,
        } => Expression::If {
            branches: rewrite_if_branches_with_der_substitution(branches, state_name, replacement),
            else_branch: Box::new(substitute_der_of_state(
                else_branch,
                state_name,
                replacement,
            )),
        },
        Expression::BuiltinCall { function, args } => Expression::BuiltinCall {
            function: *function,
            args: rewrite_exprs_with_der_substitution(args, state_name, replacement),
        },
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => Expression::FunctionCall {
            name: name.clone(),
            args: rewrite_exprs_with_der_substitution(args, state_name, replacement),
            is_constructor: *is_constructor,
        },
        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: rewrite_exprs_with_der_substitution(elements, state_name, replacement),
            is_matrix: *is_matrix,
        },
        Expression::Tuple { elements } => Expression::Tuple {
            elements: rewrite_exprs_with_der_substitution(elements, state_name, replacement),
        },
        Expression::Range { start, step, end } => Expression::Range {
            start: Box::new(substitute_der_of_state(start, state_name, replacement)),
            step: step
                .as_ref()
                .map(|s| Box::new(substitute_der_of_state(s, state_name, replacement))),
            end: Box::new(substitute_der_of_state(end, state_name, replacement)),
        },
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => Expression::ArrayComprehension {
            expr: Box::new(substitute_der_of_state(expr, state_name, replacement)),
            indices: rewrite_comprehension_indices_with_der_substitution(
                indices,
                state_name,
                replacement,
            ),
            filter: filter
                .as_ref()
                .map(|f| Box::new(substitute_der_of_state(f, state_name, replacement))),
        },
        Expression::Index { base, subscripts } => Expression::Index {
            base: Box::new(substitute_der_of_state(base, state_name, replacement)),
            subscripts: rewrite_subscripts_with_der_substitution(
                subscripts,
                state_name,
                replacement,
            ),
        },
        Expression::FieldAccess { base, field } => Expression::FieldAccess {
            base: Box::new(substitute_der_of_state(base, state_name, replacement)),
            field: field.clone(),
        },
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
    n_skip_connection_origin: usize,
    n_skip_when_assigned: usize,
    n_skip_self_der: usize,
    n_skip_der_in_defining_expr: usize,
    n_skip_unsliced_vector_ref: usize,
    n_skip_extra_state_refs: usize,
    n_skip_non_state_der: usize,
    n_skip_no_der_expr: usize,
    n_trace_logged_candidates: usize,
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
        .states
        .get(state_name)
        .map(|var| format!("{:?}", var.state_select))
        .unwrap_or_else(|| "Unknown".to_string());
    eprintln!(
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
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    let state_name_set: HashSet<String> = dae
        .states
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();
    if state_name_set.is_empty() {
        return None;
    }
    let when_assigned_states: HashSet<String> = dae
        .f_z
        .iter()
        .chain(dae.f_m.iter())
        .filter_map(|eq| eq.lhs.as_ref())
        .map(|name| name.as_str().to_string())
        .filter(|name| state_name_set.contains(name))
        .collect();
    Some((state_names, state_name_set, when_assigned_states))
}

fn log_direct_demotion_scan_summary(
    trace: bool,
    state_count: usize,
    substitutions: &HashMap<String, DirectStateDemotionPlan>,
    counters: &DirectDemotionCounters,
) {
    if !trace {
        return;
    }
    eprintln!(
        "[sim-trace] direct-assignment-demotion scan: states={} candidates={} accepted={} skip_flow_sum_origin={} skip_connection_origin={} skip_when={} skip_self_der={} skip_der_in_defining_expr={} skip_unsliced_vector_ref={} skip_extra_state_refs={} skip_no_der={} skip_non_state_der={}",
        state_count,
        counters.n_candidates,
        substitutions.len(),
        counters.n_skip_flow_sum_origin,
        counters.n_skip_connection_origin,
        counters.n_skip_when_assigned,
        counters.n_skip_self_der,
        counters.n_skip_der_in_defining_expr,
        counters.n_skip_unsliced_vector_ref,
        counters.n_skip_extra_state_refs,
        counters.n_skip_no_der_expr,
        counters.n_skip_non_state_der
    );
}

fn is_connection_equation_origin(origin: &str) -> bool {
    origin.starts_with("connection equation:")
}

fn collect_non_state_continuous_unknown_names(dae: &Dae) -> HashSet<String> {
    dae.algebraics
        .keys()
        .chain(dae.outputs.keys())
        .chain(dae.derivative_aliases.keys())
        .map(|name| name.as_str().to_string())
        .collect()
}

fn expression_contains_any_der_call(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            ..
        } => true,
        Expression::Binary { lhs, rhs, .. } => {
            expression_contains_any_der_call(lhs) || expression_contains_any_der_call(rhs)
        }
        Expression::Unary { rhs, .. }
        | Expression::FieldAccess { base: rhs, .. }
        | Expression::Index { base: rhs, .. } => expression_contains_any_der_call(rhs),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(expression_contains_any_der_call)
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expression_contains_any_der_call(cond) || expression_contains_any_der_call(value)
            }) || expression_contains_any_der_call(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(expression_contains_any_der_call)
        }
        Expression::Range { start, step, end } => {
            expression_contains_any_der_call(start)
                || step
                    .as_deref()
                    .is_some_and(expression_contains_any_der_call)
                || expression_contains_any_der_call(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expression_contains_any_der_call(expr)
                || indices
                    .iter()
                    .any(|index| expression_contains_any_der_call(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expression_contains_any_der_call)
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

fn equation_defining_expr_for_unknown(eq: &Equation, unknown_name: &VarName) -> Option<Expression> {
    if let Some(lhs) = eq.lhs.as_ref()
        && lhs == unknown_name
    {
        if expression_contains_any_der_call(&eq.rhs) {
            return None;
        }
        return Some(eq.rhs.clone());
    }
    if let Some((coef, remainder)) = split_linear_target(&eq.rhs, unknown_name) {
        let defining_expr = match coef {
            1 => sub_expr(zero_expr(), remainder),
            -1 => remainder,
            _ => return None,
        };
        if expression_contains_any_der_call(&defining_expr) {
            return None;
        }
        return Some(defining_expr);
    }
    None
}

fn defining_expr_references_non_state_alias_that_depends_on_state(
    dae: &Dae,
    defining_expr: &Expression,
    state_name: &VarName,
    non_state_unknown_names: &HashSet<String>,
) -> bool {
    let mut refs = HashSet::new();
    defining_expr.collect_var_refs(&mut refs);
    refs.into_iter().any(|ref_name| {
        if !non_state_unknown_names.contains(ref_name.as_str()) {
            return false;
        }
        let mut has_state_dependent_def = false;
        let mut has_state_independent_def = false;
        for eq in &dae.f_x {
            let Some(defining_expr) = equation_defining_expr_for_unknown(eq, &ref_name) else {
                continue;
            };
            if expr_contains_var(&defining_expr, state_name)
                || expr_contains_der_of(&defining_expr, state_name)
            {
                has_state_dependent_def = true;
            } else {
                has_state_independent_def = true;
            }
        }
        has_state_dependent_def && !has_state_independent_def
    })
}

fn apply_direct_demotion_plans(
    dae: &mut Dae,
    substitutions: &HashMap<String, DirectStateDemotionPlan>,
) -> usize {
    let mut demoted_this_round = 0usize;
    for plan in substitutions.values() {
        for eq in &mut dae.f_x {
            eq.rhs = substitute_der_of_state(&eq.rhs, &plan.state_name, &plan.der_expr);
        }
        if let Some(var) = dae.states.shift_remove(&plan.state_name) {
            dae.algebraics.insert(plan.state_name.clone(), var);
            demoted_this_round += 1;
        }
    }
    demoted_this_round
}

/// Demote states that are explicitly defined by direct assignment equations
/// (`state = expr`) and substitute `der(state)` with `d/dt(expr)` throughout
/// the system.
///
/// This removes structurally over-constrained "dummy/trajectory" states from
/// the differential set and keeps derivative chains algebraically consistent.
/// The defining expression need not reference `time` directly; if `d/dt(expr)`
/// can be resolved without introducing derivatives of non-state variables, the
/// state is demoted. States assigned in `when` clauses are preserved, since
/// they participate in event/reinit updates and must remain in the state vector.
#[expect(
    clippy::too_many_lines,
    reason = "demotion pass is intentionally linearized as a single staged filter"
)]
pub fn demote_direct_assigned_states(dae: &mut Dae) -> usize {
    let max_rounds = dae.states.len().clamp(1, 8);
    let mut total_demoted = 0usize;

    for _ in 0..max_rounds {
        let trace = sim_trace_enabled();
        let Some((state_names, state_name_set, when_assigned_states)) =
            direct_demotion_round_context(dae)
        else {
            break;
        };
        let non_state_unknown_names = collect_non_state_continuous_unknown_names(dae);

        let der_map = build_relaxed_derivative_map(dae);
        let mut substitutions: HashMap<String, DirectStateDemotionPlan> = HashMap::new();
        let mut counters = DirectDemotionCounters::default();

        for eq in &dae.f_x {
            let Some((state_name, defining_expr)) =
                extract_state_direct_assignment_equation(eq, &state_names, &state_name_set)
            else {
                continue;
            };
            counters.n_candidates += 1;
            if eq.origin.starts_with("flow sum equation:") {
                counters.n_skip_flow_sum_origin += 1;
                continue;
            }
            if is_connection_equation_origin(&eq.origin) {
                counters.n_skip_connection_origin += 1;
                continue;
            }
            log_direct_assignment_candidate(trace, &mut counters, dae, eq, &state_name);
            if when_assigned_states.contains(state_name.as_str()) {
                counters.n_skip_when_assigned += 1;
                continue;
            }
            if expr_contains_der_of(&defining_expr, &state_name) {
                counters.n_skip_self_der += 1;
                continue;
            }
            if eq_contains_any_state_der(&defining_expr, &state_names) {
                counters.n_skip_der_in_defining_expr += 1;
                continue;
            }
            if defining_expr_references_non_state_alias_that_depends_on_state(
                dae,
                &defining_expr,
                &state_name,
                &non_state_unknown_names,
            ) {
                continue;
            }
            if expr_contains_unsliced_vector_ref(&defining_expr, dae) {
                counters.n_skip_unsliced_vector_ref += 1;
                continue;
            }
            let state_non_der_ref_rows = dae
                .f_x
                .iter()
                .filter(|row| {
                    expr_contains_var(&row.rhs, &state_name)
                        && !expr_contains_der_of(&row.rhs, &state_name)
                })
                .count();
            if state_non_der_ref_rows > 1 {
                counters.n_skip_extra_state_refs += 1;
                continue;
            }
            let Some(der_expr) = choose_derivative_replacement(
                &defining_expr,
                &state_name_set,
                dae,
                &der_map,
                &mut counters,
            ) else {
                continue;
            };
            if expr_contains_der_of(&der_expr, &state_name) {
                counters.n_skip_self_der += 1;
                continue;
            }
            if expr_contains_der_of_non_state(&der_expr, &state_name_set) {
                counters.n_skip_non_state_der += 1;
                continue;
            }
            if trace && counters.n_trace_logged_candidates < 16 {
                eprintln!(
                    "[sim-trace] direct-assignment accepted state={} der_expr={}",
                    state_name.as_str(),
                    truncate_debug(&format!("{:?}", der_expr), 1200)
                );
                counters.n_trace_logged_candidates += 1;
            }
            substitutions
                .entry(state_name.as_str().to_string())
                .or_insert(DirectStateDemotionPlan {
                    state_name,
                    der_expr,
                });
        }

        log_direct_demotion_scan_summary(trace, state_name_set.len(), &substitutions, &counters);

        if substitutions.is_empty() {
            break;
        }

        let demoted_this_round = apply_direct_demotion_plans(dae, &substitutions);

        if demoted_this_round == 0 {
            break;
        }
        total_demoted += demoted_this_round;
    }

    total_demoted
}

fn state_has_any_equation_reference(dae: &Dae, state_name: &VarName) -> bool {
    dae.f_x
        .iter()
        .any(|eq| expr_contains_var(&eq.rhs, state_name))
}

fn state_has_any_derivative_reference(dae: &Dae, state_name: &VarName) -> bool {
    dae.f_x
        .iter()
        .any(|eq| expr_contains_der_of(&eq.rhs, state_name))
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
    let state_to_rows: Vec<Vec<usize>> = state_names
        .iter()
        .map(|state_name| {
            dae.f_x
                .iter()
                .enumerate()
                .filter_map(|(row_idx, eq)| {
                    expr_contains_der_of(&eq.rhs, state_name).then_some(row_idx)
                })
                .collect::<Vec<_>>()
        })
        .collect();

    let mut state_order: Vec<usize> = (0..state_names.len()).collect();
    state_order.sort_by_key(|idx| state_to_rows[*idx].len());

    let mut row_to_state: Vec<Option<usize>> = vec![None; dae.f_x.len()];
    for state_idx in state_order {
        if state_to_rows[state_idx].is_empty() {
            continue;
        }
        let mut seen_rows = vec![false; dae.f_x.len()];
        let _ =
            try_match_state_to_row(state_idx, &state_to_rows, &mut row_to_state, &mut seen_rows);
    }

    row_to_state.into_iter().flatten().collect()
}

/// Demote states that are no longer referenced by any continuous equation.
///
/// Trivial elimination may remove an alias/binding equation that was the only
/// remaining reference to a misclassified state-like variable. Such orphan
/// states cannot have valid ODE rows and should be treated as algebraics.
pub fn demote_orphan_states_without_equation_refs(dae: &mut Dae) -> usize {
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    let mut demoted = 0usize;
    for name in state_names {
        if state_has_any_equation_reference(dae, &name) {
            continue;
        }
        if let Some(var) = dae.states.shift_remove(&name) {
            dae.algebraics.insert(name, var);
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
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    let mut demoted = 0usize;
    for name in state_names {
        if state_has_any_derivative_reference(dae, &name) {
            continue;
        }
        if let Some(var) = dae.states.shift_remove(&name) {
            dae.algebraics.insert(name, var);
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
/// algebraics.
pub fn demote_states_without_assignable_derivative_rows(dae: &mut Dae) -> usize {
    let mut total_demoted = 0usize;

    loop {
        let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
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
            if let Some(var) = dae.states.shift_remove(&name) {
                dae.algebraics.insert(name, var);
                demoted_this_round += 1;
            }
        }
        if demoted_this_round == 0 {
            break;
        }
        total_demoted += demoted_this_round;
    }

    total_demoted
}

/// Phase-1 structural index reduction.
///
/// For each state without a `der(state)` equation, find a non-ODE constraint
/// referencing that state and differentiate it once with symbolic chain-rule.
/// The differentiated equation must explicitly contain `der(state)` to be
/// accepted; otherwise it is discarded.
pub fn index_reduce_missing_state_derivatives_once(dae: &mut Dae) -> usize {
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    if state_names.is_empty() {
        return 0;
    }
    let state_name_set: HashSet<String> = state_names
        .iter()
        .map(|name| name.as_str().to_string())
        .collect();

    let der_map = build_relaxed_derivative_map(dae);
    let mut changed = 0usize;
    let mut used_eq = HashSet::new();

    for state_name in &state_names {
        if state_has_standalone_der_equation(dae, state_name, &state_names) {
            continue;
        }

        let candidate_indices: Vec<usize> = dae
            .f_x
            .iter()
            .enumerate()
            .filter_map(|(idx, eq)| {
                if used_eq.contains(&idx) {
                    return None;
                }
                if eq_contains_any_state_der(&eq.rhs, &state_names) {
                    return None;
                }
                expr_contains_var(&eq.rhs, state_name).then_some(idx)
            })
            .collect();

        for idx in candidate_indices {
            let differentiated = symbolic_time_derivative(&dae.f_x[idx].rhs, dae, &der_map);
            let Some(new_rhs) = differentiated else {
                continue;
            };
            let der_states = derivative_states_in_eq(&new_rhs, &state_names);
            if der_states.len() != 1 || der_states[0] != *state_name {
                continue;
            }
            if expr_contains_der_of_non_state(&new_rhs, &state_name_set) {
                continue;
            }

            let old_origin = dae.f_x[idx].origin.clone();
            dae.f_x[idx].rhs = new_rhs;
            dae.f_x[idx].origin = if old_origin.is_empty() {
                format!("index_reduction:d_dt_for_{}", state_name.as_str())
            } else {
                format!(
                    "{}|index_reduction:d_dt_for_{}",
                    old_origin,
                    state_name.as_str()
                )
            };
            used_eq.insert(idx);
            changed += 1;
            break;
        }
    }

    changed
}

pub fn index_reduce_missing_state_derivatives(dae: &mut Dae) -> usize {
    let max_rounds = dae.states.len().clamp(1, 8);
    let mut total_changed = 0usize;
    for _round in 0..max_rounds {
        let changed = index_reduce_missing_state_derivatives_once(dae);
        if changed == 0 {
            break;
        }
        total_changed += changed;
    }
    total_changed
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
        } if args.len() == 1 && expr_refers_to_var(&args[0], state_name) => current_sign,
        Expression::Binary { op, lhs, rhs } => match op {
            OpBinary::Add(_) | OpBinary::AddElem(_) => {
                let l = der_sign_in_expr(lhs, state_name, current_sign);
                if l != 0 {
                    return l;
                }
                der_sign_in_expr(rhs, state_name, current_sign)
            }
            OpBinary::Sub(_) | OpBinary::SubElem(_) => {
                let l = der_sign_in_expr(lhs, state_name, current_sign);
                if l != 0 {
                    return l;
                }
                der_sign_in_expr(rhs, state_name, -current_sign)
            }
            OpBinary::Mul(_) | OpBinary::MulElem(_) => {
                // Check both sides — assume positive coefficient for the non-der factor
                let l = der_sign_in_expr(lhs, state_name, current_sign);
                if l != 0 {
                    return l;
                }
                der_sign_in_expr(rhs, state_name, current_sign)
            }
            _ => 0,
        },
        Expression::Unary { op, rhs } => match op {
            OpUnary::Minus(_) | OpUnary::DotMinus(_) => {
                der_sign_in_expr(rhs, state_name, -current_sign)
            }
            _ => der_sign_in_expr(rhs, state_name, current_sign),
        },
        Expression::If {
            branches,
            else_branch,
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
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    for (i, state_name) in state_names.iter().enumerate() {
        if i >= dae.f_x.len() {
            break;
        }
        let sign = der_sign_in_expr(&dae.f_x[i].rhs, state_name, 1);
        if sign < 0 {
            // Negate the entire equation: wrap in unary minus
            let old_rhs = dae.f_x[i].rhs.clone();
            dae.f_x[i].rhs = Expression::Unary {
                op: OpUnary::Minus(Default::default()),
                rhs: Box::new(old_rhs),
            };
        }
    }
}

#[cfg(test)]
mod dae_prepare_demotion_tests;
