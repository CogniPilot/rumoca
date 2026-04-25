//! Constant-fold parameter/state `start` expressions to numeric literals.
//!
//! Many Modelica parameters have `start` values that reference other parameters
//! (e.g., `G_T = G_T_ref` where `G_T_ref = 300.15`). Template backends (Julia MTK,
//! SymPy, etc.) need concrete numeric values. This pass iteratively evaluates
//! start expressions using a fixed-point approach, replacing evaluable expressions
//! with `Literal::Real(value)`.

use rumoca_ir_core::{OpBinary, OpUnary};
use rumoca_ir_dae::{BuiltinFunction, Dae, Expression, Literal, VarName, Variable};
use std::collections::HashMap;

/// Evaluate all parameter/state/constant start expressions to numeric literals
/// where possible. Modifies the DAE in place.
pub fn fold_start_values_to_literals(dae: &mut Dae) {
    // Phase 1: build a name→value map from constants, enum ordinals, and
    // parameter start expressions (fixed-point iteration).
    let mut values: HashMap<String, f64> = HashMap::new();

    // Seed with enum literal ordinals
    for (name, ordinal) in &dae.enum_literal_ordinals {
        values.insert(name.clone(), *ordinal as f64);
    }

    // Collect all named start bindings (constants, parameters, inputs, states,
    // discrete reals, discrete valued, algebraics, outputs)
    let bindings: Vec<(VarName, Expression)> = dae
        .constants
        .iter()
        .chain(dae.parameters.iter())
        .chain(dae.inputs.iter())
        .chain(dae.states.iter())
        .chain(dae.discrete_reals.iter())
        .chain(dae.discrete_valued.iter())
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
        .filter_map(|(name, var)| var.start.as_ref().map(|expr| (name.clone(), expr.clone())))
        .collect();

    // Fixed-point iteration: resolve chains like A = B, B = 3.14
    let max_passes = bindings.len().max(1) * 2;
    for _ in 0..max_passes {
        let mut changed = false;
        for (name, expr) in &bindings {
            if values.contains_key(name.as_str()) {
                continue;
            }
            if let Some(val) = eval_const_expr(expr, &values)
                && val.is_finite()
            {
                values.insert(name.to_string(), val);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }

    let tunable_parameters: std::collections::HashSet<String> = dae
        .parameters
        .iter()
        .filter(|&(_name, var)| var.is_tunable)
        .map(|(name, _var)| name.as_str().to_string())
        .collect();

    // Phase 2: rewrite start expressions to literals where we found values.
    // Also clear self-referencing defaults (start = VarRef(self_name)).
    let rewrite = |var: &mut Variable| {
        if let Some(ref start) = var.start {
            // Check for self-reference: start = VarRef(own_name)
            if let Expression::VarRef { name, subscripts } = start
                && subscripts.is_empty()
                && name.as_str() == var.name.as_str()
            {
                var.start = None;
                return;
            }
            if expr_references_any_parameter(start, &tunable_parameters) {
                return;
            }
            if let Some(&val) = values.get(var.name.as_str()) {
                var.start = Some(Expression::Literal(Literal::Real(val)));
            }
        }
    };

    for var in dae.constants.values_mut() {
        rewrite(var);
    }
    for var in dae.parameters.values_mut() {
        rewrite(var);
    }
    for var in dae.states.values_mut() {
        rewrite(var);
    }
    for var in dae.inputs.values_mut() {
        rewrite(var);
    }
    for var in dae.discrete_reals.values_mut() {
        rewrite(var);
    }
    for var in dae.discrete_valued.values_mut() {
        rewrite(var);
    }
    for var in dae.algebraics.values_mut() {
        rewrite(var);
    }
    for var in dae.outputs.values_mut() {
        rewrite(var);
    }
}

fn expr_references_any_parameter(
    expr: &Expression,
    parameters: &std::collections::HashSet<String>,
) -> bool {
    match expr {
        Expression::VarRef { name, .. } => parameters.contains(name.as_str()),
        Expression::Unary { rhs, .. } => expr_references_any_parameter(rhs, parameters),
        Expression::Binary { lhs, rhs, .. } => {
            expr_references_any_parameter(lhs, parameters)
                || expr_references_any_parameter(rhs, parameters)
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => args
            .iter()
            .any(|arg| expr_references_any_parameter(arg, parameters)),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_references_any_parameter(cond, parameters)
                    || expr_references_any_parameter(value, parameters)
            }) || expr_references_any_parameter(else_branch, parameters)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => elements
            .iter()
            .any(|element| expr_references_any_parameter(element, parameters)),
        Expression::Range { start, step, end } => {
            expr_references_any_parameter(start, parameters)
                || step
                    .as_deref()
                    .is_some_and(|step| expr_references_any_parameter(step, parameters))
                || expr_references_any_parameter(end, parameters)
        }
        Expression::Index { base, subscripts } => {
            expr_references_any_parameter(base, parameters)
                || subscripts
                    .iter()
                    .any(|subscript| subscript_references_any_parameter(subscript, parameters))
        }
        Expression::FieldAccess { base, .. } => expr_references_any_parameter(base, parameters),
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_references_any_parameter(expr, parameters)
                || indices
                    .iter()
                    .any(|index| expr_references_any_parameter(&index.range, parameters))
                || filter
                    .as_deref()
                    .is_some_and(|filter| expr_references_any_parameter(filter, parameters))
        }
        _ => false,
    }
}

fn subscript_references_any_parameter(
    subscript: &rumoca_ir_dae::Subscript,
    parameters: &std::collections::HashSet<String>,
) -> bool {
    match subscript {
        rumoca_ir_dae::Subscript::Expr(expr) => expr_references_any_parameter(expr, parameters),
        rumoca_ir_dae::Subscript::Index(_) | rumoca_ir_dae::Subscript::Colon => false,
    }
}

// ---------------------------------------------------------------------------
// Expression evaluator (subset sufficient for start-value resolution)
// ---------------------------------------------------------------------------

fn eval_const_expr(expr: &Expression, env: &HashMap<String, f64>) -> Option<f64> {
    match expr {
        Expression::Literal(Literal::Integer(v)) => Some(*v as f64),
        Expression::Literal(Literal::Real(v)) => Some(*v),
        Expression::Literal(Literal::Boolean(v)) => Some(if *v { 1.0 } else { 0.0 }),

        Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            env.get(name.as_str()).copied().or_else(|| {
                rumoca_ir_dae::component_base_name(name.as_str())
                    .and_then(|base| env.get(&base).copied())
            })
        }

        Expression::Unary { op, rhs } => {
            let r = eval_const_expr(rhs, env)?;
            match op {
                OpUnary::Minus(_) | OpUnary::DotMinus(_) => Some(-r),
                OpUnary::Plus(_) | OpUnary::DotPlus(_) => Some(r),
                OpUnary::Not(_) => Some(if r.abs() < 1e-12 { 1.0 } else { 0.0 }),
                _ => None,
            }
        }

        Expression::Binary { op, lhs, rhs } => {
            let l = eval_const_expr(lhs, env)?;
            let r = eval_const_expr(rhs, env)?;
            match op {
                OpBinary::Add(_) => Some(l + r),
                OpBinary::Sub(_) => Some(l - r),
                OpBinary::Mul(_) => Some(l * r),
                OpBinary::Div(_) => {
                    if r.abs() < 1e-300 {
                        None
                    } else {
                        Some(l / r)
                    }
                }
                OpBinary::Exp(_) | OpBinary::ExpElem(_) => Some(l.powf(r)),
                OpBinary::Lt(_) => Some(if l < r { 1.0 } else { 0.0 }),
                OpBinary::Le(_) => Some(if l <= r { 1.0 } else { 0.0 }),
                OpBinary::Gt(_) => Some(if l > r { 1.0 } else { 0.0 }),
                OpBinary::Ge(_) => Some(if l >= r { 1.0 } else { 0.0 }),
                OpBinary::Eq(_) => Some(if (l - r).abs() < 1e-12 { 1.0 } else { 0.0 }),
                OpBinary::Neq(_) => Some(if (l - r).abs() >= 1e-12 { 1.0 } else { 0.0 }),
                OpBinary::And(_) => Some(if l.abs() > 1e-12 && r.abs() > 1e-12 {
                    1.0
                } else {
                    0.0
                }),
                OpBinary::Or(_) => Some(if l.abs() > 1e-12 || r.abs() > 1e-12 {
                    1.0
                } else {
                    0.0
                }),
                _ => None,
            }
        }

        Expression::BuiltinCall { function, args } => eval_builtin(*function, args, env),

        Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            eval_named_function(short, args, env)
        }

        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                let c = eval_const_expr(cond, env)?;
                if c.abs() > 1e-12 {
                    return eval_const_expr(then_expr, env);
                }
            }
            eval_const_expr(else_branch, env)
        }

        _ => None,
    }
}

fn eval_builtin(
    func: BuiltinFunction,
    args: &[Expression],
    env: &HashMap<String, f64>,
) -> Option<f64> {
    let arg = |i: usize| args.get(i).and_then(|e| eval_const_expr(e, env));

    match func {
        BuiltinFunction::Abs => arg(0).map(f64::abs),
        BuiltinFunction::Sign => arg(0).map(f64::signum),
        BuiltinFunction::Sqrt => arg(0).map(f64::sqrt),
        BuiltinFunction::Floor => arg(0).map(f64::floor),
        BuiltinFunction::Ceil => arg(0).map(f64::ceil),
        BuiltinFunction::Sin => arg(0).map(f64::sin),
        BuiltinFunction::Cos => arg(0).map(f64::cos),
        BuiltinFunction::Tan => arg(0).map(f64::tan),
        BuiltinFunction::Asin => arg(0).map(f64::asin),
        BuiltinFunction::Acos => arg(0).map(f64::acos),
        BuiltinFunction::Atan => arg(0).map(f64::atan),
        BuiltinFunction::Atan2 => Some(arg(0)?.atan2(arg(1)?)),
        BuiltinFunction::Sinh => arg(0).map(f64::sinh),
        BuiltinFunction::Cosh => arg(0).map(f64::cosh),
        BuiltinFunction::Tanh => arg(0).map(f64::tanh),
        BuiltinFunction::Exp => arg(0).map(f64::exp),
        BuiltinFunction::Log => arg(0).map(f64::ln),
        BuiltinFunction::Log10 => arg(0).map(f64::log10),
        BuiltinFunction::Integer => arg(0).map(f64::floor),
        BuiltinFunction::Min => Some(arg(0)?.min(arg(1)?)),
        BuiltinFunction::Max => Some(arg(0)?.max(arg(1)?)),
        BuiltinFunction::Div => {
            let a = arg(0)?;
            let b = arg(1)?;
            if b.abs() < 1e-300 {
                None
            } else {
                Some((a / b).trunc())
            }
        }
        BuiltinFunction::Mod => {
            let a = arg(0)?;
            let b = arg(1)?;
            if b.abs() < 1e-300 {
                None
            } else {
                Some(a - (a / b).floor() * b)
            }
        }
        BuiltinFunction::Rem => {
            let a = arg(0)?;
            let b = arg(1)?;
            if b.abs() < 1e-300 {
                None
            } else {
                Some(a - (a / b).trunc() * b)
            }
        }
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Topological sort of parameters by start-expression dependencies
// ---------------------------------------------------------------------------

/// Collect all parameter/constant names referenced in an expression.
fn collect_param_refs(
    expr: &Expression,
    param_names: &std::collections::HashSet<String>,
) -> Vec<String> {
    let mut refs = Vec::new();
    collect_param_refs_inner(expr, param_names, &mut refs);
    refs
}

fn collect_param_refs_inner(
    expr: &Expression,
    param_names: &std::collections::HashSet<String>,
    refs: &mut Vec<String>,
) {
    match expr {
        Expression::VarRef { name, .. } => {
            let s = name.as_str().to_string();
            if param_names.contains(&s) && !refs.contains(&s) {
                refs.push(s);
            } else {
                // Also match base name for subscripted refs: "e[1]" → "e"
                let base = var_base_name(&s).to_string();
                if base != s && param_names.contains(&base) && !refs.contains(&base) {
                    refs.push(base);
                }
            }
        }
        Expression::Unary { rhs, .. } => {
            collect_param_refs_inner(rhs, param_names, refs);
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_param_refs_inner(lhs, param_names, refs);
            collect_param_refs_inner(rhs, param_names, refs);
        }
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_param_refs_inner(arg, param_names, refs);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_param_refs_inner(cond, param_names, refs);
                collect_param_refs_inner(then_expr, param_names, refs);
            }
            collect_param_refs_inner(else_branch, param_names, refs);
        }
        Expression::Array { elements, .. } => {
            for e in elements {
                collect_param_refs_inner(e, param_names, refs);
            }
        }
        _ => {}
    }
}

/// Topologically sort an `IndexMap` of variables by their start-expression
/// dependencies. Variables whose start expressions reference other variables
/// in the same map are placed after their dependencies.
///
/// Uses Kahn's algorithm. Cycles are broken arbitrarily (cyclic entries are
/// appended at the end in their original order).
fn topo_sort_by_start_deps(
    map: &indexmap::IndexMap<VarName, Variable>,
) -> indexmap::IndexMap<VarName, Variable> {
    use std::collections::{HashSet, VecDeque};

    if map.len() <= 1 {
        return map.clone();
    }

    let names: HashSet<String> = map.keys().map(|k| k.as_str().to_string()).collect();
    let name_list: Vec<String> = map.keys().map(|k| k.as_str().to_string()).collect();

    // Build adjacency: deps[i] = set of indices that i depends on
    let mut deps: Vec<HashSet<usize>> = Vec::with_capacity(map.len());
    for (_name, var) in map {
        let dep_indices = var
            .start
            .as_ref()
            .map(|start| {
                let self_idx = deps.len();
                collect_param_refs(start, &names)
                    .iter()
                    .filter_map(|r| name_list.iter().position(|n| n == r))
                    .filter(|&idx| idx != self_idx)
                    .collect()
            })
            .unwrap_or_default();
        deps.push(dep_indices);
    }

    // Kahn's algorithm
    let n = map.len();
    let mut in_degree = vec![0usize; n];
    let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (i, dep_set) in deps.iter().enumerate() {
        in_degree[i] = dep_set.len();
        for &d in dep_set {
            dependents[d].push(i);
        }
    }

    let mut queue: VecDeque<usize> = VecDeque::new();
    for (i, &deg) in in_degree.iter().enumerate() {
        if deg == 0 {
            queue.push_back(i);
        }
    }

    let mut order: Vec<usize> = Vec::with_capacity(n);
    while let Some(idx) = queue.pop_front() {
        order.push(idx);
        for &dep in &dependents[idx] {
            in_degree[dep] -= 1;
            if in_degree[dep] == 0 {
                queue.push_back(dep);
            }
        }
    }

    // Append any remaining (cyclic) entries in original order
    if order.len() < n {
        for i in 0..n {
            if !order.contains(&i) {
                order.push(i);
            }
        }
    }

    // Rebuild the map in topological order
    let entries: Vec<(VarName, Variable)> =
        map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
    let mut sorted = indexmap::IndexMap::with_capacity(n);
    for &idx in &order {
        let (k, v) = entries[idx].clone();
        sorted.insert(k, v);
    }
    sorted
}

/// Sort parameter and constant maps in the DAE by start-expression dependency
/// order. This ensures that when templates iterate `dae.p | items`, each
/// parameter's start expression can reference only previously-initialized
/// parameters.
pub fn sort_parameters_by_start_deps(dae: &mut Dae) {
    dae.constants = topo_sort_by_start_deps(&dae.constants);
    dae.parameters = topo_sort_by_start_deps(&dae.parameters);
}

/// Sort algebraic and output variable maps by equation dependency order.
///
/// For each variable in `dae.algebraics` or `dae.outputs`, finds its defining
/// equation in `dae.f_x` and extracts which other algebraic/output variables
/// the equation references. Then topologically sorts so that variables are
/// evaluated after their dependencies.
pub fn sort_algebraics_by_equation_deps(dae: &mut Dae) {
    use std::collections::HashSet;

    // Collect all algebraic + output variable names
    let alg_names: HashSet<String> = dae
        .algebraics
        .keys()
        .chain(dae.outputs.keys())
        .map(|k| k.as_str().to_string())
        .collect();

    if alg_names.len() <= 1 {
        return;
    }

    // For each algebraic/output variable, find which other alg/output vars
    // its equation references
    let mut eq_deps: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();

    for eq in &dae.f_x {
        let refs = collect_param_refs(&eq.rhs, &alg_names);
        // This equation may define one of our algebraic vars.
        // Try to identify which variable this equation defines
        // by checking if it matches the pattern `0 = var - expr` or additive form.
        for alg_name in &alg_names {
            if equation_defines_var(&eq.rhs, alg_name) {
                let deps: Vec<String> = refs
                    .iter()
                    .filter(|r| r.as_str() != alg_name.as_str())
                    .cloned()
                    .collect();
                eq_deps.insert(alg_name.clone(), deps);
            }
        }
    }

    // Sort dae.algebraics
    dae.algebraics = topo_sort_by_eq_deps(&dae.algebraics, &eq_deps);
    // Sort dae.outputs
    dae.outputs = topo_sort_by_eq_deps(&dae.outputs, &eq_deps);
}

/// Check if an equation's RHS defines a given variable (appears as LHS of subtraction
/// or as a term in an additive equation).
fn equation_defines_var(rhs: &Expression, var_name: &str) -> bool {
    match rhs {
        Expression::Binary {
            op,
            lhs,
            rhs: rhs_inner,
        } => {
            if matches!(op, rumoca_ir_core::OpBinary::Sub(_)) {
                // 0 = var - expr or 0 = expr - var
                if is_var_ref_named(lhs, var_name) || is_var_ref_named(rhs_inner, var_name) {
                    return true;
                }
            }
            if matches!(op, rumoca_ir_core::OpBinary::Add(_)) {
                // Check additive terms
                let terms = collect_additive_var_refs(rhs);
                if terms.iter().any(|t| t == var_name) {
                    return true;
                }
            }
            false
        }
        Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs: inner,
        } => equation_defines_var(inner, var_name),
        Expression::VarRef { name, .. } => name.as_str() == var_name,
        _ => false,
    }
}

/// Extract the base name from a possibly-subscripted variable name.
/// E.g., `"e[1]"` → `"e"`, `"q_err_w"` → `"q_err_w"`.
fn var_base_name(name: &str) -> &str {
    name.find('[').map_or(name, |i| &name[..i])
}

fn is_var_ref_named(expr: &Expression, name: &str) -> bool {
    matches!(expr, Expression::VarRef { name: n, .. } if n.as_str() == name || var_base_name(n.as_str()) == name)
}

/// Collect all VarRef names from an additive expression tree.
fn collect_additive_var_refs(expr: &Expression) -> Vec<String> {
    match expr {
        Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(_),
            lhs,
            rhs,
        }
        | Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            let mut v = collect_additive_var_refs(lhs);
            v.extend(collect_additive_var_refs(rhs));
            v
        }
        Expression::Unary { rhs, .. } => collect_additive_var_refs(rhs),
        Expression::VarRef { name, .. } => vec![var_base_name(name.as_str()).to_string()],
        _ => vec![],
    }
}

fn topo_sort_by_eq_deps(
    map: &indexmap::IndexMap<VarName, Variable>,
    eq_deps: &std::collections::HashMap<String, Vec<String>>,
) -> indexmap::IndexMap<VarName, Variable> {
    use std::collections::{HashSet, VecDeque};

    if map.len() <= 1 {
        return map.clone();
    }

    let name_list: Vec<String> = map.keys().map(|k| k.as_str().to_string()).collect();
    let name_set: HashSet<&str> = name_list.iter().map(|s| s.as_str()).collect();

    // Build adjacency
    let mut deps_idx: Vec<HashSet<usize>> = Vec::with_capacity(map.len());
    for name in &name_list {
        let dep_indices: HashSet<usize> = eq_deps
            .get(name)
            .map(|dep_names| {
                dep_names
                    .iter()
                    .filter(|d| name_set.contains(d.as_str()))
                    .filter_map(|d| name_list.iter().position(|n| n == d))
                    .collect()
            })
            .unwrap_or_default();
        deps_idx.push(dep_indices);
    }

    // Kahn's algorithm
    let n = map.len();
    let mut in_degree = vec![0usize; n];
    let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (i, dep_set) in deps_idx.iter().enumerate() {
        in_degree[i] = dep_set.len();
        for &d in dep_set {
            dependents[d].push(i);
        }
    }

    let mut queue: VecDeque<usize> = VecDeque::new();
    for (i, &deg) in in_degree.iter().enumerate() {
        if deg == 0 {
            queue.push_back(i);
        }
    }

    let mut order: Vec<usize> = Vec::with_capacity(n);
    while let Some(idx) = queue.pop_front() {
        order.push(idx);
        for &dep in &dependents[idx] {
            in_degree[dep] -= 1;
            if in_degree[dep] == 0 {
                queue.push_back(dep);
            }
        }
    }

    // Append cyclic entries in original order
    if order.len() < n {
        for i in 0..n {
            if !order.contains(&i) {
                order.push(i);
            }
        }
    }

    let entries: Vec<(VarName, Variable)> =
        map.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
    let mut sorted = indexmap::IndexMap::with_capacity(n);
    for &idx in &order {
        let (k, v) = entries[idx].clone();
        sorted.insert(k, v);
    }
    sorted
}

fn eval_named_function(name: &str, args: &[Expression], env: &HashMap<String, f64>) -> Option<f64> {
    let arg = |i: usize| args.get(i).and_then(|e| eval_const_expr(e, env));
    match name {
        "Integer" | "integer" | "floor" => arg(0).map(f64::floor),
        "ceil" => arg(0).map(f64::ceil),
        "abs" => arg(0).map(f64::abs),
        "sign" | "signum" => arg(0).map(f64::signum),
        "sqrt" => arg(0).map(f64::sqrt),
        "sin" => arg(0).map(f64::sin),
        "cos" => arg(0).map(f64::cos),
        "tan" => arg(0).map(f64::tan),
        "asin" => arg(0).map(f64::asin),
        "acos" => arg(0).map(f64::acos),
        "atan" => arg(0).map(f64::atan),
        "atan2" => Some(arg(0)?.atan2(arg(1)?)),
        "exp" => arg(0).map(f64::exp),
        "log" | "ln" => arg(0).map(f64::ln),
        "log10" => arg(0).map(f64::log10),
        "min" => Some(arg(0)?.min(arg(1)?)),
        "max" => Some(arg(0)?.max(arg(1)?)),
        _ => None,
    }
}
