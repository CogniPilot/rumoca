use super::*;

/// Infer output dimensions for single-output functions.
///
/// Multi-output functions are represented as tuples and are not treated as a
/// simple expression shape here.
pub(crate) fn flat_function_output_dims(func: &rumoca_ir_flat::Function) -> Option<Vec<i64>> {
    let output = func.outputs.first()?;
    if func.outputs.len() == 1 {
        Some(output.dims.clone())
    } else {
        None
    }
}

/// Compute total scalar size of a function's outputs.
pub(crate) fn flat_function_output_scalar_size(func: &rumoca_ir_flat::Function) -> usize {
    if func.outputs.is_empty() {
        return 1;
    }
    func.outputs
        .iter()
        .map(|o| compute_var_size(&o.dims).max(1))
        .sum::<usize>()
        .max(1)
}

/// Check if a flat expression is a subscripted element (e.g., `a[1]`).
///
/// After element-wise expansion, VarRefs like `a[1]` have embedded subscripts in the name
/// or explicit subscripts in the subscripts field. This indicates the expression refers to
/// a single element, not a full array.
pub(crate) fn is_subscripted_element(expr: &Expression) -> bool {
    match expr {
        Expression::VarRef { name, subscripts } => {
            !subscripts.is_empty() || name.as_str().contains('[')
        }
        _ => false,
    }
}

/// Extract an integer value from a flat expression literal.
pub(crate) fn extract_integer_from_flat_expr(expr: &Expression) -> Option<usize> {
    match expr {
        Expression::Literal(Literal::Integer(n)) => usize::try_from(*n).ok(),
        Expression::Literal(Literal::Real(f)) => {
            let n = *f as usize;
            if (n as f64 - *f).abs() < 0.001 {
                Some(n)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Build a prefix scalar-size map: for each variable prefix, sum the scalar sizes of its children.
/// This pre-computes the O(n) linear scan so individual lookups are O(1).
///
/// For record types like `Orientation = {T[3,3], w[3]}`, the prefix "rev.R_rel" maps to 12
/// (9 for T + 3 for w), not 2 (number of child entries). This ensures record-level equations
/// like `rev.R_rel = Frames.planarRotation(...)` get the correct scalar count.
pub(crate) fn build_prefix_counts(flat: &Model) -> FxHashMap<String, usize> {
    fn normalize_embedded_subscripts(name: &str) -> String {
        let mut out = String::with_capacity(name.len());
        let mut depth = 0usize;
        for ch in name.chars() {
            match ch {
                '[' => depth += 1,
                ']' => depth = depth.saturating_sub(1),
                _ if depth == 0 => out.push(ch),
                _ => {}
            }
        }
        out
    }

    let mut counts: FxHashMap<String, usize> = FxHashMap::default();
    for (name, var) in &flat.variables {
        let s = name.as_str();
        let scalar_size = compute_var_size(&var.dims);
        let normalized_full = normalize_embedded_subscripts(s);
        if normalized_full != s {
            *counts.entry(normalized_full).or_insert(0) += scalar_size;
        }
        // For "a.b.c", add scalar_size to prefixes "a.b" and "a"
        for (i, ch) in s.char_indices() {
            if ch != '.' {
                continue;
            }

            let prefix = &s[..i];
            *counts.entry(prefix.to_string()).or_insert(0) += scalar_size;

            let normalized = normalize_embedded_subscripts(prefix);
            if normalized != prefix {
                *counts.entry(normalized).or_insert(0) += scalar_size;
            }
        }
    }
    counts
}

/// Build a prefix-to-children index: maps each dotted prefix to all descendant variable names.
///
/// For flat variables `["a.b.c", "a.b.d", "a.e"]`, produces:
/// - `"a.b"` → `["a.b.c", "a.b.d"]`
/// - `"a"` → `["a.b.c", "a.b.d", "a.e"]`
///
/// This replaces O(n) linear scans with O(1) lookups in `collect_record_equation_defined_vars`,
/// `collect_algorithm_defined_vars`, and `extract_lhs_var_size`.
pub(crate) fn build_prefix_children(flat: &Model) -> FxHashMap<String, Vec<VarName>> {
    let mut children: FxHashMap<String, Vec<VarName>> = FxHashMap::default();
    for name in flat.variables.keys() {
        let s = name.as_str();
        for (i, ch) in s.char_indices() {
            if ch == '.' {
                let prefix = &s[..i];
                children
                    .entry(prefix.to_string())
                    .or_default()
                    .push(name.clone());
            }
        }
    }
    children
}

/// Extract the size of the LHS variable from a residual expression.
///
/// Returns Some(size) if the residual is `var - expr` where var is an array.
/// Also handles matrix equations like `[y] - expr = 0` where y is an array.
/// For record types (like Complex), counts the expanded component variables.
/// For tuple equations like `(a, b) = func(...)`, counts the tuple elements.
pub(crate) fn extract_lhs_var_size(
    residual: &Expression,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Option<usize> {
    let linearized_embedded_lhs_bases = HashSet::default();
    extract_lhs_var_size_with_linearized_bases(
        residual,
        flat,
        prefix_counts,
        &linearized_embedded_lhs_bases,
    )
}

fn extract_conditional_residual_lhs_size(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
    linearized_embedded_lhs_bases: &HashSet<VarName>,
) -> Option<usize> {
    let mut branch_sizes = Vec::with_capacity(branches.len() + 1);
    for (_, branch_expr) in branches {
        let size = extract_lhs_var_size_with_linearized_bases(
            branch_expr,
            flat,
            prefix_counts,
            linearized_embedded_lhs_bases,
        )?;
        branch_sizes.push(size);
    }

    let else_size = extract_lhs_var_size_with_linearized_bases(
        else_branch,
        flat,
        prefix_counts,
        linearized_embedded_lhs_bases,
    )?;
    branch_sizes.push(else_size);

    if let Some(&first) = branch_sizes.first()
        && branch_sizes.iter().all(|&size| size == first)
    {
        return Some(first);
    }
    None
}

fn extract_lhs_var_size_from_var_name(
    lhs: &Expression,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Option<usize> {
    // Try to extract the variable name from the LHS (no subscripts)
    let var_name = extract_var_from_lhs(lhs)?;

    // Look up the variable and return its size
    if let Some(var) = flat.variables.get(&var_name) {
        return Some(compute_var_size(&var.dims));
    }

    // Check if the LHS has embedded subscripts (e.g., "body.g_0[1]").
    // The flat variable map stores the base name "body.g_0", not the subscripted form.
    // When ALL dimensions are subscripted, this is a scalar equation (count=1).
    // Range subscripts like [2:n] preserve the dimension with reduced size.
    if let Some(base) = strip_embedded_subscripts(var_name.as_str())
        && let Some(var) = flat.variables.get(&VarName::new(base))
    {
        if has_embedded_range_subscript(var_name.as_str()) {
            return Some(compute_embedded_range_size(
                var_name.as_str(),
                &var.dims,
                flat,
            ));
        }
        let n = count_embedded_subscripts(var_name.as_str());
        if n >= var.dims.len() {
            // All dimensions subscripted -> scalar equation
            if var.dims.iter().any(|&d| d <= 0) {
                return Some(0);
            }
            return Some(1);
        }
        // Partially subscripted -> remaining dimensions
        return Some(compute_var_size(&var.dims[n..]));
    }

    // Variable not found directly - check for expanded record components.
    if let Some(&count) = prefix_counts.get(var_name.as_str())
        && count > 0
    {
        return Some(count);
    }

    // Also try progressively stripping subscripts:
    // "port_a[1].T[1]" -> "port_a[1].T" -> "port_a.T"
    for base in subscript_fallback_chain(&var_name) {
        if let Some(var) = flat.variables.get(&base) {
            // Subscripted element of a plain variable -> scalar element
            if var.dims.iter().any(|&d| d <= 0) {
                return Some(0);
            }
            return Some(1);
        }
        // MLS §10.2: Subscripted record array element like "bw[1]" where "bw"
        // is a record prefix (Complex[N]). Compute per-element record size.
        if let Some(&total) = prefix_counts.get(base.as_str()) {
            return Some(record_subscript_scalar_size(
                var_name.as_str(),
                base.as_str(),
                total,
                flat,
            ));
        }
    }

    // Check for unevaluated subscript expressions before concluding zero-sized.
    // VarRefs like "pc[((2 * 1) - 1)].i" have unsimplified arithmetic subscripts.
    if has_evaluable_arithmetic_subscript(var_name.as_str()) {
        return None;
    }

    // LHS variable is absent from flat.variables and prefix_counts entirely.
    // It was eliminated as a zero-sized array (MLS §10.1).
    Some(0)
}

pub(crate) fn extract_lhs_var_size_with_linearized_bases(
    residual: &Expression,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
    linearized_embedded_lhs_bases: &HashSet<VarName>,
) -> Option<usize> {
    // Conditional residuals can be produced by flattened equations such as:
    //   0 = if cond then (y - f(...)) else (y - g(...))
    // Infer scalar count from branch residuals when they agree.
    if let Expression::If {
        branches,
        else_branch,
    } = residual
    {
        return extract_conditional_residual_lhs_size(
            branches,
            else_branch,
            flat,
            prefix_counts,
            linearized_embedded_lhs_bases,
        );
    }

    // Pattern match: residual = lhs - rhs
    let Expression::Binary { op, lhs, .. } = residual else {
        return None;
    };

    // Must be a subtraction
    if !matches!(op, ast::OpBinary::Sub(_)) {
        return None;
    }

    // Handle tuple LHS: (a, b, ...) = rhs
    // MLS §8.4: tuple equations count as the sum of scalar sizes of all elements.
    // For (b, a) where b is Real[2] and a is Real[2], that's 2+2=4 scalar equations.
    // Skip discrete variables — they don't contribute to the continuous balance.
    if let Expression::Tuple { elements } = lhs.as_ref() {
        let continuous_scalar_count: usize = elements
            .iter()
            .filter(|e| {
                let Expression::VarRef { name, .. } = e else {
                    return true;
                };
                !flat
                    .variables
                    .get(name)
                    .is_some_and(|v| matches!(v.variability, Variability::Discrete(_)))
            })
            .map(|e| tuple_element_scalar_size(e, flat, prefix_counts))
            .sum();
        return Some(continuous_scalar_count.max(1));
    }

    // Handle Array LHS (vector/matrix equations).
    // MLS §10.6.1: Equations like [der(x); y] = [y; u] are matrix equations where
    // each leaf element is a scalar equation. This also handles single-element array
    // literals like `{0} = f(x)` as exactly one scalar equation.
    if let Expression::Array { .. } = lhs.as_ref() {
        return Some(count_array_lhs_scalar_elements(lhs, flat, prefix_counts));
    }

    // Handle array-construction builtins on LHS: zeros(3), ones(3), fill(v,N), etc.
    if let Expression::BuiltinCall { function, args } = lhs.as_ref()
        && let Some(size) = extract_builtin_array_size(function, args)
    {
        return Some(size);
    }

    // Handle function-call LHS by using function output dimensions.
    // Example: angularVelocity2(R_b) = w_rel_b should count as 3 scalars,
    // not by the size of record-typed argument R_b (12 scalars).
    if let Expression::FunctionCall { name, .. } = lhs.as_ref()
        && let Some(size) = infer_function_output_scalar_size(name, flat)
    {
        return Some(size);
    }

    // Handle subscripted VarRef: var[i] where var is a multi-dimensional array.
    if let Expression::VarRef { name, subscripts } = lhs.as_ref() {
        // Explicit subscripts in the subscripts field
        if !subscripts.is_empty()
            && let Some(var) = flat.variables.get(name)
        {
            return Some(compute_subscripted_size(&var.dims, subscripts));
        }
        // Embedded subscripts in the name: "RotationMatrix[1]" → "RotationMatrix"
        if let Some(size) =
            resolve_embedded_subscript_size(name.as_str(), flat, linearized_embedded_lhs_bases)
        {
            return Some(size);
        }
    }

    extract_lhs_var_size_from_var_name(lhs, flat, prefix_counts)
}

/// Extract a variable name from an LHS expression.
///
/// Handles:
/// - Direct VarRef: `y`
/// - Array-wrapped VarRef: `[y]` (common in Multiplex equations)
/// - der() call: `der(x)` (for ODE equations)
/// - Unary minus: `-y` (common in Modelica equations)
pub(crate) fn extract_var_from_lhs(lhs: &Expression) -> Option<VarName> {
    match lhs {
        // Direct variable reference without subscripts
        Expression::VarRef { name, subscripts } if subscripts.is_empty() => Some(name.clone()),
        // der(x) - extract the variable from inside the derivative
        Expression::BuiltinCall { function, args }
            if matches!(function, rumoca_ir_flat::BuiltinFunction::Der) && args.len() == 1 =>
        {
            if let Expression::VarRef { name, subscripts } = &args[0]
                && subscripts.is_empty()
            {
                return Some(name.clone());
            }
            None
        }
        // Array containing a single variable reference: [y]
        // This pattern appears in equations like `[y] = [[u1], [u2], ...]`
        Expression::Array { elements, .. } if elements.len() == 1 => {
            if let Expression::VarRef { name, subscripts } = &elements[0]
                && subscripts.is_empty()
            {
                return Some(name.clone());
            }
            None
        }
        // Unary minus wrapping a variable reference: -y
        // This pattern appears in equations like `-spacePhasor.i_ = TransformationMatrix * i`
        Expression::Unary { rhs, .. } => {
            if let Expression::VarRef { name, subscripts } = rhs.as_ref()
                && subscripts.is_empty()
            {
                return Some(name.clone());
            }
            None
        }
        _ => None,
    }
}

/// Create a Variable from a flat::Variable.
pub(crate) fn resolve_missing_start_ref(
    name: &VarName,
    known_var_names: &HashSet<String>,
) -> VarName {
    if known_var_names.contains(name.as_str()) {
        return name.clone();
    }

    let mut segments: Vec<&str> = name.as_str().split('.').collect();
    while segments.len() > 1 {
        segments.remove(0);
        let candidate = segments.join(".");
        if known_var_names.contains(candidate.as_str()) {
            return VarName::new(candidate);
        }
    }
    name.clone()
}

pub(crate) fn rewrite_start_subscripts(
    subscripts: &[Subscript],
    known_var_names: &HashSet<String>,
) -> Vec<Subscript> {
    subscripts
        .iter()
        .map(|sub| match sub {
            Subscript::Expr(sub_expr) => Subscript::Expr(Box::new(
                rewrite_start_expr_missing_refs(sub_expr, known_var_names),
            )),
            _ => sub.clone(),
        })
        .collect()
}

pub(crate) fn rewrite_start_expr_list(
    elements: &[Expression],
    known_var_names: &HashSet<String>,
) -> Vec<Expression> {
    elements
        .iter()
        .map(|elem| rewrite_start_expr_missing_refs(elem, known_var_names))
        .collect()
}

pub(crate) fn rewrite_start_expr_missing_refs(
    expr: &Expression,
    known_var_names: &HashSet<String>,
) -> Expression {
    match expr {
        Expression::VarRef { name, subscripts } => Expression::VarRef {
            name: resolve_missing_start_ref(name, known_var_names),
            subscripts: rewrite_start_subscripts(subscripts, known_var_names),
        },
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(rewrite_start_expr_missing_refs(lhs, known_var_names)),
            rhs: Box::new(rewrite_start_expr_missing_refs(rhs, known_var_names)),
        },
        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(rewrite_start_expr_missing_refs(rhs, known_var_names)),
        },
        Expression::BuiltinCall { function, args } => Expression::BuiltinCall {
            function: *function,
            args: rewrite_start_expr_list(args, known_var_names),
        },
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => Expression::FunctionCall {
            name: name.clone(),
            args: rewrite_start_expr_list(args, known_var_names),
            is_constructor: *is_constructor,
        },
        Expression::If {
            branches,
            else_branch,
        } => Expression::If {
            branches: branches
                .iter()
                .map(|(cond, value)| {
                    (
                        rewrite_start_expr_missing_refs(cond, known_var_names),
                        rewrite_start_expr_missing_refs(value, known_var_names),
                    )
                })
                .collect(),
            else_branch: Box::new(rewrite_start_expr_missing_refs(
                else_branch,
                known_var_names,
            )),
        },
        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: rewrite_start_expr_list(elements, known_var_names),
            is_matrix: *is_matrix,
        },
        Expression::Tuple { elements } => Expression::Tuple {
            elements: rewrite_start_expr_list(elements, known_var_names),
        },
        Expression::Range { start, step, end } => Expression::Range {
            start: Box::new(rewrite_start_expr_missing_refs(start, known_var_names)),
            step: step
                .as_ref()
                .map(|s| Box::new(rewrite_start_expr_missing_refs(s, known_var_names))),
            end: Box::new(rewrite_start_expr_missing_refs(end, known_var_names)),
        },
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => Expression::ArrayComprehension {
            expr: Box::new(rewrite_start_expr_missing_refs(expr, known_var_names)),
            indices: indices
                .iter()
                .map(|index| ComprehensionIndex {
                    name: index.name.clone(),
                    range: rewrite_start_expr_missing_refs(&index.range, known_var_names),
                })
                .collect(),
            filter: filter
                .as_ref()
                .map(|f| Box::new(rewrite_start_expr_missing_refs(f, known_var_names))),
        },
        Expression::Index { base, subscripts } => Expression::Index {
            base: Box::new(rewrite_start_expr_missing_refs(base, known_var_names)),
            subscripts: rewrite_start_subscripts(subscripts, known_var_names),
        },
        Expression::FieldAccess { base, field } => Expression::FieldAccess {
            base: Box::new(rewrite_start_expr_missing_refs(base, known_var_names)),
            field: field.clone(),
        },
        _ => expr.clone(),
    }
}

pub(crate) fn create_dae_variable(
    name: &VarName,
    var: &flat::Variable,
    known_var_names: &HashSet<String>,
) -> Variable {
    // MLS §4.4.1: declaration/modification bindings define parameter/constant values.
    // Start remains an initialization attribute and should not be used as the
    // primary value when a binding exists.
    let start = match var.variability {
        Variability::Parameter(_) | Variability::Constant(_) => {
            var.binding.clone().or_else(|| var.start.clone())
        }
        _ => var.start.clone(),
    }
    .map(|expr| project_scalar_start_record_alias(name, &expr, known_var_names))
    .map(|expr| rewrite_start_expr_missing_refs(&expr, known_var_names));

    Variable {
        name: name.clone(),
        dims: var.dims.clone(),
        start,
        fixed: var.fixed,
        min: var.min.clone(),
        max: var.max.clone(),
        nominal: var.nominal.clone(),
        unit: var.unit.clone(),
        state_select: var.state_select,
        description: None,
    }
}

fn project_scalar_start_record_alias(
    lhs_name: &VarName,
    expr: &Expression,
    known_var_names: &HashSet<String>,
) -> Expression {
    let Some(lhs_base) = flat::component_base_name(lhs_name.as_str()) else {
        return expr.clone();
    };
    if lhs_base == lhs_name.as_str() {
        return expr.clone();
    }
    let Some(field_suffix) = lhs_name.as_str().strip_prefix(lhs_base.as_str()) else {
        return expr.clone();
    };
    if !field_suffix.starts_with('.') {
        return expr.clone();
    }

    match expr {
        Expression::VarRef {
            name: rhs_name,
            subscripts,
        } if subscripts.is_empty() => {
            let projected = format!("{}{}", rhs_name.as_str(), field_suffix);
            if known_var_names.contains(projected.as_str()) {
                return Expression::VarRef {
                    name: VarName::new(projected),
                    subscripts: vec![],
                };
            }
            expr.clone()
        }
        Expression::FieldAccess { base, field } => {
            let Expression::VarRef {
                name: rhs_name,
                subscripts,
            } = base.as_ref()
            else {
                return expr.clone();
            };
            if !subscripts.is_empty() {
                return expr.clone();
            }
            let projected = format!("{}.{}{}", rhs_name.as_str(), field, field_suffix);
            if known_var_names.contains(projected.as_str()) {
                return Expression::VarRef {
                    name: VarName::new(projected),
                    subscripts: vec![],
                };
            }
            expr.clone()
        }
        _ => expr.clone(),
    }
}
