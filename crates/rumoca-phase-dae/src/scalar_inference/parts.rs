use super::*;

/// Infer scalar count from a pre-collected set of variable references.
pub(crate) fn infer_scalar_count_from_collected_varrefs(
    var_refs: &[CollectedVarRef],
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Option<usize> {
    if var_refs.is_empty() {
        return None;
    }

    let mut max_size = 0;
    let mut any_found = false;
    let mut has_unresolved_arithmetic_subscript = false;
    for var_ref in var_refs {
        let var_name = &var_ref.name;

        // Direct variable lookup in flat.variables.
        if let Some(var) = flat.variables.get(var_name) {
            // Skip parameters, constants, and discrete variables — they are not
            // continuous unknowns and their array dimensions shouldn't determine
            // the equation's scalar count in the continuous balance.
            // E.g., `y = TransformationMatrix * u` where TransformationMatrix[2,3] is a
            // parameter should count based on y[2], not TransformationMatrix's size=6.
            // For discrete: `(r64, state64) = random(previous(state64))` where
            // state64 is discrete Real[2] should count based on r64 (1), not state64 (2).
            if matches!(
                var.variability,
                Variability::Parameter(_) | Variability::Constant(_) | Variability::Discrete(_)
            ) {
                any_found = true;
                continue;
            }

            // Respect explicit subscripts on the VarRef. Indexed accesses are scalarized
            // and must not use the full base-array size.
            let size = if var_ref.subscripts.is_empty() {
                compute_var_size(&var.dims)
            } else {
                compute_subscripted_size(&var.dims, &var_ref.subscripts)
            };
            any_found = true;
            max_size = max_size.max(size);
            continue;
        }

        // Unevaluated arithmetic subscripts (e.g., `pc[((2 * 1) - 1)].i`) can
        // appear after for-loop substitution. Avoid mapping these to base-array
        // aggregate sizes; leave unresolved so callers can fall back conservatively.
        if has_evaluable_arithmetic_subscript(var_name.as_str()) {
            has_unresolved_arithmetic_subscript = true;
            continue;
        }

        // Skip parameters, constants, and discrete variables — they are not
        // continuous unknowns and their array dimensions shouldn't determine
        // the equation's scalar count in the continuous balance.
        // E.g., `y = TransformationMatrix * u` where TransformationMatrix[2,3] is a
        // parameter should count based on y[2], not TransformationMatrix's size=6.
        // For discrete: `(r64, state64) = random(previous(state64))` where
        // state64 is discrete Real[2] should count based on r64 (1), not state64 (2).
        // Check prefix_counts first (record expansion)
        if var_ref.subscripts.is_empty()
            && let Some(&count) = prefix_counts.get(var_name.as_str())
        {
            any_found = true;
            if count > max_size {
                max_size = count;
            }
            continue;
        }

        // Try progressively stripping embedded subscripts:
        // "a[1].b[2]" -> "a[1].b" -> "a.b"
        let fallback_chain = subscript_fallback_chain(var_name);
        let per_elem = if fallback_chain
            .iter()
            .any(|candidate| flat.variables.contains_key(candidate))
        {
            Some(1)
        } else {
            infer_record_subscript_size_from_prefix_chain(
                var_name,
                fallback_chain,
                prefix_counts,
                flat,
            )
        };
        if let Some(size) = per_elem {
            any_found = true;
            max_size = max_size.max(size);
            continue;
        }
    }

    if max_size > 0 {
        Some(max_size)
    } else if any_found {
        // All found variables had size 0 (zero-sized arrays like Real[0]).
        // Return Some(0) so the equation is correctly eliminated.
        Some(0)
    } else {
        // No variables found at all.
        // Check for unevaluated subscript expressions before concluding zero-sized.
        // VarRefs like "pc[((2 * 1) - 1)].i" arise from for-loop expansion with
        // unsimplified arithmetic — they ARE present in flat.variables under their
        // evaluated names (e.g. "pc[1].i") but can't be matched here.
        if has_unresolved_arithmetic_subscript {
            None
        } else {
            // Genuinely absent — eliminated as zero-sized arrays (MLS §10.1).
            Some(0)
        }
    }
}

/// Collect VarRefs from an expression, skipping arguments to reduction builtins.
///
/// Reduction builtins like sum(), product() reduce arrays to scalars (MLS §10.3.4),
/// so their array arguments should not inflate the equation's scalar count.
pub(crate) fn collect_var_refs_skip_reductions(expr: &Expression, vars: &mut Vec<CollectedVarRef>) {
    match expr {
        Expression::VarRef { name, subscripts } => {
            vars.push(CollectedVarRef {
                name: name.clone(),
                subscripts: subscripts.clone(),
            });
            for sub in subscripts {
                if let Subscript::Expr(e) = sub {
                    collect_var_refs_skip_reductions(e, vars);
                }
            }
        }
        Expression::BuiltinCall { function, args } => {
            if is_reduction_builtin(function) {
                // Skip args — reduction output is scalar regardless of input size
                return;
            }
            for arg in args {
                collect_var_refs_skip_reductions(arg, vars);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_var_refs_skip_reductions(lhs, vars);
            collect_var_refs_skip_reductions(rhs, vars);
        }
        Expression::Unary { rhs, .. } => {
            collect_var_refs_skip_reductions(rhs, vars);
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_var_refs_skip_reductions(arg, vars);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_var_refs_skip_reductions(cond, vars);
                collect_var_refs_skip_reductions(then_expr, vars);
            }
            collect_var_refs_skip_reductions(else_branch, vars);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for e in elements {
                collect_var_refs_skip_reductions(e, vars);
            }
        }
        Expression::Range { start, step, end } => {
            collect_var_refs_skip_reductions(start, vars);
            if let Some(s) = step {
                collect_var_refs_skip_reductions(s, vars);
            }
            collect_var_refs_skip_reductions(end, vars);
        }
        Expression::Index { base, subscripts } => {
            collect_var_refs_skip_reductions(base, vars);
            for sub in subscripts {
                if let Subscript::Expr(e) = sub {
                    collect_var_refs_skip_reductions(e, vars);
                }
            }
        }
        Expression::FieldAccess { base, .. } => {
            collect_var_refs_skip_reductions(base, vars);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            for index in indices {
                collect_var_refs_skip_reductions(&index.range, vars);
            }
            collect_var_refs_skip_reductions(expr, vars);
            if let Some(filter_expr) = filter.as_ref() {
                collect_var_refs_skip_reductions(filter_expr, vars);
            }
        }
        Expression::Literal(_) | Expression::Empty => {}
    }
}

/// Collect VarRefs from an expression while skipping function-call arguments.
///
/// Function-call arguments are not necessarily shaped like the function output.
/// Counting them can overestimate scalar equation size for equations like
/// `f(recordArg) = y` where `recordArg` is much larger than `f`'s output.
pub(crate) fn collect_var_refs_skip_reductions_and_function_args(
    expr: &Expression,
    vars: &mut Vec<CollectedVarRef>,
) {
    match expr {
        Expression::VarRef { name, subscripts } => {
            vars.push(CollectedVarRef {
                name: name.clone(),
                subscripts: subscripts.clone(),
            });
            for sub in subscripts {
                if let Subscript::Expr(e) = sub {
                    collect_var_refs_skip_reductions_and_function_args(e, vars);
                }
            }
        }
        Expression::BuiltinCall { function, args } => {
            if is_reduction_builtin(function) {
                return;
            }
            for arg in args {
                collect_var_refs_skip_reductions_and_function_args(arg, vars);
            }
        }
        Expression::FunctionCall { .. } => {
            // Intentionally skip function arguments.
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_var_refs_skip_reductions_and_function_args(lhs, vars);
            collect_var_refs_skip_reductions_and_function_args(rhs, vars);
        }
        Expression::Unary { rhs, .. } => {
            collect_var_refs_skip_reductions_and_function_args(rhs, vars);
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_var_refs_skip_reductions_and_function_args(cond, vars);
                collect_var_refs_skip_reductions_and_function_args(then_expr, vars);
            }
            collect_var_refs_skip_reductions_and_function_args(else_branch, vars);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for e in elements {
                collect_var_refs_skip_reductions_and_function_args(e, vars);
            }
        }
        Expression::Range { start, step, end } => {
            collect_var_refs_skip_reductions_and_function_args(start, vars);
            if let Some(s) = step {
                collect_var_refs_skip_reductions_and_function_args(s, vars);
            }
            collect_var_refs_skip_reductions_and_function_args(end, vars);
        }
        Expression::Index { base, subscripts } => {
            collect_var_refs_skip_reductions_and_function_args(base, vars);
            for sub in subscripts {
                if let Subscript::Expr(e) = sub {
                    collect_var_refs_skip_reductions_and_function_args(e, vars);
                }
            }
        }
        Expression::FieldAccess { base, .. } => {
            collect_var_refs_skip_reductions_and_function_args(base, vars);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            for index in indices {
                collect_var_refs_skip_reductions_and_function_args(&index.range, vars);
            }
            collect_var_refs_skip_reductions_and_function_args(expr, vars);
            if let Some(filter_expr) = filter.as_ref() {
                collect_var_refs_skip_reductions_and_function_args(filter_expr, vars);
            }
        }
        Expression::Literal(_) | Expression::Empty => {}
    }
}

/// Check if a builtin function is an array reduction (MLS §10.3.4).
///
/// Reduction builtins produce scalar output from array input,
/// so array VarRefs inside them should not affect equation scalar count.
pub(crate) fn is_reduction_builtin(f: &BuiltinFunction) -> bool {
    matches!(
        f,
        BuiltinFunction::Sum
            | BuiltinFunction::Product
            | BuiltinFunction::Ndims
            | BuiltinFunction::Size
            | BuiltinFunction::Scalar
    )
}

/// Compute the scalar size of a single tuple element expression.
///
/// MLS §8.4: Each element of a tuple equation contributes its full scalar size.
/// For a VarRef to Real[2], this returns 2 (not 1).
pub(crate) fn tuple_element_scalar_size(
    e: &Expression,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> usize {
    if let Expression::VarRef { name, .. } = e {
        // Look up array dimensions in flat variables
        if let Some(var) = flat.variables.get(name) {
            return compute_var_size(&var.dims);
        }
        // Try prefix_counts for record types
        if let Some(&count) = prefix_counts.get(name.as_str()) {
            return count;
        }
    }
    1 // Default: scalar
}

/// Count the total number of scalar leaf elements in a nested Array expression.
///
/// MLS §10.6.1: Matrix equations like `{{der(x)}, {y}} = {{y}, {u}}` represent
/// N scalar equations where N is the number of leaf elements. For a 2×1 matrix,
/// this returns 2. Leaf VarRefs are looked up in `flat.variables` to account
/// for array-typed variables (e.g., a VarRef to Real[3] contributes 3 scalars).
pub(crate) fn count_array_lhs_scalar_elements(
    expr: &Expression,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> usize {
    fn leaf_scalar_size(
        expr: &Expression,
        flat: &Model,
        prefix_counts: &FxHashMap<String, usize>,
    ) -> usize {
        match expr {
            Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
                if let Some(var) = flat.variables.get(name) {
                    compute_var_size(&var.dims)
                } else if let Some(&count) = prefix_counts.get(name.as_str()) {
                    count
                } else {
                    1
                }
            }
            // MLS §8.3.2/§10.6.1: der(v) preserves the scalar size of v.
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args,
            } if args.len() == 1 => leaf_scalar_size(&args[0], flat, prefix_counts),
            _ => 1,
        }
    }

    match expr {
        Expression::Array { elements, .. } => elements
            .iter()
            .map(|e| count_array_lhs_scalar_elements(e, flat, prefix_counts))
            .sum(),
        _ => leaf_scalar_size(expr, flat, prefix_counts),
    }
}

/// Extract scalar size from array-construction builtins on the LHS of a residual.
///
/// Handles: `zeros(N)`, `ones(N)`, `fill(v,N,M,...)`, `identity(N)`, `cross(a,b)`, `skew(a)`.
/// Returns `Some(size)` if the function has a known output shape, `None` otherwise.
pub(crate) fn extract_builtin_array_size(
    function: &BuiltinFunction,
    args: &[Expression],
) -> Option<usize> {
    match function {
        BuiltinFunction::Zeros | BuiltinFunction::Ones => {
            // zeros(n1, n2, ...) / ones(n1, n2, ...) → product of dimensions
            args.iter()
                .map(extract_integer_from_flat_expr)
                .try_fold(1usize, |acc, dim| dim.map(|d| acc * d))
        }
        BuiltinFunction::Fill => {
            // fill(value, n1, n2, ...) → product of n1*n2*...
            if args.len() > 1 {
                args[1..]
                    .iter()
                    .map(extract_integer_from_flat_expr)
                    .try_fold(1usize, |acc, dim| dim.map(|d| acc * d))
            } else {
                None
            }
        }
        BuiltinFunction::Linspace => {
            // linspace(x1, x2, n) -> Real[n], with n >= 2
            if args.len() != 3 {
                return None;
            }
            extract_integer_from_flat_expr(&args[2]).filter(|&n| n >= 2)
        }
        BuiltinFunction::Identity => {
            // identity(n) → n*n
            extract_integer_from_flat_expr(&args[0]).map(|n| n * n)
        }
        BuiltinFunction::Cross => {
            // cross(a,b) returns Real[3] only when args are full 3-vectors.
            // After element-wise expansion, cross(a[1], b[1]) is scalar.
            if args.iter().any(is_subscripted_element) {
                None
            } else {
                Some(3)
            }
        }
        BuiltinFunction::Skew => {
            // skew(a) returns Real[3,3] only for full 3-vector arg.
            if args.iter().any(is_subscripted_element) {
                None
            } else {
                Some(9)
            }
        }
        _ => None,
    }
}

/// Infer scalar output size of a function call expression.
pub(crate) fn infer_function_output_dims(name: &VarName, flat: &Model) -> Option<Vec<i64>> {
    resolve_flat_function(name, flat).and_then(flat_function_output_dims)
}

pub(crate) fn infer_function_output_scalar_size(name: &VarName, flat: &Model) -> Option<usize> {
    resolve_flat_function(name, flat).map(flat_function_output_scalar_size)
}
