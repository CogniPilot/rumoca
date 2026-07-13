use super::*;

pub(crate) fn collect_var_refs(expr: &Expression) -> HashSet<VarName> {
    let mut refs = HashSet::default();
    expr.collect_var_refs(&mut refs);
    refs
}

/// Resolve a variable name against a set with subscript fallback.
///
/// This treats refs like `x[1]` as `x` when the set stores the base variable.
pub(crate) fn resolve_name_against_set(name: &VarName, set: &HashSet<VarName>) -> Option<VarName> {
    name_resolution::resolve_name_against_set(name, set)
}

/// Compute the scalar size of a variable from its dimensions.
///
/// Per MLS §8.4, array variables represent multiple scalar unknowns.
/// The size is the product of all dimensions, or 1 for scalars.
pub(crate) fn compute_var_size(dims: &[i64]) -> usize {
    scalar_size::compute_var_size(dims)
}

pub(crate) fn compute_subscripted_size_with_context(
    dims: &[i64],
    subscripts: &[Subscript],
    flat: &Model,
) -> Option<usize> {
    scalar_size::compute_subscripted_size_with_context(dims, subscripts, flat)
}

/// Check if a VarRef name contains evaluable integer arithmetic in subscripts.
///
/// Returns true ONLY when the subscript contains integer arithmetic expressions
/// (digits, `+`, `-`, `*`, `/`, parentheses, spaces) — e.g. `pc[((2 * 1) - 1)].i`.
/// Returns false for simple subscripts (`pc[1].i`), unresolved variable names
/// (`x[n]`, `suspend[i]`), or names without subscripts.
///
/// This distinguishes genuinely-evaluable arithmetic (from for-loop expansion
/// where arithmetic wasn't simplified) from truly absent variables (zero-sized
/// arrays or unresolved loop variables).
pub(crate) fn has_evaluable_arithmetic_subscript(name: &str) -> bool {
    scalar_size::has_evaluable_arithmetic_subscript(name)
}

/// Strip embedded subscripts from a variable name.
///
/// Returns the base name if subscripts are present, e.g. "RotationMatrix[1]" → "RotationMatrix".
pub(crate) fn strip_embedded_subscripts(name: &str) -> Option<&str> {
    scalar_size::strip_embedded_subscripts(name)
}

/// Count the number of embedded subscript indices in a variable name.
///
/// Each comma-separated value inside brackets counts as one index.
/// E.g. "R[1]" → 1, "M[1][2]" → 2, "T[1,2]" → 2, "A[1,2][3]" → 3, "x" → 0.
pub(crate) fn count_embedded_subscripts(name: &str) -> usize {
    scalar_size::count_embedded_subscripts(name)
}

pub(crate) fn collect_linearized_embedded_lhs_bases(flat: &Model) -> HashSet<VarName> {
    scalar_size::collect_linearized_embedded_lhs_bases(flat)
}

/// Check whether a variable name contains an embedded range subscript.
///
/// A range subscript has `:` inside brackets, e.g., `x[2:n]` or `x[1:(n-1)]`.
/// Simple scalar subscripts like `x[1]` or `x[1,2]` do not contain `:`.
pub(crate) fn has_embedded_range_subscript(name: &str) -> bool {
    scalar_size::has_embedded_range_subscript(name)
}

/// Compute the scalar size for a variable with embedded range subscripts.
///
/// For each bracket group, processes each comma-separated subscript:
/// - Range subscripts (containing `:`) are evaluated to compute the range size
/// - Scalar subscripts select a single element (size 1 per dimension)
///
/// Falls back to the full dimension size when a range cannot be evaluated.
pub(crate) fn compute_embedded_range_size(name: &str, dims: &[i64], flat: &Model) -> usize {
    scalar_size::compute_embedded_range_size(name, dims, flat)
}

/// Resolve the scalar size of a VarRef with embedded subscripts.
///
/// When flatten expands multi-dim array equations (e.g. `RotationMatrix = {{...}}`),
/// it creates partially-expanded equations like `RotationMatrix[1] = ...` where
/// the subscript is embedded in the VarName string. This function strips the
/// embedded subscripts, looks up the base variable, and computes the remaining
/// scalar count from the unindexed dimensions.
pub(crate) fn resolve_embedded_subscript_size(
    name: &str,
    flat: &Model,
    linearized_embedded_lhs_bases: &HashSet<VarName>,
) -> Option<usize> {
    scalar_size::resolve_embedded_subscript_size(name, flat, linearized_embedded_lhs_bases)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ExpressionForm {
    Scalar,
    Vector(usize),
    Matrix(usize, usize),
    Other,
}

pub(crate) fn expression_form_from_dims(dims: &[i64]) -> ExpressionForm {
    match dims {
        [] => ExpressionForm::Scalar,
        [d] => ExpressionForm::Vector((*d).max(0) as usize),
        [r, c] => ExpressionForm::Matrix((*r).max(0) as usize, (*c).max(0) as usize),
        _ => ExpressionForm::Other,
    }
}

pub(crate) fn apply_subscripts_to_dims(dims: &[i64], subscripts: &[Subscript]) -> Vec<i64> {
    let mut remaining_dims = Vec::new();
    let mut dim_idx = 0usize;

    for sub in subscripts {
        if dim_idx >= dims.len() {
            break;
        }
        match sub {
            Subscript::Index { .. } | Subscript::Expr { expr: _, .. } => {
                dim_idx += 1;
            }
            Subscript::Colon { .. } => {
                remaining_dims.push(dims[dim_idx]);
                dim_idx += 1;
            }
        }
    }
    remaining_dims.extend_from_slice(&dims[dim_idx..]);
    remaining_dims
}

pub(crate) fn apply_subscripts_to_dims_with_context(
    dims: &[i64],
    subscripts: &[Subscript],
    flat: &Model,
) -> Option<Vec<i64>> {
    scalar_size::apply_subscripts_to_dims_with_context(dims, subscripts, flat)
}

pub(crate) fn infer_varref_form(
    name: &str,
    subscripts: &[Subscript],
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> ExpressionForm {
    let lookup_name = VarName::new(name);
    if let Some(var) = flat.variables.get(&lookup_name) {
        if subscripts.is_empty() {
            return expression_form_from_dims(&var.dims);
        }
        let Some(remaining_dims) =
            apply_subscripts_to_dims_with_context(&var.dims, subscripts, flat)
        else {
            return ExpressionForm::Other;
        };
        return expression_form_from_dims(&remaining_dims);
    }

    // Embedded subscripts: e.g. "x[1]" or "M[1,2]"
    if let Some(base) = strip_embedded_subscripts(name)
        && let Some(var) = flat.variables.get(&VarName::new(base))
    {
        if has_embedded_range_subscript(name) {
            let size = compute_embedded_range_size(name, &var.dims, flat);
            return if size <= 1 {
                ExpressionForm::Scalar
            } else {
                ExpressionForm::Vector(size)
            };
        }
        let n = count_embedded_subscripts(name);
        if n >= var.dims.len() {
            return ExpressionForm::Scalar;
        }
        return expression_form_from_dims(&var.dims[n..]);
    }

    let fallback_chain = subscript_fallback_chain(name);
    if fallback_chain
        .iter()
        .any(|candidate| flat.variables.contains_key(candidate))
    {
        return ExpressionForm::Scalar;
    }
    for base in fallback_chain {
        if let Some(&total) = prefix_counts.get(base.as_str()) {
            // Record element size via prefix map is not guaranteed to be a vector;
            // keep conservative to avoid false dot-product matches.
            return if total <= 1 {
                ExpressionForm::Scalar
            } else {
                ExpressionForm::Other
            };
        }
    }

    if let Some(&count) = prefix_counts.get(name) {
        return if count <= 1 {
            ExpressionForm::Scalar
        } else {
            ExpressionForm::Other
        };
    }

    ExpressionForm::Other
}

pub(crate) fn combine_additive_forms(lhs: ExpressionForm, rhs: ExpressionForm) -> ExpressionForm {
    match (lhs, rhs) {
        (ExpressionForm::Scalar, ExpressionForm::Scalar) => ExpressionForm::Scalar,
        (ExpressionForm::Vector(n), ExpressionForm::Scalar)
        | (ExpressionForm::Scalar, ExpressionForm::Vector(n)) => ExpressionForm::Vector(n),
        (ExpressionForm::Matrix(r, c), ExpressionForm::Scalar)
        | (ExpressionForm::Scalar, ExpressionForm::Matrix(r, c)) => ExpressionForm::Matrix(r, c),
        (ExpressionForm::Vector(a), ExpressionForm::Vector(b)) if a == b => {
            ExpressionForm::Vector(a)
        }
        (ExpressionForm::Matrix(r1, c1), ExpressionForm::Matrix(r2, c2))
            if r1 == r2 && c1 == c2 =>
        {
            ExpressionForm::Matrix(r1, c1)
        }
        _ => ExpressionForm::Other,
    }
}

pub(crate) fn combine_mul_forms(lhs: ExpressionForm, rhs: ExpressionForm) -> ExpressionForm {
    match (lhs, rhs) {
        (ExpressionForm::Scalar, ExpressionForm::Scalar) => ExpressionForm::Scalar,
        (ExpressionForm::Vector(n), ExpressionForm::Scalar)
        | (ExpressionForm::Scalar, ExpressionForm::Vector(n)) => ExpressionForm::Vector(n),
        (ExpressionForm::Matrix(r, c), ExpressionForm::Scalar)
        | (ExpressionForm::Scalar, ExpressionForm::Matrix(r, c)) => ExpressionForm::Matrix(r, c),
        // Modelica vector * vector is dot-product, yielding a scalar.
        (ExpressionForm::Vector(a), ExpressionForm::Vector(b)) if a == b => ExpressionForm::Scalar,
        // Matrix multiplication follows linear algebra dimensions.
        (ExpressionForm::Vector(v), ExpressionForm::Matrix(r, c)) if v == r => {
            ExpressionForm::Vector(c)
        }
        (ExpressionForm::Matrix(r, c), ExpressionForm::Vector(v)) if c == v => {
            ExpressionForm::Vector(r)
        }
        (ExpressionForm::Matrix(r1, c1), ExpressionForm::Matrix(r2, c2)) if c1 == r2 => {
            ExpressionForm::Matrix(r1, c2)
        }
        _ => ExpressionForm::Other,
    }
}

pub(crate) fn combine_elementwise_forms(
    lhs: ExpressionForm,
    rhs: ExpressionForm,
) -> ExpressionForm {
    match (lhs, rhs) {
        (ExpressionForm::Scalar, ExpressionForm::Scalar) => ExpressionForm::Scalar,
        (ExpressionForm::Vector(n), ExpressionForm::Scalar)
        | (ExpressionForm::Scalar, ExpressionForm::Vector(n)) => ExpressionForm::Vector(n),
        (ExpressionForm::Matrix(r, c), ExpressionForm::Scalar)
        | (ExpressionForm::Scalar, ExpressionForm::Matrix(r, c)) => ExpressionForm::Matrix(r, c),
        (ExpressionForm::Vector(a), ExpressionForm::Vector(b)) if a == b => {
            ExpressionForm::Vector(a)
        }
        (ExpressionForm::Matrix(r1, c1), ExpressionForm::Matrix(r2, c2))
            if r1 == r2 && c1 == c2 =>
        {
            ExpressionForm::Matrix(r1, c1)
        }
        _ => ExpressionForm::Other,
    }
}

pub(crate) fn infer_binary_expression_form(
    op: &rumoca_core::OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    flat: &Model,
    prefix_counts: &ScalarInferenceMetadata,
) -> ExpressionForm {
    let lhs_form = infer_expression_form(lhs, flat, prefix_counts);
    let rhs_form = infer_expression_form(rhs, flat, prefix_counts);
    match op {
        rumoca_core::OpBinary::Add
        | rumoca_core::OpBinary::Sub
        | rumoca_core::OpBinary::AddElem
        | rumoca_core::OpBinary::SubElem => combine_additive_forms(lhs_form, rhs_form),
        rumoca_core::OpBinary::Mul => combine_mul_forms(lhs_form, rhs_form),
        rumoca_core::OpBinary::MulElem => combine_elementwise_forms(lhs_form, rhs_form),
        rumoca_core::OpBinary::Div => match (lhs_form, rhs_form) {
            (ExpressionForm::Scalar, ExpressionForm::Scalar) => ExpressionForm::Scalar,
            (ExpressionForm::Vector(n), ExpressionForm::Scalar) => ExpressionForm::Vector(n),
            (ExpressionForm::Matrix(r, c), ExpressionForm::Scalar) => ExpressionForm::Matrix(r, c),
            _ => ExpressionForm::Other,
        },
        rumoca_core::OpBinary::DivElem | rumoca_core::OpBinary::ExpElem => {
            combine_elementwise_forms(lhs_form, rhs_form)
        }
        rumoca_core::OpBinary::Exp => match (lhs_form, rhs_form) {
            (ExpressionForm::Scalar, ExpressionForm::Scalar) => ExpressionForm::Scalar,
            (ExpressionForm::Vector(n), ExpressionForm::Scalar) => ExpressionForm::Vector(n),
            (ExpressionForm::Matrix(r, c), ExpressionForm::Scalar) => ExpressionForm::Matrix(r, c),
            _ => ExpressionForm::Other,
        },
        // Relational/logical expressions are scalar.
        rumoca_core::OpBinary::Eq
        | rumoca_core::OpBinary::Neq
        | rumoca_core::OpBinary::Lt
        | rumoca_core::OpBinary::Le
        | rumoca_core::OpBinary::Gt
        | rumoca_core::OpBinary::Ge
        | rumoca_core::OpBinary::And
        | rumoca_core::OpBinary::Or
        | rumoca_core::OpBinary::Assign
        | rumoca_core::OpBinary::Empty => ExpressionForm::Scalar,
    }
}

pub(crate) fn infer_builtin_expression_form(
    function: &BuiltinFunction,
    args: &[Expression],
    flat: &Model,
    prefix_counts: &ScalarInferenceMetadata,
) -> ExpressionForm {
    if is_reduction_builtin(function) {
        return ExpressionForm::Scalar;
    }
    if matches!(function, BuiltinFunction::Der | BuiltinFunction::Pre) && args.len() == 1 {
        return infer_expression_form(&args[0], flat, prefix_counts);
    }
    if let Some(size) = extract_builtin_array_size(function, args) {
        return if size <= 1 {
            ExpressionForm::Scalar
        } else {
            ExpressionForm::Vector(size)
        };
    }
    ExpressionForm::Other
}

pub(crate) fn infer_array_expression_form(
    elements: &[Expression],
    is_matrix: bool,
    flat: &Model,
    prefix_counts: &ScalarInferenceMetadata,
) -> ExpressionForm {
    if is_matrix {
        return ExpressionForm::Other;
    }
    if elements.iter().all(|e| {
        matches!(
            infer_expression_form(e, flat, prefix_counts),
            ExpressionForm::Scalar
        )
    }) {
        ExpressionForm::Vector(elements.len())
    } else {
        ExpressionForm::Other
    }
}

pub(crate) fn infer_if_expression_form(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    flat: &Model,
    prefix_counts: &ScalarInferenceMetadata,
) -> ExpressionForm {
    let else_form = infer_expression_form(else_branch, flat, prefix_counts);
    if branches
        .iter()
        .all(|(_, expr)| infer_expression_form(expr, flat, prefix_counts) == else_form)
    {
        else_form
    } else {
        ExpressionForm::Other
    }
}

pub(crate) fn infer_expression_form(
    expr: &Expression,
    flat: &Model,
    prefix_counts: &ScalarInferenceMetadata,
) -> ExpressionForm {
    match expr {
        Expression::Literal { value: _, .. } => ExpressionForm::Scalar,
        Expression::VarRef {
            name, subscripts, ..
        } => infer_varref_form(name.as_str(), subscripts, flat, prefix_counts),
        Expression::Unary { rhs, .. } => infer_expression_form(rhs, flat, prefix_counts),
        Expression::Binary { op, lhs, rhs, .. } => {
            infer_binary_expression_form(op, lhs, rhs, flat, prefix_counts)
        }
        Expression::BuiltinCall { function, args, .. } => {
            infer_builtin_expression_form(function, args, flat, prefix_counts)
        }
        Expression::FunctionCall { name, .. } => {
            if let Some(dims) = infer_function_output_dims(name.as_str(), flat) {
                return expression_form_from_dims(&dims);
            }
            ExpressionForm::Other
        }
        Expression::Array {
            elements,
            is_matrix,
            ..
        } => infer_array_expression_form(elements, *is_matrix, flat, prefix_counts),
        Expression::If {
            branches,
            else_branch,
            ..
        } => infer_if_expression_form(branches, else_branch, flat, prefix_counts),
        Expression::Index {
            base, subscripts, ..
        } => {
            let base_form = infer_expression_form(base, flat, prefix_counts);
            match (base_form, subscripts.is_empty()) {
                (ExpressionForm::Vector(_), false) => ExpressionForm::Scalar,
                _ => ExpressionForm::Other,
            }
        }
        Expression::FieldAccess { base, field, .. } => {
            infer_projected_result_field_form(base, field, flat, prefix_counts)
        }
        Expression::Tuple { .. }
        | Expression::Range { .. }
        | Expression::ArrayComprehension { .. }
        | Expression::Empty { .. } => ExpressionForm::Other,
    }
}

fn infer_projected_result_field_form(
    base: &Expression,
    field: &str,
    flat: &Model,
    metadata: &ScalarInferenceMetadata,
) -> ExpressionForm {
    let Expression::FunctionCall { name, .. } = base else {
        return ExpressionForm::Other;
    };
    let Some(dims) = metadata.projected_result_field_dims(name, field, flat) else {
        return ExpressionForm::Other;
    };
    match dims {
        [] => ExpressionForm::Scalar,
        [dimension] => ExpressionForm::Vector(*dimension as usize),
        [rows, columns] => ExpressionForm::Matrix(*rows as usize, *columns as usize),
        _ => ExpressionForm::Vector(compute_var_size(dims)),
    }
}

pub(crate) fn infer_equation_scalar_count_from_forms(
    residual: &Expression,
    flat: &Model,
    prefix_counts: &ScalarInferenceMetadata,
) -> Option<usize> {
    let Expression::Binary { op, lhs, rhs, .. } = residual else {
        return None;
    };
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }

    let lhs_form = infer_expression_form(lhs, flat, prefix_counts);
    let rhs_form = infer_expression_form(rhs, flat, prefix_counts);
    match (lhs_form, rhs_form) {
        (ExpressionForm::Scalar, ExpressionForm::Scalar) => Some(1),
        (ExpressionForm::Vector(n), ExpressionForm::Scalar)
        | (ExpressionForm::Scalar, ExpressionForm::Vector(n)) => Some(n),
        (ExpressionForm::Matrix(r, c), ExpressionForm::Scalar)
        | (ExpressionForm::Scalar, ExpressionForm::Matrix(r, c)) => Some(r.saturating_mul(c)),
        (ExpressionForm::Vector(a), ExpressionForm::Vector(b)) if a == b => Some(a),
        (ExpressionForm::Matrix(r1, c1), ExpressionForm::Matrix(r2, c2))
            if r1 == r2 && c1 == c2 =>
        {
            Some(r1.saturating_mul(c1))
        }
        _ => None,
    }
}

/// Infer the scalar count of an equation by analyzing its residual.
///
/// For equations of the form `var - expr = 0` where `var` is an array,
/// the equation represents `size(var)` scalar equations (MLS §8.4).
///
/// This handles the common case where the residual is:
/// `Binary { op: Sub, lhs: VarRef { name }, rhs: ... }`
pub(crate) fn infer_equation_scalar_count(
    residual: &Expression,
    flat: &Model,
    prefix_counts: &ScalarInferenceMetadata,
) -> usize {
    // First try extracting size from a simple LHS pattern (var - expr = 0)
    if let Some(size) = extract_lhs_var_size(residual, flat, prefix_counts) {
        return size;
    }

    // If LHS is a function call and we couldn't resolve its output size,
    // prefer RHS-based inference. This avoids inflating scalar count from
    // record-typed function arguments on the LHS.
    if let Expression::Binary { op, lhs, rhs, .. } = residual
        && matches!(op, rumoca_core::OpBinary::Sub)
        && matches!(lhs.as_ref(), Expression::FunctionCall { .. })
        && let Some(size) =
            infer_scalar_count_from_varrefs_skip_function_args(rhs, flat, prefix_counts)
    {
        return size;
    }

    // Infer scalar count from expression form when no direct LHS variable size
    // is available. This captures residuals like `0 = v1*v2` where v1,v2 are vectors:
    // dot product is scalar (1), not vector size.
    //
    // Combine with VarRef-based inference conservatively by taking the minimum when
    // both are available. This avoids inflating counts in models that rely on the
    // established VarRef heuristic while still correcting dot-product overcounts.
    let form_size = infer_equation_scalar_count_from_forms(residual, flat, prefix_counts);
    let varref_size = infer_scalar_count_from_varrefs(residual, flat, prefix_counts);
    match (form_size, varref_size) {
        (Some(1), Some(varref))
            if varref > 1 && has_top_level_record_constructor(residual, flat) =>
        {
            varref
        }
        (Some(a), Some(b)) => a.min(b),
        (Some(a), None) => a,
        (None, Some(b)) => b,
        (None, None) => 1,
    }
}

fn has_top_level_record_constructor(residual: &Expression, flat: &Model) -> bool {
    let Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = residual
    else {
        return false;
    };
    expression_is_record_constructor(lhs, flat) || expression_is_record_constructor(rhs, flat)
}

fn expression_is_record_constructor(expr: &Expression, flat: &Model) -> bool {
    let Expression::FunctionCall {
        name,
        is_constructor,
        ..
    } = expr
    else {
        return false;
    };
    *is_constructor
        || flat
            .functions
            .get(name.var_name())
            .is_some_and(|function| function.is_constructor && function.inputs.len() > 1)
}

/// Infer scalar count by finding VarRefs in an expression and checking for record expansion.
///
/// Per MLS §10.2, equations involving record types represent multiple scalar equations,
/// one for each component of the record. For example, an equation with Complex variables
/// represents 2 scalar equations (one for .re, one for .im).
///
/// VarRefs inside array reduction builtins (sum, product, etc.) are excluded because
/// those functions reduce arrays to scalars (MLS §10.3.4).
pub(crate) fn infer_scalar_count_from_varrefs(
    expr: &Expression,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Option<usize> {
    let mut var_refs = Vec::new();
    collect_var_refs_skip_reductions(expr, &mut var_refs);

    infer_scalar_count_from_collected_varrefs(&var_refs, flat, prefix_counts)
}

/// Infer scalar count from VarRefs while skipping arguments to function calls.
///
/// Used for equations with function-call LHS where RHS function-call arguments
/// should not inflate scalar count (e.g., record-typed arguments in MultiBody).
pub(crate) fn infer_scalar_count_from_varrefs_skip_function_args(
    expr: &Expression,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Option<usize> {
    let mut var_refs = Vec::new();
    collect_var_refs_skip_reductions_and_function_args(expr, &mut var_refs);

    infer_scalar_count_from_collected_varrefs(&var_refs, flat, prefix_counts)
}

#[derive(Debug, Clone)]
pub(crate) struct CollectedVarRef {
    pub(crate) name: VarName,
    pub(crate) subscripts: Vec<Subscript>,
}

/// Infer scalar count for flow-sum equations from VarRef shapes.
///
/// Connection-set flow sums can appear as mixed scalar/array expressions after flattening.
/// If exactly one array flow term is present together with scalar terms, this represents
/// a scalar Kirchhoff sum over that array and should count as one scalar equation.
/// Otherwise, preserve array-sized scalarization.
pub(crate) fn infer_flow_sum_scalar_count(
    residual: &Expression,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Option<usize> {
    let mut var_refs = Vec::new();
    collect_var_refs_skip_reductions(residual, &mut var_refs);
    if var_refs.is_empty() {
        return None;
    }

    let mut seen: HashSet<String> = HashSet::default();
    let mut array_term_count = 0usize;
    let mut has_scalar_term = false;
    let mut max_array_size = 0usize;

    for var_ref in &var_refs {
        let key = format!("{}{:?}", var_ref.name.as_str(), var_ref.subscripts);
        if !seen.insert(key) {
            continue;
        }

        let size = match infer_varref_form(
            var_ref.name.as_str(),
            &var_ref.subscripts,
            flat,
            prefix_counts,
        ) {
            ExpressionForm::Scalar => 1,
            ExpressionForm::Vector(n) => n,
            ExpressionForm::Matrix(r, c) => r.saturating_mul(c),
            ExpressionForm::Other => 0,
        };

        if size > 1 {
            array_term_count += 1;
            max_array_size = max_array_size.max(size);
        } else if size == 1 {
            has_scalar_term = true;
        }
    }

    if array_term_count == 1 && has_scalar_term {
        Some(1)
    } else if max_array_size > 0 {
        Some(max_array_size)
    } else {
        Some(1)
    }
}

mod inference_and_bindings;
mod parts;

pub(crate) use inference_and_bindings::*;
pub(crate) use parts::*;
