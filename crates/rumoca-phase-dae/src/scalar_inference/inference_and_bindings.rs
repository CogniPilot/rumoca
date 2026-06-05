use super::*;
use rumoca_core::ExpressionRewriter;

/// Infer output dimensions for single-output functions.
///
/// Multi-output functions are represented as tuples and are not treated as a
/// simple expression shape here.
pub(crate) fn flat_function_output_dims(func: &rumoca_core::Function) -> Option<Vec<i64>> {
    let output = func.outputs.first()?;
    if func.outputs.len() == 1 {
        Some(output.dims.clone())
    } else {
        None
    }
}

/// Compute total scalar size of a function's outputs.
pub(crate) fn flat_function_output_scalar_size(func: &rumoca_core::Function) -> usize {
    if func.outputs.is_empty() {
        if func.is_constructor && !func.inputs.is_empty() {
            return func
                .inputs
                .iter()
                .map(|input| compute_var_size(&input.dims).max(1))
                .sum::<usize>()
                .max(1);
        }
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
        Expression::VarRef {
            name, subscripts, ..
        } => !subscripts.is_empty() || rumoca_core::has_top_level_subscript(name.as_str()),
        _ => false,
    }
}

/// Extract an integer value from a flat expression literal.
pub(crate) fn extract_integer_from_flat_expr(expr: &Expression) -> Option<usize> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(n),
            ..
        } => usize::try_from(*n).ok(),
        Expression::Literal {
            value: Literal::Real(f),
            ..
        } => {
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
                push_prefix_child(&mut children, &s[..i], name);
            }
        }
    }
    children
}

fn push_prefix_child(children: &mut FxHashMap<String, Vec<VarName>>, prefix: &str, name: &VarName) {
    if let Some(existing) = children.get_mut(prefix) {
        existing.push(name.clone());
        return;
    }
    children.insert(prefix.to_string(), vec![name.clone()]);
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

    // Non-primitive records may exist as a flat prefix with scalarized child
    // variables. MLS record equations count by their primitive components, not
    // by the record prefix's own empty dimensions.
    if let Some(var) = flat.variables.get(&var_name) {
        if !var.is_primitive
            && let Some(&count) = prefix_counts.get(var_name.as_str())
            && count > 0
        {
            return Some(count);
        }
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
    for base in subscript_fallback_chain(var_name.as_str()) {
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
        ..
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
    if !matches!(op, rumoca_core::OpBinary::Sub) {
        return None;
    }

    // Handle tuple LHS: (a, b, ...) = rhs
    // MLS §8.4: tuple equations count as the sum of scalar sizes of all elements.
    // For (b, a) where b is Real[2] and a is Real[2], that's 2+2=4 scalar equations.
    // Skip discrete variables — they don't contribute to the continuous balance.
    if let Expression::Tuple { elements, .. } = lhs.as_ref() {
        let continuous_scalar_count: usize = elements
            .iter()
            .filter(|e| {
                let Expression::VarRef { name, .. } = e else {
                    return true;
                };
                !flat
                    .variables
                    .get(name.var_name())
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
    if let Expression::BuiltinCall { function, args, .. } = lhs.as_ref()
        && let Some(size) = extract_builtin_array_size(function, args)
    {
        return Some(size);
    }

    if let Expression::Index {
        base, subscripts, ..
    } = lhs.as_ref()
        && let Some(size) = indexed_lhs_scalar_size(base, subscripts, flat, prefix_counts)
    {
        return Some(size);
    }

    // Handle function-call LHS by using function output dimensions.
    // Example: angularVelocity2(R_b) = w_rel_b should count as 3 scalars,
    // not by the size of record-typed argument R_b (12 scalars).
    if let Expression::FunctionCall { name, .. } = lhs.as_ref()
        && let Some(size) = infer_function_output_scalar_size(name.as_str(), flat)
    {
        return Some(size);
    }

    // Handle subscripted VarRef: var[i] where var is a multi-dimensional array.
    if let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    {
        // Explicit subscripts in the subscripts field
        if !subscripts.is_empty()
            && let Some(var) = flat.variables.get(name.var_name())
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

fn indexed_lhs_scalar_size(
    base: &Expression,
    subscripts: &[Subscript],
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Option<usize> {
    let base_name = extract_var_from_lhs(base)?;
    if let Some(var) = flat.variables.get(&base_name) {
        return Some(compute_subscripted_size(&var.dims, subscripts));
    }

    let total = *prefix_counts.get(base_name.as_str())?;
    let full_name = format!(
        "{}{}",
        base_name.as_str(),
        render_subscript_suffix(subscripts)?
    );
    Some(record_subscript_scalar_size(
        &full_name,
        base_name.as_str(),
        total,
        flat,
    ))
}

fn render_subscript_suffix(subscripts: &[Subscript]) -> Option<String> {
    let mut out = String::new();
    for subscript in subscripts {
        match subscript {
            Subscript::Index { value, .. } => out.push_str(&format!("[{value}]")),
            Subscript::Expr { expr, .. } => {
                let Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } = expr.as_ref()
                else {
                    return None;
                };
                out.push_str(&format!("[{value}]"));
            }
            Subscript::Colon { .. } => out.push_str("[:]"),
        }
    }
    Some(out)
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
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.var_name().clone()),
        // der(x) - extract the variable from inside the derivative
        Expression::BuiltinCall { function, args, .. }
            if matches!(function, rumoca_core::BuiltinFunction::Der) && args.len() == 1 =>
        {
            if let Expression::VarRef {
                name, subscripts, ..
            } = &args[0]
                && subscripts.is_empty()
            {
                return Some(name.var_name().clone());
            }
            None
        }
        // Array containing a single variable reference: [y]
        // This pattern appears in equations like `[y] = [[u1], [u2], ...]`
        Expression::Array { elements, .. } if elements.len() == 1 => {
            if let Expression::VarRef {
                name, subscripts, ..
            } = &elements[0]
                && subscripts.is_empty()
            {
                return Some(name.var_name().clone());
            }
            None
        }
        // Unary minus wrapping a variable reference: -y
        // This pattern appears in equations like `-spacePhasor.i_ = TransformationMatrix * i`
        Expression::Unary { rhs, .. } => {
            if let Expression::VarRef {
                name, subscripts, ..
            } = rhs.as_ref()
                && subscripts.is_empty()
            {
                return Some(name.var_name().clone());
            }
            None
        }
        _ => None,
    }
}

/// Create a Variable from a flat::Variable.
pub(crate) fn resolve_missing_start_ref(
    name: &VarName,
    reference_index: &DaeReferenceIndex,
) -> VarName {
    if let Some(resolved) =
        crate::path_utils::resolve_known_path_suffix(name.as_str(), reference_index.known_names())
    {
        return VarName::new(resolved);
    }
    name.clone()
}

pub(crate) fn rewrite_start_expr_missing_refs(
    expr: &Expression,
    reference_index: &DaeReferenceIndex,
) -> Expression {
    StartRefRewriter { reference_index }.rewrite_expression(expr)
}

struct StartRefRewriter<'a> {
    reference_index: &'a DaeReferenceIndex,
}

impl ExpressionRewriter for StartRefRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        let Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        Expression::VarRef {
            name: resolve_missing_start_ref(name.var_name(), self.reference_index).into(),
            subscripts: self.rewrite_subscripts(subscripts),
            span: *span,
        }
    }
}

/// Post-flattening DAE reference membership index.
///
/// This is not a Modelica scope: name lookup and declaration identity have
/// already been resolved before Flat. It only prevents DAE scalar projection
/// helpers from treating scalar variables and enum literals as aggregate aliases.
pub(crate) struct DaeReferenceIndex {
    known_var_names: HashSet<String>,
    scalar_value_names: HashSet<String>,
}

impl DaeReferenceIndex {
    pub(crate) fn from_flat(flat: &flat::Model) -> Self {
        let primitive_var_names: HashSet<String> = flat
            .variables
            .iter()
            .filter(|(_, var)| var.is_primitive)
            .map(|(name, _)| name.as_str().to_string())
            .collect();
        let scalar_value_names = primitive_var_names
            .iter()
            .cloned()
            .chain(flat.enum_literal_ordinals.keys().cloned())
            .collect();
        Self {
            known_var_names: flat
                .variables
                .keys()
                .map(|name| name.as_str().to_string())
                .collect(),
            scalar_value_names,
        }
    }

    pub(crate) fn known_names(&self) -> &HashSet<String> {
        &self.known_var_names
    }

    pub(crate) fn is_known_scalar_value(&self, name: &VarName) -> bool {
        self.scalar_value_names.contains(name.as_str())
    }
}

pub(crate) fn create_dae_variable(
    name: &VarName,
    var: &flat::Variable,
    reference_index: &DaeReferenceIndex,
) -> Variable {
    // MLS §4.4.1: declaration/modification bindings define parameter/constant values.
    // Start remains an initialization attribute and should not be used as the
    // primary value when a binding exists.
    let start_source = match var.variability {
        Variability::Parameter(_) | Variability::Constant(_) => {
            var.binding.as_ref().or(var.start.as_ref())
        }
        _ => var.start.as_ref(),
    };
    let start_span = start_source.and_then(Expression::span);
    let start = start_source
        .map(|expr| select_scalar_start_record_alias(name, expr, reference_index))
        .map(|expr| rewrite_start_expr_missing_refs(&expr, reference_index))
        .map(|expr| flat_to_dae_expression(&expr));

    // A parameter is tunable (changeable at runtime in FMI 3.0 ConfigurationMode)
    // unless it is structural: evaluate=true or discrete-typed (Integer/Boolean
    // used for array sizing).
    let is_tunable = matches!(var.variability, Variability::Parameter(_))
        && !var.evaluate
        && !var.is_discrete_type;

    Variable {
        name: flat_to_dae_var_name(name),
        component_ref: var.component_ref.clone(),
        source_span: var.source_span,
        dims: var.dims.clone(),
        start,
        start_span,
        fixed: var.fixed,
        min: var.min.as_ref().map(flat_to_dae_expression),
        min_span: var.min.as_ref().and_then(Expression::span),
        max: var.max.as_ref().map(flat_to_dae_expression),
        max_span: var.max.as_ref().and_then(Expression::span),
        nominal: var.nominal.as_ref().map(flat_to_dae_expression),
        nominal_span: var.nominal.as_ref().and_then(Expression::span),
        unit: var.unit.clone(),
        state_select: var.state_select,
        description: None,
        causality: variable_causality(var, is_tunable),
        is_tunable,
    }
}

fn variable_causality(var: &flat::Variable, is_tunable: bool) -> rumoca_ir_dae::VariableCausality {
    match &var.causality {
        rumoca_core::Causality::Input(_) => rumoca_ir_dae::VariableCausality::Input,
        rumoca_core::Causality::Output(_) => rumoca_ir_dae::VariableCausality::Output,
        rumoca_core::Causality::Empty => match var.variability {
            Variability::Parameter(_) if is_tunable => rumoca_ir_dae::VariableCausality::Parameter,
            Variability::Parameter(_) | Variability::Constant(_) => {
                rumoca_ir_dae::VariableCausality::CalculatedParameter
            }
            Variability::Empty | Variability::Continuous(_) | Variability::Discrete(_) => {
                rumoca_ir_dae::VariableCausality::Local
            }
        },
    }
}

fn select_scalar_start_record_alias(
    lhs_name: &VarName,
    expr: &Expression,
    reference_index: &DaeReferenceIndex,
) -> Expression {
    let lhs_path = rumoca_core::ComponentPath::from_flat_path(lhs_name.as_str());
    let leaf_field = lhs_path.parts().last();
    let Some(lhs_base) = flat::component_base_name(lhs_name.as_str()) else {
        return select_leaf_start_record_alias(expr, leaf_field, reference_index)
            .unwrap_or_else(|| expr.clone());
    };
    if lhs_base == lhs_name.as_str() {
        return select_leaf_start_record_alias(expr, leaf_field, reference_index)
            .unwrap_or_else(|| expr.clone());
    }
    let Some(field_suffix) = lhs_name.as_str().strip_prefix(lhs_base.as_str()) else {
        return select_leaf_start_record_alias(expr, leaf_field, reference_index)
            .unwrap_or_else(|| expr.clone());
    };
    if !field_suffix.starts_with('.') {
        return select_leaf_start_record_alias(expr, leaf_field, reference_index)
            .unwrap_or_else(|| expr.clone());
    }

    match expr {
        Expression::VarRef {
            name: rhs_name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            if reference_index.is_known_scalar_value(rhs_name.var_name()) {
                return expr.clone();
            }
            let selected = format!("{}{}", rhs_name.as_str(), field_suffix);
            if let Some(selected) = crate::path_utils::resolve_known_path_suffix(
                &selected,
                reference_index.known_names(),
            ) {
                return Expression::VarRef {
                    name: VarName::new(selected).into(),
                    subscripts: vec![],
                    span: *span,
                };
            }
            if let Some(field) = leaf_field {
                let selected = format!("{}.{}", rhs_name.as_str(), field);
                if let Some(selected) = crate::path_utils::resolve_known_path_suffix(
                    &selected,
                    reference_index.known_names(),
                ) {
                    return Expression::VarRef {
                        name: VarName::new(selected).into(),
                        subscripts: vec![],
                        span: *span,
                    };
                }
            }
            expr.clone()
        }
        Expression::FieldAccess { base, field, span } => {
            let Expression::VarRef {
                name: rhs_name,
                subscripts,
                ..
            } = base.as_ref()
            else {
                return expr.clone();
            };
            if !subscripts.is_empty() {
                return expr.clone();
            }
            let selected = format!("{}.{}{}", rhs_name.as_str(), field, field_suffix);
            if let Some(selected) = crate::path_utils::resolve_known_path_suffix(
                &selected,
                reference_index.known_names(),
            ) {
                return Expression::VarRef {
                    name: VarName::new(selected).into(),
                    subscripts: vec![],
                    span: *span,
                };
            }
            if let Some(lhs_leaf) = leaf_field {
                let selected = format!("{}.{}.{}", rhs_name.as_str(), field, lhs_leaf);
                if let Some(selected) = crate::path_utils::resolve_known_path_suffix(
                    &selected,
                    reference_index.known_names(),
                ) {
                    return Expression::VarRef {
                        name: VarName::new(selected).into(),
                        subscripts: vec![],
                        span: *span,
                    };
                }
            }
            expr.clone()
        }
        _ => expr.clone(),
    }
}

fn select_leaf_start_record_alias(
    expr: &Expression,
    leaf_field: Option<&String>,
    reference_index: &DaeReferenceIndex,
) -> Option<Expression> {
    let field = leaf_field?;
    match expr {
        Expression::VarRef {
            name: rhs_name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            if reference_index.is_known_scalar_value(rhs_name.var_name()) {
                return None;
            }
            let selected = format!("{}.{}", rhs_name.as_str(), field);
            crate::path_utils::resolve_known_path_suffix(&selected, reference_index.known_names())
                .map(|selected| Expression::VarRef {
                    name: VarName::new(selected).into(),
                    subscripts: vec![],
                    span: *span,
                })
        }
        Expression::FieldAccess { base, field, span } => {
            let Expression::VarRef {
                name: rhs_name,
                subscripts,
                ..
            } = base.as_ref()
            else {
                return None;
            };
            if !subscripts.is_empty() {
                return None;
            }
            let selected = format!("{}.{}", rhs_name.as_str(), field);
            crate::path_utils::resolve_known_path_suffix(&selected, reference_index.known_names())
                .map(|selected| Expression::VarRef {
                    name: VarName::new(selected).into(),
                    subscripts: vec![],
                    span: *span,
                })
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn reference_index(names: &[&VarName]) -> DaeReferenceIndex {
        let known_var_names = names.iter().map(|name| name.as_str().to_string()).collect();
        DaeReferenceIndex {
            known_var_names,
            scalar_value_names: names.iter().map(|name| name.as_str().to_string()).collect(),
        }
    }

    #[test]
    fn create_dae_variable_preserves_component_reference() {
        let component_def_id = rumoca_core::DefId::new(17);
        let name = VarName::new("x");
        let flat_var = flat::Variable {
            name: name.clone(),
            component_ref: Some(rumoca_core::ComponentReference {
                local: false,
                span: rumoca_core::Span::DUMMY,
                parts: vec![rumoca_core::ComponentRefPart {
                    ident: "x".to_string(),
                    span: rumoca_core::Span::DUMMY,
                    subs: vec![],
                }],
                def_id: Some(component_def_id),
            }),
            is_primitive: true,
            ..Default::default()
        };
        let reference_index = reference_index(&[&name]);

        let dae_var = create_dae_variable(&name, &flat_var, &reference_index);

        assert_eq!(
            dae_var
                .component_ref
                .as_ref()
                .and_then(|reference| reference.def_id),
            Some(component_def_id)
        );
    }

    #[test]
    fn create_dae_variable_surfaces_source_causality() {
        let name = VarName::new("u");
        let flat_var = flat::Variable {
            name: name.clone(),
            causality: rumoca_core::Causality::Input(Default::default()),
            is_primitive: true,
            ..Default::default()
        };
        let reference_index = reference_index(&[&name]);

        let dae_var = create_dae_variable(&name, &flat_var, &reference_index);

        assert_eq!(dae_var.causality, rumoca_ir_dae::VariableCausality::Input);
    }

    #[test]
    fn create_dae_variable_marks_parameter_causality() {
        let name = VarName::new("p");
        let flat_var = flat::Variable {
            name: name.clone(),
            variability: Variability::Parameter(Default::default()),
            is_primitive: true,
            ..Default::default()
        };
        let reference_index = reference_index(&[&name]);

        let dae_var = create_dae_variable(&name, &flat_var, &reference_index);

        assert_eq!(
            dae_var.causality,
            rumoca_ir_dae::VariableCausality::Parameter
        );
    }
}
