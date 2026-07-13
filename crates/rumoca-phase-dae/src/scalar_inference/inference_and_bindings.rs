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
    if func.is_constructor && !func.inputs.is_empty() {
        return func
            .inputs
            .iter()
            .map(|input| compute_var_size(&input.dims).max(1))
            .sum::<usize>()
            .max(1);
    }
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
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct ProjectedResultFieldKey {
    function: rumoca_core::DefId,
    field: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ProjectedResultFieldShape {
    Known(Vec<i64>),
    Unknown,
}

pub(crate) struct ScalarInferenceMetadata {
    prefix_counts: FxHashMap<String, usize>,
    projected_result_fields: FxHashMap<ProjectedResultFieldKey, ProjectedResultFieldShape>,
}

impl std::ops::Deref for ScalarInferenceMetadata {
    type Target = FxHashMap<String, usize>;

    fn deref(&self) -> &Self::Target {
        &self.prefix_counts
    }
}

impl ScalarInferenceMetadata {
    pub(crate) fn projected_result_field_dims(
        &self,
        function: &rumoca_core::Reference,
        field: &str,
        flat: &Model,
    ) -> Option<&[i64]> {
        let function = resolved_function_def_id(function, flat)?;
        let key = ProjectedResultFieldKey {
            function,
            field: field.to_string(),
        };
        match self.projected_result_fields.get(&key)? {
            ProjectedResultFieldShape::Known(dims) => Some(dims),
            ProjectedResultFieldShape::Unknown => None,
        }
    }
}

pub(crate) fn build_prefix_counts(flat: &Model) -> ScalarInferenceMetadata {
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
    ScalarInferenceMetadata {
        prefix_counts: counts,
        projected_result_fields: build_projected_result_field_shapes(flat),
    }
}

fn build_projected_result_field_shapes(
    flat: &Model,
) -> FxHashMap<ProjectedResultFieldKey, ProjectedResultFieldShape> {
    let mut shapes = FxHashMap::default();
    collect_constructor_result_field_shapes(flat, &mut shapes);
    for variable in flat.variables.values() {
        let Some(Expression::FieldAccess { base, field, .. }) = variable.binding.as_ref() else {
            continue;
        };
        let Expression::FunctionCall { name, .. } = base.as_ref() else {
            continue;
        };
        let Some(function) = resolved_function_def_id(name, flat) else {
            continue;
        };
        merge_projected_result_field_shape(
            &mut shapes,
            ProjectedResultFieldKey {
                function,
                field: field.clone(),
            },
            concrete_result_field_dims(&variable.dims),
        );
    }
    shapes
}

fn collect_constructor_result_field_shapes(
    flat: &Model,
    shapes: &mut FxHashMap<ProjectedResultFieldKey, ProjectedResultFieldShape>,
) {
    for function in flat.functions.values() {
        let (Some(function_def_id), [output]) = (function.def_id, function.outputs.as_slice())
        else {
            continue;
        };
        if output.type_class != Some(rumoca_core::ClassType::Record) {
            continue;
        }
        let Some(fields) = crate::dae_lowering::record_constructor_fields_from_metadata(
            flat.functions.iter(),
            &output.type_name,
        ) else {
            continue;
        };
        for field in fields {
            let dims = concrete_constructor_result_field_dims(&field);
            merge_projected_result_field_shape(
                shapes,
                ProjectedResultFieldKey {
                    function: function_def_id,
                    field: field.name,
                },
                dims,
            );
        }
    }
}

fn resolved_function_def_id(
    reference: &rumoca_core::Reference,
    flat: &Model,
) -> Option<rumoca_core::DefId> {
    if let Some(referenced) = reference
        .component_ref()
        .and_then(|component_ref| component_ref.def_id)
    {
        return flat
            .functions
            .values()
            .any(|function| function.def_id == Some(referenced))
            .then_some(referenced);
    }
    resolve_flat_function(reference.as_str(), flat)?.def_id
}

fn concrete_result_field_dims(dims: &[i64]) -> Option<Vec<i64>> {
    dims.iter()
        .all(|dimension| *dimension >= 0)
        .then(|| dims.to_vec())
}

fn concrete_constructor_result_field_dims(field: &rumoca_core::FunctionParam) -> Option<Vec<i64>> {
    field
        .shape_expr
        .is_empty()
        .then(|| concrete_result_field_dims(&field.dims))
        .flatten()
}

fn merge_projected_result_field_shape(
    shapes: &mut FxHashMap<ProjectedResultFieldKey, ProjectedResultFieldShape>,
    key: ProjectedResultFieldKey,
    dims: Option<Vec<i64>>,
) {
    let candidate = dims.map_or(
        ProjectedResultFieldShape::Unknown,
        ProjectedResultFieldShape::Known,
    );
    match shapes.entry(key) {
        std::collections::hash_map::Entry::Vacant(entry) => {
            entry.insert(candidate);
        }
        std::collections::hash_map::Entry::Occupied(mut entry) => {
            if entry.get() != &candidate {
                entry.insert(ProjectedResultFieldShape::Unknown);
            }
        }
    }
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
    if let Expression::VarRef {
        name, subscripts, ..
    } = lhs
        && !subscripts.is_empty()
        && let Some(var) = flat.variables.get(name.var_name())
    {
        return compute_subscripted_size_with_context(&var.dims, subscripts, flat);
    }

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

    if let Some(size) = resolve_singleton_indexed_lhs_path_size(&var_name, flat, prefix_counts) {
        return Some(size);
    }

    if let Some(size) = resolve_repeated_indexed_component_path_size(&var_name, flat, prefix_counts)
    {
        return Some(size);
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

    if matches!(lhs.as_ref(), Expression::FieldAccess { .. })
        && let Some(var_name) = render_lhs_path(lhs)
        && let Some(span) = lhs.span()
        && let Some(size) = extract_lhs_var_size_from_var_name(
            &Expression::VarRef {
                name: var_name.into(),
                subscripts: vec![],
                span,
            },
            flat,
            prefix_counts,
        )
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
            && let Some(size) = compute_subscripted_size_with_context(&var.dims, subscripts, flat)
        {
            return Some(size);
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
        return compute_subscripted_size_with_context(&var.dims, subscripts, flat);
    }

    let total = *prefix_counts.get(base_name.as_str())?;
    let full_name = if let Some(suffix) = render_subscript_suffix(subscripts) {
        format!("{}{}", base_name.as_str(), suffix)
    } else if subscripts.is_empty() {
        return None;
    } else {
        // Dynamic indexing into a scalarized record array still selects one
        // array element. The exact index is runtime-valued, but every element
        // has the same scalar record width in flat variables.
        format!("{}[1]", base_name.as_str())
    };
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

fn render_lhs_path(lhs: &Expression) -> Option<VarName> {
    match lhs {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            let mut out = name.as_str().to_string();
            out.push_str(&render_subscript_suffix(subscripts)?);
            Some(VarName::new(out))
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            let mut out = render_lhs_path(base)?.as_str().to_string();
            out.push_str(&render_subscript_suffix(subscripts)?);
            Some(VarName::new(out))
        }
        Expression::FieldAccess { base, field, .. } => {
            let base = render_lhs_path(base)?;
            Some(VarName::new(format!("{}.{}", base.as_str(), field)))
        }
        _ => None,
    }
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

fn resolve_singleton_indexed_lhs_path_size(
    var_name: &VarName,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Option<usize> {
    for candidate in singleton_indexed_path_candidates(var_name.as_str()) {
        let candidate_name = VarName::new(candidate.clone());
        if let Some(var) = flat.variables.get(&candidate_name) {
            return Some(compute_var_size(&var.dims));
        }
        if let Some(&count) = prefix_counts.get(candidate.as_str()) {
            return Some(count);
        }
    }
    None
}

fn resolve_repeated_indexed_component_path_size(
    var_name: &VarName,
    flat: &Model,
    prefix_counts: &FxHashMap<String, usize>,
) -> Option<usize> {
    for candidate in
        crate::path_utils::repeated_indexed_component_path_candidates(var_name.as_str())
    {
        let candidate_name = VarName::new(candidate.clone());
        if let Some(var) = flat.variables.get(&candidate_name) {
            return Some(compute_var_size(&var.dims));
        }
        if let Some(&count) = prefix_counts.get(candidate.as_str()) {
            return Some(count);
        }
    }
    None
}

fn singleton_indexed_path_candidates(path: &str) -> Vec<String> {
    let parts = split_lhs_path_parts(path);
    let mut candidates = Vec::new();
    for index in 0..parts.len() {
        if parts[index].contains('[') {
            continue;
        }
        let mut candidate = parts.clone();
        candidate[index] = format!("{}[1]", candidate[index]);
        candidates.push(candidate.join("."));
    }
    candidates
}

fn split_lhs_path_parts(path: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut depth = 0i32;
    let mut start = 0usize;
    for (index, ch) in path.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => depth -= 1,
            '.' if depth == 0 => {
                parts.push(path[start..index].to_string());
                start = index + 1;
            }
            _ => {}
        }
    }
    parts.push(path[start..].to_string());
    parts
}

/// Create a Variable from a flat::Variable.
pub(crate) fn resolve_missing_start_ref(
    name: &VarName,
    known_var_names: &HashSet<String>,
) -> VarName {
    if let Some(resolved) =
        crate::path_utils::resolve_known_path_suffix(name.as_str(), known_var_names)
    {
        return VarName::new(resolved);
    }
    name.clone()
}

pub(crate) fn rewrite_start_expr_missing_refs(
    expr: &Expression,
    known_var_names: &HashSet<String>,
) -> Expression {
    StartRefRewriter { known_var_names }.rewrite_expression(expr)
}

struct StartRefRewriter<'a> {
    known_var_names: &'a HashSet<String>,
}

impl ExpressionRewriter for StartRefRewriter<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> Expression {
        let resolved_name = if name.has_structure() {
            if !self.known_var_names.contains(name.as_str())
                && let Some(resolved) = crate::path_utils::resolve_known_path_suffix(
                    name.as_str(),
                    self.known_var_names,
                )
            {
                VarName::new(resolved).into()
            } else {
                name.clone()
            }
        } else if let Some(resolved) =
            crate::path_utils::resolve_known_path_suffix(name.as_str(), self.known_var_names)
        {
            VarName::new(resolved).into()
        } else {
            resolve_missing_start_ref(name.var_name(), self.known_var_names).into()
        };
        Expression::VarRef {
            name: resolved_name,
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

pub(crate) fn create_dae_variable(
    name: &VarName,
    var: &flat::Variable,
    known_var_names: &HashSet<String>,
) -> Result<Variable, ToDaeError> {
    // MLS §4.4.1: declaration/modification bindings define parameter/constant values.
    // Start remains an initialization attribute and should not be used as the
    // primary value when a binding exists.
    let start_source = match var.variability {
        Variability::Parameter(_) | Variability::Constant(_) => {
            var.binding.as_ref().or(var.start.as_ref())
        }
        _ => var.start.as_ref(),
    };
    let start_span = start_source
        .map(|expr| variable_attribute_owner_span(name, "start", expr))
        .transpose()?;
    let start = if let Some(expr) = start_source {
        let owner_span = variable_attribute_owner_span(name, "start", expr)?;
        let selected = select_scalar_start_record_alias(name, expr, known_var_names, owner_span);
        let rewritten = rewrite_start_expr_missing_refs(&selected, known_var_names);
        Some(flat_to_dae_expression(&rewritten))
    } else {
        None
    };
    let min_span = var
        .min
        .as_ref()
        .map(|expr| variable_attribute_owner_span(name, "min", expr))
        .transpose()?;
    let max_span = var
        .max
        .as_ref()
        .map(|expr| variable_attribute_owner_span(name, "max", expr))
        .transpose()?;
    let nominal_span = var
        .nominal
        .as_ref()
        .map(|expr| variable_attribute_owner_span(name, "nominal", expr))
        .transpose()?;

    // A parameter is tunable (changeable at runtime in FMI 3.0 ConfigurationMode)
    // unless it is structural: evaluate=true or discrete-typed (Integer/Boolean
    // used for array sizing).
    let is_tunable = matches!(var.variability, Variability::Parameter(_))
        && !var.evaluate
        && !var.is_discrete_type;

    Ok(Variable {
        name: flat_to_dae_var_name(name),
        component_ref: var.component_ref.clone(),
        source_span: var.source_span,
        dims: var.dims.clone(),
        start,
        start_span,
        fixed: var.fixed,
        min: var.min.as_ref().map(flat_to_dae_expression),
        min_span,
        max: var.max.as_ref().map(flat_to_dae_expression),
        max_span,
        nominal: var.nominal.as_ref().map(flat_to_dae_expression),
        nominal_span,
        unit: var.unit.clone(),
        state_select: var.state_select,
        description: var.description.clone(),
        causality: variable_causality(var, is_tunable),
        is_tunable,
        origin: rumoca_ir_dae::VariableOrigin::Source,
    })
}

fn variable_attribute_owner_span(
    variable: &VarName,
    attribute: &str,
    expr: &Expression,
) -> Result<rumoca_core::Span, ToDaeError> {
    expr.span().filter(|span| !span.is_dummy()).ok_or_else(|| {
        ToDaeError::runtime_metadata_violation(format!(
            "variable attribute `{}.{attribute}` is missing source provenance",
            variable.as_str()
        ))
    })
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
    known_var_names: &HashSet<String>,
    owner_span: rumoca_core::Span,
) -> Expression {
    let lhs_path = rumoca_core::ComponentPath::from_flat_path(lhs_name.as_str());
    let leaf_field = lhs_path.parts().last();
    let Some(lhs_base) = flat::component_base_name(lhs_name.as_str()) else {
        return select_leaf_or_original(expr, leaf_field, known_var_names, owner_span);
    };
    if lhs_base == lhs_name.as_str() {
        return select_leaf_or_original(expr, leaf_field, known_var_names, owner_span);
    }
    let Some(field_suffix) = lhs_name.as_str().strip_prefix(lhs_base.as_str()) else {
        return select_leaf_or_original(expr, leaf_field, known_var_names, owner_span);
    };
    if !field_suffix.starts_with('.') {
        return select_leaf_or_original(expr, leaf_field, known_var_names, owner_span);
    }
    let selector = StartRecordAliasSelector {
        field_suffix,
        leaf_field,
        known_var_names,
        owner_span,
    };

    match expr {
        Expression::VarRef {
            name: rhs_name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            select_varref_start_record_alias(expr, rhs_name.var_name(), *span, &selector)
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
            select_field_start_record_alias(expr, rhs_name.var_name(), field, *span, &selector)
        }
        Expression::FunctionCall {
            name,
            is_constructor: false,
            ..
        } if is_state_constructor_function(name) => select_function_record_field_start(
            expr,
            field_suffix,
            leaf_field.map(String::as_str),
            owner_span,
        )
        .unwrap_or_else(|| expr.clone()),
        _ => expr.clone(),
    }
}

struct StartRecordAliasSelector<'a> {
    field_suffix: &'a str,
    leaf_field: Option<&'a String>,
    known_var_names: &'a HashSet<String>,
    owner_span: rumoca_core::Span,
}

fn select_leaf_or_original(
    expr: &Expression,
    leaf_field: Option<&String>,
    known_var_names: &HashSet<String>,
    owner_span: rumoca_core::Span,
) -> Expression {
    select_leaf_start_record_alias(expr, leaf_field, known_var_names, owner_span)
        .unwrap_or_else(|| expr.clone())
}

fn select_varref_start_record_alias(
    expr: &Expression,
    rhs_name: &VarName,
    span: rumoca_core::Span,
    selector: &StartRecordAliasSelector<'_>,
) -> Expression {
    // A binding that already names a known scalar variable is a complete value;
    // record-field selection would graft the LHS field onto an unrelated
    // variable (e.g. `resistor.m = multiStarResistance.mBasic` must not become
    // `m`).
    if selector.known_var_names.contains(rhs_name.as_str()) {
        return expr.clone();
    }
    resolve_selected_start_var(
        &format!("{}{}", rhs_name.as_str(), selector.field_suffix),
        selector.known_var_names,
        span,
        selector.owner_span,
    )
    .or_else(|| {
        selector.leaf_field.and_then(|field| {
            resolve_selected_start_var(
                &format!("{}.{}", rhs_name.as_str(), field),
                selector.known_var_names,
                span,
                selector.owner_span,
            )
        })
    })
    .unwrap_or_else(|| expr.clone())
}

fn select_field_start_record_alias(
    expr: &Expression,
    rhs_name: &VarName,
    field: &str,
    span: rumoca_core::Span,
    selector: &StartRecordAliasSelector<'_>,
) -> Expression {
    if selector
        .known_var_names
        .contains(&format!("{}.{}", rhs_name.as_str(), field))
    {
        return expr.clone();
    }
    resolve_selected_start_var(
        &format!("{}.{}{}", rhs_name.as_str(), field, selector.field_suffix),
        selector.known_var_names,
        span,
        selector.owner_span,
    )
    .or_else(|| {
        selector.leaf_field.and_then(|lhs_leaf| {
            resolve_selected_start_var(
                &format!("{}.{}.{}", rhs_name.as_str(), field, lhs_leaf),
                selector.known_var_names,
                span,
                selector.owner_span,
            )
        })
    })
    .unwrap_or_else(|| expr.clone())
}

fn resolve_selected_start_var(
    candidate: &str,
    known_var_names: &HashSet<String>,
    span: rumoca_core::Span,
    owner_span: rumoca_core::Span,
) -> Option<Expression> {
    crate::path_utils::resolve_known_path_suffix(candidate, known_var_names).map(|selected| {
        Expression::VarRef {
            name: VarName::new(selected).into(),
            subscripts: vec![],
            span: real_or_owner_span(span, owner_span),
        }
    })
}

fn is_state_constructor_function(name: &rumoca_core::Reference) -> bool {
    matches!(
        name.var_name().last_segment(),
        "setState_pTX"
            | "setState_pT"
            | "setState_dTX"
            | "setState_phX"
            | "setState_ph"
            | "setState_psX"
            | "setState_ps"
            | "setSmoothState"
    )
}

fn select_function_record_field_start(
    expr: &Expression,
    field_suffix: &str,
    leaf_field: Option<&str>,
    owner_span: rumoca_core::Span,
) -> Option<Expression> {
    let suffix = field_suffix.strip_prefix('.')?;
    let mut segments = rumoca_core::ComponentPath::from_flat_path(suffix).into_parts();
    if segments.is_empty()
        && let Some(field) = leaf_field
    {
        segments.push(field.to_string());
    }
    let mut selected = expr.clone();
    for field in segments {
        selected = Expression::FieldAccess {
            base: Box::new(selected),
            field,
            span: owner_span,
        };
    }
    Some(selected)
}

fn select_leaf_start_record_alias(
    expr: &Expression,
    leaf_field: Option<&String>,
    known_var_names: &HashSet<String>,
    owner_span: rumoca_core::Span,
) -> Option<Expression> {
    let field = leaf_field?;
    match expr {
        Expression::VarRef {
            name: rhs_name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            if known_var_names.contains(rhs_name.as_str()) {
                return None;
            }
            let selected = format!("{}.{}", rhs_name.as_str(), field);
            crate::path_utils::resolve_known_path_suffix(&selected, known_var_names).map(
                |selected| Expression::VarRef {
                    name: VarName::new(selected).into(),
                    subscripts: vec![],
                    span: real_or_owner_span(*span, owner_span),
                },
            )
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
            crate::path_utils::resolve_known_path_suffix(&selected, known_var_names).map(
                |selected| Expression::VarRef {
                    name: VarName::new(selected).into(),
                    subscripts: vec![],
                    span: real_or_owner_span(*span, owner_span),
                },
            )
        }
        Expression::FunctionCall {
            name,
            is_constructor: false,
            ..
        } if is_state_constructor_function(name) => Some(Expression::FieldAccess {
            base: Box::new(expr.clone()),
            field: field.clone(),
            span: owner_span,
        }),
        _ => None,
    }
}

fn real_or_owner_span(span: rumoca_core::Span, owner_span: rumoca_core::Span) -> rumoca_core::Span {
    if span.is_dummy() { owner_span } else { span }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("scalar_inference_test.mo"),
            4,
            12,
        )
    }

    #[test]
    fn create_dae_variable_preserves_component_reference() {
        let component_def_id = rumoca_core::DefId::new(17);
        let name = VarName::new("x");
        let span = test_span();
        let flat_var = flat::Variable {
            name: name.clone(),
            component_ref: Some(rumoca_core::ComponentReference {
                local: false,
                span,
                parts: vec![rumoca_core::ComponentRefPart {
                    ident: "x".to_string(),
                    span,
                    subs: vec![],
                }],
                def_id: Some(component_def_id),
            }),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        };
        let known_var_names = HashSet::from([name.as_str().to_string()]);

        let dae_var = create_dae_variable(&name, &flat_var, &known_var_names)
            .unwrap_or_else(|err| panic!("DAE variable should build: {err}"));

        assert_eq!(
            dae_var
                .component_ref
                .as_ref()
                .and_then(|reference| reference.def_id),
            Some(component_def_id)
        );
    }

    #[test]
    fn record_field_start_from_function_call_preserves_field_access() {
        let span = test_span();
        let expr = Expression::FunctionCall {
            name: rumoca_core::Reference::new("Buildings.Media.Air.setState_pTX"),
            args: vec![],
            is_constructor: false,
            span,
        };
        let selected = select_scalar_start_record_alias(
            &VarName::new("state_default.X"),
            &expr,
            &HashSet::from(["state_default.X".to_string()]),
            span,
        );

        let Expression::FieldAccess { base, field, .. } = selected else {
            panic!("expected function field access start, got {selected:?}");
        };
        assert_eq!(field, "X");
        assert!(matches!(
            base.as_ref(),
            Expression::FunctionCall { name, .. }
                if name.as_str() == "Buildings.Media.Air.setState_pTX"
        ));
    }

    #[test]
    fn record_field_start_from_constructor_call_is_not_rewritten_to_field_access() {
        let span = test_span();
        let expr = Expression::FunctionCall {
            name: rumoca_core::Reference::new("Modelica.Blocks.Types.ExternalCombiTable1D"),
            args: vec![],
            is_constructor: true,
            span,
        };
        let selected = select_scalar_start_record_alias(
            &VarName::new("tableID"),
            &expr,
            &HashSet::from(["tableID".to_string()]),
            span,
        );

        assert!(matches!(
            selected,
            Expression::FunctionCall {
                is_constructor: true,
                ..
            }
        ));
    }

    #[test]
    fn scalar_start_from_function_call_is_not_rewritten_to_field_access() {
        let span = test_span();
        let expr = Expression::FunctionCall {
            name: rumoca_core::Reference::new("Modelica.Units.Conversions.from_degC"),
            args: vec![],
            is_constructor: false,
            span,
        };
        let selected = select_scalar_start_record_alias(
            &VarName::new("component.T_start"),
            &expr,
            &HashSet::from(["component.T_start".to_string()]),
            span,
        );

        assert!(matches!(
            selected,
            Expression::FunctionCall {
                is_constructor: false,
                ..
            }
        ));
    }

    #[test]
    fn indexed_lhs_scalar_size_uses_record_element_width_for_dynamic_subscript() {
        let mut flat = Model::new();
        for name in ["sym[1].re", "sym[1].im", "sym[2].re", "sym[2].im", "idx[1]"] {
            flat.add_variable(
                VarName::new(name),
                flat::Variable {
                    name: VarName::new(name),
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }

        let residual = Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::Index {
                base: Box::new(Expression::VarRef {
                    name: VarName::new("sym").into(),
                    subscripts: vec![],
                    span: test_span(),
                }),
                subscripts: vec![Subscript::Expr {
                    expr: Box::new(Expression::VarRef {
                        name: VarName::new("idx").into(),
                        subscripts: vec![Subscript::Index {
                            value: 1,
                            span: test_span(),
                        }],
                        span: test_span(),
                    }),
                    span: test_span(),
                }],
                span: test_span(),
            }),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("Complex").into(),
                args: vec![
                    Expression::Literal {
                        value: Literal::Integer(0),
                        span: test_span(),
                    },
                    Expression::Literal {
                        value: Literal::Integer(0),
                        span: test_span(),
                    },
                ],
                is_constructor: true,
                span: test_span(),
            }),
            span: test_span(),
        };

        let prefix_counts = build_prefix_counts(&flat);
        assert_eq!(
            infer_equation_scalar_count(&residual, &flat, &prefix_counts),
            2
        );
    }

    #[test]
    fn record_constructor_residual_uses_scalarized_unknown_width() {
        let mut flat = Model::new();
        let mut complex = rumoca_core::Function::new("Complex", test_span());
        complex.is_constructor = true;
        complex.add_input(rumoca_core::FunctionParam::new("re", "Real", test_span()));
        complex.add_input(rumoca_core::FunctionParam::new("im", "Real", test_span()));
        complex.add_output(rumoca_core::FunctionParam::new(
            "result",
            "Complex",
            test_span(),
        ));
        flat.add_function(complex);
        for name in ["currentSensor.i.re", "currentSensor.i.im"] {
            flat.add_variable(
                VarName::new(name),
                flat::Variable {
                    name: VarName::new(name),
                    is_primitive: true,
                    ..flat::Variable::empty_with_span(test_span())
                },
            );
        }

        let residual = Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(Expression::FunctionCall {
                name: VarName::new("Complex").into(),
                args: vec![
                    Expression::Literal {
                        value: Literal::Integer(0),
                        span: test_span(),
                    },
                    Expression::Literal {
                        value: Literal::Integer(0),
                        span: test_span(),
                    },
                ],
                is_constructor: true,
                span: test_span(),
            }),
            rhs: Box::new(Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Box::new(Expression::VarRef {
                    name: VarName::new("currentSensor.i").into(),
                    subscripts: vec![],
                    span: test_span(),
                }),
                rhs: Box::new(Expression::VarRef {
                    name: VarName::new("currentSensor.i").into(),
                    subscripts: vec![],
                    span: test_span(),
                }),
                span: test_span(),
            }),
            span: test_span(),
        };

        let prefix_counts = build_prefix_counts(&flat);
        assert_eq!(
            infer_equation_scalar_count(&residual, &flat, &prefix_counts),
            2
        );
    }

    #[test]
    fn create_dae_variable_preserves_structured_binding_references() {
        let record_def_id = rumoca_core::DefId::new(42);
        let name = VarName::new("x");
        let span = test_span();
        let record_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "mp".to_string(),
                    span,
                    subs: vec![],
                },
                rumoca_core::ComponentRefPart {
                    ident: "modelcard".to_string(),
                    span,
                    subs: vec![],
                },
            ],
            def_id: Some(record_def_id),
        };
        let flat_var = flat::Variable {
            name: name.clone(),
            variability: Variability::Parameter(Default::default()),
            binding: Some(Expression::FunctionCall {
                name: rumoca_core::Reference::new("f"),
                args: vec![Expression::VarRef {
                    name: rumoca_core::Reference::with_component_reference(
                        "mp.modelcard",
                        record_ref.clone(),
                    ),
                    subscripts: vec![],
                    span,
                }],
                is_constructor: false,
                span,
            }),
            is_primitive: true,
            source_span: span,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        };
        let known_var_names = HashSet::from([name.as_str().to_string()]);

        let dae_var = create_dae_variable(&name, &flat_var, &known_var_names)
            .unwrap_or_else(|err| panic!("DAE variable should build: {err}"));

        let Some(Expression::FunctionCall { args, .. }) = dae_var.start else {
            panic!("expected function-call start");
        };
        let Some(Expression::VarRef {
            name: start_name, ..
        }) = args.first()
        else {
            panic!("expected structured record VarRef argument");
        };
        assert_eq!(start_name.component_ref(), Some(&record_ref));
    }

    #[test]
    fn rewrite_start_expr_preserves_top_level_structured_reference_when_not_aliasing() {
        let record_def_id = rumoca_core::DefId::new(43);
        let span = test_span();
        let record_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "mp".to_string(),
                    span,
                    subs: vec![],
                },
                rumoca_core::ComponentRefPart {
                    ident: "modelcard".to_string(),
                    span,
                    subs: vec![],
                },
            ],
            def_id: Some(record_def_id),
        };
        let expr = Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                "mp.modelcard",
                record_ref.clone(),
            ),
            subscripts: vec![],
            span,
        };
        let known_var_names = HashSet::new();

        let Expression::VarRef {
            name: rewritten, ..
        } = rewrite_start_expr_missing_refs(&expr, &known_var_names)
        else {
            panic!("expected structured VarRef");
        };
        assert_eq!(rewritten.component_ref(), Some(&record_ref));
    }

    #[test]
    fn rewrite_start_expr_rebases_structured_class_qualified_suffix() {
        let span =
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 3, 4);
        let class_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: ["Modelica", "Fluid", "PipeFlow", "system", "use_eps_Re"]
                .into_iter()
                .map(|ident| rumoca_core::ComponentRefPart {
                    ident: ident.to_string(),
                    span,
                    subs: vec![],
                })
                .collect(),
            def_id: None,
        };
        let expr = Expression::VarRef {
            name: rumoca_core::Reference::with_component_reference(
                "Modelica.Fluid.PipeFlow.system.use_eps_Re",
                class_ref,
            ),
            subscripts: vec![],
            span,
        };
        let known_var_names = HashSet::from(["system.use_eps_Re".to_string()]);

        let Expression::VarRef {
            name: rewritten, ..
        } = rewrite_start_expr_missing_refs(&expr, &known_var_names)
        else {
            panic!("expected structured VarRef");
        };
        assert_eq!(rewritten.as_str(), "system.use_eps_Re");
        assert!(
            rewritten.component_ref().is_none(),
            "rebased suffix references must drop the stale class-qualified component reference"
        );
    }

    #[test]
    fn create_dae_variable_surfaces_source_causality() {
        let name = VarName::new("u");
        let flat_var = flat::Variable {
            name: name.clone(),
            causality: rumoca_core::Causality::Input(Default::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        };
        let known_var_names = HashSet::from([name.as_str().to_string()]);

        let dae_var = create_dae_variable(&name, &flat_var, &known_var_names)
            .unwrap_or_else(|err| panic!("DAE variable should build: {err}"));

        assert_eq!(dae_var.causality, rumoca_ir_dae::VariableCausality::Input);
    }

    #[test]
    fn create_dae_variable_marks_parameter_causality() {
        let name = VarName::new("p");
        let flat_var = flat::Variable {
            name: name.clone(),
            variability: Variability::Parameter(Default::default()),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        };
        let known_var_names = HashSet::from([name.as_str().to_string()]);

        let dae_var = create_dae_variable(&name, &flat_var, &known_var_names)
            .unwrap_or_else(|err| panic!("DAE variable should build: {err}"));

        assert_eq!(
            dae_var.causality,
            rumoca_ir_dae::VariableCausality::Parameter
        );
    }

    #[test]
    fn create_dae_variable_rejects_unspanned_start_attribute() {
        let name = VarName::new("x");
        let flat_var = flat::Variable {
            name: name.clone(),
            start: Some(Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span: rumoca_core::Span::DUMMY,
            }),
            is_primitive: true,
            ..rumoca_ir_flat::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        };
        let known_var_names = HashSet::from([name.as_str().to_string()]);

        let err = create_dae_variable(&name, &flat_var, &known_var_names)
            .expect_err("unspanned start attribute should fail");

        assert!(matches!(err, ToDaeError::RuntimeMetadataViolation { .. }));
    }
}
