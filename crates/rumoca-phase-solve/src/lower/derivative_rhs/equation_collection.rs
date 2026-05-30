use super::*;

pub(in crate::lower) fn collect_derivative_equations(
    dae_model: &dae::Dae,
    state_names: &[String],
    structural_bindings: &IndexMap<String, f64>,
) -> (Vec<DerivativeEquation>, Vec<bool>) {
    let mut equations = Vec::new();
    let mut flags = Vec::with_capacity(dae_model.continuous.equations.len());
    for equation in &dae_model.continuous.equations {
        let before = equations.len();
        if let Some(projected) =
            function_projected_residuals(&equation.rhs, dae_model, structural_bindings)
        {
            for residual in projected {
                append_derivative_rows_for_residual(
                    &mut equations,
                    &residual,
                    state_names,
                    dae_model,
                    structural_bindings,
                    Some(1),
                );
            }
            flags.push(equations.len() > before);
            continue;
        }
        append_derivative_rows_for_residual(
            &mut equations,
            &equation.rhs,
            state_names,
            dae_model,
            structural_bindings,
            Some(equation.scalar_count),
        );
        flags.push(equations.len() > before);
    }
    (equations, flags)
}

pub(in crate::lower) fn append_derivative_rows_for_residual(
    equations: &mut Vec<DerivativeEquation>,
    residual: &rumoca_core::Expression,
    state_names: &[String],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    scalar_count: Option<usize>,
) {
    if let Some(mut rows) = derivative_equations_from_residual(
        residual,
        state_names,
        dae_model,
        structural_bindings,
        scalar_count,
    ) {
        equations.append(&mut rows);
    }
}

pub(in crate::lower) fn collect_direct_assignments(
    dae_model: &dae::Dae,
    equation_flags: &[bool],
) -> IndexMap<String, DirectAssignmentValue> {
    let mut assignments = IndexMap::new();
    for (idx, equation) in dae_model.continuous.equations.iter().enumerate() {
        if equation_flags.get(idx).copied().unwrap_or(false) {
            continue;
        }
        let Some((target, rhs)) = direct_assignment_target_rhs(equation) else {
            continue;
        };
        insert_direct_assignment(
            dae_model,
            &mut assignments,
            target,
            rhs,
            equation.scalar_count,
        );
    }
    assignments
}

pub(in crate::lower) fn collect_missing_indexed_record_field_assignments(
    dae_model: &dae::Dae,
    state_names: &[String],
    layout: &VarLayout,
    structural_bindings: &IndexMap<String, f64>,
) -> IndexMap<String, DirectAssignmentValue> {
    let (_, equation_flags) =
        collect_derivative_equations(dae_model, state_names, structural_bindings);
    collect_direct_assignments(dae_model, &equation_flags)
        .into_iter()
        .filter(|(key, _assignment)| {
            layout.binding(key).is_none() && has_indexed_record_field_segment(key)
        })
        .collect()
}

pub(in crate::lower) fn has_indexed_record_field_segment(key: &str) -> bool {
    rumoca_core::split_path_with_indices(key)
        .iter()
        .any(|segment| rumoca_core::split_trailing_subscript_suffix(segment).is_some())
}

pub(in crate::lower) fn direct_assignment_target_rhs(
    equation: &dae::Equation,
) -> Option<(rumoca_core::VarName, rumoca_core::Expression)> {
    if let Some(lhs) = equation.lhs.as_ref()
        && !equation.rhs.contains_der()
    {
        return Some((lhs.clone(), equation.rhs.clone()));
    }

    let (lhs, rhs) = split_subtraction(&equation.rhs)?;
    if let Some(target) = direct_assignment_target(lhs)
        && !rhs.contains_der()
    {
        return Some((target, rhs.clone()));
    }
    if let Some(target) = direct_assignment_target(rhs)
        && !lhs.contains_der()
    {
        return Some((target, lhs.clone()));
    }
    None
}

pub(in crate::lower) fn direct_assignment_target(
    expr: &rumoca_core::Expression,
) -> Option<rumoca_core::VarName> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            if subscripts.is_empty() {
                return Some(name.var_name().clone());
            }
            let indices = super::super::static_subscript_indices(subscripts).ok()??;
            Some(rumoca_core::VarName::new(dae::format_subscript_key(
                name.as_str(),
                &indices,
            )))
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base = direct_assignment_target(base)?;
            let indices = super::super::static_subscript_indices(subscripts).ok()??;
            Some(rumoca_core::VarName::new(dae::format_subscript_key(
                base.as_str(),
                &indices,
            )))
        }
        _ => None,
    }
}

pub(in crate::lower) fn insert_direct_assignment(
    dae_model: &dae::Dae,
    assignments: &mut IndexMap<String, DirectAssignmentValue>,
    target: rumoca_core::VarName,
    rhs: rumoca_core::Expression,
    scalar_count: usize,
) {
    let target_key = target.as_str();
    let Some((base, dims, size)) = direct_assignment_shape(dae_model, target_key, scalar_count)
    else {
        return;
    };
    if dae_model
        .variables
        .states
        .contains_key(&rumoca_core::VarName::new(base.as_str()))
    {
        return;
    }

    if size <= 1 || base != target_key {
        assignments.insert(target_key.to_string(), DirectAssignmentValue::full(rhs));
        return;
    }

    let repeat_period = direct_assignment_repeat_period(dae_model, &dims, &rhs);
    assignments.insert(base.clone(), DirectAssignmentValue::full(rhs.clone()));
    for flat_index in 0..size {
        let key = dae::scalar_name_text_for_flat_index(&base, &dims, flat_index);
        assignments.insert(
            key,
            DirectAssignmentValue::scalar(rhs.clone(), flat_index, repeat_period),
        );
    }
}

pub(in crate::lower) fn direct_assignment_repeat_period(
    dae_model: &dae::Dae,
    target_dims: &[i64],
    rhs: &rumoca_core::Expression,
) -> Option<usize> {
    let rhs_dims = direct_assignment_rhs_dims(dae_model, rhs)?;
    if rhs_dims.is_empty() || target_dims.len() != rhs_dims.len() + 1 {
        return None;
    }
    let target_tail = target_dims.get(1..)?;
    let target_matches_rhs = target_tail
        .iter()
        .zip(rhs_dims.iter())
        .all(|(target_dim, rhs_dim)| usize::try_from(*target_dim).ok() == Some(*rhs_dim));
    if !target_matches_rhs {
        return None;
    }
    let period = rhs_dims.iter().product::<usize>();
    (period > 1).then_some(period)
}

pub(in crate::lower) fn direct_assignment_rhs_dims(
    dae_model: &dae::Dae,
    rhs: &rumoca_core::Expression,
) -> Option<Vec<usize>> {
    match rhs {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => variable_dims(dae_model, name.as_str()),
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => direct_assignment_array_dims(elements, *is_matrix),
        _ => None,
    }
}

pub(in crate::lower) fn direct_assignment_array_dims(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
) -> Option<Vec<usize>> {
    if !is_matrix {
        return Some(vec![elements.len()]);
    }
    let first = elements.first()?;
    let rumoca_core::Expression::Array {
        elements: first_row,
        ..
    } = first
    else {
        return None;
    };
    Some(vec![elements.len(), first_row.len()])
}

pub(in crate::lower) fn direct_assignment_shape(
    dae_model: &dae::Dae,
    target: &str,
    scalar_count: usize,
) -> Option<(String, Vec<i64>, usize)> {
    if let Some(var) = dae_model
        .variables
        .algebraics
        .get(&rumoca_core::VarName::new(target))
    {
        return Some((target.to_string(), var.dims.clone(), var.size()));
    }
    if let Some(var) = dae_model
        .variables
        .outputs
        .get(&rumoca_core::VarName::new(target))
    {
        return Some((target.to_string(), var.dims.clone(), var.size()));
    }
    let base = dae::component_base_name(target)?;
    if let Some(var) = dae_model
        .variables
        .algebraics
        .get(&rumoca_core::VarName::new(base.as_str()))
    {
        return Some((base, var.dims.clone(), 1));
    }
    if let Some(var) = dae_model
        .variables
        .outputs
        .get(&rumoca_core::VarName::new(base.as_str()))
    {
        return Some((base, var.dims.clone(), 1));
    }
    (scalar_count == 1).then(|| (target.to_string(), Vec::new(), 1))
}

pub(in crate::lower) fn derivative_equation_from_residual(
    residual: &rumoca_core::Expression,
    ctx: &DerivativeLinearCtx<'_>,
) -> Option<DerivativeEquation> {
    if let Some(equation) = derivative_equation_from_if_residual(residual, ctx) {
        return Some(equation);
    }

    if let Some((lhs, rhs)) = split_subtraction(residual) {
        if let Some((coefficients, remainder)) = derivative_linear_parts(lhs, ctx)
            && !rhs.contains_der()
        {
            return Some(DerivativeEquation {
                coefficients,
                rhs: rhs_without_remainder(rhs.clone(), remainder),
                span: residual.span().unwrap_or(rumoca_core::Span::DUMMY),
            });
        }
        if let Some((coefficients, remainder)) = derivative_linear_parts(rhs, ctx)
            && !lhs.contains_der()
        {
            return Some(DerivativeEquation {
                coefficients,
                rhs: rhs_without_remainder(lhs.clone(), remainder),
                span: residual.span().unwrap_or(rumoca_core::Span::DUMMY),
            });
        }
    }
    if let Some((coefficients, remainder)) = derivative_linear_parts(residual, ctx) {
        return Some(DerivativeEquation {
            coefficients,
            rhs: rhs_without_remainder(zero_expr(), remainder),
            span: residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        });
    }
    None
}

pub(in crate::lower) fn derivative_equations_from_residual(
    residual: &rumoca_core::Expression,
    state_names: &[String],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    scalar_count: Option<usize>,
) -> Option<Vec<DerivativeEquation>> {
    if let Some(equations) = matrix_vector_derivative_equations_from_residual(
        residual,
        state_names,
        dae_model,
        structural_bindings,
    ) {
        return Some(equations);
    }
    if let Some(equations) = direct_derivative_equations_from_residual(
        residual,
        state_names,
        dae_model,
        structural_bindings,
    ) {
        return Some(equations);
    }
    if let Some(equations) = affine_vector_derivative_equations_from_residual(
        residual,
        state_names,
        dae_model,
        structural_bindings,
    ) {
        return Some(equations);
    }
    let ctx = DerivativeLinearCtx {
        state_names,
        dae_model,
        structural_bindings,
    };
    if let Some(equations) =
        projected_vector_derivative_equations_from_residual(residual, scalar_count, &ctx)
    {
        return Some(equations);
    }
    derivative_equation_from_residual(residual, &ctx).map(|equation| vec![equation])
}

pub(in crate::lower) fn projected_vector_derivative_equations_from_residual(
    residual: &rumoca_core::Expression,
    scalar_count: Option<usize>,
    ctx: &DerivativeLinearCtx<'_>,
) -> Option<Vec<DerivativeEquation>> {
    if !residual.contains_der() {
        return None;
    }
    if scalar_count.is_none_or(|count| count <= 1) {
        return None;
    }
    let dims = expression_result_dims(residual, ctx.dae_model, ctx.structural_bindings).ok()?;
    let count = dims.iter().product::<usize>();
    if count <= 1 {
        return None;
    }
    let scalars =
        project_expression_scalars(residual, &dims, ctx.dae_model, ctx.structural_bindings)
            .ok()??;
    if scalars.len() != count {
        return None;
    }
    let equations = scalars
        .into_iter()
        .map(|scalar| derivative_equation_from_residual(&scalar, ctx))
        .collect::<Option<Vec<_>>>()?;
    (!equations.is_empty()).then_some(equations)
}

pub(in crate::lower) fn direct_derivative_equations_from_residual(
    residual: &rumoca_core::Expression,
    state_names: &[String],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Option<Vec<DerivativeEquation>> {
    let (lhs, rhs) = split_subtraction(residual)?;
    if let Some(target) = pure_der_arg(lhs)
        && !rhs.contains_der()
    {
        return expanded_direct_derivative_equations(
            target,
            rhs,
            state_names,
            dae_model,
            structural_bindings,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        );
    }
    if let Some(target) = pure_der_arg(rhs)
        && !lhs.contains_der()
    {
        return expanded_direct_derivative_equations(
            target,
            lhs,
            state_names,
            dae_model,
            structural_bindings,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        );
    }
    None
}

pub(in crate::lower) struct MatrixDerivativeTerm {
    target: rumoca_core::Expression,
    coefficient_rows: Vec<IndexMap<String, rumoca_core::Expression>>,
}

pub(in crate::lower) struct AffineVectorDerivativeTerm {
    target: rumoca_core::Expression,
    coefficient: rumoca_core::Expression,
    remainder: Option<rumoca_core::Expression>,
}

pub(in crate::lower) fn affine_vector_derivative_equations_from_residual(
    residual: &rumoca_core::Expression,
    state_names: &[String],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Option<Vec<DerivativeEquation>> {
    let (lhs, rhs) = split_subtraction(residual)?;
    if let Some(term) = affine_vector_derivative_term(rhs)
        && !lhs.contains_der()
    {
        return build_affine_vector_derivative_equations(
            term,
            lhs,
            state_names,
            dae_model,
            structural_bindings,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        );
    }
    if let Some(term) = affine_vector_derivative_term(lhs)
        && !rhs.contains_der()
    {
        return build_affine_vector_derivative_equations(
            term,
            rhs,
            state_names,
            dae_model,
            structural_bindings,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        );
    }
    None
}

pub(in crate::lower) fn affine_vector_derivative_term(
    expr: &rumoca_core::Expression,
) -> Option<AffineVectorDerivativeTerm> {
    if let Some(target) = pure_der_arg(expr) {
        return Some(AffineVectorDerivativeTerm {
            target: target.clone(),
            coefficient: one_expr(),
            remainder: None,
        });
    }

    let rumoca_core::Expression::Binary { op, lhs, rhs, span } = expr else {
        return None;
    };
    match op {
        op if is_add(op) => {
            if let Some(mut term) = affine_vector_derivative_term(lhs)
                && !rhs.contains_der()
            {
                term.remainder = Some(combine_optional_remainder(
                    term.remainder,
                    rhs.as_ref().clone(),
                    false,
                    *span,
                ));
                return Some(term);
            }
            if let Some(mut term) = affine_vector_derivative_term(rhs)
                && !lhs.contains_der()
            {
                term.remainder = Some(combine_optional_remainder(
                    term.remainder,
                    lhs.as_ref().clone(),
                    false,
                    *span,
                ));
                return Some(term);
            }
            None
        }
        op if is_sub(op) => {
            if let Some(mut term) = affine_vector_derivative_term(lhs)
                && !rhs.contains_der()
            {
                term.remainder = Some(combine_optional_remainder(
                    term.remainder,
                    rhs.as_ref().clone(),
                    true,
                    *span,
                ));
                return Some(term);
            }
            if let Some(mut term) = affine_vector_derivative_term(rhs)
                && !lhs.contains_der()
            {
                term.coefficient = neg_with_span(term.coefficient, *span);
                term.remainder = Some(combine_optional_remainder(
                    Some(lhs.as_ref().clone()),
                    term.remainder.unwrap_or(rumoca_core::Expression::Literal {
                        value: Literal::Real(0.0),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    true,
                    *span,
                ));
                return Some(term);
            }
            None
        }
        op if is_mul(op) => {
            if let Some(target) = pure_der_arg(lhs)
                && !rhs.contains_der()
            {
                return Some(AffineVectorDerivativeTerm {
                    target: target.clone(),
                    coefficient: rhs.as_ref().clone(),
                    remainder: None,
                });
            }
            if let Some(target) = pure_der_arg(rhs)
                && !lhs.contains_der()
            {
                return Some(AffineVectorDerivativeTerm {
                    target: target.clone(),
                    coefficient: lhs.as_ref().clone(),
                    remainder: None,
                });
            }
            None
        }
        _ => None,
    }
}

pub(in crate::lower) fn combine_optional_remainder(
    current: Option<rumoca_core::Expression>,
    next: rumoca_core::Expression,
    subtract_next: bool,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    match (current, subtract_next) {
        (Some(current), false) => add_with_span(current, next, span),
        (Some(current), true) => sub_with_span(current, next, span),
        (None, false) => next,
        (None, true) => neg_with_span(next, span),
    }
}

pub(in crate::lower) fn build_affine_vector_derivative_equations(
    term: AffineVectorDerivativeTerm,
    base_rhs: &rumoca_core::Expression,
    state_names: &[String],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
    span: rumoca_core::Span,
) -> Option<Vec<DerivativeEquation>> {
    let target_keys =
        derivative_arg_binding_keys(&term.target, dae_model, structural_bindings).ok()?;
    if target_keys.len() <= 1 || !target_keys.iter().all(|key| state_names.contains(key)) {
        return None;
    }
    let base_values = scalarized_rhs_expressions(
        base_rhs,
        &term.target,
        target_keys.len(),
        dae_model,
        structural_bindings,
    )
    .ok()?;
    if base_values.len() != target_keys.len() {
        return None;
    }
    let remainder_values = term
        .remainder
        .as_ref()
        .map(|remainder| {
            scalarized_rhs_expressions(
                remainder,
                &term.target,
                target_keys.len(),
                dae_model,
                structural_bindings,
            )
        })
        .transpose()
        .ok()?;
    let coefficient_values = scalarized_coefficient_expressions(
        &term.coefficient,
        &term.target,
        target_keys.len(),
        dae_model,
        structural_bindings,
    )
    .ok()?;

    Some(
        target_keys
            .into_iter()
            .enumerate()
            .map(|(idx, key)| {
                let rhs = match &remainder_values {
                    Some(remainders) => {
                        sub_with_span(base_values[idx].clone(), remainders[idx].clone(), span)
                    }
                    None => base_values[idx].clone(),
                };
                DerivativeEquation {
                    coefficients: IndexMap::from([(key, coefficient_values[idx].clone())]),
                    rhs,
                    span,
                }
            })
            .collect(),
    )
}

pub(in crate::lower) fn matrix_vector_derivative_equations_from_residual(
    residual: &rumoca_core::Expression,
    state_names: &[String],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Option<Vec<DerivativeEquation>> {
    let (lhs, rhs) = split_subtraction(residual)?;
    if let Some(term) =
        matrix_vector_derivative_term(lhs, state_names, dae_model, structural_bindings)
        && !rhs.contains_der()
    {
        return build_matrix_derivative_equations(term, rhs, dae_model, structural_bindings);
    }
    if let Some(term) =
        matrix_vector_derivative_term(rhs, state_names, dae_model, structural_bindings)
        && !lhs.contains_der()
    {
        return build_matrix_derivative_equations(term, lhs, dae_model, structural_bindings);
    }
    None
}

pub(in crate::lower) fn matrix_vector_derivative_term(
    expr: &rumoca_core::Expression,
    state_names: &[String],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Option<MatrixDerivativeTerm> {
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Mul,
        lhs,
        rhs,
        ..
    } = expr
    else {
        return None;
    };

    if let Some(target) = pure_der_arg(rhs) {
        return matrix_times_derivative_coefficients(
            lhs,
            target,
            state_names,
            dae_model,
            structural_bindings,
        );
    }
    if let Some(target) = pure_der_arg(lhs) {
        return derivative_times_matrix_coefficients(
            target,
            rhs,
            state_names,
            dae_model,
            structural_bindings,
        );
    }
    None
}

pub(in crate::lower) fn matrix_times_derivative_coefficients(
    matrix: &rumoca_core::Expression,
    target: &rumoca_core::Expression,
    state_names: &[String],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Option<MatrixDerivativeTerm> {
    let target_keys = derivative_arg_binding_keys(target, dae_model, structural_bindings).ok()?;
    let n = target_keys.len();
    if n == 0 || !target_keys.iter().all(|key| state_names.contains(key)) {
        return None;
    }
    let matrix_keys = expression_binding_keys(matrix, dae_model, structural_bindings).ok()??;
    if matrix_keys.len() != n * n {
        return None;
    }

    let coefficient_rows = (0..n)
        .map(|row| {
            target_keys
                .iter()
                .enumerate()
                .map(|(col, target_key)| {
                    (
                        target_key.clone(),
                        scalar_key_expr(matrix_keys[row * n + col].clone()),
                    )
                })
                .collect()
        })
        .collect();
    Some(MatrixDerivativeTerm {
        target: target.clone(),
        coefficient_rows,
    })
}

pub(in crate::lower) fn derivative_times_matrix_coefficients(
    target: &rumoca_core::Expression,
    matrix: &rumoca_core::Expression,
    state_names: &[String],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Option<MatrixDerivativeTerm> {
    let target_keys = derivative_arg_binding_keys(target, dae_model, structural_bindings).ok()?;
    let n = target_keys.len();
    if n == 0 || !target_keys.iter().all(|key| state_names.contains(key)) {
        return None;
    }
    let matrix_keys = expression_binding_keys(matrix, dae_model, structural_bindings).ok()??;
    if matrix_keys.len() != n * n {
        return None;
    }

    let coefficient_rows = (0..n)
        .map(|col| {
            target_keys
                .iter()
                .enumerate()
                .map(|(row, target_key)| {
                    (
                        target_key.clone(),
                        scalar_key_expr(matrix_keys[row * n + col].clone()),
                    )
                })
                .collect()
        })
        .collect();
    Some(MatrixDerivativeTerm {
        target: target.clone(),
        coefficient_rows,
    })
}

pub(in crate::lower) fn build_matrix_derivative_equations(
    term: MatrixDerivativeTerm,
    rhs: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Option<Vec<DerivativeEquation>> {
    let rhs_values = scalarized_rhs_expressions(
        rhs,
        &term.target,
        term.coefficient_rows.len(),
        dae_model,
        structural_bindings,
    )
    .ok()?;
    if rhs_values.len() != term.coefficient_rows.len() {
        return None;
    }
    let span = rhs.span().unwrap_or(rumoca_core::Span::DUMMY);
    Some(
        term.coefficient_rows
            .into_iter()
            .zip(rhs_values)
            .map(|(coefficients, rhs)| DerivativeEquation {
                coefficients,
                rhs,
                span,
            })
            .collect(),
    )
}

pub(in crate::lower) fn pure_der_arg(
    expr: &rumoca_core::Expression,
) -> Option<&rumoca_core::Expression> {
    let rumoca_core::Expression::BuiltinCall { function, args, .. } = expr else {
        return None;
    };
    (*function == rumoca_core::BuiltinFunction::Der)
        .then(|| args.first())
        .flatten()
}
