use super::*;

pub(in crate::lower) fn derivative_arg_binding_keys(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<String>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => binding_keys_for_subscripted_name(
            name.as_str(),
            subscripts,
            dae_model,
            structural_bindings,
        ),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base = binding_base_name(base)?;
            binding_keys_for_subscripted_name(&base, subscripts, dae_model, structural_bindings)
        }
        _ => Err(LowerError::Unsupported {
            reason: "unsupported der() argument in derivative RHS lowering".to_string(),
        }),
    }
}

pub(in crate::lower) fn scalarized_rhs_expressions(
    expr: &rumoca_core::Expression,
    target: &rumoca_core::Expression,
    expected: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    if let Some(expressions) = expression_binding_expressions(expr, dae_model, structural_bindings)?
        && expressions.len() == expected
    {
        return Ok(expressions);
    }
    if let Some(elements) = literal_array_elements(expr)
        && elements.len() == expected
    {
        return Ok(elements);
    }
    if let Some(values) = function_call_projected_scalars(expr, dae_model, structural_bindings)?
        && values.len() == expected
    {
        return Ok(values);
    }
    if expected == 1 {
        return Ok(vec![expr.clone()]);
    }
    let target_dims = derivative_target_result_dims(target, dae_model, structural_bindings)?;
    if target_dims.iter().product::<usize>() == expected
        && let Some(values) =
            project_expression_scalars(expr, &target_dims, dae_model, structural_bindings)?
        && values.len() == expected
    {
        return Ok(values);
    }
    indexed_rhs_expressions(expr, target, expected, dae_model, structural_bindings)
}

pub(in crate::lower) fn scalarized_coefficient_expressions(
    expr: &rumoca_core::Expression,
    target: &rumoca_core::Expression,
    expected: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    if expected == 1 || expression_result_dims(expr, dae_model, structural_bindings)?.is_empty() {
        return Ok(vec![expr.clone(); expected]);
    }
    scalarized_rhs_expressions(expr, target, expected, dae_model, structural_bindings)
}

pub(in crate::lower) fn project_expression_scalars(
    expr: &rumoca_core::Expression,
    dims: &[usize],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    let count = dims.iter().product::<usize>();
    (0..count)
        .map(|flat_index| {
            project_expression_scalar(expr, dims, flat_index, dae_model, structural_bindings)
        })
        .collect()
}

pub(in crate::lower) fn project_expression_scalar(
    expr: &rumoca_core::Expression,
    dims: &[usize],
    flat_index: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let ctx = ProjectionContext {
        dims,
        flat_index,
        dae_model,
        structural_bindings,
    };
    project_expression_scalar_ctx(expr, &ctx)
}

struct ProjectionContext<'a> {
    dims: &'a [usize],
    flat_index: usize,
    dae_model: &'a dae::Dae,
    structural_bindings: &'a IndexMap<String, f64>,
}

fn project_expression_scalar_ctx(
    expr: &rumoca_core::Expression,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef { .. } | rumoca_core::Expression::Index { .. } => {
            if let Some(expressions) =
                expression_binding_expressions(expr, ctx.dae_model, ctx.structural_bindings)?
                && let Some(expr) = expressions.get(ctx.flat_index)
            {
                return Ok(Some(expr.clone()));
            }
            Ok(None)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            Ok(project_literal_array_scalar(elements, ctx.flat_index))
        }
        rumoca_core::Expression::Unary { op, rhs, span } => {
            let Some(rhs) = project_operand_scalar_ctx(rhs, ctx)? else {
                return Ok(None);
            };
            Ok(Some(rumoca_core::Expression::Unary {
                op: op.clone(),
                rhs: Box::new(rhs),
                span: *span,
            }))
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args,
            span,
        } if args.len() == 1 => {
            let Some(arg) = project_operand_scalar_ctx(&args[0], ctx)? else {
                return Ok(None);
            };
            Ok(Some(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args: vec![arg],
                span: *span,
            }))
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Zeros,
            ..
        } => Ok(Some(real_literal_expr(0.0, expr))),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Ones,
            ..
        } => Ok(Some(real_literal_expr(1.0, expr))),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args,
            ..
        } => args
            .first()
            .map(|value| project_operand_scalar_ctx(value, ctx))
            .unwrap_or(Ok(None)),
        rumoca_core::Expression::FunctionCall { .. } => project_function_call_scalar(expr, ctx),
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_mul(op) => {
            if let Some(product) = project_tensor_product_scalar(lhs, rhs, *span, ctx)? {
                return Ok(Some(product));
            }
            if tensor_product_matches_result_shape(lhs, rhs, ctx)? {
                return Ok(None);
            }
            project_binary_elementwise_scalar(op.clone(), lhs, rhs, *span, ctx)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_add(op) || is_sub(op) => {
            project_binary_elementwise_scalar(op.clone(), lhs, rhs, *span, ctx)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_div(op) => {
            project_binary_elementwise_scalar(op.clone(), lhs, rhs, *span, ctx)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            span,
        } => project_if_scalar_expr(branches, else_branch, *span, ctx),
        _ => Ok(None),
    }
}

fn project_literal_array_scalar(
    elements: &[rumoca_core::Expression],
    flat_index: usize,
) -> Option<rumoca_core::Expression> {
    literal_array_elements_flat(elements)
        .get(flat_index)
        .cloned()
}

fn real_literal_expr(value: f64, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(value),
        span: expr.span().unwrap_or(rumoca_core::Span::DUMMY),
    }
}

fn project_function_call_scalar(
    expr: &rumoca_core::Expression,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let values = function_call_projected_scalars(expr, ctx.dae_model, ctx.structural_bindings)?;
    Ok(values.and_then(|values| values.get(ctx.flat_index).cloned()))
}

fn project_if_scalar_expr(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let branches = branches
        .iter()
        .map(|(condition, branch)| {
            let branch = project_branch_scalar_ctx(branch, ctx)?;
            Ok((condition.clone(), branch))
        })
        .collect::<Result<Vec<_>, LowerError>>()?;
    let else_branch = project_branch_scalar_ctx(else_branch, ctx)?;
    Ok(Some(rumoca_core::Expression::If {
        branches,
        else_branch: Box::new(else_branch),
        span,
    }))
}

fn project_branch_scalar_ctx(
    expr: &rumoca_core::Expression,
    ctx: &ProjectionContext<'_>,
) -> Result<rumoca_core::Expression, LowerError> {
    if let Some(projected) = project_operand_scalar_ctx(expr, ctx)? {
        return Ok(projected);
    }
    if expression_result_dims(expr, ctx.dae_model, ctx.structural_bindings)?.is_empty() {
        return Ok(expr.clone());
    }
    Err(LowerError::Unsupported {
        reason: "array derivative if branch could not be projected to a scalar".to_string(),
    })
}

fn project_operand_scalar_ctx(
    expr: &rumoca_core::Expression,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let expr_dims = expression_result_dims(expr, ctx.dae_model, ctx.structural_bindings)?;
    if expr_dims.is_empty() {
        return Ok(Some(expr.clone()));
    }
    if expr_dims != ctx.dims {
        return Ok(None);
    }
    project_expression_scalar_ctx(expr, ctx)
}

fn project_binary_elementwise_scalar(
    op: OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let Some(lhs) = project_operand_scalar_ctx(lhs, ctx)? else {
        return Ok(None);
    };
    let Some(rhs) = project_operand_scalar_ctx(rhs, ctx)? else {
        return Ok(None);
    };
    Ok(Some(rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }))
}

fn project_tensor_product_scalar(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let lhs_dims = expression_result_dims(lhs, ctx.dae_model, ctx.structural_bindings)?;
    let rhs_dims = expression_result_dims(rhs, ctx.dae_model, ctx.structural_bindings)?;
    match (lhs_dims.as_slice(), rhs_dims.as_slice(), ctx.dims) {
        ([rows, cols], [n], [_]) if cols == n => {
            project_matrix_vector_product_scalar(lhs, rhs, span, ctx, *rows, *cols)
        }
        ([n], [rows, cols], [_]) if n == rows => {
            project_vector_matrix_product_scalar(lhs, rhs, span, ctx, *rows, *cols)
        }
        ([rows, inner_lhs], [inner_rhs, cols], [out_rows, out_cols])
            if inner_lhs == inner_rhs && rows == out_rows && cols == out_cols =>
        {
            project_matrix_matrix_product_scalar(lhs, rhs, span, ctx, *inner_lhs, *cols)
        }
        _ => Ok(None),
    }
}

fn tensor_product_matches_result_shape(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    ctx: &ProjectionContext<'_>,
) -> Result<bool, LowerError> {
    let lhs_dims = expression_result_dims(lhs, ctx.dae_model, ctx.structural_bindings)?;
    let rhs_dims = expression_result_dims(rhs, ctx.dae_model, ctx.structural_bindings)?;
    Ok(matches!(
        (lhs_dims.as_slice(), rhs_dims.as_slice(), ctx.dims),
        ([_, cols], [n], [_])
            if cols == n
    ) || matches!(
        (lhs_dims.as_slice(), rhs_dims.as_slice(), ctx.dims),
        ([n], [rows, _], [_])
            if n == rows
    ) || matches!(
        (lhs_dims.as_slice(), rhs_dims.as_slice(), ctx.dims),
        ([rows, inner_lhs], [inner_rhs, cols], [out_rows, out_cols])
            if inner_lhs == inner_rhs && rows == out_rows && cols == out_cols
    ))
}

fn project_matrix_vector_product_scalar(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
    rows: usize,
    cols: usize,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    if ctx.flat_index >= rows {
        return Ok(None);
    }
    let lhs_dims = [rows, cols];
    let rhs_dims = [cols];

    let mut terms = Vec::with_capacity(cols);
    for col in 0..cols {
        let matrix_flat_index = ctx.flat_index * cols + col;
        let Some(lhs_term) = project_expression_scalar(
            lhs,
            &lhs_dims,
            matrix_flat_index,
            ctx.dae_model,
            ctx.structural_bindings,
        )?
        else {
            return Ok(None);
        };
        let Some(rhs_term) =
            project_expression_scalar(rhs, &rhs_dims, col, ctx.dae_model, ctx.structural_bindings)?
        else {
            return Ok(None);
        };
        terms.push(mul_with_span(lhs_term, rhs_term, span));
    }
    Ok(Some(sum_expressions(terms, span)))
}

fn project_vector_matrix_product_scalar(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
    rows: usize,
    cols: usize,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    if ctx.flat_index >= cols {
        return Ok(None);
    }
    let lhs_dims = [rows];
    let rhs_dims = [rows, cols];
    let col = ctx.flat_index;

    let mut terms = Vec::with_capacity(rows);
    for row in 0..rows {
        let Some(lhs_term) =
            project_expression_scalar(lhs, &lhs_dims, row, ctx.dae_model, ctx.structural_bindings)?
        else {
            return Ok(None);
        };
        let rhs_flat_index = row * cols + col;
        let Some(rhs_term) = project_expression_scalar(
            rhs,
            &rhs_dims,
            rhs_flat_index,
            ctx.dae_model,
            ctx.structural_bindings,
        )?
        else {
            return Ok(None);
        };
        terms.push(mul_with_span(lhs_term, rhs_term, span));
    }
    Ok(Some(sum_expressions(terms, span)))
}

fn project_matrix_matrix_product_scalar(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &ProjectionContext<'_>,
    inner: usize,
    cols: usize,
) -> Result<Option<rumoca_core::Expression>, LowerError> {
    let lhs_dims = expression_result_dims(lhs, ctx.dae_model, ctx.structural_bindings)?;
    let rhs_dims = expression_result_dims(rhs, ctx.dae_model, ctx.structural_bindings)?;
    let row = ctx.flat_index / cols;
    let col = ctx.flat_index % cols;

    let mut terms = Vec::with_capacity(inner);
    for inner_idx in 0..inner {
        let lhs_flat_index = row * inner + inner_idx;
        let rhs_flat_index = inner_idx * cols + col;
        let Some(lhs_term) = project_expression_scalar(
            lhs,
            &lhs_dims,
            lhs_flat_index,
            ctx.dae_model,
            ctx.structural_bindings,
        )?
        else {
            return Ok(None);
        };
        let Some(rhs_term) = project_expression_scalar(
            rhs,
            &rhs_dims,
            rhs_flat_index,
            ctx.dae_model,
            ctx.structural_bindings,
        )?
        else {
            return Ok(None);
        };
        terms.push(mul_with_span(lhs_term, rhs_term, span));
    }
    Ok(Some(sum_expressions(terms, span)))
}

pub(in crate::lower) fn expression_result_dims(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => expression_dims_for_subscripted_binding(
            name.as_str(),
            subscripts,
            dae_model,
            structural_bindings,
        ),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base = binding_base_name(base)?;
            expression_dims_for_subscripted_binding(
                &base,
                subscripts,
                dae_model,
                structural_bindings,
            )
        }
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            ..
        } => Ok(array_expression_dims(elements, *is_matrix)
            .into_iter()
            .filter_map(|dim| usize::try_from(dim).ok())
            .collect()),
        rumoca_core::Expression::Tuple { elements, .. } => Ok(vec![elements.len()]),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args,
            span,
        } => {
            let arg = args.first().ok_or_else(|| LowerError::ContractViolation {
                reason: "der() in derivative projection requires one argument".to_string(),
                span: *span,
            })?;
            expression_result_dims(arg, dae_model, structural_bindings)
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Zeros | rumoca_core::BuiltinFunction::Ones,
            args,
            ..
        } => builtin_size_args_dims(args, structural_bindings),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args,
            span,
        } => {
            let size_args = args
                .get(1..)
                .filter(|args| !args.is_empty())
                .ok_or_else(|| LowerError::ContractViolation {
                    reason:
                        "fill() in derivative projection requires at least one dimension argument"
                            .to_string(),
                    span: *span,
                })?;
            builtin_size_args_dims(size_args, structural_bindings)
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            expression_result_dims(rhs, dae_model, structural_bindings)
        }
        rumoca_core::Expression::FunctionCall {
            name,
            is_constructor: false,
            ..
        } => Ok(declared_function_output_dims(dae_model, name)
            .expect("function output dims must be declared before derivative projection")),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_mul(op) => {
            binary_mul_result_dims(lhs, rhs, dae_model, structural_bindings)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_add(op) || is_sub(op) => {
            binary_elementwise_result_dims(lhs, rhs, dae_model, structural_bindings)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_div(op) => {
            binary_elementwise_result_dims(lhs, rhs, dae_model, structural_bindings)
        }
        rumoca_core::Expression::If { else_branch, .. } => {
            expression_result_dims(else_branch, dae_model, structural_bindings)
        }
        _ => Ok(Vec::new()),
    }
}

pub(in crate::lower) fn builtin_size_args_dims(
    args: &[rumoca_core::Expression],
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    args.iter()
        .map(|arg| super::super::compile_time_index_expr(arg, structural_bindings))
        .collect()
}

pub(in crate::lower) fn expression_dims_for_subscripted_binding(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    if let Some(var) = variable_by_name(dae_model, base) {
        if var.dims.is_empty() {
            if subscripts.is_empty() {
                return Ok(Vec::new());
            }
            return Err(LowerError::Unsupported {
                reason: format!("scalar binding `{base}` was indexed"),
            });
        }
        return result_dims_for_subscripted_binding(
            base,
            subscripts,
            dae_model,
            structural_bindings,
        );
    }

    if subscripts.is_empty() {
        return Err(LowerError::MissingBinding {
            name: base.to_string(),
        });
    }
    let scalarized_key = scalarized_binding_key(base, subscripts, structural_bindings)?;
    if variable_by_name(dae_model, &scalarized_key).is_some() {
        return Ok(Vec::new());
    }
    Err(LowerError::MissingBinding {
        name: scalarized_key,
    })
}

pub(in crate::lower) fn declared_function_output_dims(
    dae_model: &dae::Dae,
    name: &rumoca_core::Reference,
) -> Option<Vec<usize>> {
    let function = dae_model.symbols.functions.get(name.var_name())?;
    let [output] = function.outputs.as_slice() else {
        return None;
    };
    output
        .dims
        .iter()
        .map(|dim| usize::try_from(*dim).ok())
        .collect::<Option<Vec<_>>>()
        .filter(|dims| !dims.is_empty())
}

pub(in crate::lower) fn binary_elementwise_result_dims(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    let lhs_dims = expression_result_dims(lhs, dae_model, structural_bindings)?;
    let rhs_dims = expression_result_dims(rhs, dae_model, structural_bindings)?;
    Ok(match (lhs_dims.is_empty(), rhs_dims.is_empty()) {
        (true, true) => Vec::new(),
        (true, false) => rhs_dims,
        (false, true) => lhs_dims,
        (false, false) if lhs_dims == rhs_dims => lhs_dims,
        _ => Vec::new(),
    })
}

pub(in crate::lower) fn binary_mul_result_dims(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    let lhs_dims = expression_result_dims(lhs, dae_model, structural_bindings)?;
    let rhs_dims = expression_result_dims(rhs, dae_model, structural_bindings)?;
    Ok(match (lhs_dims.as_slice(), rhs_dims.as_slice()) {
        ([], dims) if !dims.is_empty() => dims.to_vec(),
        (dims, []) if !dims.is_empty() => dims.to_vec(),
        ([lhs_rows, lhs_cols], [rhs_rows, rhs_cols]) if lhs_cols == rhs_rows => {
            vec![*lhs_rows, *rhs_cols]
        }
        ([rows, cols], [n]) if cols == n => vec![*rows],
        ([n], [rows, cols]) if n == rows => vec![*cols],
        ([n], [m]) if n == m => Vec::new(),
        _ if lhs_dims == rhs_dims => lhs_dims,
        _ => Vec::new(),
    })
}

pub(in crate::lower) fn expression_binding_keys(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Option<Vec<String>>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => Ok(Some(binding_keys_for_subscripted_name(
            name.as_str(),
            subscripts,
            dae_model,
            structural_bindings,
        )?)),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base = binding_base_name(base)?;
            Ok(Some(binding_keys_for_subscripted_name(
                &base,
                subscripts,
                dae_model,
                structural_bindings,
            )?))
        }
        _ => Ok(None),
    }
}

fn expression_binding_expressions(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } => Ok(Some(binding_expressions_for_subscripted_reference(
            name,
            subscripts,
            *span,
            dae_model,
            structural_bindings,
        )?)),
        rumoca_core::Expression::Index {
            base,
            subscripts,
            span,
        } => {
            let name = binding_base_reference(base)?;
            Ok(Some(binding_expressions_for_subscripted_reference(
                name,
                subscripts,
                *span,
                dae_model,
                structural_bindings,
            )?))
        }
        _ => Ok(None),
    }
}

fn binding_base_reference(
    expr: &rumoca_core::Expression,
) -> Result<&rumoca_core::Reference, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Ok(name),
        _ => Err(LowerError::Unsupported {
            reason: "unsupported sliced derivative binding base".to_string(),
        }),
    }
}

fn binding_expressions_for_subscripted_reference(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    if let Some(dims) = variable_dims(dae_model, name.as_str()) {
        let selections = if subscripts.is_empty() {
            dims.iter().map(|dim| (1..=*dim).collect()).collect()
        } else {
            slice_selections(subscripts, &dims, structural_bindings)?
        };
        let mut expressions = Vec::new();
        collect_slice_reference_expressions(
            name,
            span,
            &selections,
            0,
            &mut Vec::new(),
            &mut expressions,
        );
        return Ok(expressions);
    }

    if subscripts.is_empty() {
        let variable = variable_by_name(dae_model, name.as_str()).ok_or_else(|| {
            LowerError::MissingBinding {
                name: name.as_str().to_string(),
            }
        })?;
        return Ok(vec![dae_variable_ref_expr(
            name.as_str(),
            variable,
            span,
            Vec::new(),
        )?]);
    }

    let indices = compile_time_subscript_indices(subscripts, structural_bindings)?;
    let scalarized_key = dae::format_subscript_key(name.as_str(), &indices);
    let variable =
        variable_by_name(dae_model, &scalarized_key).ok_or_else(|| LowerError::MissingBinding {
            name: scalarized_key.clone(),
        })?;
    Ok(vec![dae_variable_ref_expr(
        &scalarized_key,
        variable,
        span,
        Vec::new(),
    )?])
}

fn collect_slice_reference_expressions(
    name: &rumoca_core::Reference,
    span: rumoca_core::Span,
    selections: &[Vec<usize>],
    depth: usize,
    current: &mut Vec<usize>,
    expressions: &mut Vec<rumoca_core::Expression>,
) {
    if depth == selections.len() {
        expressions.push(rumoca_core::Expression::VarRef {
            name: name.clone(),
            subscripts: current
                .iter()
                .map(|index| rumoca_core::Subscript::index(*index as i64, span))
                .collect(),
            span,
        });
        return;
    }
    for &index in &selections[depth] {
        current.push(index);
        collect_slice_reference_expressions(
            name,
            span,
            selections,
            depth + 1,
            current,
            expressions,
        );
        current.pop();
    }
}

fn dae_variable_ref_expr(
    key: &str,
    variable: &dae::Variable,
    span: rumoca_core::Span,
    subscripts: Vec<rumoca_core::Subscript>,
) -> Result<rumoca_core::Expression, LowerError> {
    let name = match variable.origin {
        dae::VariableOrigin::Generated => rumoca_core::Reference::generated(key),
        dae::VariableOrigin::Source => {
            let component_ref =
                variable
                    .component_ref
                    .clone()
                    .ok_or_else(|| LowerError::ContractViolation {
                        reason: format!(
                            "source DAE variable `{key}` lost structured component-reference metadata before derivative projection"
                        ),
                        span,
                    })?;
            rumoca_core::Reference::from_component_reference(component_ref)
        }
    };
    Ok(rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    })
}

pub(in crate::lower) fn binding_base_name(
    expr: &rumoca_core::Expression,
) -> Result<String, LowerError> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Ok(name.as_str().to_string()),
        _ => Err(LowerError::Unsupported {
            reason: "unsupported sliced derivative binding base".to_string(),
        }),
    }
}

pub(in crate::lower) fn binding_keys_for_subscripted_name(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<String>, LowerError> {
    let Some(dims) = variable_dims(dae_model, base) else {
        if subscripts.is_empty() {
            if variable_by_name(dae_model, base).is_some() {
                return Ok(vec![base.to_string()]);
            }
            return Err(LowerError::MissingBinding {
                name: base.to_string(),
            });
        }
        let scalarized_key = scalarized_binding_key(base, subscripts, structural_bindings)?;
        if variable_by_name(dae_model, &scalarized_key).is_some() {
            return Ok(vec![scalarized_key]);
        }
        return Err(LowerError::MissingBinding {
            name: scalarized_key,
        });
    };
    if subscripts.is_empty() {
        return Ok(scalar_keys_for_dims(base, &dims));
    }
    let selections = slice_selections(subscripts, &dims, structural_bindings)?;
    let mut keys = Vec::new();
    collect_slice_keys(base, &selections, 0, &mut Vec::new(), &mut keys);
    Ok(keys)
}

pub(in crate::lower) fn scalarized_binding_key(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    structural_bindings: &IndexMap<String, f64>,
) -> Result<String, LowerError> {
    let indices = compile_time_subscript_indices(subscripts, structural_bindings)?;
    Ok(dae::format_subscript_key(base, &indices))
}

pub(in crate::lower) fn variable_by_name<'a>(
    dae_model: &'a dae::Dae,
    base: &str,
) -> Option<&'a dae::Variable> {
    let name = rumoca_core::VarName::new(base);
    dae_model
        .variables
        .states
        .get(&name)
        .or_else(|| dae_model.variables.algebraics.get(&name))
        .or_else(|| dae_model.variables.outputs.get(&name))
        .or_else(|| dae_model.variables.inputs.get(&name))
        .or_else(|| dae_model.variables.parameters.get(&name))
        .or_else(|| dae_model.variables.constants.get(&name))
        .or_else(|| dae_model.variables.discrete_reals.get(&name))
        .or_else(|| dae_model.variables.discrete_valued.get(&name))
}

pub(in crate::lower) fn variable_dims(dae_model: &dae::Dae, base: &str) -> Option<Vec<usize>> {
    let var = variable_by_name(dae_model, base)?;
    if var.dims.is_empty() {
        return None;
    }
    var.dims
        .iter()
        .map(|dim| usize::try_from(*dim).ok())
        .collect()
}

pub(in crate::lower) fn scalar_keys_for_dims(base: &str, dims: &[usize]) -> Vec<String> {
    let dims_i64 = dims.iter().map(|dim| *dim as i64).collect::<Vec<_>>();
    let size = dims.iter().product::<usize>();
    (0..size)
        .filter_map(|flat_index| {
            dae::flat_index_to_subscripts(&dims_i64, flat_index)
                .map(|subscripts| dae::format_subscript_key(base, &subscripts))
        })
        .collect()
}

pub(in crate::lower) fn slice_selections(
    subscripts: &[rumoca_core::Subscript],
    dims: &[usize],
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<Vec<usize>>, LowerError> {
    if subscripts.len() > dims.len() {
        return Err(LowerError::Unsupported {
            reason: "array derivative slice has more subscripts than dimensions".to_string(),
        });
    }
    let mut selections = Vec::with_capacity(dims.len());
    for (idx, subscript) in subscripts.iter().enumerate() {
        selections.push(slice_subscript_indices(
            subscript,
            dims[idx],
            structural_bindings,
        )?);
    }
    for &dim in &dims[subscripts.len()..] {
        selections.push((1..=dim).collect());
    }
    Ok(selections)
}

pub(in crate::lower) fn slice_subscript_indices(
    subscript: &rumoca_core::Subscript,
    dim: usize,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    let indices = match subscript {
        rumoca_core::Subscript::Index { value, .. } if *value > 0 => vec![*value as usize],
        rumoca_core::Subscript::Expr { expr, .. } => slice_expr_indices(expr, structural_bindings)?,
        rumoca_core::Subscript::Colon { .. } => (1..=dim).collect(),
        _ => {
            return Err(LowerError::Unsupported {
                reason: "non-positive derivative slice subscript is unsupported".to_string(),
            });
        }
    };
    if indices.iter().all(|index| *index > 0 && *index <= dim) {
        Ok(indices)
    } else {
        Err(LowerError::Unsupported {
            reason: "derivative slice index is outside dimension bounds".to_string(),
        })
    }
}

pub(in crate::lower) fn slice_expr_indices(
    expr: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    match expr {
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => compile_time_positive_range(start, step.as_deref(), end, structural_bindings),
        _ => Ok(vec![super::super::compile_time_index_expr(
            expr,
            structural_bindings,
        )?]),
    }
}

pub(in crate::lower) fn compile_time_positive_range(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    let start = compile_time_integer(start, structural_bindings)?;
    let step = match step {
        Some(step) => compile_time_integer(step, structural_bindings)?,
        None => 1,
    };
    let end = compile_time_integer(end, structural_bindings)?;
    if step == 0 {
        return Err(LowerError::Unsupported {
            reason: "zero derivative slice range step is unsupported".to_string(),
        });
    }
    let mut values = Vec::new();
    let mut current = start;
    while range_step_keeps_going(current, end, step) {
        let value = usize::try_from(current).map_err(|_| LowerError::Unsupported {
            reason: "derivative slice range index must be positive".to_string(),
        })?;
        if value == 0 {
            return Err(LowerError::Unsupported {
                reason: "derivative slice range index must be positive".to_string(),
            });
        }
        values.push(value);
        if values.len() > 100_000 {
            return Err(LowerError::Unsupported {
                reason: "derivative slice range is too large".to_string(),
            });
        }
        current = current.saturating_add(step);
    }
    Ok(values)
}

pub(in crate::lower) fn range_step_keeps_going(current: i64, end: i64, step: i64) -> bool {
    if step > 0 {
        current <= end
    } else {
        current >= end
    }
}

pub(in crate::lower) fn compile_time_integer(
    expr: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<i64, LowerError> {
    let raw = super::super::compile_time_index_raw(expr, structural_bindings)?;
    let rounded = raw.round();
    if rounded.is_finite() && (rounded - raw).abs() < f64::EPSILON {
        return Ok(rounded as i64);
    }
    Err(LowerError::Unsupported {
        reason: "derivative slice range expression must be an integer".to_string(),
    })
}

pub(in crate::lower) fn compile_time_subscript_indices(
    subscripts: &[rumoca_core::Subscript],
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    subscripts
        .iter()
        .map(|subscript| super::super::compile_time_subscript_index(subscript, structural_bindings))
        .collect()
}

pub(in crate::lower) fn collect_slice_keys(
    base: &str,
    selections: &[Vec<usize>],
    depth: usize,
    current: &mut Vec<usize>,
    keys: &mut Vec<String>,
) {
    if depth == selections.len() {
        keys.push(dae::format_subscript_key(base, current));
        return;
    }
    for &index in &selections[depth] {
        current.push(index);
        collect_slice_keys(base, selections, depth + 1, current, keys);
        current.pop();
    }
}

pub(in crate::lower) fn literal_array_elements(
    expr: &rumoca_core::Expression,
) -> Option<Vec<rumoca_core::Expression>> {
    match expr {
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => Some(elements.clone()),
        _ => None,
    }
}

pub(in crate::lower) fn literal_array_elements_flat(
    elements: &[rumoca_core::Expression],
) -> Vec<rumoca_core::Expression> {
    let mut flattened = Vec::new();
    for element in elements {
        match element {
            rumoca_core::Expression::Array { elements: row, .. } => {
                flattened.extend(row.iter().cloned());
            }
            _ => flattened.push(element.clone()),
        }
    }
    flattened
}

pub(in crate::lower) fn indexed_rhs_expressions(
    expr: &rumoca_core::Expression,
    target: &rumoca_core::Expression,
    expected: usize,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    let dims = derivative_target_result_dims(target, dae_model, structural_bindings)?;
    if dims.iter().product::<usize>() != expected {
        return Err(LowerError::Unsupported {
            reason: "array derivative RHS shape does not match target shape".to_string(),
        });
    }
    let dims_i64 = dims.iter().map(|dim| *dim as i64).collect::<Vec<_>>();
    (0..expected)
        .map(|flat_index| {
            let Some(indices) = dae::flat_index_to_subscripts(&dims_i64, flat_index) else {
                return Err(LowerError::Unsupported {
                    reason: "array derivative RHS could not compute scalar subscripts".to_string(),
                });
            };
            Ok(rumoca_core::Expression::Index {
                base: Box::new(expr.clone()),
                subscripts: indices
                    .into_iter()
                    .map(|index| {
                        rumoca_core::Subscript::generated_index(
                            index as i64,
                            rumoca_core::Span::DUMMY,
                        )
                    })
                    .collect(),
                span: rumoca_core::Span::DUMMY,
            })
        })
        .collect()
}

pub(in crate::lower) fn derivative_target_result_dims(
    target: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    match target {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => result_dims_for_subscripted_binding(
            name.as_str(),
            subscripts,
            dae_model,
            structural_bindings,
        ),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let base = binding_base_name(base)?;
            result_dims_for_subscripted_binding(&base, subscripts, dae_model, structural_bindings)
        }
        _ => Err(LowerError::Unsupported {
            reason: "array derivative RHS target shape is unsupported".to_string(),
        }),
    }
}

pub(in crate::lower) fn result_dims_for_subscripted_binding(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Vec<usize>, LowerError> {
    let Some(dims) = variable_dims(dae_model, base) else {
        return Err(LowerError::Unsupported {
            reason: "array derivative RHS target has no array shape".to_string(),
        });
    };
    if subscripts.is_empty() {
        return Ok(dims);
    }
    let selections = slice_selections(subscripts, &dims, structural_bindings)?;
    Ok(selections
        .into_iter()
        .map(|selection| selection.len())
        .collect())
}

pub(in crate::lower) fn scalar_key_expr(key: String) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(key),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn expression_result_dims_rejects_der_without_argument() {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(11), 3, 8);
        let expr = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: Vec::new(),
            span,
        };

        let err = expression_result_dims(&expr, &dae::Dae::new(), &IndexMap::new())
            .expect_err("der() without an argument is malformed IR");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
    }

    #[test]
    fn expression_result_dims_rejects_fill_without_dimension_argument() {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(12), 5, 14);
        let expr = rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Fill,
            args: vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            }],
            span,
        };

        let err = expression_result_dims(&expr, &dae::Dae::new(), &IndexMap::new())
            .expect_err("fill() without dimension arguments is malformed IR");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
    }
}
