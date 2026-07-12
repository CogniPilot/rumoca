use std::collections::HashMap;

use super::{
    Equation, Expression, ExpressionShape, IndexProjectionContext, Literal, OpBinary, OpUnary,
    Reference, ScalarizedLhsTarget, Span, StructuralError, Subscript, scalarization_var_ref_name,
};

pub(super) struct ScalarProjectionContext<'a> {
    pub(super) context_span: Option<Span>,
    pub(super) var_dims: &'a HashMap<String, Vec<i64>>,
    pub(super) var_spans: &'a HashMap<String, Span>,
    pub(super) structural_values: &'a HashMap<String, i64>,
    pub(super) complex_fields: &'a HashMap<String, [Option<String>; 2]>,
    pub(super) component_index_map: &'a HashMap<String, HashMap<usize, String>>,
    pub(super) function_output_index_map: &'a super::FunctionOutputProjectionMap,
    pub(super) function_output_dims_map: &'a HashMap<rumoca_core::FunctionInstanceId, Vec<i64>>,
    pub(super) dynamic_function_output_map: &'a HashMap<rumoca_core::FunctionInstanceId, String>,
    pub(super) record_field_projection_map: &'a super::RecordFieldProjectionMap,
    pub(super) expected_dims: Option<&'a [i64]>,
}

impl<'a> ScalarProjectionContext<'a> {
    pub(super) fn with_context_span(&self, span: Span) -> ScalarProjectionContext<'a> {
        ScalarProjectionContext {
            context_span: (!span.is_dummy()).then_some(span),
            var_dims: self.var_dims,
            var_spans: self.var_spans,
            structural_values: self.structural_values,
            complex_fields: self.complex_fields,
            component_index_map: self.component_index_map,
            function_output_index_map: self.function_output_index_map,
            function_output_dims_map: self.function_output_dims_map,
            dynamic_function_output_map: self.dynamic_function_output_map,
            record_field_projection_map: self.record_field_projection_map,
            expected_dims: self.expected_dims,
        }
    }

    pub(super) fn with_expected_dims(
        &self,
        dims: Option<&'a [i64]>,
    ) -> ScalarProjectionContext<'a> {
        ScalarProjectionContext {
            context_span: self.context_span,
            var_dims: self.var_dims,
            var_spans: self.var_spans,
            structural_values: self.structural_values,
            complex_fields: self.complex_fields,
            component_index_map: self.component_index_map,
            function_output_index_map: self.function_output_index_map,
            function_output_dims_map: self.function_output_dims_map,
            dynamic_function_output_map: self.dynamic_function_output_map,
            record_field_projection_map: self.record_field_projection_map,
            expected_dims: dims,
        }
    }

    fn index_context(&self, scalar_idx: usize) -> IndexProjectionContext<'_> {
        IndexProjectionContext {
            i: scalar_idx,
            context_span: self.context_span,
            var_dims: self.var_dims,
            var_spans: self.var_spans,
            structural_values: self.structural_values,
            complex_fields: self.complex_fields,
            component_index_map: self.component_index_map,
            function_output_index_map: self.function_output_index_map,
            function_output_dims_map: self.function_output_dims_map,
            dynamic_function_output_map: self.dynamic_function_output_map,
            record_field_projection_map: self.record_field_projection_map,
            expected_dims: self.expected_dims,
            allow_dynamic_function_projection: true,
        }
    }

    pub(super) fn project_index(
        &self,
        expr: &Expression,
        scalar_idx: usize,
    ) -> Result<Expression, StructuralError> {
        self.index_context(scalar_idx).project(expr)
    }

    pub(super) fn lower_scalar_linear_algebra(
        &self,
        expr: &Expression,
    ) -> Result<Expression, StructuralError> {
        self.index_context(1).lower_scalar_linear_algebra(expr)
    }

    pub(super) fn expression_shape(&self, expr: &Expression) -> ExpressionShape {
        self.index_context(1).expression_shape(expr)
    }
}

fn complex_zero(span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(0.0),
        span,
    }
}

fn complex_add(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn complex_sub(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn complex_mul(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn complex_div(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Div,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn var_ref_expr_with_span(name: &Reference, subscripts: &[Subscript], span: Span) -> Expression {
    Expression::VarRef {
        name: name.clone(),
        subscripts: subscripts.to_vec(),
        span,
    }
}

fn project_complex_var_ref(
    name: &Reference,
    subscripts: &[Subscript],
    field_idx: usize,
    span: Span,
    projection: &ScalarProjectionContext<'_>,
) -> Expression {
    if is_complex_field_scalar_name(name.as_str(), "re") {
        return if field_idx == 1 {
            var_ref_expr_with_span(name, subscripts, span)
        } else {
            complex_zero(span)
        };
    }
    if is_complex_field_scalar_name(name.as_str(), "im") {
        return if field_idx == 2 {
            var_ref_expr_with_span(name, subscripts, span)
        } else {
            complex_zero(span)
        };
    }
    if let Some(fields) = projection.complex_fields.get(name.as_str()) {
        let projected_name = match field_idx {
            1 => fields[0].as_ref(),
            2 => fields[1].as_ref(),
            _ => None,
        };
        if let Some(projected_name) = projected_name {
            return Expression::VarRef {
                name: rumoca_core::Reference::new(projected_name.clone()),
                subscripts: vec![],
                span,
            };
        }
    }
    if field_idx == 1 {
        var_ref_expr_with_span(name, subscripts, span)
    } else {
        complex_zero(span)
    }
}

pub(super) fn is_complex_field_scalar_name(name: &str, field: &str) -> bool {
    rumoca_core::top_level_path_ends_with(name, field)
}

fn project_complex_unary(
    expr: &Expression,
    op: &OpUnary,
    rhs: &Expression,
    span: Span,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    if matches!(op, OpUnary::Minus | OpUnary::DotMinus) {
        return Ok(Expression::Unary {
            op: op.clone(),
            rhs: Box::new(project_complex_component(rhs, field_idx, projection)?),
            span,
        });
    }
    if field_idx == 1 {
        Ok(expr.clone())
    } else {
        Ok(complex_zero(span))
    }
}

fn project_complex_mul_or_div(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    span: Span,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    let lhs_re = project_complex_component(lhs, 1, projection)?;
    let lhs_im = project_complex_component(lhs, 2, projection)?;
    let rhs_re = project_complex_component(rhs, 1, projection)?;
    let rhs_im = project_complex_component(rhs, 2, projection)?;

    if matches!(op, OpBinary::Mul | OpBinary::MulElem) {
        return Ok(if field_idx == 1 {
            complex_sub(
                complex_mul(lhs_re, rhs_re, span),
                complex_mul(lhs_im, rhs_im, span),
                span,
            )
        } else {
            complex_add(
                complex_mul(lhs_re, rhs_im, span),
                complex_mul(lhs_im, rhs_re, span),
                span,
            )
        });
    }

    let denom = complex_add(
        complex_mul(rhs_re.clone(), rhs_re.clone(), span),
        complex_mul(rhs_im.clone(), rhs_im.clone(), span),
        span,
    );
    if field_idx == 1 {
        Ok(complex_div(
            complex_add(
                complex_mul(lhs_re, rhs_re, span),
                complex_mul(lhs_im, rhs_im, span),
                span,
            ),
            denom,
            span,
        ))
    } else {
        Ok(complex_div(
            complex_sub(
                complex_mul(lhs_im, rhs_re, span),
                complex_mul(lhs_re, rhs_im, span),
                span,
            ),
            denom,
            span,
        ))
    }
}

fn project_complex_binary(
    expr: &Expression,
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    span: Span,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    if matches!(
        op,
        OpBinary::Add | OpBinary::AddElem | OpBinary::Sub | OpBinary::SubElem
    ) {
        return Ok(Expression::Binary {
            op: op.clone(),
            lhs: Box::new(project_complex_component(lhs, field_idx, projection)?),
            rhs: Box::new(project_complex_component(rhs, field_idx, projection)?),
            span,
        });
    }
    if matches!(
        op,
        OpBinary::Mul | OpBinary::MulElem | OpBinary::Div | OpBinary::DivElem
    ) {
        return project_complex_mul_or_div(op, lhs, rhs, span, field_idx, projection);
    }
    if field_idx == 1 {
        Ok(expr.clone())
    } else {
        Ok(complex_zero(span))
    }
}

fn project_constructor_component(
    expr: &Expression,
    name: &Reference,
    args: &[Expression],
    field_idx: usize,
    span: Span,
) -> Expression {
    if let Some(arg) = args.get(field_idx.saturating_sub(1)) {
        return arg.clone();
    }
    if field_idx == 2
        && args.len() == 1
        && rumoca_core::qualified_type_name_matches(name.as_str(), "Complex")
    {
        return complex_zero(span);
    }
    expr.clone()
}

fn project_function_call_component(
    expr: &Expression,
    name: &Reference,
    args: &[Expression],
    is_constructor: bool,
    span: Span,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    if is_constructor {
        return Ok(project_constructor_component(
            expr, name, args, field_idx, span,
        ));
    }
    let instance_id = name
        .resolved_function()
        .map(|resolved| resolved.instance_id)
        .ok_or_else(|| StructuralError::ContractViolation {
            reason: "function call lacks resolved instance identity".to_string(),
            span,
        })?;
    if let Some(by_index) = projection.function_output_index_map.get(&instance_id)
        && let Some(projected_output) = by_index.get(&field_idx)
    {
        let projected_name = name
            .with_appended_parts(projected_output, span)
            .ok_or_else(|| StructuralError::ContractViolation {
                reason: "projected function call lacks structured identity".to_string(),
                span,
            })?;
        return Ok(Expression::FunctionCall {
            name: projected_name,
            args: args.to_vec(),
            is_constructor: false,
            span,
        });
    }
    if field_idx == 1 {
        Ok(expr.clone())
    } else {
        Ok(complex_zero(span))
    }
}

fn project_if_component(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    span: Span,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    Ok(Expression::If {
        branches: branches
            .iter()
            .map(|(cond, val)| {
                Ok((
                    cond.clone(),
                    project_complex_component(val, field_idx, projection)?,
                ))
            })
            .collect::<Result<Vec<_>, StructuralError>>()?,
        else_branch: Box::new(project_complex_component(
            else_branch,
            field_idx,
            projection,
        )?),
        span,
    })
}

fn project_array_component(
    expr: &Expression,
    elements: &[Expression],
    is_matrix: bool,
    span: Span,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    if elements.len() == 1 {
        return project_complex_component(&elements[0], field_idx, projection);
    }
    if elements.is_empty() {
        return Ok(expr.clone());
    }
    Ok(Expression::Array {
        elements: elements
            .iter()
            .map(|element| project_complex_component(element, field_idx, projection))
            .collect::<Result<Vec<_>, _>>()?,
        is_matrix,
        span,
    })
}

fn project_complex_component(
    expr: &Expression,
    field_idx: usize,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    match expr {
        Expression::Literal { value: _, span } => {
            if field_idx == 1 {
                Ok(expr.clone())
            } else {
                Ok(complex_zero(*span))
            }
        }
        Expression::VarRef {
            name,
            subscripts,
            span,
        } => Ok(project_complex_var_ref(
            name, subscripts, field_idx, *span, projection,
        )),
        Expression::Unary { op, rhs, span } => {
            project_complex_unary(expr, op, rhs, *span, field_idx, projection)
        }
        Expression::Binary { op, lhs, rhs, span } => {
            project_complex_binary(expr, op, lhs, rhs, *span, field_idx, projection)
        }
        Expression::If {
            branches,
            else_branch,
            span,
        } => project_if_component(branches, else_branch, *span, field_idx, projection),
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } => project_function_call_component(
            expr,
            name,
            args,
            *is_constructor,
            *span,
            field_idx,
            projection,
        ),
        Expression::Array {
            elements,
            is_matrix,
            span,
        } => project_array_component(expr, elements, *is_matrix, *span, field_idx, projection),
        _ => projection.project_index(expr, field_idx),
    }
}

pub(super) fn project_rhs_for_scalar_target(
    rhs: &Expression,
    scalar_idx: usize,
    lhs_target: Option<&str>,
    target: Option<&ScalarizedLhsTarget>,
    span: Span,
    projection: &ScalarProjectionContext<'_>,
) -> Result<Expression, StructuralError> {
    if let (Some(lhs_name), Some(target)) = (lhs_target, target)
        && (target.array_selector.is_some() || target.field_selector.is_some())
    {
        if let Expression::Binary {
            op,
            lhs,
            rhs: row_rhs,
            ..
        } = rhs
            && matches!(op, OpBinary::Sub)
            && let Expression::VarRef {
                name, subscripts, ..
            } = lhs.as_ref()
            && scalarization_var_ref_name(name, subscripts)
                .as_deref()
                .is_some_and(|lhs_row_name| lhs_row_name == lhs_name)
        {
            let mut projected_rhs = (*row_rhs.clone()).clone();
            if let Some(idx) = target.array_selector {
                projected_rhs = projection.project_index(&projected_rhs, idx)?;
            }
            if let Some(field_idx) = target.field_selector {
                projected_rhs = project_complex_component(&projected_rhs, field_idx, projection)?;
            }
            return Ok(Expression::Binary {
                op: op.clone(),
                lhs: Box::new(target.expr.clone()),
                rhs: Box::new(projected_rhs),
                span,
            });
        }

        let mut projected = rhs.clone();
        if let Some(idx) = target.array_selector {
            projected = projection.project_index(&projected, idx)?;
        }
        if let Some(field_idx) = target.field_selector {
            projected = project_complex_component(&projected, field_idx, projection)?;
        }
        return Ok(projected);
    }

    projection.project_index(rhs, scalar_idx)
}

pub(super) fn scalarized_equation_lhs(
    eq: &Equation,
    target: Option<&ScalarizedLhsTarget>,
    scalar_idx: usize,
    span: Span,
) -> Result<Option<rumoca_core::Reference>, StructuralError> {
    let Some(lhs) = eq.lhs.as_ref() else {
        return Ok(None);
    };
    if let Some(name) = target {
        return Ok(Some(structured_scalar_target(
            &rumoca_core::VarName::new(name.name.clone()),
            span,
        )));
    }
    let indexed = lhs.with_appended_index(
        scalar_idx as i64,
        scalarized_lhs_index_owner_span(lhs, span)?,
    );
    if indexed.component_ref().is_some() {
        return Ok(Some(indexed));
    }
    Ok(Some(structured_scalar_target(
        &rumoca_core::VarName::new(indexed.as_str().to_string()),
        span,
    )))
}

fn scalarized_lhs_index_owner_span(
    lhs: &rumoca_core::Reference,
    equation_span: Span,
) -> Result<rumoca_core::ProvenanceSpan, StructuralError> {
    let Some(span) = lhs
        .span()
        .or_else(|| (!equation_span.is_dummy()).then_some(equation_span))
    else {
        return Err(StructuralError::UnspannedContractViolation {
            reason: format!(
                "cannot append scalarized index to `{}` without source provenance on the LHS reference or equation",
                lhs.as_str()
            ),
        });
    };
    span.require_provenance("structural scalarized equation LHS subscript")
        .map_err(|err| StructuralError::UnspannedContractViolation {
            reason: err.to_string(),
        })
}

/// Structured reference for a rendered scalar target (the target names come
/// from the scalarized output table, so their subscripts are static).
fn structured_scalar_target(name: &rumoca_core::VarName, span: Span) -> rumoca_core::Reference {
    match rumoca_core::component_reference_from_flat_name(name, span) {
        Some(component_ref) => {
            rumoca_core::Reference::with_component_reference(name.as_str(), component_ref)
        }
        None => rumoca_core::Reference::from_var_name(name.clone()),
    }
}

pub(super) fn lower_scalar_linear_algebra_exprs(
    exprs: &mut [Expression],
    projection: &ScalarProjectionContext<'_>,
) -> Result<(), StructuralError> {
    for expr in exprs {
        *expr = projection.lower_scalar_linear_algebra(expr)?;
    }
    Ok(())
}
