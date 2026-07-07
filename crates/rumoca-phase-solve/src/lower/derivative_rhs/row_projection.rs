use super::*;

pub(super) fn project_derivative_row_expr(
    builder: &LowerBuilder,
    expr: &rumoca_core::Expression,
    row_index: usize,
    row_count: usize,
    source_context_span: rumoca_core::Span,
    scope: &Scope,
) -> Result<rumoca_core::Expression, LowerError> {
    let dims = builder.infer_expr_dims(expr, scope)?;
    if dims.is_empty() {
        return Ok(expr.clone());
    }
    let span = expr.span().unwrap_or(source_context_span);
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } if subscripts.is_empty() && dims.as_slice() == [row_count] => {
            let index = checked_usize_to_i64(
                row_index + 1,
                "derivative row projection subscript",
                source_context_span,
            )?;
            let subscript = rumoca_core::Subscript::try_generated_index(
                index,
                *span,
                "derivative row projection subscript",
            )?;
            Ok(rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: vec![subscript],
                span: *span,
            })
        }
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args,
            span,
        } => {
            let [arg] = args.as_slice() else {
                return Ok(expr.clone());
            };
            Ok(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args: vec![project_derivative_row_expr(
                    builder,
                    arg,
                    row_index,
                    row_count,
                    source_context_span,
                    scope,
                )?],
                span: *span,
            })
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, span } => {
            Ok(rumoca_core::Expression::Binary {
                op: op.clone(),
                lhs: Box::new(project_derivative_row_expr(
                    builder,
                    lhs,
                    row_index,
                    row_count,
                    source_context_span,
                    scope,
                )?),
                rhs: Box::new(project_derivative_row_expr(
                    builder,
                    rhs,
                    row_index,
                    row_count,
                    source_context_span,
                    scope,
                )?),
                span: *span,
            })
        }
        rumoca_core::Expression::Unary { op, rhs, span } => Ok(rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(project_derivative_row_expr(
                builder,
                rhs,
                row_index,
                row_count,
                source_context_span,
                scope,
            )?),
            span: *span,
        }),
        _ if dims.as_slice() == [row_count] => {
            let index =
                checked_usize_to_i64(row_index + 1, "derivative row projection subscript", span)?;
            let subscript = rumoca_core::Subscript::try_generated_index(
                index,
                span,
                "derivative row projection subscript",
            )?;
            Ok(rumoca_core::Expression::Index {
                base: Box::new(expr.clone()),
                subscripts: vec![subscript],
                span,
            })
        }
        _ => Ok(expr.clone()),
    }
}
