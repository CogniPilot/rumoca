//! Preserve fixed-shape conditional equation tuples inside symbolic families.

mod calls;
#[cfg(test)]
mod tests;

use super::*;

#[cfg(test)]
thread_local! {
    static ENABLED: std::cell::Cell<bool> = const { std::cell::Cell::new(true) };
}

struct CaptureContext<'a> {
    ctx: &'a Context,
    prefix: &'a ast::QualifiedName,
    def_map: Option<&'a crate::ResolveDefMap>,
    locals: &'a HashSet<String>,
}

struct Residual {
    expression: rumoca_core::Expression,
    shape: ExpressionShape,
}

pub(super) fn capture(
    ctx: &Context,
    equation: &ast::Equation,
    prefix: &ast::QualifiedName,
    def_map: Option<&crate::ResolveDefMap>,
    locals: &HashSet<String>,
) -> Option<Vec<rumoca_core::Expression>> {
    #[cfg(test)]
    if !ENABLED.get() {
        return None;
    }
    let context = CaptureContext {
        ctx,
        prefix,
        def_map,
        locals,
    };
    Some(
        conditional(&context, equation)?
            .into_iter()
            .map(|value| value.expression)
            .collect(),
    )
}

fn conditional(context: &CaptureContext<'_>, equation: &ast::Equation) -> Option<Vec<Residual>> {
    let ast::Equation::If {
        cond_blocks,
        else_block,
    } = equation
    else {
        return None;
    };
    let fallback = residuals(context, else_block.as_deref()?)?;
    let mut branches = Vec::with_capacity(cond_blocks.len());
    for block in cond_blocks {
        if template_domains::has_binder_dependent_domain(&block.cond, context.locals) {
            return None;
        }
        let values = residuals(context, &block.eqs)?;
        if !same_shapes(&fallback, &values) {
            return None;
        }
        let condition = qualify_expression_imports_with_def_map_ctx(
            &block.cond,
            context.prefix,
            &context.ctx.current_imports,
            context.def_map,
            context.ctx,
            Some(context.locals),
        )
        .ok()?;
        branches.push((condition, values));
    }
    if branches.is_empty() {
        return None;
    }
    fallback
        .into_iter()
        .enumerate()
        .map(|(ordinal, fallback)| {
            let span = fallback.expression.span()?;
            Some(Residual {
                shape: fallback.shape,
                expression: rumoca_core::Expression::If {
                    branches: branches
                        .iter()
                        .map(|(condition, values)| {
                            (condition.clone(), values[ordinal].expression.clone())
                        })
                        .collect(),
                    else_branch: Box::new(fallback.expression),
                    span,
                },
            })
        })
        .collect()
}

fn same_shapes(left: &[Residual], right: &[Residual]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| left.shape == right.shape)
}

fn residuals(context: &CaptureContext<'_>, equations: &[ast::Equation]) -> Option<Vec<Residual>> {
    let mut result = Vec::new();
    for equation in equations {
        match equation {
            ast::Equation::Simple { lhs, rhs } => result.push(simple(context, lhs, rhs)?),
            ast::Equation::If { .. } => result.extend(conditional(context, equation)?),
            _ => return None,
        }
    }
    Some(result)
}

fn simple(
    context: &CaptureContext<'_>,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
) -> Option<Residual> {
    if template_domains::has_binder_dependent_domain(lhs, context.locals)
        || template_domains::has_binder_dependent_domain(rhs, context.locals)
    {
        return None;
    }
    let lhs_shape = shape_inference::infer_expression_shape(lhs, context.prefix, context.ctx);
    let rhs_shape = shape_inference::infer_expression_shape(rhs, context.prefix, context.ctx);
    let shape = match (lhs_shape, rhs_shape) {
        (ExpressionShape::Other, _) => return None,
        (shape, ExpressionShape::Other)
            if calls::fixed_scalar(context, rhs) && shape == ExpressionShape::Scalar =>
        {
            shape
        }
        (lhs, rhs) if lhs == rhs => lhs,
        _ => return None,
    };
    if matches!(shape, ExpressionShape::Other) {
        return None;
    }
    Some(Residual {
        expression: make_residual(
            context.ctx,
            lhs,
            rhs,
            context.prefix,
            context.def_map,
            Some(context.locals),
        )
        .ok()?,
        shape,
    })
}
