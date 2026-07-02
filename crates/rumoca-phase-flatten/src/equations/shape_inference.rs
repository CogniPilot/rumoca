//! Expression-shape (scalar vs array) inference for equation flattening: deriving
//! the scalar element count of an equation's RHS so the flattener can scalarize
//! array equations. Split out of `equations/mod.rs` to keep that module under the
//! SPEC_0021 size limit.

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ExpressionShape {
    Scalar,
    Vector(i64),
    Matrix(i64, i64),
    Other,
}

pub(crate) fn expression_shape_from_dims(dims: &[i64]) -> ExpressionShape {
    match dims {
        [] => ExpressionShape::Scalar,
        [n] => ExpressionShape::Vector((*n).max(0)),
        [r, c] => ExpressionShape::Matrix((*r).max(0), (*c).max(0)),
        _ => ExpressionShape::Other,
    }
}

pub(crate) fn infer_component_ref_shape(
    cr: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> ExpressionShape {
    let qualified = build_qualified_name(prefix, cr);
    // Exact lookup already includes any expanded parent indices in the key
    // (e.g. `medium_T[2].state.X`), so dims are already projected.
    if let Some(dims) = ctx.get_array_dimensions(&qualified) {
        return expression_shape_from_dims(dims);
    }

    // Fall back to the unscripted path and project by subscripts in the reference.
    // This handles references like `A[i]` when only `A` has known dimensions.
    let unscripted = strip_subscripts_from_component_ref(cr);
    let qualified_unscripted = build_qualified_name(prefix, &unscripted);
    let Some(dims) = ctx.get_array_dimensions(&qualified_unscripted) else {
        return ExpressionShape::Scalar;
    };
    expression_shape_from_dims(&project_component_dims_by_subscripts(dims, cr))
}

pub(crate) fn strip_subscripts_from_component_ref(
    cr: &ast::ComponentReference,
) -> ast::ComponentReference {
    let mut stripped = cr.clone();
    for part in &mut stripped.parts {
        part.subs = None;
    }
    stripped
}

pub(crate) fn project_component_dims_by_subscripts(
    dims: &[i64],
    cr: &ast::ComponentReference,
) -> Vec<i64> {
    if dims.is_empty() {
        return Vec::new();
    }

    let mut remaining_dims = Vec::new();
    let mut dim_idx = 0usize;

    for part in &cr.parts {
        let Some(subs) = &part.subs else {
            continue;
        };
        for sub in subs {
            if dim_idx >= dims.len() {
                break;
            }
            match sub {
                ast::Subscript::Expression(_) => {
                    dim_idx += 1;
                }
                ast::Subscript::Range { .. } => {
                    remaining_dims.push(dims[dim_idx]);
                    dim_idx += 1;
                }
                ast::Subscript::Empty => {}
            }
        }
    }

    remaining_dims.extend_from_slice(&dims[dim_idx..]);
    remaining_dims
}

pub(crate) fn combine_additive_shapes(
    lhs: ExpressionShape,
    rhs: ExpressionShape,
) -> ExpressionShape {
    match (lhs, rhs) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Vector(n)) => ExpressionShape::Vector(n),
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Matrix(r, c)) => ExpressionShape::Matrix(r, c),
        (ExpressionShape::Vector(a), ExpressionShape::Vector(b)) if a == b => {
            ExpressionShape::Vector(a)
        }
        (ExpressionShape::Matrix(r1, c1), ExpressionShape::Matrix(r2, c2))
            if r1 == r2 && c1 == c2 =>
        {
            ExpressionShape::Matrix(r1, c1)
        }
        _ => ExpressionShape::Other,
    }
}

pub(crate) fn combine_mul_shapes(lhs: ExpressionShape, rhs: ExpressionShape) -> ExpressionShape {
    match (lhs, rhs) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Vector(n)) => ExpressionShape::Vector(n),
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Matrix(r, c)) => ExpressionShape::Matrix(r, c),
        // Modelica vector * vector is dot-product.
        (ExpressionShape::Vector(a), ExpressionShape::Vector(b)) if a == b => {
            ExpressionShape::Scalar
        }
        (ExpressionShape::Vector(v), ExpressionShape::Matrix(r, c)) if v == r => {
            ExpressionShape::Vector(c)
        }
        (ExpressionShape::Matrix(r, c), ExpressionShape::Vector(v)) if c == v => {
            ExpressionShape::Vector(r)
        }
        (ExpressionShape::Matrix(r1, c1), ExpressionShape::Matrix(r2, c2)) if c1 == r2 => {
            ExpressionShape::Matrix(r1, c2)
        }
        _ => ExpressionShape::Other,
    }
}

pub(crate) fn combine_elementwise_shapes(
    lhs: ExpressionShape,
    rhs: ExpressionShape,
) -> ExpressionShape {
    match (lhs, rhs) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Vector(n)) => ExpressionShape::Vector(n),
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Matrix(r, c)) => ExpressionShape::Matrix(r, c),
        (ExpressionShape::Vector(a), ExpressionShape::Vector(b)) if a == b => {
            ExpressionShape::Vector(a)
        }
        (ExpressionShape::Matrix(r1, c1), ExpressionShape::Matrix(r2, c2))
            if r1 == r2 && c1 == c2 =>
        {
            ExpressionShape::Matrix(r1, c1)
        }
        _ => ExpressionShape::Other,
    }
}

pub(crate) fn infer_scalar_rhs_shape(
    lhs_shape: ExpressionShape,
    rhs_shape: ExpressionShape,
) -> ExpressionShape {
    match (lhs_shape, rhs_shape) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => ExpressionShape::Scalar,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar) => ExpressionShape::Vector(n),
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar) => ExpressionShape::Matrix(r, c),
        _ => ExpressionShape::Other,
    }
}

pub(crate) fn infer_binary_shape(
    op: &rumoca_core::OpBinary,
    lhs_shape: ExpressionShape,
    rhs_shape: ExpressionShape,
) -> ExpressionShape {
    match op {
        rumoca_core::OpBinary::Add
        | rumoca_core::OpBinary::Sub
        | rumoca_core::OpBinary::AddElem
        | rumoca_core::OpBinary::SubElem => combine_additive_shapes(lhs_shape, rhs_shape),
        rumoca_core::OpBinary::Mul => combine_mul_shapes(lhs_shape, rhs_shape),
        rumoca_core::OpBinary::MulElem
        | rumoca_core::OpBinary::DivElem
        | rumoca_core::OpBinary::ExpElem => combine_elementwise_shapes(lhs_shape, rhs_shape),
        rumoca_core::OpBinary::Div | rumoca_core::OpBinary::Exp => {
            infer_scalar_rhs_shape(lhs_shape, rhs_shape)
        }
        rumoca_core::OpBinary::Eq
        | rumoca_core::OpBinary::Neq
        | rumoca_core::OpBinary::Lt
        | rumoca_core::OpBinary::Le
        | rumoca_core::OpBinary::Gt
        | rumoca_core::OpBinary::Ge
        | rumoca_core::OpBinary::And
        | rumoca_core::OpBinary::Or
        | rumoca_core::OpBinary::Assign
        | rumoca_core::OpBinary::Empty => ExpressionShape::Scalar,
    }
}

pub(crate) fn infer_expression_shape(
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> ExpressionShape {
    match expr {
        ast::Expression::Terminal { .. } => ExpressionShape::Scalar,
        ast::Expression::ComponentReference(cr) => infer_component_ref_shape(cr, prefix, ctx),
        ast::Expression::Unary { rhs, .. } | ast::Expression::Parenthesized { inner: rhs, .. } => {
            infer_expression_shape(rhs, prefix, ctx)
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs_shape = infer_expression_shape(lhs, prefix, ctx);
            let rhs_shape = infer_expression_shape(rhs, prefix, ctx);
            infer_binary_shape(op, lhs_shape, rhs_shape)
        }
        ast::Expression::FunctionCall { comp, .. } => {
            if is_reduction_operator(comp) {
                ExpressionShape::Scalar
            } else {
                ExpressionShape::Other
            }
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let else_shape = infer_expression_shape(else_branch, prefix, ctx);
            if branches
                .iter()
                .all(|(_, expr)| infer_expression_shape(expr, prefix, ctx) == else_shape)
            {
                else_shape
            } else {
                ExpressionShape::Other
            }
        }
        ast::Expression::Array {
            elements,
            is_matrix,
            ..
        } => {
            if *is_matrix {
                ExpressionShape::Other
            } else if elements
                .iter()
                .all(|e| infer_expression_shape(e, prefix, ctx) == ExpressionShape::Scalar)
            {
                ExpressionShape::Vector(elements.len() as i64)
            } else {
                ExpressionShape::Other
            }
        }
        ast::Expression::Range { .. }
        | ast::Expression::FieldAccess { .. }
        | ast::Expression::Tuple { .. }
        | ast::Expression::ArrayComprehension { .. }
        | ast::Expression::ArrayIndex { .. }
        | ast::Expression::NamedArgument { .. }
        | ast::Expression::Modification { .. }
        | ast::Expression::ClassModification { .. }
        | ast::Expression::Empty { .. } => ExpressionShape::Other,
    }
}

pub(crate) fn shape_scalar_size(shape: ExpressionShape) -> Option<usize> {
    match shape {
        ExpressionShape::Scalar => Some(1),
        ExpressionShape::Vector(n) => Some(n.max(0) as usize),
        ExpressionShape::Matrix(r, c) => {
            Some((r.max(0) as usize).saturating_mul(c.max(0) as usize))
        }
        ExpressionShape::Other => None,
    }
}

pub(crate) fn dims_scalar_size(dims: &[i64]) -> usize {
    if dims.is_empty() {
        1
    } else {
        dims.iter()
            .fold(1usize, |acc, d| acc.saturating_mul((*d).max(0) as usize))
    }
}

/// Infer scalar equation count for a simple equation without expanding it.
///
/// This preserves array equations as single residuals while still carrying scalar
/// size information for balance checking.
pub(crate) fn infer_simple_equation_scalar_count(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> usize {
    let lhs_shape = infer_expression_shape(lhs, prefix, ctx);
    let rhs_shape = infer_expression_shape(rhs, prefix, ctx);

    match (lhs_shape, rhs_shape) {
        (ExpressionShape::Scalar, ExpressionShape::Scalar) => return 1,
        (ExpressionShape::Vector(n), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Vector(n)) => return n.max(0) as usize,
        (ExpressionShape::Matrix(r, c), ExpressionShape::Scalar)
        | (ExpressionShape::Scalar, ExpressionShape::Matrix(r, c)) => {
            return (r.max(0) as usize).saturating_mul(c.max(0) as usize);
        }
        (ExpressionShape::Vector(a), ExpressionShape::Vector(b)) if a == b => {
            return a.max(0) as usize;
        }
        (ExpressionShape::Matrix(r1, c1), ExpressionShape::Matrix(r2, c2))
            if r1 == r2 && c1 == c2 =>
        {
            return (r1.max(0) as usize).saturating_mul(c1.max(0) as usize);
        }
        _ => {}
    }

    // Prefer LHS shape when available (equation result shape is usually driven by LHS).
    if let Some(size) = shape_scalar_size(lhs_shape)
        && size != 1
    {
        return size;
    }
    if let Some(size) = shape_scalar_size(rhs_shape)
        && size != 1
    {
        return size;
    }

    // Fallback to array-reference dimensional metadata.
    if let Some(array_ref) = find_array_refs_needing_expansion(lhs, prefix, ctx).first() {
        return dims_scalar_size(&array_ref.dims);
    }
    1
}
