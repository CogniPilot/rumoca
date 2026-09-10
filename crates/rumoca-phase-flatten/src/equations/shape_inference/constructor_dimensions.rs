//! Exact dimensions of MLS §10.3.3 array constructors. Preserve every axis;
//! equation cardinality and the Flat tensor domain must use the same shape.

use super::{
    Context, ExpressionShape, ast, dims_for_shape, infer_component_ref_dims,
    infer_expression_shape, try_eval_integer_with_ctx,
};
use rumoca_core::BuiltinFunction;

pub(super) fn infer_constructor_dims(
    expression: &ast::Expression,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> Option<Vec<i64>> {
    if let ast::Expression::Parenthesized { inner, .. } = expression {
        return infer_constructor_dims(inner, prefix, ctx);
    }
    let ast::Expression::FunctionCall { comp, args, .. } = expression else {
        return None;
    };
    let function = ctx
        .predefined_intrinsics
        .array_constructor(comp.target_def_id())?;
    match (function, args.as_slice()) {
        (BuiltinFunction::Zeros | BuiltinFunction::Ones, [_, ..]) => {
            constructor_extents(args, prefix, ctx)
        }
        (BuiltinFunction::Identity, [extent]) => {
            let extent = constructor_extent(extent, prefix, ctx)?;
            Some(vec![extent, extent])
        }
        (BuiltinFunction::Linspace, [_, _, extent]) => {
            let extent = constructor_extent(extent, prefix, ctx)?;
            (extent >= 2).then(|| vec![extent])
        }
        (BuiltinFunction::Fill, [value, extents @ ..]) if !extents.is_empty() => {
            let mut dims = constructor_extents(extents, prefix, ctx)?;
            // fill repeats the whole value. Its axes follow the new axes,
            // even when one of the new extents is zero (MLS §§10.3.3, 10.7).
            dims.extend(fill_value_dims(value, prefix, ctx)?);
            Some(dims)
        }
        _ => None,
    }
}

fn constructor_extent(
    expression: &ast::Expression,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> Option<i64> {
    let extent = try_eval_integer_with_ctx(ctx, expression, prefix)?;
    (extent >= 0).then_some(extent)
}

fn constructor_extents(
    expressions: &[ast::Expression],
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> Option<Vec<i64>> {
    expressions
        .iter()
        .map(|expression| constructor_extent(expression, prefix, ctx))
        .collect()
}

fn fill_value_dims(
    expression: &ast::Expression,
    prefix: &ast::QualifiedName,
    ctx: &Context,
) -> Option<Vec<i64>> {
    if let Some(dims) = infer_constructor_dims(expression, prefix, ctx) {
        return Some(dims);
    }
    if let ast::Expression::ComponentReference(reference) = expression {
        return infer_component_ref_dims(reference, prefix, ctx);
    }
    match infer_expression_shape(expression, prefix, ctx) {
        ExpressionShape::Scalar => Some(Vec::new()),
        shape => dims_for_shape(shape),
    }
}
