//! Type helper functions for type checking.

use std::collections::HashMap;

use crate::ir::ast::{Expression, Subscript};

use crate::ir::analysis::type_inference::SymbolType;

/// Check if shape_expr contains any Range subscripts (`:`)
/// When `:` is used, the dimension is inferred from the binding
pub(super) fn has_inferred_dimensions(shape_expr: &[Subscript]) -> bool {
    shape_expr
        .iter()
        .any(|s| matches!(s, Subscript::Range { .. }))
}

/// Build an array type from a base type and shape dimensions
pub(super) fn build_array_type(base: SymbolType, shape: &[usize]) -> SymbolType {
    if shape.is_empty() {
        return base;
    }
    // Build array type from innermost to outermost
    let mut result = base;
    for &dim in shape.iter().rev() {
        result = SymbolType::Array(Box::new(result), Some(dim));
    }
    result
}

/// Get array dimensions from a type
pub(super) fn get_array_dimensions(ty: &SymbolType) -> Vec<usize> {
    let mut dims = Vec::new();
    let mut current = ty;
    while let SymbolType::Array(inner, size) = current {
        if let Some(s) = size {
            dims.push(*s);
        }
        current = inner;
    }
    dims
}

/// Infer the shape of an array expression from its structure.
///
/// For array literals like `{1, 2, 3}`, returns `[3]`.
/// For nested arrays like `{{1, 2}, {3, 4}}`, returns `[2, 2]`.
/// For component references, looks up the shape from the provided map.
/// For subscripted expressions like `x[1]`, removes the indexed dimension.
pub(super) fn infer_expression_shape(
    expr: &Expression,
    param_shapes: &HashMap<String, Vec<usize>>,
) -> Option<Vec<usize>> {
    match expr {
        Expression::Array { elements, .. } => {
            if elements.is_empty() {
                return Some(vec![0]);
            }
            // Get the shape of the first element
            let inner_shape = infer_expression_shape(&elements[0], param_shapes);
            // Prepend the number of elements
            let mut shape = vec![elements.len()];
            if let Some(inner) = inner_shape {
                shape.extend(inner);
            }
            Some(shape)
        }
        Expression::ComponentReference(comp_ref) => {
            if let Some(first) = comp_ref.parts.first() {
                let name = &first.ident.text;
                if let Some(base_shape) = param_shapes.get(name) {
                    // Check for subscripts
                    if let Some(subs) = &first.subs {
                        // Each scalar subscript removes one dimension
                        let num_scalar_subs = subs
                            .iter()
                            .filter(|s| matches!(s, Subscript::Expression(_)))
                            .count();
                        if num_scalar_subs <= base_shape.len() {
                            return Some(base_shape[num_scalar_subs..].to_vec());
                        }
                    }
                    return Some(base_shape.clone());
                }
            }
            None
        }
        Expression::Terminal { .. } => Some(vec![]), // Scalar
        _ => None,
    }
}
