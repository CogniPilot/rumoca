//! Array-comprehension and component-array-dimension expansion helpers (MLS §10.4/§10.5).

use crate::equations::{expand_range_indices, substitute_index_in_expression};
use crate::pipeline::{
    ComponentOverrideMap, ImportCaches, OverlayScopeIndex, variable_import_context_for_instance,
};
use crate::qualify::{ImportMap, QualifyOptions, qualify_expression_with_imports};
use crate::{Context, FlattenError, ast_lower, variables};
use rumoca_eval_flat::phase_constant::infer_array_dimensions;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

/// Extract array dimensions from overlay components for connector arrays.
///
/// This enables equation expansion for vectorized equations like:
/// `v = plug_p.pin.v - plug_n.pin.v` where `plug_p.pin` is an array of connectors.
///
/// The flat model stores individual scalar variables like `plug_p.pin[1].v`,
/// but we need the dimension of `plug_p.pin` (size 3) to expand the equation.
pub(crate) fn extract_component_array_dimensions(
    ctx: &mut Context,
    overlay: &ast::InstanceOverlay,
) {
    for (_def_id, instance_data) in &overlay.components {
        // Only process components with non-empty dimensions.
        if instance_data.dims.is_empty() {
            continue;
        }

        let name = instance_data.qualified_name.to_flat_string();

        // Don't overwrite existing dimensions (might be more accurate from flat model).
        if ctx.array_dimensions.contains_key(&name) {
            continue;
        }

        ctx.array_dimensions
            .insert(name, instance_data.dims.clone());
    }

    // Also include array parent dimensions from expanded array components.
    // When plug_p.pin[3] is expanded to plug_p.pin[1], plug_p.pin[2], plug_p.pin[3],
    // the parent path "plug_p.pin" is registered with dimensions [3] for use
    // in array equation expansion (MLS §10.5).
    for (name, dims) in &overlay.array_parent_dims {
        if !ctx.array_dimensions.contains_key(name) {
            ctx.array_dimensions.insert(name.clone(), dims.clone());
        }
    }
}

pub(crate) fn has_expandable_array_comprehension_bindings(overlay: &ast::InstanceOverlay) -> bool {
    overlay.components.values().any(|instance_data| {
        matches!(
            instance_data.binding,
            Some(ast::Expression::ArrayComprehension { filter: None, .. })
        )
    })
}

/// Reconcile array-comprehension bindings once parameter context is available.
///
/// Policy:
/// 1. Keep structural `Expression::ArrayComprehension` when dimensions can be
///    inferred without scalar expansion.
/// 2. Fall back to eager expansion only when structural inference is insufficient.
///
/// MLS §10.4.1: Array comprehensions with parameter-dependent ranges.
pub(crate) fn expand_array_comprehension_bindings(
    ctx: &mut Context,
    flat: &mut flat::Model,
    overlay: &ast::InstanceOverlay,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    component_override_map: &ComponentOverrideMap,
) -> Result<bool, FlattenError> {
    let def_map = &tree.def_map;
    let scope_index = OverlayScopeIndex::new(overlay);
    let mut import_cache = ImportCaches::default();
    let mut changed_binding = false;

    for (_def_id, instance_data) in &overlay.components {
        let Some(binding) = &instance_data.binding else {
            continue;
        };
        let ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            span,
        } = binding
        else {
            continue;
        };

        if filter.is_some() {
            continue;
        }

        let var_name = rumoca_core::VarName::new(instance_data.qualified_name.to_flat_string());
        let prefix = if instance_data.binding_from_modification {
            variables::modification_binding_prefix_pub(instance_data, tree)?
        } else {
            variables::parent_prefix_pub(&instance_data.qualified_name)
        };
        let imports = variable_import_context_for_instance(
            instance_data,
            tree,
            class_index,
            &mut import_cache,
            &scope_index,
            component_override_map,
        )?;
        let binding_imports = if instance_data.binding_from_modification {
            &imports.binding
        } else {
            &imports.declaration
        };

        let Some(flat_var) = flat.variables.get_mut(&var_name) else {
            continue;
        };

        // Preserve structured comprehensions when dimensions are inferable from
        // symbolic ranges/body shape; avoid eager scalar expansion.
        if let Some(binding) = &flat_var.binding
            && let Some(inferred_dims) = infer_array_dimensions(binding)
        {
            if inferred_dims.len() >= flat_var.dims.len() {
                flat_var.dims = inferred_dims.clone();
            }
            ctx.array_dimensions
                .insert(var_name.to_string(), inferred_dims);
            continue;
        }

        let Some(index_ranges) = try_expand_index_ranges(ctx, indices, &prefix)? else {
            continue;
        };

        let expanded_elements = expand_comprehension(expr, &index_ranges);
        if expanded_elements.is_empty() {
            continue;
        }

        let is_matrix = matches!(&**expr, ast::Expression::Array { .. });
        let expanded_array = ast::Expression::Array {
            elements: expanded_elements,
            is_matrix,
            span: *span,
        };

        let flat_binding = lower_expanded_comprehension_binding(
            &expanded_array,
            &prefix,
            binding_imports,
            def_map,
        )?;

        flat_var.binding = Some(flat_binding.clone());
        changed_binding = true;

        if let Some(inferred_dims) = infer_array_dimensions(&flat_binding) {
            flat_var.dims = inferred_dims.clone();
            ctx.array_dimensions
                .insert(var_name.to_string(), inferred_dims);
            #[cfg(feature = "tracing")]
            tracing::debug!(
                var = %var_name,
                dims = ?flat_var.dims,
                "expanded array comprehension and inferred dimensions"
            );
        }
    }
    Ok(changed_binding)
}

/// Try to evaluate all index ranges for a comprehension. Returns None if any range
/// cannot be expanded or there are no indices.
type ExpandedIndexRanges<'a> = Vec<(&'a str, Vec<i64>)>;

fn try_expand_index_ranges<'a>(
    ctx: &Context,
    indices: &'a [ast::ForIndex],
    prefix: &ast::QualifiedName,
) -> Result<Option<ExpandedIndexRanges<'a>>, FlattenError> {
    if indices.is_empty() {
        return Ok(None);
    }

    let mut ranges = Vec::new();
    for idx in indices {
        let idx_name = idx.ident.text.as_ref();
        let values = expand_range_indices(ctx, &idx.range, prefix, idx.range.span())?;
        ranges.push((idx_name, values));
    }
    Ok(Some(ranges))
}

/// Expand an array comprehension expression by substituting all index combinations.
fn expand_comprehension(
    expr: &ast::Expression,
    index_ranges: &[(&str, Vec<i64>)],
) -> Vec<ast::Expression> {
    if index_ranges.is_empty() {
        return vec![expr.clone()];
    }

    // For single index, just iterate.
    if index_ranges.len() == 1 {
        let (idx_name, values) = &index_ranges[0];
        return values
            .iter()
            .map(|&val| substitute_index_in_expression(expr, idx_name, val))
            .collect();
    }

    // For multiple indices, recursively expand.
    let (first_name, first_values) = &index_ranges[0];
    let remaining = &index_ranges[1..];

    let mut result = Vec::new();
    for &val in first_values {
        let substituted = substitute_index_in_expression(expr, first_name, val);
        let inner = expand_comprehension(&substituted, remaining);
        result.extend(inner);
    }
    result
}

fn lower_expanded_comprehension_binding(
    expanded_array: &ast::Expression,
    prefix: &ast::QualifiedName,
    imports: &ImportMap,
    def_map: &crate::ResolveDefMap,
) -> Result<rumoca_core::Expression, FlattenError> {
    let opts = QualifyOptions::default();
    let qualified = qualify_expression_with_imports(expanded_array, prefix, opts, imports);
    ast_lower::expression_from_ast_with_def_map(&qualified, Some(def_map))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{Location, Token};
    use rumoca_ir_ast::{ComponentRefPart, ComponentReference, TerminalType};
    use std::sync::Arc;

    fn cref(name: &str) -> ast::Expression {
        let mut parts = Vec::new();
        for seg in crate::path_utils::segments(name) {
            parts.push(ComponentRefPart {
                ident: Token {
                    text: Arc::from(seg),
                    location: Location::default(),
                    token_number: 0,
                    token_type: 0,
                },
                subs: None,
            });
        }
        ast::Expression::ComponentReference(ComponentReference {
            local: false,
            parts,
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        })
    }

    fn int_lit(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: Arc::from(value.to_string()),
                location: Location::default(),
                token_number: 0,
                token_type: 0,
            },
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn test_expand_comprehension_single_index_substitutes_all_values() {
        let expr = cref("i");
        let expanded = expand_comprehension(&expr, &[("i", vec![1, 2, 3])]);
        assert_eq!(expanded.len(), 3);
        assert_eq!(expanded[0], int_lit(1));
        assert_eq!(expanded[1], int_lit(2));
        assert_eq!(expanded[2], int_lit(3));
    }

    #[test]
    fn test_expand_comprehension_multiple_indices_cartesian_product() {
        let expr = ast::Expression::Array {
            elements: vec![cref("i"), cref("j")],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        };
        let expanded = expand_comprehension(&expr, &[("i", vec![1, 2]), ("j", vec![10, 20])]);
        assert_eq!(expanded.len(), 4);
    }

    #[test]
    fn test_expand_comprehension_empty_ranges_returns_original_expression() {
        let expr = int_lit(42);
        let expanded = expand_comprehension(&expr, &[]);
        assert_eq!(expanded, vec![expr]);
    }

    #[test]
    fn test_lower_expanded_comprehension_preserves_imported_constant_aliases() {
        let expanded_array = ast::Expression::Array {
            elements: vec![cref("pi")],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        };
        let prefix = ast::QualifiedName::from_dotted("machine.spacePhasorS");
        let mut imports = ImportMap::default();
        imports.insert("pi".to_string(), "Modelica.Constants.pi".to_string());
        let def_map = crate::ResolveDefMap::default();

        let lowered =
            lower_expanded_comprehension_binding(&expanded_array, &prefix, &imports, &def_map)
                .unwrap();

        let rumoca_core::Expression::Array { elements, .. } = lowered else {
            panic!("expected lowered array");
        };
        let rumoca_core::Expression::VarRef { name, .. } = &elements[0] else {
            panic!("expected imported constant var ref");
        };
        assert_eq!(name.as_str(), "Modelica.Constants.pi");
    }
}
