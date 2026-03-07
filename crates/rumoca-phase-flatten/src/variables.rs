//! Variable flattening for the flatten phase.
//!
//! This module converts instance data to flat variables with
//! globally unique names.
//!
//! Per SPEC_0022 §3.19-3.20, type prefixes (variability, causality, flow, stream)
//! are preserved from the component declaration through to the flat model.

use rumoca_core::TypeId;
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

use crate::ast_lower;
use crate::errors::FlattenError;
use crate::qualify::{ImportMap, QualifyOptions, qualify_expression_with_imports};

/// Get the parent prefix from a qualified name.
///
/// For `filter.m`, returns `filter`.
/// For `x`, returns an empty prefix.
fn parent_prefix(qn: &ast::QualifiedName) -> ast::QualifiedName {
    if qn.parts.len() <= 1 {
        ast::QualifiedName::new()
    } else {
        ast::QualifiedName {
            parts: qn.parts[..qn.parts.len() - 1].to_vec(),
        }
    }
}

/// Public wrapper for parent_prefix.
pub(crate) fn parent_prefix_pub(qn: &ast::QualifiedName) -> ast::QualifiedName {
    parent_prefix(qn)
}

/// Get the grandparent prefix of a qualified name.
/// For "a.b.c" returns "a".
/// For "a.b" returns "".
/// Used for modification bindings that reference the outer scope.
fn grandparent_prefix(qn: &ast::QualifiedName) -> ast::QualifiedName {
    if qn.parts.len() <= 2 {
        ast::QualifiedName::new()
    } else {
        ast::QualifiedName {
            parts: qn.parts[..qn.parts.len() - 2].to_vec(),
        }
    }
}

/// Resolve the lexical scope prefix for a modification-derived binding.
///
/// Preferred source is `binding_source_scope` captured during instantiation.
/// Falls back to historical `grandparent` derivation when no source scope is present.
fn modification_binding_prefix(instance: &ast::InstanceData) -> ast::QualifiedName {
    instance
        .binding_source_scope
        .clone()
        .unwrap_or_else(|| grandparent_prefix(&instance.qualified_name))
}

/// Public wrapper for modification_binding_prefix.
pub(crate) fn modification_binding_prefix_pub(instance: &ast::InstanceData) -> ast::QualifiedName {
    modification_binding_prefix(instance)
}

const MAX_TYPE_RESOLVE_DEPTH: usize = 16;

fn resolve_flat_output_type_name(tree: &ast::ClassTree, mut type_id: TypeId) -> Option<String> {
    for _ in 0..MAX_TYPE_RESOLVE_DEPTH {
        let ty = tree.type_table.get(type_id)?;
        match ty {
            ast::Type::Builtin(builtin) => return Some(builtin.name().to_string()),
            ast::Type::Enumeration(enumeration) => return Some(enumeration.name.clone()),
            ast::Type::Alias(alias) => {
                if alias.aliased.is_unknown() || alias.aliased == type_id {
                    return Some(alias.name.clone());
                }
                type_id = alias.aliased;
            }
            ast::Type::Array(array) => {
                if array.element.is_unknown() || array.element == type_id {
                    return None;
                }
                type_id = array.element;
            }
            ast::Type::Class(class_ty) => return Some(class_ty.name.clone()),
            ast::Type::Function(function_ty) => return Some(function_ty.name.clone()),
            ast::Type::Unknown => return None,
        }
    }
    None
}

pub(crate) fn flat_output_type_name(instance: &ast::InstanceData, tree: &ast::ClassTree) -> String {
    resolve_flat_output_type_name(tree, instance.type_id)
        .or_else(|| (!instance.type_name.is_empty()).then(|| instance.type_name.clone()))
        .unwrap_or_else(|| "Real".to_string())
}

/// Create a flat::Variable from instance data.
///
/// Preserves all type prefixes (variability, causality, flow, stream) from
/// the component declaration per MLS §4.4.2 and SPEC_0022 §3.19-3.20.
///
/// Binding and attribute expressions are qualified with the component's parent
/// prefix so that references to sibling variables are properly resolved.
/// For example, if `filter.m` has binding `integer(n/2)`, the reference `n`
/// becomes `filter.n` after qualification.
///
/// Per MLS §7.2.4, modification bindings (from outer scope) reference variables
/// in the scope where the modification is written, not the component's scope.
/// These are NOT qualified to preserve correct scoping semantics.
///
/// Function calls in bindings use def_id to resolve fully qualified names,
/// ensuring that imported functions are correctly looked up by name.
pub(crate) fn create_flat_variable(
    instance: &ast::InstanceData,
    tree: &ast::ClassTree,
    imports: &ImportMap,
) -> Result<flat::Variable, FlattenError> {
    let name = flat::VarName::new(instance.qualified_name.to_flat_string());

    // Get the parent prefix for qualifying attribute expressions.
    // For "filter.m", the prefix is "filter" so that references like "n"
    // become "filter.n".
    let prefix = parent_prefix(&instance.qualified_name);
    let opts = QualifyOptions::default();

    // Get def_map for resolving function call def_ids to qualified names
    let def_map = &tree.def_map;

    // Helper to qualify and convert an expression, using def_map for function resolution.
    // MLS §13.2: Uses global import map so short names like `pi` resolve to FQN
    // instead of being incorrectly prefixed with the component path.
    let qualify_and_convert = |expr: &ast::Expression| {
        let qualified = qualify_expression_with_imports(expr, &prefix, opts, imports);
        ast_lower::expression_from_ast_with_def_map(&qualified, Some(def_map))
    };

    // Convert attributes with qualification
    // Attribute expressions (start, min, max, nominal) reference sibling variables
    // and need qualification with the parent prefix.
    let start = instance.start.as_ref().map(&qualify_and_convert);
    let min = instance.min.as_ref().map(&qualify_and_convert);
    let max = instance.max.as_ref().map(&qualify_and_convert);
    let nominal = instance.nominal.as_ref().map(&qualify_and_convert);

    // Binding expressions need careful handling:
    // - Declaration bindings (e.g., `parameter Integer m = integer(n/2)`) reference
    //   sibling variables within the same class and need qualification with parent prefix.
    // - Modification bindings (e.g., `body(useQuaternions=useQuaternions)`) reference
    //   variables in the lexical scope where the modification is written.
    //   This scope is tracked during instantiation (MLS §7.2.4).
    // Both use the same global import map since imports are unambiguous.
    let binding_expr = instance
        .binding_source
        .as_ref()
        .or(instance.binding.as_ref());
    let binding = binding_expr.map(|e| {
        if instance.binding_from_modification {
            // Modification bindings: qualify using captured modifier source scope.
            let mod_prefix = modification_binding_prefix(instance);
            let qualified = qualify_expression_with_imports(e, &mod_prefix, opts, imports);
            ast_lower::expression_from_ast_with_def_map(&qualified, Some(def_map))
        } else {
            // Declaration bindings: qualify to resolve sibling references
            qualify_and_convert(e)
        }
    });

    Ok(flat::Variable {
        name,
        type_id: instance.type_id,
        // Type prefixes from component declaration (MLS §4.4.2)
        variability: flat::variability_from_ast(&instance.variability),
        causality: flat::causality_from_ast(&instance.causality),
        flow: instance.flow,
        stream: instance.stream,
        dims: instance.dims.clone(),
        connected: false, // Will be set during connection processing
        start,
        fixed: instance.fixed,
        min,
        max,
        nominal,
        quantity: instance.quantity.clone(),
        unit: instance.unit.clone(),
        display_unit: instance.display_unit.clone(),
        description: instance.description.clone(),
        state_select: instance.state_select,
        binding,
        binding_from_modification: instance.binding_from_modification,
        evaluate: instance.evaluate,
        is_discrete_type: instance.is_discrete_type,
        is_primitive: instance.is_primitive,
        from_expandable_connector: instance.from_expandable_connector,
        is_overconstrained: instance.is_overconstrained,
        is_protected: instance.is_protected,
        oc_record_path: instance.oc_record_path.clone(),
        oc_eq_constraint_size: instance.oc_eq_constraint_size,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_ast as ast;
    use rumoca_ir_flat as flat;
    use std::sync::Arc;

    fn comp_ref(path: &[&str]) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: path
                .iter()
                .map(|segment| ast::ComponentRefPart {
                    ident: rumoca_ir_core::Token {
                        text: Arc::from(*segment),
                        ..rumoca_ir_core::Token::default()
                    },
                    subs: None,
                })
                .collect(),
            def_id: None,
        })
    }

    #[test]
    fn test_create_flat_variable_uses_modifier_source_scope_for_nested_field_binding() {
        let instance = ast::InstanceData {
            qualified_name: ast::QualifiedName::from_dotted("aimc.airGap.L0.d"),
            binding_source: Some(comp_ref(&["L0", "d"])),
            binding_from_modification: true,
            binding_source_scope: Some(ast::QualifiedName::from_dotted("aimc")),
            is_primitive: true,
            ..ast::InstanceData::default()
        };
        let tree = ast::ClassTree::default();
        let imports = ImportMap::default();
        let flat = create_flat_variable(&instance, &tree, &imports).expect("flat variable");
        let binding = flat.binding.expect("binding");
        match binding {
            flat::Expression::VarRef { name, subscripts } => {
                assert_eq!(name.as_str(), "aimc.L0.d");
                assert!(subscripts.is_empty());
            }
            _ => panic!("expected binding to become a qualified VarRef"),
        }
    }
}
