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
use crate::functions;
use crate::qualify::{ImportMap, QualifyOptions, qualify_expression_with_imports};
use rustc_hash::FxHashMap;

#[derive(Debug, Clone, Default)]
pub(crate) struct VariableImportContext {
    pub(crate) declaration: ImportMap,
    pub(crate) binding: ImportMap,
    pub(crate) attributes: FxHashMap<String, ImportMap>,
    pub(crate) declaration_function_scope: Option<String>,
    pub(crate) binding_function_scope: Option<String>,
    pub(crate) attribute_function_scopes: FxHashMap<String, String>,
}

impl VariableImportContext {
    fn binding_imports(&self) -> &ImportMap {
        &self.binding
    }

    fn attribute_imports(&self, attr_name: &str) -> &ImportMap {
        self.attributes.get(attr_name).unwrap_or(&self.declaration)
    }
}

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

/// Resolve the lexical scope prefix for a modification-derived binding.
///
/// Source is `binding_source_scope` captured during instantiation.
fn modification_binding_prefix(
    instance: &ast::InstanceData,
    tree: &ast::ClassTree,
) -> Result<ast::QualifiedName, FlattenError> {
    instance.binding_source_scope.clone().ok_or_else(|| {
        FlattenError::missing_source_scope(
            instance.qualified_name.to_flat_string(),
            "modifier binding",
            tree.source_map.location_to_span(
                &instance.source_location.file_name,
                instance.source_location.start as usize,
                instance.source_location.end as usize,
            ),
        )
    })
}

fn attribute_prefix(
    instance: &ast::InstanceData,
    attr_name: &str,
    fallback: ast::QualifiedName,
) -> ast::QualifiedName {
    instance
        .attribute_source_scopes
        .get(attr_name)
        .cloned()
        .unwrap_or(fallback)
}

/// Public wrapper for modification_binding_prefix.
pub(crate) fn modification_binding_prefix_pub(
    instance: &ast::InstanceData,
    tree: &ast::ClassTree,
) -> Result<ast::QualifiedName, FlattenError> {
    modification_binding_prefix(instance, tree)
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
    class_index: &ast::ClassDefIndex<'_>,
    imports: &VariableImportContext,
) -> Result<flat::Variable, FlattenError> {
    let name = rumoca_core::VarName::new(instance.qualified_name.to_flat_string());
    let source_span = tree.source_map.location_to_span(
        &instance.source_location.file_name,
        instance.source_location.start as usize,
        instance.source_location.end as usize,
    );

    // Get the parent prefix for qualifying attribute expressions.
    // For "filter.m", the prefix is "filter" so that references like "n"
    // become "filter.n".
    let prefix = parent_prefix(&instance.qualified_name);
    let opts = QualifyOptions {
        preserve_def_id: true,
        ..QualifyOptions::default()
    };

    // Get def_map for resolving function call def_ids to qualified names
    let def_map = &tree.def_map;

    let attrs = qualify_variable_attributes(VariableQualifyContext {
        instance,
        tree,
        class_index,
        imports,
        prefix: &prefix,
        opts,
        def_map,
    })?;

    // Binding expressions need careful handling:
    // - Declaration bindings (e.g., `parameter Integer m = integer(n/2)`) reference
    //   sibling variables within the same class and need qualification with parent prefix.
    // - Modification bindings (e.g., `body(useQuaternions=useQuaternions)`) reference
    //   variables in the lexical scope where the modification is written.
    //   This scope is tracked during instantiation (MLS §7.2.4).
    let binding = qualify_variable_binding(VariableQualifyContext {
        instance,
        tree,
        class_index,
        imports,
        prefix: &prefix,
        opts,
        def_map,
    })?;

    let component_ref = Some(ast::instance::component_reference_for_instance(
        &instance.qualified_name,
        source_span,
        instance
            .component_ref
            .as_ref()
            .and_then(|reference| reference.def_id),
    ));

    Ok(flat::Variable {
        name,
        component_ref,
        source_span,
        type_id: instance.type_id,
        // Type prefixes from component declaration (MLS §4.4.2)
        variability: instance.variability.clone(),
        causality: instance.causality.clone(),
        flow: instance.flow,
        stream: instance.stream,
        dims: instance.dims.clone(),
        connected: false, // Will be set during connection processing
        start: attrs.start,
        fixed: instance.fixed,
        min: attrs.min,
        max: attrs.max,
        nominal: attrs.nominal,
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

fn canonicalize_function_calls(
    mut expr: rumoca_core::Expression,
    source_scope: Option<&str>,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
) -> rumoca_core::Expression {
    functions::canonicalize_function_calls_in_expression_with_scope(
        &mut expr,
        tree,
        class_index,
        source_scope,
    );
    expr
}

#[derive(Clone, Copy)]
struct VariableQualifyContext<'a, 'tree> {
    instance: &'a ast::InstanceData,
    tree: &'a ast::ClassTree,
    class_index: &'a ast::ClassDefIndex<'tree>,
    imports: &'a VariableImportContext,
    prefix: &'a ast::QualifiedName,
    opts: QualifyOptions,
    def_map: &'a crate::ResolveDefMap,
}

struct QualifiedVariableAttributes {
    start: Option<rumoca_core::Expression>,
    min: Option<rumoca_core::Expression>,
    max: Option<rumoca_core::Expression>,
    nominal: Option<rumoca_core::Expression>,
}

fn qualify_variable_attributes(
    ctx: VariableQualifyContext<'_, '_>,
) -> Result<QualifiedVariableAttributes, FlattenError> {
    Ok(QualifiedVariableAttributes {
        start: qualify_variable_attribute(ctx, "start", ctx.instance.start.as_ref())?,
        min: qualify_variable_attribute(ctx, "min", ctx.instance.min.as_ref())?,
        max: qualify_variable_attribute(ctx, "max", ctx.instance.max.as_ref())?,
        nominal: qualify_variable_attribute(ctx, "nominal", ctx.instance.nominal.as_ref())?,
    })
}

fn qualify_variable_attribute(
    ctx: VariableQualifyContext<'_, '_>,
    attr_name: &str,
    expr: Option<&ast::Expression>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let Some(expr) = expr else {
        return Ok(None);
    };
    let attr_prefix = attribute_prefix(ctx.instance, attr_name, ctx.prefix.clone());
    let qualified = qualify_expression_with_imports(
        expr,
        &attr_prefix,
        ctx.opts,
        ctx.imports.attribute_imports(attr_name),
    );
    let source_scope = ctx
        .imports
        .attribute_function_scopes
        .get(attr_name)
        .map(String::as_str)
        .or(ctx.imports.declaration_function_scope.as_deref());
    Ok(Some(canonicalize_function_calls(
        ast_lower::expression_from_ast_with_def_map(&qualified, Some(ctx.def_map))?,
        source_scope,
        ctx.tree,
        ctx.class_index,
    )))
}

fn qualify_variable_binding(
    ctx: VariableQualifyContext<'_, '_>,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let Some(expr) = ctx
        .instance
        .binding_source
        .as_ref()
        .or(ctx.instance.binding.as_ref())
    else {
        return Ok(None);
    };
    if ctx.instance.binding_from_modification {
        return qualify_modification_binding(ctx, expr).map(Some);
    }
    qualify_declaration_binding(ctx, expr).map(Some)
}

fn qualify_modification_binding(
    ctx: VariableQualifyContext<'_, '_>,
    expr: &ast::Expression,
) -> Result<rumoca_core::Expression, FlattenError> {
    let mod_prefix = modification_binding_prefix(ctx.instance, ctx.tree)?;
    let qualified =
        qualify_expression_with_imports(expr, &mod_prefix, ctx.opts, ctx.imports.binding_imports());
    Ok(canonicalize_function_calls(
        ast_lower::expression_from_ast_with_def_map(&qualified, Some(ctx.def_map))?,
        ctx.imports.binding_function_scope.as_deref(),
        ctx.tree,
        ctx.class_index,
    ))
}

fn qualify_declaration_binding(
    ctx: VariableQualifyContext<'_, '_>,
    expr: &ast::Expression,
) -> Result<rumoca_core::Expression, FlattenError> {
    let qualified =
        qualify_expression_with_imports(expr, ctx.prefix, ctx.opts, &ctx.imports.declaration);
    let source_scope = ctx
        .imports
        .binding_function_scope
        .as_deref()
        .or(ctx.imports.declaration_function_scope.as_deref());
    Ok(canonicalize_function_calls(
        ast_lower::expression_from_ast_with_def_map(&qualified, Some(ctx.def_map))?,
        source_scope,
        ctx.tree,
        ctx.class_index,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_ast as ast;
    use std::sync::Arc;

    fn comp_ref(path: &[&str]) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: path
                .iter()
                .map(|segment| ast::ComponentRefPart {
                    ident: rumoca_core::Token {
                        text: Arc::from(*segment),
                        ..rumoca_core::Token::default()
                    },
                    subs: None,
                })
                .collect(),
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        })
    }

    #[test]
    fn test_create_flat_variable_uses_modifier_source_scope_for_nested_field_binding() {
        let component_def_id = rumoca_core::DefId::new(42);
        let qualified_name = ast::QualifiedName::from_dotted("aimc.airGap.L0.d");
        let instance = ast::InstanceData {
            component_ref: Some(ast::instance::component_reference_for_instance(
                &qualified_name,
                rumoca_core::Span::DUMMY,
                Some(component_def_id),
            )),
            qualified_name,
            binding_source: Some(comp_ref(&["L0", "d"])),
            binding_from_modification: true,
            binding_source_scope: Some(ast::QualifiedName::from_dotted("aimc")),
            is_primitive: true,
            ..ast::InstanceData::default()
        };
        let tree = ast::ClassTree::default();
        let imports = VariableImportContext::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let flat =
            create_flat_variable(&instance, &tree, &class_index, &imports).expect("flat variable");
        assert_eq!(
            flat.component_ref
                .as_ref()
                .and_then(|reference| reference.def_id),
            Some(component_def_id)
        );
        let binding = flat.binding.expect("binding");
        match binding {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                assert_eq!(name.as_str(), "aimc.L0.d");
                assert!(subscripts.is_empty());
            }
            _ => panic!("expected binding to become a qualified VarRef"),
        }
    }

    #[test]
    fn test_create_flat_variable_uses_modifier_source_scope_for_attribute() {
        let mut attribute_source_scopes = ast::AstIndexMap::default();
        attribute_source_scopes.insert(
            "max".to_string(),
            ast::QualifiedName::from_dotted("leftBoundary1"),
        );
        let instance = ast::InstanceData {
            qualified_name: ast::QualifiedName::from_dotted("leftBoundary1.ports.m_flow"),
            max: Some(comp_ref(&["flowDirection"])),
            attribute_source_scopes,
            is_primitive: true,
            ..ast::InstanceData::default()
        };
        let tree = ast::ClassTree::default();
        let imports = VariableImportContext::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let flat =
            create_flat_variable(&instance, &tree, &class_index, &imports).expect("flat variable");
        let max = flat.max.expect("max");
        match max {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                assert_eq!(name.as_str(), "leftBoundary1.flowDirection");
                assert!(subscripts.is_empty());
            }
            _ => panic!("expected max to become a qualified VarRef"),
        }
    }
}
