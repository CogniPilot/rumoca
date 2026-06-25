//! Variable flattening for the flatten phase.
//!
//! This module converts instance data to flat variables with
//! globally unique names.
//!
//! Per SPEC_0022 §3.19-3.20, type prefixes (variability, causality, flow, stream)
//! are preserved from the component declaration through to the flat model.

use rumoca_core::{ProvenanceSpan, TypeId};
use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

use crate::ast_lower;
use crate::errors::FlattenError;
use crate::functions;
use crate::qualify::{ImportMap, QualifyOptions, qualify_expression_with_imports};
use crate::source_spans::required_location_span;
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
    instance
        .binding_source_scope
        .clone()
        .ok_or_else(|| missing_instance_source_scope_error(instance, tree, "modifier binding"))
}

fn missing_instance_source_scope_error(
    instance: &ast::InstanceData,
    tree: &ast::ClassTree,
    context: &str,
) -> FlattenError {
    match instance_source_span(instance, tree, context) {
        Ok(span) => FlattenError::missing_source_scope(
            instance.qualified_name.to_flat_string(),
            context,
            span,
        ),
        Err(error) => error,
    }
}

fn instance_source_span(
    instance: &ast::InstanceData,
    tree: &ast::ClassTree,
    context: &str,
) -> Result<rumoca_core::Span, FlattenError> {
    required_location_span(&tree.source_map, &instance.source_location, context)
}

fn require_component_ref_provenance(
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<ProvenanceSpan, FlattenError> {
    span.require_provenance(context)
        .map_err(|err| FlattenError::missing_source_context(err.to_string()))
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

pub(crate) fn flat_output_type_name(
    instance: &ast::InstanceData,
    tree: &ast::ClassTree,
) -> Result<String, FlattenError> {
    if let Some(type_name) = resolve_flat_output_type_name(tree, instance.type_id)
        .or_else(|| (!instance.type_name.is_empty()).then(|| instance.type_name.clone()))
    {
        return Ok(type_name);
    }

    let span = instance_source_span(instance, tree, "flat output type")?;
    Err(FlattenError::unresolved_variable_type(
        instance.qualified_name.to_flat_string(),
        span,
    ))
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
    simulated_root_name: Option<&str>,
) -> Result<flat::Variable, FlattenError> {
    let name = rumoca_core::VarName::new(instance.qualified_name.to_flat_string());
    let source_span = instance_source_span(instance, tree, "flat variable")?;

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

    let mut attrs = qualify_variable_attributes(VariableQualifyContext {
        instance,
        tree,
        class_index,
        imports,
        prefix: &prefix,
        opts,
        def_map,
        simulated_root_name,
    })?;

    // Binding expressions need careful handling:
    // - Declaration bindings (e.g., `parameter Integer m = integer(n/2)`) reference
    //   sibling variables within the same class and need qualification with parent prefix.
    // - Modification bindings (e.g., `body(useQuaternions=useQuaternions)`) reference
    //   variables in the lexical scope where the modification is written.
    //   This scope is tracked during instantiation (MLS §7.2.4).
    let mut binding = qualify_variable_binding(VariableQualifyContext {
        instance,
        tree,
        class_index,
        imports,
        prefix: &prefix,
        opts,
        def_map,
        simulated_root_name,
    })?;
    if instance_is_string_type(instance) {
        recover_string_literal_expr(&mut binding, &prefix);
        recover_string_literal_expr(&mut attrs.start, &prefix);
    }

    let component_ref = Some(ast::instance::component_reference_for_instance(
        &instance.qualified_name,
        require_component_ref_provenance(source_span, "flat instance component reference")?,
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

fn instance_is_string_type(instance: &ast::InstanceData) -> bool {
    rumoca_core::qualified_type_name_matches(&instance.type_name, "String")
        || instance.type_id == rumoca_core::TypeId(3)
}

fn recover_string_literal_expr(
    expr: &mut Option<rumoca_core::Expression>,
    prefix: &ast::QualifiedName,
) {
    let Some(recovered) = expr.as_ref().and_then(|expr| {
        recover_string_literal_from_invalid_var_ref(expr, &prefix.to_flat_string())
    }) else {
        return;
    };
    *expr = Some(recovered);
}

fn recover_string_literal_from_invalid_var_ref(
    expr: &rumoca_core::Expression,
    prefix: &str,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::VarRef {
        name,
        subscripts,
        span,
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let path = rumoca_core::ComponentPath::from_reference(name);
    if path
        .parts()
        .iter()
        .all(|part| is_valid_modelica_identifier(part))
    {
        return None;
    }
    let literal_path = if prefix.is_empty() {
        path
    } else {
        path.strip_prefix(&rumoca_core::ComponentPath::from_flat_path(prefix))?
    };
    Some(rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String(literal_path.to_flat_string()),
        span: *span,
    })
}

fn is_valid_modelica_identifier(part: &str) -> bool {
    let mut chars = part.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    (first == '_' || first.is_ascii_alphabetic())
        && chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
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
    simulated_root_name: Option<&'a str>,
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
    let instance_name = declaration_instance_name(ctx.simulated_root_name, ctx.prefix);
    Ok(Some(canonicalize_function_calls(
        ast_lower::expression_from_ast_with_context(
            &qualified,
            ast_lower::LoweringContext {
                def_map: Some(ctx.def_map),
                instance_name: instance_name.as_deref(),
            },
        )?,
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
    let instance_name = declaration_instance_name(ctx.simulated_root_name, &mod_prefix);
    Ok(canonicalize_function_calls(
        ast_lower::expression_from_ast_with_context(
            &qualified,
            ast_lower::LoweringContext {
                def_map: Some(ctx.def_map),
                instance_name: instance_name.as_deref(),
            },
        )?,
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
    let instance_name = declaration_instance_name(ctx.simulated_root_name, ctx.prefix);
    Ok(canonicalize_function_calls(
        ast_lower::expression_from_ast_with_context(
            &qualified,
            ast_lower::LoweringContext {
                def_map: Some(ctx.def_map),
                instance_name: instance_name.as_deref(),
            },
        )?,
        source_scope,
        ctx.tree,
        ctx.class_index,
    ))
}

fn declaration_instance_name(
    simulated_root_name: Option<&str>,
    prefix: &ast::QualifiedName,
) -> Option<String> {
    let root = simulated_root_name?;
    let suffix = prefix.to_flat_string();
    if suffix.is_empty() {
        Some(root.to_string())
    } else {
        Some(format!("{root}.{suffix}"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_ast as ast;
    use std::sync::Arc;

    fn test_tree() -> ast::ClassTree {
        let mut tree = ast::ClassTree::default();
        tree.source_map.add(
            "variable_fixture.mo",
            "model M\n  Real d;\n  Real m_flow;\nend M;\n",
        );
        tree
    }

    fn test_location(start: u32, end: u32) -> rumoca_core::Location {
        rumoca_core::Location {
            start_line: 1,
            start_column: start + 1,
            end_line: 1,
            end_column: end + 1,
            start,
            end,
            file_name: "variable_fixture.mo".to_string(),
        }
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("variable_fixture.mo"),
            10,
            16,
        )
    }

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
            span: test_span(),
        })
    }

    #[test]
    fn test_create_flat_variable_uses_modifier_source_scope_for_nested_field_binding()
    -> Result<(), FlattenError> {
        let component_def_id = rumoca_core::DefId::new(42);
        let qualified_name = ast::QualifiedName::from_dotted("aimc.airGap.L0.d");
        let instance = ast::InstanceData {
            component_ref: Some(ast::instance::component_reference_for_instance(
                &qualified_name,
                require_component_ref_provenance(test_span(), "flat test component reference")?,
                Some(component_def_id),
            )),
            qualified_name,
            source_location: test_location(10, 16),
            binding_source: Some(comp_ref(&["L0", "d"])),
            binding_from_modification: true,
            binding_source_scope: Some(ast::QualifiedName::from_dotted("aimc")),
            is_primitive: true,
            ..ast::InstanceData::default()
        };
        let tree = test_tree();
        let imports = VariableImportContext::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let flat = create_flat_variable(&instance, &tree, &class_index, &imports, None)?;
        assert_eq!(
            flat.component_ref
                .as_ref()
                .and_then(|reference| reference.def_id),
            Some(component_def_id)
        );
        let Some(binding) = flat.binding else {
            return Err(FlattenError::missing_source_context(
                "test flat variable binding missing",
            ));
        };
        match binding {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                assert_eq!(name.as_str(), "aimc.L0.d");
                assert!(subscripts.is_empty());
            }
            _ => {
                return Err(FlattenError::missing_source_context(
                    "expected binding to become a qualified VarRef",
                ));
            }
        }
        Ok(())
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
            source_location: test_location(20, 31),
            max: Some(comp_ref(&["flowDirection"])),
            attribute_source_scopes,
            is_primitive: true,
            ..ast::InstanceData::default()
        };
        let tree = test_tree();
        let imports = VariableImportContext::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let flat = create_flat_variable(&instance, &tree, &class_index, &imports, None)
            .expect("flat variable");
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

    #[test]
    fn test_create_flat_variable_lowers_get_instance_name_to_parent_instance() {
        let instance = ast::InstanceData {
            qualified_name: ast::QualifiedName::from_dotted("building.modelicaNameBuilding"),
            source_location: test_location(40, 64),
            binding_source: Some(ast::Expression::FunctionCall {
                comp: ast::ComponentReference {
                    local: false,
                    parts: vec![ast::ComponentRefPart {
                        ident: rumoca_core::Token {
                            text: Arc::from("getInstanceName"),
                            ..rumoca_core::Token::default()
                        },
                        subs: None,
                    }],
                    def_id: None,
                    span: test_span(),
                },
                args: Vec::new(),
                span: test_span(),
            }),
            is_primitive: true,
            ..ast::InstanceData::default()
        };
        let tree = test_tree();
        let imports = VariableImportContext::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let flat =
            create_flat_variable(&instance, &tree, &class_index, &imports, Some("RootModel"))
                .expect("flat variable");

        assert_eq!(
            flat.binding,
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("RootModel.building".to_string()),
                span: test_span(),
            })
        );
    }

    #[test]
    fn test_create_flat_variable_preserves_string_literal_binding() {
        let instance = ast::InstanceData {
            qualified_name: ast::QualifiedName::from_dotted("building.spawnExe"),
            source_location: test_location(6, 19),
            binding_source: Some(ast::Expression::Terminal {
                terminal_type: ast::TerminalType::String,
                token: rumoca_core::Token {
                    text: Arc::from("\"spawn-0.4.3-7048a72798\""),
                    ..rumoca_core::Token::default()
                },
                span: test_span(),
            }),
            is_primitive: true,
            ..ast::InstanceData::default()
        };
        let tree = test_tree();
        let imports = VariableImportContext::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let flat = create_flat_variable(&instance, &tree, &class_index, &imports, None)
            .expect("flat variable");

        assert_eq!(
            flat.binding,
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("spawn-0.4.3-7048a72798".to_string()),
                span: test_span(),
            })
        );
    }

    #[test]
    fn test_create_flat_variable_recovers_invalid_string_literal_varref() {
        let instance = ast::InstanceData {
            qualified_name: ast::QualifiedName::from_dotted("building.spawnExe"),
            source_location: test_location(6, 19),
            type_name: "String".to_string(),
            binding: Some(comp_ref(&["spawn-0", "4", "3-7048a72798"])),
            start: Some(comp_ref(&["spawn-0", "4", "3-7048a72798"])),
            is_primitive: true,
            ..ast::InstanceData::default()
        };
        let tree = test_tree();
        let imports = VariableImportContext::default();
        let class_index = ast::ClassDefIndex::from_tree(&tree);
        let flat = create_flat_variable(&instance, &tree, &class_index, &imports, None)
            .expect("flat variable");

        let expected = Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("spawn-0.4.3-7048a72798".to_string()),
            span: test_span(),
        });
        assert_eq!(flat.binding, expected);
        assert_eq!(flat.start, expected);
    }
}
