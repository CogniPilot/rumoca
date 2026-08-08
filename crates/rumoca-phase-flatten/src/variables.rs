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
use crate::source_spans::required_location_span;
use rustc_hash::{FxHashMap, FxHashSet};

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
    canonical_type_id: TypeId,
    tree: &ast::ClassTree,
) -> Result<String, FlattenError> {
    if let Some(type_name) = resolve_flat_output_type_name(tree, canonical_type_id)
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
    effective_type_id: TypeId,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    imports: &VariableImportContext,
) -> Result<flat::Variable, FlattenError> {
    let name = rumoca_core::VarName::new(instance.qualified_name.to_flat_string());
    let source_span = instance_source_span(instance, tree, "flat variable")?;

    // Get the parent prefix for qualifying attribute expressions.
    // For "filter.m", the prefix is "filter" so that references like "n"
    // become "filter.n".
    let prefix = parent_prefix(&instance.qualified_name);
    let opts = QualifyOptions::default();

    let attrs = qualify_variable_attributes(VariableQualifyContext {
        instance,
        tree,
        class_index,
        imports,
        prefix: &prefix,
        opts,
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
    })?;

    let component_ref =
        Some(instance.component_ref.clone().ok_or_else(|| {
            FlattenError::missing_flat_variable_identity(name.as_str(), source_span)
        })?);

    Ok(flat::Variable {
        instance_id: instance.instance_id,
        name,
        component_ref,
        source_span,
        type_id: effective_type_id,
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

pub(crate) fn create_record_instance(
    instance: &ast::InstanceData,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    effective_type_id: TypeId,
    canonical_type_id: TypeId,
) -> Result<Option<flat::RecordInstance>, FlattenError> {
    let declared_type_def_id = instance.type_def_id;
    let candidate = match tree.type_table.get(canonical_type_id) {
        Some(ast::Type::Class(class_type)) if class_type.kind == ast::ClassKind::Record => {
            Some(class_type.def_id)
        }
        _ => declared_type_def_id,
    };
    let Some(type_def_id) = candidate
        .and_then(|def_id| record_base_def_id(class_index, def_id, &mut FxHashSet::default()))
    else {
        return Ok(None);
    };
    let Some(class_def) = class_index.get(type_def_id) else {
        return Ok(None);
    };
    if class_def.class_type != rumoca_core::ClassType::Record {
        return Ok(None);
    }
    let source_span = instance_source_span(instance, tree, "flat record instance")?;
    let component_ref = instance.component_ref.clone().ok_or_else(|| {
        FlattenError::missing_source_context(format!(
            "record instance `{}` lacks a resolved component reference",
            instance.qualified_name.to_flat_string()
        ))
    })?;
    Ok(Some(flat::RecordInstance {
        instance_id: instance.instance_id,
        component_ref,
        source_span,
        effective_type_id,
        type_name: class_index
            .qualified_name(type_def_id)
            .unwrap_or(class_def.name.text.as_ref())
            .to_string(),
        type_def_id,
        dims: instance.dims.clone(),
    }))
}

/// Resolve the record denoted by a short class definition using only exact
/// Resolve identities.  MLS short connector declarations such as
/// `connector Input = input Sample` are represented as one inheritance edge;
/// the connector occurrence is therefore a record container even though its
/// declaration's specialized-class keyword is `connector`.
fn record_base_def_id(
    class_index: &ast::ClassDefIndex<'_>,
    def_id: rumoca_core::DefId,
    active: &mut FxHashSet<rumoca_core::DefId>,
) -> Option<rumoca_core::DefId> {
    if !active.insert(def_id) {
        return None;
    }
    let class = class_index.get(def_id)?;
    if class.class_type == rumoca_core::ClassType::Record {
        return Some(def_id);
    }
    if class.end_name_token.is_some() || class.extends.len() != 1 {
        return None;
    }
    let base = class.extends[0].base_def_id?;
    record_base_def_id(class_index, base, active)
}

pub(crate) fn create_record_type(
    type_def_id: rumoca_core::DefId,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    type_catalog: crate::functions::FunctionTypeCatalog<'_>,
) -> Result<flat::RecordType, FlattenError> {
    let class_def = class_index.get(type_def_id).ok_or_else(|| {
        FlattenError::missing_source_context(format!(
            "record type {type_def_id} is absent from the resolved class index"
        ))
    })?;
    let qualified_name = class_index
        .qualified_name(type_def_id)
        .unwrap_or(class_def.name.text.as_ref());
    Ok(flat::RecordType {
        name: qualified_name.to_string(),
        fields: functions::record_type_fields(
            class_index,
            class_def,
            qualified_name,
            tree,
            type_catalog,
        )?,
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
    let source_scope = ctx
        .instance
        .attribute_source_scopes
        .get(attr_name)
        .or(ctx.instance.declaration_source_scope.as_ref());
    let imports = imports_without_component_shadowing(
        ctx.imports.attribute_imports(attr_name),
        source_scope,
        ctx.class_index,
        expr,
    );
    let qualified = qualify_expression_with_imports(expr, &attr_prefix, ctx.opts, &imports);
    let source_scope = ctx
        .imports
        .attribute_function_scopes
        .get(attr_name)
        .map(String::as_str)
        .or(ctx.imports.declaration_function_scope.as_deref());
    Ok(Some(canonicalize_function_calls(
        ast_lower::expression_from_ast_with_context(
            &qualified,
            ast_lower::LoweringContext {
                instance_name: None,
                predefined_string_declaration: ctx
                    .tree
                    .scope_tree
                    .predefined_member(&rumoca_core::ComponentPath::from_flat_path("String")),
                predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(ctx.tree),
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
    if ctx.instance.binding_from_modification
        && !references_declaration_component(ctx.instance, ctx.class_index, expr)
    {
        return qualify_modification_binding(ctx, expr).map(Some);
    }
    qualify_declaration_binding(ctx, expr).map(Some)
}

fn references_declaration_component(
    instance: &ast::InstanceData,
    class_index: &ast::ClassDefIndex<'_>,
    expression: &ast::Expression,
) -> bool {
    let Some(scope) = instance.declaration_source_scope.as_ref() else {
        return false;
    };
    let scope = scope.to_flat_string();
    let Some(class_def) = class_index.get_by_qualified_name(&scope) else {
        return false;
    };
    let declarations = class_def
        .components
        .values()
        .filter_map(|component| component.def_id)
        .collect::<FxHashSet<_>>();
    let mut finder = DeclarationComponentReferenceFinder {
        declarations: &declarations,
        found: false,
    };
    let _ = rumoca_ir_ast::Visitor::visit_expression(&mut finder, expression);
    finder.found
}

struct DeclarationComponentReferenceFinder<'declaration> {
    declarations: &'declaration FxHashSet<rumoca_core::DefId>,
    found: bool,
}

impl rumoca_ir_ast::Visitor for DeclarationComponentReferenceFinder<'_> {
    fn visit_component_reference_ctx(
        &mut self,
        reference: &ast::ComponentReference,
        _context: rumoca_ir_ast::ComponentReferenceContext,
    ) -> std::ops::ControlFlow<()> {
        if reference
            .parts
            .first()
            .and_then(|part| part.def_id)
            .is_some_and(|definition| self.declarations.contains(&definition))
        {
            self.found = true;
            return std::ops::ControlFlow::Break(());
        }
        self.visit_component_reference(reference)
    }
}

fn qualify_modification_binding(
    ctx: VariableQualifyContext<'_, '_>,
    expr: &ast::Expression,
) -> Result<rumoca_core::Expression, FlattenError> {
    let mod_prefix = modification_binding_prefix(ctx.instance, ctx.tree)?;
    let imports = imports_without_component_shadowing(
        ctx.imports.binding_imports(),
        ctx.instance.binding_source_scope.as_ref(),
        ctx.class_index,
        expr,
    );
    let qualified = qualify_expression_with_imports(expr, &mod_prefix, ctx.opts, &imports);
    Ok(canonicalize_function_calls(
        ast_lower::expression_from_ast_with_context(
            &qualified,
            ast_lower::LoweringContext {
                instance_name: None,
                predefined_string_declaration: ctx
                    .tree
                    .scope_tree
                    .predefined_member(&rumoca_core::ComponentPath::from_flat_path("String")),
                predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(ctx.tree),
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
    let imports = imports_without_component_shadowing(
        &ctx.imports.declaration,
        ctx.instance.declaration_source_scope.as_ref(),
        ctx.class_index,
        expr,
    );
    let qualified = qualify_expression_with_imports(expr, ctx.prefix, ctx.opts, &imports);
    let source_scope = ctx
        .imports
        .binding_function_scope
        .as_deref()
        .or(ctx.imports.declaration_function_scope.as_deref());
    Ok(canonicalize_function_calls(
        ast_lower::expression_from_ast_with_context(
            &qualified,
            ast_lower::LoweringContext {
                instance_name: None,
                predefined_string_declaration: ctx
                    .tree
                    .scope_tree
                    .predefined_member(&rumoca_core::ComponentPath::from_flat_path("String")),
                predefined_intrinsics: ast_lower::PredefinedIntrinsicIds::from_tree(ctx.tree),
            },
        )?,
        source_scope,
        ctx.tree,
        ctx.class_index,
    ))
}

/// Imported names never outrank a component declared in the expression's
/// lexical class.  Instance expansion may contribute identity aliases such as
/// `mass -> mass`; retaining one while qualifying `vehicle.weight = mass*g`
/// loses the concrete `vehicle` scope and manufactures an unresolved Flat
/// reference.  Remove only proven component names and leave class/package
/// aliases available for constructors and qualified constants.
fn imports_without_component_shadowing(
    imports: &ImportMap,
    source_scope: Option<&ast::QualifiedName>,
    class_index: &ast::ClassDefIndex<'_>,
    expression: &ast::Expression,
) -> ImportMap {
    let mut filtered = imports.clone();
    if let Some(class_def) = source_scope
        .map(ast::QualifiedName::to_flat_string)
        .as_deref()
        .and_then(|scope| class_index.get_by_qualified_name(scope))
    {
        for component in class_def.components.keys() {
            filtered.remove(component);
        }
    }
    let mut collector = ValueComponentImportShadows {
        class_index,
        names: FxHashSet::default(),
    };
    let _ = rumoca_ir_ast::Visitor::visit_expression(&mut collector, expression);
    for name in collector.names {
        if filtered.get(&name).is_some_and(|target| target == &name) {
            filtered.remove(&name);
        }
    }
    filtered
}

struct ValueComponentImportShadows<'tree, 'index> {
    class_index: &'index ast::ClassDefIndex<'tree>,
    names: FxHashSet<String>,
}

impl rumoca_ir_ast::Visitor for ValueComponentImportShadows<'_, '_> {
    fn visit_component_reference_ctx(
        &mut self,
        reference: &ast::ComponentReference,
        _context: rumoca_ir_ast::ComponentReferenceContext,
    ) -> std::ops::ControlFlow<()> {
        if let Some(part) = reference.parts.first()
            && part
                .def_id
                .is_some_and(|definition| self.class_index.get(definition).is_none())
        {
            self.names.insert(part.ident.text.to_string());
        }
        self.visit_component_reference(reference)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_ast as ast;
    use std::sync::Arc;

    fn test_tree() -> ast::ClassTree {
        let mut tree = ast::ClassTree::new();
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
            source: rumoca_core::SourceId::from_source_name("variable_fixture.mo"),
        }
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("variable_fixture.mo"),
            10,
            16,
        )
    }

    fn fixture_def_id(name: &str) -> rumoca_core::DefId {
        let hash = name.bytes().fold(2_166_136_261_u32, |hash, byte| {
            hash.wrapping_mul(16_777_619) ^ u32::from(byte)
        });
        rumoca_core::DefId::new(hash.max(1))
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
                    def_id: Some(fixture_def_id(segment)),
                })
                .collect(),
            span: test_span(),
            qualified_display_name: None,
        })
    }

    fn core_component_ref(path: &[&str]) -> rumoca_core::ComponentReference {
        let parts = path
            .iter()
            .map(|segment| rumoca_core::ComponentRefPart {
                ident: (*segment).to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: fixture_def_id(segment),
            })
            .collect();
        rumoca_core::ComponentReference::construct(false, test_span(), parts)
            .expect("fixture component path is nonempty and resolved")
    }

    #[test]
    fn test_create_flat_variable_uses_modifier_source_scope_for_nested_field_binding()
    -> Result<(), FlattenError> {
        let component_def_id = rumoca_core::DefId::new(42);
        let qualified_name = ast::QualifiedName::from_dotted("aimc.airGap.L0.d");
        let instance = ast::InstanceData {
            component_ref: Some(
                rumoca_core::ComponentReference::construct(
                    false,
                    test_span(),
                    vec![
                        rumoca_core::ComponentRefPart {
                            ident: "aimc".to_string(),
                            span: test_span(),
                            subs: Vec::new(),
                            def_id: rumoca_core::DefId::new(1),
                        },
                        rumoca_core::ComponentRefPart {
                            ident: "airGap".to_string(),
                            span: test_span(),
                            subs: Vec::new(),
                            def_id: rumoca_core::DefId::new(2),
                        },
                        rumoca_core::ComponentRefPart {
                            ident: "L0".to_string(),
                            span: test_span(),
                            subs: Vec::new(),
                            def_id: rumoca_core::DefId::new(3),
                        },
                        rumoca_core::ComponentRefPart {
                            ident: "d".to_string(),
                            span: test_span(),
                            subs: Vec::new(),
                            def_id: component_def_id,
                        },
                    ],
                )
                .expect("test component reference is nonempty and resolved"),
            ),
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
        let flat =
            create_flat_variable(&instance, instance.type_id, &tree, &class_index, &imports)?;
        assert_eq!(
            flat.component_ref
                .as_ref()
                .map(|reference| reference.target_def_id()),
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
            component_ref: Some(core_component_ref(&["leftBoundary1", "ports", "m_flow"])),
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
        let flat = create_flat_variable(&instance, instance.type_id, &tree, &class_index, &imports)
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
    fn flat_output_type_uses_the_canonical_type_root() {
        let instance = ast::InstanceData {
            type_name: "Modelica.Units.SI.Angle".to_string(),
            ..ast::InstanceData::default()
        };
        let tree = test_tree();

        let name = flat_output_type_name(&instance, tree.type_table.real(), &tree)
            .expect("canonical primitive type");

        assert_eq!(name, "Real");
    }
}
