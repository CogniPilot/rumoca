#[cfg(test)]
mod mod_env_tests;
mod record_projection;
#[cfg(test)]
mod tests;

use super::inheritance::{
    find_class_in_tree, get_effective_components, resolve_effective_components_for_eval,
};
use super::nested_scope::remap_redeclare_class_modifier;
use super::type_overrides::{
    TypeOverrideMap, class_redeclare_alias_ref, contains_component_by_def_id_in_hierarchy,
    direct_source_redeclare, find_nested_class_by_def_id_in_hierarchy,
};
use super::{InstantiateContext, InstantiateError, InstantiateResult};
use rumoca_eval_ast::eval_instantiate::{
    InstantiateEvalCtx, evaluate_component_condition, try_eval_integer_expr, try_eval_string_expr,
};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rustc_hash::FxHashSet;

pub(super) use record_projection::{RecordBindingProjection, propagate_record_binding_to_fields};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ModificationResolveMode {
    Modifier,
    DeclarationBinding,
}

/// Populate the modification environment with a component's modifications.
///
/// MLS §7.2: This handles both:
/// - Direct value modifications like `p(k = 5.0)` - stored as binding for k
/// - Nested class modifications like `sub(x(start = 10))` - stored for nested lookup
///
/// Modifications are stored with paths relative to the component being instantiated.
/// The `effective_components` parameter provides the parent scope's components for
/// resolving parameter references in modifications like `plug_p(final m=m)`.
/// The `target_class` parameter provides the target class's definition for checking
/// final restrictions on its components.
pub(super) struct PopulateModEnvInput<'a> {
    pub(super) comp: &'a ast::Component,
    pub(super) effective_components: &'a IndexMap<String, ast::Component>,
    pub(super) type_overrides: &'a TypeOverrideMap,
    pub(super) target_class: Option<&'a ast::ClassDef>,
    pub(super) parent_snapshot: &'a IndexMap<ast::QualifiedName, rumoca_ir_ast::ModificationValue>,
    pub(super) shifted_parent_keys: &'a IndexMap<ast::QualifiedName, ()>,
    /// Import aliases of the class that wrote these modifications (MLS §13.2).
    pub(super) modifier_imports: crate::dims::ImportRewrite<'a>,
}

struct ScopedInsertContext<'a> {
    parent_snapshot: &'a IndexMap<ast::QualifiedName, rumoca_ir_ast::ModificationValue>,
    shifted_parent_keys: &'a IndexMap<ast::QualifiedName, ()>,
    source_scope: Option<ast::QualifiedName>,
    /// Import aliases of the class that wrote these modifications (MLS §13.2).
    imports: crate::dims::ImportRewrite<'a>,
}

struct ModifierEvalContext<'a> {
    tree: &'a ast::ClassTree,
    comp: &'a ast::Component,
    effective_components: &'a IndexMap<String, ast::Component>,
    type_overrides: &'a TypeOverrideMap,
    target_class: Option<&'a ast::ClassDef>,
    insert_ctx: ScopedInsertContext<'a>,
}

#[derive(Clone, Copy, Default)]
struct ModifierPrefixes {
    final_: bool,
    each: bool,
}

#[derive(Clone, Copy)]
struct ModifierInsertOptions {
    allow_string_eval: bool,
    prefixes: ModifierPrefixes,
}

struct NestedModificationContext<'a> {
    effective_components: &'a IndexMap<String, ast::Component>,
    tree: &'a ast::ClassTree,
    source_scope: Option<ast::QualifiedName>,
    /// Import aliases of the class that wrote these modifications (MLS §13.2).
    imports: crate::dims::ImportRewrite<'a>,
}

struct NestedModificationFlags<'a> {
    prefixes: ModifierPrefixes,
    each_flags: &'a [bool],
    final_flags: &'a [bool],
}

struct ScopedModifierBinding {
    key: ast::QualifiedName,
    value: ast::Expression,
    source: Option<ast::Expression>,
    source_scope: Option<ast::QualifiedName>,
    prefixes: ModifierPrefixes,
}

pub(super) fn populate_modification_environment(
    ctx: &mut InstantiateContext,
    tree: &ast::ClassTree,
    input: PopulateModEnvInput<'_>,
) -> InstantiateResult<()> {
    let PopulateModEnvInput {
        comp,
        effective_components,
        type_overrides,
        target_class,
        parent_snapshot,
        shifted_parent_keys,
        modifier_imports,
    } = input;
    let eval_ctx = ModifierEvalContext {
        tree,
        comp,
        effective_components,
        type_overrides,
        target_class,
        insert_ctx: ScopedInsertContext {
            parent_snapshot,
            shifted_parent_keys,
            source_scope: Some(enclosing_modifier_scope(ctx)),
            imports: modifier_imports,
        },
    };

    let mod_env_snapshot = ctx.mod_env().clone();
    for (target_name, mod_expr) in &comp.modifications {
        let prefixes = ModifierPrefixes {
            final_: comp.final_attributes.contains(target_name),
            each: comp.each_modifications.contains(target_name),
        };
        if let Err(error) =
            apply_component_modifier(ctx, target_name, mod_expr, prefixes, &eval_ctx)
        {
            *ctx.mod_env_mut() = mod_env_snapshot;
            return Err(error);
        }
    }
    Ok(())
}

fn insert_modifier_value_with_structural_overrides(
    ctx: &mut InstantiateContext,
    target_name: &str,
    value_expr: &ast::Expression,
    options: ModifierInsertOptions,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    insert_ctx: &ScopedInsertContext<'_>,
) -> InstantiateResult<()> {
    let qn = ast::QualifiedName::from_ident(target_name);
    let (binding_source, binding_source_scope) =
        inherited_modifier_source_metadata(target_name, value_expr, ctx.mod_env()).map_or(
            (Some(value_expr.clone()), insert_ctx.source_scope.clone()),
            |(src, scope)| {
                (
                    src.or_else(|| Some(value_expr.clone())),
                    scope.or_else(|| insert_ctx.source_scope.clone()),
                )
            },
        );
    let resolved_expr = resolve_modification_expr(
        value_expr,
        ModifierResolveScope {
            mod_env: ctx.mod_env(),
            effective_components,
            tree,
            imports: insert_ctx.imports,
        },
        options.allow_string_eval,
    )?;
    let structural_field_overrides = collect_structural_integer_fields_from_sibling_reference(
        value_expr,
        ctx.mod_env(),
        effective_components,
        tree,
    );
    insert_scoped_modifier_binding(
        ctx,
        ScopedModifierBinding {
            key: qn,
            value: resolved_expr,
            source: binding_source,
            source_scope: binding_source_scope.clone(),
            prefixes: options.prefixes,
        },
        insert_ctx.parent_snapshot,
        insert_ctx.shifted_parent_keys,
    )?;
    for (field_name, field_value) in structural_field_overrides {
        let field_qn = ast::QualifiedName::from_ident(target_name).child(&field_name);
        insert_scoped_modifier_binding(
            ctx,
            ScopedModifierBinding {
                key: field_qn,
                value: field_value,
                source: None,
                source_scope: binding_source_scope.clone(),
                prefixes: options.prefixes,
            },
            insert_ctx.parent_snapshot,
            insert_ctx.shifted_parent_keys,
        )?;
    }
    Ok(())
}

fn inherited_modifier_source_metadata(
    target_name: &str,
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
) -> Option<(Option<ast::Expression>, Option<ast::QualifiedName>)> {
    let target_qn = ast::QualifiedName::from_ident(target_name);
    if let Some(existing) = mod_env.get(&target_qn)
        && existing.value == *expr
    {
        return Some((
            existing
                .source
                .clone()
                .or_else(|| Some(existing.value.clone())),
            existing.source_scope.clone(),
        ));
    }

    let ast::Expression::ComponentReference(comp_ref) = expr else {
        return None;
    };
    if comp_ref.parts.len() != 1 || comp_ref.parts[0].subs.is_some() {
        return None;
    }

    let name = comp_ref.parts[0].ident.text.as_ref();
    if name != target_name {
        return None;
    }
    let qn = ast::QualifiedName::from_ident(name);
    let mod_value = mod_env.get(&qn)?;
    if mod_value.value == *expr {
        return None;
    }

    Some((
        mod_value
            .source
            .clone()
            .or_else(|| Some(mod_value.value.clone())),
        mod_value.source_scope.clone(),
    ))
}

fn apply_component_modifier(
    ctx: &mut InstantiateContext,
    target_name: &str,
    mod_expr: &ast::Expression,
    prefixes: ModifierPrefixes,
    eval_ctx: &ModifierEvalContext<'_>,
) -> InstantiateResult<()> {
    let target_component = modifier_target_component(eval_ctx, target_name)?;
    let allow_string_eval = target_component.as_ref().is_some_and(|target_comp| {
        component_type_allows_string_modifier(&target_comp.type_name.to_string())
    });

    // INST-010: Check if target component is final in the target class (MLS §7.2.6)
    if target_component
        .as_ref()
        .is_some_and(|target_comp| target_comp.is_final)
    {
        let span = required_modifier_expr_span(mod_expr, "final component modifier")?;
        return Err(Box::new(InstantiateError::redeclare_final(
            target_name,
            span,
        )));
    }

    match mod_expr {
        // Nested class modification: l2(x(start = 100))
        ast::Expression::ClassModification {
            modifications,
            each_flags,
            final_flags,
            ..
        } => {
            apply_nested_class_modifier(
                ctx,
                target_name,
                modifications,
                eval_ctx,
                prefixes,
                each_flags,
                final_flags,
            )?;
            preserve_redeclare_class_modifier(ctx, target_name, mod_expr, eval_ctx)?;
        }
        // Nested class modification WITH binding: field(start=X) = expr.
        // MLS §7.2: process both nested attribute modifications and binding.
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            rhs,
            ..
        } => {
            if let ast::Expression::ClassModification {
                modifications,
                each_flags,
                final_flags,
                ..
            } = &**lhs
            {
                apply_nested_class_modifier(
                    ctx,
                    target_name,
                    modifications,
                    eval_ctx,
                    prefixes,
                    each_flags,
                    final_flags,
                )?;
            }
            insert_modifier_value_with_structural_overrides(
                ctx,
                target_name,
                rhs,
                ModifierInsertOptions {
                    allow_string_eval,
                    prefixes,
                },
                eval_ctx.effective_components,
                eval_ctx.tree,
                &eval_ctx.insert_ctx,
            )?;
        }
        // Direct value modification: k = 5.0.
        _ => {
            insert_modifier_value_with_structural_overrides(
                ctx,
                target_name,
                mod_expr,
                ModifierInsertOptions {
                    allow_string_eval,
                    prefixes,
                },
                eval_ctx.effective_components,
                eval_ctx.tree,
                &eval_ctx.insert_ctx,
            )?;
        }
    }

    Ok(())
}

fn apply_nested_class_modifier(
    ctx: &mut InstantiateContext,
    target_name: &str,
    modifications: &[ast::Expression],
    eval_ctx: &ModifierEvalContext<'_>,
    prefixes: ModifierPrefixes,
    each_flags: &[bool],
    final_flags: &[bool],
) -> InstantiateResult<()> {
    let prefix = ast::QualifiedName::from_ident(target_name);
    let nested_ctx = NestedModificationContext {
        effective_components: eval_ctx.effective_components,
        tree: eval_ctx.tree,
        source_scope: eval_ctx.insert_ctx.source_scope.clone(),
        imports: eval_ctx.insert_ctx.imports,
    };
    process_nested_modifications_recursive(
        ctx,
        &prefix,
        modifications,
        &nested_ctx,
        NestedModificationFlags {
            prefixes,
            each_flags,
            final_flags,
        },
    )
}

fn preserve_redeclare_class_modifier(
    ctx: &mut InstantiateContext,
    target_name: &str,
    mod_expr: &ast::Expression,
    eval_ctx: &ModifierEvalContext<'_>,
) -> InstantiateResult<()> {
    // MLS §7.3: preserve class/package redeclare bindings in mod_env so
    // downstream type resolution sees component-level redeclare overrides.
    let is_redeclare_class_target = match (
        eval_ctx.target_class,
        direct_source_redeclare(eval_ctx.comp, target_name),
    ) {
        (Some(target_class), Some(source_redeclare)) => {
            let alias_def_id = class_redeclare_alias_ref(source_redeclare)
                .and_then(ast::ComponentReference::target_def_id)
                .ok_or_else(|| {
                    Box::new(InstantiateError::redeclare_error(
                        target_name,
                        "direct redeclare LHS has no Resolve-issued receiver-slot identity",
                        source_redeclare.span(),
                    ))
                })?;
            match find_nested_class_by_def_id_in_hierarchy(
                eval_ctx.tree,
                target_class,
                alias_def_id,
            )? {
                Some(nested) => nested.is_replaceable,
                None if contains_component_by_def_id_in_hierarchy(
                    eval_ctx.tree,
                    target_class,
                    alias_def_id,
                )? =>
                {
                    false
                }
                None => {
                    return Err(Box::new(InstantiateError::redeclare_error(
                        target_name,
                        format!(
                            "Resolve-issued LHS {alias_def_id:?} is not an exact direct or inherited receiver slot"
                        ),
                        source_redeclare.span(),
                    )));
                }
            }
        }
        _ => false,
    };
    if is_redeclare_class_target {
        let qn = ast::QualifiedName::from_ident(target_name);
        let resolved_class_mod = remap_redeclare_class_modifier(
            eval_ctx.tree,
            mod_expr,
            target_name,
            eval_ctx.type_overrides,
        )?;
        ctx.mod_env_mut().add(
            qn,
            ast::ModificationValue::with_source_scope(
                resolved_class_mod,
                None,
                eval_ctx.insert_ctx.source_scope.clone(),
            ),
        );
    }
    Ok(())
}

fn modifier_target_component(
    eval_ctx: &ModifierEvalContext<'_>,
    target_name: &str,
) -> InstantiateResult<Option<ast::Component>> {
    let Some(target_class) = eval_ctx.target_class else {
        return Ok(None);
    };
    Ok(get_effective_components(eval_ctx.tree, target_class)?
        .get(target_name)
        .cloned())
}

fn component_type_allows_string_modifier(type_name: &str) -> bool {
    rumoca_core::qualified_type_name_matches(type_name, "String")
}

fn insert_scoped_modifier_binding(
    ctx: &mut InstantiateContext,
    binding: ScopedModifierBinding,
    parent_snapshot: &IndexMap<ast::QualifiedName, rumoca_ir_ast::ModificationValue>,
    shifted_parent_keys: &IndexMap<ast::QualifiedName, ()>,
) -> InstantiateResult<()> {
    let ScopedModifierBinding {
        key,
        value,
        source,
        source_scope,
        prefixes,
    } = binding;
    // MLS §7.2: local modifier bindings must replace colliding parent-scope keys.
    // Preserve explicitly shifted parent keys because those are real nested overrides.
    let replace_parent =
        parent_snapshot.contains_key(&key) && !shifted_parent_keys.contains_key(&key);
    if shifted_parent_keys.contains_key(&key)
        && ctx
            .mod_env()
            .get(&key)
            .is_some_and(|existing| existing.final_)
    {
        return Ok(());
    }
    if ctx
        .mod_env()
        .get(&key)
        .is_some_and(|existing| existing.final_ && !replace_parent)
    {
        let span = required_binding_source_span(source.as_ref(), &value, "final modifier binding")?;
        return Err(Box::new(InstantiateError::redeclare_final(
            key.to_flat_string(),
            span,
        )));
    }
    let mod_env = ctx.mod_env_mut();
    if replace_parent {
        mod_env.active.shift_remove(&key);
    }
    mod_env.add(
        key,
        rumoca_ir_ast::ModificationValue::with_source_scope_and_prefixes(
            value,
            source,
            source_scope,
            prefixes.each,
            prefixes.final_,
        ),
    );
    Ok(())
}

fn required_binding_source_span(
    source: Option<&ast::Expression>,
    value: &ast::Expression,
    context: &'static str,
) -> InstantiateResult<rumoca_core::Span> {
    match source {
        Some(expr) => required_modifier_expr_span(expr, context),
        None => required_modifier_expr_span(value, context),
    }
}

fn required_modifier_expr_span(
    expr: &ast::Expression,
    context: &'static str,
) -> InstantiateResult<rumoca_core::Span> {
    let span = expr.span();
    if span.is_dummy() {
        return Err(Box::new(InstantiateError::missing_source_context(format!(
            "{context} is missing source provenance"
        ))));
    }
    Ok(span)
}

fn enclosing_modifier_scope(ctx: &InstantiateContext) -> ast::QualifiedName {
    let mut path = ctx.current_path();
    path.parts.pop();
    path
}

fn collect_structural_integer_fields_from_sibling_reference(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Vec<(String, ast::Expression)> {
    let ast::Expression::ComponentReference(comp_ref) = expr else {
        return Vec::new();
    };
    if comp_ref.parts.len() != 1 || comp_ref.parts[0].subs.is_some() {
        return Vec::new();
    }

    let source_name = comp_ref.parts[0].ident.text.as_ref();
    let Some(source_component) = effective_components.get(source_name) else {
        return Vec::new();
    };
    if !component_type_is_record(source_component, tree) {
        return Vec::new();
    }

    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    source_component
        .modifications
        .iter()
        .filter_map(|(field_name, field_expr)| {
            let binding = field_expr.component_modifier_binding_value()?;
            try_eval_integer_expr(&eval_ctx, binding).map(|value| {
                (
                    field_name.clone(),
                    ast::Expression::Terminal {
                        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                        token: rumoca_core::Token {
                            text: value.to_string().into(),
                            ..Default::default()
                        },
                        span: binding.span(),
                    },
                )
            })
        })
        .collect()
}

fn component_type_is_record(comp: &ast::Component, tree: &ast::ClassTree) -> bool {
    comp.type_def_id
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .or_else(|| find_class_in_tree(tree, &comp.type_name.to_string()))
        .is_some_and(|class| class.class_type == rumoca_core::ClassType::Record)
}

/// Everything a modifier expression is resolved against (MLS §7.2.4: the scope
/// where the modification was written).
#[derive(Clone, Copy)]
struct ModifierResolveScope<'a> {
    mod_env: &'a ast::ModificationEnvironment,
    effective_components: &'a IndexMap<String, ast::Component>,
    tree: &'a ast::ClassTree,
    /// Import aliases visible where the expression was written (MLS §13.2), used
    /// to reach a constant that the writing class named through an `import`.
    imports: crate::dims::ImportRewrite<'a>,
}

/// Resolve a modification expression by evaluating component references in scope.
fn resolve_modification_expr(
    expr: &ast::Expression,
    scope: ModifierResolveScope<'_>,
    allow_string_eval: bool,
) -> InstantiateResult<ast::Expression> {
    resolve_modification_expr_checked(
        expr,
        scope,
        allow_string_eval,
        ModificationResolveMode::Modifier,
        &mut FxHashSet::default(),
    )
}

pub(super) fn resolve_declaration_binding_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
) -> InstantiateResult<ast::Expression> {
    resolve_modification_expr_checked(
        expr,
        ModifierResolveScope {
            mod_env,
            effective_components,
            tree,
            imports: crate::dims::ImportRewrite::without_imports(class_index),
        },
        false,
        ModificationResolveMode::DeclarationBinding,
        &mut FxHashSet::default(),
    )
}

/// Decide a Boolean parameter expression written in this scope (MLS §4.4.5).
///
/// The expression is first read exactly as written. Only when that leaves it
/// undecided is it retried with the writing class's `import` aliases applied
/// (MLS §13.2/§5.3.2) — a modifier such as `useStrayPermeance =
/// ratioCommonLeakage < (1 - eps)` names an imported package constant that is
/// invisible under its short spelling. Qualification is a lexical fact about the
/// name, not a value: the retry can only *find* a declaration, never invent one,
/// and the qualified spelling is used solely to decide, never stored.
fn decide_boolean_modifier(
    expr: &ast::Expression,
    scope: ModifierResolveScope<'_>,
    eval_ctx: &InstantiateEvalCtx<'_>,
) -> InstantiateResult<Option<bool>> {
    if let Some(value) = evaluate_component_condition(eval_ctx, expr) {
        return Ok(Some(value));
    }
    if !crate::dims::expr_mentions_import_alias(expr, scope.imports) {
        return Ok(None);
    }
    let qualified = crate::dims::qualify_shape_expr_imports(scope.tree, expr, scope.imports)?;
    Ok(evaluate_component_condition(eval_ctx, &qualified))
}

fn resolve_modification_expr_checked(
    expr: &ast::Expression,
    scope: ModifierResolveScope<'_>,
    allow_string_eval: bool,
    mode: ModificationResolveMode,
    active: &mut FxHashSet<rumoca_core::DefId>,
) -> InstantiateResult<ast::Expression> {
    let ModifierResolveScope {
        mod_env,
        effective_components,
        tree,
        imports,
    } = scope;

    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };

    // Resolve booleans first (e.g., useFilter=useFilter) so conditional
    // components in nested classes evaluate against the parent's value.
    if let Some(value) = decide_boolean_modifier(expr, scope, &eval_ctx)? {
        return Ok(ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::Bool,
            token: rumoca_core::Token {
                text: value.to_string().into(),
                ..Default::default()
            },
            span: expr.span(),
        });
    }

    // Resolve string-valued modifiers in the parent scope so nested conditional
    // components can evaluate against concrete values (e.g. "D"/"Y").
    if allow_string_eval && let Some(value) = try_eval_string_expr(&eval_ctx, expr) {
        return Ok(ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::String,
            token: rumoca_core::Token {
                text: format!("\"{value}\"").into(),
                ..Default::default()
            },
            span: expr.span(),
        });
    }

    // Try to evaluate as an integer (common for array dimension parameters).
    if let Some(value) = try_eval_integer_expr(&eval_ctx, expr) {
        return Ok(ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: value.to_string().into(),
                ..Default::default()
            },
            span: expr.span(),
        });
    }

    // Resolve direct references in current scope (e.g. resolveInFrame=resolveInFrame).
    if mode == ModificationResolveMode::Modifier
        && let Some((edge_def_id, resolved_ref)) = resolve_single_part_ref_expr(
            expr,
            mod_env,
            effective_components,
            tree,
            imports.class_index,
        )?
    {
        return resolve_modifier_edge(
            edge_def_id,
            &resolved_ref,
            scope,
            allow_string_eval,
            mode,
            active,
        );
    }

    // MLS §7.2: Resolve multi-part references through sibling modifications.
    if let Some((edge_def_id, resolved)) =
        resolve_sibling_modification(expr, effective_components, imports.class_index)?
    {
        return resolve_modifier_edge(
            edge_def_id,
            &resolved,
            scope,
            allow_string_eval,
            mode,
            active,
        );
    }

    Ok(expr.clone())
}

fn resolve_modifier_edge(
    edge_def_id: rumoca_core::DefId,
    resolved: &ast::Expression,
    scope: ModifierResolveScope<'_>,
    allow_string_eval: bool,
    mode: ModificationResolveMode,
    active: &mut FxHashSet<rumoca_core::DefId>,
) -> InstantiateResult<ast::Expression> {
    if !active.insert(edge_def_id) {
        return Err(Box::new(InstantiateError::instantiation_cycle(
            format!("modifier reference {edge_def_id:?}"),
            resolved.span(),
        )));
    }
    let result =
        resolve_modification_expr_checked(resolved, scope, allow_string_eval, mode, active);
    active.remove(&edge_def_id);
    result
}

fn resolve_single_part_ref_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
) -> InstantiateResult<Option<(rumoca_core::DefId, ast::Expression)>> {
    let ast::Expression::ComponentReference(comp_ref) = expr else {
        return Ok(None);
    };
    if comp_ref.parts.len() != 1 {
        return Ok(None);
    }

    let name = comp_ref.parts[0].ident.text.as_ref();
    let qn = ast::QualifiedName::from_ident(name);
    let reference_def_id = comp_ref.root_def_id().ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("modifier reference `{comp_ref}`"),
            comp_ref.span,
        ))
    })?;
    let Some(component) = exact_modifier_component(
        class_index,
        effective_components,
        name,
        reference_def_id,
        comp_ref.span,
    )?
    else {
        return Ok(None);
    };

    if let Some(subscripts) = comp_ref.parts[0].subs.as_ref() {
        let Some(mod_value) = mod_env.get(&qn) else {
            return Ok(None);
        };
        return match select_array_value(&mod_value.value, subscripts) {
            ArrayValueSelection::NotStatic => Ok(None),
            ArrayValueSelection::Selected(value) => Ok(Some((reference_def_id, value))),
            ArrayValueSelection::Invalid(reason) => Err(Box::new(
                InstantiateError::invalid_mod_path(format!("{comp_ref}: {reason}"), comp_ref.span),
            )),
        };
    }

    if let Some(mod_value) = mod_env.get(&qn)
        && mod_value.value != *expr
    {
        return Ok(Some((reference_def_id, mod_value.value.clone())));
    }

    // A modifier is evaluated in the scope where it is written (MLS §7.2.4).
    // Enumeration values have no arithmetic evaluator, so follow an enum
    // parameter's declaration binding here until the chain settles to a literal.
    // Other component references deliberately retain their identity: inlining a
    // numeric binding, for example, would change tensor-family ownership.
    let type_table_proves_enum = matches!(
        component
            .type_id
            .and_then(|type_id| tree.type_table.get(type_id)),
        Some(ast::Type::Enumeration(_))
    );
    let declaration_proves_enum = component
        .type_def_id
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .is_some_and(|class| !class.enum_literals.is_empty());
    if !type_table_proves_enum && !declaration_proves_enum {
        return Ok(None);
    }
    Ok(component
        .binding
        .clone()
        .map(|value| (reference_def_id, value)))
}

#[derive(Debug, Clone, PartialEq)]
enum ArrayValueSelection {
    /// This phase cannot see enough to decide the selection. Either a selector
    /// is not a static one-based index, or the modifier value is not a literal
    /// array whose element the index could name here. Both defer to a later
    /// phase rather than refusing: a modifier value that is a component
    /// reference, `fill`/`zeros`, an arithmetic expression, or even a scalar
    /// broadcast into an array component carries no static evidence of the
    /// value's shape at this point, so the modification is preserved unchanged.
    NotStatic,
    /// Every selector was a valid one-based index into the known modifier.
    Selected(ast::Expression),
    /// Static evidence proves the selection malformed. This is reserved for the
    /// cases the value alone already refutes: a selector outside the one-based
    /// domain (zero, negative, unrepresentable), or an index beyond the extent
    /// of a literal array whose full length is visible here.
    Invalid(String),
}

fn select_array_value(
    mut expr: &ast::Expression,
    subscripts: &[ast::Subscript],
) -> ArrayValueSelection {
    if subscripts.is_empty() {
        return ArrayValueSelection::NotStatic;
    }
    for subscript in subscripts {
        let ast::Subscript::Expression(selector) = subscript else {
            return ArrayValueSelection::NotStatic;
        };
        let index = match literal_array_index(selector) {
            Ok(Some(index)) => index,
            Ok(None) => return ArrayValueSelection::NotStatic,
            Err(reason) => return ArrayValueSelection::Invalid(reason),
        };
        let ast::Expression::Array { elements, .. } = expr else {
            // The modifier value is not a literal array, so its element extent is
            // invisible here. A component reference, `fill`/`zeros`, or an
            // arithmetic expression may still be array-valued, and a scalar
            // literal may be a legitimate broadcast into an array component
            // (`Real x[3] = 1`). Deciding the shape needs the component's
            // declared dimensionality, which a later phase owns. Refusing now
            // would reject valid programs, so defer with the reference intact.
            return ArrayValueSelection::NotStatic;
        };
        let Some(selected) = elements.get(index - 1) else {
            return ArrayValueSelection::Invalid(format!(
                "array index {index} is out of bounds for extent {}",
                elements.len()
            ));
        };
        expr = selected;
    }
    ArrayValueSelection::Selected(expr.clone())
}

/// Recognize only literal signed syntax here. General structural evaluation is
/// owned elsewhere, but unary `+`/`-` around an integer token is already exact
/// evidence and must not be conflated with a genuinely dynamic selector.
fn literal_array_index(selector: &ast::Expression) -> Result<Option<usize>, String> {
    let (sign, token) = match selector {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => (1_i8, token),
        ast::Expression::Unary { op, rhs, .. } => {
            let ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token,
                ..
            } = rhs.as_ref()
            else {
                return Ok(None);
            };
            match op {
                rumoca_core::OpUnary::Plus => (1, token),
                rumoca_core::OpUnary::Minus => (-1, token),
                _ => return Ok(None),
            }
        }
        _ => return Ok(None),
    };
    let magnitude = token.text.parse::<usize>().map_err(|_| {
        format!(
            "array index `{}` is not representable as a positive integer",
            token.text
        )
    })?;
    if sign < 0 {
        return Err(format!(
            "Modelica array index -{magnitude} is negative; indices are one-based"
        ));
    }
    if magnitude == 0 {
        return Err("Modelica array indices are one-based; zero is invalid".to_string());
    }
    Ok(Some(magnitude))
}

/// Resolve a multi-part component reference by following sibling modifications.
fn resolve_sibling_modification(
    expr: &ast::Expression,
    effective_components: &IndexMap<String, ast::Component>,
    class_index: &ast::ClassDefIndex<'_>,
) -> InstantiateResult<Option<(rumoca_core::DefId, ast::Expression)>> {
    let ast::Expression::ComponentReference(comp_ref) = expr else {
        return Ok(None);
    };
    if comp_ref.parts.len() < 2 {
        return Ok(None);
    }
    let first = comp_ref.parts[0].ident.text.as_ref();
    let second = comp_ref.parts[1].ident.text.as_ref();
    let root_def_id = comp_ref.root_def_id().ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("sibling modifier reference `{comp_ref}`"),
            comp_ref.span,
        ))
    })?;
    let Some(comp) = exact_modifier_component(
        class_index,
        effective_components,
        first,
        root_def_id,
        comp_ref.span,
    )?
    else {
        // A resolved package, class, or enumeration root is not a sibling
        // component modifier. Preserve it for its semantic owner.
        return Ok(None);
    };
    let Some(mod_expr) = comp.modifications.get(second) else {
        return Ok(None);
    };
    // Keep record/class-modification bindings as references so declaration
    // defaults remain visible during record-field projection.
    if is_non_scalar_sibling_modifier_expr(mod_expr) {
        return Ok(None);
    }
    let edge_def_id = comp_ref.target_def_id().ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("sibling modifier target `{comp_ref}`"),
            comp_ref.span,
        ))
    })?;
    Ok(Some((edge_def_id, mod_expr.clone())))
}

/// Select the declaration named by a resolved modifier reference.
///
/// A modifier is evaluated in its lexical source scope (MLS §7.2.4), so the
/// reference may intentionally identify a component in an enclosing class
/// rather than the same-spelled active target slot. Exact `DefId` ownership is
/// the authority; spelling is used only to query an already-proven active slot.
/// `None` positively classifies the root as a class/package rather than a
/// component.
fn exact_modifier_component<'a>(
    index: &'a ast::ClassDefIndex<'a>,
    effective_components: &'a IndexMap<String, ast::Component>,
    name: &str,
    reference_def_id: rumoca_core::DefId,
    span: rumoca_core::Span,
) -> InstantiateResult<Option<&'a ast::Component>> {
    if let Some(component) = effective_components.get(name)
        && component.def_id == Some(reference_def_id)
    {
        return Ok(Some(component));
    }

    if index.get(reference_def_id).is_some() {
        return Ok(None);
    }
    // Predefined builtin classes (e.g. `StateSelect`, MLS §4.4.4.2) have no
    // `ClassDef` in the user tree; their resolved identities live in the
    // index's predefined-type authority. A reference rooted at one of them is
    // a class reference, never a sibling component.
    if rumoca_core::BUILTIN_TYPES
        .iter()
        .any(|builtin| index.predefined_def_id(builtin) == Some(reference_def_id))
    {
        return Ok(None);
    }
    let component = index
        .parent_def_id(reference_def_id)
        .and_then(|owner| index.get(owner))
        .and_then(|owner| {
            owner
                .components
                .values()
                .find(|component| component.def_id == Some(reference_def_id))
        })
        .ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!(
                    "modifier reference `{name}` identifies {reference_def_id:?}, which is absent from the exact component/class declaration graph"
                ),
                span,
            ))
        })?;
    Ok(Some(component))
}

fn is_non_scalar_sibling_modifier_expr(expr: &ast::Expression) -> bool {
    match expr {
        ast::Expression::ClassModification { .. }
        | ast::Expression::FunctionCall { .. }
        | ast::Expression::Array { .. }
        | ast::Expression::Tuple { .. }
        | ast::Expression::Range { .. }
        | ast::Expression::ArrayComprehension { .. } => true,
        ast::Expression::Modification {
            value: Some(value), ..
        } => is_non_scalar_sibling_modifier_expr(value),
        ast::Expression::Parenthesized { inner, .. } => is_non_scalar_sibling_modifier_expr(inner),
        _ => false,
    }
}

/// Process nested modifications recursively within a class modification.
///
/// MLS §7.2: Handles nested modifications like `l1(l2(x(start = 100)))`.
fn process_nested_modifications_recursive(
    ctx: &mut InstantiateContext,
    prefix: &ast::QualifiedName,
    modifications: &[ast::Expression],
    nested_ctx: &NestedModificationContext<'_>,
    flags: NestedModificationFlags<'_>,
) -> InstantiateResult<()> {
    for (idx, nested_mod) in modifications.iter().enumerate() {
        let nested_prefixes = ModifierPrefixes {
            each: flags.prefixes.each || flags.each_flags.get(idx).copied().unwrap_or(false),
            final_: flags.prefixes.final_ || flags.final_flags.get(idx).copied().unwrap_or(false),
        };
        match nested_mod {
            // A value-less modifier (`x(start)`) binds nothing, so the
            // modification environment records no entry for it (MLS §7.2).
            ast::Expression::Modification { value: None, .. } => {}
            ast::Expression::Modification {
                target: attr_target,
                value: Some(value),
                ..
            } => {
                let attr_name = attr_target.to_string();
                let preserve_source = preserves_source_scoped_attribute(attr_name.as_str());
                let mut qn = prefix.clone();
                qn.push(attr_name, Vec::new());
                let stored_expr = if preserve_source {
                    value.as_ref().clone()
                } else {
                    resolve_modification_expr(
                        value,
                        ModifierResolveScope {
                            mod_env: ctx.mod_env(),
                            effective_components: nested_ctx.effective_components,
                            tree: nested_ctx.tree,
                            imports: nested_ctx.imports,
                        },
                        false,
                    )?
                };
                ctx.mod_env_mut().add(
                    qn,
                    ast::ModificationValue::with_source_scope_and_prefixes(
                        stored_expr,
                        Some(value.as_ref().clone()),
                        nested_ctx.source_scope.clone(),
                        nested_prefixes.each,
                        nested_prefixes.final_,
                    ),
                );
            }
            ast::Expression::ClassModification {
                target,
                modifications: nested_mods,
                each_flags: nested_each_flags,
                final_flags: nested_final_flags,
                ..
            } => {
                let target_name = target.to_string();
                let mut nested_prefix = prefix.clone();
                nested_prefix.push(target_name, Vec::new());
                process_nested_modifications_recursive(
                    ctx,
                    &nested_prefix,
                    nested_mods,
                    nested_ctx,
                    NestedModificationFlags {
                        prefixes: nested_prefixes,
                        each_flags: nested_each_flags,
                        final_flags: nested_final_flags,
                    },
                )?;
            }
            _ => {}
        }
    }

    Ok(())
}

fn preserves_source_scoped_attribute(attr_name: &str) -> bool {
    matches!(
        attr_name,
        "start" | "min" | "max" | "nominal" | "stateSelect"
    )
}
