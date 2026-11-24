#[cfg(test)]
mod mod_env_tests;
mod record_projection;
#[cfg(test)]
mod tests;

use super::inheritance::{
    find_class_in_tree, get_effective_components, is_type_subtype, nested_arguments,
    resolve_effective_components_for_eval,
};
use super::nested_scope::{SelectedClassModifiers, target_path};
use super::type_overrides::ClassModifierValues;
use super::{InstantiateContext, InstantiateError, InstantiateResult};
use rumoca_eval_ast::eval_instantiate::{
    InstantiateEvalCtx, evaluate_component_condition, try_eval_integer_expr, try_eval_string_expr,
};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

pub(super) use record_projection::{RecordBindingProjection, propagate_record_binding_to_fields};

const MAX_MOD_RESOLVE_DEPTH: usize = 20;

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
    pub(super) target_class: Option<&'a ast::ClassDef>,
    pub(super) class_overrides: &'a ast::ClassOverrideMap,
    pub(super) parent_snapshot: &'a IndexMap<ast::QualifiedName, rumoca_ir_ast::ModificationValue>,
    pub(super) shifted_parent_keys: &'a IndexMap<ast::QualifiedName, ()>,
    /// Import aliases of the class that wrote these modifications (MLS §13.2).
    pub(super) modifier_imports: &'a [(String, String)],
}

struct ScopedInsertContext<'a> {
    parent_snapshot: &'a IndexMap<ast::QualifiedName, rumoca_ir_ast::ModificationValue>,
    shifted_parent_keys: &'a IndexMap<ast::QualifiedName, ()>,
    source_scope: Option<ast::QualifiedName>,
    /// Import aliases of the class that wrote these modifications (MLS §13.2).
    imports: &'a [(String, String)],
}

struct ModifierEvalContext<'a> {
    tree: &'a ast::ClassTree,
    effective_components: &'a IndexMap<String, ast::Component>,
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
    imports: &'a [(String, String)],
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
) -> InstantiateResult<SelectedClassModifiers> {
    let PopulateModEnvInput {
        comp,
        effective_components,
        target_class,
        class_overrides,
        parent_snapshot,
        shifted_parent_keys,
        modifier_imports,
    } = input;
    let eval_ctx = ModifierEvalContext {
        tree,
        effective_components,
        target_class,
        insert_ctx: ScopedInsertContext {
            parent_snapshot,
            shifted_parent_keys,
            source_scope: Some(enclosing_modifier_scope(ctx)),
            imports: modifier_imports,
        },
    };

    let mut selected_modifiers = IndexMap::default();
    for (target_name, mod_expr) in &comp.modifications {
        let prefixes = ModifierPrefixes {
            final_: comp.final_attributes.contains(target_name),
            each: comp.each_modifications.contains(target_name),
        };
        let mut owners = class_overrides
            .values()
            .filter(|selection| selection.alias == *target_name)
            .peekable();
        if let Some(owner) = owners.peek()
            && let ast::Expression::ClassModification {
                modifications,
                each_flags,
                final_flags,
                ..
            } = mod_expr
        {
            let values = evaluate_selected_class_modifiers(
                ctx,
                owner,
                &eval_ctx,
                modifications,
                NestedModificationFlags {
                    prefixes,
                    each_flags,
                    final_flags,
                },
            )?;
            preserve_redeclare_class_modifier(ctx, owner, mod_expr, &eval_ctx);
            for owner in owners {
                selected_modifiers.insert(owner.alias_def_id, values.clone());
            }
        } else {
            apply_component_modifier(ctx, target_name, mod_expr, prefixes, &eval_ctx)?;
        }
    }
    Ok(selected_modifiers)
}

/// Evaluate the written arguments inside their resolved selection's own
/// modifier environment. An enclosing alias with the same name contributes
/// only when the caller forwards its exact declaration.
fn evaluate_selected_class_modifiers(
    ctx: &mut InstantiateContext,
    owner: &ast::ClassOverride,
    eval_ctx: &ModifierEvalContext<'_>,
    modifications: &[ast::Expression],
    flags: NestedModificationFlags<'_>,
) -> InstantiateResult<ClassModifierValues> {
    let mut written = ClassModifierValues::default();
    process_nested_modifications_recursive(
        ctx,
        &ast::QualifiedName::new(),
        modifications,
        &NestedModificationContext {
            effective_components: eval_ctx.effective_components,
            tree: eval_ctx.tree,
            source_scope: eval_ctx.insert_ctx.source_scope.clone(),
            imports: eval_ctx.insert_ctx.imports,
        },
        flags,
        Some(&mut written),
    )?;

    let mut bindings = ctx
        .mod_env()
        .active
        .iter()
        .filter(|(name, _)| eval_ctx.insert_ctx.shifted_parent_keys.contains_key(*name))
        .filter_map(|(name, value)| Some((name.strip_prefix(&owner.alias)?, value.clone())))
        .collect();
    add_class_modifier_defaults(
        &mut bindings,
        &written,
        |_| true,
        eval_ctx.tree,
        eval_ctx.tree.get_class_by_def_id(owner.target_def_id),
    )?;
    Ok(ClassModifierValues {
        bindings,
        final_paths: written.final_paths,
    })
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
    let qn = target_path(target_name);
    let (binding_source, binding_source_scope) = inherited_modifier_source_metadata(
        target_name,
        value_expr,
        ctx.mod_env(),
        effective_components,
    )
    .map_or(
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
    enclosing_components: &IndexMap<String, ast::Component>,
) -> Option<(Option<ast::Expression>, Option<ast::QualifiedName>)> {
    // A resolved reference to an enclosing declaration is a parameter
    // dependency. Its own lexical expression and scope remain authoritative.
    if let ast::Expression::ComponentReference(reference) = expr
        && reference.root_def_id().is_some_and(|definition| {
            enclosing_components
                .values()
                .any(|component| component.def_id == Some(definition))
        })
    {
        return None;
    }
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
    let prefix = target_path(target_name);
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
        None,
    )
}

fn preserve_redeclare_class_modifier(
    ctx: &mut InstantiateContext,
    owner: &ast::ClassOverride,
    mod_expr: &ast::Expression,
    eval_ctx: &ModifierEvalContext<'_>,
) {
    let qn = ast::QualifiedName::from_ident(&owner.alias);
    let mut resolved_class_mod = mod_expr.clone();
    if let ast::Expression::ClassModification { target, .. } = &mut resolved_class_mod {
        target.set_target_def_id(Some(owner.target_def_id));
    }
    if !eval_ctx.insert_ctx.shifted_parent_keys.contains_key(&qn) {
        ctx.mod_env_mut().active.shift_remove(&qn);
    }
    ctx.mod_env_mut().add(
        qn,
        ast::ModificationValue::with_source_scope(
            resolved_class_mod,
            None,
            eval_ctx.insert_ctx.source_scope.clone(),
        ),
    );
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

/// The occurrence's modifiers take precedence over the selected class's own
/// modifiers. Both sets already carry the scope where each value was written.
pub(super) fn add_class_modifier_defaults(
    outer: &mut IndexMap<ast::QualifiedName, ast::ModificationValue>,
    class_modifiers: &ClassModifierValues,
    is_outer_modifier: impl Fn(&ast::QualifiedName) -> bool,
    tree: &ast::ClassTree,
    class: Option<&ast::ClassDef>,
) -> InstantiateResult<()> {
    let final_paths = class_modifiers.final_paths.iter().chain(
        class_modifiers
            .bindings
            .iter()
            .filter_map(|(name, value)| value.final_.then_some(name)),
    );
    for path in final_paths {
        let descendant = outer
            .iter()
            .find(|(name, _)| is_outer_modifier(name) && name.parts.starts_with(&path.parts))
            .map(|(_, value)| value);
        let replacing = match descendant {
            Some(value) => Some(value),
            None => replacing_modifier(outer, path, &is_outer_modifier, tree, class)?,
        };
        if let Some(override_value) = replacing {
            let span = required_binding_source_span(
                override_value.source.as_ref(),
                &override_value.value,
                "final redeclared class modifier",
            )?;
            return Err(Box::new(InstantiateError::redeclare_final(
                path.to_flat_string(),
                span,
            )));
        }
    }
    let mut admitted = Vec::new();
    for (name, modifier) in &class_modifiers.bindings {
        if !(is_outer_modifier(name) && outer.contains_key(name))
            && replacing_modifier(outer, name, &is_outer_modifier, tree, class)?.is_none()
        {
            admitted.push((name.clone(), modifier.clone()));
        }
    }
    outer.extend(admitted);
    Ok(())
}

fn replacing_modifier<'a>(
    outer: &'a IndexMap<ast::QualifiedName, ast::ModificationValue>,
    path: &ast::QualifiedName,
    is_outer_modifier: &impl Fn(&ast::QualifiedName) -> bool,
    tree: &ast::ClassTree,
    class: Option<&ast::ClassDef>,
) -> InstantiateResult<Option<&'a ast::ModificationValue>> {
    for (name, value) in outer {
        if !is_outer_modifier(name)
            || name.parts.len() >= path.parts.len()
            || !path.parts.starts_with(&name.parts)
        {
            continue;
        }
        let segments = name
            .parts
            .iter()
            .map(|(segment, _)| rumoca_core::strip_all_subscripts(segment))
            .collect::<Vec<_>>();
        let arguments = nested_arguments(tree, class, &segments).ok_or_else(|| {
            Box::new(InstantiateError::redeclare_error(
                name.to_flat_string(),
                "the type of a binding above inherited class modifiers is unresolved",
                value.value.span(),
            ))
        })?;
        let next = &path.parts[..name.parts.len() + 1];
        let restated = outer
            .keys()
            .any(|other| is_outer_modifier(other) && other.parts.starts_with(next));
        if !arguments.retains_below_binding(restated) {
            return Ok(Some(value));
        }
    }
    Ok(None)
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
    // MLS §7.2.4: an enclosing modifier that binds the whole source record
    // overrides the field modifiers of its declaration.
    let whole_record_is_bound = mod_env
        .get(&ast::QualifiedName::from_ident(source_name))
        .is_some();
    let mut values: Vec<_> = source_component
        .modifications
        .iter()
        .filter(|_| !whole_record_is_bound)
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
        .collect();
    collect_bound_structural_integer_fields(
        source_component,
        comp_ref,
        &eval_ctx,
        expr.span(),
        &mut values,
    );
    values
}

fn collect_bound_structural_integer_fields(
    source_component: &ast::Component,
    comp_ref: &ast::ComponentReference,
    eval_ctx: &InstantiateEvalCtx<'_>,
    span: rumoca_core::Span,
    values: &mut Vec<(String, ast::Expression)>,
) {
    // MLS §§7.2, 10.1, 12.6: settle fields in the writing scope before the
    // record reference enters the child. Both constructor bindings and defaults
    // can own structural dimensions even without any field modifiers.
    if let Some(record) = source_component
        .type_def_id
        .and_then(|id| eval_ctx.tree.get_class_by_def_id(id))
    {
        for (field_name, field) in resolve_effective_components_for_eval(eval_ctx.tree, record) {
            if values.iter().any(|(name, _)| name == &field_name) || field.def_id.is_none() {
                continue;
            }
            let field_type = super::type_lookup::resolve_primitive_type_id(
                eval_ctx.tree,
                &field.type_name.to_string(),
                field
                    .type_def_id
                    .and_then(|id| eval_ctx.tree.get_class_by_def_id(id)),
            );
            if field_type != eval_ctx.tree.type_table.integer()
                || !matches!(
                    field.variability,
                    rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
                )
            {
                continue;
            }
            let mut reference = comp_ref.clone();
            reference.parts.push(ast::ComponentRefPart {
                ident: field.name_token.clone(),
                subs: None,
                def_id: field.def_id,
            });
            let expression = ast::Expression::ComponentReference(reference);
            if let Some(value) = try_eval_integer_expr(eval_ctx, &expression) {
                values.push((
                    field_name,
                    ast::Expression::Terminal {
                        terminal_type: ast::TerminalType::UnsignedInteger,
                        token: rumoca_core::Token {
                            text: value.to_string().into(),
                            ..Default::default()
                        },
                        span,
                    },
                ));
            }
        }
    }
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
    imports: &'a [(String, String)],
}

/// Resolve a modification expression by evaluating component references in scope.
fn resolve_modification_expr(
    expr: &ast::Expression,
    scope: ModifierResolveScope<'_>,
    allow_string_eval: bool,
) -> InstantiateResult<ast::Expression> {
    resolve_modification_expr_with_depth(
        expr,
        scope,
        allow_string_eval,
        ModificationResolveMode::Modifier,
        0,
    )
}

pub(super) fn resolve_declaration_binding_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> InstantiateResult<ast::Expression> {
    resolve_modification_expr_with_depth(
        expr,
        ModifierResolveScope {
            mod_env,
            effective_components,
            tree,
            imports: &[],
        },
        false,
        ModificationResolveMode::DeclarationBinding,
        0,
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
) -> Option<bool> {
    if let Some(value) = evaluate_component_condition(eval_ctx, expr) {
        return Some(value);
    }
    if !crate::dims::expr_mentions_import_alias(expr, scope.imports) {
        return None;
    }
    let qualified = crate::dims::qualify_shape_expr_imports(scope.tree, expr, scope.imports);
    evaluate_component_condition(eval_ctx, &qualified)
}

fn resolve_modification_expr_with_depth(
    expr: &ast::Expression,
    scope: ModifierResolveScope<'_>,
    allow_string_eval: bool,
    mode: ModificationResolveMode,
    depth: usize,
) -> InstantiateResult<ast::Expression> {
    if depth > MAX_MOD_RESOLVE_DEPTH {
        return Ok(expr.clone());
    }
    let ModifierResolveScope {
        mod_env,
        effective_components,
        tree,
        imports: _,
    } = scope;

    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };

    // Resolve booleans first (e.g., useFilter=useFilter) so conditional
    // components in nested classes evaluate against the parent's value.
    if let Some(value) = decide_boolean_modifier(expr, scope, &eval_ctx) {
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
        && let Some(resolved_ref) =
            resolve_single_part_ref_expr(expr, mod_env, effective_components, tree)
    {
        return resolve_modification_expr_with_depth(
            &resolved_ref,
            scope,
            allow_string_eval,
            mode,
            depth + 1,
        );
    }

    // MLS §7.2: Resolve multi-part references through sibling modifications.
    if let Some(resolved) = resolve_sibling_modification(expr, effective_components) {
        return resolve_modification_expr_with_depth(
            &resolved,
            scope,
            allow_string_eval,
            mode,
            depth + 1,
        );
    }

    Ok(expr.clone())
}

fn resolve_single_part_ref_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<ast::Expression> {
    let ast::Expression::ComponentReference(comp_ref) = expr else {
        return None;
    };
    if comp_ref.parts.len() != 1 {
        return None;
    }

    let name = comp_ref.parts[0].ident.text.as_ref();
    let qn = ast::QualifiedName::from_ident(name);

    if let Some(subscripts) = comp_ref.parts[0].subs.as_ref() {
        let mod_value = mod_env.get(&qn)?;
        return select_array_value(&mod_value.value, subscripts);
    }

    if let Some(mod_value) = mod_env.get(&qn)
        && mod_value.value != *expr
    {
        return Some(mod_value.value.clone());
    }

    // A modifier is evaluated in the scope where it is written (MLS §7.2.4).
    // Enumeration values have no arithmetic evaluator, so follow an enum
    // parameter's declaration binding here until the chain settles to a literal.
    // Other component references deliberately retain their identity: inlining a
    // numeric binding, for example, would change tensor-family ownership.
    let component = effective_components.get(name)?;
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
        return None;
    }
    component.binding.clone()
}

fn select_array_value(
    mut expr: &ast::Expression,
    subscripts: &[ast::Subscript],
) -> Option<ast::Expression> {
    subscripts.first()?;
    for subscript in subscripts {
        let ast::Subscript::Expression(ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        }) = subscript
        else {
            return None;
        };
        let ast::Expression::Array { elements, .. } = expr else {
            return None;
        };
        expr = elements.get(token.text.parse::<usize>().ok()?.checked_sub(1)?)?;
    }
    Some(expr.clone())
}

/// Resolve a multi-part component reference by following sibling modifications.
fn resolve_sibling_modification(
    expr: &ast::Expression,
    effective_components: &IndexMap<String, ast::Component>,
) -> Option<ast::Expression> {
    let ast::Expression::ComponentReference(comp_ref) = expr else {
        return None;
    };
    if comp_ref.parts.len() < 2 {
        return None;
    }
    let first = comp_ref.parts[0].ident.text.as_ref();
    let second = comp_ref.parts[1].ident.text.as_ref();
    let comp = effective_components.get(first)?;
    let mod_expr = comp.modifications.get(second)?;
    // Keep record/class-modification bindings as references so declaration
    // defaults remain visible during record-field projection.
    if is_non_scalar_sibling_modifier_expr(mod_expr) {
        return None;
    }
    Some(mod_expr.clone())
}

fn is_non_scalar_sibling_modifier_expr(expr: &ast::Expression) -> bool {
    match expr {
        ast::Expression::ClassModification { .. }
        | ast::Expression::FunctionCall { .. }
        | ast::Expression::Array { .. }
        | ast::Expression::Tuple { .. }
        | ast::Expression::Range { .. }
        | ast::Expression::ArrayComprehension { .. } => true,
        ast::Expression::Modification { value, .. } => is_non_scalar_sibling_modifier_expr(value),
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
    mut selected: Option<&mut ClassModifierValues>,
) -> InstantiateResult<()> {
    for (idx, nested_mod) in modifications.iter().enumerate() {
        let nested_prefixes = ModifierPrefixes {
            each: flags.prefixes.each || flags.each_flags.get(idx).copied().unwrap_or(false),
            final_: flags.prefixes.final_ || flags.final_flags.get(idx).copied().unwrap_or(false),
        };
        let (target, nested, value) = match nested_mod {
            ast::Expression::Modification { target, value, .. } => {
                (target, None, Some(value.as_ref()))
            }
            ast::Expression::ClassModification { target, .. } => (target, Some(nested_mod), None),
            ast::Expression::Binary {
                op: rumoca_core::OpBinary::Assign,
                lhs,
                rhs,
                ..
            } => {
                let ast::Expression::ClassModification { target, .. } = lhs.as_ref() else {
                    continue;
                };
                (target, Some(lhs.as_ref()), Some(rhs.as_ref()))
            }
            _ => continue,
        };
        let mut path = prefix.clone();
        path.parts
            .extend(ast::QualifiedName::from_component_reference(target).parts);
        if nested_prefixes.final_
            && value.is_none()
            && let Some(selection) = selected.as_deref_mut()
        {
            selection.final_paths.push(path.clone());
        }
        if let Some(ast::Expression::ClassModification {
            modifications,
            each_flags,
            final_flags,
            ..
        }) = nested
        {
            process_nested_modifications_recursive(
                ctx,
                &path,
                modifications,
                nested_ctx,
                NestedModificationFlags {
                    prefixes: nested_prefixes,
                    each_flags,
                    final_flags,
                },
                selected.as_deref_mut(),
            )?;
        }
        if let Some(value) = value {
            let binding = nested_modifier_binding(ctx, target, value, nested_ctx, nested_prefixes)?;
            if let Some(selection) = selected.as_deref_mut() {
                selection.bindings.entry(path).or_insert(binding);
            } else {
                ctx.mod_env_mut().add(path, binding);
            }
        }
    }

    Ok(())
}

fn nested_modifier_binding(
    ctx: &InstantiateContext,
    target: &ast::ComponentReference,
    value: &ast::Expression,
    nested_ctx: &NestedModificationContext<'_>,
    prefixes: ModifierPrefixes,
) -> InstantiateResult<ast::ModificationValue> {
    let resolved = if preserves_source_scoped_attribute(&target.to_string()) {
        value.clone()
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
    Ok(ast::ModificationValue::with_source_scope_and_prefixes(
        resolved,
        Some(value.clone()),
        nested_ctx.source_scope.clone(),
        prefixes.each,
        prefixes.final_,
    ))
}

fn preserves_source_scoped_attribute(attr_name: &str) -> bool {
    matches!(
        attr_name,
        "start" | "min" | "max" | "nominal" | "stateSelect"
    )
}
