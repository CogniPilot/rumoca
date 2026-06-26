use super::inheritance::{
    find_class_in_tree, get_effective_components, is_type_subtype,
    resolve_effective_components_for_eval,
};
use super::nested_scope::remap_redeclare_class_modifier;
use super::path_utils;
use super::type_overrides::{TypeOverrideMap, find_nested_class_in_hierarchy};
use super::{InstantiateContext, InstantiateError, InstantiateResult};
use rumoca_eval_ast::eval_instantiate::{
    InstantiateEvalCtx, evaluate_component_condition, try_eval_enum_expr, try_eval_integer_expr,
    try_eval_string_expr,
};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

mod record_projection;

pub(super) use record_projection::propagate_record_binding_to_fields;

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
    pub(super) type_overrides: &'a TypeOverrideMap,
    pub(super) target_class: Option<&'a ast::ClassDef>,
    pub(super) parent_snapshot: &'a IndexMap<ast::QualifiedName, rumoca_ir_ast::ModificationValue>,
    pub(super) shifted_parent_keys: &'a IndexMap<ast::QualifiedName, ()>,
}

struct ScopedInsertContext<'a> {
    parent_snapshot: &'a IndexMap<ast::QualifiedName, rumoca_ir_ast::ModificationValue>,
    shifted_parent_keys: &'a IndexMap<ast::QualifiedName, ()>,
    source_scope: Option<ast::QualifiedName>,
}

struct ModifierEvalContext<'a> {
    tree: &'a ast::ClassTree,
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
    } = input;
    let eval_ctx = ModifierEvalContext {
        tree,
        effective_components,
        type_overrides,
        target_class,
        insert_ctx: ScopedInsertContext {
            parent_snapshot,
            shifted_parent_keys,
            source_scope: Some(enclosing_modifier_scope(ctx)),
        },
    };

    for (target_name, mod_expr) in &comp.modifications {
        let prefixes = ModifierPrefixes {
            final_: comp.final_attributes.contains(target_name),
            each: comp.each_modifications.contains(target_name),
        };
        apply_component_modifier(ctx, target_name, mod_expr, prefixes, &eval_ctx)?;
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
        ctx.mod_env(),
        effective_components,
        tree,
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
        if mod_value.source.is_some() || mod_value.source_scope.is_some() {
            return Some((
                mod_value
                    .source
                    .clone()
                    .or_else(|| Some(mod_value.value.clone())),
                mod_value.source_scope.clone(),
            ));
        }
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

    if let Some(target_comp) = target_component.as_ref()
        && target_comp.is_final
    {
        let qn = ast::QualifiedName::from_ident(target_name);
        if prefixes.final_
            && let Some(existing) = ctx.mod_env().get(&qn)
            && existing.final_
            && modification_forwards_to_existing(mod_expr, existing, ctx.mod_env())
        {
            return Ok(());
        }
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
            preserve_redeclare_class_modifier(ctx, target_name, mod_expr, eval_ctx);
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
            preserve_redeclare_class_modifier(ctx, target_name, mod_expr, eval_ctx);
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
) {
    // MLS §7.3: preserve class/package redeclare bindings in mod_env so
    // downstream type resolution sees component-level redeclare overrides.
    let is_redeclare_class_target = eval_ctx.target_class.is_some_and(|tc| {
        find_nested_class_in_hierarchy(eval_ctx.tree, tc, target_name)
            .is_some_and(|nested| nested.is_replaceable)
    });
    if is_redeclare_class_target {
        let qn = ast::QualifiedName::from_ident(target_name);
        let resolved_class_mod =
            remap_redeclare_class_modifier(mod_expr, target_name, eval_ctx.type_overrides);
        ctx.mod_env_mut().add(
            qn,
            ast::ModificationValue::with_source_scope(
                resolved_class_mod,
                None,
                eval_ctx.insert_ctx.source_scope.clone(),
            ),
        );
    }
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

fn modification_forwards_to_existing(
    value: &ast::Expression,
    existing: &rumoca_ir_ast::ModificationValue,
    mod_env: &ast::ModificationEnvironment,
) -> bool {
    if existing.value == *value {
        return true;
    }
    let ast::Expression::ComponentReference(cref) = value else {
        return false;
    };
    if cref.parts.len() != 1 {
        return false;
    }
    let qn = ast::QualifiedName::from_ident(cref.parts[0].ident.text.as_ref());
    mod_env
        .get(&qn)
        .is_some_and(|outer| outer.value == existing.value)
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
        if prefixes.final_
            && modification_forwards_to_existing(
                &value,
                ctx.mod_env().get(&key).expect("checked above"),
                ctx.mod_env(),
            )
        {
            return Ok(());
        }
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
        resolve_class_components: &resolve_effective_components_for_eval,
    };
    source_component
        .modifications
        .iter()
        .filter_map(|(field_name, field_expr)| {
            try_eval_integer_expr(&eval_ctx, field_expr).map(|value| {
                (
                    field_name.clone(),
                    ast::Expression::Terminal {
                        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                        token: rumoca_core::Token {
                            text: value.to_string().into(),
                            ..Default::default()
                        },
                        span: field_expr.span(),
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

/// Resolve a modification expression by evaluating component references in scope.
fn resolve_modification_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    allow_string_eval: bool,
) -> InstantiateResult<ast::Expression> {
    resolve_modification_expr_with_depth(
        expr,
        mod_env,
        effective_components,
        tree,
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
        mod_env,
        effective_components,
        tree,
        false,
        ModificationResolveMode::DeclarationBinding,
        0,
    )
}

fn resolve_modification_expr_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    allow_string_eval: bool,
    mode: ModificationResolveMode,
    depth: usize,
) -> InstantiateResult<ast::Expression> {
    if depth > MAX_MOD_RESOLVE_DEPTH {
        return Ok(expr.clone());
    }

    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components: &resolve_effective_components_for_eval,
    };

    // Resolve booleans first (e.g., useFilter=useFilter) so conditional
    // components in nested classes evaluate against the parent's value.
    if let Some(value) = evaluate_component_condition(&eval_ctx, expr) {
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

    // MLS §7.2: Resolve multi-part references through sibling modifications
    // before treating unresolved references as enum literals.
    if let Some(resolved) = resolve_sibling_modification(expr, effective_components, mode) {
        if resolved == *expr {
            return Ok(expr.clone());
        }
        return resolve_modification_expr_with_depth(
            &resolved,
            mod_env,
            effective_components,
            tree,
            allow_string_eval,
            mode,
            depth + 1,
        );
    }

    if let Some(value) = try_eval_enum_expr(&eval_ctx, expr) {
        return Ok(path_utils::component_ref_expr_from_dotted(
            &value,
            expr.span(),
        ));
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
            resolve_single_part_ref_expr(expr, mod_env, effective_components)
    {
        return resolve_modification_expr_with_depth(
            &resolved_ref,
            mod_env,
            effective_components,
            tree,
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
        let indices = literal_integer_subscripts(subscripts)?;
        return index_array_value(&mod_value.value, &indices);
    }

    if effective_components.contains_key(name) {
        return None;
    }

    if let Some(mod_value) = mod_env.get(&qn)
        && mod_value.value != *expr
    {
        return Some(mod_value.value.clone());
    }

    None
}

fn literal_integer_subscripts(subscripts: &[ast::Subscript]) -> Option<Vec<i64>> {
    subscripts
        .iter()
        .map(|subscript| {
            let ast::Subscript::Expression(ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token,
                ..
            }) = subscript
            else {
                return None;
            };
            token.text.parse::<i64>().ok()
        })
        .collect()
}

fn index_array_value(expr: &ast::Expression, indices: &[i64]) -> Option<ast::Expression> {
    let (&first, rest) = indices.split_first()?;
    match expr {
        ast::Expression::Array { elements, .. } => {
            let index = first.checked_sub(1)? as usize;
            let selected = elements.get(index)?;
            if rest.is_empty() {
                Some(selected.clone())
            } else {
                index_array_value(selected, rest)
            }
        }
        ast::Expression::Parenthesized { inner, .. } => index_array_value(inner, indices),
        _ => None,
    }
}

/// Resolve a multi-part component reference by following sibling modifications.
fn resolve_sibling_modification(
    expr: &ast::Expression,
    effective_components: &IndexMap<String, ast::Component>,
    mode: ModificationResolveMode,
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
    if mode == ModificationResolveMode::DeclarationBinding
        && matches!(mod_expr, ast::Expression::ComponentReference(_))
    {
        return Some(expr.clone());
    }
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
) -> InstantiateResult<()> {
    for (idx, nested_mod) in modifications.iter().enumerate() {
        let nested_prefixes = ModifierPrefixes {
            each: flags.prefixes.each || flags.each_flags.get(idx).copied().unwrap_or(false),
            final_: flags.prefixes.final_ || flags.final_flags.get(idx).copied().unwrap_or(false),
        };
        match nested_mod {
            ast::Expression::Modification {
                target: attr_target,
                value,
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
                        ctx.mod_env(),
                        nested_ctx.effective_components,
                        nested_ctx.tree,
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
    matches!(attr_name, "start" | "min" | "max" | "nominal")
}

#[cfg(test)]
#[path = "mod_env_tests.rs"]
mod mod_env_tests;
