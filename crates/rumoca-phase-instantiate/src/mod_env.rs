use super::evaluator::{evaluate_component_condition, try_eval_integer_expr, try_eval_string_expr};
use super::inheritance::get_effective_components;
use super::nested_scope::remap_redeclare_class_modifier;
use super::type_overrides::find_nested_class_in_hierarchy;
use super::{InstantiateContext, InstantiateError, InstantiateResult};
use indexmap::IndexMap;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use std::sync::Arc;

const MAX_MOD_RESOLVE_DEPTH: usize = 20;

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
    pub(super) type_overrides: &'a IndexMap<String, DefId>,
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
    type_overrides: &'a IndexMap<String, DefId>,
    target_class: Option<&'a ast::ClassDef>,
    insert_ctx: ScopedInsertContext<'a>,
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
        apply_component_modifier(ctx, target_name, mod_expr, &eval_ctx)?;
    }
    Ok(())
}

fn insert_modifier_value_with_structural_overrides(
    ctx: &mut InstantiateContext,
    target_name: &str,
    value_expr: &ast::Expression,
    allow_string_eval: bool,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    insert_ctx: &ScopedInsertContext<'_>,
) -> InstantiateResult<()> {
    let qn = ast::QualifiedName::from_ident(target_name);
    let (binding_source, binding_source_scope) =
        inherited_modifier_source_metadata(target_name, value_expr, ctx.mod_env()).map_or(
            (Some(value_expr.clone()), insert_ctx.source_scope.clone()),
            |(src, scope)| (src.or_else(|| Some(value_expr.clone())), scope),
        );
    let resolved_expr = resolve_modification_expr(
        value_expr,
        ctx.mod_env(),
        effective_components,
        tree,
        allow_string_eval,
    )?;
    let structural_field_overrides = collect_structural_integer_fields_from_sibling_reference(
        value_expr,
        ctx.mod_env(),
        effective_components,
        tree,
    );
    insert_scoped_modifier_binding(
        ctx,
        qn,
        resolved_expr,
        binding_source,
        binding_source_scope.clone(),
        insert_ctx.parent_snapshot,
        insert_ctx.shifted_parent_keys,
    );
    for (field_name, field_value) in structural_field_overrides {
        let field_qn = ast::QualifiedName::from_ident(target_name).child(&field_name);
        insert_scoped_modifier_binding(
            ctx,
            field_qn,
            field_value,
            None,
            binding_source_scope.clone(),
            insert_ctx.parent_snapshot,
            insert_ctx.shifted_parent_keys,
        );
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
    eval_ctx: &ModifierEvalContext<'_>,
) -> InstantiateResult<()> {
    let allow_string_eval = eval_ctx
        .target_class
        .and_then(|tc| tc.components.get(target_name))
        .is_some_and(|target_comp| {
            let type_name = target_comp.type_name.to_string();
            type_name == "String" || type_name.ends_with(".String")
        });

    // INST-010: Check if target component is final in the target class (MLS §7.2.6)
    if let Some(tc) = eval_ctx.target_class
        && let Some(target_comp) = tc.components.get(target_name)
        && target_comp.is_final
    {
        return Err(Box::new(InstantiateError::redeclare_final(
            target_name,
            rumoca_core::Span::DUMMY,
        )));
    }

    match mod_expr {
        // Nested class modification: l2(x(start = 100))
        ast::Expression::ClassModification { modifications, .. } => {
            // Process nested modifications recursively.
            let prefix = ast::QualifiedName::from_ident(target_name);
            process_nested_modifications_recursive(
                ctx,
                &prefix,
                modifications,
                eval_ctx.effective_components,
                eval_ctx.tree,
                eval_ctx.insert_ctx.source_scope.clone(),
            )?;

            // MLS §7.3: preserve class/package redeclare bindings in mod_env so
            // downstream type resolution sees component-level redeclare overrides.
            let is_redeclare_class_target = eval_ctx.target_class.is_some_and(|tc| {
                find_nested_class_in_hierarchy(eval_ctx.tree, tc, target_name)
                    .is_some_and(|nested| nested.is_replaceable)
                    || tc
                        .components
                        .get(target_name)
                        .is_some_and(|target_comp| target_comp.is_replaceable)
            });
            if is_redeclare_class_target {
                let qn = ast::QualifiedName::from_ident(target_name);
                let resolved_class_mod = remap_redeclare_class_modifier(
                    eval_ctx.tree,
                    mod_expr,
                    target_name,
                    eval_ctx.type_overrides,
                );
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
        // Nested class modification WITH binding: field(start=X) = expr.
        // MLS §7.2: process both nested attribute modifications and binding.
        ast::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Assign(_),
            lhs,
            rhs,
        } => {
            if let ast::Expression::ClassModification { modifications, .. } = &**lhs {
                let prefix = ast::QualifiedName::from_ident(target_name);
                process_nested_modifications_recursive(
                    ctx,
                    &prefix,
                    modifications,
                    eval_ctx.effective_components,
                    eval_ctx.tree,
                    eval_ctx.insert_ctx.source_scope.clone(),
                )?;
            }
            insert_modifier_value_with_structural_overrides(
                ctx,
                target_name,
                rhs,
                allow_string_eval,
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
                allow_string_eval,
                eval_ctx.effective_components,
                eval_ctx.tree,
                &eval_ctx.insert_ctx,
            )?;
        }
    }

    Ok(())
}

fn insert_scoped_modifier_binding(
    ctx: &mut InstantiateContext,
    key: ast::QualifiedName,
    value: ast::Expression,
    source: Option<ast::Expression>,
    source_scope: Option<ast::QualifiedName>,
    parent_snapshot: &IndexMap<ast::QualifiedName, rumoca_ir_ast::ModificationValue>,
    shifted_parent_keys: &IndexMap<ast::QualifiedName, ()>,
) {
    // MLS §7.2: local modifier bindings must replace colliding parent-scope keys.
    // Preserve explicitly shifted parent keys because those are real nested overrides.
    let replace_parent =
        parent_snapshot.contains_key(&key) && !shifted_parent_keys.contains_key(&key);
    let mod_env = ctx.mod_env_mut();
    if replace_parent {
        mod_env.active.shift_remove(&key);
    }
    mod_env.add(
        key,
        rumoca_ir_ast::ModificationValue::with_source_scope(value, source, source_scope),
    );
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

    source_component
        .modifications
        .iter()
        .filter_map(|(field_name, field_expr)| {
            try_eval_integer_expr(field_expr, mod_env, effective_components, tree).map(|value| {
                (
                    field_name.clone(),
                    ast::Expression::Terminal {
                        terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                        token: rumoca_ir_core::Token {
                            text: value.to_string().into(),
                            ..Default::default()
                        },
                    },
                )
            })
        })
        .collect()
}

/// Propagate a record binding to scalar field bindings.
///
/// MLS §7.2: a record binding like `Complex u = expr` projects bindings for fields
/// (for example `re = expr.re`, `im = expr.im`).
pub(super) fn propagate_record_binding_to_fields(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    binding_expr: &ast::Expression,
    binding_source_scope: Option<ast::QualifiedName>,
    nested_class: &ast::ClassDef,
    targeted_keys: &IndexMap<ast::QualifiedName, ()>,
) {
    // MLS §7.2 record binding projection applies only to record components.
    // For non-record classes (model/block/connector), class modifications must
    // remain component modifiers and must not synthesize per-field bindings.
    if nested_class.class_type != ast::ClassType::Record {
        return;
    }

    // Get effective components including inherited ones (MLS §7.2).
    // For type aliases like `ComplexVoltage = Complex(...)`, direct components may
    // be empty while fields come from a base class.
    let effective = get_effective_components(tree, nested_class).unwrap_or_default();
    let components: &IndexMap<String, ast::Component> = if effective.is_empty() {
        &nested_class.components
    } else {
        &effective
    };
    let preserve_declared_defaults = is_default_record_constructor_call(binding_expr, nested_class);

    for (field_name, field_comp) in components {
        let field_qn = ast::QualifiedName::from_ident(field_name);
        // Preserve explicit field modifiers targeting this record component
        // (either local `state(T=...)` or shifted parent `comp.state.T=...`).
        if targeted_keys.contains_key(&field_qn) {
            continue;
        }
        // MLS §12.6 (record constructors): `R()` without arguments uses the
        // record's declared field defaults. Keep existing field defaults instead
        // of replacing them with synthetic `R().field` bindings.
        if preserve_declared_defaults && has_declared_field_default(field_comp) {
            continue;
        }

        let field_access = project_record_field_binding(binding_expr, field_name);

        ctx.mod_env_mut().active.insert(
            field_qn,
            ast::ModificationValue::with_source_scope(
                field_access.clone(),
                Some(field_access),
                binding_source_scope.clone(),
            ),
        );
    }
}

fn project_record_field_binding(
    binding_expr: &ast::Expression,
    field_name: &str,
) -> ast::Expression {
    match binding_expr {
        ast::Expression::If {
            branches,
            else_branch,
        } => ast::Expression::If {
            branches: branches
                .iter()
                .map(|(cond, branch_expr)| {
                    (
                        cond.clone(),
                        project_record_field_binding(branch_expr, field_name),
                    )
                })
                .collect(),
            else_branch: Arc::new(project_record_field_binding(else_branch, field_name)),
        },
        ast::Expression::Parenthesized { inner } => ast::Expression::Parenthesized {
            inner: Arc::new(project_record_field_binding(inner, field_name)),
        },
        _ => ast::Expression::FieldAccess {
            base: Arc::new(binding_expr.clone()),
            field: field_name.to_string(),
        },
    }
}

fn is_default_record_constructor_call(
    expr: &ast::Expression,
    nested_class: &ast::ClassDef,
) -> bool {
    match expr {
        // MLS §12.6: only `R()` for the declared record `R` preserves the
        // record's own field defaults. A different zero-argument record
        // constructor (e.g. `BaseData x = Derived()`) must still project the
        // bound record fields rather than freezing the declared base defaults.
        ast::Expression::FunctionCall { comp, args } => {
            args.is_empty() && record_constructor_matches_class(comp, nested_class)
        }
        ast::Expression::Parenthesized { inner } => {
            is_default_record_constructor_call(inner, nested_class)
        }
        _ => false,
    }
}

fn record_constructor_matches_class(
    comp: &ast::ComponentReference,
    nested_class: &ast::ClassDef,
) -> bool {
    if let (Some(comp_def_id), Some(class_def_id)) = (comp.def_id, nested_class.def_id) {
        return comp_def_id == class_def_id;
    }

    comp.parts
        .last()
        .is_some_and(|part| part.ident.text.as_ref() == nested_class.name.text.as_ref())
}

fn has_declared_field_default(comp: &ast::Component) -> bool {
    comp.binding.is_some() || !matches!(comp.start, ast::Expression::Empty)
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
        0,
    )
}

fn resolve_modification_expr_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    allow_string_eval: bool,
    depth: usize,
) -> InstantiateResult<ast::Expression> {
    if depth > MAX_MOD_RESOLVE_DEPTH {
        return Ok(expr.clone());
    }

    // Resolve booleans first (e.g., useFilter=useFilter) so conditional
    // components in nested classes evaluate against the parent's value.
    if let Some(value) = evaluate_component_condition(expr, mod_env, effective_components, tree) {
        return Ok(ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::Bool,
            token: rumoca_ir_core::Token {
                text: value.to_string().into(),
                ..Default::default()
            },
        });
    }

    // Resolve string-valued modifiers in the parent scope so nested conditional
    // components can evaluate against concrete values (e.g. "D"/"Y").
    if allow_string_eval
        && let Some(value) = try_eval_string_expr(expr, mod_env, effective_components, tree)
    {
        return Ok(ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::String,
            token: rumoca_ir_core::Token {
                text: format!("\"{value}\"").into(),
                ..Default::default()
            },
        });
    }

    // Try to evaluate as an integer (common for array dimension parameters).
    if let Some(value) = try_eval_integer_expr(expr, mod_env, effective_components, tree) {
        return Ok(ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_ir_core::Token {
                text: value.to_string().into(),
                ..Default::default()
            },
        });
    }

    // Resolve direct references in current scope (e.g. resolveInFrame=resolveInFrame).
    if let Some(resolved_ref) = resolve_single_part_ref_expr(expr, mod_env) {
        return resolve_modification_expr_with_depth(
            &resolved_ref,
            mod_env,
            effective_components,
            tree,
            allow_string_eval,
            depth + 1,
        );
    }

    // MLS §7.2: Resolve multi-part references through sibling modifications.
    if let Some(resolved) = resolve_sibling_modification(expr, effective_components) {
        return resolve_modification_expr_with_depth(
            &resolved,
            mod_env,
            effective_components,
            tree,
            allow_string_eval,
            depth + 1,
        );
    }

    Ok(expr.clone())
}

fn resolve_single_part_ref_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
) -> Option<ast::Expression> {
    let ast::Expression::ComponentReference(comp_ref) = expr else {
        return None;
    };
    if comp_ref.parts.len() != 1 {
        return None;
    }

    let name = comp_ref.parts[0].ident.text.as_ref();
    let qn = ast::QualifiedName::from_ident(name);

    if let Some(mod_value) = mod_env.get(&qn)
        && mod_value.value != *expr
    {
        return Some(mod_value.value.clone());
    }

    None
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
        ast::Expression::Parenthesized { inner } => is_non_scalar_sibling_modifier_expr(inner),
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
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    source_scope: Option<ast::QualifiedName>,
) -> InstantiateResult<()> {
    for nested_mod in modifications {
        match nested_mod {
            ast::Expression::Modification {
                target: attr_target,
                value,
            } => {
                let attr_name = attr_target.to_string();
                let mut qn = prefix.clone();
                qn.push(attr_name, Vec::new());
                let resolved_expr = resolve_modification_expr(
                    value,
                    ctx.mod_env(),
                    effective_components,
                    tree,
                    false,
                )?;
                ctx.mod_env_mut().add(
                    qn,
                    ast::ModificationValue::with_source_scope(
                        resolved_expr,
                        Some(value.as_ref().clone()),
                        source_scope.clone(),
                    ),
                );
            }
            ast::Expression::ClassModification {
                target,
                modifications: nested_mods,
            } => {
                let target_name = target.to_string();
                let mut nested_prefix = prefix.clone();
                nested_prefix.push(target_name, Vec::new());
                process_nested_modifications_recursive(
                    ctx,
                    &nested_prefix,
                    nested_mods,
                    effective_components,
                    tree,
                    source_scope.clone(),
                )?;
            }
            _ => {}
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_token(text: &str) -> rumoca_ir_core::Token {
        rumoca_ir_core::Token {
            text: std::sync::Arc::from(text),
            location: rumoca_ir_core::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn make_int_expr(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token(&value.to_string()),
        }
    }

    fn make_comp_ref_expr(names: &[&str]) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: names
                .iter()
                .map(|name| ast::ComponentRefPart {
                    ident: make_token(name),
                    subs: None,
                })
                .collect(),
            def_id: None,
        })
    }

    fn make_named_arg(name: &str, value: ast::Expression) -> ast::Expression {
        ast::Expression::NamedArgument {
            name: make_token(name),
            value: Arc::new(value),
        }
    }

    fn active_mod_env_keys(ctx: &InstantiateContext) -> Vec<String> {
        ctx.mod_env()
            .active
            .keys()
            .map(ToString::to_string)
            .collect()
    }

    #[test]
    fn test_resolve_sibling_modification_keeps_class_modification_reference() {
        let mut effective_components: IndexMap<String, ast::Component> = IndexMap::new();
        let mut data = ast::Component {
            name: "aimcData".to_string(),
            ..Default::default()
        };
        data.modifications.insert(
            "statorCoreParameters".to_string(),
            ast::Expression::ClassModification {
                target: ast::ComponentReference {
                    local: false,
                    parts: vec![
                        ast::ComponentRefPart {
                            ident: make_token("Modelica"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Electrical"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Machines"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Losses"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("CoreParameters"),
                            subs: None,
                        },
                    ],
                    def_id: None,
                },
                modifications: vec![
                    make_named_arg("PRef", make_int_expr(410)),
                    make_named_arg("VRef", make_int_expr(388)),
                ],
            },
        );
        effective_components.insert("aimcData".to_string(), data);

        let expr = make_comp_ref_expr(&["aimcData", "statorCoreParameters"]);
        let resolved = resolve_modification_expr(
            &expr,
            &ast::ModificationEnvironment::default(),
            &effective_components,
            &ast::ClassTree::default(),
            false,
        )
        .expect("resolution should succeed");

        assert_eq!(
            resolved, expr,
            "record bindings should stay as references so declaration defaults are preserved"
        );
    }

    #[test]
    fn test_resolve_sibling_modification_still_resolves_scalar_field_override() {
        let mut effective_components: IndexMap<String, ast::Component> = IndexMap::new();
        let mut data = ast::Component {
            name: "stackData".to_string(),
            ..Default::default()
        };
        data.modifications
            .insert("mSystems".to_string(), make_int_expr(2));
        effective_components.insert("stackData".to_string(), data);

        let expr = make_comp_ref_expr(&["stackData", "mSystems"]);
        let resolved = resolve_modification_expr(
            &expr,
            &ast::ModificationEnvironment::default(),
            &effective_components,
            &ast::ClassTree::default(),
            false,
        )
        .expect("resolution should succeed");

        assert_eq!(
            resolved,
            make_int_expr(2),
            "scalar sibling field overrides should keep existing behavior"
        );
    }

    #[test]
    fn test_resolve_sibling_modification_keeps_function_call_record_like_binding() {
        let mut effective_components: IndexMap<String, ast::Component> = IndexMap::new();
        let mut data = ast::Component {
            name: "aimcData".to_string(),
            ..Default::default()
        };
        data.modifications.insert(
            "statorCoreParameters".to_string(),
            ast::Expression::FunctionCall {
                comp: ast::ComponentReference {
                    local: false,
                    parts: vec![
                        ast::ComponentRefPart {
                            ident: make_token("Modelica"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Electrical"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Machines"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("Losses"),
                            subs: None,
                        },
                        ast::ComponentRefPart {
                            ident: make_token("CoreParameters"),
                            subs: None,
                        },
                    ],
                    def_id: None,
                },
                args: vec![make_int_expr(410), make_int_expr(388)],
            },
        );
        effective_components.insert("aimcData".to_string(), data);

        let expr = make_comp_ref_expr(&["aimcData", "statorCoreParameters"]);
        let resolved = resolve_modification_expr(
            &expr,
            &ast::ModificationEnvironment::default(),
            &effective_components,
            &ast::ClassTree::default(),
            false,
        )
        .expect("resolution should succeed");

        assert_eq!(
            resolved, expr,
            "function-call record-like overrides should stay as references"
        );
    }

    #[test]
    fn test_insert_scoped_modifier_binding_reorders_non_shifted_parent_key() {
        let mut ctx = InstantiateContext::new();
        let key = ast::QualifiedName::from_ident("k");
        let sibling = ast::QualifiedName::from_ident("a");

        ctx.mod_env_mut().add(
            key.clone(),
            ast::ModificationValue::simple(make_int_expr(1)),
        );
        ctx.mod_env_mut().add(
            sibling.clone(),
            ast::ModificationValue::simple(make_int_expr(2)),
        );

        let mut parent_snapshot = IndexMap::new();
        parent_snapshot.insert(
            key.clone(),
            ast::ModificationValue::simple(make_int_expr(1)),
        );
        parent_snapshot.insert(
            sibling.clone(),
            ast::ModificationValue::simple(make_int_expr(2)),
        );

        insert_scoped_modifier_binding(
            &mut ctx,
            key.clone(),
            make_int_expr(9),
            None,
            None,
            &parent_snapshot,
            &IndexMap::new(),
        );

        assert_eq!(
            active_mod_env_keys(&ctx),
            vec!["a".to_string(), "k".to_string()],
            "non-shifted parent key should be replaced as a new local binding"
        );
        assert_eq!(
            ctx.mod_env().get(&key).map(|mv| mv.value.clone()),
            Some(make_int_expr(9))
        );
    }

    #[test]
    fn test_insert_scoped_modifier_binding_keeps_shifted_parent_key_position() {
        let mut ctx = InstantiateContext::new();
        let key = ast::QualifiedName::from_ident("k");
        let sibling = ast::QualifiedName::from_ident("a");

        ctx.mod_env_mut().add(
            key.clone(),
            ast::ModificationValue::simple(make_int_expr(1)),
        );
        ctx.mod_env_mut().add(
            sibling.clone(),
            ast::ModificationValue::simple(make_int_expr(2)),
        );

        let mut parent_snapshot = IndexMap::new();
        parent_snapshot.insert(
            key.clone(),
            ast::ModificationValue::simple(make_int_expr(1)),
        );
        parent_snapshot.insert(
            sibling.clone(),
            ast::ModificationValue::simple(make_int_expr(2)),
        );

        let mut shifted_parent_keys = IndexMap::new();
        shifted_parent_keys.insert(key.clone(), ());

        insert_scoped_modifier_binding(
            &mut ctx,
            key.clone(),
            make_int_expr(11),
            None,
            None,
            &parent_snapshot,
            &shifted_parent_keys,
        );

        assert_eq!(
            active_mod_env_keys(&ctx),
            vec!["k".to_string(), "a".to_string()],
            "shifted parent key should remain in place"
        );
        assert_eq!(
            ctx.mod_env().get(&key).map(|mv| mv.value.clone()),
            Some(make_int_expr(1)),
            "outer/shifted parent modifier must keep precedence (MLS §7.2.4)"
        );
    }

    #[test]
    fn test_forwarded_modifier_keeps_forwarded_source_scope() {
        let mut ctx = InstantiateContext::new();
        let key = ast::QualifiedName::from_ident("frictionParameters");
        let forwarded_value = make_comp_ref_expr(&["aimcData", "frictionParameters"]);
        let forwarded_scope = Some(ast::QualifiedName::new());

        ctx.mod_env_mut().active.insert(
            key.clone(),
            ast::ModificationValue::with_source_scope(
                forwarded_value.clone(),
                Some(forwarded_value.clone()),
                forwarded_scope.clone(),
            ),
        );

        let parent_snapshot = ctx.mod_env().active.clone();
        let shifted_parent_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::new();
        let insert_ctx = ScopedInsertContext {
            parent_snapshot: &parent_snapshot,
            shifted_parent_keys: &shifted_parent_keys,
            source_scope: Some(ast::QualifiedName::from_ident("aimc")),
        };

        insert_modifier_value_with_structural_overrides(
            &mut ctx,
            "frictionParameters",
            &make_comp_ref_expr(&["frictionParameters"]),
            false,
            &IndexMap::new(),
            &ast::ClassTree::default(),
            &insert_ctx,
        )
        .expect("forwarded modifier insertion should succeed");

        let stored = ctx
            .mod_env()
            .get(&key)
            .expect("forwarded modifier binding should exist");
        assert_eq!(
            stored.value, forwarded_value,
            "forwarded binding should preserve resolved parent expression"
        );
        assert_eq!(
            stored.source_scope, forwarded_scope,
            "forwarded binding should preserve original lexical source scope"
        );
        assert_eq!(
            stored.source.as_ref(),
            Some(&forwarded_value),
            "forwarded binding should preserve symbolic source expression"
        );
    }

    #[test]
    fn test_modifier_with_same_resolved_value_keeps_existing_source_scope() {
        let mut ctx = InstantiateContext::new();
        let key = ast::QualifiedName::from_ident("frictionParameters");
        let forwarded_value = make_comp_ref_expr(&["aimcData", "frictionParameters"]);
        let forwarded_scope = Some(ast::QualifiedName::new());

        ctx.mod_env_mut().active.insert(
            key.clone(),
            ast::ModificationValue::with_source_scope(
                forwarded_value.clone(),
                Some(forwarded_value.clone()),
                forwarded_scope.clone(),
            ),
        );

        let parent_snapshot = ctx.mod_env().active.clone();
        let shifted_parent_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::new();
        let insert_ctx = ScopedInsertContext {
            parent_snapshot: &parent_snapshot,
            shifted_parent_keys: &shifted_parent_keys,
            source_scope: Some(ast::QualifiedName::from_ident("aimc")),
        };

        insert_modifier_value_with_structural_overrides(
            &mut ctx,
            "frictionParameters",
            &forwarded_value,
            false,
            &IndexMap::new(),
            &ast::ClassTree::default(),
            &insert_ctx,
        )
        .expect("same-value modifier insertion should succeed");

        let stored = ctx
            .mod_env()
            .get(&key)
            .expect("modifier binding should exist");
        assert_eq!(
            stored.source_scope, forwarded_scope,
            "resolved multi-part modifier should inherit source scope from existing parent binding"
        );
    }

    #[test]
    fn test_propagate_record_binding_overrides_non_targeted_field_values() {
        let mut nested_record = ast::ClassDef {
            name: make_token("State"),
            class_type: ast::ClassType::Record,
            ..Default::default()
        };
        nested_record
            .components
            .insert("phase".to_string(), ast::Component::default());
        nested_record
            .components
            .insert("p".to_string(), ast::Component::default());

        let mut ctx = InstantiateContext::new();
        ctx.mod_env_mut().add(
            ast::QualifiedName::from_ident("phase"),
            ast::ModificationValue::simple(make_int_expr(7)),
        );

        let binding_expr = make_comp_ref_expr(&["state_in"]);
        let targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::new();
        propagate_record_binding_to_fields(
            &ast::ClassTree::default(),
            &mut ctx,
            &binding_expr,
            None,
            &nested_record,
            &targeted_keys,
        );

        let phase_mod = ctx
            .mod_env()
            .active
            .get(&ast::QualifiedName::from_ident("phase"))
            .expect("phase field binding should be present");
        match &phase_mod.value {
            ast::Expression::FieldAccess { base, field } => {
                assert_eq!(field, "phase");
                match base.as_ref() {
                    ast::Expression::ComponentReference(cref) => {
                        assert_eq!(cref.parts.len(), 1);
                        assert_eq!(cref.parts[0].ident.text.as_ref(), "state_in");
                    }
                    _ => panic!("field binding should project from record binding expression"),
                }
            }
            _ => panic!("phase field should be rebound from record binding"),
        }
    }

    #[test]
    fn test_propagate_record_binding_preserves_targeted_field_modifiers() {
        let mut nested_record = ast::ClassDef {
            name: make_token("State"),
            class_type: ast::ClassType::Record,
            ..Default::default()
        };
        nested_record
            .components
            .insert("phase".to_string(), ast::Component::default());

        let mut ctx = InstantiateContext::new();
        let phase_qn = ast::QualifiedName::from_ident("phase");
        ctx.mod_env_mut().add(
            phase_qn.clone(),
            ast::ModificationValue::simple(make_int_expr(42)),
        );

        let mut targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::new();
        targeted_keys.insert(phase_qn.clone(), ());
        let binding_expr = make_comp_ref_expr(&["state_in"]);
        propagate_record_binding_to_fields(
            &ast::ClassTree::default(),
            &mut ctx,
            &binding_expr,
            None,
            &nested_record,
            &targeted_keys,
        );

        let phase_mod = ctx
            .mod_env()
            .active
            .get(&phase_qn)
            .expect("targeted phase modifier should still be present");
        match &phase_mod.value {
            ast::Expression::Terminal { token, .. } => assert_eq!(token.text.as_ref(), "42"),
            _ => panic!("targeted field modifier should not be replaced"),
        }
    }

    #[test]
    fn test_propagate_record_binding_projects_if_expression_branches_per_field() {
        let mut nested_record = ast::ClassDef {
            name: make_token("CellData"),
            class_type: ast::ClassType::Record,
            ..Default::default()
        };
        nested_record
            .components
            .insert("OCV_SOC".to_string(), ast::Component::default());

        let mut ctx = InstantiateContext::new();
        let binding_expr = ast::Expression::If {
            branches: vec![(
                make_comp_ref_expr(&["isDegraded"]),
                make_comp_ref_expr(&["cellDataDegraded"]),
            )],
            else_branch: Arc::new(make_comp_ref_expr(&["cellDataOriginal"])),
        };
        let targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::new();
        propagate_record_binding_to_fields(
            &ast::ClassTree::default(),
            &mut ctx,
            &binding_expr,
            None,
            &nested_record,
            &targeted_keys,
        );

        let field_mod = ctx
            .mod_env()
            .active
            .get(&ast::QualifiedName::from_ident("OCV_SOC"))
            .expect("OCV_SOC field binding should be present");
        let ast::Expression::If {
            branches,
            else_branch,
        } = &field_mod.value
        else {
            panic!("field projection should preserve if-expression structure");
        };
        assert_eq!(branches.len(), 1);
        let (_cond, then_expr) = &branches[0];
        let ast::Expression::FieldAccess { base, field } = then_expr else {
            panic!("then-branch should project field access");
        };
        assert_eq!(field, "OCV_SOC");
        assert_eq!(*base.as_ref(), make_comp_ref_expr(&["cellDataDegraded"]));

        let ast::Expression::FieldAccess {
            base: else_base,
            field: else_field,
        } = else_branch.as_ref()
        else {
            panic!("else-branch should project field access");
        };
        assert_eq!(else_field, "OCV_SOC");
        assert_eq!(
            *else_base.as_ref(),
            make_comp_ref_expr(&["cellDataOriginal"])
        );
    }

    #[test]
    fn test_propagate_record_binding_preserves_matching_default_record_constructor() {
        let mut nested_record = ast::ClassDef {
            name: make_token("BaseData"),
            class_type: ast::ClassType::Record,
            ..Default::default()
        };
        nested_record.components.insert(
            "mu_i".to_string(),
            ast::Component {
                binding: Some(make_int_expr(1)),
                start: make_int_expr(1),
                ..Default::default()
            },
        );

        let mut ctx = InstantiateContext::new();
        let binding_expr = ast::Expression::FunctionCall {
            comp: ast::ComponentReference {
                local: false,
                parts: vec![ast::ComponentRefPart {
                    ident: make_token("BaseData"),
                    subs: None,
                }],
                def_id: None,
            },
            args: Vec::new(),
        };

        propagate_record_binding_to_fields(
            &ast::ClassTree::default(),
            &mut ctx,
            &binding_expr,
            None,
            &nested_record,
            &IndexMap::new(),
        );

        assert!(
            ctx.mod_env().active.is_empty(),
            "matching zero-argument record constructors should preserve declared defaults"
        );
    }

    #[test]
    fn test_propagate_record_binding_projects_subtype_default_record_constructor_fields() {
        let mut nested_record = ast::ClassDef {
            name: make_token("BaseData"),
            class_type: ast::ClassType::Record,
            ..Default::default()
        };
        nested_record.components.insert(
            "mu_i".to_string(),
            ast::Component {
                binding: Some(make_int_expr(1)),
                start: make_int_expr(1),
                ..Default::default()
            },
        );

        let mut ctx = InstantiateContext::new();
        let binding_expr = ast::Expression::FunctionCall {
            comp: ast::ComponentReference {
                local: false,
                parts: vec![ast::ComponentRefPart {
                    ident: make_token("M350_50A"),
                    subs: None,
                }],
                def_id: None,
            },
            args: Vec::new(),
        };

        propagate_record_binding_to_fields(
            &ast::ClassTree::default(),
            &mut ctx,
            &binding_expr,
            None,
            &nested_record,
            &IndexMap::new(),
        );

        let field_mod = ctx
            .mod_env()
            .active
            .get(&ast::QualifiedName::from_ident("mu_i"))
            .expect("subtype default record constructor should project field binding");
        let ast::Expression::FieldAccess { base, field } = &field_mod.value else {
            panic!("subtype constructor field should be projected");
        };
        assert_eq!(field, "mu_i");
        assert_eq!(base.as_ref(), &binding_expr);
    }

    #[test]
    fn test_propagate_record_binding_skips_non_record_classes() {
        let mut nested_block = ast::ClassDef {
            name: make_token("UniformNoise"),
            class_type: ast::ClassType::Block,
            ..Default::default()
        };
        nested_block
            .components
            .insert("y".to_string(), ast::Component::default());
        nested_block
            .components
            .insert("seedState".to_string(), ast::Component::default());

        let mut ctx = InstantiateContext::new();
        let binding_expr = make_comp_ref_expr(&["noise"]);
        let targeted_keys: IndexMap<ast::QualifiedName, ()> = IndexMap::new();
        propagate_record_binding_to_fields(
            &ast::ClassTree::default(),
            &mut ctx,
            &binding_expr,
            None,
            &nested_block,
            &targeted_keys,
        );

        assert!(
            ctx.mod_env().active.is_empty(),
            "non-record class modifiers must not synthesize per-field record bindings"
        );
    }

    #[test]
    fn test_collect_structural_integer_fields_from_sibling_reference() {
        let mut effective_components: IndexMap<String, ast::Component> = IndexMap::new();
        let mut stack_data = ast::Component {
            name: "stackData".to_string(),
            ..Default::default()
        };
        stack_data
            .modifications
            .insert("Ns".to_string(), make_int_expr(3));
        stack_data
            .modifications
            .insert("Np".to_string(), make_int_expr(2));
        stack_data.modifications.insert(
            "cellDataOriginal".to_string(),
            make_comp_ref_expr(&["cellDataOriginal"]),
        );
        effective_components.insert("stackData".to_string(), stack_data);

        let expr = make_comp_ref_expr(&["stackData"]);
        let resolved = collect_structural_integer_fields_from_sibling_reference(
            &expr,
            &ast::ModificationEnvironment::default(),
            &effective_components,
            &ast::ClassTree::default(),
        );
        assert_eq!(resolved.len(), 2);

        let mut seen: IndexMap<String, i64> = IndexMap::new();
        for (name, value) in resolved {
            let ast::Expression::Terminal { token, .. } = value else {
                panic!("expected terminal integer");
            };
            seen.insert(
                name,
                token
                    .text
                    .parse::<i64>()
                    .expect("integer terminal should parse"),
            );
        }

        assert_eq!(seen.get("Ns"), Some(&3));
        assert_eq!(seen.get("Np"), Some(&2));
    }
}
