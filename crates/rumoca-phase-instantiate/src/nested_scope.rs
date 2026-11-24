use super::mod_env::add_class_modifier_defaults;
use super::type_overrides::{
    ClassModifierValues, TypeOverrideMap, build_type_override_map, class_redeclare_modifier_args,
    component_source_modifier_target_name, extract_component_class_overrides,
    find_all_nested_classes_in_hierarchy, find_nested_class_in_hierarchy,
    resolve_class_override_modifier_targets, validate_component_class_redeclare_target,
};
use super::{InstantiateContext, InstantiateError, InstantiateResult, location_to_span};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::collections::BTreeSet;

pub(super) type NestedTypeOverrides = (ast::ClassOverrideMap, bool, TypeOverrideMap);
pub(super) type SelectedClassModifiers = IndexMap<DefId, ClassModifierValues>;

pub(super) struct NestedClassTypeSelection<'a> {
    pub(super) overrides: &'a TypeOverrideMap,
    pub(super) redeclarations: &'a ast::ClassOverrideMap,
    pub(super) selected_class_modifiers: Option<&'a ClassModifierValues>,
}

impl NestedClassTypeSelection<'_> {
    pub(super) fn final_paths(
        &self,
        parent_paths: &[ast::QualifiedName],
        component_name: &str,
    ) -> Vec<ast::QualifiedName> {
        parent_paths
            .iter()
            .filter_map(|path| path.strip_prefix(component_name))
            .chain(
                self.selected_class_modifiers
                    .into_iter()
                    .flat_map(|modifiers| modifiers.final_paths.iter().cloned()),
            )
            .collect()
    }
}

pub(super) fn prepare_nested_class_modifiers(
    ctx: &mut InstantiateContext,
    comp: &ast::Component,
    nested_types: NestedClassTypeSelection<'_>,
    parent_snapshot: &IndexMap<ast::QualifiedName, ast::ModificationValue>,
    targeted_keys: &IndexMap<ast::QualifiedName, ()>,
    record_projected_keys: &IndexMap<ast::QualifiedName, ()>,
    (tree, class, declared): (&ast::ClassTree, &ast::ClassDef, SelectedClassModifiers),
) -> InstantiateResult<TypeOverrideMap> {
    let referenced_mod_roots = collect_referenced_mod_roots(comp);
    let is_outer_modifier = |key: &ast::QualifiedName| {
        !parent_snapshot.contains_key(key)
            || record_projected_keys.contains_key(key)
            || targeted_keys.contains_key(key)
    };
    ctx.mod_env_mut().active.retain(|key, _| {
        // A colliding parent value may remain only when this occurrence owns
        // the binding or a modifier in this occurrence needs it for lookup.
        is_outer_modifier(key) || key_matches_referenced_root(key, &referenced_mod_roots)
    });

    if let Some(modifiers) = nested_types.selected_class_modifiers {
        add_class_modifier_defaults(
            &mut ctx.mod_env_mut().active,
            modifiers,
            is_outer_modifier,
            tree,
            Some(class),
        )?;
    }

    let mut selections = nested_types.overrides.clone();
    capture_redeclared_class_modifiers(
        &mut selections,
        ctx.active_type_overrides.last(),
        nested_types.redeclarations,
        declared,
        tree,
    )?;
    Ok(selections)
}

/// Associate the evaluated class modification with its resolved alias before
/// entering the nested scope. A forwarded alias retains its original values
/// and source scopes through its exact referenced declaration.
fn capture_redeclared_class_modifiers(
    selections: &mut TypeOverrideMap,
    enclosing: Option<&TypeOverrideMap>,
    class_overrides: &ast::ClassOverrideMap,
    mut declared: SelectedClassModifiers,
    tree: &ast::ClassTree,
) -> InstantiateResult<()> {
    for class_override in class_overrides.values() {
        let mut values = match declared.shift_remove(&class_override.alias_def_id) {
            Some(values) => values,
            None if class_override.modifier_args.is_empty() => ClassModifierValues::default(),
            None => {
                return Err(Box::new(InstantiateError::redeclare_error(
                    &class_override.alias,
                    "no scoped values were constructed for the class modifier",
                    class_override.modifier_args[0].span(),
                )));
            }
        };
        let forwarded = class_override
            .target_ref
            .as_ref()
            .and_then(ast::ComponentReference::target_def_id)
            .and_then(|alias| enclosing?.class_modifier_values(alias));
        if let Some(forwarded) = forwarded {
            add_class_modifier_defaults(
                &mut values.bindings,
                forwarded,
                |_| true,
                tree,
                tree.get_class_by_def_id(class_override.target_def_id),
            )?;
            values
                .final_paths
                .extend(forwarded.final_paths.iter().cloned());
        }
        selections.insert_class_override_with_values(class_override, values);
    }
    Ok(())
}

pub(super) fn collect_referenced_mod_roots(comp: &ast::Component) -> BTreeSet<String> {
    let mut roots = BTreeSet::new();
    for expr in comp.modifications.values() {
        for comp_ref in ast::visitor::collect_component_refs(expr) {
            if let Some(first) = comp_ref.parts.first() {
                roots.insert(first.ident.text.to_string());
            }
        }
    }
    roots
}

pub(super) fn key_matches_referenced_root(
    key: &ast::QualifiedName,
    referenced_roots: &BTreeSet<String>,
) -> bool {
    // Keep only qualified parent keys as lookup context for nested modifiers.
    // Unqualified parent keys (e.g., `m_flow`) can collide with nested members
    // and incorrectly act as direct bindings for those members (MLS §7.2 scope).
    if key.parts.len() <= 1 {
        return false;
    }

    key.first_name()
        .is_some_and(|name| referenced_roots.contains(name))
}

#[cfg(test)]
fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("nested_scope_test.mo"),
        1,
        2,
    )
}

/// Collect the mod_env keys that are explicitly targeted at a component's nested class.
///
/// Returns keys from two sources:
/// 1. Shifted keys: parent-scope entries like `heatPort.T` that become `T` via
///    `shift_modifications_down`
/// 2. Populated keys: entries from the component's own declared modifications
///    (e.g., `sub(n=n)` targets key `n`)
///
/// This is used by step 2.6 in `instantiate_nested_class` to distinguish legitimate
/// modifications from parent-scope entries that collide by name.
pub(super) fn collect_targeted_mod_keys(
    comp: &ast::Component,
    parent_snapshot: &IndexMap<ast::QualifiedName, rumoca_ir_ast::ModificationValue>,
    class_overrides: &ast::ClassOverrideMap,
) -> IndexMap<ast::QualifiedName, ()> {
    let mut keys = IndexMap::default();

    // Shifted keys: parent entries with this component's name as prefix
    for path in parent_snapshot.keys() {
        if let Some(new_path) = path.strip_prefix(&comp.name) {
            keys.insert(new_path, ());
        }
    }

    // Populated keys: from the component's own modifications
    for (target_name, mod_expr) in &comp.modifications {
        let qn = target_path(target_name);
        match mod_expr {
            // Class modifications like `m_flow(each min=..., each max=...)` target
            // attribute paths (`m_flow.min`, `m_flow.max`) and must not mark the
            // bare `m_flow` key as targeted; doing so can leak unrelated parent
            // bindings into nested members with the same name (MLS §7.2 scope).
            ast::Expression::ClassModification { modifications, .. } => {
                // An empty argument has no binding. A resolved class
                // redeclaration also carries a selection at the alias key.
                if modifications.is_empty()
                    && class_overrides
                        .values()
                        .any(|selection| selection.alias == *target_name)
                {
                    keys.insert(qn, ());
                } else {
                    collect_nested_mod_keys_recursive(&qn, modifications, &mut keys);
                }
            }
            _ => {
                keys.insert(qn.clone(), ());
            }
        }
    }

    keys
}

/// Collect keys that come from parent modifications explicitly targeting this component.
///
/// Parent entries like `r0.useHeatPort` become `useHeatPort` after shifting.
/// These shifted keys are legitimate outer overrides for this nested scope.
pub(super) fn collect_shifted_parent_mod_keys(
    comp: &ast::Component,
    parent_snapshot: &IndexMap<ast::QualifiedName, rumoca_ir_ast::ModificationValue>,
) -> IndexMap<ast::QualifiedName, ()> {
    let mut keys = IndexMap::default();
    for path in parent_snapshot.keys() {
        if let Some(new_path) = path.strip_prefix(&comp.name) {
            keys.insert(new_path, ());
        }
    }
    keys
}

/// The field modifiers a record binding leaves in force.
///
/// MLS §7.2.4: an enclosing modifier that binds the whole record overrides
/// the field modifiers of the declaration it modifies, so only the field
/// modifiers written beside it, in the enclosing scopes (`shifted`), still
/// select their fields. A declaration's own binding keeps every targeted
/// field modifier.
pub(super) fn retained_field_modifiers<'a>(
    binding_from_enclosing_modifier: bool,
    shifted: &'a IndexMap<ast::QualifiedName, ()>,
    targeted: &'a IndexMap<ast::QualifiedName, ()>,
) -> &'a IndexMap<ast::QualifiedName, ()> {
    if binding_from_enclosing_modifier {
        shifted
    } else {
        targeted
    }
}

/// Recursively collect modification keys from nested class modifications.
fn collect_nested_mod_keys_recursive(
    prefix: &ast::QualifiedName,
    modifications: &[ast::Expression],
    keys: &mut IndexMap<ast::QualifiedName, ()>,
) {
    for nested_mod in modifications {
        match nested_mod {
            ast::Expression::Modification { target, .. } => {
                let mut qn = prefix.clone();
                qn.parts
                    .extend(ast::QualifiedName::from_component_reference(target).parts);
                keys.insert(qn, ());
            }
            ast::Expression::ClassModification {
                target,
                modifications: nested_mods,
                ..
            } => {
                let mut nested_prefix = prefix.clone();
                nested_prefix
                    .parts
                    .extend(ast::QualifiedName::from_component_reference(target).parts);
                collect_nested_mod_keys_recursive(&nested_prefix, nested_mods, keys);
            }
            _ => {}
        }
    }
}

/// `a.b(c = 1)` means `a(b(c = 1))`; a quoted identifier is one name, dots included.
pub(super) fn target_path(target_name: &str) -> ast::QualifiedName {
    if target_name.starts_with('\'') {
        ast::QualifiedName::from_ident(target_name)
    } else {
        ast::QualifiedName::from_dotted(target_name)
    }
}

/// Shift modifications down when descending into a nested component.
///
/// MLS §7.2: When we descend into a component `l2`, modifications like `l2.x.start = 100`
/// need to become `x.start = 100` so they apply to the children of `l2`.
///
/// This uses `ast::QualifiedName::strip_prefix` to preserve array subscripts in paths,
/// avoiding the information loss that would occur with string-based manipulation.
pub(super) fn shift_modifications_down(ctx: &mut InstantiateContext, comp_name: &str) {
    // Collect entries to add (with shifted paths)
    // Using strip_prefix preserves subscripts on the remaining path parts
    let shifted: Vec<_> = ctx
        .mod_env()
        .active
        .iter()
        .filter_map(|(path, value)| {
            path.strip_prefix(comp_name)
                .map(|new_path| (new_path, value.clone()))
        })
        .collect();

    // Component-qualified outer modifiers are the active modifiers inside the
    // nested component. Replace any same-name parent key from the enclosing
    // scope; the caller restores the snapshot after nested instantiation.
    for (path, value) in shifted {
        let mod_env = ctx.mod_env_mut();
        mod_env.active.shift_remove(&path);
        mod_env.active.insert(path, value);
    }
}

/// Remap a class-redeclare modifier target to the active enclosing override.
///
/// MLS §7.3: `redeclare package Medium = Medium` inside component modifiers should
/// forward to the enclosing class's active `Medium` redeclare, not the local default.
pub(super) fn remap_redeclare_class_modifier(
    mod_expr: &ast::Expression,
    target_name: &str,
    type_overrides: &TypeOverrideMap,
) -> ast::Expression {
    let ast::Expression::ClassModification {
        target,
        modifications,
        span,
        ..
    } = mod_expr
    else {
        return mod_expr.clone();
    };

    let Some(last) = target.parts.last() else {
        return mod_expr.clone();
    };
    if last.ident.text.as_ref() != target_name {
        return mod_expr.clone();
    }

    let Some(override_def_id) = type_overrides.target_for_reference(target) else {
        return mod_expr.clone();
    };
    if target.root_def_id() == Some(override_def_id)
        && target.target_def_id() == Some(override_def_id)
    {
        return mod_expr.clone();
    }

    let mut remapped_target = target.clone();
    remapped_target.set_root_def_id(Some(override_def_id));
    remapped_target.set_target_def_id(Some(override_def_id));
    ast::Expression::ClassModification {
        target: remapped_target,
        modifications: modifications.clone(),
        each_flags: Vec::new(),
        final_flags: Vec::new(),
        redeclare_flags: Vec::new(),
        span: *span,
    }
}

fn is_self_forwarding_redeclare(mod_expr: &ast::Expression, target_name: &str) -> bool {
    let ast::Expression::ClassModification { target, .. } = mod_expr else {
        return false;
    };
    let [part] = target.parts.as_slice() else {
        return false;
    };
    part.subs.is_none() && part.ident.text.as_ref() == target_name
}

/// Resolve component-scoped class/package redeclares for nested instantiation.
///
/// MLS §7.3:
/// - forwarding redeclares (`redeclare package X = X`) bind to active overrides.
/// - class/package redeclares specialize nested type aliases.
pub(super) fn resolve_component_nested_type_overrides(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    class_def: Option<&ast::ClassDef>,
    mod_env: &ast::ModificationEnvironment,
    type_overrides: &TypeOverrideMap,
) -> InstantiateResult<NestedTypeOverrides> {
    let mut class_overrides =
        extract_component_class_overrides(tree, comp, class_def, Some(mod_env))?;
    select_redeclare_values_in_scope(&mut class_overrides, type_overrides);
    let mut has_forwarding_class_redeclare = false;

    if let Some(target_class) = class_def {
        for (source_modifier, &is_redeclare) in comp
            .source_modifications
            .iter()
            .zip(&comp.source_modification_redeclare_flags)
        {
            if !is_redeclare {
                continue;
            }
            let Some(target_name) = component_source_modifier_target_name(source_modifier) else {
                continue;
            };
            let Some(mod_expr) = comp.modifications.get(&target_name) else {
                continue;
            };
            let nested_class = find_nested_class_in_hierarchy(tree, target_class, &target_name);
            if !nested_class.is_some_and(|nested| nested.is_replaceable)
                || !is_self_forwarding_redeclare(mod_expr, &target_name)
            {
                continue;
            }
            let Some(nested_class) = nested_class else {
                continue;
            };
            let Some(alias_def_id) = nested_class.def_id else {
                return Err(Box::new(InstantiateError::redeclare_error(
                    &target_name,
                    "resolved forwarding redeclare target has no DefId",
                    location_to_span(
                        &target_class.location,
                        &tree.source_map,
                        "forwarding redeclare target class",
                    )?,
                )));
            };
            if let Some(effective_def_id) = type_overrides
                .target_for_alias_def_id(alias_def_id)
                .or_else(|| type_overrides.target_for_alias_name(&target_name))
            {
                validate_component_class_redeclare_target(
                    tree,
                    &target_name,
                    nested_class,
                    mod_expr,
                    effective_def_id,
                )?;
                let modifier_args = resolve_class_override_modifier_targets(
                    tree,
                    effective_def_id,
                    class_redeclare_modifier_args(mod_expr),
                )?;
                insert_forwarded_declaration_overrides(
                    &mut class_overrides,
                    (tree, target_class, &target_name),
                    (effective_def_id, mod_expr, modifier_args),
                );
                has_forwarding_class_redeclare = true;
            }
        }
    }

    let mut nested_type_overrides = type_overrides.clone();
    if let Some(exposed_package) = exposed_type_package(tree, comp) {
        let exposure_overrides = build_type_override_map(tree, exposed_package, Some(mod_env));
        nested_type_overrides.extend_from(&exposure_overrides);
    }
    if let Some(type_prefix) = comp.type_name.name.first()
        && comp.type_name.name.len() > 1
        && let Some(effective_package_def_id) =
            type_overrides.target_for_alias_name(type_prefix.text.as_ref())
    {
        nested_type_overrides.specialize_inherited_nested_types(tree, effective_package_def_id);
    }
    for class_override in class_overrides.values() {
        nested_type_overrides.insert_class_override(class_override);
    }

    Ok((
        class_overrides,
        has_forwarding_class_redeclare,
        nested_type_overrides,
    ))
}

/// Replace a redeclare value that names a replaceable alias of the enclosing
/// scope with the class that scope selected for it.
///
/// MLS §7.3: in `Inner a(redeclare package Medium = MA)`, `MA` denotes the
/// class `MA` is in this occurrence of the enclosing class, which a
/// redeclaration of the enclosing occurrence may have replaced. The recorded
/// override is the selection every later phase reads, so it names that class
/// rather than the lexical alias, whose default would otherwise be selected.
fn select_redeclare_values_in_scope(
    class_overrides: &mut IndexMap<DefId, ast::ClassOverride>,
    type_overrides: &TypeOverrideMap,
) {
    for class_override in class_overrides.values_mut() {
        let mut visited = rustc_hash::FxHashSet::default();
        while visited.insert(class_override.target_def_id)
            && let Some(selected) =
                type_overrides.target_for_alias_def_id(class_override.target_def_id)
        {
            class_override.target_def_id = selected;
        }
    }
}

/// Record a forwarded redeclare for every inherited declaration of the member.
///
/// MLS 7.1.2: several bases may declare the same element; the redeclare
/// reaches each declaration, not only the first one found.
fn insert_forwarded_declaration_overrides(
    class_overrides: &mut IndexMap<DefId, ast::ClassOverride>,
    (tree, target_class, target_name): (&ast::ClassTree, &ast::ClassDef, &str),
    (effective_def_id, mod_expr, modifier_args): (DefId, &ast::Expression, Vec<ast::Expression>),
) {
    for declaration in find_all_nested_classes_in_hierarchy(tree, target_class, target_name) {
        let Some(declaration_def_id) = declaration.def_id else {
            continue;
        };
        class_overrides.insert(
            declaration_def_id,
            ast::ClassOverride::new(
                target_name.to_string(),
                declaration_def_id,
                effective_def_id,
                class_redeclare_target_ref(mod_expr),
            )
            .with_modifier_args(modifier_args.clone()),
        );
    }
}

fn exposed_type_package<'a>(
    tree: &'a ast::ClassTree,
    comp: &ast::Component,
) -> Option<&'a ast::ClassDef> {
    let package_parts = comp
        .type_name
        .name
        .get(..comp.type_name.name.len().checked_sub(1)?)?;
    let package_path =
        rumoca_core::ComponentPath::from_parts(package_parts.iter().map(|part| part.text.as_ref()));
    tree.get_class_by_qualified_name(&package_path.to_flat_string())
}

fn class_redeclare_target_ref(mod_expr: &ast::Expression) -> Option<ast::ComponentReference> {
    let ast::Expression::ClassModification { target, .. } = mod_expr else {
        return None;
    };
    Some(target.clone())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::DefId;
    use std::sync::Arc;

    fn make_token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            location: rumoca_core::Location::default(),
            token_number: 0,
            token_type: 0,
        }
    }

    fn make_comp_ref(parts: &[&str]) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: parts
                .iter()
                .map(|part| ast::ComponentRefPart {
                    ident: make_token(part),
                    subs: None,
                    def_id: None,
                })
                .collect(),
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        }
    }

    fn make_int_expr(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token(&value.to_string()),
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn test_collect_targeted_mod_keys_omits_bare_key_for_nested_attrs() {
        let mut comp = ast::Component {
            name: "port".to_string(),
            ..ast::Component::empty_with_span(test_span())
        };
        comp.modifications.insert(
            "m_flow".to_string(),
            ast::Expression::ClassModification {
                target: make_comp_ref(&["m_flow"]),
                modifications: vec![
                    ast::Expression::Modification {
                        target: make_comp_ref(&["min"]),
                        value: Arc::new(make_int_expr(1)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    ast::Expression::Modification {
                        target: make_comp_ref(&["max"]),
                        value: Arc::new(make_int_expr(2)),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                each_flags: vec![false, false],
                final_flags: vec![false, false],
                redeclare_flags: vec![false, false],
                span: rumoca_core::Span::DUMMY,
            },
        );

        let keys = collect_targeted_mod_keys(&comp, &IndexMap::default(), &IndexMap::default());
        let key_names: std::collections::BTreeSet<String> =
            keys.keys().map(ToString::to_string).collect();

        assert!(key_names.contains("m_flow.min"));
        assert!(key_names.contains("m_flow.max"));
        assert!(
            !key_names.contains("m_flow"),
            "bare key must not be marked targeted for nested class-modification attributes"
        );
    }

    #[test]
    fn test_key_matches_referenced_root_requires_qualified_key() {
        let roots = BTreeSet::from(["m_flow".to_string()]);

        assert!(!key_matches_referenced_root(
            &ast::QualifiedName::from_ident("m_flow"),
            &roots
        ));
        assert!(key_matches_referenced_root(
            &ast::QualifiedName::from_dotted("m_flow.start"),
            &roots
        ));
        assert!(!key_matches_referenced_root(
            &ast::QualifiedName::from_dotted("other.start"),
            &roots
        ));
    }

    #[test]
    fn test_collect_referenced_mod_roots_finds_nested_component_refs() {
        let mut comp = ast::Component::empty_with_span(rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("nested_scope_test.mo"),
            1,
            2,
        ));
        comp.modifications.insert(
            "k".to_string(),
            ast::Expression::ArrayIndex {
                base: Arc::new(ast::Expression::FieldAccess {
                    base: Arc::new(ast::Expression::ComponentReference(make_comp_ref(&[
                        "state",
                    ]))),
                    field: "x".to_string(),
                    field_def_id: None,
                    span: rumoca_core::Span::DUMMY,
                }),
                subscripts: vec![ast::Subscript::Expression(
                    ast::Expression::ComponentReference(make_comp_ref(&["idx"])),
                )],
                span: rumoca_core::Span::DUMMY,
            },
        );

        let roots = collect_referenced_mod_roots(&comp);
        assert!(roots.contains("state"));
        assert!(roots.contains("idx"));
    }

    #[test]
    fn shift_modifications_down_replaces_colliding_parent_key() {
        let mut ctx = InstantiateContext::new();
        let bare_m = ast::QualifiedName::from_ident("m");
        let nested_m = ast::QualifiedName::from_dotted("plug.m");

        ctx.mod_env_mut().add(
            bare_m.clone(),
            ast::ModificationValue::with_prefixes(make_int_expr(3), false, true),
        );
        ctx.mod_env_mut().add(
            nested_m,
            ast::ModificationValue::with_prefixes(make_int_expr(5), false, true),
        );

        shift_modifications_down(&mut ctx, "plug");

        let shifted = ctx
            .mod_env()
            .get(&bare_m)
            .expect("shifted component modifier should replace parent key");
        assert_eq!(shifted.value, make_int_expr(5));
        assert!(shifted.final_);
    }

    #[test]
    fn remap_redeclare_class_modifier_preserves_source_reference_parts() {
        let source_medium = DefId::new(7);
        let concrete_medium = DefId::new(42);
        let mut target = make_comp_ref(&["Medium"]);
        target.set_root_def_id(Some(source_medium));
        target.set_target_def_id(Some(source_medium));
        let mod_expr = ast::Expression::ClassModification {
            target,
            modifications: Vec::new(),
            each_flags: Vec::new(),
            final_flags: Vec::new(),
            redeclare_flags: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };
        let mut type_overrides = TypeOverrideMap::new();
        type_overrides.insert_alias(
            ast::QualifiedName::from_ident("Medium"),
            None,
            concrete_medium,
        );

        let remapped = remap_redeclare_class_modifier(&mod_expr, "Medium", &type_overrides);

        let ast::Expression::ClassModification { target, .. } = remapped else {
            panic!("expected class modification");
        };
        assert_eq!(target.root_def_id(), Some(concrete_medium));
        assert_eq!(target.target_def_id(), Some(concrete_medium));
        assert_eq!(target.to_string(), "Medium");
    }

    #[test]
    fn component_field_redeclare_is_not_a_class_override() {
        let mut target_class = ast::ClassDef {
            name: make_token("Complex"),
            def_id: Some(DefId::new(1)),
            ..Default::default()
        };
        target_class.components.insert(
            "re".to_string(),
            ast::Component {
                name: "re".to_string(),
                is_replaceable: true,
                ..ast::Component::empty_with_span(test_span())
            },
        );
        let mut comp = ast::Component {
            name: "y".to_string(),
            ..ast::Component::empty_with_span(test_span())
        };
        comp.modifications.insert(
            "re".to_string(),
            ast::Expression::ClassModification {
                target: make_comp_ref(&["re"]),
                modifications: Vec::new(),
                each_flags: Vec::new(),
                final_flags: Vec::new(),
                redeclare_flags: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            },
        );

        let (class_overrides, has_forwarding_class_redeclare, _) =
            resolve_component_nested_type_overrides(
                &ast::ClassTree::default(),
                &comp,
                Some(&target_class),
                &ast::ModificationEnvironment::new(),
                &TypeOverrideMap::new(),
            )
            .expect("component-field redeclare must not require a nested class DefId");

        assert!(class_overrides.is_empty());
        assert!(!has_forwarding_class_redeclare);
    }
}
