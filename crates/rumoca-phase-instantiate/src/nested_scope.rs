use super::type_overrides::{
    SourceForwardingEvidence, SourceForwardingEvidenceError, TypeOverrideMap,
    build_type_override_map, checked_source_forwarding_witness, class_redeclare_alias_ref,
    class_redeclare_modifier_args, contains_component_by_def_id_in_hierarchy,
    direct_source_redeclare, extract_component_class_overrides,
    find_component_by_def_id_in_hierarchy, find_nested_class_by_def_id_in_hierarchy,
    resolve_class_override_modifier_targets, resolve_redeclare_value_def_id,
    validate_component_class_redeclare_target,
};
use super::{InstantiateContext, InstantiateError, InstantiateResult};
use rumoca_core::{DefId, Span};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::collections::BTreeSet;

pub(super) struct NestedTypeOverrides {
    class_overrides: ast::ClassOverrideMap,
    has_forwarding_class_redeclare: bool,
    type_overrides: TypeOverrideMap,
    component_type_selections: NestedComponentTypeSelections,
}

impl NestedTypeOverrides {
    pub(super) fn into_parts(
        self,
    ) -> (
        ast::ClassOverrideMap,
        bool,
        TypeOverrideMap,
        NestedComponentTypeSelections,
    ) {
        (
            self.class_overrides,
            self.has_forwarding_class_redeclare,
            self.type_overrides,
            self.component_type_selections,
        )
    }
}

struct ComponentTypeSelectionClaim {
    target_name: String,
    selected_type_def_id: DefId,
    span: Span,
}

/// Affine occurrence-local component selections issued before the nested class
/// occurrence is published. Each exact declaration-slot claim is removed once
/// while that occurrence's complete selected-type plan is constructed.
pub(super) struct NestedComponentTypeSelections {
    claims: IndexMap<DefId, ComponentTypeSelectionClaim>,
}

impl NestedComponentTypeSelections {
    fn new() -> Self {
        Self {
            claims: IndexMap::default(),
        }
    }

    fn insert(
        &mut self,
        declaration_def_id: DefId,
        claim: ComponentTypeSelectionClaim,
    ) -> InstantiateResult<()> {
        if let Some(previous) = self.claims.get(&declaration_def_id) {
            return Err(Box::new(InstantiateError::redeclare_error(
                &claim.target_name,
                format!(
                    "component slot {declaration_def_id:?} was redeclared more than once (first selected {:?})",
                    previous.selected_type_def_id
                ),
                claim.span,
            )));
        }
        self.claims.insert(declaration_def_id, claim);
        Ok(())
    }

    pub(super) fn consume(&mut self, declaration_def_id: DefId) -> Option<DefId> {
        self.claims
            .shift_remove(&declaration_def_id)
            .map(|claim| claim.selected_type_def_id)
    }

    pub(super) fn finish(self) -> InstantiateResult<()> {
        let Some((declaration_def_id, claim)) = self.claims.into_iter().next() else {
            return Ok(());
        };
        Err(Box::new(InstantiateError::redeclare_error(
            &claim.target_name,
            format!(
                "component redeclare claim for exact slot {declaration_def_id:?} was not consumed by the effective nested component inventory"
            ),
            claim.span,
        )))
    }
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
        let qn = ast::QualifiedName::from_ident(target_name);
        match mod_expr {
            // Class modifications like `m_flow(each min=..., each max=...)` target
            // attribute paths (`m_flow.min`, `m_flow.max`) and must not mark the
            // bare `m_flow` key as targeted; doing so can leak unrelated parent
            // bindings into nested members with the same name (MLS §7.2 scope).
            ast::Expression::ClassModification { modifications, .. } => {
                // MLS §7.3: pure class/package redeclare forwarding has no nested
                // attribute modifications (e.g., `redeclare package Medium = Medium`).
                // Keep the bare key targeted so the forwarded binding survives
                // nested-scope pruning in instantiate_nested_class.
                if modifications.is_empty() {
                    keys.insert(qn.clone(), ());
                    continue;
                }
                collect_nested_mod_keys_recursive(&qn, modifications, &mut keys);
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
                qn.push(target.to_string(), Vec::new());
                keys.insert(qn, ());
            }
            ast::Expression::ClassModification {
                target,
                modifications: nested_mods,
                ..
            } => {
                let mut nested_prefix = prefix.clone();
                nested_prefix.push(target.to_string(), Vec::new());
                collect_nested_mod_keys_recursive(&nested_prefix, nested_mods, keys);
            }
            _ => {}
        }
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
    tree: &ast::ClassTree,
    mod_expr: &ast::Expression,
    target_name: &str,
    type_overrides: &TypeOverrideMap,
) -> InstantiateResult<ast::Expression> {
    let ast::Expression::ClassModification {
        target,
        modifications,
        span,
        ..
    } = mod_expr
    else {
        return Ok(mod_expr.clone());
    };

    let Some(last) = target.parts.last() else {
        return Ok(mod_expr.clone());
    };
    if last.ident.text.as_ref() != target_name {
        return Ok(mod_expr.clone());
    }

    let Some(override_def_id) = type_overrides.target_for_reference(tree, target)? else {
        return Ok(mod_expr.clone());
    };
    if target.root_def_id() == Some(override_def_id)
        && target.target_def_id() == Some(override_def_id)
    {
        return Ok(mod_expr.clone());
    }

    let mut remapped_target = target.clone();
    remapped_target.set_root_def_id(Some(override_def_id));
    remapped_target.set_target_def_id(Some(override_def_id));
    Ok(ast::Expression::ClassModification {
        target: remapped_target,
        modifications: modifications.clone(),
        each_flags: Vec::new(),
        final_flags: Vec::new(),
        redeclare_flags: Vec::new(),
        span: *span,
    })
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
        extract_component_class_overrides(tree, comp, class_def, Some(mod_env), type_overrides)?;
    let component_type_selections = match class_def {
        Some(target_class) => plan_component_type_selections(tree, comp, target_class)?,
        None => NestedComponentTypeSelections::new(),
    };
    let has_forwarding_class_redeclare = match class_def {
        Some(target_class) => append_forwarding_class_redeclares(
            tree,
            comp,
            target_class,
            type_overrides,
            &mut class_overrides,
        )?,
        None => false,
    };

    let mut nested_type_overrides = type_overrides.clone();
    if let Some(exposed_package) = exposed_type_package(tree, comp) {
        let exposure_overrides = build_type_override_map(tree, exposed_package, Some(mod_env))?;
        nested_type_overrides.extend_from(&exposure_overrides);
    }
    if comp.type_name.name.len() > 1 {
        let dynamic_root_def_id = comp.type_name.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("dynamic type root `{}`", comp.type_name),
                comp.location.span(),
            ))
        })?;
        if let Some(effective_package_def_id) = type_overrides.checked_target_for_alias_def_id(
            tree,
            dynamic_root_def_id,
            comp.location.span(),
        )? {
            nested_type_overrides
                .specialize_inherited_nested_types(tree, effective_package_def_id)?;
        }
    }
    for class_override in class_overrides.values() {
        nested_type_overrides.insert_class_override(class_override);
    }

    Ok(NestedTypeOverrides {
        class_overrides,
        has_forwarding_class_redeclare,
        type_overrides: nested_type_overrides,
        component_type_selections,
    })
}

enum NestedRedeclareSlot<'a> {
    ClassOrPackage,
    Component(&'a ast::Component),
}

fn classify_nested_redeclare_slot<'a>(
    tree: &'a ast::ClassTree,
    target_class: &'a ast::ClassDef,
    target_name: &str,
    alias_def_id: DefId,
    span: Span,
) -> InstantiateResult<NestedRedeclareSlot<'a>> {
    let nested_class = find_nested_class_by_def_id_in_hierarchy(tree, target_class, alias_def_id)?;
    let component = find_component_by_def_id_in_hierarchy(tree, target_class, alias_def_id)?;
    match (nested_class, component) {
        (Some(_), None) => Ok(NestedRedeclareSlot::ClassOrPackage),
        (None, Some(component)) => Ok(NestedRedeclareSlot::Component(component)),
        (Some(_), Some(_)) => Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            format!(
                "Resolve-issued LHS {alias_def_id:?} identifies both a nested class and a component slot"
            ),
            span,
        ))),
        (None, None) => Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            format!(
                "Resolve-issued LHS {alias_def_id:?} is not an exact direct or inherited receiver slot"
            ),
            span,
        ))),
    }
}

fn plan_component_type_selections(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    target_class: &ast::ClassDef,
) -> InstantiateResult<NestedComponentTypeSelections> {
    let mut selections = NestedComponentTypeSelections::new();
    for (source, is_redeclare) in comp
        .source_modifications
        .iter()
        .zip(&comp.source_modification_redeclare_flags)
    {
        if !*is_redeclare {
            continue;
        }
        let target_name = class_redeclare_alias_ref(source)
            .and_then(|target| target.parts.first())
            .map(|part| part.ident.text.to_string())
            .ok_or_else(|| {
                Box::new(InstantiateError::redeclare_error(
                    &comp.name,
                    "direct redeclare has no receiver-slot reference",
                    source.span(),
                ))
            })?;
        let alias_def_id = class_redeclare_alias_ref(source)
            .and_then(ast::ComponentReference::target_def_id)
            .ok_or_else(|| {
                Box::new(InstantiateError::redeclare_error(
                    &target_name,
                    "direct redeclare LHS has no Resolve-issued receiver-slot identity",
                    source.span(),
                ))
            })?;
        let NestedRedeclareSlot::Component(component_slot) = classify_nested_redeclare_slot(
            tree,
            target_class,
            &target_name,
            alias_def_id,
            source.span(),
        )?
        else {
            continue;
        };
        let resolved = comp.modifications.get(&target_name).ok_or_else(|| {
            Box::new(InstantiateError::redeclare_error(
                &target_name,
                "direct redeclare has no normalized semantic modifier",
                source.span(),
            ))
        })?;
        let selected_type_def_id = resolve_redeclare_value_def_id(tree, resolved, None)?
            .ok_or_else(|| {
                Box::new(InstantiateError::redeclare_error(
                    &target_name,
                    "component redeclare value did not resolve to an exact selected type",
                    source.span(),
                ))
            })?;
        crate::inheritance::validate_component_redeclaration_selection(
            tree,
            component_slot,
            &target_name,
            selected_type_def_id,
            source.span(),
        )?;
        selections.insert(
            alias_def_id,
            ComponentTypeSelectionClaim {
                target_name,
                selected_type_def_id,
                span: source.span(),
            },
        )?;
    }
    Ok(selections)
}

fn append_forwarding_class_redeclares(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    target_class: &ast::ClassDef,
    type_overrides: &TypeOverrideMap,
    class_overrides: &mut ast::ClassOverrideMap,
) -> InstantiateResult<bool> {
    let mut appended = false;
    for (target_name, mod_expr) in &comp.modifications {
        let Some(source_redeclare) = direct_source_redeclare(comp, target_name) else {
            continue;
        };
        let alias_def_id = class_redeclare_alias_ref(source_redeclare)
            .and_then(ast::ComponentReference::target_def_id)
            .ok_or_else(|| {
                Box::new(InstantiateError::redeclare_error(
                    target_name,
                    "direct redeclare LHS has no Resolve-issued receiver-slot identity",
                    source_redeclare.span(),
                ))
            })?;
        let nested_class = match find_nested_class_by_def_id_in_hierarchy(
            tree,
            target_class,
            alias_def_id,
        )? {
            Some(nested) => nested,
            None if contains_component_by_def_id_in_hierarchy(
                tree,
                target_class,
                alias_def_id,
            )? =>
            {
                continue;
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
        };
        if !nested_class.is_replaceable {
            continue;
        }
        let Some(forwarding_witness) =
            checked_source_forwarding_witness(SourceForwardingEvidence {
                tree,
                type_overrides,
                is_redeclare: true,
                source: source_redeclare,
                resolved: mod_expr,
                target_name,
                alias_def_id,
            })
            .map_err(|reason| match reason {
                SourceForwardingEvidenceError::Resolution(error) => error,
                other => Box::new(InstantiateError::redeclare_error(
                    target_name,
                    other.to_string(),
                    source_redeclare.span(),
                )),
            })?
        else {
            continue;
        };
        if forwarding_witness.lhs_slot_def_id() != alias_def_id {
            return Err(Box::new(InstantiateError::redeclare_error(
                target_name,
                "forwarding redeclare witness is bound to another nested slot",
                mod_expr.span(),
            )));
        }
        let effective_def_id = forwarding_witness.effective_target_def_id();
        validate_component_class_redeclare_target(
            tree,
            target_name,
            nested_class,
            mod_expr,
            effective_def_id,
        )?;
        let modifier_args = resolve_class_override_modifier_targets(
            tree,
            effective_def_id,
            class_redeclare_modifier_args(mod_expr),
        )?;
        class_overrides.insert(
            alias_def_id,
            ast::ClassOverride::new(
                target_name.clone(),
                alias_def_id,
                effective_def_id,
                class_redeclare_target_ref(mod_expr),
            )
            .with_modifier_args(modifier_args),
        );
        appended = true;
    }
    Ok(appended)
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

    macro_rules! assert_not_implemented {
        ($ty:ty, $bound:path) => {
            const _: fn() = || {
                trait AmbiguousIfImplemented<Marker> {
                    fn probe() {}
                }
                impl<T> AmbiguousIfImplemented<()> for T {}
                struct Implements;
                impl<T: $bound> AmbiguousIfImplemented<Implements> for T {}
                let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
            };
        };
    }

    assert_not_implemented!(NestedTypeOverrides, ::core::clone::Clone);
    assert_not_implemented!(NestedTypeOverrides, ::core::default::Default);
    assert_not_implemented!(NestedComponentTypeSelections, ::core::clone::Clone);
    assert_not_implemented!(NestedComponentTypeSelections, ::core::default::Default);

    #[test]
    fn compiler_rejects_generic_construction_or_duplication_of_nested_selection_plans() {}

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

    fn resolved_forwarding_fixture() -> ast::ClassTree {
        crate::test_support::resolved_tree(
            "nested_scope_forwarding.mo",
            r"
package SourceMedium end SourceMedium;
package ConcreteMedium
  extends SourceMedium;
end ConcreteMedium;
model Holder
  replaceable package Medium = SourceMedium constrainedby SourceMedium;
end Holder;
model Use
  replaceable package Medium = SourceMedium constrainedby SourceMedium;
  Holder h(redeclare package Medium = Medium);
end Use;
",
        )
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
                        value: Some(Arc::new(make_int_expr(1))),
                        span: rumoca_core::Span::DUMMY,
                    },
                    ast::Expression::Modification {
                        target: make_comp_ref(&["max"]),
                        value: Some(Arc::new(make_int_expr(2))),
                        span: rumoca_core::Span::DUMMY,
                    },
                ],
                each_flags: vec![false, false],
                final_flags: vec![false, false],
                redeclare_flags: vec![false, false],
                span: rumoca_core::Span::DUMMY,
            },
        );

        let keys = collect_targeted_mod_keys(&comp, &IndexMap::default());
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
        let tree = resolved_forwarding_fixture();
        let source_medium = tree
            .get_class_by_qualified_name("Use.Medium")
            .and_then(|class| class.def_id)
            .expect("resolved forwarding alias");
        let concrete_medium = tree
            .get_class_by_qualified_name("ConcreteMedium")
            .and_then(|class| class.def_id)
            .expect("resolved selected package");
        let mod_expr = tree
            .get_class_by_qualified_name("Use")
            .and_then(|class| class.components.get("h"))
            .and_then(|component| component.modifications.get("Medium"))
            .expect("resolved forwarding modifier");
        let mut type_overrides = TypeOverrideMap::new();
        type_overrides.insert_alias(source_medium, concrete_medium);

        let remapped = remap_redeclare_class_modifier(&tree, mod_expr, "Medium", &type_overrides)
            .expect("resolved override remaps");

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

        let (class_overrides, has_forwarding_class_redeclare, _, _) =
            resolve_component_nested_type_overrides(
                &ast::ClassTree::default(),
                &comp,
                Some(&target_class),
                &ast::ModificationEnvironment::new(),
                &TypeOverrideMap::new(),
            )
            .expect("component-field redeclare must not require a nested class DefId")
            .into_parts();

        assert!(class_overrides.is_empty());
        assert!(!has_forwarding_class_redeclare);
    }

    #[test]
    fn normalized_alias_shape_cannot_forge_a_source_forwarding_redeclare() {
        let slot_def_id = DefId::new(99);
        let selected_def_id = DefId::new(102);
        let mut source_alias = make_comp_ref(&["equalityConstraint"]);
        source_alias.set_target_def_id(Some(slot_def_id));
        let mut comp = ast::Component {
            name: "recordValue".to_string(),
            ..ast::Component::empty_with_span(test_span())
        };
        comp.source_modifications
            .push(ast::Expression::ClassModification {
                target: source_alias,
                modifications: Vec::new(),
                each_flags: Vec::new(),
                final_flags: Vec::new(),
                redeclare_flags: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            });
        comp.source_modification_redeclare_flags.push(true);
        let mut selected_target = make_comp_ref(&["AlternateConstraint"]);
        selected_target.set_target_def_id(Some(selected_def_id));
        comp.modifications.insert(
            "equalityConstraint".to_string(),
            ast::Expression::Modification {
                target: make_comp_ref(&["equalityConstraint"]),
                value: Some(Arc::new(ast::Expression::ClassModification {
                    target: selected_target,
                    modifications: Vec::new(),
                    each_flags: Vec::new(),
                    final_flags: Vec::new(),
                    redeclare_flags: Vec::new(),
                    span: rumoca_core::Span::DUMMY,
                })),
                span: rumoca_core::Span::DUMMY,
            },
        );

        assert!(
            checked_source_forwarding_witness(SourceForwardingEvidence {
                tree: &ast::ClassTree::default(),
                type_overrides: &TypeOverrideMap::new(),
                is_redeclare: true,
                source: &comp.source_modifications[0],
                resolved: &comp.modifications["equalityConstraint"],
                target_name: "equalityConstraint",
                alias_def_id: slot_def_id,
            })
            .expect("non-forwarding evidence is well formed")
            .is_none(),
        );
    }

    #[test]
    fn exact_alias_identity_preserves_a_true_source_forwarding_redeclare() {
        let tree = resolved_forwarding_fixture();
        let slot_def_id = tree
            .get_class_by_qualified_name("Holder.Medium")
            .and_then(|class| class.def_id)
            .expect("resolved redeclare slot");
        let forwarding_alias_def_id = tree
            .get_class_by_qualified_name("Use.Medium")
            .and_then(|class| class.def_id)
            .expect("resolved forwarding alias");
        let effective_def_id = tree
            .get_class_by_qualified_name("ConcreteMedium")
            .and_then(|class| class.def_id)
            .expect("resolved selected package");
        let comp = tree
            .get_class_by_qualified_name("Use")
            .and_then(|class| class.components.get("h"))
            .expect("resolved Holder occurrence");
        let mut type_overrides = TypeOverrideMap::new();
        type_overrides.insert_alias(forwarding_alias_def_id, effective_def_id);

        let witness = checked_source_forwarding_witness(SourceForwardingEvidence {
            tree: &tree,
            type_overrides: &type_overrides,
            is_redeclare: true,
            source: &comp.source_modifications[0],
            resolved: &comp.modifications["Medium"],
            target_name: "Medium",
            alias_def_id: slot_def_id,
        })
        .expect("forwarding evidence is well formed")
        .expect("exact forwarding witness");
        assert_eq!(witness.lhs_slot_def_id(), slot_def_id);
        assert_eq!(witness.rhs_alias_def_id(), forwarding_alias_def_id);
        assert_eq!(witness.effective_target_def_id(), effective_def_id);
    }

    #[test]
    fn malformed_self_forwarding_evidence_is_not_direct_redeclare_absence() {
        let slot_def_id = DefId::new(99);
        let rhs_alias_def_id = DefId::new(101);
        let source = ast::Expression::ClassModification {
            target: make_comp_ref(&["equalityConstraint"]),
            modifications: Vec::new(),
            each_flags: Vec::new(),
            final_flags: Vec::new(),
            redeclare_flags: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };
        let tree = ast::ClassTree::default();
        let type_overrides = TypeOverrideMap::new();
        let missing_identity = checked_source_forwarding_witness(SourceForwardingEvidence {
            tree: &tree,
            type_overrides: &type_overrides,
            is_redeclare: true,
            source: &source,
            resolved: &source,
            target_name: "equalityConstraint",
            alias_def_id: slot_def_id,
        })
        .expect_err("self-forwarding LHS without DefId must be malformed");
        assert!(
            missing_identity
                .to_string()
                .contains("LHS has no exact resolved DefId")
        );

        let mut resolved_alias = make_comp_ref(&["equalityConstraint"]);
        resolved_alias.set_target_def_id(Some(rhs_alias_def_id));
        let resolved = ast::Expression::ClassModification {
            target: resolved_alias,
            modifications: Vec::new(),
            each_flags: Vec::new(),
            final_flags: Vec::new(),
            redeclare_flags: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };
        let mut tree = ast::ClassTree::default();
        tree.definitions.classes.insert(
            "equalityConstraint".to_string(),
            ast::ClassDef {
                def_id: Some(rhs_alias_def_id),
                ..Default::default()
            },
        );
        tree.def_map
            .insert(rhs_alias_def_id, "equalityConstraint".to_string());
        let mut exact_source_alias = make_comp_ref(&["equalityConstraint"]);
        exact_source_alias.set_target_def_id(Some(slot_def_id));
        let exact_source = ast::Expression::ClassModification {
            target: exact_source_alias,
            modifications: Vec::new(),
            each_flags: Vec::new(),
            final_flags: Vec::new(),
            redeclare_flags: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };
        let type_overrides = TypeOverrideMap::new();
        let missing_mapping = checked_source_forwarding_witness(SourceForwardingEvidence {
            tree: &tree,
            type_overrides: &type_overrides,
            is_redeclare: true,
            source: &exact_source,
            resolved: &resolved,
            target_name: "equalityConstraint",
            alias_def_id: slot_def_id,
        })
        .expect_err("self-forwarding RHS without active map must be malformed");
        assert!(
            missing_mapping
                .to_string()
                .contains("no exact active alias mapping")
        );
    }
}
