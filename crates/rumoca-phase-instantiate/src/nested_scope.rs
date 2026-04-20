use super::type_overrides::{extract_component_class_overrides, find_nested_class_in_hierarchy};
use super::{InstantiateContext, InstantiateResult};
use indexmap::IndexMap;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use std::collections::BTreeSet;

pub(super) type NestedTypeOverrides = (IndexMap<String, DefId>, bool, IndexMap<String, DefId>);

pub(super) fn collect_referenced_mod_roots(comp: &ast::Component) -> BTreeSet<String> {
    let mut roots = BTreeSet::new();
    for expr in comp.modifications.values() {
        collect_expression_roots(expr, &mut roots);
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

fn collect_expression_roots(expr: &ast::Expression, roots: &mut BTreeSet<String>) {
    match expr {
        ast::Expression::ComponentReference(comp_ref) => {
            if let Some(first) = comp_ref.parts.first() {
                roots.insert(first.ident.text.to_string());
            }
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            collect_expression_roots(lhs, roots);
            collect_expression_roots(rhs, roots);
        }
        ast::Expression::Unary { rhs, .. } => {
            collect_expression_roots(rhs, roots);
        }
        ast::Expression::Parenthesized { inner } => {
            collect_expression_roots(inner, roots);
        }
        ast::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, branch) in branches {
                collect_expression_roots(cond, roots);
                collect_expression_roots(branch, roots);
            }
            collect_expression_roots(else_branch, roots);
        }
        ast::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_expression_roots(arg, roots);
            }
        }
        ast::Expression::ClassModification { modifications, .. } => {
            for m in modifications {
                collect_expression_roots(m, roots);
            }
        }
        ast::Expression::Modification { value, .. } => {
            collect_expression_roots(value, roots);
        }
        ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements } => {
            for e in elements {
                collect_expression_roots(e, roots);
            }
        }
        ast::Expression::ArrayComprehension { expr, filter, .. } => {
            collect_expression_roots(expr, roots);
            if let Some(f) = filter {
                collect_expression_roots(f, roots);
            }
        }
        ast::Expression::ArrayIndex { base, subscripts } => {
            collect_expression_roots(base, roots);
            for sub in subscripts {
                if let rumoca_ir_ast::Subscript::Expression(e) = sub {
                    collect_expression_roots(e, roots);
                }
            }
        }
        ast::Expression::FieldAccess { base, .. } => {
            collect_expression_roots(base, roots);
        }
        _ => {}
    }
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
    let mut keys = IndexMap::new();

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
    let mut keys = IndexMap::new();
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

    // Add shifted modifications
    for (path, value) in shifted {
        ctx.mod_env_mut().add(path, value);
    }
}

/// Remap a class-redeclare modifier target to the active enclosing override.
///
/// MLS §7.3: `redeclare package Medium = Medium` and
/// `redeclare package Medium = MediumAir` inside component modifiers should
/// forward through active enclosing package aliases, not the local replaceable
/// defaults declared on the component class.
pub(super) fn remap_redeclare_class_modifier(
    tree: &ast::ClassTree,
    mod_expr: &ast::Expression,
    target_name: &str,
    type_overrides: &IndexMap<String, DefId>,
) -> ast::Expression {
    let ast::Expression::ClassModification {
        target,
        modifications,
    } = mod_expr
    else {
        return mod_expr.clone();
    };

    let Some(last) = target.parts.last() else {
        return mod_expr.clone();
    };
    let rhs_name = last.ident.text.as_ref();
    let override_name = if rhs_name == target_name {
        target_name
    } else {
        rhs_name
    };

    let Some(&override_def_id) = type_overrides.get(override_name) else {
        return mod_expr.clone();
    };
    if target.def_id == Some(override_def_id) {
        return mod_expr.clone();
    }

    let mut remapped_target = target.clone();
    remapped_target.def_id = Some(override_def_id);
    if let Some(qualified_name) = tree.def_map.get(&override_def_id) {
        let template_ident = remapped_target
            .parts
            .last()
            .map(|part| part.ident.clone())
            .unwrap_or_default();
        let new_parts: Vec<ast::ComponentRefPart> = qualified_name
            .split('.')
            .map(|segment| {
                let mut ident: rumoca_ir_core::Token = template_ident.clone();
                ident.text = segment.to_string().into();
                ast::ComponentRefPart { ident, subs: None }
            })
            .collect();
        if !new_parts.is_empty() {
            remapped_target.parts = new_parts;
        }
    }
    ast::Expression::ClassModification {
        target: remapped_target,
        modifications: modifications.clone(),
    }
}

fn is_self_forwarding_redeclare(mod_expr: &ast::Expression, target_name: &str) -> bool {
    let ast::Expression::ClassModification { target, .. } = mod_expr else {
        return false;
    };
    target
        .parts
        .last()
        .is_some_and(|part| part.ident.text.as_ref() == target_name)
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
    type_overrides: &IndexMap<String, DefId>,
) -> InstantiateResult<NestedTypeOverrides> {
    let mut class_overrides =
        extract_component_class_overrides(tree, comp, class_def, Some(mod_env))?;
    let mut has_forwarding_class_redeclare = false;

    if let Some(target_class) = class_def {
        for (target_name, mod_expr) in &comp.modifications {
            let is_replaceable_target =
                find_nested_class_in_hierarchy(tree, target_class, target_name)
                    .is_some_and(|nested| nested.is_replaceable)
                    || target_class
                        .components
                        .get(target_name)
                        .is_some_and(|target_comp| target_comp.is_replaceable);
            if !is_replaceable_target || !is_self_forwarding_redeclare(mod_expr, target_name) {
                continue;
            }
            if let Some(&effective_def_id) = type_overrides.get(target_name) {
                class_overrides.insert(target_name.clone(), effective_def_id);
                has_forwarding_class_redeclare = true;
            }
        }
    }

    let mut nested_type_overrides = type_overrides.clone();
    for (name, def_id) in &class_overrides {
        nested_type_overrides.insert(name.clone(), *def_id);
    }

    Ok((
        class_overrides,
        has_forwarding_class_redeclare,
        nested_type_overrides,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    fn make_token(text: &str) -> rumoca_ir_core::Token {
        rumoca_ir_core::Token {
            text: Arc::from(text),
            location: rumoca_ir_core::Location::default(),
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
                })
                .collect(),
            def_id: None,
        }
    }

    fn make_int_expr(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: make_token(&value.to_string()),
        }
    }

    #[test]
    fn test_collect_targeted_mod_keys_omits_bare_key_for_nested_attrs() {
        let mut comp = ast::Component {
            name: "port".to_string(),
            ..Default::default()
        };
        comp.modifications.insert(
            "m_flow".to_string(),
            ast::Expression::ClassModification {
                target: make_comp_ref(&["m_flow"]),
                modifications: vec![
                    ast::Expression::Modification {
                        target: make_comp_ref(&["min"]),
                        value: Arc::new(make_int_expr(1)),
                    },
                    ast::Expression::Modification {
                        target: make_comp_ref(&["max"]),
                        value: Arc::new(make_int_expr(2)),
                    },
                ],
            },
        );

        let keys = collect_targeted_mod_keys(&comp, &IndexMap::new());
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
        let mut comp = ast::Component::default();
        comp.modifications.insert(
            "k".to_string(),
            ast::Expression::ArrayIndex {
                base: Arc::new(ast::Expression::FieldAccess {
                    base: Arc::new(ast::Expression::ComponentReference(make_comp_ref(&[
                        "state",
                    ]))),
                    field: "x".to_string(),
                }),
                subscripts: vec![ast::Subscript::Expression(
                    ast::Expression::ComponentReference(make_comp_ref(&["idx"])),
                )],
            },
        );

        let roots = collect_referenced_mod_roots(&comp);
        assert!(roots.contains("state"));
        assert!(roots.contains("idx"));
    }
}
