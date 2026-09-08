//! Class identity proofs for redeclare values.
//!
//! A redeclare value arrives as a modification, class-modification, function
//! call, or plain reference expression, optionally forwarded through the
//! modification environment. Each shape is unwrapped until the exact class
//! declaration identity proved by Resolve is reached.

use crate::{InstantiateError, InstantiateResult};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rustc_hash::FxHashSet;

pub(crate) fn resolve_redeclare_value_def_id(
    tree: &ast::ClassTree,
    value: &ast::Expression,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> InstantiateResult<Option<DefId>> {
    resolve_redeclare_value_def_id_inner(tree, value, mod_env, &mut FxHashSet::default())
}

fn resolve_redeclare_value_def_id_inner(
    tree: &ast::ClassTree,
    value: &ast::Expression,
    mod_env: Option<&ast::ModificationEnvironment>,
    active: &mut FxHashSet<DefId>,
) -> InstantiateResult<Option<DefId>> {
    match value {
        ast::Expression::Modification {
            value: Some(value), ..
        } => resolve_redeclare_value_def_id_inner(tree, value, mod_env, active),
        ast::Expression::ClassModification { target, .. }
        | ast::Expression::FunctionCall { comp: target, .. }
        | ast::Expression::ComponentReference(target) => {
            resolve_cref_or_forwarding_value(tree, target, mod_env, active)
        }
        _ => Ok(None),
    }
}

fn resolve_cref_or_forwarding_value(
    tree: &ast::ClassTree,
    cref: &ast::ComponentReference,
    mod_env: Option<&ast::ModificationEnvironment>,
    active: &mut FxHashSet<DefId>,
) -> InstantiateResult<Option<DefId>> {
    let def_id = resolve_cref_def_id(cref).ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            format!("redeclare value `{cref}`"),
            cref.span,
        ))
    })?;
    let Some(mod_env) = mod_env else {
        validate_redeclare_target(tree, def_id, cref)?;
        return Ok(Some(def_id));
    };
    let Some(qn) = cref_to_qualified_name(cref) else {
        return Err(Box::new(InstantiateError::missing_resolved_identity(
            "empty redeclare value reference",
            cref.span,
        )));
    };
    let Some(mod_value) = mod_env.get(&qn) else {
        validate_redeclare_target(tree, def_id, cref)?;
        return Ok(Some(def_id));
    };
    if !active.insert(def_id) {
        return Err(Box::new(InstantiateError::instantiation_cycle(
            format!("redeclare forwarding value `{cref}` ({def_id:?})"),
            cref.span,
        )));
    }
    let resolved =
        resolve_redeclare_value_def_id_inner(tree, &mod_value.value, Some(mod_env), active);
    active.remove(&def_id);
    resolved
}

fn validate_redeclare_target(
    tree: &ast::ClassTree,
    def_id: DefId,
    cref: &ast::ComponentReference,
) -> InstantiateResult<()> {
    let is_predefined = rumoca_core::BUILTIN_TYPES.iter().any(|name| {
        tree.scope_tree
            .predefined_member(&rumoca_core::ComponentPath::from_flat_path(name))
            == Some(def_id)
    });
    if tree.get_class_by_def_id(def_id).is_none() && !is_predefined {
        return Err(Box::new(InstantiateError::missing_resolved_identity(
            format!("redeclare class target `{cref}` ({def_id:?})"),
            cref.span,
        )));
    }
    Ok(())
}

pub(super) fn cref_to_qualified_name(cref: &ast::ComponentReference) -> Option<ast::QualifiedName> {
    let mut parts = cref.parts.iter();
    let first = parts.next()?;
    let mut qn = ast::QualifiedName::from_ident(first.ident.text.as_ref());
    for part in parts {
        qn = qn.child(part.ident.text.as_ref());
    }
    Some(qn)
}

pub(crate) fn resolve_cref_def_id(cref: &ast::ComponentReference) -> Option<DefId> {
    // Resolve has already proved class-reference identity. Prefer its exact
    // final target because a one-token reference may itself contain a dotted
    // class path. Only a truly one-part reference can use its root identity.
    if let Some(target_def_id) = cref.target_def_id() {
        return Some(target_def_id);
    }
    (cref.parts.len() == 1)
        .then_some(cref.root_def_id())
        .flatten()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: text.into(),
            ..Default::default()
        }
    }

    fn reference(name: &str, def_id: DefId) -> ast::Expression {
        ast::Expression::ComponentReference(ast::ComponentReference {
            local: false,
            parts: vec![ast::ComponentRefPart {
                ident: token(name),
                subs: None,
                def_id: Some(def_id),
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        })
    }

    fn assert_mod_env_matches(
        current: &ast::ModificationEnvironment,
        snapshot: &ast::ModificationEnvironment,
    ) {
        assert_eq!(current.active.len(), snapshot.active.len());
        for (key, before) in &snapshot.active {
            let after = current
                .active
                .get(key)
                .expect("snapshot key remains present");
            assert_eq!(after.value, before.value);
            assert_eq!(after.source, before.source);
            assert_eq!(after.source_scope, before.source_scope);
            assert_eq!(after.each, before.each);
            assert_eq!(after.final_, before.final_);
        }
    }

    #[test]
    fn forwarding_follows_more_than_the_old_limit() {
        let tree = crate::test_support::resolved_tree(
            "long_redeclare_forwarding.mo",
            "package P model Final end Final; end P;",
        );
        let final_id = tree
            .get_class_by_qualified_name("P.Final")
            .and_then(|class| class.def_id)
            .expect("resolved final target");
        let mut mod_env = ast::ModificationEnvironment::default();
        for index in 0..12_u32 {
            let name = format!("p{index}");
            let value = if index == 11 {
                reference("Final", final_id)
            } else {
                reference(&format!("p{}", index + 1), DefId::new(1_001 + index))
            };
            mod_env.add(
                ast::QualifiedName::from_ident(&name),
                ast::ModificationValue::simple(value),
            );
        }
        let resolved = resolve_redeclare_value_def_id(
            &tree,
            &reference("p0", DefId::new(1_000)),
            Some(&mod_env),
        )
        .expect("long acyclic forwarding chain resolves");
        assert_eq!(resolved, Some(final_id));
    }

    #[test]
    fn forwarding_cycle_is_atomic() {
        let tree = ast::ClassTree::default();
        let mut mod_env = ast::ModificationEnvironment::default();
        mod_env.add(
            ast::QualifiedName::from_ident("a"),
            ast::ModificationValue::simple(reference("b", DefId::new(1_201))),
        );
        mod_env.add(
            ast::QualifiedName::from_ident("b"),
            ast::ModificationValue::simple(reference("a", DefId::new(1_200))),
        );
        let snapshot = mod_env.clone();
        let error = resolve_redeclare_value_def_id(
            &tree,
            &reference("a", DefId::new(1_200)),
            Some(&mod_env),
        )
        .expect_err("forwarding cycle is invalid");
        assert!(matches!(
            *error,
            InstantiateError::InstantiationCycle { .. }
        ));
        assert_mod_env_matches(&mod_env, &snapshot);
    }

    #[test]
    fn final_target_requires_catalog_identity_and_ignores_spelling_collisions() {
        let tree = crate::test_support::resolved_tree(
            "redeclare_target_identity.mo",
            r"
package A model Foo end Foo; end A;
package B model Foo end Foo; end B;
",
        );
        let first = tree
            .get_class_by_qualified_name("A.Foo")
            .and_then(|class| class.def_id)
            .expect("first exact class target");
        let second = tree
            .get_class_by_qualified_name("B.Foo")
            .and_then(|class| class.def_id)
            .expect("second exact class target");
        assert_ne!(first, second);
        assert_eq!(
            resolve_redeclare_value_def_id(&tree, &reference("Foo", second), None)
                .expect("exact class target resolves"),
            Some(second)
        );
        let error =
            resolve_redeclare_value_def_id(&tree, &reference("Foo", DefId::new(1_399)), None)
                .expect_err("unissued class identity fails closed");
        assert!(matches!(
            *error,
            InstantiateError::MissingResolvedIdentity { .. }
        ));
    }
}
