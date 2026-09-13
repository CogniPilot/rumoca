//! Callable reference interfaces for MLS §§6.3, 6.6, and 7.3.2.
//!
//! A reference exposes its constraining interface. Its declaration's
//! `replaceable` prefix governs redeclaration of the slot, not the signature
//! obtained by referring to it. Construction also checks the default against
//! its constraint before exposing that interface.

use super::*;

#[derive(Clone)]
struct FunctionInterface {
    pure: bool,
    final_: bool,
    members: IndexMap<String, ast::Component>,
}

impl FunctionInterface {
    fn compatible_with(&self, tree: &ast::ClassTree, constraint: &Self) -> bool {
        (!constraint.pure || self.pure)
            && (!constraint.final_ || self.final_)
            && crate::plug_compat::public_members_plug_compatible(
                tree,
                &self.members,
                &constraint.members,
                &rumoca_core::ClassType::Function,
            )
    }
}

#[derive(Default)]
struct FunctionInterfaces {
    references: IndexMap<DefId, FunctionInterface>,
    active: IndexSet<DefId>,
    inherited: InheritanceCache,
}

impl FunctionInterfaces {
    fn reference(&mut self, tree: &ast::ClassTree, id: DefId) -> Option<FunctionInterface> {
        if let Some(interface) = self.references.get(&id) {
            return Some(interface.clone());
        }
        if !self.active.insert(id) {
            return None;
        }
        let result = self.construct_reference(tree, id);
        self.active.shift_remove(&id);
        if let Some(interface) = &result {
            self.references.insert(id, interface.clone());
        }
        result
    }

    fn construct_reference(
        &mut self,
        tree: &ast::ClassTree,
        id: DefId,
    ) -> Option<FunctionInterface> {
        let class = tree.get_class_by_def_id(id)?;
        if class.class_type != rumoca_core::ClassType::Function {
            return None;
        }
        let actual = self.actual(tree, class)?;
        let Some(constraint) = class
            .constrainedby
            .as_ref()
            .filter(|_| class.is_replaceable)
        else {
            return Some(actual);
        };
        let constraint = self.reference(tree, constraint.def_id?)?;
        actual
            .compatible_with(tree, &constraint)
            .then_some(constraint)
    }

    fn actual(
        &mut self,
        tree: &ast::ClassTree,
        class: &ast::ClassDef,
    ) -> Option<FunctionInterface> {
        // The parser retains the end-name token on long definitions. A short
        // function definition has exactly one alias edge and no end-name.
        if class.end_name_token.is_none() {
            let [base] = class.extends.as_slice() else {
                return None;
            };
            let mut interface =
                self.reference(tree, base.base_def_id.or(base.base_name.def_id)?)?;
            apply_alias_modifiers(tree, &mut interface, base)?;
            if class.purity_declared {
                interface.pure &= class.pure;
            }
            interface.final_ |= class.is_final;
            return Some(interface);
        }
        let mut members =
            get_effective_components_with_cache(tree, class, &mut self.inherited).ok()?;
        members.retain(|_, member| !member.is_protected);
        Some(FunctionInterface {
            pure: class.pure,
            final_: class.is_final,
            members,
        })
    }
}

fn apply_alias_modifiers(
    tree: &ast::ClassTree,
    interface: &mut FunctionInterface,
    extend: &ast::Extend,
) -> Option<()> {
    if extend.modifications.is_empty() {
        return Some(());
    }
    let mut values = IndexMap::default();
    let mut valid = true;
    walk_extend_modifications(extend, |modification| {
        let Some(name) = extract_extend_modification_target(extend, &modification.expr) else {
            valid = false;
            return;
        };
        if modification.redeclare || !interface.members.contains_key(&name) {
            valid = false;
            return;
        }
        if let Some((name, value, final_)) =
            try_extract_value_modification_any(modification, extend)
        {
            values.insert(name, (value, final_));
        }
    });
    if !valid {
        return None;
    }
    let span = location_to_span(
        &extend.location,
        &tree.source_map,
        "function alias modifiers",
    )
    .ok()?;
    let mut content = InheritedContent {
        components: std::mem::take(&mut interface.members),
        ..Default::default()
    };
    apply_value_modifications(&mut content, values, span).ok()?;
    merge_nested_extends_modifications(&mut content, extend);
    interface.members = content.components;
    Some(())
}

pub(crate) fn function_reference_compatible(
    tree: &ast::ClassTree,
    replacement: DefId,
    constraint: DefId,
    modifiers: &[ast::Expression],
) -> bool {
    let mut interfaces = FunctionInterfaces::default();
    let Some(mut replacement_interface) = interfaces.reference(tree, replacement) else {
        return false;
    };
    let Some(constraint_interface) = interfaces.reference(tree, constraint) else {
        return false;
    };
    if !modifiers.is_empty() {
        let Some(class) = tree.get_class_by_def_id(replacement) else {
            return false;
        };
        let extend = ast::Extend {
            base_name: ast::Name::from_string(&class.name.text),
            base_def_id: Some(replacement),
            location: class.location.clone(),
            modifications: modifiers
                .iter()
                .map(|expr| ast::ExtendModification {
                    expr: expr.clone(),
                    each: false,
                    final_: false,
                    redeclare: false,
                })
                .collect(),
            ..Default::default()
        };
        if apply_alias_modifiers(tree, &mut replacement_interface, &extend).is_none() {
            return false;
        }
    }
    replacement_interface.compatible_with(tree, &constraint_interface)
}
