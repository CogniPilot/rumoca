//! Materialize effective inherited members into class scopes.

use crate::Resolver;
use indexmap::map::Entry;
use rumoca_core::{ComponentPath, DefId};
use rumoca_ir_ast::{AstIndexMap as IndexMap, InheritedMember};
use std::collections::{HashMap, HashSet};

type InheritedMembers = IndexMap<ComponentPath, InheritedMember>;

#[derive(Clone, Copy)]
enum InheritedDeclaration<'a> {
    Class(&'a rumoca_ir_ast::ClassDef),
    Component(&'a rumoca_ir_ast::Component),
}

impl Resolver {
    /// Populate the inherited-member view after every extends edge is known.
    ///
    /// Imports are deliberately excluded (MLS §13.2.2). Direct members of a
    /// base override that base's inherited view; declarations contributed by
    /// different bases become ambiguous unless they carry the same DefId.
    pub(crate) fn populate_inherited_scope_members(
        &mut self,
        definitions: &rumoca_ir_ast::StoredDefinition,
    ) {
        let mut class_ids: Vec<_> = self.class_def_scopes.keys().copied().collect();
        class_ids.sort_unstable_by_key(DefId::index);

        let declarations = collect_declarations(definitions);
        let mut memo = HashMap::new();
        let mut entries = Vec::with_capacity(class_ids.len());
        for class_id in class_ids {
            let members =
                self.inherited_members_for(class_id, &declarations, &mut memo, &mut HashSet::new());
            if let Some(&scope) = self.class_def_scopes.get(&class_id) {
                entries.push((scope, members));
            }
        }

        for (scope, members) in entries {
            self.scope_tree.set_inherited_members(scope, members);
        }
    }

    fn inherited_members_for(
        &self,
        class_id: DefId,
        declarations: &HashMap<DefId, InheritedDeclaration<'_>>,
        memo: &mut HashMap<DefId, InheritedMembers>,
        visiting: &mut HashSet<DefId>,
    ) -> InheritedMembers {
        if let Some(members) = memo.get(&class_id) {
            return members.clone();
        }
        if !visiting.insert(class_id) {
            return InheritedMembers::default();
        }

        let reconcilable = extends_modified_names(class_id, declarations);
        let mut inherited = InheritedMembers::default();
        for base_id in self.class_to_bases.get(&class_id).into_iter().flatten() {
            let mut visible = self.inherited_members_for(*base_id, declarations, memo, visiting);
            self.add_direct_base_members(*base_id, &mut visible);
            merge_base_members(&mut inherited, visible, declarations, &reconcilable);
        }
        self.apply_extends_class_redeclarations(class_id, declarations, &mut inherited);

        visiting.remove(&class_id);
        memo.insert(class_id, inherited.clone());
        inherited
    }

    /// Replace an inherited class element by the class an `extends`-clause
    /// redeclaration names.
    ///
    /// MLS §7.3: `extends PM(redeclare record State = SR)` replaces the element
    /// `State` that the derived class inherits from `PM`, so every lookup of
    /// `State` in the derived class (its own functions included) and in its
    /// descendants sees `SR`. Only a short class definition without its own
    /// modification is a plain class identity; a redeclared package keeps its
    /// slot because instantiation applies package selections per occurrence.
    fn apply_extends_class_redeclarations(
        &self,
        class_id: DefId,
        declarations: &HashMap<DefId, InheritedDeclaration<'_>>,
        inherited: &mut InheritedMembers,
    ) {
        let Some(InheritedDeclaration::Class(class)) = declarations.get(&class_id) else {
            return;
        };
        let Some(&scope) = self.class_def_scopes.get(&class_id) else {
            return;
        };
        let redeclarations = class
            .extends
            .iter()
            .flat_map(|extend| &extend.modifications)
            .filter(|modification| modification.redeclare)
            .filter_map(|modification| class_alias_redeclaration(&modification.expr));
        for (name, replacement) in redeclarations {
            let Some(InheritedMember::Unique(slot)) = inherited.get(&name).copied() else {
                continue;
            };
            if !is_redeclarable_class_identity(slot, declarations) {
                continue;
            }
            let Some(target) = self.resolve_reference_in_scope(replacement, scope) else {
                continue;
            };
            if target != slot
                && matches!(
                    declarations.get(&target),
                    Some(InheritedDeclaration::Class(_))
                )
            {
                inherited.insert(name, InheritedMember::Unique(target));
            }
        }
    }

    /// Resolve an unsubscripted dotted class reference written in `scope`.
    fn resolve_reference_in_scope(
        &self,
        reference: &rumoca_ir_ast::ComponentReference,
        scope: rumoca_core::ScopeId,
    ) -> Option<DefId> {
        if reference.local || reference.parts.iter().any(|part| part.subs.is_some()) {
            return None;
        }
        let (head, tail) = reference.parts.split_first()?;
        let mut current = self.scope_tree.lookup_excluding(
            scope,
            &ComponentPath::from_flat_path(&head.ident.text),
            None,
        )?;
        for part in tail {
            current = self.lookup_class_member(current, &part.ident.text)?;
        }
        Some(current)
    }

    fn add_direct_base_members(&self, base_id: DefId, visible: &mut InheritedMembers) {
        let Some(base_scope) = self
            .class_def_scopes
            .get(&base_id)
            .and_then(|scope| self.scope_tree.get(*scope))
        else {
            return;
        };
        for (name, def_id) in &base_scope.members {
            visible.insert(name.clone(), InheritedMember::Unique(*def_id));
        }
    }
}

/// The element name and class reference of `redeclare <class> Name = Type`
/// without a modification of its own.
fn class_alias_redeclaration(
    expression: &rumoca_ir_ast::Expression,
) -> Option<(ComponentPath, &rumoca_ir_ast::ComponentReference)> {
    let rumoca_ir_ast::Expression::Modification { target, value, .. } = expression else {
        return None;
    };
    let rumoca_ir_ast::Expression::ClassModification {
        target: replacement,
        modifications,
        ..
    } = value.as_ref()
    else {
        return None;
    };
    let [part] = target.parts.as_slice() else {
        return None;
    };
    if part.subs.is_some() || !modifications.is_empty() {
        return None;
    }
    Some((ComponentPath::from_flat_path(&part.ident.text), replacement))
}

/// A redeclared class element whose identity is the replacing class: any
/// class except a package (a component slot is redeclared by instantiation).
fn is_redeclarable_class_identity(
    slot: DefId,
    declarations: &HashMap<DefId, InheritedDeclaration<'_>>,
) -> bool {
    matches!(
        declarations.get(&slot),
        Some(InheritedDeclaration::Class(class))
            if class.class_type != rumoca_core::ClassType::Package
    )
}

fn collect_declarations(
    definitions: &rumoca_ir_ast::StoredDefinition,
) -> HashMap<DefId, InheritedDeclaration<'_>> {
    let mut declarations = HashMap::new();
    for class in definitions.classes.values() {
        collect_class_declarations(class, &mut declarations);
    }
    declarations
}

fn collect_class_declarations<'a>(
    class: &'a rumoca_ir_ast::ClassDef,
    declarations: &mut HashMap<DefId, InheritedDeclaration<'a>>,
) {
    if let Some(def_id) = class.def_id {
        declarations.insert(def_id, InheritedDeclaration::Class(class));
    }
    for component in class.components.values() {
        if let Some(def_id) = component.def_id {
            declarations.insert(def_id, InheritedDeclaration::Component(component));
        }
    }
    for nested in class.classes.values() {
        collect_class_declarations(nested, declarations);
    }
}

fn declarations_are_compatible(
    existing: DefId,
    candidate: DefId,
    declarations: &HashMap<DefId, InheritedDeclaration<'_>>,
) -> bool {
    if existing == candidate {
        return true;
    }
    match (declarations.get(&existing), declarations.get(&candidate)) {
        (
            Some(InheritedDeclaration::Component(existing)),
            Some(InheritedDeclaration::Component(candidate)),
        ) => rumoca_ir_ast::components_are_semantically_compatible(existing, candidate),
        (
            Some(InheritedDeclaration::Class(existing)),
            Some(InheritedDeclaration::Class(candidate)),
        ) => rumoca_ir_ast::classes_are_semantically_compatible(existing, candidate),
        _ => false,
    }
}

/// Member names an `extends`-clause modification of `class_id` reaches.
///
/// MLS §5.6.1.4 compares duplicate inherited elements *after* the enclosing
/// class's extends-clause modifications have been applied to them, and keeps
/// the first. Resolve has no modification environment — that belongs to
/// instantiation — so it cannot prove that two such declarations stay
/// distinct, and must not turn the name into a lookup failure.
fn extends_modified_names(
    class_id: DefId,
    declarations: &HashMap<DefId, InheritedDeclaration<'_>>,
) -> HashSet<ComponentPath> {
    let Some(InheritedDeclaration::Class(class)) = declarations.get(&class_id) else {
        return HashSet::new();
    };
    let mut names = HashSet::new();
    for extend in &class.extends {
        for modification in &extend.modifications {
            insert_modification_root(&modification.expr, &mut names);
        }
        for break_name in &extend.break_names {
            names.insert(ComponentPath::from_flat_path(break_name));
        }
    }
    names
}

/// Record the outermost element name a single extends modification targets.
fn insert_modification_root(
    expression: &rumoca_ir_ast::Expression,
    names: &mut HashSet<ComponentPath>,
) {
    use rumoca_ir_ast::Expression;
    let target = match expression {
        Expression::Modification { target, .. } | Expression::ClassModification { target, .. } => {
            target
        }
        Expression::NamedArgument { name, .. } => {
            names.insert(ComponentPath::from_flat_path(&name.text));
            return;
        }
        _ => return,
    };
    if let Some(part) = target.parts.first() {
        names.insert(ComponentPath::from_flat_path(&part.ident.text));
    }
}

fn merge_base_members(
    target: &mut InheritedMembers,
    source: InheritedMembers,
    declarations: &HashMap<DefId, InheritedDeclaration<'_>>,
    reconcilable: &HashSet<ComponentPath>,
) {
    for (name, candidate) in source {
        let modified_by_extends = reconcilable.contains(&name);
        match target.entry(name) {
            Entry::Vacant(entry) => {
                entry.insert(candidate);
            }
            Entry::Occupied(entry)
                if matches!(
                    (*entry.get(), candidate),
                    (InheritedMember::Unique(existing), InheritedMember::Unique(candidate))
                        if declarations_are_compatible(existing, candidate, declarations)
                ) => {}
            // MLS §5.6.1.4: the first duplicate element is the one kept, so a
            // name an extends modification may still reconcile keeps binding.
            Entry::Occupied(_) if modified_by_extends => {}
            Entry::Occupied(mut entry) if *entry.get() != candidate => {
                entry.insert(InheritedMember::Ambiguous);
            }
            Entry::Occupied(_) => {}
        }
    }
}
