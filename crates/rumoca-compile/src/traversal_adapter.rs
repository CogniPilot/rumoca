use indexmap::{IndexMap, IndexSet};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::{FunctionCallContext, SubscriptContext, TypeNameContext, Visitor};
use std::ops::ControlFlow::{self, Continue};

/// Exact class substitutions installed by `redeclare` (MLS §7.3).
///
/// A lookup written against a replaceable class slot cannot be bound by
/// Resolve, because the class that finally occupies the slot is chosen by a
/// redeclaration. The strict reachability closure still has to retain every
/// definition such a lookup can select, so it needs the slot-to-substitute
/// edges. Both sides are recorded as exact `DefId`s produced by Resolve, never
/// re-derived from rendered names.
pub(crate) struct RedeclareSubstitutions {
    substitutes_by_slot: IndexMap<DefId, IndexSet<DefId>>,
}

impl RedeclareSubstitutions {
    pub(crate) fn from_index(class_index: &ast::ClassDefIndex<'_>) -> Self {
        let mut substitutions = Self {
            substitutes_by_slot: IndexMap::new(),
        };
        for slot_def_id in class_index.def_ids() {
            let Some(class) = class_index.get(slot_def_id) else {
                continue;
            };
            // Element-level `redeclare class C ...` carries the replaced slot
            // as resolved semantic identity.
            if let Some(replaced_def_id) = class.redeclare_target_def_id {
                substitutions.insert(replaced_def_id, slot_def_id);
            }
            for modification in redeclare_modifications(class) {
                substitutions.insert_from_modification(class_index, modification);
            }
        }
        substitutions
    }

    fn insert(&mut self, slot_def_id: DefId, substitute_def_id: DefId) {
        if slot_def_id == substitute_def_id {
            return;
        }
        self.substitutes_by_slot
            .entry(slot_def_id)
            .or_default()
            .insert(substitute_def_id);
    }

    /// Record `redeclare <kind> Slot = Substitute` written as a modification.
    ///
    /// Only class redeclarations contribute here: both the modified slot and
    /// the substituting type must resolve to classes, which is exactly what
    /// distinguishes a class redeclaration from a component redeclaration or a
    /// value modification.
    fn insert_from_modification(
        &mut self,
        class_index: &ast::ClassDefIndex<'_>,
        modification: &ast::Expression,
    ) {
        let ast::Expression::Modification { target, value, .. } = modification else {
            return;
        };
        let ast::Expression::ClassModification {
            target: substitute, ..
        } = value.as_ref()
        else {
            return;
        };
        let (Some(slot_def_id), Some(substitute_def_id)) =
            (target.target_def_id(), substitute.target_def_id())
        else {
            return;
        };
        if class_index.get(slot_def_id).is_none() || class_index.get(substitute_def_id).is_none() {
            return;
        }
        self.insert(slot_def_id, substitute_def_id);
    }

    fn substitutes(&self, slot_def_id: DefId) -> impl Iterator<Item = DefId> + '_ {
        self.substitutes_by_slot
            .get(&slot_def_id)
            .into_iter()
            .flat_map(|substitutes| substitutes.iter().copied())
    }
}

/// Every modification a class writes with a `redeclare` prefix (MLS §7.3),
/// from its extends clauses and from its component declarations.
fn redeclare_modifications(class: &ast::ClassDef) -> impl Iterator<Item = &ast::Expression> {
    let extend_modifications = class
        .extends
        .iter()
        .flat_map(|extend| extend.modifications.iter())
        .filter(|modification| modification.redeclare)
        .map(|modification| &modification.expr);
    let component_modifications = class.components.values().flat_map(|component| {
        component
            .source_modifications
            .iter()
            .enumerate()
            .filter(|(index, _)| {
                component
                    .source_modification_redeclare_flags
                    .get(*index)
                    .copied()
                    .unwrap_or(false)
            })
            .map(|(_, modification)| modification)
    });
    extend_modifications.chain(component_modifications)
}

pub(crate) fn collect_class_dependencies(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    substitutions: &RedeclareSubstitutions,
    class: &ast::ClassDef,
    class_name: &str,
) -> IndexSet<String> {
    let mut collector = ClassDependencyCollector::new(tree, class_index, substitutions, class_name);
    assert!(
        !collector.collect_class(class).is_break(),
        "class dependency traversal stopped while collecting `{class_name}`"
    );
    collector.finish()
}

struct ClassDependencyCollector<'tree, 'index, 'name> {
    tree: &'tree ast::ClassTree,
    class_index: &'index ast::ClassDefIndex<'tree>,
    substitutions: &'index RedeclareSubstitutions,
    class_name: &'name str,
    deps: IndexSet<String>,
}

impl<'tree, 'index, 'name> ClassDependencyCollector<'tree, 'index, 'name> {
    fn new(
        tree: &'tree ast::ClassTree,
        class_index: &'index ast::ClassDefIndex<'tree>,
        substitutions: &'index RedeclareSubstitutions,
        class_name: &'name str,
    ) -> Self {
        Self {
            tree,
            class_index,
            substitutions,
            class_name,
            deps: IndexSet::new(),
        }
    }

    fn finish(mut self) -> IndexSet<String> {
        self.deps.shift_remove(self.class_name);
        self.deps
    }

    fn collect_class(&mut self, class: &ast::ClassDef) -> ControlFlow<()> {
        if let Some(constrainedby) = &class.constrainedby {
            self.visit_type_name(constrainedby, TypeNameContext::ClassConstrainedBy)?;
        }
        for extend in &class.extends {
            self.collect_extend(extend)?;
        }
        let scope_imports = class
            .scope_id
            .and_then(|scope_id| self.tree.scope_tree.get(scope_id))
            .map(|scope| scope.imports.as_slice());
        for import in &class.imports {
            self.collect_import(import, scope_imports);
        }
        for subscript in &class.array_subscripts {
            self.visit_subscript(subscript)?;
        }
        for annotation in &class.annotation {
            self.visit_expression(annotation)?;
        }

        for component in class.components.values() {
            self.collect_component(component)?;
        }

        for equation in &class.equations {
            self.visit_equation(equation)?;
        }
        for equation in &class.initial_equations {
            self.visit_equation(equation)?;
        }
        for algorithm in &class.algorithms {
            for statement in algorithm {
                self.visit_statement(statement)?;
            }
        }
        for algorithm in &class.initial_algorithms {
            for statement in algorithm {
                self.visit_statement(statement)?;
            }
        }

        if let Some(external) = &class.external {
            self.collect_external(external)?;
        }
        Continue(())
    }

    fn collect_extend(&mut self, extend: &ast::Extend) -> ControlFlow<()> {
        if let Some(base_def_id) = extend.base_def_id {
            self.add_class_dep_by_def_id(base_def_id);
        }
        self.visit_extend(extend)?;
        for annotation in &extend.annotation {
            self.visit_expression(annotation)?;
        }
        Continue(())
    }

    fn collect_component(&mut self, component: &ast::Component) -> ControlFlow<()> {
        if let Some(type_def_id) = component.type_def_id {
            self.add_class_dep_by_def_id(type_def_id);
        }
        self.visit_component(component)?;
        if let Some(binding) = &component.binding {
            self.visit_expression(binding)?;
        }
        for shape in &component.shape_expr {
            self.visit_subscript(shape)?;
        }
        Continue(())
    }

    fn collect_external(&mut self, external: &ast::ExternalFunction) -> ControlFlow<()> {
        if let Some(output) = &external.output {
            self.visit_component_reference(output)?;
        }
        for arg in &external.args {
            self.visit_expression(arg)?;
        }
        Continue(())
    }

    fn collect_import(
        &mut self,
        import: &ast::Import,
        scope_imports: Option<&[ast::scope::Import]>,
    ) {
        // MLS §13.2: qualified, renamed, and selective imports bind concrete
        // imported definitions into the class scope. Use the resolved scope
        // imports rather than Name::def_id so the dependency graph tracks the
        // imported classes instead of only the package path.
        match import {
            ast::Import::Qualified { path, .. } => {
                if !self.add_resolved_import_dep(path, scope_imports) {
                    self.add_class_dep_from_name(path);
                }
            }
            ast::Import::Renamed { path, .. } => {
                if !self.add_resolved_import_dep(path, scope_imports) {
                    self.add_class_dep_from_name(path);
                }
            }
            ast::Import::Selective { path, names, .. } => {
                if !self.add_selective_import_deps(path, names, scope_imports) {
                    self.add_class_dep_from_name(path);
                }
            }
            ast::Import::Unqualified { path, .. } => self.add_class_dep_from_name(path),
        }
    }

    fn add_resolved_import_dep(
        &mut self,
        path: &ast::Name,
        scope_imports: Option<&[ast::scope::Import]>,
    ) -> bool {
        let Some(scope_imports) = scope_imports else {
            return false;
        };
        for import in scope_imports {
            match import {
                ast::scope::Import::Qualified {
                    path: import_path,
                    def_id,
                }
                | ast::scope::Import::Renamed {
                    path: import_path,
                    def_id,
                    ..
                } if import_path_matches(path, import_path) => {
                    self.add_class_dep_by_def_id(*def_id);
                    return true;
                }
                _ => {}
            }
        }
        false
    }

    fn add_selective_import_deps(
        &mut self,
        path: &ast::Name,
        names: &[rumoca_core::Token],
        scope_imports: Option<&[ast::scope::Import]>,
    ) -> bool {
        let Some(scope_imports) = scope_imports else {
            return false;
        };
        let mut found = false;
        for import in scope_imports {
            let ast::scope::Import::Unqualified {
                path: import_path,
                names: resolved_names,
            } = import
            else {
                continue;
            };
            if !import_path_matches(path, import_path) {
                continue;
            }
            for def_id in names.iter().filter_map(|name| {
                resolved_names
                    .get(&rumoca_core::ComponentPath::from_flat_path(
                        name.text.as_ref(),
                    ))
                    .copied()
            }) {
                self.add_class_dep_by_def_id(def_id);
                found = true;
            }
        }
        found
    }

    fn add_class_dep_from_name(&mut self, name: &ast::Name) {
        let Some(def_id) = name.def_id else {
            return;
        };
        self.add_class_dep_by_def_id(def_id);
        self.add_deferred_name_deps(name, def_id);
    }

    /// Follow the segments of a partially resolved composite name.
    ///
    /// `Name::def_id` holds the exact definition of the deepest segment Resolve
    /// could bind. A composite name crossing a replaceable class (for example
    /// `Rotation.Orientation`) stops at the slot, because the selected member
    /// only exists once redeclarations are applied (MLS §7.3). The strict
    /// closure must still retain those members, so the remaining segments are
    /// looked up through the slot's declared and substituted classes.
    fn add_deferred_name_deps(&mut self, name: &ast::Name, anchor_def_id: DefId) {
        let Some(anchor_local_name) = self.class_index.local_name(anchor_def_id) else {
            return;
        };
        let Some(anchor_index) = name
            .name
            .iter()
            .position(|segment| segment.text.as_ref() == anchor_local_name)
        else {
            return;
        };
        let mut owners = vec![anchor_def_id];
        for segment in name.name.iter().skip(anchor_index + 1) {
            owners = self.add_deferred_segment_deps(&owners, segment.text.as_ref());
            if owners.is_empty() {
                break;
            }
        }
    }

    /// Resolve one deferred segment against the owners reached so far and
    /// return the exact definitions it can select.
    fn add_deferred_segment_deps(&mut self, owners: &[DefId], segment: &str) -> Vec<DefId> {
        let member_def_ids = owners
            .iter()
            .flat_map(|owner_def_id| self.deferred_member_def_ids(*owner_def_id, segment))
            .collect::<IndexSet<_>>();
        for member_def_id in &member_def_ids {
            self.add_class_dep_by_def_id(*member_def_id);
        }
        member_def_ids.into_iter().collect()
    }

    /// Every definition named `segment` that a lookup through `owner_def_id`
    /// can select once redeclarations are applied.
    ///
    /// MLS §5.3 composite name lookup searches the class and its inherited
    /// elements; MLS §7.3 lets a replaceable class be replaced by its
    /// constraining class or by a redeclaration, and a redeclared element keeps
    /// the name of the element it replaces. The search therefore walks the
    /// declared class, its bases, its constraining class, the slot it
    /// redeclares, and the classes redeclared into it.
    fn deferred_member_def_ids(&self, owner_def_id: DefId, segment: &str) -> Vec<DefId> {
        let mut visited = IndexSet::new();
        let mut pending = vec![owner_def_id];
        let mut members = Vec::new();
        while let Some(scope_def_id) = pending.pop() {
            if !visited.insert(scope_def_id) {
                continue;
            }
            let Some(scope) = self.class_index.get(scope_def_id) else {
                continue;
            };
            if let Some(member_def_id) = scope.classes.get(segment).and_then(|class| class.def_id) {
                members.push(member_def_id);
            }
            if let Some(member_def_id) = scope
                .components
                .get(segment)
                .and_then(|component| component.def_id)
            {
                members.push(member_def_id);
            }
            pending.extend(scope.extends.iter().filter_map(|extend| extend.base_def_id));
            pending.extend(
                scope
                    .constrainedby
                    .as_ref()
                    .and_then(|constrainedby| constrainedby.def_id),
            );
            pending.extend(scope.redeclare_target_def_id);
            pending.extend(self.substitutions.substitutes(scope_def_id));
        }
        members
    }

    fn add_class_dep_by_def_id(&mut self, def_id: DefId) {
        for owner_def_id in self.class_index.def_ancestry(def_id).into_iter().rev() {
            if let Some(qualified_name) = self.class_index.qualified_name(owner_def_id) {
                self.deps.insert(qualified_name.to_string());
                return;
            }
        }
    }
}

fn import_path_matches(path: &ast::Name, import_path: &[String]) -> bool {
    path.name.len() == import_path.len()
        && path
            .name
            .iter()
            .zip(import_path)
            .all(|(token, import_part)| token.text.as_ref() == import_part)
}

impl Visitor for ClassDependencyCollector<'_, '_, '_> {
    fn visit_expr_function_call_ctx(
        &mut self,
        comp: &ast::ComponentReference,
        args: &[ast::Expression],
        ctx: FunctionCallContext,
    ) -> ControlFlow<()> {
        ast::visitor::walk_expr_function_call_ctx_default(self, comp, args, ctx)
    }

    fn visit_type_name(&mut self, name: &ast::Name, _ctx: TypeNameContext) -> ControlFlow<()> {
        self.add_class_dep_from_name(name);
        Continue(())
    }

    fn visit_component_reference(&mut self, cr: &ast::ComponentReference) -> ControlFlow<()> {
        // A resolved component reference carries two complementary semantic
        // identities. The root declaration anchors the written lookup path,
        // while the target declaration identifies the exact final member.
        // Strict pruning must preserve both owners: inherited members can have
        // a target owner outside the root's lexical subtree.
        if let Some(root_def_id) = cr.root_def_id() {
            self.add_class_dep_by_def_id(root_def_id);
        }
        if let Some(target_def_id) = cr.target_def_id() {
            self.add_class_dep_by_def_id(target_def_id);
        }
        // Segments Resolve left unbound stop at a replaceable class slot
        // (MLS §7.3); carry the reached owners forward so the members the
        // instantiated lookup selects stay inside the strict closure.
        let mut owners: Vec<DefId> = Vec::new();
        for part in &cr.parts {
            if let Some(part_def_id) = part.def_id {
                self.add_class_dep_by_def_id(part_def_id);
                owners = vec![part_def_id];
            } else if !owners.is_empty() {
                owners = self.add_deferred_segment_deps(&owners, part.ident.text.as_ref());
            }
            let Some(subscripts) = &part.subs else {
                continue;
            };
            for subscript in subscripts {
                self.visit_subscript_ctx(subscript, SubscriptContext::ComponentReferencePart)?;
            }
        }
        Continue(())
    }
}
