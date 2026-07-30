use indexmap::IndexSet;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::{FunctionCallContext, SubscriptContext, TypeNameContext, Visitor};
use std::ops::ControlFlow::{self, Continue};

pub(crate) fn collect_class_dependencies(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    class: &ast::ClassDef,
    class_name: &str,
) -> IndexSet<String> {
    let mut collector = ClassDependencyCollector::new(tree, class_index, class_name);
    assert!(
        !collector.collect_class(class).is_break(),
        "class dependency traversal stopped while collecting `{class_name}`"
    );
    collector.finish()
}

struct ClassDependencyCollector<'tree, 'index, 'name> {
    tree: &'tree ast::ClassTree,
    class_index: &'index ast::ClassDefIndex<'tree>,
    class_name: &'name str,
    deps: IndexSet<String>,
}

impl<'tree, 'index, 'name> ClassDependencyCollector<'tree, 'index, 'name> {
    fn new(
        tree: &'tree ast::ClassTree,
        class_index: &'index ast::ClassDefIndex<'tree>,
        class_name: &'name str,
    ) -> Self {
        Self {
            tree,
            class_index,
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
        if let Some(root_def_id) = cr.def_id {
            self.add_class_dep_by_def_id(root_def_id);
        }
        if let Some(target_def_id) = cr.target_def_id {
            self.add_class_dep_by_def_id(target_def_id);
        }
        for part in &cr.parts {
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
