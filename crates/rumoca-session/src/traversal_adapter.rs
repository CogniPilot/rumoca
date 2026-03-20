use indexmap::IndexSet;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::{FunctionCallContext, SubscriptContext, TypeNameContext, Visitor};
use std::ops::ControlFlow::{self, Continue};

pub(crate) fn collect_class_dependencies(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    class_name: &str,
) -> IndexSet<String> {
    let mut collector = ClassDependencyCollector::new(tree, class_name);
    collector.collect_class(class);
    collector.finish()
}

struct ClassDependencyCollector<'a> {
    tree: &'a ast::ClassTree,
    class_name: &'a str,
    deps: IndexSet<String>,
}

impl<'a> ClassDependencyCollector<'a> {
    fn new(tree: &'a ast::ClassTree, class_name: &'a str) -> Self {
        Self {
            tree,
            class_name,
            deps: IndexSet::new(),
        }
    }

    fn finish(mut self) -> IndexSet<String> {
        self.deps.shift_remove(self.class_name);
        self.deps
    }

    fn collect_class(&mut self, class: &ast::ClassDef) {
        if let Some(constrainedby) = &class.constrainedby {
            let _ = self.visit_type_name(constrainedby, TypeNameContext::ClassConstrainedBy);
        }
        for extend in &class.extends {
            self.collect_extend(extend);
        }
        let scope_imports = class
            .scope_id
            .and_then(|scope_id| self.tree.scope_tree.get(scope_id))
            .map(|scope| scope.imports.as_slice());
        for import in &class.imports {
            self.collect_import(import, scope_imports);
        }
        for subscript in &class.array_subscripts {
            let _ = self.visit_subscript(subscript);
        }
        for annotation in &class.annotation {
            let _ = self.visit_expression(annotation);
        }

        for component in class.components.values() {
            self.collect_component(component);
        }

        for equation in &class.equations {
            let _ = self.visit_equation(equation);
        }
        for equation in &class.initial_equations {
            let _ = self.visit_equation(equation);
        }
        for algorithm in &class.algorithms {
            for statement in algorithm {
                let _ = self.visit_statement(statement);
            }
        }
        for algorithm in &class.initial_algorithms {
            for statement in algorithm {
                let _ = self.visit_statement(statement);
            }
        }

        if let Some(external) = &class.external {
            self.collect_external(external);
        }
    }

    fn collect_extend(&mut self, extend: &ast::Extend) {
        if let Some(base_def_id) = extend.base_def_id {
            self.add_class_dep_by_def_id(base_def_id);
        }
        let _ = self.visit_extend(extend);
        for annotation in &extend.annotation {
            let _ = self.visit_expression(annotation);
        }
    }

    fn collect_component(&mut self, component: &ast::Component) {
        if let Some(type_def_id) = component.type_def_id {
            self.add_class_dep_by_def_id(type_def_id);
        }
        let _ = self.visit_component(component);
        if let Some(binding) = &component.binding {
            let _ = self.visit_expression(binding);
        }
        for shape in &component.shape_expr {
            let _ = self.visit_subscript(shape);
        }
    }

    fn collect_external(&mut self, external: &ast::ExternalFunction) {
        if let Some(output) = &external.output {
            let _ = self.visit_component_reference(output);
        }
        for arg in &external.args {
            let _ = self.visit_expression(arg);
        }
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
        names: &[ast::Token],
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
            for def_id in names
                .iter()
                .filter_map(|name| resolved_names.get(name.text.as_ref()).copied())
            {
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
        let Some(qualified_name) = self.tree.def_map.get(&def_id) else {
            return;
        };
        if self.tree.get_class_by_def_id(def_id).is_some() {
            self.deps.insert(qualified_name.clone());
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

impl Visitor for ClassDependencyCollector<'_> {
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
        if let Some(def_id) = cr.def_id {
            self.add_class_dep_by_def_id(def_id);
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
