use super::*;

#[derive(Default)]
pub(crate) struct ComponentMemberScopes {
    members: IndexMap<rumoca_core::ComponentPath, indexmap::IndexSet<String>>,
}

impl ComponentMemberScopes {
    pub(crate) fn clear(&mut self) {
        self.members.clear();
    }

    pub(crate) fn insert_component_member_path(&mut self, path: &rumoca_core::ComponentPath) {
        let parts = path.parts();
        for child_index in 0..parts.len() {
            let parent = rumoca_core::ComponentPath::from_parts(parts[..child_index].iter());
            self.members
                .entry(parent)
                .or_default()
                .insert(parts[child_index].clone());
        }
    }

    pub(crate) fn contains_component_path(&self, path: &rumoca_core::ComponentPath) -> bool {
        let Some((member, parent_parts)) = path.parts().split_last() else {
            return false;
        };
        let parent = rumoca_core::ComponentPath::from_parts(parent_parts.iter());
        self.members
            .get(&parent)
            .is_some_and(|members| members.contains(member))
    }

    pub(crate) fn has_member(&self, scope: &QualifiedName, name: &str) -> bool {
        self.members
            .get(&scope.to_component_path())
            .is_some_and(|members| members.contains(name))
    }

    pub(crate) fn scoped_component_imports(
        &self,
        expr: &ast::Expression,
        scope: &QualifiedName,
        imports: &qualify::ImportMap,
    ) -> qualify::ImportMap {
        let mut scoped_imports = imports.clone();
        let mut roots = indexmap::IndexSet::new();
        collect_expression_component_roots(expr, &mut roots);
        for root in roots {
            if scoped_imports.contains_key(root.as_str()) || self.has_member(scope, &root) {
                continue;
            }
            if let Some(parent_name) = self.nearest_parent_name_with_member(scope, &root) {
                let target = if parent_name.parts.is_empty() {
                    root.clone()
                } else {
                    parent_name.child(&root).to_flat_string()
                };
                scoped_imports.insert(root, target);
            }
        }
        scoped_imports
    }

    fn nearest_parent_name_with_member(
        &self,
        scope: &QualifiedName,
        root: &str,
    ) -> Option<QualifiedName> {
        let mut candidate = parent_qualified_name(scope);
        loop {
            if self.has_member(&candidate, root) {
                return Some(candidate);
            }
            if candidate.parts.is_empty() {
                return None;
            }
            candidate = parent_qualified_name(&candidate);
        }
    }
}

fn parent_qualified_name(scope: &QualifiedName) -> QualifiedName {
    if scope.parts.len() <= 1 {
        QualifiedName::new()
    } else {
        QualifiedName {
            parts: scope.parts[..scope.parts.len() - 1].to_vec(),
        }
    }
}

fn collect_expression_component_roots(
    expr: &ast::Expression,
    roots: &mut indexmap::IndexSet<String>,
) {
    use rumoca_ir_ast::visitor::Visitor;
    use std::ops::ControlFlow;

    struct RootCollector<'a> {
        roots: &'a mut indexmap::IndexSet<String>,
    }

    impl Visitor for RootCollector<'_> {
        fn visit_component_reference_ctx(
            &mut self,
            cr: &ast::ComponentReference,
            component_ctx: ast::ComponentReferenceContext,
        ) -> ControlFlow<()> {
            if matches!(component_ctx, ast::ComponentReferenceContext::Expression)
                && let Some(first) = cr.parts.first()
            {
                self.roots.insert(first.ident.text.to_string());
            }
            ast::walk_component_reference_default(self, cr)
        }
    }

    let mut collector = RootCollector { roots };
    let _ = collector.visit_expression(expr);
}

impl Context {
    pub(crate) fn seed_component_member_scopes(&mut self, overlay: &InstanceOverlay) {
        self.component_members.clear();
        for instance_data in overlay.components.values() {
            self.component_members
                .insert_component_member_path(&instance_data.qualified_name.to_component_path());
        }
        for class_data in overlay.classes.values() {
            self.component_members
                .insert_component_member_path(&class_data.qualified_name.to_component_path());
        }
    }

    pub(crate) fn has_component_member(&self, scope: &QualifiedName, name: &str) -> bool {
        self.component_members.has_member(scope, name)
    }
}

pub(super) fn imports_without_instance_member_aliases(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    ctx: &Context,
) -> qualify::ImportMap {
    let mut shadowed = indexmap::IndexSet::new();
    collect_instance_member_shadowed_import_aliases(expr, prefix, imports, ctx, &mut shadowed);
    let unshadowed = imports
        .iter()
        .filter(|(alias, _)| !shadowed.contains(alias.as_str()))
        .map(|(alias, target)| (alias.clone(), target.clone()))
        .collect();
    ctx.component_members
        .scoped_component_imports(expr, prefix, &unshadowed)
}

fn collect_instance_member_shadowed_import_aliases(
    expr: &ast::Expression,
    prefix: &QualifiedName,
    imports: &qualify::ImportMap,
    ctx: &Context,
    shadowed: &mut indexmap::IndexSet<String>,
) {
    use rumoca_ir_ast::visitor::Visitor;
    use std::ops::ControlFlow;

    struct InstanceMemberImportShadowCollector<'a> {
        prefix: &'a QualifiedName,
        imports: &'a qualify::ImportMap,
        ctx: &'a Context,
        shadowed: &'a mut indexmap::IndexSet<String>,
    }

    impl Visitor for InstanceMemberImportShadowCollector<'_> {
        fn visit_component_reference_ctx(
            &mut self,
            cr: &ast::ComponentReference,
            component_ctx: ast::ComponentReferenceContext,
        ) -> ControlFlow<()> {
            if matches!(component_ctx, ast::ComponentReferenceContext::Expression) {
                self.collect_component_reference(cr);
            }
            ast::walk_component_reference_default(self, cr)
        }
    }

    impl InstanceMemberImportShadowCollector<'_> {
        fn collect_component_reference(&mut self, cr: &ast::ComponentReference) {
            let Some(first) = cr.parts.first() else {
                return;
            };
            let alias = first.ident.text.as_ref();
            if self.imports.contains_key(alias) && self.ctx.has_component_member(self.prefix, alias)
            {
                self.shadowed.insert(alias.to_string());
            }
        }
    }

    let mut collector = InstanceMemberImportShadowCollector {
        prefix,
        imports,
        ctx,
        shadowed,
    };
    let _ = collector.visit_expression(expr);
}
