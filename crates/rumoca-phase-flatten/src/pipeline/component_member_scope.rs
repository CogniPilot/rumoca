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
    if shadowed.is_empty() {
        return imports.clone();
    }

    imports
        .iter()
        .filter(|(alias, _)| !shadowed.contains(alias.as_str()))
        .map(|(alias, target)| (alias.clone(), target.clone()))
        .collect()
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
