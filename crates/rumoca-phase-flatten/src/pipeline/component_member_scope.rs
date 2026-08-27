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
        self.class_owner_components.clear();
        self.component_instance_references.clear();
        self.root_class_instance = None;
        for instance_data in overlay.components.values() {
            self.component_members
                .insert_component_member_path(&instance_data.qualified_name.to_component_path());
            if let Some(component_ref) = instance_data.component_ref.as_ref() {
                self.component_instance_references
                    .insert(instance_data.instance_id, component_ref.clone());
            }
        }
        for class_data in overlay.classes.values() {
            self.component_members
                .insert_component_member_path(&class_data.qualified_name.to_component_path());
            if let Some(owner_component) = class_data.owner_component_id {
                self.class_owner_components
                    .insert(class_data.instance_id, owner_component);
            } else {
                self.root_class_instance = Some(class_data.instance_id);
            }
        }
    }

    pub(crate) fn constant_owner_for_class(
        &self,
        class_instance: rumoca_core::InstanceId,
    ) -> Option<rumoca_core::InstanceId> {
        self.class_owner_components
            .get(&class_instance)
            .copied()
            .or_else(|| {
                (self.root_class_instance == Some(class_instance)).then_some(class_instance)
            })
    }

    pub(crate) fn has_component_member(&self, scope: &QualifiedName, name: &str) -> bool {
        self.component_members.has_member(scope, name)
    }

    /// Instance path of the component occurrence that owns `class_instance`.
    ///
    /// The root class occurrence has no containing component, so it answers
    /// `None`; every nested class occurrence answers the exact reference
    /// Instantiate proved for its owning component (`alg`, `bus.ctl[2]`, …),
    /// one part per enclosing component with its declaration identity.
    pub(crate) fn owner_instance_reference(
        &self,
        class_instance: rumoca_core::InstanceId,
    ) -> Option<&rumoca_core::ComponentReference> {
        let owner = self.class_owner_components.get(&class_instance)?;
        self.component_instance_references.get(owner)
    }

    /// Instance path that this class occurrence's algorithm write targets must
    /// carry, as the part chain to prepend.
    ///
    /// The root class occurrence owns no component and answers an empty path.
    /// A nested occurrence answers `owner_instance_reference`, because an
    /// algorithm section there writes members of that component occurrence and
    /// a Flat write target's identity is its part chain (see
    /// `algorithms::qualify_write_targets_with_owner_path`). A nested
    /// occurrence that has algorithm sections but no proven owner path cannot
    /// be flattened without inventing a coordinate, so it fails here.
    pub(crate) fn algorithm_owner_instance_path(
        &self,
        class_data: &ClassInstanceData,
    ) -> Result<Vec<rumoca_core::ComponentRefPart>, FlattenError> {
        if class_data.owner_component_id.is_none()
            || (class_data.algorithms.is_empty() && class_data.initial_algorithms.is_empty())
        {
            return Ok(Vec::new());
        }
        let owner = self
            .owner_instance_reference(class_data.instance_id)
            .ok_or_else(|| {
                let reason = format!(
                    "algorithm section of `{}` has no proven owning component instance path",
                    class_data.qualified_name.to_flat_string()
                );
                class_data
                    .algorithms
                    .iter()
                    .chain(&class_data.initial_algorithms)
                    .flatten()
                    .map(|statement| statement.span)
                    .find(|span| !span.is_dummy())
                    .map_or_else(
                        || FlattenError::missing_source_context(reason.clone()),
                        |span| FlattenError::missing_flat_variable_identity(reason.clone(), span),
                    )
            })?;
        Ok(owner.parts().to_vec())
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
