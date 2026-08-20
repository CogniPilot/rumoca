//! Rewrite context threaded through function override rewriting.

use super::*;

pub(crate) struct FunctionOverrideRewriteContext<'a> {
    pub(super) tree: &'a ClassTree,
    pub(super) class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
    pub(super) override_packages: &'a [OverrideTarget],
    pub(super) override_functions: &'a OverrideFunctionMap,
    pub(super) component_members: Option<&'a component_member_scope::ComponentMemberScopes>,
    pub(super) active_scope: ComponentPath,
    pub(super) local_def_ids: FxHashSet<rumoca_core::DefId>,
    pub(super) lexical_package_def_id: Option<rumoca_core::DefId>,
    predefined_callables: PredefinedCallableIds,
    package_chain_cache: std::cell::RefCell<
        rustc_hash::FxHashMap<rumoca_core::DefId, rustc_hash::FxHashSet<rumoca_core::DefId>>,
    >,
}

impl<'a> FunctionOverrideRewriteContext<'a> {
    pub(super) fn new(
        tree: &'a ClassTree,
        class_index: &'a rumoca_ir_ast::ClassDefIndex<'a>,
        override_packages: &'a [OverrideTarget],
        override_functions: &'a OverrideFunctionMap,
    ) -> Self {
        Self {
            tree,
            class_index,
            override_packages,
            override_functions,
            component_members: None,
            active_scope: ComponentPath::root(),
            local_def_ids: FxHashSet::default(),
            lexical_package_def_id: None,
            predefined_callables: PredefinedCallableIds::from_tree(tree),
            package_chain_cache: std::cell::RefCell::new(rustc_hash::FxHashMap::default()),
        }
    }

    /// True when the call target is one of the predefined operators Resolve
    /// registered as a scope member (MLS §3.7), matched on its exact `DefId`.
    pub(super) fn targets_predefined_callable(&self, def_id: rumoca_core::DefId) -> bool {
        self.predefined_callables.contains(def_id)
    }

    pub(super) fn with_active_scope(mut self, active_scope: ComponentPath) -> Self {
        self.active_scope = active_scope;
        self
    }

    pub(super) fn with_component_member_scope(
        mut self,
        component_members: &'a component_member_scope::ComponentMemberScopes,
    ) -> Self {
        self.component_members = Some(component_members);
        self
    }

    pub(super) fn with_local_def_ids(
        mut self,
        local_def_ids: FxHashSet<rumoca_core::DefId>,
    ) -> Self {
        self.local_def_ids = local_def_ids;
        self
    }

    pub(super) fn with_lexical_package_def_id(
        mut self,
        lexical_package_def_id: Option<rumoca_core::DefId>,
    ) -> Self {
        self.lexical_package_def_id = lexical_package_def_id;
        self
    }

    fn package_chain_contains_def_id(
        &self,
        package: &OverrideTarget,
        query_def_id: rumoca_core::DefId,
    ) -> bool {
        if !self
            .package_chain_cache
            .borrow()
            .contains_key(&package.def_id)
        {
            let mut chain = Vec::new();
            let mut visited = FxHashSet::default();
            collect_package_chain(
                self.tree,
                self.class_index,
                &package.name,
                &mut chain,
                &mut visited,
            );
            self.package_chain_cache
                .borrow_mut()
                .insert(package.def_id, chain.into_iter().collect());
        }
        self.package_chain_cache
            .borrow()
            .get(&package.def_id)
            .is_some_and(|chain| chain.contains(&query_def_id))
    }

    pub(super) fn active_override_package_for_source_package(
        &self,
        source_package_def_id: rumoca_core::DefId,
    ) -> Option<&'a OverrideTarget> {
        let mut matches = self.override_packages.iter().filter(|package| {
            package.active && self.package_chain_contains_def_id(package, source_package_def_id)
        });
        let package = matches.next()?;
        matches.next().is_none().then_some(package)
    }

    pub(super) fn concrete_override_package_for_source_package(
        &self,
        source_package_def_id: rumoca_core::DefId,
    ) -> Option<&'a OverrideTarget> {
        if let Some(package) =
            self.active_override_package_for_source_package(source_package_def_id)
        {
            return Some(package);
        }
        let mut matches = self
            .override_packages
            .iter()
            .filter(|package| self.package_chain_contains_def_id(package, source_package_def_id));
        let package = matches.next()?;
        matches.next().is_none().then_some(package)
    }

    pub(super) fn unique_active_override_package(&self) -> Option<&'a OverrideTarget> {
        let mut matches = self
            .override_packages
            .iter()
            .filter(|package| package.active);
        let package = matches.next()?;
        matches.next().is_none().then_some(package)
    }
}
