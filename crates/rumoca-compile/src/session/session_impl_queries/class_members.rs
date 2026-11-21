//! Parsed-source member queries shared by editor features.

use super::super::session_impl::apply_break_exclusions;
use super::*;
use std::borrow::Cow;

impl Session {
    /// Resolve one component member type and its declaring class from the query layer.
    pub fn class_component_member_info_query(
        &mut self,
        class_name: &str,
        component_name: &str,
    ) -> Option<(String, String)> {
        let target = self.lookup_query_class_target(class_name)?;
        let mut visiting = std::collections::HashSet::<String>::new();
        self.class_component_member_info_in_class(
            &target.uri,
            &target.qualified_name,
            component_name,
            &mut visiting,
            &mut ClassMemberDependencies::default(),
        )
    }

    /// Collect component members from the parsed query layer.
    ///
    /// This follows local components and `extends` edges, applying `break`
    /// exclusions after each base expansion. If the query layer cannot resolve
    /// the class uniquely, returns an empty vector and callers may fall back to
    /// resolved-tree caches.
    pub fn class_component_members_query(&mut self, class_name: &str) -> Vec<(String, String)> {
        let Some(target) = self.lookup_query_class_target(class_name) else {
            return Vec::new();
        };
        if let Some(members) = self.cached_query_class_component_members(&target.qualified_name) {
            return members;
        }

        let mut members = IndexMap::<String, String>::new();
        let mut visiting = std::collections::HashSet::<String>::new();
        let mut dependencies = ClassMemberDependencies::default();
        self.collect_query_class_component_members(
            &target.uri,
            &target.qualified_name,
            &mut members,
            &mut visiting,
            &mut dependencies,
        );

        let members = members.into_iter().collect::<Vec<_>>();
        self.insert_query_class_component_members(
            target.qualified_name,
            dependencies,
            members.clone(),
        );
        members
    }

    fn cached_query_class_component_members(
        &mut self,
        class_name: &str,
    ) -> Option<Vec<(String, String)>> {
        let entry = self
            .query_state
            .ast
            .class_component_members_query_cache
            .shift_remove(class_name)?;
        if !self.member_dependencies_are_current(&entry.dependencies) {
            return None;
        }

        let members = entry.members.clone();
        self.query_state
            .ast
            .class_component_members_query_cache
            .insert(class_name.to_string(), entry);
        Some(members)
    }

    fn insert_query_class_component_members(
        &mut self,
        class_name: String,
        dependencies: ClassMemberDependencies,
        members: Vec<(String, String)>,
    ) {
        self.query_state
            .ast
            .class_component_members_query_cache
            .shift_remove(&class_name);
        self.query_state
            .ast
            .class_component_members_query_cache
            .insert(
                class_name,
                ClassComponentMembersQueryCache {
                    dependencies,
                    members,
                },
            );
        Self::trim_lru_cache(
            &mut self.query_state.ast.class_component_members_query_cache,
            MAX_SESSION_CLASS_MEMBER_QUERY_CACHE_ENTRIES,
        );
    }

    fn class_component_member_info_in_class(
        &mut self,
        uri: &str,
        qualified_name: &str,
        component_name: &str,
        visiting: &mut std::collections::HashSet<String>,
        dependencies: &mut ClassMemberDependencies,
    ) -> Option<(String, String)> {
        if !visiting.insert(qualified_name.to_string()) {
            return None;
        }

        let Some(class_interface) = self.class_interface_query(uri, qualified_name) else {
            visiting.remove(qualified_name);
            return None;
        };
        let mut inherited = None;

        for ext in class_interface.extends() {
            if let Some(base) = self.query_member_base(
                qualified_name,
                &class_interface,
                ext.base_name(),
                dependencies,
            ) && let Some(info) = self.class_component_member_info_in_class(
                &base.uri,
                &base.qualified_name,
                component_name,
                visiting,
                dependencies,
            ) {
                inherited = Some(info);
            }
            if ext.break_names().iter().any(|name| name == component_name) {
                inherited = None;
            }
        }

        let local = class_interface
            .component_type(component_name)
            .map(|member_type| (qualified_name.to_string(), member_type.to_string()));
        visiting.remove(qualified_name);
        local.or(inherited)
    }

    fn collect_query_class_component_members(
        &mut self,
        uri: &str,
        qualified_name: &str,
        members: &mut IndexMap<String, String>,
        visiting: &mut std::collections::HashSet<String>,
        dependencies: &mut ClassMemberDependencies,
    ) {
        if !visiting.insert(qualified_name.to_string()) {
            return;
        }

        self.record_query_dependency_signature(uri, &mut dependencies.summaries);

        let Some(class_interface) = self.class_interface_query(uri, qualified_name) else {
            visiting.remove(qualified_name);
            return;
        };
        for extend in class_interface.extends() {
            if let Some(base_target) = self.query_member_base(
                qualified_name,
                &class_interface,
                extend.base_name(),
                dependencies,
            ) {
                self.collect_query_class_component_members(
                    &base_target.uri,
                    &base_target.qualified_name,
                    members,
                    visiting,
                    dependencies,
                );
                apply_break_exclusions(members, extend.break_names());
            }
        }

        for (name, component_interface) in class_interface.component_interfaces() {
            members.insert(name.clone(), component_interface.type_name().to_string());
        }

        visiting.remove(qualified_name);
    }

    fn record_query_dependency_signature(&self, uri: &str, signature: &mut SummarySignature) {
        if let Some(file_id) = self.file_id_for_uri(uri)
            && let Some(fingerprint) = self.file_summary_fingerprint(uri)
        {
            signature.insert(file_id, fingerprint);
        }
    }

    fn member_dependencies_are_current(&mut self, dependencies: &ClassMemberDependencies) -> bool {
        if !dependencies.summaries.iter().all(|(file_id, fingerprint)| {
            let Some(uri) = self.file_uris.get(file_id) else {
                return false;
            };
            self.file_summary_fingerprint(uri) == Some(*fingerprint)
        }) {
            return false;
        }
        let Some(map) = self.query_state.ast.package_def_map.session_cache.as_ref() else {
            return false;
        };
        dependencies.declarations.iter().all(|(class, expected)| {
            map.def_map
                .declared_class(class)
                .map(|entry| entry.item_key.file_id())
                == *expected
        })
    }

    /// Find a base for the parsed-source member view. Candidate class names
    /// come from the lexical scopes and imports in the source documents.
    fn query_member_base(
        &mut self,
        mut scope: &str,
        interface: &ClassInterface,
        base: &str,
        dependencies: &mut ClassMemberDependencies,
    ) -> Option<QueryClassLookup> {
        let derived = scope;
        let mut interface = Some(Cow::Borrowed(interface));
        loop {
            let candidate = format!("{scope}.{base}");
            if candidate != derived
                && let Some(target) = self.member_declaration(&candidate, dependencies)
            {
                return Some(target);
            }
            let mut imports = interface
                .iter()
                .flat_map(|current| current.import_map().resolve_candidates(base));
            if let Some(target) =
                imports.find_map(|name| self.member_declaration(&name, dependencies))
            {
                return Some(target);
            }
            if interface.is_some_and(|current| current.is_encapsulated()) {
                return None;
            }
            let Some(parent) = rumoca_core::parent_scope(scope) else {
                return self.member_declaration(base, dependencies);
            };
            scope = parent;
            interface = self
                .member_declaration(scope, dependencies)
                .and_then(|target| {
                    self.record_query_dependency_signature(
                        &target.uri,
                        &mut dependencies.summaries,
                    );
                    self.class_interface_query(&target.uri, scope)
                        .map(Cow::Owned)
                });
        }
    }

    fn member_declaration(
        &mut self,
        candidate: &str,
        dependencies: &mut ClassMemberDependencies,
    ) -> Option<QueryClassLookup> {
        // The outer class lookup has already prepared the map for this revision.
        let file_id = self
            .query_state
            .ast
            .package_def_map
            .session_cache
            .as_ref()?
            .def_map
            .declared_class(candidate)
            .map(|entry| entry.item_key.file_id());
        dependencies
            .declarations
            .insert(candidate.to_owned(), file_id);
        let uri = self.file_uris.get(&file_id?)?;
        Some(QueryClassLookup {
            uri: uri.clone(),
            qualified_name: candidate.to_owned(),
        })
    }
}
