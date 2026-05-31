use super::class_body::ModifierClassTarget;
use super::declaration_index::ItemKind;
use super::session_impl_queries::{QueryClassLookup, QueryClassNavigationTarget};
use super::*;

pub(super) fn source_map_content_for_doc(doc: &Document) -> std::sync::Arc<str> {
    if !doc.content.is_empty() {
        return doc.content.clone();
    }
    std::fs::read_to_string(&doc.uri)
        .map(std::sync::Arc::<str>::from)
        .unwrap_or_else(|_| doc.content.clone())
}

pub(crate) fn class_name_matches_query_target(
    qualified_name: &str,
    class_name: &str,
    suffix: Option<&str>,
) -> bool {
    suffix.map_or(qualified_name == class_name, |suffix| {
        qualified_name == class_name
            || rumoca_core::top_level_path_ends_with(qualified_name, suffix)
    })
}
pub(crate) fn apply_break_exclusions(
    members: &mut IndexMap<String, String>,
    break_names: &[String],
) {
    for break_name in break_names {
        members.shift_remove(break_name);
    }
}
pub(crate) fn record_query_class_lookup_match(
    matched: &mut Option<QueryClassLookup>,
    uri: &str,
    qualified_name: String,
) -> Option<()> {
    if matched.is_some() {
        return None;
    }
    *matched = Some(QueryClassLookup {
        uri: uri.to_string(),
        qualified_name,
    });
    Some(())
}
pub(crate) struct NavigationReadContext<'a> {
    session: &'a Session,
}
impl<'a> NavigationReadContext<'a> {
    pub(crate) fn new(session: &'a Session) -> Self {
        Self { session }
    }

    fn class_lookup_query(&self, class_name: &str) -> Option<String> {
        let def_map = self
            .session
            .query_state
            .ast
            .package_def_map
            .session_cache
            .as_ref()
            .map(|entry| &entry.def_map)?;
        let suffix =
            (!rumoca_core::has_top_level_dot(class_name)).then(|| format!(".{class_name}"));
        let mut matched: Option<QueryClassLookup> = None;
        for (qualified_name, entry) in def_map.class_entries() {
            if !class_name_matches_query_target(qualified_name, class_name, suffix.as_deref()) {
                continue;
            }
            let uri = self
                .session
                .file_uris
                .get(&entry.item_key.file_id())
                .map(String::as_str)?;
            record_query_class_lookup_match(&mut matched, uri, qualified_name.clone())?;
        }
        matched.map(|target| target.qualified_name)
    }

    fn resolve_navigation_class_name(
        &self,
        uri: &str,
        enclosing_qualified_name: &str,
        raw_type_name: &str,
    ) -> Option<String> {
        for candidate in self.class_type_resolution_candidates_query(
            uri,
            enclosing_qualified_name,
            raw_type_name,
        ) {
            if let Some(qualified_name) = self.class_lookup_query(&candidate) {
                return Some(qualified_name);
            }
        }
        None
    }

    fn class_type_resolution_candidates_query(
        &self,
        uri: &str,
        qualified_name: &str,
        raw_name: &str,
    ) -> Vec<String> {
        self.class_interface_query(uri, qualified_name)
            .map(|class_interface| {
                class_interface.type_resolution_candidates(qualified_name, raw_name)
            })
            .unwrap_or_else(|| {
                if raw_name.is_empty() {
                    Vec::new()
                } else {
                    vec![raw_name.to_string()]
                }
            })
    }

    fn class_interface_query(&self, uri: &str, qualified_name: &str) -> Option<&ClassInterface> {
        let file_id = self.session.file_id_for_uri(uri)?;
        self.session
            .query_state
            .ast
            .class_interface_query_cache
            .get(&file_id)
            .and_then(|entry| entry.index.class_interface(qualified_name))
    }

    fn modifier_targets_for_class<'b>(
        &'b self,
        uri: &str,
        container_path: &str,
        class_name: &str,
    ) -> &'b [ModifierClassTarget] {
        let Some(file_id) = self.session.file_id_for_uri(uri) else {
            return &[];
        };
        let Some(entry) = self
            .session
            .query_state
            .ast
            .class_body_semantics_cache
            .get(&file_id)
        else {
            return &[];
        };
        let item_key = ItemKey::new(file_id, ItemKind::Class, container_path, class_name);
        entry.semantics.modifier_class_targets(&item_key)
    }
}

pub(crate) fn collect_navigation_class_reference_locations_in_definition(
    read: &NavigationReadContext<'_>,
    uri: &str,
    definition: &ast::StoredDefinition,
    target_qualified_name: &str,
    include_declaration: bool,
    locations: &mut Vec<(String, rumoca_core::Location)>,
) {
    let within_prefix = definition
        .within
        .as_ref()
        .map(ToString::to_string)
        .filter(|prefix| !prefix.is_empty())
        .unwrap_or_default();
    let mut collector = NavigationClassReferenceCollector {
        read,
        uri,
        target_qualified_name,
        include_declaration,
        locations,
    };
    for (name, class) in &definition.classes {
        collector.collect_class(&within_prefix, name, class);
    }
}

pub(crate) fn collect_navigation_class_rename_locations_in_definition(
    read: &NavigationReadContext<'_>,
    uri: &str,
    definition: &ast::StoredDefinition,
    target: &QueryClassNavigationTarget,
    locations: &mut Vec<(String, rumoca_core::Location)>,
) {
    let within_prefix = definition
        .within
        .as_ref()
        .map(ToString::to_string)
        .filter(|prefix| !prefix.is_empty())
        .unwrap_or_default();
    let mut collector = NavigationClassRenameCollector {
        read,
        uri,
        target,
        locations,
    };
    for (name, class) in &definition.classes {
        collector.collect_class(&within_prefix, name, class);
    }
}

struct NavigationClassReferenceCollector<'a> {
    read: &'a NavigationReadContext<'a>,
    uri: &'a str,
    target_qualified_name: &'a str,
    include_declaration: bool,
    locations: &'a mut Vec<(String, rumoca_core::Location)>,
}

impl NavigationClassReferenceCollector<'_> {
    fn collect_class(&mut self, container_path: &str, class_name: &str, class: &ast::ClassDef) {
        let qualified_name = navigation_join_qualified_name(container_path, class_name);
        if self.include_declaration && qualified_name == self.target_qualified_name {
            self.locations
                .push((self.uri.to_string(), class.name.location.clone()));
        }
        if let Some(constrainedby) = &class.constrainedby {
            push_navigation_type_reference_if_matches(
                self.read,
                self.uri,
                &qualified_name,
                constrainedby,
                self.target_qualified_name,
                self.locations,
            );
        }
        for import in &class.imports {
            push_navigation_import_references_if_matches(
                self.read,
                self.uri,
                import,
                self.target_qualified_name,
                self.locations,
            );
        }
        for extend in &class.extends {
            push_navigation_type_reference_if_matches(
                self.read,
                self.uri,
                &qualified_name,
                &extend.base_name,
                self.target_qualified_name,
                self.locations,
            );
        }
        for (nested_name, nested_class) in &class.classes {
            self.collect_class(&qualified_name, nested_name, nested_class);
        }
        for component in class.components.values() {
            push_navigation_type_reference_if_matches(
                self.read,
                self.uri,
                &qualified_name,
                &component.type_name,
                self.target_qualified_name,
                self.locations,
            );
            if let Some(constrainedby) = &component.constrainedby {
                push_navigation_type_reference_if_matches(
                    self.read,
                    self.uri,
                    &qualified_name,
                    constrainedby,
                    self.target_qualified_name,
                    self.locations,
                );
            }
        }
        push_navigation_body_modifier_references_if_matches(
            self.read,
            self.uri,
            &qualified_name,
            self.read
                .modifier_targets_for_class(self.uri, container_path, class_name),
            self.target_qualified_name,
            self.locations,
        );
    }
}

struct NavigationClassRenameCollector<'a> {
    read: &'a NavigationReadContext<'a>,
    uri: &'a str,
    target: &'a QueryClassNavigationTarget,
    locations: &'a mut Vec<(String, rumoca_core::Location)>,
}

impl NavigationClassRenameCollector<'_> {
    fn collect_class(&mut self, container_path: &str, class_name: &str, class: &ast::ClassDef) {
        let qualified_name = navigation_join_qualified_name(container_path, class_name);
        if qualified_name == self.target.qualified_name {
            self.locations
                .push((self.uri.to_string(), class.name.location.clone()));
            if let Some(end_name) = &class.end_name_token {
                self.locations
                    .push((self.uri.to_string(), end_name.location.clone()));
            }
        }
        if let Some(constrainedby) = &class.constrainedby {
            push_navigation_type_rename_location_if_matches(
                self.read,
                self.uri,
                &qualified_name,
                constrainedby,
                self.target,
                self.locations,
            );
        }
        for import in &class.imports {
            push_navigation_import_rename_locations_if_matches(
                self.read,
                self.uri,
                import,
                self.target,
                self.locations,
            );
        }
        for extend in &class.extends {
            push_navigation_type_rename_location_if_matches(
                self.read,
                self.uri,
                &qualified_name,
                &extend.base_name,
                self.target,
                self.locations,
            );
        }
        for (nested_name, nested_class) in &class.classes {
            self.collect_class(&qualified_name, nested_name, nested_class);
        }
        for component in class.components.values() {
            push_navigation_type_rename_location_if_matches(
                self.read,
                self.uri,
                &qualified_name,
                &component.type_name,
                self.target,
                self.locations,
            );
            if let Some(constrainedby) = &component.constrainedby {
                push_navigation_type_rename_location_if_matches(
                    self.read,
                    self.uri,
                    &qualified_name,
                    constrainedby,
                    self.target,
                    self.locations,
                );
            }
        }
        push_navigation_body_modifier_rename_locations_if_matches(
            self.read,
            self.uri,
            &qualified_name,
            self.read
                .modifier_targets_for_class(self.uri, container_path, class_name),
            self.target,
            self.locations,
        );
    }
}
fn push_navigation_body_modifier_references_if_matches(
    read: &NavigationReadContext<'_>,
    uri: &str,
    enclosing_qualified_name: &str,
    modifier_targets: &[ModifierClassTarget],
    target_qualified_name: &str,
    locations: &mut Vec<(String, rumoca_core::Location)>,
) {
    for modifier_target in modifier_targets {
        let Some(qualified_name) = read.resolve_navigation_class_name(
            uri,
            enclosing_qualified_name,
            modifier_target.raw_name(),
        ) else {
            continue;
        };
        if qualified_name == target_qualified_name {
            locations.push((uri.to_string(), modifier_target.location().clone()));
        }
    }
}

fn push_navigation_body_modifier_rename_locations_if_matches(
    read: &NavigationReadContext<'_>,
    uri: &str,
    enclosing_qualified_name: &str,
    modifier_targets: &[ModifierClassTarget],
    target: &QueryClassNavigationTarget,
    locations: &mut Vec<(String, rumoca_core::Location)>,
) {
    for modifier_target in modifier_targets {
        let Some(qualified_name) = read.resolve_navigation_class_name(
            uri,
            enclosing_qualified_name,
            modifier_target.raw_name(),
        ) else {
            continue;
        };
        if qualified_name == target.qualified_name
            && modifier_target.token_text() == target.token_text
        {
            locations.push((uri.to_string(), modifier_target.location().clone()));
        }
    }
}

fn push_navigation_import_references_if_matches(
    read: &NavigationReadContext<'_>,
    uri: &str,
    import: &ast::Import,
    target_qualified_name: &str,
    locations: &mut Vec<(String, rumoca_core::Location)>,
) {
    for token in navigation_import_reference_tokens(read, import, target_qualified_name) {
        locations.push((uri.to_string(), token.location.clone()));
    }
}

fn push_navigation_import_rename_locations_if_matches(
    read: &NavigationReadContext<'_>,
    uri: &str,
    import: &ast::Import,
    target: &QueryClassNavigationTarget,
    locations: &mut Vec<(String, rumoca_core::Location)>,
) {
    for token in navigation_import_reference_tokens(read, import, &target.qualified_name) {
        if token.text.as_ref() == target.token_text {
            locations.push((uri.to_string(), token.location.clone()));
        }
    }
}

fn push_navigation_type_reference_if_matches(
    read: &NavigationReadContext<'_>,
    uri: &str,
    enclosing_qualified_name: &str,
    type_name: &ast::Name,
    target_qualified_name: &str,
    locations: &mut Vec<(String, rumoca_core::Location)>,
) {
    let Some(qualified_name) =
        read.resolve_navigation_class_name(uri, enclosing_qualified_name, &type_name.to_string())
    else {
        return;
    };
    if qualified_name != target_qualified_name {
        return;
    }
    let Some(token) = type_name.name.last() else {
        return;
    };
    locations.push((uri.to_string(), token.location.clone()));
}

fn push_navigation_type_rename_location_if_matches(
    read: &NavigationReadContext<'_>,
    uri: &str,
    enclosing_qualified_name: &str,
    type_name: &ast::Name,
    target: &QueryClassNavigationTarget,
    locations: &mut Vec<(String, rumoca_core::Location)>,
) {
    let Some(qualified_name) =
        read.resolve_navigation_class_name(uri, enclosing_qualified_name, &type_name.to_string())
    else {
        return;
    };
    if qualified_name != target.qualified_name {
        return;
    }
    let Some(token) = type_name.name.last() else {
        return;
    };
    if token.text.as_ref() == target.token_text {
        locations.push((uri.to_string(), token.location.clone()));
    }
}

fn navigation_import_reference_tokens<'a>(
    read: &NavigationReadContext<'_>,
    import: &'a ast::Import,
    target_qualified_name: &str,
) -> Vec<&'a rumoca_core::Token> {
    match import {
        ast::Import::Qualified { path, .. } | ast::Import::Renamed { path, .. } => {
            let qualified_name = read.class_lookup_query(&path.to_string());
            match (qualified_name.as_deref(), path.name.last()) {
                (Some(found), Some(token)) if found == target_qualified_name => vec![token],
                _ => Vec::new(),
            }
        }
        ast::Import::Selective { path, names, .. } => names
            .iter()
            .filter(|token| {
                let candidate = format!("{path}.{}", token.text);
                read.class_lookup_query(&candidate).as_deref() == Some(target_qualified_name)
            })
            .collect(),
        ast::Import::Unqualified { .. } => Vec::new(),
    }
}

pub(crate) fn navigation_join_qualified_name(prefix: &str, name: &str) -> String {
    if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}.{name}")
    }
}

pub(crate) fn navigation_location_contains_position(
    location: &rumoca_core::Location,
    line: u32,
    character: u32,
) -> bool {
    let start_line = location.start_line.saturating_sub(1);
    let end_line = location.end_line.saturating_sub(1);
    if line < start_line || line > end_line {
        return false;
    }

    let start_character = if line == start_line {
        location.start_column.saturating_sub(1)
    } else {
        0
    };
    let end_character = if line == end_line {
        location.end_column
    } else {
        u32::MAX
    };

    character >= start_character && character <= end_character
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn query_class_match_requires_top_level_segment_boundary() {
        assert!(class_name_matches_query_target(
            "Pkg.Target",
            "Target",
            Some(".Target")
        ));
        assert!(class_name_matches_query_target(
            "Root.Pkg.Target",
            "Pkg.Target",
            Some(".Pkg.Target")
        ));
        assert!(!class_name_matches_query_target(
            "Pkg.MyTarget",
            "Target",
            Some(".Target")
        ));
        assert!(!class_name_matches_query_target(
            "Pkg.TargetAlias",
            "Target",
            Some(".Target")
        ));
        assert!(!class_name_matches_query_target(
            "Pkg[index.Target]",
            "Target",
            Some(".Target")
        ));
    }
}
