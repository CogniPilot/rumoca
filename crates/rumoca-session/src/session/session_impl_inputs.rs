use super::*;

struct PreparedTextDocumentChange {
    was_source_root_backed_document: bool,
    document: Document,
    parse_error: Option<String>,
    invalidate_resolved: bool,
    source_root_edit_invalidation: SourceRootEditInvalidation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SourceRootEditInvalidation {
    None,
    BodyOnly,
    InterfaceChange {
        dirty_class_prefixes: IndexSet<String>,
    },
    MembershipChange {
        dirty_class_prefixes: IndexSet<String>,
    },
}

impl SourceRootEditInvalidation {
    fn dirty_class_prefixes(&self) -> Option<&IndexSet<String>> {
        match self {
            Self::InterfaceChange {
                dirty_class_prefixes,
            }
            | Self::MembershipChange {
                dirty_class_prefixes,
            } => Some(dirty_class_prefixes),
            Self::None | Self::BodyOnly => None,
        }
    }
}

impl Session {
    /// Apply a transactional input change to the session.
    pub fn apply_change(&mut self, change: SessionChange) {
        if change.is_empty() || !self.session_change_has_effect(&change) {
            return;
        }

        let revision = self.bump_revision();
        for change in change.source_root_changes {
            match change {
                SourceRootInputChange::Replace { key, kind, uris } => {
                    self.apply_source_root_change_at_revision(&key, kind, uris, revision);
                }
                SourceRootInputChange::Remove { key } => {
                    self.remove_source_root_at_revision(&key, revision);
                }
            }
        }
        for change in change.file_changes {
            match change {
                FileInputChange::SetText { uri, text } => {
                    self.apply_text_document_change_at_revision(&uri, &text, revision);
                }
                FileInputChange::Remove { uri } => {
                    self.apply_document_removal_at_revision(&uri, revision);
                }
            }
        }
    }

    pub(crate) fn apply_text_document_change_at_revision(
        &mut self,
        uri: &str,
        content: &str,
        revision: RevisionId,
    ) -> Option<String> {
        let source_root_backing_keys_before_detach = self.source_root_backing_keys_for_uri(uri);
        let existing_live_source_root_backing_keys = self
            .documents
            .get(uri)
            .filter(|doc| !doc.content.is_empty())
            .map(|_| self.source_root_backing_keys_for_uri(uri))
            .unwrap_or_default();
        let prepared = self.prepare_text_document_change(uri, content);
        self.detach_uri_from_source_sets(
            uri,
            revision,
            prepared.was_source_root_backed_document && !content.is_empty(),
        );
        self.insert_document(prepared.document, revision);
        self.cache_detached_live_source_root_membership(
            uri,
            &source_root_backing_keys_before_detach,
        );
        if prepared.invalidate_resolved {
            self.invalidate_resolved_state(CacheInvalidationCause::DocumentMutation);
        } else {
            self.invalidate_strict_compile_state(CacheInvalidationCause::DocumentMutation);
        }
        for source_root_key in &existing_live_source_root_backing_keys {
            if let Some(source_set_id) = self.source_set_id(source_root_key) {
                self.invalidate_source_root_resolved_aggregate_for_source_set(source_set_id);
            }
        }
        if !existing_live_source_root_backing_keys.is_empty()
            && let Some(dirty_class_prefixes) = prepared
                .source_root_edit_invalidation
                .dirty_class_prefixes()
        {
            self.mark_source_roots_for_refresh(
                &existing_live_source_root_backing_keys,
                dirty_class_prefixes,
            );
        }
        prepared.parse_error
    }

    fn cache_detached_live_source_root_membership(
        &mut self,
        uri: &str,
        source_root_backing_keys_before_detach: &IndexSet<String>,
    ) {
        if source_root_backing_keys_before_detach.is_empty()
            || self.uri_is_in_source_set(uri)
            || !self.detached_source_root_keys_for_uri(uri).is_empty()
        {
            return;
        }
        let Some(parsed) = self
            .documents
            .get(uri)
            .and_then(|document| document.parsed().cloned())
        else {
            return;
        };
        self.cache_detached_source_root_document(
            uri,
            Document::from_parsed(uri.to_string(), String::new(), parsed),
            source_root_backing_keys_before_detach.clone(),
        );
    }

    pub(crate) fn apply_document_removal_at_revision(&mut self, uri: &str, revision: RevisionId) {
        let existing_live_source_root_backing_keys = self.source_root_backing_keys_for_uri(uri);
        self.delete_document_entry(uri);
        self.record_file_revision(uri, revision);
        self.detach_uri_from_source_sets(uri, revision, false);
        self.restore_detached_source_root_document(uri, revision);
        self.invalidate_resolved_state(CacheInvalidationCause::DocumentRemoval);
        for source_root_key in &existing_live_source_root_backing_keys {
            if let Some(source_set_id) = self.source_set_id(source_root_key) {
                self.invalidate_source_root_resolved_aggregate_for_source_set(source_set_id);
            }
        }
    }

    pub(crate) fn apply_source_root_change_at_revision(
        &mut self,
        source_root_key: &str,
        kind: SourceRootKind,
        uris: IndexSet<String>,
        revision: RevisionId,
    ) {
        self.update_source_set_record(source_root_key, kind, uris, revision);
        self.invalidate_resolved_state(CacheInvalidationCause::SourceSetMutation);
        if let Some(source_set_id) = self.source_set_id(source_root_key) {
            self.invalidate_source_root_resolved_aggregate_for_source_set(source_set_id);
            self.invalidate_source_root_completion_state_for_source_set(
                source_set_id,
                CacheInvalidationCause::SourceSetMutation,
            );
        } else {
            self.invalidate_source_root_completion_state(CacheInvalidationCause::SourceSetMutation);
        }
    }

    pub(crate) fn remove_source_root_at_revision(
        &mut self,
        source_root_key: &str,
        revision: RevisionId,
    ) {
        let previous = self
            .source_set_uris(source_root_key)
            .cloned()
            .unwrap_or_default();
        let source_set_cache_id = self.source_set_id(source_root_key);
        if previous.is_empty() {
            self.drop_detached_source_root_membership(source_root_key);
            return;
        }

        let removable: Vec<String> = previous
            .iter()
            .filter(|uri| {
                self.documents
                    .get(*uri)
                    .is_some_and(|doc| doc.content.is_empty())
            })
            .cloned()
            .collect();
        for uri in removable {
            self.delete_document_entry(&uri);
        }
        for uri in previous.iter() {
            self.record_file_revision(uri, revision);
        }
        let kind = self
            .source_sets
            .get(source_root_key)
            .map(|record| record.kind)
            .unwrap_or_default();
        self.update_source_set_record(source_root_key, kind, IndexSet::new(), revision);
        self.drop_detached_source_root_membership(source_root_key);
        self.invalidate_resolved_state(CacheInvalidationCause::SourceSetMutation);
        if let Some(source_set_id) = source_set_cache_id {
            self.invalidate_source_root_resolved_aggregate_for_source_set(source_set_id);
            self.invalidate_source_root_completion_state_for_source_set(
                source_set_id,
                CacheInvalidationCause::SourceSetMutation,
            );
        } else {
            self.invalidate_source_root_completion_state(CacheInvalidationCause::SourceSetMutation);
        }
    }

    fn session_change_has_effect(&self, change: &SessionChange) -> bool {
        change
            .source_root_changes
            .iter()
            .any(|change| self.source_root_change_has_effect(change))
            || change
                .file_changes
                .iter()
                .any(|change| self.file_input_change_has_effect(change))
    }

    fn source_root_change_has_effect(&self, change: &SourceRootInputChange) -> bool {
        match change {
            SourceRootInputChange::Replace { key, kind, uris } => self
                .source_sets
                .get(key)
                .is_none_or(|record| record.kind != *kind || record.uris != *uris),
            SourceRootInputChange::Remove { key } => self
                .source_sets
                .get(key)
                .is_some_and(|record| !record.uris.is_empty()),
        }
    }

    fn file_input_change_has_effect(&self, change: &FileInputChange) -> bool {
        match change {
            FileInputChange::SetText { uri, text } => self
                .documents
                .get(uri)
                .is_none_or(|doc| doc.content != *text),
            FileInputChange::Remove { uri } => {
                self.documents.contains_key(uri) || self.uri_is_in_source_set(uri)
            }
        }
    }

    fn prepare_text_document_change(&self, uri: &str, content: &str) -> PreparedTextDocumentChange {
        let was_source_root_backed_document = self.is_source_root_backed_uri(uri);
        let previous_parsed = self
            .documents
            .get(uri)
            .and_then(|doc| doc.parsed().cloned());
        record_document_parse();
        let parse_started = maybe_start_timer();
        let syntax = crate::parse::parse_source_to_syntax(content, uri)
            .with_fallback_parsed(previous_parsed.clone());
        if let Some(elapsed) = maybe_elapsed_duration(parse_started) {
            record_document_parse_duration(elapsed);
        }
        if syntax.has_errors() {
            record_document_parse_error();
        }
        let invalidate_resolved = if syntax.has_errors() {
            previous_parsed.is_none()
        } else {
            true
        };
        let parse_error = syntax.parse_error().map(ToString::to_string);
        let document = Document::new(uri.to_string(), content.to_string(), syntax);
        let source_root_edit_invalidation = self.classify_source_root_edit_invalidation(
            uri,
            &document,
            was_source_root_backed_document,
        );

        PreparedTextDocumentChange {
            was_source_root_backed_document,
            document,
            parse_error,
            invalidate_resolved,
            source_root_edit_invalidation,
        }
    }

    fn classify_source_root_edit_invalidation(
        &self,
        uri: &str,
        document: &Document,
        was_source_root_backed_document: bool,
    ) -> SourceRootEditInvalidation {
        if !was_source_root_backed_document {
            return SourceRootEditInvalidation::None;
        }
        let Some(previous) = self.documents.get(uri) else {
            return SourceRootEditInvalidation::None;
        };
        if previous.content.is_empty() {
            return SourceRootEditInvalidation::None;
        }
        if previous.summary_fingerprint() == document.summary_fingerprint() {
            return SourceRootEditInvalidation::BodyOnly;
        }

        let previous_summary =
            FileSummary::from_definition(FileId::default(), previous.best_effort());
        let updated_summary =
            FileSummary::from_definition(FileId::default(), document.best_effort());
        let dirty_class_prefixes =
            dirty_class_prefixes_for_summary_change(&previous_summary, &updated_summary);
        if summary_membership_changed(&previous_summary, &updated_summary) {
            SourceRootEditInvalidation::MembershipChange {
                dirty_class_prefixes,
            }
        } else {
            SourceRootEditInvalidation::InterfaceChange {
                dirty_class_prefixes,
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn non_workspace_source_set_ids_for_uri(&self, uri: &str) -> Vec<SourceSetId> {
        self.non_workspace_source_root_keys_for_uri(uri)
            .into_iter()
            .filter_map(|source_set_key| {
                self.source_sets
                    .get(&source_set_key)
                    .map(|record| record.id)
            })
            .collect()
    }
}

fn summary_membership_changed(previous: &FileSummary, updated: &FileSummary) -> bool {
    previous.within_path != updated.within_path
        || previous
            .class_keys_by_name
            .keys()
            .ne(updated.class_keys_by_name.keys())
}

fn dirty_class_prefixes_for_summary_change(
    previous: &FileSummary,
    updated: &FileSummary,
) -> IndexSet<String> {
    let mut dirty_class_names = IndexSet::new();
    for qualified_name in previous.class_keys_by_name.keys() {
        let previous_class = previous
            .class_keys_by_name
            .get(qualified_name)
            .and_then(|item_key| previous.classes.get(item_key));
        let updated_class = updated
            .class_keys_by_name
            .get(qualified_name)
            .and_then(|item_key| updated.classes.get(item_key));
        if updated_class != previous_class {
            dirty_class_names.insert(qualified_name.clone());
        }
    }
    for qualified_name in updated.class_keys_by_name.keys() {
        if !previous.class_keys_by_name.contains_key(qualified_name) {
            dirty_class_names.insert(qualified_name.clone());
        }
    }

    let mut dirty_class_prefixes = IndexSet::new();
    for qualified_name in dirty_class_names {
        accumulate_qualified_name_ancestors(&mut dirty_class_prefixes, &qualified_name);
    }
    dirty_class_prefixes
}

fn accumulate_qualified_name_ancestors(
    dirty_class_prefixes: &mut IndexSet<String>,
    qualified_name: &str,
) {
    let mut current = qualified_name.trim();
    while !current.is_empty() {
        dirty_class_prefixes.insert(current.to_string());
        let Some((parent, _)) = current.rsplit_once('.') else {
            break;
        };
        current = parent;
    }
}
