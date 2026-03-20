use super::*;

struct PreparedTextDocumentChange {
    was_library_document: bool,
    document: Document,
    parse_error: Option<String>,
    invalidate_resolved: bool,
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
        let existing_live_library_source_roots = self
            .documents
            .get(uri)
            .filter(|doc| !doc.content.is_empty())
            .map(|_| self.library_source_root_keys_for_uri(uri))
            .unwrap_or_default();
        let prepared = self.prepare_text_document_change(uri, content);
        self.detach_uri_from_source_sets(
            uri,
            revision,
            prepared.was_library_document && !content.is_empty(),
        );
        self.insert_document(prepared.document, revision);
        if prepared.invalidate_resolved {
            self.invalidate_resolved_state(CacheInvalidationCause::DocumentMutation);
        } else {
            self.invalidate_strict_compile_state(CacheInvalidationCause::DocumentMutation);
        }
        if !existing_live_library_source_roots.is_empty() {
            self.mark_source_roots_for_refresh(&existing_live_library_source_roots);
        }
        prepared.parse_error
    }

    pub(crate) fn apply_document_removal_at_revision(&mut self, uri: &str, revision: RevisionId) {
        self.delete_document_entry(uri);
        self.record_file_revision(uri, revision);
        self.detach_uri_from_source_sets(uri, revision, false);
        self.restore_detached_source_root_document(uri, revision);
        self.invalidate_resolved_state(CacheInvalidationCause::DocumentRemoval);
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
            self.invalidate_library_completion_state_for_source_set(
                source_set_id,
                CacheInvalidationCause::SourceSetMutation,
            );
        } else {
            self.invalidate_library_completion_state(CacheInvalidationCause::SourceSetMutation);
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
            self.invalidate_library_completion_state_for_source_set(
                source_set_id,
                CacheInvalidationCause::SourceSetMutation,
            );
        } else {
            self.invalidate_library_completion_state(CacheInvalidationCause::SourceSetMutation);
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
        let was_library_document = self.is_library_backed_uri(uri);
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

        PreparedTextDocumentChange {
            was_library_document,
            document: Document::new(uri.to_string(), content.to_string(), syntax),
            parse_error,
            invalidate_resolved,
        }
    }

    #[cfg(test)]
    pub(crate) fn library_source_set_ids_for_uri(&self, uri: &str) -> Vec<SourceSetId> {
        self.library_source_root_keys_for_uri(uri)
            .into_iter()
            .filter_map(|source_set_key| {
                self.source_sets
                    .get(&source_set_key)
                    .map(|record| record.id)
            })
            .collect()
    }
}
