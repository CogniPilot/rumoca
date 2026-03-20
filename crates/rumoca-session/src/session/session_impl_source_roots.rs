use super::*;

impl Session {
    pub(super) fn source_set_uris(&self, source_set_key: &str) -> Option<&IndexSet<String>> {
        self.source_sets
            .get(source_set_key)
            .map(|record| &record.uris)
    }

    fn detached_source_root_keys_for_uri(&self, uri: &str) -> IndexSet<String> {
        self.file_id_for_uri(uri)
            .and_then(|file_id| self.detached_source_root_documents.get(&file_id))
            .map(|detached| detached.source_root_keys.clone())
            .unwrap_or_default()
    }

    fn source_set_key_for_id(&self, source_set_id: SourceSetId) -> Option<&str> {
        self.source_set_keys.get(&source_set_id).map(String::as_str)
    }

    fn source_set_ids_for_file_id(&self, file_id: FileId) -> IndexSet<SourceSetId> {
        self.file_source_sets
            .get(&file_id)
            .cloned()
            .unwrap_or_default()
    }

    fn remove_file_from_source_set(&mut self, source_set_id: SourceSetId, uri: &str) {
        let Some(file_id) = self.file_id_for_uri(uri) else {
            return;
        };
        let Some(source_sets) = self.file_source_sets.get_mut(&file_id) else {
            return;
        };
        source_sets.shift_remove(&source_set_id);
        if source_sets.is_empty() {
            self.file_source_sets.shift_remove(&file_id);
        }
    }

    pub(super) fn library_source_root_keys_for_uri(&self, uri: &str) -> IndexSet<String> {
        let mut keys = IndexSet::new();
        let Some(file_id) = self.file_id_for_uri(uri) else {
            return keys;
        };
        for source_set_id in self.source_set_ids_for_file_id(file_id) {
            let Some(source_set_key) = self.source_set_key_for_id(source_set_id) else {
                continue;
            };
            if self
                .source_sets
                .get(source_set_key)
                .is_some_and(|record| record.kind.is_library())
            {
                keys.insert(source_set_key.to_string());
            }
        }
        for source_set_key in self.detached_source_root_keys_for_uri(uri) {
            if self
                .source_sets
                .get(&source_set_key)
                .is_some_and(|record| record.kind.is_library())
            {
                keys.insert(source_set_key);
            }
        }
        keys
    }

    fn source_set_ids_for_uri(&self, uri: &str) -> Vec<SourceSetId> {
        self.file_id_for_uri(uri)
            .map(|file_id| {
                self.source_set_ids_for_file_id(file_id)
                    .into_iter()
                    .collect()
            })
            .unwrap_or_default()
    }

    pub(super) fn uri_is_in_source_set(&self, uri: &str) -> bool {
        !self.source_set_ids_for_uri(uri).is_empty()
    }

    pub(super) fn update_source_set_record(
        &mut self,
        source_set_key: &str,
        kind: SourceRootKind,
        uris: IndexSet<String>,
        revision: RevisionId,
    ) {
        let previous_record = self.source_sets.get(source_set_key).cloned();
        let source_set_id = previous_record
            .as_ref()
            .map(|record| record.id)
            .unwrap_or_else(|| {
                let id = SourceSetId::new(self.next_source_set_id);
                self.next_source_set_id = self.next_source_set_id.saturating_add(1);
                id
            });
        if let Some(previous) = previous_record.as_ref() {
            for uri in &previous.uris {
                self.remove_file_from_source_set(source_set_id, uri);
            }
        }
        let file_uris: Vec<_> = uris.iter().cloned().collect();
        self.source_set_keys
            .insert(source_set_id, source_set_key.to_string());
        self.source_sets.insert(
            source_set_key.to_string(),
            SourceSetRecord {
                id: source_set_id,
                kind,
                durability: kind.durability(),
                uris,
                revision,
                needs_refresh: false,
            },
        );
        for uri in file_uris {
            let file_id = self.record_file_revision(&uri, revision);
            self.file_source_sets
                .entry(file_id)
                .or_default()
                .insert(source_set_id);
            self.sync_detached_document_uri(&uri);
        }
    }

    pub(super) fn mark_source_roots_for_refresh(&mut self, source_root_keys: &IndexSet<String>) {
        for source_root_key in source_root_keys {
            if let Some(record) = self.source_sets.get_mut(source_root_key)
                && record.kind.is_library()
            {
                record.needs_refresh = true;
            }
        }
    }

    pub(super) fn clear_source_root_refresh(&mut self, source_set_key: &str) {
        if let Some(record) = self.source_sets.get_mut(source_set_key) {
            record.needs_refresh = false;
        }
    }

    /// Return loaded library source-root keys that still need a source refresh.
    pub fn dirty_library_source_root_keys(&self) -> Vec<String> {
        self.source_sets
            .iter()
            .filter(|(_, record)| record.kind.is_library() && record.needs_refresh)
            .map(|(key, _)| key.clone())
            .collect()
    }

    pub fn source_root_durability(&self, source_root_key: &str) -> Option<SourceRootDurability> {
        self.source_sets
            .get(source_root_key)
            .map(|record| record.durability)
    }

    /// Return whether a URI is currently owned by a loaded library source root.
    pub fn is_loaded_library_document(&self, uri: &str) -> bool {
        !self.library_source_root_keys_for_uri(uri).is_empty()
    }

    fn cache_detached_source_root_document(
        &mut self,
        uri: &str,
        document: Document,
        source_root_keys: IndexSet<String>,
    ) {
        let file_id = self.ensure_file_id(uri);
        let document = Arc::new(document);
        let entry = self
            .detached_source_root_documents
            .entry(file_id)
            .or_insert_with(|| DetachedSourceRootDocument {
                document: document.clone(),
                source_root_keys: IndexSet::new(),
            });
        entry.document = document;
        entry.source_root_keys.extend(source_root_keys);
    }

    pub(super) fn cache_detached_source_root_parsed_document(
        &mut self,
        source_root_key: &str,
        uri: &str,
        parsed: ast::StoredDefinition,
    ) {
        let mut source_root_keys = IndexSet::new();
        source_root_keys.insert(source_root_key.to_string());
        self.cache_detached_source_root_document(
            uri,
            Document::new(
                uri.to_string(),
                String::new(),
                crate::parse::SyntaxFile::from_parsed(parsed),
            ),
            source_root_keys,
        );
    }

    pub(super) fn drop_detached_source_root_membership(&mut self, source_root_key: &str) {
        self.detached_source_root_documents.retain(|_, detached| {
            detached.source_root_keys.shift_remove(source_root_key);
            !detached.source_root_keys.is_empty()
        });
    }

    pub(super) fn restore_detached_source_root_document(
        &mut self,
        uri: &str,
        revision: RevisionId,
    ) -> bool {
        let Some(file_id) = self.file_id_for_uri(uri) else {
            return false;
        };
        let Some(detached) = self.detached_source_root_documents.shift_remove(&file_id) else {
            return false;
        };

        for source_root_key in &detached.source_root_keys {
            let record_id = match self.source_sets.get_mut(source_root_key) {
                Some(record) => {
                    record.uris.insert(detached.document.uri.clone());
                    record.revision = revision;
                    record.id
                }
                None => continue,
            };
            self.file_source_sets
                .entry(file_id)
                .or_default()
                .insert(record_id);
        }
        self.record_file_revision(&detached.document.uri, revision);
        self.documents
            .insert(detached.document.uri.clone(), detached.document);
        self.sync_detached_document_uri(uri);
        true
    }

    fn source_root_attached_uri(&self, source_root_key: &str, file_id: FileId) -> Option<String> {
        let record = self.source_sets.get(source_root_key)?;
        if !record.kind.is_library() {
            return None;
        }
        record
            .uris
            .iter()
            .find(|candidate| {
                self.file_id_for_uri(candidate)
                    .is_some_and(|id| id == file_id)
            })
            .cloned()
    }

    fn remove_matches_from_source_root(
        &mut self,
        source_root_key: &str,
        matched_uri: &str,
        revision: RevisionId,
    ) {
        let record_id = {
            let Some(record) = self.source_sets.get_mut(source_root_key) else {
                return;
            };
            let record_id = record.id;
            record.uris.shift_remove(matched_uri);
            record.revision = revision;
            record_id
        };
        self.remove_file_from_source_set(record_id, matched_uri);
    }

    fn maybe_capture_detached_source_root_document(
        &self,
        preserve_backing_document: bool,
        source_root_key: &str,
        matched_uri: &str,
        detached_source_root_keys: &mut IndexSet<String>,
        detached_document: &mut Option<Document>,
    ) {
        if !preserve_backing_document {
            return;
        }
        detached_source_root_keys.insert(source_root_key.to_string());
        if detached_document.is_none() {
            *detached_document = self
                .documents
                .get(matched_uri)
                .filter(|doc| doc.content.is_empty())
                .map(|doc| doc.as_ref().clone());
        }
    }

    pub(super) fn detach_uri_from_source_sets(
        &mut self,
        uri: &str,
        revision: RevisionId,
        preserve_backing_document: bool,
    ) {
        let Some(file_id) = self.file_id_for_uri(uri) else {
            return;
        };
        let mut removable_docs = Vec::new();
        let mut touched_file_uris = Vec::new();
        let source_root_keys: Vec<String> = self
            .source_set_ids_for_file_id(file_id)
            .into_iter()
            .filter_map(|source_set_id| {
                self.source_set_key_for_id(source_set_id)
                    .map(ToString::to_string)
            })
            .collect();
        let mut detached_source_root_keys = IndexSet::new();
        let mut detached_document = None;
        for source_root_key in source_root_keys {
            let Some(matched_uri) = self.source_root_attached_uri(&source_root_key, file_id) else {
                continue;
            };
            self.maybe_capture_detached_source_root_document(
                preserve_backing_document,
                &source_root_key,
                &matched_uri,
                &mut detached_source_root_keys,
                &mut detached_document,
            );
            if matched_uri != uri {
                removable_docs.push(matched_uri.clone());
            }
            touched_file_uris.push(matched_uri.clone());
            self.remove_matches_from_source_root(&source_root_key, &matched_uri, revision);
        }

        if preserve_backing_document
            && let Some(document) = detached_document
            && !detached_source_root_keys.is_empty()
        {
            self.cache_detached_source_root_document(uri, document, detached_source_root_keys);
        }

        for doc_uri in removable_docs {
            let should_remove = self
                .documents
                .get(&doc_uri)
                .is_some_and(|doc| doc.content.is_empty());
            if should_remove {
                self.delete_document_entry(&doc_uri);
            }
        }
        for touched_uri in touched_file_uris {
            self.record_file_revision(&touched_uri, revision);
            self.sync_detached_document_uri(&touched_uri);
        }
    }
}
