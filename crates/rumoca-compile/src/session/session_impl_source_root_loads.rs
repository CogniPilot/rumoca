use super::*;

impl Session {
    /// Replace a parsed source-set in one operation.
    ///
    /// Existing parsed docs in this source-set are removed first. Documents with
    /// non-empty content (workspace/open docs) are preserved.
    /// Returns the number of parsed documents inserted.
    pub fn replace_parsed_source_set(
        &mut self,
        source_set_id: &str,
        kind: SourceRootKind,
        definitions: Vec<(String, ast::StoredDefinition)>,
        exclude_uri: Option<&str>,
    ) -> usize {
        let mut desired_docs: IndexMap<String, ast::StoredDefinition> = IndexMap::new();
        for (uri, parsed) in definitions {
            if exclude_uri.is_some_and(|excluded| same_path(&uri, excluded)) {
                self.cache_detached_source_root_parsed_document(source_set_id, &uri, parsed);
                continue;
            }
            if self
                .documents
                .get(&uri)
                .is_some_and(|existing| !existing.content.is_empty())
            {
                self.cache_detached_source_root_parsed_document(source_set_id, &uri, parsed);
                continue;
            }

            desired_docs.insert(uri, parsed);
        }
        let inserted_uris: IndexSet<String> = desired_docs.keys().cloned().collect();

        if let Some(previous_uris) = self.source_set_uris(source_set_id)
            && previous_uris == &inserted_uris
        {
            let unchanged = desired_docs.iter().all(|(uri, parsed)| {
                self.documents
                    .get(uri)
                    .and_then(|doc| doc.parsed())
                    .is_some_and(|existing| existing == parsed)
            });
            if unchanged {
                self.clear_source_root_refresh(source_set_id);
                return inserted_uris.len();
            }
        }

        let revision = self.bump_revision();
        let previous_uris = self
            .source_set_uris(source_set_id)
            .cloned()
            .unwrap_or_default();
        let removed_uris: Vec<String> = previous_uris.iter().cloned().collect();
        if !previous_uris.is_empty() {
            let removable: Vec<String> = previous_uris
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
        }

        let mut inserted_count = 0usize;
        for (uri, parsed) in desired_docs {
            // Source-set replacement updates file revisions and detached membership
            // in one bulk pass via update_source_set_record below.
            let document = Document::new(
                uri,
                String::new(),
                crate::parse::SyntaxFile::from_parsed(parsed),
            );
            self.documents
                .insert(document.uri.clone(), Arc::new(document));
            inserted_count += 1;
        }
        for removed_uri in removed_uris {
            self.record_file_revision(&removed_uri, revision);
        }

        self.update_source_set_record(source_set_id, kind, inserted_uris, revision);
        self.invalidate_resolved_state(CacheInvalidationCause::SourceSetMutation);
        if let Some(source_set_id) = self.source_set_id(source_set_id) {
            self.invalidate_source_root_resolved_aggregate_for_source_set(source_set_id);
            self.invalidate_source_root_completion_state_for_source_set(
                source_set_id,
                CacheInvalidationCause::SourceSetMutation,
            );
        } else {
            self.invalidate_source_root_completion_state(CacheInvalidationCause::SourceSetMutation);
        }
        self.mark_source_root_graph_changed();
        inserted_count
    }

    /// Tolerantly load one source-root path into a parsed source-set.
    ///
    /// Parsing/load/cache failures are reported in `diagnostics` and do not
    /// panic or abort the session.
    pub fn load_source_root_tolerant(
        &mut self,
        source_set_id: &str,
        kind: SourceRootKind,
        source_root_path: &Path,
        exclude_uri: Option<&str>,
    ) -> SourceRootLoadReport {
        let cache_dir = resolve_source_root_cache_dir();
        self.load_source_root_tolerant_with_cache_dir(
            source_set_id,
            kind,
            source_root_path,
            exclude_uri,
            cache_dir.as_deref(),
        )
    }

    fn load_source_root_tolerant_with_cache_dir(
        &mut self,
        source_set_id: &str,
        kind: SourceRootKind,
        source_root_path: &Path,
        exclude_uri: Option<&str>,
        cache_dir: Option<&Path>,
    ) -> SourceRootLoadReport {
        let source_root_path_string = source_root_path.display().to_string();
        let parsed = match parse_source_root_with_cache_in(source_root_path, cache_dir) {
            Ok(parsed) => parsed,
            Err(err) => {
                return SourceRootLoadReport {
                    source_set_id: source_set_id.to_string(),
                    source_root_path: source_root_path_string,
                    parsed_file_count: 0,
                    inserted_file_count: 0,
                    cache_status: None,
                    cache_key: None,
                    cache_file: None,
                    diagnostics: vec![format!(
                        "Failed to load source root '{}': {}",
                        source_root_path.display(),
                        err
                    )],
                };
            }
        };
        let inserted_file_count =
            self.replace_parsed_source_set(source_set_id, kind, parsed.documents, exclude_uri);
        let _ = self.sync_source_root_semantic_summary_cache(
            source_set_id,
            source_root_path,
            cache_dir,
        );
        SourceRootLoadReport {
            source_set_id: source_set_id.to_string(),
            source_root_path: source_root_path_string,
            parsed_file_count: parsed.file_count,
            inserted_file_count,
            cache_status: Some(parsed.cache_status),
            cache_key: Some(parsed.cache_key),
            cache_file: parsed.cache_file,
            diagnostics: Vec::new(),
        }
    }

    #[cfg(test)]
    pub(crate) fn load_source_root_tolerant_with_cache_dir_for_tests(
        &mut self,
        source_set_id: &str,
        kind: SourceRootKind,
        source_root_path: &Path,
        exclude_uri: Option<&str>,
        cache_dir: Option<&Path>,
    ) -> SourceRootLoadReport {
        self.load_source_root_tolerant_with_cache_dir(
            source_set_id,
            kind,
            source_root_path,
            exclude_uri,
            cache_dir,
        )
    }
}
