use super::*;

impl Session {
    /// Replace a parsed source-set (e.g., a library) in one operation.
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

        let source_set_cache_id = self.source_set_id(source_set_id);
        self.update_source_set_record(source_set_id, kind, inserted_uris, revision);
        self.invalidate_resolved_state(CacheInvalidationCause::SourceSetMutation);
        if let Some(source_set_id) = source_set_cache_id {
            self.invalidate_library_completion_state_for_source_set(
                source_set_id,
                CacheInvalidationCause::SourceSetMutation,
            );
        } else {
            self.invalidate_library_completion_state(CacheInvalidationCause::SourceSetMutation);
        }
        inserted_count
    }

    /// Tolerantly index one library path into a parsed source-set.
    ///
    /// Parsing/index/cache failures are reported in `diagnostics` and do not
    /// panic or abort the session.
    pub fn index_library_tolerant(
        &mut self,
        source_set_id: &str,
        kind: SourceRootKind,
        library_path: &Path,
        exclude_uri: Option<&str>,
    ) -> IndexingReport {
        let cache_dir = resolve_library_cache_dir();
        self.index_library_tolerant_with_cache_dir(
            source_set_id,
            kind,
            library_path,
            exclude_uri,
            cache_dir.as_deref(),
        )
    }

    fn index_library_tolerant_with_cache_dir(
        &mut self,
        source_set_id: &str,
        kind: SourceRootKind,
        library_path: &Path,
        exclude_uri: Option<&str>,
        cache_dir: Option<&Path>,
    ) -> IndexingReport {
        let library_path_str = library_path.display().to_string();
        let parsed = match parse_library_with_cache_in(library_path, cache_dir) {
            Ok(parsed) => parsed,
            Err(err) => {
                return IndexingReport {
                    source_set_id: source_set_id.to_string(),
                    library_path: library_path_str,
                    indexed_file_count: 0,
                    inserted_file_count: 0,
                    cache_status: None,
                    cache_key: None,
                    cache_file: None,
                    diagnostics: vec![format!(
                        "Failed to load library '{}': {}",
                        library_path.display(),
                        err
                    )],
                };
            }
        };
        let summary_cache_dir = resolve_semantic_summary_cache_dir_from_root(cache_dir);
        let semantic_summary = read_library_semantic_summary(
            summary_cache_dir.as_deref(),
            &parsed.cache_key,
            &parsed.documents,
        )
        .unwrap_or_else(|| {
            let summary = LibrarySemanticSummary::from_documents(&parsed.documents);
            let _ = write_library_semantic_summary(
                summary_cache_dir.as_deref(),
                library_path,
                &parsed.cache_key,
                &summary,
            );
            summary
        });

        let inserted_file_count =
            self.replace_parsed_source_set(source_set_id, kind, parsed.documents, exclude_uri);
        self.hydrate_source_set_semantic_summary(source_set_id, &semantic_summary);
        IndexingReport {
            source_set_id: source_set_id.to_string(),
            library_path: library_path_str,
            indexed_file_count: parsed.file_count,
            inserted_file_count,
            cache_status: Some(parsed.cache_status),
            cache_key: Some(parsed.cache_key),
            cache_file: parsed.cache_file,
            diagnostics: Vec::new(),
        }
    }

    #[cfg(test)]
    pub(crate) fn index_library_tolerant_with_cache_dir_for_tests(
        &mut self,
        source_set_id: &str,
        kind: SourceRootKind,
        library_path: &Path,
        exclude_uri: Option<&str>,
        cache_dir: Option<&Path>,
    ) -> IndexingReport {
        self.index_library_tolerant_with_cache_dir(
            source_set_id,
            kind,
            library_path,
            exclude_uri,
            cache_dir,
        )
    }
}
