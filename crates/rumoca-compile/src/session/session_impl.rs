use super::class_body::ModifierClassTarget;
use super::declaration_index::ItemKind;
use super::session_impl_queries::{QueryClassLookup, QueryClassNavigationTarget};
use super::session_impl_symbols::*;
use super::*;
impl Session {
    /// Return the current session revision token for snapshot/read coordination.
    pub fn revision(&self) -> u64 {
        self.current_revision.0
    }

    /// Query namespace completion cache for a namespace prefix.
    pub fn namespace_index_query(&mut self, prefix: &str) -> Result<Vec<(String, String, bool)>> {
        let rebuilt = self.refresh_source_root_namespace_cache();
        if rebuilt {
            record_namespace_index_query_miss();
        } else {
            record_namespace_index_query_hit();
        }

        Ok(self.namespace_children_cached(prefix))
    }

    /// Return the stable file id for a URI if it exists in this session.
    pub(crate) fn file_id_for_uri(&self, uri: &str) -> Option<FileId> {
        self.file_ids
            .get(uri)
            .copied()
            .or_else(|| self.file_path_keys.get(&path_lookup_key(uri)).copied())
    }

    /// Return the URI for a stable file id.
    #[cfg(test)]
    fn file_uri_for_id(&self, file_id: FileId) -> Option<&str> {
        self.file_uris.get(&file_id).map(String::as_str)
    }

    /// Return the stable file id for a URI if it exists in this session.
    #[cfg(test)]
    pub(crate) fn file_id(&self, uri: &str) -> Option<FileId> {
        self.file_id_for_uri(uri)
    }

    /// Return the URI for a stable file id.
    #[cfg(test)]
    pub(crate) fn file_uri(&self, file_id: FileId) -> Option<&str> {
        self.file_uri_for_id(file_id)
    }

    /// Return the last revision at which a URI was touched.
    #[cfg(test)]
    pub(crate) fn file_revision(&self, uri: &str) -> Option<RevisionId> {
        self.file_ids
            .get(uri)
            .and_then(|file_id| self.file_revisions.get(file_id).copied())
    }

    /// Return the stable source-set id for a source-set key if it exists.
    pub(crate) fn source_set_id(&self, source_set_key: &str) -> Option<SourceSetId> {
        self.source_sets.get(source_set_key).map(|record| record.id)
    }

    /// Return the last revision at which a source-set was mutated.
    #[cfg(test)]
    pub(crate) fn source_set_revision(&self, source_set_key: &str) -> Option<RevisionId> {
        self.source_sets
            .get(source_set_key)
            .map(|record| record.revision)
    }

    /// Return stable file ids currently associated with the source-set key.
    #[cfg(test)]
    pub(crate) fn source_set_file_ids(&self, source_set_key: &str) -> Vec<FileId> {
        self.source_set_uris(source_set_key)
            .map(|uris| uris.iter().filter_map(|uri| self.file_id(uri)).collect())
            .unwrap_or_default()
    }

    /// Return file ids changed after a revision.
    #[cfg(test)]
    pub(crate) fn changed_file_ids_since(&self, revision: RevisionId) -> Vec<FileId> {
        self.file_revisions
            .iter()
            .filter(|(_, changed)| **changed > revision)
            .map(|(file_id, _)| *file_id)
            .collect()
    }

    /// Return URIs for files changed after a revision.
    #[cfg(test)]
    pub(crate) fn changed_file_uris_since(&self, revision: RevisionId) -> Vec<String> {
        self.changed_file_ids_since(revision)
            .into_iter()
            .filter_map(|file_id| self.file_uri(file_id).map(ToString::to_string))
            .collect()
    }

    pub(super) fn insert_document(&mut self, document: Document, revision: RevisionId) {
        self.record_file_revision(&document.uri, revision);
        let uri = document.uri.clone();
        self.documents.insert(uri.clone(), Arc::new(document));
        self.sync_detached_document_uri(&uri);
    }

    pub(super) fn delete_document_entry(&mut self, uri: &str) {
        self.documents.shift_remove(uri);
        self.detached_document_uris.shift_remove(uri);
    }

    pub(super) fn sync_detached_document_uri(&mut self, uri: &str) {
        if self.documents.contains_key(uri) && !self.uri_is_in_source_set(uri) {
            self.detached_document_uris.insert(uri.to_string());
            return;
        }
        self.detached_document_uris.shift_remove(uri);
    }

    pub(super) fn is_source_root_backed_uri(&self, uri: &str) -> bool {
        !self.source_root_backing_keys_for_uri(uri).is_empty()
    }

    /// Add or update a document in the session.
    ///
    /// Returns an error if parsing fails. For LSP use where you want to store
    /// documents even on parse failure, use [`Session::update_document`] instead.
    pub fn add_document(&mut self, uri: &str, content: &str) -> Result<()> {
        let was_source_root_backed_document = self.is_source_root_backed_uri(uri);
        let revision = self.bump_revision();
        record_document_parse();
        let parse_started = maybe_start_timer();
        let parsed = match rumoca_phase_parse::parse_to_ast(content, uri) {
            Ok(parsed) => parsed,
            Err(error) => {
                if let Some(elapsed) = maybe_elapsed_duration(parse_started) {
                    record_document_parse_duration(elapsed);
                }
                record_document_parse_error();
                return Err(error);
            }
        };
        if let Some(elapsed) = maybe_elapsed_duration(parse_started) {
            record_document_parse_duration(elapsed);
        }
        self.detach_uri_from_source_sets(uri, revision, was_source_root_backed_document);
        self.insert_document(
            Document::new(
                uri.to_string(),
                content.to_string(),
                crate::parse::SyntaxFile::from_parsed(parsed),
            ),
            revision,
        );
        // Invalidate cached state
        self.invalidate_resolved_state(CacheInvalidationCause::DocumentMutation);
        Ok(())
    }

    /// Update a document, storing it even if parsing fails.
    ///
    /// This is designed for LSP use where documents need to be tracked
    /// even when they contain syntax errors. Returns the parse error if any.
    pub fn update_document(&mut self, uri: &str, content: &str) -> Option<String> {
        if let Some(existing) = self.documents.get(uri)
            && existing.content == content
        {
            return existing.parse_error().map(ToString::to_string);
        }
        let revision = self.bump_revision();
        self.apply_text_document_change_at_revision(uri, content, revision)
    }

    /// Add a pre-parsed definition to the session.
    ///
    /// This is more efficient than `add_document` when you've already parsed
    /// the file (e.g., using parallel parsing).
    pub fn add_parsed(&mut self, uri: &str, parsed: ast::StoredDefinition) {
        let revision = self.bump_revision();
        self.detach_uri_from_source_sets(uri, revision, false);
        self.insert_document(
            Document::new(
                uri.to_string(),
                String::new(), // Content not needed when pre-parsed
                crate::parse::SyntaxFile::from_parsed(parsed),
            ),
            revision,
        );
        // Invalidate cached state
        self.invalidate_resolved_state(CacheInvalidationCause::SourceSetMutation);
        self.invalidate_source_root_completion_state(CacheInvalidationCause::SourceSetMutation);
    }

    /// Add multiple pre-parsed definitions to the session.
    ///
    /// This is the most efficient way to load a large source root like MSL.
    pub fn add_parsed_batch(&mut self, definitions: Vec<(String, ast::StoredDefinition)>) {
        let revision = self.bump_revision();
        for (uri, parsed) in definitions {
            self.detach_uri_from_source_sets(&uri, revision, false);
            self.insert_document(
                Document::new(
                    uri,
                    String::new(),
                    crate::parse::SyntaxFile::from_parsed(parsed),
                ),
                revision,
            );
        }
        self.invalidate_resolved_state(CacheInvalidationCause::SourceSetMutation);
        self.invalidate_source_root_completion_state(CacheInvalidationCause::SourceSetMutation);
    }

    /// Remove all parsed documents previously loaded for a source-set id.
    pub fn remove_source_set(&mut self, source_set_id: &str) {
        let revision = self.bump_revision();
        self.remove_source_root_at_revision(source_set_id, revision);
    }

    /// Remove a document from the session.
    pub fn remove_document(&mut self, uri: &str) {
        let revision = self.bump_revision();
        self.apply_document_removal_at_revision(uri, revision);
    }

    /// Get a document by URI.
    pub fn get_document(&self, uri: &str) -> Option<&Document> {
        self.documents.get(uri).map(Arc::as_ref)
    }

    /// Return structured parse diagnostics and a source map for one document.
    pub fn document_parse_diagnostics_with_source_map(
        &self,
        uri: &str,
    ) -> Option<(Vec<CommonDiagnostic>, SourceMap)> {
        let doc = self.documents.get(uri)?;
        let source_map = self.session_source_map();
        let diagnostics = document_parse_diagnostics(doc, &source_map);
        (!diagnostics.is_empty()).then_some((diagnostics, source_map))
    }

    /// Get all document URIs.
    pub fn document_uris(&self) -> Vec<&str> {
        self.documents.keys().map(|s| s.as_str()).collect()
    }

    /// Build the resolved tree from all documents.
    ///
    /// This performs Parse -> Resolve but NOT typecheck.
    /// Typechecking happens after instantiation so dimension expressions
    /// can be evaluated with full modifier context (MLS §10.1).
    pub(crate) fn build_resolved(&mut self) -> Result<()> {
        self.build_resolved_with_diagnostics()
            .map(|_| ())
            .map_err(|diags| diagnostics_to_anyhow(&diags))
    }

    /// Build the resolved tree, returning diagnostics on failure.
    fn build_resolved_with_diagnostics(
        &mut self,
    ) -> Result<(Arc<ast::ResolvedTree>, CommonDiagnostics), CommonDiagnostics> {
        self.build_resolved_with_diagnostics_inner(ResolveBuildMode::Standard)
    }

    /// Build the resolved tree for strict target compilation.
    ///
    /// Unlike generic build flows, this ignores parse-error diagnostics from
    /// unrelated documents and resolves from available parsed ASTs. Unresolved
    /// name errors stay tolerant here so closure planning is not blocked by
    /// unrelated symbols.
    pub(crate) fn build_resolved_for_strict_compile_with_diagnostics(
        &mut self,
    ) -> Result<(Arc<ast::ResolvedTree>, CommonDiagnostics), CommonDiagnostics> {
        self.build_resolved_with_diagnostics_inner(ResolveBuildMode::StrictCompileRecovery)
    }

    fn ensure_parse_state_for_mode(&self, mode: ResolveBuildMode) -> Result<(), CommonDiagnostics> {
        if !mode.include_parse_error_diags() {
            return Ok(());
        }

        let parse_error_diags =
            collect_parse_error_diagnostics(&self.documents, &self.session_source_map());
        if parse_error_diags.is_empty() {
            Ok(())
        } else {
            Err(diagnostics_from_vec(parse_error_diags))
        }
    }

    pub(in crate::session) fn resolve_documents_for_mode(
        &self,
        mode: ResolveBuildMode,
    ) -> Result<(Arc<ast::ResolvedTree>, CommonDiagnostics, Vec<String>), CommonDiagnostics> {
        self.ensure_parse_state_for_mode(mode)?;

        let session_source_map = self.session_source_map();

        let definitions: Vec<_> = self
            .documents
            .values()
            .filter_map(|doc| {
                let parsed = if mode == ResolveBuildMode::StrictCompileRecovery {
                    doc.parse_error()
                        .and(doc.recovered().cloned())
                        .or_else(|| doc.parsed().cloned())
                } else {
                    doc.parsed().cloned()
                }?;
                Some((doc.uri.clone(), parsed))
            })
            .collect();
        let multi_document_session = definitions.len() > 1;
        let merged = merge_stored_definitions(definitions).map_err(|e| {
            let mut diags = CommonDiagnostics::new();
            diags.emit(merge_error_to_common(&e, &session_source_map));
            diags
        })?;
        let mut tree = ast::ClassTree::from_parsed(merged);

        for doc in self.documents.values() {
            tree.source_map.add(&doc.uri, &doc.content);
        }

        let parsed = ast::ParsedTree::new(tree);
        let unresolved_are_errors =
            mode.unresolved_refs_are_errors_in_single_document() && !multi_document_session;
        let resolve_options = ResolveOptions {
            unresolved_component_refs_are_errors: unresolved_are_errors,
            unresolved_function_calls_are_errors: unresolved_are_errors,
        };
        let (resolved, diagnostics) = if mode == ResolveBuildMode::StrictCompileRecovery {
            resolve_with_options_collect(parsed, resolve_options)
        } else {
            let resolved = resolve_with_options(parsed, resolve_options)?;
            (resolved, CommonDiagnostics::new())
        };
        let model_names = collect_model_names(&resolved.0.definitions);
        Ok((Arc::new(resolved), diagnostics, model_names))
    }

    fn build_resolved_with_diagnostics_inner(
        &mut self,
        mode: ResolveBuildMode,
    ) -> Result<(Arc<ast::ResolvedTree>, CommonDiagnostics), CommonDiagnostics> {
        self.ensure_parse_state_for_mode(mode)?;

        if mode == ResolveBuildMode::Standard
            && let Some(resolved) = self.query_state.resolved.builds.get(mode)
        {
            record_standard_resolved_cache_hit();
            return Ok((resolved.clone(), CommonDiagnostics::new()));
        }

        if mode == ResolveBuildMode::StrictCompileRecovery
            && let (Some(resolved), Some(diagnostics)) = (
                self.query_state
                    .resolved
                    .builds
                    .strict_compile_recovery
                    .as_ref(),
                self.query_state
                    .resolved
                    .builds
                    .strict_compile_recovery_diagnostics
                    .as_deref(),
            )
        {
            return Ok((resolved.clone(), diagnostics_from_vec(diagnostics.to_vec())));
        }

        if mode == ResolveBuildMode::StrictCompileRecovery
            && let Some(cached) = self.strict_compile_recovery_from_save_diagnostics_cache()
        {
            return Ok(cached);
        }

        let build_started = maybe_start_timer();
        let (resolved, diagnostics, model_names) = self.resolve_documents_for_mode(mode)?;
        self.query_state.resolved.model_names = model_names;
        if mode == ResolveBuildMode::StrictCompileRecovery {
            let cached_diags: Vec<_> = diagnostics.iter().cloned().collect();
            self.query_state.resolved.builds.strict_compile_recovery = Some(resolved.clone());
            self.query_state
                .resolved
                .builds
                .strict_compile_recovery_diagnostics = Some(cached_diags);
        } else {
            self.query_state.resolved.builds.set(mode, resolved.clone());
        }
        if let Some(elapsed) = maybe_elapsed_duration(build_started) {
            match mode {
                ResolveBuildMode::Standard => record_standard_resolved_build(elapsed),
                ResolveBuildMode::StrictCompileRecovery => record_strict_resolved_build(elapsed),
            }
        }

        Ok((resolved, diagnostics))
    }

    fn strict_compile_recovery_from_save_diagnostics_cache(
        &mut self,
    ) -> Option<(Arc<ast::ResolvedTree>, CommonDiagnostics)> {
        let resolved = self
            .query_state
            .flat
            .semantic_diagnostics
            .resolved_by_mode
            .get(&SemanticDiagnosticsMode::Save)?
            .clone();
        let diagnostics = self
            .query_state
            .flat
            .semantic_diagnostics
            .resolved_diagnostics_by_mode
            .get(&SemanticDiagnosticsMode::Save)
            .cloned()
            .unwrap_or_default();
        self.query_state.resolved.builds.strict_compile_recovery = Some(resolved.clone());
        self.query_state
            .resolved
            .builds
            .strict_compile_recovery_diagnostics = Some(diagnostics.clone());
        Some((resolved, diagnostics_from_vec(diagnostics)))
    }

    /// Get the resolved tree, returning an error if resolution hasn't been performed.
    pub(crate) fn ensure_resolved(&self) -> Result<&Arc<ast::ResolvedTree>> {
        self.query_state
            .resolved
            .builds
            .get(ResolveBuildMode::Standard)
            .ok_or_else(|| {
                anyhow::anyhow!("Session has no resolved tree — call build_resolved() first")
            })
    }

    /// Get all model names in the session.
    pub fn model_names(&mut self) -> Result<&[String]> {
        self.build_resolved()?;
        Ok(&self.query_state.resolved.model_names)
    }

    /// Count all class types in the session (model, connector, function, etc.).
    pub fn class_type_counts(&mut self) -> Result<std::collections::HashMap<String, usize>> {
        self.build_resolved()?;
        let resolved = self.ensure_resolved()?;
        Ok(collect_class_type_counts(&resolved.0.definitions))
    }

    /// Get all qualified class names from the resolved class tree.
    ///
    /// This includes only declared classes (packages, models, connectors,
    /// records, functions, etc.) from all documents in the session.
    /// Triggers resolution if not already done.
    pub fn all_class_names(&mut self) -> Result<Vec<String>> {
        self.build_resolved()?;
        let resolved = self.ensure_resolved()?;
        Ok(collect_qualified_class_names(&resolved.0.definitions))
    }

    /// Get all qualified class names from a completion-tolerant resolved tree.
    ///
    /// Unlike [`Session::all_class_names`], this uses the strict-compile recovery
    /// resolve mode so unrelated source-root diagnostics do not block editor
    /// namespace completion. Only declared classes are returned; components,
    /// loop indices, and other non-class definitions are excluded.
    pub fn all_class_names_for_completion(&mut self) -> Result<Vec<String>> {
        let (resolved, _) = self
            .build_resolved_for_strict_compile_with_diagnostics()
            .map_err(|diags| diagnostics_to_anyhow(&diags))?;
        Ok(collect_qualified_class_names(&resolved.0.definitions))
    }

    /// Get all qualified class names from indexed source-sets.
    ///
    /// This cache is used for editor namespace completion such as `Modelica.`
    /// and `import NewFolder.Test`. Workspace source-sets participate here so
    /// project packages complete the same way as source-root packages.
    fn collect_namespace_refresh_inputs(
        &self,
    ) -> (
        Vec<SourceSetId>,
        IndexMap<SourceSetId, SourceSetClassGraphSignature>,
        SummarySignature,
        Vec<String>,
    ) {
        let source_set_ids: Vec<SourceSetId> =
            self.source_sets.values().map(|record| record.id).collect();
        let source_set_signatures = source_set_ids
            .iter()
            .filter_map(|source_set_id| {
                self.source_set_class_graph_signature(*source_set_id)
                    .map(|signature| (*source_set_id, signature))
            })
            .collect();

        let mut orphan_signature = SummarySignature::new();
        let mut orphan_uris = Vec::new();
        for (uri, doc) in &self.documents {
            if !doc.content.is_empty() || doc.parsed().is_none() || self.uri_is_in_source_set(uri) {
                continue;
            }
            if let Some(file_id) = self.file_ids.get(uri).copied() {
                orphan_signature.insert(file_id, doc.summary_fingerprint());
                orphan_uris.push(uri.to_string());
            }
        }

        (
            source_set_ids,
            source_set_signatures,
            orphan_signature,
            orphan_uris,
        )
    }

    fn refresh_source_set_namespace_entries(
        &mut self,
        cache_state: &mut SourceRootNamespaceCache,
        source_set_signatures: &IndexMap<SourceSetId, SourceSetClassGraphSignature>,
    ) {
        for (source_set_id, signature) in source_set_signatures {
            let is_hit = cache_state
                .source_set_caches
                .get(source_set_id)
                .is_some_and(|entry| entry.signature == *signature);
            if is_hit {
                continue;
            }
            let Some(def_map) = self
                .source_set_package_def_map_query(*source_set_id)
                .cloned()
            else {
                cache_state.source_set_caches.shift_remove(source_set_id);
                continue;
            };
            let mut cache = NamespaceCompletionCache::default();
            cache.extend_from_package_def_map(&def_map);
            cache_state.source_set_caches.insert(
                *source_set_id,
                SourceSetNamespaceQueryCache {
                    signature: signature.clone(),
                    cache: cache.finalize(),
                },
            );
        }
    }

    fn refresh_orphan_namespace_entry(
        &mut self,
        cache_state: &mut SourceRootNamespaceCache,
        orphan_signature: &SummarySignature,
        orphan_uris: &[String],
    ) -> bool {
        if orphan_uris.is_empty() {
            cache_state.orphan_cache = None;
            return true;
        }

        let is_hit = cache_state
            .orphan_cache
            .as_ref()
            .is_some_and(|entry| entry.signature == *orphan_signature);
        if is_hit {
            return true;
        }

        let Some(orphan_def_map) = self
            .orphan_package_def_map_query(orphan_signature, orphan_uris)
            .cloned()
        else {
            cache_state.orphan_cache = None;
            return false;
        };

        let mut cache = NamespaceCompletionCache::default();
        cache.extend_from_package_def_map(&orphan_def_map);
        cache_state.orphan_cache = Some(OrphanNamespaceQueryCache {
            signature: orphan_signature.clone(),
            cache: cache.finalize(),
        });
        true
    }

    pub(crate) fn refresh_source_root_namespace_cache(&mut self) -> bool {
        let collect_started = maybe_start_timer();
        let (source_set_ids, source_set_signatures, orphan_signature, orphan_uris) =
            self.collect_namespace_refresh_inputs();
        if let Some(elapsed) = maybe_elapsed_duration(collect_started) {
            record_namespace_refresh_collect(elapsed);
        }

        if self
            .query_state
            .ast
            .source_root_namespace_cache
            .as_ref()
            .is_some_and(|cache| {
                cache.merged_cache.is_some()
                    && cache.merged_source_set_signatures == source_set_signatures
                    && cache.orphan_signature == orphan_signature
            })
        {
            record_namespace_completion_cache_hit();
            return false;
        }

        let mut cache_state = self
            .query_state
            .ast
            .source_root_namespace_cache
            .take()
            .unwrap_or_default();
        cache_state
            .source_set_caches
            .retain(|source_set_id, _| source_set_signatures.contains_key(source_set_id));

        let build_started = maybe_start_timer();
        self.refresh_source_set_namespace_entries(&mut cache_state, &source_set_signatures);
        if !self.refresh_orphan_namespace_entry(&mut cache_state, &orphan_signature, &orphan_uris) {
            self.query_state.ast.source_root_namespace_cache = Some(cache_state);
            record_namespace_completion_cache_miss();
            return true;
        }
        if let Some(elapsed) = maybe_elapsed_duration(build_started) {
            record_namespace_refresh_build(elapsed);
        }

        let finalize_started = maybe_start_timer();
        let mut merged_cache = NamespaceCompletionCache::default();
        for source_set_id in &source_set_ids {
            if let Some(entry) = cache_state.source_set_caches.get(source_set_id) {
                merged_cache.extend_from_namespace_cache(&entry.cache);
            }
        }
        if let Some(orphan_cache) = &cache_state.orphan_cache {
            merged_cache.extend_from_namespace_cache(&orphan_cache.cache);
        }
        let merged_cache = merged_cache.finalize();
        if let Some(elapsed) = maybe_elapsed_duration(finalize_started) {
            record_namespace_refresh_finalize(elapsed);
        }

        cache_state.store_merged_cache(merged_cache, source_set_signatures, orphan_signature);
        record_namespace_completion_cache_miss();
        self.query_state.ast.source_root_namespace_cache = Some(cache_state);
        true
    }

    pub fn namespace_class_names_for_completion(&mut self) -> Result<Vec<String>> {
        self.refresh_source_root_namespace_cache();
        Ok(self
            .query_state
            .ast
            .source_root_namespace_cache
            .as_ref()
            .and_then(|cache| cache.merged_cache.as_ref())
            .map(|cache| cache.class_names().to_vec())
            .unwrap_or_default())
    }

    /// Get cached immediate namespace children for a completion prefix.
    ///
    /// Prefixes use the editor completion form (`""`, `Modelica.`, `Modelica.Blocks.`).
    pub fn namespace_children_for_completion(
        &mut self,
        prefix: &str,
    ) -> Result<Vec<(String, String, bool)>> {
        self.namespace_index_query(prefix)
    }

    /// Get cached namespace children without triggering a rebuild.
    pub fn namespace_children_cached(&self, prefix: &str) -> Vec<(String, String, bool)> {
        self.query_state
            .ast
            .source_root_namespace_cache
            .as_ref()
            .and_then(|cache| cache.merged_cache.as_ref())
            .map(|cache| cache.children(prefix))
            .unwrap_or_default()
    }

    /// Get the cached namespace closure fingerprint for a completion prefix.
    pub fn namespace_fingerprint_cached(&self, prefix: &str) -> Option<String> {
        self.query_state
            .ast
            .source_root_namespace_cache
            .as_ref()
            .and_then(|cache| cache.merged_cache.as_ref())
            .and_then(|cache| cache.fingerprint_hex(prefix))
    }

    /// Get cached namespace class names without triggering a rebuild.
    pub fn namespace_class_names_cached(&self) -> Vec<String> {
        self.query_state
            .ast
            .source_root_namespace_cache
            .as_ref()
            .and_then(|cache| cache.merged_cache.as_ref())
            .map(|cache| cache.class_names().to_vec())
            .unwrap_or_default()
    }

    /// Get cached class names without triggering resolution.
    ///
    /// Returns the declared class names from the already-resolved tree, or an
    /// empty vector if resolution hasn't been performed yet. This is safe to
    /// call from read-only contexts (e.g., LSP completion with `&self`).
    pub fn all_class_names_cached(&self) -> Vec<String> {
        self.query_state
            .resolved
            .builds
            .any()
            .map(|r| collect_qualified_class_names(&r.0.definitions))
            .unwrap_or_default()
    }

    /// Returns true when a resolved tree is already cached in the session.
    ///
    /// This is useful for latency-sensitive paths (like editor completion)
    /// to avoid rebuilding resolution unless it is actually needed.
    pub fn has_resolved_cached(&self) -> bool {
        self.query_state.resolved.builds.any().is_some()
    }

    /// Returns true when the standard resolved session is already cached.
    pub fn has_standard_resolved_cached(&self) -> bool {
        self.query_state
            .resolved
            .builds
            .get(ResolveBuildMode::Standard)
            .is_some()
    }

    /// Returns true when semantic navigation artifacts already exist for a model.
    pub fn has_semantic_navigation_cached(&self, model_name: &str) -> bool {
        self.query_state
            .resolved
            .semantic_navigation
            .contains_key(model_name)
    }

    /// Returns true when semantic diagnostics artifacts already exist for a model.
    pub fn has_semantic_diagnostics_cached(&self, model_name: &str) -> bool {
        self.query_state
            .flat
            .semantic_diagnostics
            .interface_artifacts
            .keys()
            .chain(
                self.query_state
                    .flat
                    .semantic_diagnostics
                    .body_artifacts
                    .keys(),
            )
            .chain(
                self.query_state
                    .flat
                    .semantic_diagnostics
                    .model_stage_artifacts
                    .keys(),
            )
            .any(|key| key.model_name == model_name)
    }

    fn cached_compile_result(
        &mut self,
        model_name: &str,
        fingerprint: Fingerprint,
    ) -> Option<PhaseResult> {
        let entry = self
            .query_state
            .dae
            .compile_results
            .shift_remove(model_name)?;
        let is_hit = entry.fingerprint == fingerprint;
        let result = entry.result.clone();
        self.query_state
            .dae
            .compile_results
            .insert(model_name.to_string(), entry);
        is_hit.then_some(result)
    }

    pub(crate) fn trim_lru_cache<K, T>(cache: &mut IndexMap<K, T>, max_entries: usize)
    where
        K: Clone + std::hash::Hash + Eq,
    {
        while cache.len() > max_entries {
            let Some(oldest) = cache.keys().next().cloned() else {
                break;
            };
            cache.shift_remove(&oldest);
        }
    }

    fn insert_compile_result(
        &mut self,
        model_name: String,
        fingerprint: Fingerprint,
        result: PhaseResult,
    ) {
        self.query_state
            .dae
            .compile_results
            .shift_remove(&model_name);
        self.query_state.dae.compile_results.insert(
            model_name,
            CompileCacheEntry {
                fingerprint,
                result,
            },
        );
        Self::trim_lru_cache(
            &mut self.query_state.dae.compile_results,
            MAX_SESSION_COMPILE_CACHE_ENTRIES,
        );
    }

    fn cached_semantic_navigation(
        &mut self,
        model_name: &str,
        fingerprint: Fingerprint,
    ) -> Option<Arc<ast::ResolvedTree>> {
        let artifact = self
            .query_state
            .resolved
            .semantic_navigation
            .shift_remove(model_name)?;
        let is_hit = artifact.fingerprint == fingerprint;
        let resolved = artifact.resolved.clone();
        self.query_state
            .resolved
            .semantic_navigation
            .insert(model_name.to_string(), artifact);
        is_hit.then_some(resolved)
    }

    fn insert_semantic_navigation(
        &mut self,
        model_name: String,
        fingerprint: Fingerprint,
        resolved: Arc<ast::ResolvedTree>,
    ) {
        self.query_state
            .resolved
            .semantic_navigation
            .shift_remove(&model_name);
        self.query_state.resolved.semantic_navigation.insert(
            model_name,
            SemanticNavigationArtifact {
                fingerprint,
                resolved,
            },
        );
        Self::trim_lru_cache(
            &mut self.query_state.resolved.semantic_navigation,
            MAX_SESSION_SEMANTIC_NAVIGATION_CACHE_ENTRIES,
        );
    }

    /// Get component members for a class name from cached resolved state.
    ///
    /// Returns `(member_name, member_type_name)` pairs, including inherited members
    /// after applying extends `break` exclusions. If resolution is not available
    /// or the class name is ambiguous, returns an empty vector.
    ///
    /// This does not trigger resolution and is safe for read-only contexts.
    pub fn class_component_members_cached(&self, class_name: &str) -> Vec<(String, String)> {
        self.query_state
            .resolved
            .builds
            .any()
            .map(|resolved| class_component_members_from_tree(&resolved.0, class_name))
            .unwrap_or_default()
    }

    /// Get component members from cached active-target semantic navigation state.
    ///
    /// Falls back to the generic resolved cache when no active-target artifact is
    /// available for the requested model.
    pub fn class_component_members_for_navigation_cached(
        &self,
        model_name: &str,
        class_name: &str,
    ) -> Vec<(String, String)> {
        self.query_state
            .resolved
            .semantic_navigation
            .get(model_name)
            .map(|artifact| class_component_members_from_tree(&artifact.resolved.0, class_name))
            .filter(|members| !members.is_empty())
            .unwrap_or_else(|| self.class_component_members_cached(class_name))
    }

    /// Get the class tree.
    pub fn tree(&mut self) -> Result<&ast::ClassTree> {
        self.build_resolved()?;
        Ok(&self.ensure_resolved()?.0)
    }

    /// Get the resolved tree.
    pub fn resolved(&mut self) -> Result<ast::ResolvedTree> {
        self.build_resolved()?;
        Ok(ast::ResolvedTree(self.ensure_resolved()?.0.clone()))
    }

    /// Get any cached resolved tree without triggering a full rebuild.
    pub fn resolved_cached(&self) -> Option<ast::ResolvedTree> {
        self.query_state
            .resolved
            .builds
            .any()
            .map(|resolved| ast::ResolvedTree(resolved.0.clone()))
    }

    /// Get a strict-recovery resolved tree for an active editor target.
    ///
    /// This reuses the strict-recovery resolved snapshot so unrelated parse or
    /// resolve issues do not block hover/goto on the active model.
    pub fn resolved_for_semantic_navigation(
        &mut self,
        model_name: &str,
    ) -> Result<Arc<ast::ResolvedTree>> {
        let (resolved, _) = self
            .build_resolved_for_strict_compile_with_diagnostics()
            .map_err(|diags| diagnostics_to_anyhow(&diags))?;
        let fingerprint = self.model_dependency_fingerprint(
            &resolved.0,
            ResolveBuildMode::StrictCompileRecovery,
            model_name,
        );
        if let Some(cached) = self.cached_semantic_navigation(model_name, fingerprint) {
            record_semantic_navigation_cache_hit();
            return Ok(cached);
        }

        record_semantic_navigation_cache_miss();
        record_semantic_navigation_build();
        self.insert_semantic_navigation(model_name.to_string(), fingerprint, resolved.clone());
        Ok(resolved)
    }

    /// Compile a specific model.
    ///
    /// Uses the phase order: Resolve -> Instantiate -> Typecheck -> Flatten -> ToDae
    pub fn compile_model(&mut self, model_name: &str) -> Result<CompilationResult> {
        match self.compile_model_phases(model_name)? {
            PhaseResult::Success(result) => Ok(*result),
            PhaseResult::NeedsInner { missing_inners } => Err(anyhow::anyhow!(
                "Instantiate error: missing inner declarations: {}",
                missing_inners.join(", ")
            )),
            PhaseResult::Failed {
                phase,
                error,
                error_code,
            } => {
                if let Some(code) = error_code {
                    Err(anyhow::anyhow!("{} error [{}]: {}", phase, code, error))
                } else {
                    Err(anyhow::anyhow!("{} error: {}", phase, error))
                }
            }
        }
    }

    /// Compile a model with phase-level tracking.
    ///
    /// Uses the new phase order: Resolve -> Instantiate -> Typecheck -> Flatten -> ToDae
    pub fn compile_model_phases(&mut self, model_name: &str) -> Result<PhaseResult> {
        self.build_resolved()?;
        let resolved = self.ensure_resolved()?.clone();
        let fingerprint =
            self.model_dependency_fingerprint(&resolved.0, ResolveBuildMode::Standard, model_name);
        if let Some(cached) = self.cached_compile_result(model_name, fingerprint) {
            return Ok(cached);
        }

        let result =
            self.compile_phase_result_query(&resolved.0, ResolveBuildMode::Standard, model_name);
        self.insert_compile_result(model_name.to_string(), fingerprint, result.clone());
        Ok(result)
    }

    /// Compile multiple models in parallel.
    pub fn compile_models_parallel(
        &mut self,
        model_names: &[&str],
    ) -> Result<Vec<(String, PhaseResult)>> {
        self.build_resolved()?;
        let resolved = self.ensure_resolved()?.clone();
        let names: Vec<String> = model_names.iter().map(|name| (*name).to_string()).collect();
        Ok(self.compile_models_with_cache(&resolved.0, ResolveBuildMode::Standard, &names))
    }

    /// Compile all models in parallel.
    pub fn compile_all_parallel(&mut self) -> Result<Vec<(String, PhaseResult)>> {
        self.build_resolved()?;
        let resolved = self.ensure_resolved()?.clone();
        let names = self.query_state.resolved.model_names.clone();
        Ok(self.compile_models_with_cache(&resolved.0, ResolveBuildMode::Standard, &names))
    }

    /// Compile all models and return summary.
    pub fn compile_all_with_summary(
        &mut self,
    ) -> Result<(Vec<(String, PhaseResult)>, CompilationSummary)> {
        let results = self.compile_all_parallel()?;
        let summary = CompilationSummary::from_results(&results);
        Ok((results, summary))
    }

    /// Compile the requested model using strict-reachable semantics with
    /// internal recovery to surface additional diagnostics.
    ///
    /// This compiles the requested model strictly and collects diagnostics from
    /// the target's reachable transitive dependency closure.
    pub fn compile_model_strict_reachable_with_recovery(
        &mut self,
        model_name: &str,
    ) -> StrictCompileReport {
        self.compile_model_strict_reachable_report(model_name, true)
    }

    /// Compile the requested model using strict-reachable semantics with
    /// internal recovery while bypassing the session compile cache.
    ///
    /// This is intended for focused editor-style compiles where source-root AST
    /// and resolved state should be reused, but per-model phase results should not
    /// accumulate across unrelated requests.
    pub fn compile_model_strict_reachable_uncached_with_recovery(
        &mut self,
        model_name: &str,
    ) -> StrictCompileReport {
        self.compile_model_strict_reachable_report(model_name, false)
    }

    /// Compile the requested model through DAE using strict-reachable
    /// semantics with internal recovery while bypassing the session compile
    /// cache.
    ///
    /// This keeps the same strict failure behavior as
    /// `compile_model_strict_reachable_uncached_with_recovery`, but returns
    /// only the DAE artifact and experiment metadata that simulation needs.
    pub fn compile_model_dae_strict_reachable_uncached_with_recovery(
        &mut self,
        model_name: &str,
    ) -> std::result::Result<Box<DaeCompilationResult>, String> {
        let (resolved, resolve_diags) =
            match self.build_resolved_for_strict_compile_with_diagnostics() {
                Ok(build) => build,
                Err(diags) => {
                    let failures: Vec<ModelFailureDiagnostic> = diags
                        .iter()
                        .map(|diag| ModelFailureDiagnostic {
                            model_name: "<resolve>".to_string(),
                            phase: None,
                            error_code: diag.code.clone(),
                            error: diag.message.clone(),
                            primary_label: diag.labels.iter().find(|label| label.primary).cloned(),
                        })
                        .collect();
                    let requested = requested_missing_result_message(model_name, &failures);
                    return Err(format_strict_failure_summary(
                        model_name, requested, &failures, 8,
                    ));
                }
            };

        let tree = &resolved.0;
        let closure = self.reachable_model_closure_query(
            tree,
            ResolveBuildMode::StrictCompileRecovery,
            model_name,
        );
        let target_source_files = collect_target_source_files(tree, &closure.reachable_classes);
        let mut failures = collect_parse_failures_for_files(
            &self.documents,
            &tree.source_map,
            &target_source_files,
        );
        let resolve_failures = collect_resolve_failures_for_files(
            &resolve_diags,
            &tree.source_map,
            &target_source_files,
        );
        let target_has_resolve_failures = !resolve_failures.is_empty();
        failures.extend(resolve_failures);

        let related_targets: Vec<String> = closure
            .compile_targets
            .iter()
            .filter(|name| name.as_str() != model_name)
            .cloned()
            .collect();
        for (name, result) in self.compile_models_without_cache(tree, &related_targets) {
            if let Some(failure) = phase_result_to_failure(tree, &name, &result) {
                failures.push(failure);
            }
        }

        if target_has_resolve_failures {
            let requested = requested_missing_result_message(model_name, &failures);
            return Err(format_strict_failure_summary(
                model_name, requested, &failures, 8,
            ));
        }

        let requested_result =
            self.dae_phase_result_query(tree, ResolveBuildMode::StrictCompileRecovery, model_name);
        let requested = dae_phase_result_requested_message(model_name, &requested_result);
        if let Some(failure) = dae_phase_result_to_failure(tree, model_name, &requested_result) {
            failures.push(failure);
        }
        if !failures.is_empty() {
            return Err(format_strict_failure_summary(
                model_name, requested, &failures, 8,
            ));
        }

        match requested_result {
            DaePhaseResult::Success(result) => Ok(result),
            DaePhaseResult::NeedsInner { .. } | DaePhaseResult::Failed { .. } => {
                unreachable!("strict DAE compile returned failure without diagnostics")
            }
        }
    }

    fn compile_model_strict_reachable_report(
        &mut self,
        model_name: &str,
        use_compile_cache: bool,
    ) -> StrictCompileReport {
        let (resolved, resolve_diags) =
            match self.build_resolved_for_strict_compile_with_diagnostics() {
                Ok(build) => build,
                Err(diags) => {
                    let mut failures = Vec::new();
                    failures.extend(diags.iter().map(|diag| ModelFailureDiagnostic {
                        model_name: "<resolve>".to_string(),
                        phase: None,
                        error_code: diag.code.clone(),
                        error: diag.message.clone(),
                        primary_label: diag.labels.iter().find(|label| label.primary).cloned(),
                    }));
                    return StrictCompileReport {
                        requested_model: model_name.to_string(),
                        requested_result: None,
                        summary: CompilationSummary::default(),
                        failures,
                        source_map: Some(self.session_source_map()),
                    };
                }
            };

        let tree = &resolved.0;
        let closure = self.reachable_model_closure_query(
            tree,
            ResolveBuildMode::StrictCompileRecovery,
            model_name,
        );
        let target_source_files = collect_target_source_files(tree, &closure.reachable_classes);
        let mut failures = collect_parse_failures_for_files(
            &self.documents,
            &tree.source_map,
            &target_source_files,
        );
        let resolve_failures = collect_resolve_failures_for_files(
            &resolve_diags,
            &tree.source_map,
            &target_source_files,
        );
        let target_has_resolve_failures = !resolve_failures.is_empty();
        failures.extend(resolve_failures);
        let results = if use_compile_cache {
            self.compile_models_with_cache(
                tree,
                ResolveBuildMode::StrictCompileRecovery,
                &closure.compile_targets,
            )
        } else {
            self.compile_models_without_cache(tree, &closure.compile_targets)
        };
        finalize_strict_compile_report(
            tree,
            model_name,
            target_has_resolve_failures,
            failures,
            results,
        )
    }

    /// Compile a model with an explicit compilation mode.
    pub fn compile_model_with_mode(
        &mut self,
        model_name: &str,
        mode: CompilationMode,
    ) -> StrictCompileReport {
        match mode {
            CompilationMode::StrictReachable | CompilationMode::StrictReachableWithRecovery => {
                self.compile_model_strict_reachable_with_recovery(model_name)
            }
            CompilationMode::StrictReachableUncachedWithRecovery => {
                self.compile_model_strict_reachable_uncached_with_recovery(model_name)
            }
        }
    }

    pub(in crate::session) fn session_source_map(&self) -> SourceMap {
        let mut source_map = SourceMap::new();
        for doc in self.documents.values() {
            source_map.add(&doc.uri, &doc.content);
        }
        source_map
    }

    pub(in crate::session) fn model_dependency_fingerprint(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
    ) -> Fingerprint {
        self.query_state
            .resolved
            .dependency_fingerprints
            .get_or_insert_with(mode, || DependencyFingerprintCache::from_tree(tree))
            .model_fingerprint(model_name)
    }

    fn compile_models_with_cache(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_names: &[String],
    ) -> Vec<(String, PhaseResult)> {
        if model_names.len() == 1 {
            let name = model_names[0].clone();
            let fingerprint = self.model_dependency_fingerprint(tree, mode, &name);
            if let Some(result) = self.cached_compile_result(&name, fingerprint) {
                return vec![(name, result)];
            }

            let result = self.compile_phase_result_query(tree, mode, &name);
            self.insert_compile_result(name.clone(), fingerprint, result.clone());
            return vec![(name, result)];
        }

        let dep_cache = self
            .query_state
            .resolved
            .dependency_fingerprints
            .get_or_insert_with(mode, || DependencyFingerprintCache::from_tree(tree));

        let models_with_fingerprints: Vec<(String, Fingerprint)> = model_names
            .iter()
            .map(|name| (name.clone(), dep_cache.model_fingerprint(name)))
            .collect();

        let misses: Vec<(String, Fingerprint)> = models_with_fingerprints
            .iter()
            .filter_map(|(name, fingerprint)| {
                let hit = self
                    .query_state
                    .dae
                    .compile_results
                    .get(name)
                    .is_some_and(|entry| entry.fingerprint == *fingerprint);
                if hit {
                    None
                } else {
                    Some((name.clone(), *fingerprint))
                }
            })
            .collect();

        let compiled_misses: Vec<(String, Fingerprint, PhaseResult)> = misses
            .par_iter()
            .map(|(name, fingerprint)| {
                (
                    name.clone(),
                    *fingerprint,
                    compile_model_internal(tree, name),
                )
            })
            .collect();

        for (name, fingerprint, result) in compiled_misses {
            self.insert_compile_result(name, fingerprint, result);
        }

        let mut results = Vec::with_capacity(models_with_fingerprints.len());
        for (name, fingerprint) in models_with_fingerprints {
            if let Some(result) = self.cached_compile_result(&name, fingerprint) {
                results.push((name, result));
                continue;
            }

            // Defensive fallback: compile directly if cache entry is absent.
            let result = compile_model_internal(tree, &name);
            self.insert_compile_result(name.clone(), fingerprint, result.clone());
            results.push((name, result));
        }
        results
    }

    fn compile_models_without_cache(
        &mut self,
        tree: &ast::ClassTree,
        model_names: &[String],
    ) -> Vec<(String, PhaseResult)> {
        if model_names.len() == 1 {
            let name = model_names[0].clone();
            let result = self.compile_phase_result_query(
                tree,
                ResolveBuildMode::StrictCompileRecovery,
                &name,
            );
            return vec![(name, result)];
        }

        model_names
            .par_iter()
            .map(|name| (name.clone(), compile_model_internal(tree, name)))
            .collect()
    }
}

fn dae_phase_result_requested_message(model_name: &str, result: &DaePhaseResult) -> String {
    match result {
        DaePhaseResult::Success(_) => format!("{model_name} compiled successfully"),
        DaePhaseResult::NeedsInner { missing_inners } => format!(
            "{model_name} requires inner declarations: {}",
            missing_inners.join(", ")
        ),
        DaePhaseResult::Failed { phase, error, .. } => {
            format!("{model_name} failed in {phase}: {error}")
        }
    }
}

pub(crate) fn class_name_matches_query_target(
    qualified_name: &str,
    class_name: &str,
    suffix: Option<&str>,
) -> bool {
    suffix.map_or(qualified_name == class_name, |suffix| {
        qualified_name == class_name || qualified_name.ends_with(suffix)
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
        let suffix = (!class_name.contains('.')).then(|| format!(".{class_name}"));
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
    locations: &mut Vec<(String, ast::Location)>,
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
    locations: &mut Vec<(String, ast::Location)>,
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
    locations: &'a mut Vec<(String, ast::Location)>,
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
    locations: &'a mut Vec<(String, ast::Location)>,
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
    locations: &mut Vec<(String, ast::Location)>,
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
    locations: &mut Vec<(String, ast::Location)>,
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
    locations: &mut Vec<(String, ast::Location)>,
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
    locations: &mut Vec<(String, ast::Location)>,
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
    locations: &mut Vec<(String, ast::Location)>,
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
    locations: &mut Vec<(String, ast::Location)>,
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
) -> Vec<&'a ast::Token> {
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
    location: &ast::Location,
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
