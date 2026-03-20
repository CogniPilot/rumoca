use super::class_body::FileClassBodyIndex;
use super::class_body_semantics::FileClassBodySemantics;
use super::class_interface::resolve_import_candidates;
use super::declaration_index::ItemKind;
use super::session_impl::{
    NavigationReadContext, apply_break_exclusions, class_name_matches_query_target,
    collect_navigation_class_reference_locations_in_definition,
    collect_navigation_class_rename_locations_in_definition, navigation_location_contains_position,
    record_query_class_lookup_match,
};
use super::session_impl_symbols::*;
use super::*;
use crate::instrumentation::{
    record_declaration_index_query_hit, record_declaration_index_query_miss,
    record_orphan_package_membership_query_hit, record_orphan_package_membership_query_miss,
    record_scope_query_hit, record_scope_query_miss,
    record_source_set_package_membership_query_hit,
    record_source_set_package_membership_query_miss,
};

#[derive(Debug, Clone)]
pub(crate) struct QueryClassLookup {
    pub(crate) uri: String,
    pub(crate) qualified_name: String,
}

#[derive(Debug, Clone)]
pub(crate) struct QueryClassNavigationTarget {
    pub(crate) qualified_name: String,
    pub(crate) token_text: String,
    pub(crate) location: ast::Location,
}

struct NavigationTargetSearch<'a> {
    uri: &'a str,
    file_id: FileId,
    class_bodies: &'a FileClassBodyIndex,
    line: u32,
    character: u32,
}

impl Session {
    /// Create a new compilation session.
    pub fn new(config: SessionConfig) -> Self {
        // Initialize rayon thread pool if parallel mode is enabled
        if config.parallel {
            init_rayon_pool();
        }
        let query_state = SessionQueryState::default();
        Self {
            documents: IndexMap::new(),
            detached_document_uris: IndexSet::new(),
            detached_source_root_documents: IndexMap::new(),
            source_sets: IndexMap::new(),
            file_ids: IndexMap::new(),
            file_path_keys: IndexMap::new(),
            file_uris: IndexMap::new(),
            source_set_keys: IndexMap::new(),
            file_source_sets: IndexMap::new(),
            source_set_signature_overrides: IndexMap::new(),
            file_revisions: IndexMap::new(),
            next_file_id: 0,
            next_source_set_id: 0,
            current_revision: RevisionId::default(),
            next_revision: 0,
            snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            lightweight_snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            workspace_symbol_snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            query_state,
        }
    }

    pub(crate) fn invalidate_resolved_state(&mut self, cause: CacheInvalidationCause) {
        record_resolved_state_invalidation(cause);
        self.query_state.resolved.clear();
        self.query_state.flat.invalidate_diagnostics_inputs();
    }

    pub(crate) fn invalidate_strict_compile_state(&mut self, cause: CacheInvalidationCause) {
        record_strict_resolved_state_invalidation(cause);
        self.query_state
            .resolved
            .clear_mode(ResolveBuildMode::StrictCompileRecovery);
        self.query_state
            .flat
            .invalidate_diagnostics_inputs_for_mode(SemanticDiagnosticsMode::Save);
    }

    pub(crate) fn invalidate_library_completion_state(&mut self, cause: CacheInvalidationCause) {
        record_library_completion_state_invalidation(cause);
        self.query_state.ast.library_namespace_cache = None;
        self.query_state.ast.package_def_map.clear();
    }

    pub(crate) fn invalidate_library_completion_state_for_source_set(
        &mut self,
        source_set_id: SourceSetId,
        cause: CacheInvalidationCause,
    ) {
        record_library_completion_state_invalidation(cause);
        if let Some(cache) = self.query_state.ast.library_namespace_cache.as_mut() {
            cache.invalidate_source_set(source_set_id);
        } else {
            self.query_state.ast.library_namespace_cache = None;
        }
        self.query_state
            .ast
            .package_def_map
            .invalidate_source_set(source_set_id);
    }

    pub(crate) fn bump_revision(&mut self) -> RevisionId {
        self.sync_query_state_from_snapshots();
        self.next_revision = self.next_revision.saturating_add(1);
        let revision = RevisionId::new(self.next_revision);
        self.current_revision = revision;
        revision
    }

    pub(super) fn ensure_file_id(&mut self, uri: &str) -> FileId {
        if let Some(file_id) = self.file_ids.get(uri) {
            return *file_id;
        }
        let path_key = path_lookup_key(uri);
        if let Some(file_id) = self.file_path_keys.get(&path_key).copied() {
            self.file_ids.insert(uri.to_string(), file_id);
            return file_id;
        }
        let file_id = FileId::new(self.next_file_id);
        self.next_file_id = self.next_file_id.saturating_add(1);
        self.file_ids.insert(uri.to_string(), file_id);
        self.file_path_keys.insert(path_key, file_id);
        self.file_uris.insert(file_id, uri.to_string());
        file_id
    }

    pub(crate) fn record_file_revision(&mut self, uri: &str, revision: RevisionId) -> FileId {
        let file_id = self.ensure_file_id(uri);
        self.file_revisions.insert(file_id, revision);
        self.query_state.ast.record_file_revision(file_id);
        file_id
    }

    fn file_summary_fingerprint(&self, uri: &str) -> Option<Fingerprint> {
        self.documents.get(uri).map(|doc| doc.summary_fingerprint())
    }

    fn file_body_fingerprint(&self, uri: &str) -> Option<Fingerprint> {
        self.documents.get(uri).map(|doc| doc.body_fingerprint())
    }

    fn file_outline_fingerprint(&self, uri: &str) -> Option<Fingerprint> {
        self.documents.get(uri).map(|doc| doc.outline_fingerprint())
    }

    fn file_navigation_fingerprint(&self, uri: &str) -> Option<Fingerprint> {
        self.documents
            .get(uri)
            .map(|doc| doc.navigation_fingerprint())
    }

    fn summary_signature_for_uris(&self, uris: &[String]) -> SummarySignature {
        let mut signature = SummarySignature::new();
        for uri in uris {
            let Some(file_id) = self.file_id_for_uri(uri) else {
                continue;
            };
            let Some(fingerprint) = self.file_summary_fingerprint(uri) else {
                continue;
            };
            signature.insert(file_id, fingerprint);
        }
        signature
    }

    pub(super) fn source_set_query_signature(
        &self,
        source_set_id: SourceSetId,
    ) -> Option<SourceSetQuerySignature> {
        if let Some(signature) = self.source_set_signature_overrides.get(&source_set_id) {
            return Some(signature.clone());
        }
        let record = self
            .source_sets
            .values()
            .find(|record| record.id == source_set_id)?;
        match record.durability {
            SourceRootDurability::Volatile => Some(SourceSetQuerySignature::Summary(
                self.summary_signature_for_uris(&record.uris.iter().cloned().collect::<Vec<_>>()),
            )),
            SourceRootDurability::Normal | SourceRootDurability::Durable => {
                Some(SourceSetQuerySignature::Revision(record.revision))
            }
        }
    }

    pub(super) fn detached_summary_signature(&self) -> (SummarySignature, Vec<String>) {
        let mut signature = SummarySignature::new();
        let mut uris = Vec::new();
        for uri in &self.detached_document_uris {
            let Some(file_id) = self.file_id_for_uri(uri) else {
                continue;
            };
            let Some(fingerprint) = self.file_summary_fingerprint(uri) else {
                continue;
            };
            signature.insert(file_id, fingerprint);
            uris.push(uri.clone());
        }
        (signature, uris)
    }

    fn session_query_signature(&self) -> SessionQuerySignature {
        let source_sets = if self.source_set_signature_overrides.is_empty() {
            self.source_sets
                .values()
                .map(|record| (record.id, self.source_set_query_signature(record.id)))
                .filter_map(|(id, signature)| signature.map(|signature| (id, signature)))
                .collect()
        } else {
            self.source_set_signature_overrides.clone()
        };
        let (detached, _) = self.detached_summary_signature();
        SessionQuerySignature {
            source_sets,
            detached,
        }
    }

    fn source_set_uris_by_id(&self, source_set_id: SourceSetId) -> Vec<String> {
        self.source_sets
            .values()
            .find(|record| record.id == source_set_id)
            .map(|record| record.uris.iter().cloned().collect())
            .unwrap_or_default()
    }

    fn with_workspace_symbol_source_set_query<R>(
        &mut self,
        source_set_id: SourceSetId,
        signature: SourceSetQuerySignature,
        f: impl FnOnce(&SourceSetWorkspaceSymbolCache) -> R,
    ) -> R {
        let is_hit = self
            .query_state
            .ast
            .workspace_symbol_query_cache
            .as_ref()
            .and_then(|cache| cache.source_set_caches.get(&source_set_id))
            .is_some_and(|cache| cache.signature == signature);
        if !is_hit {
            let mut symbols = Vec::new();
            for uri in self.source_set_uris_by_id(source_set_id) {
                symbols.extend(self.file_item_index_query(&uri));
            }
            self.query_state
                .ast
                .workspace_symbol_query_cache
                .get_or_insert_with(WorkspaceSymbolQueryCache::default)
                .source_set_caches
                .insert(
                    source_set_id,
                    Arc::new({
                        let entries = symbols
                            .into_iter()
                            .map(WorkspaceSymbolSearchEntry::from_symbol)
                            .collect::<Vec<_>>();
                        let search_index = WorkspaceSymbolSearchIndex::from_entries(&entries);
                        SourceSetWorkspaceSymbolCache {
                            signature,
                            entries,
                            search_index,
                        }
                    }),
                );
        }

        let cache = self
            .query_state
            .ast
            .workspace_symbol_query_cache
            .as_ref()
            .and_then(|cache| cache.source_set_caches.get(&source_set_id))
            .expect("workspace-symbol source-set cache should exist");
        f(cache.as_ref())
    }

    fn with_detached_workspace_symbol_query<R>(
        &mut self,
        signature: SummarySignature,
        uris: Vec<String>,
        f: impl FnOnce(&DetachedWorkspaceSymbolCache) -> R,
    ) -> R {
        let is_hit = self
            .query_state
            .ast
            .workspace_symbol_query_cache
            .as_ref()
            .and_then(|cache| cache.detached_cache.as_ref())
            .is_some_and(|cache| cache.signature == signature);
        if !is_hit {
            let mut symbols = Vec::new();
            for uri in uris {
                symbols.extend(self.file_item_index_query(&uri));
            }
            self.query_state
                .ast
                .workspace_symbol_query_cache
                .get_or_insert_with(WorkspaceSymbolQueryCache::default)
                .detached_cache = Some(Arc::new({
                let entries = symbols
                    .into_iter()
                    .map(WorkspaceSymbolSearchEntry::from_symbol)
                    .collect::<Vec<_>>();
                let search_index = WorkspaceSymbolSearchIndex::from_entries(&entries);
                DetachedWorkspaceSymbolCache {
                    signature,
                    entries,
                    search_index,
                }
            }));
        }

        let cache = self
            .query_state
            .ast
            .workspace_symbol_query_cache
            .as_ref()
            .and_then(|cache| cache.detached_cache.as_ref())
            .expect("workspace-symbol detached cache should exist");
        f(cache.as_ref())
    }

    pub(crate) fn hydrate_source_set_semantic_summary(
        &mut self,
        source_set_key: &str,
        summary: &LibrarySemanticSummary,
    ) {
        let Some((source_set_id, uris)) = self
            .source_sets
            .get(source_set_key)
            .map(|record| (record.id, record.uris.iter().cloned().collect::<Vec<_>>()))
        else {
            return;
        };

        let mut signature = SummarySignature::new();
        for uri in uris {
            let Some(file_id) = self.file_id_for_uri(&uri) else {
                continue;
            };
            let Some(file_fingerprint) = summary
                .summary_fingerprint_for_uri(&uri)
                .or_else(|| self.file_summary_fingerprint(&uri))
            else {
                continue;
            };
            let Some(file_summary) = summary.file_summary_for_uri(&uri, file_id) else {
                continue;
            };
            let Some(index) = summary.declaration_index_for_uri(&uri, file_id) else {
                continue;
            };
            signature.insert(file_id, file_fingerprint);
            let workspace_symbols = index.workspace_symbols(&uri);
            let class_interface_index = FileClassInterfaceIndex::from_summary(&file_summary);
            self.query_state.ast.file_summary_cache.insert(
                file_id,
                FileSummaryQueryCache {
                    fingerprint: file_fingerprint,
                    summary: file_summary,
                },
            );
            self.query_state.ast.declaration_index_cache.insert(
                file_id,
                DeclarationIndexQueryCache {
                    fingerprint: file_fingerprint,
                    index,
                },
            );
            self.query_state.ast.file_item_index_cache.insert(
                file_id,
                FileItemIndex {
                    fingerprint: file_fingerprint,
                    symbols: workspace_symbols,
                },
            );
            self.query_state.ast.class_interface_query_cache.insert(
                file_id,
                ClassInterfaceQueryCache {
                    fingerprint: file_fingerprint,
                    index: class_interface_index,
                },
            );
        }
        let package_def_map = summary.package_def_map(|uri| self.file_id_for_uri(uri));

        self.query_state
            .ast
            .package_def_map
            .source_set_caches
            .insert(
                source_set_id,
                PackageDefMapQueryCache {
                    signature: self
                        .source_set_query_signature(source_set_id)
                        .unwrap_or(SourceSetQuerySignature::Summary(signature)),
                    def_map: package_def_map,
                },
            );
    }

    /// Get the cached parsed AST for a URI, or `None` if unavailable.
    pub fn parsed_file_query(&mut self, uri: &str) -> Option<&ast::StoredDefinition> {
        let file_id = self.file_id_for_uri(uri)?;
        let revision = self.file_revisions.get(&file_id).copied()?;
        let has_parsed = self
            .documents
            .get(uri)
            .is_some_and(|doc| doc.parsed().is_some());
        if !has_parsed {
            return None;
        }

        let cached = self
            .query_state
            .ast
            .parsed_file_query_revisions
            .get(&file_id)
            .is_some_and(|cached_revision| *cached_revision == revision);
        if cached {
            record_parsed_file_query_hit();
        } else {
            self.query_state
                .ast
                .parsed_file_query_revisions
                .insert(file_id, revision);
            record_parsed_file_query_miss();
        }

        self.documents.get(uri).and_then(|doc| doc.parsed())
    }

    /// Get the parsed AST for recovery-oriented flows when strict parse is unavailable.
    pub fn recovered_file_query(&mut self, uri: &str) -> Option<&ast::StoredDefinition> {
        let file_id = self.file_id_for_uri(uri)?;
        let revision = self.file_revisions.get(&file_id).copied()?;
        let has_recovered = self
            .documents
            .get(uri)
            .is_some_and(|doc| doc.recovered().is_some() || doc.parsed().is_some());
        if !has_recovered {
            return None;
        }

        let cached = self
            .query_state
            .ast
            .recovered_file_query_revisions
            .get(&file_id)
            .is_some_and(|cached_revision| *cached_revision == revision);
        if cached {
            record_recovered_file_query_hit();
        } else {
            self.query_state
                .ast
                .recovered_file_query_revisions
                .insert(file_id, revision);
            record_recovered_file_query_miss();
        }

        self.documents
            .get(uri)
            .and_then(|doc| doc.recovered().or(doc.parsed()))
    }

    pub(crate) fn file_summary_query(&mut self, uri: &str) -> Option<&FileSummary> {
        let file_id = self.file_id_for_uri(uri)?;
        let fingerprint = self.file_summary_fingerprint(uri)?;
        let is_hit = self
            .query_state
            .ast
            .file_summary_cache
            .get(&file_id)
            .is_some_and(|entry| entry.fingerprint == fingerprint);
        if is_hit {
            return self
                .query_state
                .ast
                .file_summary_cache
                .get(&file_id)
                .map(|entry| &entry.summary);
        }

        let summary = {
            let document = self.documents.get(uri)?;
            FileSummary::from_definition(file_id, document.best_effort())
        };
        self.query_state.ast.file_summary_cache.insert(
            file_id,
            FileSummaryQueryCache {
                fingerprint,
                summary,
            },
        );
        self.query_state
            .ast
            .file_summary_cache
            .get(&file_id)
            .map(|entry| &entry.summary)
    }

    pub(crate) fn file_outline_query(&mut self, uri: &str) -> Option<&FileOutline> {
        let file_id = self.file_id_for_uri(uri)?;
        let fingerprint = self.file_outline_fingerprint(uri)?;
        let is_hit = self
            .query_state
            .ast
            .file_outline_cache
            .get(&file_id)
            .is_some_and(|entry| entry.fingerprint == fingerprint);
        if is_hit {
            return self
                .query_state
                .ast
                .file_outline_cache
                .get(&file_id)
                .map(|entry| &entry.outline);
        }

        let class_bodies = self.file_class_body_query(uri)?.clone();
        let outline = {
            let document = self.documents.get(uri)?;
            FileOutline::from_definition(file_id, document.best_effort(), &class_bodies)
        };
        self.query_state.ast.file_outline_cache.insert(
            file_id,
            FileOutlineQueryCache {
                fingerprint,
                outline,
            },
        );
        self.query_state
            .ast
            .file_outline_cache
            .get(&file_id)
            .map(|entry| &entry.outline)
    }

    pub(crate) fn file_class_body_query(&mut self, uri: &str) -> Option<&FileClassBodyIndex> {
        let file_id = self.file_id_for_uri(uri)?;
        let fingerprint = self.file_body_fingerprint(uri)?;
        let is_hit = self
            .query_state
            .ast
            .file_class_body_cache
            .get(&file_id)
            .is_some_and(|entry| entry.fingerprint == fingerprint);
        if is_hit {
            return self
                .query_state
                .ast
                .file_class_body_cache
                .get(&file_id)
                .map(|entry| &entry.index);
        }

        let index = {
            let document = self.documents.get(uri)?;
            FileClassBodyIndex::from_definition(file_id, document.best_effort())
        };
        self.query_state
            .ast
            .file_class_body_cache
            .insert(file_id, FileClassBodyQueryCache { fingerprint, index });
        self.query_state
            .ast
            .file_class_body_cache
            .get(&file_id)
            .map(|entry| &entry.index)
    }

    /// Get cached workspace-symbol index entries for one file.
    pub fn file_item_index_query(&mut self, uri: &str) -> Vec<WorkspaceSymbol> {
        let file_id = match self.file_id_for_uri(uri) {
            Some(file_id) => file_id,
            None => return Vec::new(),
        };
        let fingerprint = match self.file_summary_fingerprint(uri) {
            Some(fingerprint) => fingerprint,
            None => return Vec::new(),
        };

        if let Some(cached) = self
            .query_state
            .ast
            .file_item_index_cache
            .get(&file_id)
            .filter(|entry| entry.fingerprint == fingerprint)
        {
            record_file_item_index_query_hit();
            return cached.symbols.clone();
        }

        let symbols = self
            .declaration_index_query(uri)
            .map(|index| index.workspace_symbols(uri))
            .unwrap_or_default();

        if symbols.is_empty()
            && self
                .documents
                .get(uri)
                .is_none_or(|doc| doc.recovered().is_none() && doc.parsed().is_none())
        {
            return symbols;
        }

        record_file_item_index_query_miss();
        self.query_state.ast.file_item_index_cache.insert(
            file_id,
            FileItemIndex {
                fingerprint,
                symbols: symbols.clone(),
            },
        );
        symbols
    }

    /// Query a file-local declaration index keyed by stable `ItemKey`.
    pub(crate) fn declaration_index_query(&mut self, uri: &str) -> Option<&DeclarationIndex> {
        let file_id = self.file_id_for_uri(uri)?;
        let fingerprint = self.file_summary_fingerprint(uri)?;
        let is_hit = self
            .query_state
            .ast
            .declaration_index_cache
            .get(&file_id)
            .is_some_and(|entry| entry.fingerprint == fingerprint);
        if is_hit {
            record_declaration_index_query_hit();
            return self
                .query_state
                .ast
                .declaration_index_cache
                .get(&file_id)
                .map(|entry| &entry.index);
        }

        let summary = self.file_summary_query(uri)?;
        let index = DeclarationIndex::from_summary(summary);
        record_declaration_index_query_miss();
        self.query_state
            .ast
            .declaration_index_cache
            .insert(file_id, DeclarationIndexQueryCache { fingerprint, index });
        self.query_state
            .ast
            .declaration_index_cache
            .get(&file_id)
            .map(|entry| &entry.index)
    }

    pub(crate) fn class_interface_index_query(
        &mut self,
        uri: &str,
    ) -> Option<&FileClassInterfaceIndex> {
        let file_id = self.file_id_for_uri(uri)?;
        let fingerprint = self.file_summary_fingerprint(uri)?;
        let is_hit = self
            .query_state
            .ast
            .class_interface_query_cache
            .get(&file_id)
            .is_some_and(|entry| entry.fingerprint == fingerprint);
        if is_hit {
            record_scope_query_hit();
            return self
                .query_state
                .ast
                .class_interface_query_cache
                .get(&file_id)
                .map(|entry| &entry.index);
        }

        let summary = self.file_summary_query(uri)?;
        let index = FileClassInterfaceIndex::from_summary(summary);
        record_scope_query_miss();
        self.query_state
            .ast
            .class_interface_query_cache
            .insert(file_id, ClassInterfaceQueryCache { fingerprint, index });
        self.query_state
            .ast
            .class_interface_query_cache
            .get(&file_id)
            .map(|entry| &entry.index)
    }

    pub(in crate::session) fn class_body_semantics_query(
        &mut self,
        uri: &str,
    ) -> Option<&FileClassBodySemantics> {
        let file_id = self.file_id_for_uri(uri)?;
        let fingerprint = self.file_navigation_fingerprint(uri)?;
        let is_hit = self
            .query_state
            .ast
            .class_body_semantics_cache
            .get(&file_id)
            .is_some_and(|entry| entry.fingerprint == fingerprint);
        if is_hit {
            return self
                .query_state
                .ast
                .class_body_semantics_cache
                .get(&file_id)
                .map(|entry| &entry.semantics);
        }

        let summary = self.file_summary_query(uri)?.clone();
        let class_bodies = self.file_class_body_query(uri)?.clone();
        let semantics = FileClassBodySemantics::from_parts(&summary, &class_bodies);
        self.query_state.ast.class_body_semantics_cache.insert(
            file_id,
            ClassBodySemanticsQueryCache {
                fingerprint,
                semantics,
            },
        );
        self.query_state
            .ast
            .class_body_semantics_cache
            .get(&file_id)
            .map(|entry| &entry.semantics)
    }

    /// Query parsed navigation references for the target under a cursor.
    ///
    /// This AST-tier query covers same-file component targets via cached
    /// class-body semantics, then falls back to parsed class targets across the
    /// loaded session documents.
    pub fn navigation_references_query(
        &mut self,
        uri: &str,
        line: u32,
        character: u32,
        include_declaration: bool,
    ) -> Option<Vec<(String, ast::Location)>> {
        if let Some(locations) = self.class_body_semantics_query(uri)?.references_at(
            line,
            character,
            include_declaration,
        ) {
            return Some(
                locations
                    .into_iter()
                    .map(|location| (uri.to_string(), location))
                    .collect(),
            );
        }

        self.navigation_class_references_query(uri, line, character, include_declaration)
    }

    /// Query the rename span for a parsed navigation target.
    pub fn navigation_prepare_rename_query(
        &mut self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<ast::Location> {
        if let Some(location) = self
            .class_body_semantics_query(uri)?
            .rename_span_at(line, character)
        {
            return Some(location);
        }

        self.navigation_class_prepare_rename_query(uri, line, character)
    }

    /// Query parsed rename locations for a navigation target.
    pub fn navigation_rename_locations_query(
        &mut self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<Vec<(String, ast::Location)>> {
        if let Some(locations) = self
            .class_body_semantics_query(uri)?
            .rename_locations_at(line, character)
        {
            return Some(
                locations
                    .into_iter()
                    .map(|location| (uri.to_string(), location))
                    .collect(),
            );
        }

        self.navigation_class_rename_locations_query(uri, line, character)
    }

    /// Query the parsed class target under a cursor.
    ///
    /// This AST-tier query resolves cross-file class targets for hover/goto
    /// without building semantic-navigation artifacts.
    pub fn navigation_class_target_query(
        &mut self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<NavigationClassTargetInfo> {
        let qualified_name = self
            .navigation_qualified_class_name_at_position(uri, line, character)
            .or_else(|| {
                self.navigation_class_target_at_position(uri, line, character)
                    .map(|target| target.qualified_name)
            })?;
        let target = self.lookup_query_class_target(&qualified_name)?;
        self.navigation_class_target_info(&target.uri, &target.qualified_name)
    }

    /// Query one class interface using cached parsed syntax.
    pub(crate) fn class_interface_query(
        &mut self,
        uri: &str,
        qualified_name: &str,
    ) -> Option<ClassInterface> {
        self.class_interface_index_query(uri)?
            .class_interface(qualified_name)
            .cloned()
    }

    /// Resolve type candidates for one class scope from the class-interface layer.
    pub fn class_type_resolution_candidates_query(
        &mut self,
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

    /// Resolve one local component type for a class scope using cached parsed AST.
    pub fn class_component_type_query(
        &mut self,
        uri: &str,
        qualified_name: &str,
        component_name: &str,
    ) -> Option<String> {
        self.class_interface_query(uri, qualified_name)
            .and_then(|class_interface| {
                class_interface
                    .component_type(component_name)
                    .map(ToString::to_string)
            })
    }

    /// Resolve local completion entries for one class scope from the class-interface layer.
    pub fn class_local_completion_items_query(
        &mut self,
        uri: &str,
        qualified_name: &str,
    ) -> Vec<ClassLocalCompletionItem> {
        self.class_interface_query(uri, qualified_name)
            .map(|class_interface| class_interface.local_completion_items())
            .unwrap_or_default()
    }

    /// Resolve local component hover/goto data from class-body semantics.
    pub fn local_component_info_query(
        &mut self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<LocalComponentInfo> {
        let item_key = self
            .class_body_semantics_query(uri)?
            .component_target_at(line, character)?
            .clone();
        self.class_interface_query(uri, item_key.container_path())
            .and_then(|class_interface| class_interface.local_component_info(item_key.name()))
    }

    /// Resolve the innermost enclosing class name at one source line.
    pub fn enclosing_class_qualified_name_query(&mut self, uri: &str, line: u32) -> Option<String> {
        let definition = self.documents.get(uri)?.best_effort();
        let target_line = line + 1;
        let within_prefix = definition
            .within
            .as_ref()
            .map(ToString::to_string)
            .filter(|prefix| !prefix.is_empty())
            .unwrap_or_default();
        for (name, class) in &definition.classes {
            if let Some(found) =
                enclosing_class_qualified_name_in_class(name, class, target_line, &within_prefix)
            {
                return Some(found);
            }
        }
        None
    }

    /// Resolve a class name to one unique declared class in the parsed query layer.
    ///
    /// Qualified names match exactly. Unqualified names require a unique suffix
    /// match across the current session documents.
    pub fn class_lookup_query(&mut self, class_name: &str) -> Option<String> {
        self.lookup_query_class_target(class_name)
            .map(|target| target.qualified_name)
    }

    pub(in crate::session) fn model_key_query(&mut self, qualified_name: &str) -> Option<ModelKey> {
        let target = self.lookup_query_class_target(qualified_name)?;
        self.class_interface_index_query(&target.uri)?
            .item_key_for_name(&target.qualified_name)
            .cloned()
            .map(ModelKey::new)
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
        let mut signature = SummarySignature::new();
        self.collect_query_class_component_members(
            &target.uri,
            &target.qualified_name,
            &mut members,
            &mut visiting,
            &mut signature,
        );

        let members = members.into_iter().collect::<Vec<_>>();
        self.insert_query_class_component_members(
            target.qualified_name,
            signature,
            members.clone(),
        );
        members
    }

    pub(in crate::session) fn lookup_query_class_target(
        &mut self,
        class_name: &str,
    ) -> Option<QueryClassLookup> {
        if class_name.contains('.') {
            let file_id = self
                .session_package_def_map_query()?
                .declared_class(class_name)?
                .item_key
                .file_id();
            let uri = self.file_uris.get(&file_id).map(String::as_str)?;
            return Some(QueryClassLookup {
                uri: uri.to_string(),
                qualified_name: class_name.to_string(),
            });
        }
        let def_map = self.session_package_def_map_query()?;
        let suffix = (!class_name.contains('.')).then(|| format!(".{class_name}"));
        let mut matched: Option<QueryClassLookup> = None;
        let matches = def_map
            .class_entries()
            .filter(|(qualified_name, _)| {
                class_name_matches_query_target(qualified_name, class_name, suffix.as_deref())
            })
            .map(|(qualified_name, entry)| (qualified_name.clone(), entry.item_key.file_id()))
            .collect::<Vec<_>>();

        for (qualified_name, file_id) in matches {
            let uri = self.file_uris.get(&file_id).map(String::as_str)?;
            record_query_class_lookup_match(&mut matched, uri, qualified_name.clone())?;
        }

        matched
    }

    fn session_package_def_map_query(&mut self) -> Option<&PackageDefMap> {
        let signature = self.session_query_signature();
        if self
            .query_state
            .ast
            .package_def_map
            .session_cache
            .as_ref()
            .is_some_and(|entry| entry.signature == signature)
        {
            return self
                .query_state
                .ast
                .package_def_map
                .session_cache
                .as_ref()
                .map(|entry| &entry.def_map);
        }

        let mut def_map = PackageDefMap::default();
        let source_set_ids = self
            .source_sets
            .values()
            .map(|record| record.id)
            .collect::<Vec<_>>();
        for source_set_id in source_set_ids {
            let Some(source_set_def_map) = self.source_set_package_def_map_query(source_set_id)
            else {
                continue;
            };
            def_map.extend_from_package_def_map(source_set_def_map);
        }

        let (orphan_signature, orphan_uris) = self.detached_summary_signature();
        if let Some(orphan_def_map) =
            self.orphan_package_def_map_query(&orphan_signature, &orphan_uris)
        {
            def_map.extend_from_package_def_map(orphan_def_map);
        }

        self.query_state.ast.package_def_map.session_cache =
            Some(SessionPackageDefMapQueryCache { signature, def_map });
        self.query_state
            .ast
            .package_def_map
            .session_cache
            .as_ref()
            .map(|entry| &entry.def_map)
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
        if !self.signature_is_current(&entry.signature) {
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
        signature: SummarySignature,
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
                ClassComponentMembersQueryCache { signature, members },
            );
        Self::trim_lru_cache(
            &mut self.query_state.ast.class_component_members_query_cache,
            MAX_SESSION_CLASS_MEMBER_QUERY_CACHE_ENTRIES,
        );
    }

    fn collect_query_class_component_members(
        &mut self,
        uri: &str,
        qualified_name: &str,
        members: &mut IndexMap<String, String>,
        visiting: &mut std::collections::HashSet<String>,
        signature: &mut SummarySignature,
    ) {
        if !visiting.insert(qualified_name.to_string()) {
            return;
        }

        self.record_query_dependency_signature(uri, signature);

        let Some(class_interface) = self.class_interface_query(uri, qualified_name) else {
            visiting.remove(qualified_name);
            return;
        };
        let imports = class_interface.import_map().clone();

        for extend in class_interface.extends() {
            if let Some(base_target) =
                self.lookup_query_extends_target(extend.base_name(), &imports)
            {
                self.collect_query_class_component_members(
                    &base_target.uri,
                    &base_target.qualified_name,
                    members,
                    visiting,
                    signature,
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

    fn signature_is_current(&self, signature: &SummarySignature) -> bool {
        signature.iter().all(|(file_id, fingerprint)| {
            let Some(uri) = self.file_uris.get(file_id) else {
                return false;
            };
            self.file_summary_fingerprint(uri) == Some(*fingerprint)
        })
    }

    fn lookup_query_extends_target(
        &mut self,
        base_name: &str,
        imports: &ImportMap,
    ) -> Option<QueryClassLookup> {
        let base_candidates = resolve_import_candidates(base_name, Some(imports));
        for candidate in base_candidates {
            if let Some(target) = self.lookup_query_class_target(&candidate) {
                return Some(target);
            }
        }
        None
    }

    fn navigation_class_references_query(
        &mut self,
        uri: &str,
        line: u32,
        character: u32,
        include_declaration: bool,
    ) -> Option<Vec<(String, ast::Location)>> {
        let target = self.navigation_class_target_at_position(uri, line, character)?;
        let document_uris = self.parsed_navigation_document_uris();
        self.prewarm_navigation_read_queries(&document_uris);
        let read = NavigationReadContext::new(self);
        let mut locations = Vec::new();
        for document_uri in document_uris {
            let Some(definition) = self
                .documents
                .get(&document_uri)
                .and_then(|doc| doc.parsed())
            else {
                continue;
            };
            collect_navigation_class_reference_locations_in_definition(
                &read,
                &document_uri,
                definition,
                &target.qualified_name,
                include_declaration,
                &mut locations,
            );
        }
        (!locations.is_empty()).then_some(locations)
    }

    fn navigation_class_prepare_rename_query(
        &mut self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<ast::Location> {
        self.navigation_class_target_at_position(uri, line, character)
            .map(|target| target.location)
    }

    fn navigation_class_rename_locations_query(
        &mut self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<Vec<(String, ast::Location)>> {
        let target = self.navigation_class_target_at_position(uri, line, character)?;
        let document_uris = self.parsed_navigation_document_uris();
        self.prewarm_navigation_read_queries(&document_uris);
        let read = NavigationReadContext::new(self);
        let mut locations = Vec::new();
        for document_uri in document_uris {
            let Some(definition) = self
                .documents
                .get(&document_uri)
                .and_then(|doc| doc.parsed())
            else {
                continue;
            };
            collect_navigation_class_rename_locations_in_definition(
                &read,
                &document_uri,
                definition,
                &target,
                &mut locations,
            );
        }
        (!locations.is_empty()).then_some(locations)
    }

    fn navigation_qualified_class_name_at_position(
        &mut self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<String> {
        let dotted_name = {
            let source = self.documents.get(uri)?.content.clone();
            qualified_class_name_at_position(&source, line, character)?
        };
        self.class_lookup_query(&dotted_name)
    }

    fn navigation_class_target_info(
        &self,
        target_uri: &str,
        qualified_name: &str,
    ) -> Option<NavigationClassTargetInfo> {
        let definition = self
            .documents
            .get(target_uri)
            .and_then(|doc| doc.parsed().or(doc.recovered()))?;
        let class = parsed_class_by_qualified_name(definition, qualified_name)?;
        Some(NavigationClassTargetInfo {
            target_uri: target_uri.to_string(),
            qualified_name: qualified_name.to_string(),
            class_name: class.name.text.to_string(),
            class_type: class.class_type.clone(),
            description: class.description.first().map(|desc| desc.text.to_string()),
            component_count: class.components.len(),
            equation_count: class.equations.len() + class.initial_equations.len(),
            declaration_location: class.name.location.clone(),
        })
    }

    fn navigation_class_target_at_position(
        &mut self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<QueryClassNavigationTarget> {
        let file_id = self.file_id_for_uri(uri)?;
        let class_bodies = self.file_class_body_query(uri)?.clone();
        let definition = self.documents.get(uri)?.parsed().cloned()?;
        let within_prefix = definition
            .within
            .as_ref()
            .map(ToString::to_string)
            .filter(|prefix| !prefix.is_empty())
            .unwrap_or_default();
        let search = NavigationTargetSearch {
            uri,
            file_id,
            class_bodies: &class_bodies,
            line,
            character,
        };
        for (name, class) in &definition.classes {
            if let Some(target) =
                self.navigation_class_target_in_class(&search, &within_prefix, name, class)
            {
                return Some(target);
            }
        }
        None
    }

    fn navigation_class_target_in_class(
        &mut self,
        search: &NavigationTargetSearch<'_>,
        container_path: &str,
        class_name: &str,
        class: &ast::ClassDef,
    ) -> Option<QueryClassNavigationTarget> {
        let class_item_key =
            ItemKey::new(search.file_id, ItemKind::Class, container_path, class_name);
        let qualified_name = class_item_key.qualified_name();
        if navigation_location_contains_position(
            &class.name.location,
            search.line,
            search.character,
        ) {
            return Some(QueryClassNavigationTarget {
                qualified_name: qualified_name.clone(),
                token_text: class.name.text.to_string(),
                location: class.name.location.clone(),
            });
        }
        if let Some(end_name) = &class.end_name_token
            && navigation_location_contains_position(
                &end_name.location,
                search.line,
                search.character,
            )
        {
            return Some(QueryClassNavigationTarget {
                qualified_name: qualified_name.clone(),
                token_text: end_name.text.to_string(),
                location: end_name.location.clone(),
            });
        }
        if let Some(constrainedby) = &class.constrainedby
            && let Some(target) = self.navigation_class_target_in_type_name(
                search.uri,
                &qualified_name,
                constrainedby,
                search.line,
                search.character,
            )
        {
            return Some(target);
        }
        for import in &class.imports {
            if let Some(target) =
                self.navigation_class_target_in_import(import, search.line, search.character)
            {
                return Some(target);
            }
        }
        for extend in &class.extends {
            if let Some(target) = self.navigation_class_target_in_type_name(
                search.uri,
                &qualified_name,
                &extend.base_name,
                search.line,
                search.character,
            ) {
                return Some(target);
            }
        }
        if let Some(target) =
            self.navigation_class_target_in_body(search, &qualified_name, &class_item_key)
        {
            return Some(target);
        }
        for (nested_name, nested_class) in &class.classes {
            if let Some(target) = self.navigation_class_target_in_class(
                search,
                &qualified_name,
                nested_name,
                nested_class,
            ) {
                return Some(target);
            }
        }
        for component in class.components.values() {
            if let Some(target) = self.navigation_class_target_in_type_name(
                search.uri,
                &qualified_name,
                &component.type_name,
                search.line,
                search.character,
            ) {
                return Some(target);
            }
            if let Some(constrainedby) = &component.constrainedby
                && let Some(target) = self.navigation_class_target_in_type_name(
                    search.uri,
                    &qualified_name,
                    constrainedby,
                    search.line,
                    search.character,
                )
            {
                return Some(target);
            }
        }
        None
    }

    fn navigation_class_target_in_body(
        &mut self,
        search: &NavigationTargetSearch<'_>,
        enclosing_qualified_name: &str,
        class_item_key: &ItemKey,
    ) -> Option<QueryClassNavigationTarget> {
        let class_body = search.class_bodies.class_body(class_item_key)?;
        for target in class_body.modifier_class_targets() {
            if !navigation_location_contains_position(
                target.location(),
                search.line,
                search.character,
            ) {
                continue;
            }
            let qualified_name = self.resolve_navigation_class_name(
                search.uri,
                enclosing_qualified_name,
                target.raw_name(),
            )?;
            return Some(QueryClassNavigationTarget {
                qualified_name,
                token_text: target.token_text().to_string(),
                location: target.location().clone(),
            });
        }
        None
    }

    fn navigation_class_target_in_import(
        &mut self,
        import: &ast::Import,
        line: u32,
        character: u32,
    ) -> Option<QueryClassNavigationTarget> {
        match import {
            ast::Import::Qualified { path, .. } => {
                let token = path.name.last()?;
                if !navigation_location_contains_position(&token.location, line, character) {
                    return None;
                }
                let qualified_name = self.class_lookup_query(&path.to_string())?;
                Some(QueryClassNavigationTarget {
                    qualified_name,
                    token_text: token.text.to_string(),
                    location: token.location.clone(),
                })
            }
            ast::Import::Renamed { alias, path, .. } => {
                let path_token = path.name.last()?;
                let on_alias =
                    navigation_location_contains_position(&alias.location, line, character);
                let on_path =
                    navigation_location_contains_position(&path_token.location, line, character);
                if !on_alias && !on_path {
                    return None;
                }
                let token = if on_alias { alias } else { path_token };
                let qualified_name = self.class_lookup_query(&path.to_string())?;
                Some(QueryClassNavigationTarget {
                    qualified_name,
                    token_text: token.text.to_string(),
                    location: token.location.clone(),
                })
            }
            ast::Import::Selective { path, names, .. } => {
                let token = names.iter().find(|token| {
                    navigation_location_contains_position(&token.location, line, character)
                })?;
                let candidate = format!("{path}.{}", token.text);
                let qualified_name = self.class_lookup_query(&candidate)?;
                Some(QueryClassNavigationTarget {
                    qualified_name,
                    token_text: token.text.to_string(),
                    location: token.location.clone(),
                })
            }
            ast::Import::Unqualified { .. } => None,
        }
    }

    fn navigation_class_target_in_type_name(
        &mut self,
        uri: &str,
        enclosing_qualified_name: &str,
        type_name: &ast::Name,
        line: u32,
        character: u32,
    ) -> Option<QueryClassNavigationTarget> {
        let token = type_name.name.last()?;
        if !navigation_location_contains_position(&token.location, line, character) {
            return None;
        }
        let qualified_name = self.resolve_navigation_class_name(
            uri,
            enclosing_qualified_name,
            &type_name.to_string(),
        )?;
        Some(QueryClassNavigationTarget {
            qualified_name,
            token_text: token.text.to_string(),
            location: token.location.clone(),
        })
    }

    pub(crate) fn resolve_navigation_class_name(
        &mut self,
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

    fn parsed_navigation_document_uris(&self) -> Vec<String> {
        self.documents
            .iter()
            .filter(|(_, doc)| doc.parsed().is_some())
            .map(|(uri, _)| uri.clone())
            .collect()
    }

    fn prewarm_navigation_read_queries(&mut self, document_uris: &[String]) {
        let _ = self.session_package_def_map_query();
        for document_uri in document_uris {
            let _ = self.class_interface_index_query(document_uri);
            let _ = self.class_body_semantics_query(document_uri);
        }
    }

    pub(crate) fn source_set_package_def_map_query(
        &mut self,
        source_set_id: SourceSetId,
    ) -> Option<&PackageDefMap> {
        let signature = self.source_set_query_signature(source_set_id)?;
        let is_hit = self
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .get(&source_set_id)
            .is_some_and(|entry| entry.signature == signature);
        if is_hit {
            record_source_set_package_membership_query_hit();
            return self
                .query_state
                .ast
                .package_def_map
                .source_set_caches
                .get(&source_set_id)
                .map(|entry| &entry.def_map);
        }

        let uris = self
            .source_sets
            .values()
            .find(|record| record.id == source_set_id)
            .map(|record| record.uris.iter().cloned().collect::<Vec<_>>())?;

        let mut def_map = PackageDefMap::default();
        for uri in uris {
            let Some(summary) = self.file_summary_query(&uri) else {
                continue;
            };
            def_map.extend_from_summary(summary);
        }
        record_source_set_package_membership_query_miss();
        self.query_state
            .ast
            .package_def_map
            .source_set_caches
            .insert(
                source_set_id,
                PackageDefMapQueryCache { signature, def_map },
            );
        self.query_state
            .ast
            .package_def_map
            .source_set_caches
            .get(&source_set_id)
            .map(|entry| &entry.def_map)
    }

    pub(crate) fn orphan_package_def_map_query(
        &mut self,
        orphan_signature: &SummarySignature,
        orphan_uris: &[String],
    ) -> Option<&PackageDefMap> {
        if orphan_uris.is_empty() {
            self.query_state.ast.package_def_map.orphan_cache = None;
            return None;
        }
        if self
            .query_state
            .ast
            .package_def_map
            .orphan_cache
            .as_ref()
            .is_some_and(|entry| entry.signature == *orphan_signature)
        {
            record_orphan_package_membership_query_hit();
            return self
                .query_state
                .ast
                .package_def_map
                .orphan_cache
                .as_ref()
                .map(|entry| &entry.def_map);
        }

        let mut def_map = PackageDefMap::default();
        for uri in orphan_uris {
            let Some(summary) = self.file_summary_query(uri) else {
                continue;
            };
            def_map.extend_from_summary(summary);
        }
        record_orphan_package_membership_query_miss();
        self.query_state.ast.package_def_map.orphan_cache = Some(OrphanPackageDefMapQueryCache {
            signature: orphan_signature.clone(),
            def_map,
        });
        self.query_state
            .ast
            .package_def_map
            .orphan_cache
            .as_ref()
            .map(|entry| &entry.def_map)
    }

    /// Query cached document outline symbols for one file.
    pub fn document_symbol_query(&mut self, uri: &str) -> Option<Vec<DocumentSymbol>> {
        let file_id = self.file_id_for_uri(uri)?;
        let fingerprint = self.file_outline_fingerprint(uri)?;
        if let Some(cached) = self
            .query_state
            .ast
            .file_outline_cache
            .get(&file_id)
            .filter(|entry| entry.fingerprint == fingerprint)
        {
            record_document_symbol_query_hit();
            return Some(cached.outline.document_symbols().to_vec());
        }

        record_document_symbol_query_miss();
        let outline = self.file_outline_query(uri)?;
        Some(outline.document_symbols().to_vec())
    }

    /// Query workspace symbols by name using per-file symbol indexes.
    pub fn workspace_symbol_query(&mut self, query: &str) -> Vec<WorkspaceSymbol> {
        let current_signature = self.session_query_signature();
        let is_hit = self
            .query_state
            .ast
            .workspace_symbol_query_cache
            .as_ref()
            .is_some_and(|cache| cache.signature == current_signature);
        if is_hit {
            record_workspace_symbol_query_hit();
        } else {
            record_workspace_symbol_query_miss();
        }

        let query_lower = query.to_lowercase();
        let mut matched = Vec::new();
        let source_set_signatures = current_signature.source_sets.clone();
        for (source_set_id, signature) in source_set_signatures {
            self.with_workspace_symbol_source_set_query(source_set_id, signature, |cache| {
                extend_workspace_symbol_matches(
                    &mut matched,
                    cache.entries.as_slice(),
                    &cache.search_index,
                    &query_lower,
                );
            });
        }
        let (detached_signature, detached_uris) = self.detached_summary_signature();
        self.with_detached_workspace_symbol_query(detached_signature, detached_uris, |cache| {
            extend_workspace_symbol_matches(
                &mut matched,
                cache.entries.as_slice(),
                &cache.search_index,
                &query_lower,
            );
        });

        let cache = self
            .query_state
            .ast
            .workspace_symbol_query_cache
            .get_or_insert_with(WorkspaceSymbolQueryCache::default);
        cache.signature = current_signature;
        let active_source_sets = cache
            .signature
            .source_sets
            .keys()
            .copied()
            .collect::<Vec<_>>();
        cache
            .source_set_caches
            .retain(|source_set_id, _| active_source_sets.contains(source_set_id));
        if cache.signature.detached.is_empty() {
            cache.detached_cache = None;
        }

        if query.is_empty() {
            return matched;
        }

        matched.sort_by(|left, right| {
            let left_score = workspace_symbol_query_match_score(&left.name, &query_lower);
            let right_score = workspace_symbol_query_match_score(&right.name, &query_lower);
            left_score.cmp(&right_score)
        });
        matched
    }

    pub(crate) fn prewarm_workspace_symbol_query_caches(&mut self) {
        let current_signature = self.session_query_signature();
        for (source_set_id, signature) in current_signature.source_sets.clone() {
            self.with_workspace_symbol_source_set_query(source_set_id, signature, |_| {});
        }
        let (detached_signature, detached_uris) = self.detached_summary_signature();
        self.with_detached_workspace_symbol_query(detached_signature, detached_uris, |_| {});

        let cache = self
            .query_state
            .ast
            .workspace_symbol_query_cache
            .get_or_insert_with(WorkspaceSymbolQueryCache::default);
        cache.signature = current_signature;
        let active_source_sets = cache
            .signature
            .source_sets
            .keys()
            .copied()
            .collect::<Vec<_>>();
        cache
            .source_set_caches
            .retain(|source_set_id, _| active_source_sets.contains(source_set_id));
        if cache.signature.detached.is_empty() {
            cache.detached_cache = None;
        }
    }
}

fn extend_workspace_symbol_matches(
    matched: &mut Vec<WorkspaceSymbol>,
    entries: &[WorkspaceSymbolSearchEntry],
    search_index: &WorkspaceSymbolSearchIndex,
    query_lower: &str,
) {
    if query_lower.is_empty() {
        matched.extend(entries.iter().map(|entry| entry.symbol.clone()));
        return;
    }

    let candidate_indices = search_index.candidate_indices(query_lower);
    match candidate_indices {
        Some(indices) => {
            matched.extend(indices.into_iter().filter_map(|index| {
                let entry = entries.get(index)?;
                entry
                    .name_lower
                    .contains(query_lower)
                    .then(|| entry.symbol.clone())
            }));
        }
        None => {
            matched.extend(
                entries
                    .iter()
                    .filter(|entry| entry.name_lower.contains(query_lower))
                    .map(|entry| entry.symbol.clone()),
            );
        }
    }
}

fn qualified_class_name_at_position(source: &str, line: u32, character: u32) -> Option<String> {
    let dotted_token = dotted_token_at_position(source, line, character)?;
    dotted_token
        .contains('.')
        .then_some(dotted_token)
        .filter(|token| token.chars().next().is_some_and(|c| c.is_ascii_uppercase()))
}

fn enclosing_class_qualified_name_in_class(
    name: &str,
    class: &ast::ClassDef,
    line: u32,
    prefix: &str,
) -> Option<String> {
    if class.location.start_line > line || line > class.location.end_line {
        return None;
    }

    let qualified = if prefix.is_empty() {
        name.to_string()
    } else {
        format!("{prefix}.{name}")
    };

    for (nested_name, nested) in &class.classes {
        if let Some(found) =
            enclosing_class_qualified_name_in_class(nested_name, nested, line, &qualified)
        {
            return Some(found);
        }
    }

    Some(qualified)
}

fn dotted_token_at_position(source: &str, line: u32, character: u32) -> Option<String> {
    let line = source.lines().nth(line as usize)?;
    let col = character as usize;
    if col > line.len() {
        return None;
    }
    let start = line[..col]
        .rfind(|c: char| !c.is_alphanumeric() && c != '_' && c != '.')
        .map(|idx| idx + 1)
        .unwrap_or(0);
    let end = line[col..]
        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '.')
        .map(|idx| col + idx)
        .unwrap_or(line.len());
    (start < end).then(|| line[start..end].to_string())
}

fn parsed_class_by_qualified_name<'a>(
    definition: &'a ast::StoredDefinition,
    qualified_name: &str,
) -> Option<&'a ast::ClassDef> {
    let within_prefix = definition
        .within
        .as_ref()
        .map(ToString::to_string)
        .filter(|prefix| !prefix.is_empty());
    let relative_name = within_prefix
        .as_ref()
        .and_then(|prefix| qualified_name.strip_prefix(&format!("{prefix}.")))
        .unwrap_or(qualified_name);
    let mut parts = relative_name.split('.');
    let first = parts.next()?;
    let mut class = definition.classes.get(first)?;
    for part in parts {
        class = class.classes.get(part)?;
    }
    Some(class)
}
