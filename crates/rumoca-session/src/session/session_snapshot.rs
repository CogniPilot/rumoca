use super::*;
use std::time::Instant;

struct IsolatedLocalSourceRootCloneState {
    kept_source_root_keys: IndexSet<String>,
    kept_source_set_ids: IndexSet<SourceSetId>,
    source_sets: IndexMap<String, SourceSetRecord>,
    source_set_keys: IndexMap<SourceSetId, String>,
}

impl Session {
    /// Clone the full session state for isolated background work.
    ///
    /// The clone keeps current documents, source roots, and query caches, but
    /// resets snapshot-cache handles so the clone cannot alias host snapshots.
    pub fn clone_for_isolated_work(&self) -> Session {
        let mut clone = self.clone();
        clone.snapshot_cache = Arc::new(Mutex::new(SharedSessionSnapshot::default()));
        clone.lightweight_snapshot_cache = Arc::new(Mutex::new(SharedSessionSnapshot::default()));
        clone.workspace_symbol_snapshot_cache =
            Arc::new(Mutex::new(SharedSessionSnapshot::default()));
        clone
    }

    /// Clone only the local input identity plus downstream warm artifacts for
    /// isolated local compile work.
    ///
    /// This keeps stable file ids for the selected local documents so
    /// save-diagnostics/model-stage caches remain reusable, while omitting
    /// non-workspace source-root AST state from the isolated clone. Workspace
    /// source roots remain because they participate in the active project graph.
    pub fn clone_for_isolated_local_work(&self, keep_uris: &[String]) -> Session {
        let keep_uris = keep_uris.iter().cloned().collect::<IndexSet<_>>();
        let IsolatedLocalSourceRootCloneState {
            kept_source_root_keys,
            kept_source_set_ids,
            source_sets,
            source_set_keys,
        } = self.isolated_local_clone_source_roots();
        let mut documents = IndexMap::new();
        let mut detached_document_uris = IndexSet::new();
        let mut detached_source_root_documents = IndexMap::new();
        let mut file_ids = IndexMap::new();
        let mut file_path_keys = IndexMap::new();
        let mut file_uris = IndexMap::new();
        let mut file_source_sets = IndexMap::new();
        let mut file_revisions = IndexMap::new();

        for uri in self.documents.keys() {
            if !self.should_keep_isolated_local_document(uri, &keep_uris, &kept_source_root_keys) {
                continue;
            }
            let Some(document) = self.documents.get(uri.as_str()).cloned() else {
                continue;
            };
            let Some(file_id) = self.file_ids.get(uri.as_str()).copied() else {
                continue;
            };
            documents.insert(uri.clone(), document);
            if self.detached_document_uris.contains(uri.as_str()) {
                detached_document_uris.insert(uri.clone());
            }
            file_ids.insert(uri.clone(), file_id);
            file_path_keys.insert(path_lookup_key(uri), file_id);
            file_uris.insert(file_id, uri.clone());
            if let Some(revision) = self.file_revisions.get(&file_id).copied() {
                file_revisions.insert(file_id, revision);
            }
            if let Some(kept_membership) =
                self.retained_isolated_local_membership(file_id, &kept_source_set_ids)
            {
                file_source_sets.insert(file_id, kept_membership);
            }
            if let Some(detached) =
                self.retained_isolated_detached_document(file_id, &kept_source_root_keys)
            {
                detached_source_root_documents.insert(file_id, detached);
            }
        }

        Session {
            documents,
            detached_document_uris,
            detached_source_root_documents,
            source_sets,
            file_ids,
            file_path_keys,
            file_uris,
            source_set_keys,
            file_source_sets,
            source_set_signature_overrides: IndexMap::new(),
            file_revisions,
            next_file_id: self.next_file_id,
            next_source_set_id: self.next_source_set_id,
            current_revision: self.current_revision,
            next_revision: self.next_revision,
            source_root_indexing: SourceRootIndexingCoordinatorState::default(),
            query_state: SessionQueryState {
                ast: AstQueryState::default(),
                resolved: self.query_state.resolved.clone(),
                flat: self.query_state.flat.clone(),
                dae: self.query_state.dae.clone(),
            },
            snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            lightweight_snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            workspace_symbol_snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
        }
    }

    fn isolated_local_clone_source_roots(&self) -> IsolatedLocalSourceRootCloneState {
        let mut kept_source_root_keys = IndexSet::new();
        let mut kept_source_set_ids = IndexSet::new();
        let mut source_sets = IndexMap::new();
        let mut source_set_keys = IndexMap::new();

        for (key, record) in &self.source_sets {
            if record.kind.is_non_workspace_root() {
                continue;
            }
            kept_source_root_keys.insert(key.clone());
            kept_source_set_ids.insert(record.id);
            source_set_keys.insert(record.id, key.clone());
            source_sets.insert(key.clone(), record.clone());
        }

        IsolatedLocalSourceRootCloneState {
            kept_source_root_keys,
            kept_source_set_ids,
            source_sets,
            source_set_keys,
        }
    }

    fn should_keep_isolated_local_document(
        &self,
        uri: &str,
        keep_uris: &IndexSet<String>,
        kept_source_root_keys: &IndexSet<String>,
    ) -> bool {
        keep_uris.contains(uri)
            || self
                .source_root_backing_keys_for_uri(uri)
                .iter()
                .any(|key| kept_source_root_keys.contains(key))
    }

    fn retained_isolated_local_membership(
        &self,
        file_id: FileId,
        kept_source_set_ids: &IndexSet<SourceSetId>,
    ) -> Option<IndexSet<SourceSetId>> {
        let kept_membership = self
            .file_source_sets
            .get(&file_id)?
            .iter()
            .copied()
            .filter(|source_set_id| kept_source_set_ids.contains(source_set_id))
            .collect::<IndexSet<_>>();
        (!kept_membership.is_empty()).then_some(kept_membership)
    }

    fn retained_isolated_detached_document(
        &self,
        file_id: FileId,
        kept_source_root_keys: &IndexSet<String>,
    ) -> Option<DetachedSourceRootDocument> {
        let detached = self.detached_source_root_documents.get(&file_id)?;
        let kept_keys = detached
            .source_root_keys
            .iter()
            .filter(|key| kept_source_root_keys.contains(*key))
            .cloned()
            .collect::<IndexSet<_>>();
        (!kept_keys.is_empty()).then_some(DetachedSourceRootDocument {
            document: detached.document.clone(),
            source_root_keys: kept_keys,
        })
    }

    pub(crate) fn sync_query_state_from_snapshots(&mut self) {
        self.sync_ast_query_state_from_snapshot(self.snapshot_cache.clone());
        self.sync_ast_query_state_from_snapshot(self.lightweight_snapshot_cache.clone());
        self.sync_ast_query_state_from_snapshot(self.workspace_symbol_snapshot_cache.clone());
    }

    /// Capture one immutable analysis snapshot from the current host revision.
    pub fn snapshot(&self) -> SessionSnapshot {
        self.cached_snapshot(&self.snapshot_cache, || {
            self.build_snapshot(self.query_state.ast.clone())
        })
    }

    /// Capture one lightweight snapshot for local document-scoped IDE queries.
    pub fn lightweight_snapshot(&self) -> SessionSnapshot {
        self.cached_snapshot(&self.lightweight_snapshot_cache, || {
            self.build_snapshot(AstQueryState {
                package_def_map: self.query_state.ast.package_def_map.clone(),
                ..AstQueryState::default()
            })
        })
    }

    /// Capture a workspace-symbol snapshot without cloning unrelated AST-tier caches.
    pub fn workspace_symbol_snapshot(&self) -> SessionSnapshot {
        self.workspace_symbol_snapshot_with_timing().0
    }

    /// Capture a workspace-symbol snapshot and report the internal build breakdown.
    pub fn workspace_symbol_snapshot_with_timing(
        &self,
    ) -> (SessionSnapshot, WorkspaceSymbolSnapshotTiming) {
        let Ok(mut shared_snapshot) = self.workspace_symbol_snapshot_cache.lock() else {
            return self.build_workspace_symbol_snapshot_with_timing();
        };
        if shared_snapshot.revision == self.current_revision
            && let Some(snapshot) = shared_snapshot.snapshot.as_ref()
        {
            return (
                snapshot.clone(),
                WorkspaceSymbolSnapshotTiming {
                    cache_hit: true,
                    ..WorkspaceSymbolSnapshotTiming::default()
                },
            );
        }
        let (snapshot, timing) = self.build_workspace_symbol_snapshot_with_timing();
        shared_snapshot.revision = self.current_revision;
        shared_snapshot.snapshot = Some(snapshot.clone());
        (snapshot, timing)
    }

    fn sync_ast_query_state_from_snapshot(&mut self, cache: Arc<Mutex<SharedSessionSnapshot>>) {
        let Ok(shared_snapshot) = cache.lock() else {
            return;
        };
        if shared_snapshot.revision != self.current_revision {
            return;
        }
        let Some(snapshot) = shared_snapshot.snapshot.as_ref() else {
            return;
        };
        let Ok(snapshot_session) = snapshot.session.lock() else {
            return;
        };
        // Host mutations only need the AST-tier warmth discovered by read snapshots.
        // Resolved/model artifacts remain owned by the host mutation/compile path.
        self.query_state
            .ast
            .merge_from(&snapshot_session.query_state.ast);
    }

    fn cached_snapshot(
        &self,
        cache: &Arc<Mutex<SharedSessionSnapshot>>,
        build: impl FnOnce() -> SessionSnapshot,
    ) -> SessionSnapshot {
        let Ok(mut shared_snapshot) = cache.lock() else {
            return build();
        };
        if shared_snapshot.revision == self.current_revision
            && let Some(snapshot) = shared_snapshot.snapshot.as_ref()
        {
            return snapshot.clone();
        }
        let snapshot = build();
        shared_snapshot.revision = self.current_revision;
        shared_snapshot.snapshot = Some(snapshot.clone());
        snapshot
    }

    fn build_snapshot(&self, ast_query_state: AstQueryState) -> SessionSnapshot {
        let snapshot = Session {
            documents: self.documents.clone(),
            detached_document_uris: self.detached_document_uris.clone(),
            detached_source_root_documents: self.detached_source_root_documents.clone(),
            source_sets: self.source_sets.clone(),
            file_ids: self.file_ids.clone(),
            file_path_keys: self.file_path_keys.clone(),
            file_uris: self.file_uris.clone(),
            source_set_keys: self.source_set_keys.clone(),
            file_source_sets: self.file_source_sets.clone(),
            source_set_signature_overrides: IndexMap::new(),
            file_revisions: self.file_revisions.clone(),
            next_file_id: self.next_file_id,
            next_source_set_id: self.next_source_set_id,
            current_revision: self.current_revision,
            next_revision: self.next_revision,
            source_root_indexing: SourceRootIndexingCoordinatorState::default(),
            query_state: SessionQueryState {
                ast: ast_query_state,
                ..SessionQueryState::default()
            },
            snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            lightweight_snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            workspace_symbol_snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
        };
        SessionSnapshot {
            session: Arc::new(Mutex::new(snapshot)),
        }
    }

    fn build_workspace_symbol_snapshot_with_timing(
        &self,
    ) -> (SessionSnapshot, WorkspaceSymbolSnapshotTiming) {
        let mut timing = WorkspaceSymbolSnapshotTiming::default();
        let validation_started = Instant::now();
        let source_set_caches_are_current = self.workspace_symbol_source_set_caches_are_current();
        timing.source_set_validation_ms = validation_started.elapsed().as_millis() as u64;
        if !source_set_caches_are_current {
            return self.build_workspace_symbol_rebuild_snapshot_with_timing(timing);
        }

        let detached_documents_started = Instant::now();
        let detached_documents = self
            .detached_document_uris
            .iter()
            .filter_map(|uri| {
                self.documents
                    .get(uri)
                    .map(|doc| (uri.clone(), doc.clone()))
            })
            .collect();
        timing.detached_documents_ms = detached_documents_started.elapsed().as_millis() as u64;

        let detached_file_ids_started = Instant::now();
        let detached_file_ids: IndexMap<String, FileId> = self
            .detached_document_uris
            .iter()
            .filter_map(|uri| {
                self.file_ids
                    .get(uri)
                    .copied()
                    .map(|file_id| (uri.clone(), file_id))
            })
            .collect();
        timing.detached_file_ids_ms = detached_file_ids_started.elapsed().as_millis() as u64;
        let detached_file_id_set = detached_file_ids.values().copied().collect::<IndexSet<_>>();

        let source_set_signatures_started = Instant::now();
        let source_set_signature_overrides = self
            .source_sets
            .values()
            .filter_map(|record| {
                self.source_set_query_signature(record.id)
                    .map(|signature| (record.id, signature))
            })
            .collect();
        timing.source_set_signatures_ms =
            source_set_signatures_started.elapsed().as_millis() as u64;

        let ast_state_started = Instant::now();
        let ast_query_state = self
            .query_state
            .ast
            .workspace_symbol_snapshot_state_for_detached(&detached_file_id_set);
        timing.ast_state_ms = ast_state_started.elapsed().as_millis() as u64;

        let session_assemble_started = Instant::now();
        let snapshot = Session {
            documents: detached_documents,
            detached_document_uris: self.detached_document_uris.clone(),
            detached_source_root_documents: IndexMap::new(),
            source_sets: IndexMap::new(),
            file_ids: detached_file_ids,
            file_path_keys: IndexMap::new(),
            file_uris: IndexMap::new(),
            source_set_keys: IndexMap::new(),
            file_source_sets: IndexMap::new(),
            source_set_signature_overrides,
            file_revisions: IndexMap::new(),
            next_file_id: self.next_file_id,
            next_source_set_id: self.next_source_set_id,
            current_revision: self.current_revision,
            next_revision: self.next_revision,
            source_root_indexing: SourceRootIndexingCoordinatorState::default(),
            query_state: SessionQueryState {
                ast: ast_query_state,
                ..SessionQueryState::default()
            },
            snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            lightweight_snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            workspace_symbol_snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
        };
        timing.session_assemble_ms = session_assemble_started.elapsed().as_millis() as u64;
        (
            SessionSnapshot {
                session: Arc::new(Mutex::new(snapshot)),
            },
            timing,
        )
    }

    fn build_workspace_symbol_rebuild_snapshot_with_timing(
        &self,
        mut timing: WorkspaceSymbolSnapshotTiming,
    ) -> (SessionSnapshot, WorkspaceSymbolSnapshotTiming) {
        timing.used_source_set_rebuild_snapshot = true;
        let detached_file_ids = self
            .detached_document_uris
            .iter()
            .filter_map(|uri| self.file_ids.get(uri).copied())
            .collect::<IndexSet<_>>();

        let source_set_documents_started = Instant::now();
        let documents = self.documents.clone();
        timing.source_set_documents_ms = source_set_documents_started.elapsed().as_millis() as u64;

        let source_set_signatures_started = Instant::now();
        let source_set_signature_overrides = self
            .source_sets
            .values()
            .filter_map(|record| {
                self.source_set_query_signature(record.id)
                    .map(|signature| (record.id, signature))
            })
            .collect();
        timing.source_set_signatures_ms =
            source_set_signatures_started.elapsed().as_millis() as u64;

        let ast_state_started = Instant::now();
        let ast_query_state = self
            .query_state
            .ast
            .workspace_symbol_rebuild_snapshot_state(&detached_file_ids);
        timing.ast_state_ms = ast_state_started.elapsed().as_millis() as u64;

        let session_assemble_started = Instant::now();
        let snapshot = Session {
            documents,
            detached_document_uris: self.detached_document_uris.clone(),
            detached_source_root_documents: IndexMap::new(),
            source_sets: self.source_sets.clone(),
            file_ids: self.file_ids.clone(),
            file_path_keys: IndexMap::new(),
            file_uris: IndexMap::new(),
            source_set_keys: IndexMap::new(),
            file_source_sets: IndexMap::new(),
            source_set_signature_overrides,
            file_revisions: IndexMap::new(),
            next_file_id: self.next_file_id,
            next_source_set_id: self.next_source_set_id,
            current_revision: self.current_revision,
            next_revision: self.next_revision,
            source_root_indexing: SourceRootIndexingCoordinatorState::default(),
            query_state: SessionQueryState {
                ast: ast_query_state,
                ..SessionQueryState::default()
            },
            snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            lightweight_snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
            workspace_symbol_snapshot_cache: Arc::new(Mutex::new(SharedSessionSnapshot::default())),
        };
        timing.session_assemble_ms = session_assemble_started.elapsed().as_millis() as u64;
        (
            SessionSnapshot {
                session: Arc::new(Mutex::new(snapshot)),
            },
            timing,
        )
    }

    fn workspace_symbol_source_set_caches_are_current(&self) -> bool {
        let Some(cache) = self.query_state.ast.workspace_symbol_query_cache.as_ref() else {
            return self.source_sets.is_empty();
        };
        self.source_sets.values().all(|record| {
            self.source_set_query_signature(record.id)
                .is_some_and(|signature| {
                    cache
                        .source_set_caches
                        .get(&record.id)
                        .is_some_and(|entry| entry.signature == signature)
                })
        })
    }
}

impl SessionSnapshot {
    fn with_session<T>(&self, f: impl FnOnce(&mut Session) -> T) -> T {
        let mut session = self
            .session
            .lock()
            .expect("session snapshot lock should not be poisoned");
        f(&mut session)
    }

    fn with_session_ref<T>(&self, f: impl FnOnce(&Session) -> T) -> T {
        let session = self
            .session
            .lock()
            .expect("session snapshot lock should not be poisoned");
        f(&session)
    }

    pub fn get_document(&self, uri: &str) -> Option<Document> {
        self.with_session_ref(|session| session.get_document(uri).cloned())
    }

    pub fn document_uris(&self) -> Vec<String> {
        self.with_session_ref(|session| {
            session
                .document_uris()
                .into_iter()
                .map(ToString::to_string)
                .collect()
        })
    }

    pub fn dirty_source_root_keys(&self) -> Vec<String> {
        self.with_session_ref(Session::dirty_source_root_keys)
    }

    pub fn dirty_non_workspace_source_root_keys(&self) -> Vec<String> {
        self.with_session_ref(Session::dirty_non_workspace_source_root_keys)
    }

    pub fn source_root_kind(&self, source_root_key: &str) -> Option<SourceRootKind> {
        self.with_session_ref(|session| session.source_root_kind(source_root_key))
    }

    pub fn source_root_durability(&self, source_root_key: &str) -> Option<SourceRootDurability> {
        self.with_session_ref(|session| session.source_root_durability(source_root_key))
    }

    pub fn is_source_root_backed_document(&self, uri: &str) -> bool {
        self.with_session_ref(|session| session.is_source_root_backed_document(uri))
    }

    pub fn is_non_workspace_source_root_document(&self, uri: &str) -> bool {
        self.with_session_ref(|session| session.is_non_workspace_source_root_document(uri))
    }

    pub fn document_needs_source_root_read_prewarm(&self, uri: &str) -> bool {
        self.with_session_ref(|session| session.document_needs_source_root_read_prewarm(uri))
    }

    pub fn needs_source_root_read_prewarm(&self) -> bool {
        self.with_session_ref(Session::needs_source_root_read_prewarm)
    }

    pub fn namespace_index_query(&self, prefix: &str) -> Result<Vec<(String, String, bool)>> {
        self.with_session(|session| session.namespace_index_query(prefix))
    }

    pub fn namespace_class_names_cached(&self) -> Vec<String> {
        self.with_session_ref(Session::namespace_class_names_cached)
    }

    pub fn all_class_names_cached(&self) -> Vec<String> {
        self.with_session_ref(Session::all_class_names_cached)
    }

    pub fn namespace_children_cached(&self, prefix: &str) -> Vec<(String, String, bool)> {
        self.with_session_ref(|session| session.namespace_children_cached(prefix))
    }

    pub fn namespace_fingerprint_cached(&self, prefix: &str) -> Option<String> {
        self.with_session_ref(|session| session.namespace_fingerprint_cached(prefix))
    }

    pub fn file_item_index_query(&self, uri: &str) -> Vec<WorkspaceSymbol> {
        self.with_session(|session| session.file_item_index_query(uri))
    }

    pub fn navigation_references_query(
        &self,
        uri: &str,
        line: u32,
        character: u32,
        include_declaration: bool,
    ) -> Option<Vec<(String, ast::Location)>> {
        self.with_session(|session| {
            session.navigation_references_query(uri, line, character, include_declaration)
        })
    }

    pub fn navigation_prepare_rename_query(
        &self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<ast::Location> {
        self.with_session(|session| session.navigation_prepare_rename_query(uri, line, character))
    }

    pub fn navigation_rename_locations_query(
        &self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<Vec<(String, ast::Location)>> {
        self.with_session(|session| session.navigation_rename_locations_query(uri, line, character))
    }

    pub fn navigation_class_target_query(
        &self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<NavigationClassTargetInfo> {
        self.with_session(|session| session.navigation_class_target_query(uri, line, character))
    }

    pub fn class_type_resolution_candidates_query(
        &self,
        uri: &str,
        qualified_name: &str,
        raw_name: &str,
    ) -> Vec<String> {
        self.with_session(|session| {
            session.class_type_resolution_candidates_query(uri, qualified_name, raw_name)
        })
    }

    pub fn class_component_type_query(
        &self,
        uri: &str,
        qualified_name: &str,
        component_name: &str,
    ) -> Option<String> {
        self.with_session(|session| {
            session.class_component_type_query(uri, qualified_name, component_name)
        })
    }

    pub fn class_type_resolution_candidates_in_class_query(
        &self,
        class_name: &str,
        raw_name: &str,
    ) -> Vec<String> {
        self.with_session(|session| {
            session.class_type_resolution_candidates_in_class_query(class_name, raw_name)
        })
    }

    pub fn class_component_member_info_query(
        &self,
        class_name: &str,
        component_name: &str,
    ) -> Option<(String, String)> {
        self.with_session(|session| {
            session.class_component_member_info_query(class_name, component_name)
        })
    }

    pub fn class_local_completion_items_query(
        &self,
        uri: &str,
        qualified_name: &str,
    ) -> Vec<ClassLocalCompletionItem> {
        self.with_session(|session| session.class_local_completion_items_query(uri, qualified_name))
    }

    pub fn local_component_info_query(
        &self,
        uri: &str,
        line: u32,
        character: u32,
    ) -> Option<LocalComponentInfo> {
        self.with_session(|session| session.local_component_info_query(uri, line, character))
    }

    pub fn enclosing_class_qualified_name_query(&self, uri: &str, line: u32) -> Option<String> {
        self.with_session(|session| session.enclosing_class_qualified_name_query(uri, line))
    }

    pub fn class_lookup_query(&self, class_name: &str) -> Option<String> {
        self.with_session(|session| session.class_lookup_query(class_name))
    }

    pub fn class_component_members_query(&self, class_name: &str) -> Vec<(String, String)> {
        self.with_session(|session| session.class_component_members_query(class_name))
    }

    pub fn prewarm_document_ide_queries(&self, uri: &str) {
        self.with_session(|session| {
            let class_names = session
                .file_summary_query(uri)
                .map(|summary| {
                    summary
                        .iter()
                        .map(|(item_key, _)| item_key.qualified_name())
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            let _ = session.class_interface_index_query(uri);
            let _ = session.class_body_semantics_query(uri);
            for class_name in class_names {
                let _ = session.class_component_members_query(&class_name);
            }
        });
    }

    pub fn prewarm_document_read_queries(&self, uri: &str) {
        if self.document_needs_source_root_read_prewarm(uri) {
            self.prewarm_source_root_read_queries();
            return;
        }
        self.prewarm_document_ide_queries(uri);
    }

    pub fn prewarm_source_root_read_queries(&self) {
        if !self.needs_source_root_read_prewarm() {
            return;
        }
        self.prewarm_source_root_namespace_queries();
        self.prewarm_workspace_symbol_queries();
    }

    pub fn prewarm_source_root_namespace_queries(&self) {
        if !self.needs_source_root_read_prewarm() {
            return;
        }
        let _ = self.namespace_index_query("");
    }

    pub fn document_symbol_query(&self, uri: &str) -> Option<Vec<DocumentSymbol>> {
        self.with_session(|session| session.document_symbol_query(uri))
    }

    pub fn prewarm_workspace_symbol_queries(&self) {
        if !self.needs_source_root_read_prewarm() {
            return;
        }
        self.with_session(Session::prewarm_workspace_symbol_query_caches);
    }

    pub fn workspace_symbol_query(&self, query: &str) -> Vec<WorkspaceSymbol> {
        self.with_session(|session| session.workspace_symbol_query(query))
    }

    pub fn semantic_diagnostics_query(
        &self,
        model_name: &str,
        mode: SemanticDiagnosticsMode,
    ) -> ModelDiagnostics {
        self.with_session(|session| session.semantic_diagnostics_query(model_name, mode))
    }
}
