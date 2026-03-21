use super::package_def_map::{leaf_dirty_prefixes, qualified_name_in_subtree};
use super::*;

impl Session {
    pub(super) fn mark_source_root_graph_changed(&mut self) {
        self.source_root_indexing.state_epoch =
            self.source_root_indexing.state_epoch.saturating_add(1);
        self.source_root_indexing.read_prewarm_session_revision = None;
    }

    fn source_root_refresh_candidate_uris(&self, source_root_key: &str) -> IndexSet<String> {
        let mut uris = self
            .source_sets
            .get(source_root_key)
            .map(|record| record.uris.clone())
            .unwrap_or_default();
        for detached in self.detached_source_root_documents.values() {
            if detached.source_root_keys.contains(source_root_key) {
                uris.insert(detached.document.uri.clone());
            }
        }
        uris
    }

    fn source_root_refresh_document(&self, source_root_key: &str, uri: &str) -> Option<&Document> {
        self.documents.get(uri).map(AsRef::as_ref).or_else(|| {
            self.file_id_for_uri(uri)
                .and_then(|file_id| self.detached_source_root_documents.get(&file_id))
                .filter(|detached| detached.source_root_keys.contains(source_root_key))
                .map(|detached| detached.document.as_ref())
        })
    }

    fn file_summary_matches_dirty_prefixes(
        summary: &FileSummary,
        dirty_prefixes: &[String],
    ) -> Vec<String> {
        dirty_prefixes
            .iter()
            .filter(|prefix| {
                summary.iter().any(|(item_key, _)| {
                    qualified_name_in_subtree(&item_key.qualified_name(), prefix)
                })
            })
            .cloned()
            .collect()
    }

    fn source_root_load_activity_kind(
        cache_status: SourceRootCacheStatus,
    ) -> SourceRootActivityKind {
        match cache_status {
            SourceRootCacheStatus::Hit => SourceRootActivityKind::WarmCacheRestore,
            SourceRootCacheStatus::Miss | SourceRootCacheStatus::Disabled => {
                SourceRootActivityKind::ColdIndexBuild
            }
        }
    }

    pub(super) fn source_set_uris(&self, source_set_key: &str) -> Option<&IndexSet<String>> {
        self.source_sets
            .get(source_set_key)
            .map(|record| &record.uris)
    }

    pub(super) fn detached_source_root_keys_for_uri(&self, uri: &str) -> IndexSet<String> {
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

    pub(super) fn source_root_backing_keys_for_uri(&self, uri: &str) -> IndexSet<String> {
        let mut keys = self.detached_source_root_keys_for_uri(uri);
        if self
            .documents
            .get(uri)
            .is_none_or(|document| document.content.is_empty())
        {
            keys.extend(self.source_set_keys_for_uri(uri));
        }
        keys
    }

    pub(super) fn non_workspace_source_root_keys_for_uri(&self, uri: &str) -> IndexSet<String> {
        self.source_root_backing_keys_for_uri(uri)
            .into_iter()
            .filter(|source_set_key| {
                self.source_sets
                    .get(source_set_key)
                    .is_some_and(|record| record.kind.is_non_workspace_root())
            })
            .collect()
    }

    fn source_set_keys_for_uri(&self, uri: &str) -> IndexSet<String> {
        let mut keys = IndexSet::new();
        let Some(file_id) = self.file_id_for_uri(uri) else {
            return keys;
        };
        for source_set_id in self.source_set_ids_for_file_id(file_id) {
            if let Some(source_set_key) = self.source_set_key_for_id(source_set_id) {
                keys.insert(source_set_key.to_string());
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
        let previous_source_root_path = previous_record
            .as_ref()
            .and_then(|record| record.source_root_path.clone());
        let previous_activity = previous_record
            .as_ref()
            .map(|record| record.activity.clone())
            .unwrap_or_default();
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
                source_root_path: previous_source_root_path,
                uris,
                revision,
                dirty_class_prefixes: IndexSet::new(),
                needs_refresh: false,
                activity: previous_activity,
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

    pub(super) fn set_source_root_path(&mut self, source_root_key: &str, source_root_path: &Path) {
        if let Some(record) = self.source_sets.get_mut(source_root_key) {
            record.source_root_path = Some(source_root_path.display().to_string());
        }
    }

    pub fn begin_source_root_load(
        &mut self,
        source_root_key: &str,
        source_root_path: &Path,
        cache_status: SourceRootCacheStatus,
    ) {
        self.set_source_root_path(source_root_key, source_root_path);
        self.set_source_root_activity_running(
            source_root_key,
            Self::source_root_load_activity_kind(cache_status),
        );
    }

    pub fn complete_source_root_load(
        &mut self,
        source_root_key: &str,
        source_root_path: &Path,
        cache_status: SourceRootCacheStatus,
    ) {
        self.set_source_root_path(source_root_key, source_root_path);
        self.complete_source_root_activity(
            source_root_key,
            Self::source_root_load_activity_kind(cache_status),
            IndexSet::new(),
        );
    }

    pub(super) fn set_source_root_activity_running(
        &mut self,
        source_root_key: &str,
        kind: SourceRootActivityKind,
    ) {
        if let Some(record) = self.source_sets.get_mut(source_root_key) {
            record.activity.current = Some(SourceRootActivityRecord::running(kind));
        }
    }

    pub(super) fn complete_source_root_activity(
        &mut self,
        source_root_key: &str,
        kind: SourceRootActivityKind,
        dirty_class_prefixes: IndexSet<String>,
    ) {
        if let Some(record) = self.source_sets.get_mut(source_root_key) {
            record.activity.current = None;
            record.activity.last_completed = Some(SourceRootActivityRecord::completed(
                kind,
                dirty_class_prefixes,
            ));
        }
    }

    pub(super) fn mark_source_roots_for_refresh(
        &mut self,
        source_root_keys: &IndexSet<String>,
        dirty_class_prefixes: &IndexSet<String>,
    ) {
        for source_root_key in source_root_keys {
            let Some(record) = self.source_sets.get_mut(source_root_key) else {
                continue;
            };
            record
                .dirty_class_prefixes
                .extend(dirty_class_prefixes.iter().cloned());
            record.needs_refresh = !record.dirty_class_prefixes.is_empty();
            if !record.needs_refresh {
                continue;
            }
            record.activity.current = Some(SourceRootActivityRecord::pending_reindex(
                &record.dirty_class_prefixes,
            ));
        }
    }

    pub(super) fn clear_source_root_refresh(&mut self, source_set_key: &str) {
        if let Some(record) = self.source_sets.get_mut(source_set_key) {
            let dirty_class_prefixes = mem::take(&mut record.dirty_class_prefixes);
            record.needs_refresh = false;
            record.activity.current = None;
            if !dirty_class_prefixes.is_empty() {
                record.activity.last_completed = Some(SourceRootActivityRecord::completed(
                    SourceRootActivityKind::SubtreeReindex,
                    dirty_class_prefixes,
                ));
            }
        }
    }

    /// Return all source-root keys that still need a refresh.
    pub fn dirty_source_root_keys(&self) -> Vec<String> {
        self.source_sets
            .iter()
            .filter(|(_, record)| record.needs_refresh)
            .map(|(key, _)| key.clone())
            .collect()
    }

    pub fn dirty_source_root_class_prefixes(&self, source_root_key: &str) -> Vec<String> {
        self.source_sets
            .get(source_root_key)
            .map(|record| record.dirty_class_prefixes.iter().cloned().collect())
            .unwrap_or_default()
    }

    pub fn source_root_status(&self, source_root_key: &str) -> Option<SourceRootStatusSnapshot> {
        let record = self.source_sets.get(source_root_key)?;
        Some(SourceRootStatusSnapshot {
            source_root_key: source_root_key.to_string(),
            source_root_path: record.source_root_path.clone(),
            current: record
                .activity
                .current
                .as_ref()
                .map(|activity| activity.snapshot()),
            last_completed: record
                .activity
                .last_completed
                .as_ref()
                .map(|activity| activity.snapshot()),
        })
    }

    pub fn source_root_statuses(&self) -> Vec<SourceRootStatusSnapshot> {
        self.source_sets
            .keys()
            .filter_map(|source_root_key| self.source_root_status(source_root_key))
            .collect()
    }

    pub fn source_root_state_epoch(&self) -> u64 {
        self.source_root_indexing.state_epoch
    }

    pub fn loaded_source_root_path_keys(&self) -> HashSet<String> {
        self.source_root_indexing.loaded_path_keys.clone()
    }

    pub fn is_source_root_path_loaded(&self, path_key: &str) -> bool {
        self.source_root_indexing
            .loaded_path_keys
            .contains(path_key)
    }

    pub fn source_root_load_reservation_epoch(&self, path_key: &str) -> Option<u64> {
        self.source_root_indexing
            .loading_path_keys
            .get(path_key)
            .copied()
    }

    pub fn reserve_source_root_load(&mut self, path_key: &str, expected_epoch: u64) -> bool {
        if expected_epoch != self.source_root_state_epoch()
            || self.is_source_root_path_loaded(path_key)
            || self
                .source_root_indexing
                .loading_path_keys
                .contains_key(path_key)
        {
            return false;
        }
        self.source_root_indexing
            .loading_path_keys
            .insert(path_key.to_string(), expected_epoch);
        true
    }

    pub fn cancel_source_root_load(&mut self, path_key: &str, reservation_epoch: u64) {
        if self
            .source_root_indexing
            .loading_path_keys
            .get(path_key)
            .is_some_and(|owner_epoch| *owner_epoch == reservation_epoch)
        {
            self.source_root_indexing.loading_path_keys.remove(path_key);
        }
    }

    pub fn apply_parsed_source_root_if_current(
        &mut self,
        source_root_key: &str,
        load: ParsedSourceRootLoad<'_>,
    ) -> Option<(usize, Option<SourceRootStatusSnapshot>)> {
        let ParsedSourceRootLoad {
            source_root_kind,
            source_root_path,
            cache_status,
            path_key,
            current_document_path,
            documents,
            expected_epoch,
        } = load;
        let state_epoch_before_apply = self.source_root_state_epoch();
        if expected_epoch != state_epoch_before_apply || self.is_source_root_path_loaded(path_key) {
            self.cancel_source_root_load(path_key, expected_epoch);
            return None;
        }

        self.begin_source_root_load(source_root_key, source_root_path, cache_status);
        let inserted = self.replace_parsed_source_set(
            source_root_key,
            source_root_kind,
            documents,
            current_document_path,
        );
        self.complete_source_root_load(source_root_key, source_root_path, cache_status);
        self.source_root_indexing
            .loaded_path_keys
            .insert(path_key.to_string());
        if self.source_root_state_epoch() == state_epoch_before_apply {
            self.mark_source_root_graph_changed();
        }
        self.cancel_source_root_load(path_key, expected_epoch);
        Some((inserted, self.source_root_status(source_root_key)))
    }

    pub fn reset_to_open_documents(&mut self) {
        let open_documents = self
            .documents
            .values()
            .filter(|document| !document.content.is_empty())
            .map(|document| (document.uri.clone(), document.content.clone()))
            .collect::<Vec<_>>();
        let next_state_epoch = self.source_root_state_epoch().saturating_add(1);

        let mut rebuilt = Session::new(SessionConfig::default());
        let mut change = SessionChange::default();
        for (uri, content) in open_documents {
            change.set_file_text(uri, content);
        }
        if !change.is_empty() {
            rebuilt.apply_change(change);
        }
        rebuilt.source_root_indexing.state_epoch = next_state_epoch;
        *self = rebuilt;
    }

    pub fn begin_source_root_read_prewarm(&mut self, session_revision: u64) -> bool {
        if self.source_root_indexing.read_prewarm_session_revision == Some(session_revision) {
            return false;
        }
        self.source_root_indexing.read_prewarm_session_revision = Some(session_revision);
        true
    }

    pub fn finish_source_root_read_prewarm(&mut self, session_revision: u64) {
        if self.source_root_indexing.read_prewarm_session_revision == Some(session_revision) {
            self.source_root_indexing.read_prewarm_session_revision = None;
        }
    }

    pub fn source_root_read_prewarm_is_pending(&self, session_revision: u64) -> bool {
        self.source_root_indexing.read_prewarm_session_revision == Some(session_revision)
    }

    pub fn source_root_refresh_plan(&self, source_root_key: &str) -> Option<SourceRootRefreshPlan> {
        let record = self.source_sets.get(source_root_key)?;
        if !record.needs_refresh || record.dirty_class_prefixes.is_empty() {
            return None;
        }

        let refresh_class_prefixes = leaf_dirty_prefixes(&record.dirty_class_prefixes);
        let candidate_uris = self.source_root_refresh_candidate_uris(source_root_key);
        let mut affected_uris = IndexSet::new();
        let mut matched_class_prefixes = IndexSet::new();
        let mut missing_document_summaries = false;

        for uri in candidate_uris {
            let Some(document) = self.source_root_refresh_document(source_root_key, &uri) else {
                continue;
            };
            let Some(parsed) = document.parsed() else {
                missing_document_summaries = true;
                continue;
            };
            let summary = FileSummary::from_definition(FileId::default(), parsed);
            let matched_prefixes =
                Self::file_summary_matches_dirty_prefixes(&summary, &refresh_class_prefixes);
            if matched_prefixes.is_empty() {
                continue;
            }
            affected_uris.insert(uri);
            matched_class_prefixes.extend(matched_prefixes);
        }

        let unmatched_class_prefixes = refresh_class_prefixes
            .iter()
            .filter(|prefix| !matched_class_prefixes.contains(*prefix))
            .cloned()
            .collect::<Vec<_>>();

        Some(SourceRootRefreshPlan {
            source_root_key: source_root_key.to_string(),
            source_root_path: record.source_root_path.clone(),
            dirty_class_prefixes: record.dirty_class_prefixes.iter().cloned().collect(),
            refresh_class_prefixes,
            affected_uris: affected_uris.into_iter().collect(),
            unmatched_class_prefixes: unmatched_class_prefixes.clone(),
            rebuild_package_membership: true,
            full_root_fallback: missing_document_summaries || !unmatched_class_prefixes.is_empty(),
        })
    }

    pub fn apply_source_root_refresh_plan(
        &mut self,
        source_root_key: &str,
    ) -> Option<SourceRootRefreshPlan> {
        let plan = self.source_root_refresh_plan(source_root_key)?;
        if plan.full_root_fallback {
            return Some(plan);
        }

        let source_set_id = self
            .source_sets
            .get(source_root_key)
            .map(|record| record.id);
        if plan.rebuild_package_membership
            && let Some(source_set_id) = source_set_id
        {
            let _ = self.source_set_package_def_map_query(source_set_id);
        }
        self.refresh_source_root_namespace_cache();
        self.clear_source_root_refresh(source_root_key);
        self.mark_source_root_graph_changed();
        Some(plan)
    }

    /// Return all source-root keys that share a stable caller prefix.
    pub fn source_root_keys_with_prefix(&self, prefix: &str) -> Vec<String> {
        self.source_sets
            .keys()
            .filter(|key| key.starts_with(prefix))
            .cloned()
            .collect()
    }

    /// Replace a family of parsed source roots by partitioning definitions into
    /// top-level package roots plus a loose-file root bucket.
    ///
    /// Frontends should parse text into stored definitions and let the session
    /// own source-root partitioning so all package-like sources share the same
    /// cache and invalidation rules.
    pub fn sync_partitioned_source_root_family(
        &mut self,
        source_root_prefix: &str,
        kind: SourceRootKind,
        definitions: Vec<(String, ast::StoredDefinition)>,
        cache_root: Option<&Path>,
        exclude_uri: Option<&str>,
    ) -> usize {
        let top_level_package_roots = top_level_package_roots_for_definitions(&definitions);
        let mut grouped_definitions: IndexMap<String, Vec<(String, ast::StoredDefinition)>> =
            IndexMap::new();
        for (uri, parsed) in definitions {
            let package_root = partitioned_package_root_for_uri(&uri, &top_level_package_roots);
            grouped_definitions
                .entry(package_root)
                .or_default()
                .push((uri, parsed));
        }

        let mut stale_root_keys: IndexSet<String> = self
            .source_root_keys_with_prefix(source_root_prefix)
            .into_iter()
            .collect();
        if self.source_root_kind(source_root_prefix).is_some() {
            stale_root_keys.insert(source_root_prefix.to_string());
        }

        let mut inserted_count = 0usize;
        for (package_root, definitions) in grouped_definitions {
            let source_root_key =
                partitioned_source_root_key(source_root_prefix, package_root.as_str());
            stale_root_keys.shift_remove(&source_root_key);
            inserted_count +=
                self.replace_parsed_source_set(&source_root_key, kind, definitions, exclude_uri);
            if let Some(cache_root) = cache_root {
                let _ = self.sync_source_root_semantic_summary_cache(
                    &source_root_key,
                    &partitioned_source_root_path(&package_root),
                    Some(cache_root),
                );
            }
        }

        for stale_root_key in stale_root_keys {
            let _ = self.replace_parsed_source_set(&stale_root_key, kind, Vec::new(), exclude_uri);
        }

        inserted_count
    }

    /// Return dirty source-root keys managed outside the active workspace.
    pub fn dirty_non_workspace_source_root_keys(&self) -> Vec<String> {
        self.dirty_source_root_keys()
            .into_iter()
            .filter(|source_root_key| {
                self.source_sets
                    .get(source_root_key)
                    .is_some_and(|record| record.kind.is_non_workspace_root())
            })
            .collect()
    }

    pub fn source_root_kind(&self, source_root_key: &str) -> Option<SourceRootKind> {
        self.source_sets
            .get(source_root_key)
            .map(|record| record.kind)
    }

    pub fn source_root_durability(&self, source_root_key: &str) -> Option<SourceRootDurability> {
        self.source_sets
            .get(source_root_key)
            .map(|record| record.durability)
    }

    /// Return whether a URI is currently backed by any parsed source root.
    ///
    /// This is the generic source graph view used by compile/simulation
    /// isolation and other semantics.
    pub fn is_source_root_backed_document(&self, uri: &str) -> bool {
        !self.source_root_backing_keys_for_uri(uri).is_empty()
    }

    /// Return whether a URI is currently backed by a non-workspace source root.
    ///
    /// This is intentionally narrower than the general source-root-backed
    /// document concept used for lookup and refresh behavior.
    pub fn is_non_workspace_source_root_document(&self, uri: &str) -> bool {
        !self.non_workspace_source_root_keys_for_uri(uri).is_empty()
    }

    /// Return whether a document should use source-root read-query prewarm.
    ///
    /// This keeps client scheduling generic while leaving the source-root
    /// ownership decision inside the session.
    pub fn document_needs_source_root_read_prewarm(&self, uri: &str) -> bool {
        self.is_non_workspace_source_root_document(uri)
    }

    /// Return whether the session currently contains any non-workspace source
    /// roots that benefit from source-root-wide read-query prewarm.
    pub fn needs_source_root_read_prewarm(&self) -> bool {
        self.source_sets
            .values()
            .any(|record| record.kind.is_non_workspace_root())
    }

    pub(super) fn cache_detached_source_root_document(
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
        let allow_live_overlay_detach = record.kind.is_non_workspace_root();
        record
            .uris
            .iter()
            .find(|candidate| {
                self.file_id_for_uri(candidate)
                    .is_some_and(|id| id == file_id)
                    && (allow_live_overlay_detach
                        || self
                            .documents
                            .get(candidate.as_str())
                            .is_some_and(|document| document.content.is_empty()))
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

fn top_level_package_roots_for_definitions(
    definitions: &[(String, ast::StoredDefinition)],
) -> Vec<String> {
    let mut package_dirs = definitions
        .iter()
        .filter_map(|(uri, _)| package_directory(uri))
        .collect::<Vec<_>>();
    package_dirs.sort_by_key(String::len);
    let mut top_level_roots: Vec<String> = Vec::new();
    'candidate: for dir in package_dirs {
        for existing in &top_level_roots {
            if dir == *existing || path_is_within_root(&dir, existing) {
                continue 'candidate;
            }
        }
        top_level_roots.push(dir);
    }
    top_level_roots
}

fn partitioned_package_root_for_uri(uri: &str, package_roots: &[String]) -> String {
    let normalized = normalize_source_root_uri_path(uri);
    for root in package_roots {
        if normalized == format!("{root}/package.mo") || path_is_within_root(&normalized, root) {
            return root.clone();
        }
    }
    String::new()
}

fn partitioned_source_root_key(source_root_prefix: &str, package_root: &str) -> String {
    if package_root.is_empty() {
        format!("{source_root_prefix}::root")
    } else {
        format!("{source_root_prefix}::{package_root}")
    }
}

fn partitioned_source_root_path(package_root: &str) -> PathBuf {
    if package_root.is_empty() {
        PathBuf::from("workspace-root")
    } else {
        PathBuf::from(package_root)
    }
}

fn package_directory(uri: &str) -> Option<String> {
    let normalized = normalize_source_root_uri_path(uri);
    if !normalized.ends_with("/package.mo") && normalized != "package.mo" {
        return None;
    }
    normalized
        .rfind('/')
        .map(|index| normalized[..index].to_string())
}

fn normalize_source_root_uri_path(uri: &str) -> String {
    uri.replace('\\', "/")
}

fn path_is_within_root(path: &str, root: &str) -> bool {
    path.strip_prefix(root)
        .is_some_and(|suffix| suffix.starts_with('/'))
}
