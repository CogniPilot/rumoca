use super::session_impl_symbols::workspace_symbol_query_match_score;
use super::*;
use crate::instrumentation::{
    record_document_symbol_query_hit, record_document_symbol_query_miss,
    record_orphan_package_membership_query_hit, record_orphan_package_membership_query_miss,
    record_source_set_package_membership_query_hit,
    record_source_set_package_membership_query_miss, record_workspace_symbol_query_hit,
    record_workspace_symbol_query_miss,
};

impl Session {
    pub(crate) fn source_set_package_def_map_query(
        &mut self,
        source_set_id: SourceSetId,
    ) -> Option<&PackageDefMap> {
        let record = self
            .source_sets
            .iter()
            .find(|(_, record)| record.id == source_set_id)
            .map(|(key, record)| (key.clone(), record.clone()))?;
        let signature = self.source_set_class_graph_signature(source_set_id)?;
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

        let (source_set_key, source_set_record) = record;
        let uris = source_set_record.uris.iter().cloned().collect::<Vec<_>>();
        let (_, detached_uris) = self.detached_source_root_summary_signature(&source_set_key);
        let dirty_class_prefixes = source_set_record.dirty_class_prefixes.clone();
        let detached_summaries = detached_uris
            .iter()
            .filter_map(|uri| self.interface_file_summary(uri))
            .collect::<Vec<_>>();

        let def_map = if let Some(previous) = self
            .query_state
            .ast
            .package_def_map
            .source_set_caches
            .get(&source_set_id)
            && previous.signature.source_set == signature.source_set
            && !dirty_class_prefixes.is_empty()
            && !detached_summaries.is_empty()
        {
            previous
                .def_map
                .patched_with_summaries(&dirty_class_prefixes, detached_summaries.iter())
        } else {
            self.rebuild_source_set_package_def_map(&uris, &detached_summaries)
        };
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
            let Some(summary) = self.interface_file_summary(uri) else {
                continue;
            };
            def_map.extend_from_summary(&summary);
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
