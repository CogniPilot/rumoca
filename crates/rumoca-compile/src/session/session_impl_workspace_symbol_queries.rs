use super::*;

impl Session {
    pub(super) fn with_workspace_symbol_source_set_query<R>(
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

    pub(super) fn with_detached_workspace_symbol_query<R>(
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
}
