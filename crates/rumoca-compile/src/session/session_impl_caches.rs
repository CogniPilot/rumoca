use super::*;

impl Session {
    pub(super) fn cached_compile_result(
        &mut self,
        model_name: &str,
        fingerprint: Fingerprint,
    ) -> Option<CachedCompileResult> {
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

    pub(super) fn compile_result_from_cache_hit(
        &mut self,
        tree: &ast::ClassTree,
        mode: ResolveBuildMode,
        model_name: &str,
        cached: CachedCompileResult,
    ) -> PhaseResult {
        match cached {
            CachedCompileResult::Full(result) => result,
            CachedCompileResult::Success => self.compile_phase_result_query(tree, mode, model_name),
        }
    }

    pub(crate) fn trim_lru_cache<C>(cache: &mut C, max_entries: usize)
    where
        C: SessionLruCache,
    {
        cache.trim_lru_to(max_entries);
    }

    pub(super) fn insert_compile_result(
        &mut self,
        model_name: String,
        fingerprint: Fingerprint,
        result: PhaseResult,
    ) {
        self.insert_compile_cache_entry(
            model_name,
            fingerprint,
            CachedCompileResult::from_phase_result(result),
        );
    }

    pub(super) fn insert_full_compile_result(
        &mut self,
        model_name: String,
        fingerprint: Fingerprint,
        result: PhaseResult,
    ) {
        self.insert_compile_cache_entry(model_name, fingerprint, CachedCompileResult::Full(result));
    }

    fn insert_compile_cache_entry(
        &mut self,
        model_name: String,
        fingerprint: Fingerprint,
        result: CachedCompileResult,
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

    pub(super) fn cached_semantic_navigation(
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

    pub(super) fn insert_semantic_navigation(
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
}
