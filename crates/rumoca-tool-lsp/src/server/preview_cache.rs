//! Cached, off-write-lock hover previews of the flattened DAE.
//!
//! Hovering a model name renders a flattened-DAE summary. That summary used to
//! be produced by `compile_model_strict_reachable_uncached_with_recovery` while
//! holding the session's exclusive **write** lock, so every mouse move over a
//! model name ran a full strict compile and blocked completions, diagnostics
//! and edits for its duration — and re-ran it identically on the next hover.
//!
//! This module reuses the simulation machinery instead: the same
//! `SimulationCompileKey` (model + focus document + source-root epoch + local
//! source fingerprint) identifies a preview, the compile runs on
//! `spawn_blocking` under the `strict` work lane rather than the session write
//! lock, and the rendered markdown is memoized. Negative results are memoized
//! too, so a model that does not compile is not recompiled on every hover.

use super::*;
use std::collections::VecDeque;

/// Bounded FIFO memo of rendered previews.
///
/// Hover is unbounded user input, so the map needs a cap. FIFO (rather than
/// LRU) keeps eviction deterministic, which SPEC_0021 asks for over a marginal
/// hit-rate gain.
#[derive(Debug, Default)]
pub(super) struct HoverPreviewCache {
    entries: HashMap<SimulationCompileKey, Option<Arc<str>>>,
    order: VecDeque<SimulationCompileKey>,
}

impl HoverPreviewCache {
    /// Maximum number of memoized previews.
    pub(super) const CAPACITY: usize = 8;

    fn get(&self, key: &SimulationCompileKey) -> Option<Option<Arc<str>>> {
        self.entries.get(key).cloned()
    }

    #[cfg(test)]
    pub(super) fn len(&self) -> usize {
        self.entries.len()
    }

    fn insert(&mut self, key: SimulationCompileKey, preview: Option<Arc<str>>) {
        if self.entries.insert(key.clone(), preview).is_none() {
            self.order.push_back(key);
        }
        while self.order.len() > Self::CAPACITY {
            if let Some(evicted) = self.order.pop_front() {
                self.entries.remove(&evicted);
            }
        }
    }

    pub(super) fn clear(&mut self) {
        self.entries.clear();
        self.order.clear();
    }
}

impl ModelicaLanguageServer {
    /// Rendered flattened-DAE preview for `model`, compiled at most once per
    /// (model, focus document, session state) combination.
    ///
    /// Returns `None` when the model does not compile — including when the
    /// request went stale while waiting for the strict lane, so a hover issued
    /// during a long code-lens resolve does not publish an outdated preview.
    pub(super) async fn hover_flat_preview(
        &self,
        model: &str,
        focus_document_path: &str,
        request_token: AnalysisRequestToken,
    ) -> Option<Arc<str>> {
        let context = match self
            .prepare_simulation_compile_context(focus_document_path, false)
            .await
        {
            Ok(context) => context,
            // The on-disk package-layout walk cannot see a buffer that has
            // never been written; fall back to the in-memory document so a
            // brand-new model still previews.
            Err(_) => {
                self.prepare_in_memory_compile_context(focus_document_path)
                    .await?
            }
        };
        let source_root_epoch = self.session.read().await.source_root_state_epoch();
        let cache_key = Self::simulation_compile_key(model, &context, source_root_epoch);
        if let Some(cached) = self.hover_preview_cache.read().await.get(&cache_key) {
            return cached;
        }

        let _strict_lane = self.work_lanes.strict.lock().await;
        if self.analysis_request_is_stale(request_token).await {
            return None;
        }
        // Another hover may have filled the entry while this one waited on the
        // lane; re-check before paying for a compile.
        if let Some(cached) = self.hover_preview_cache.read().await.get(&cache_key) {
            return cached;
        }

        let preview = self.compile_flat_preview(model, context).await;
        self.hover_preview_cache
            .write()
            .await
            .insert(cache_key, preview.clone());
        preview
    }

    async fn compile_flat_preview(
        &self,
        model: &str,
        context: SimulationCompileContext,
    ) -> Option<Arc<str>> {
        let snapshot = self.build_simulation_snapshot(context);
        let model_name = model.to_string();
        let compiled = tokio::task::spawn_blocking(move || snapshot.compile_model(&model_name))
            .await
            .ok()?
            .ok()?;
        Some(Arc::from(
            render_flattened_preview(model, &compiled).as_str(),
        ))
    }

    /// Drop every memoized preview (session content changed).
    pub(super) async fn clear_hover_preview_cache(&self) {
        self.hover_preview_cache.write().await.clear();
    }
}
