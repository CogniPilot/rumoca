use std::time::Instant;

use crate::completion_metrics::extract_namespace_completion_prefix;
use rumoca_session::compile::{
    PhaseResult, SessionCacheStatsSnapshot, SessionSnapshot, StrictCompileReport,
    session_cache_stats,
};
use tower_lsp::lsp_types::Position;

use super::{
    ModelicaLanguageServer, dae_balance, dae_balance_detail, maybe_log_completion_debug,
    write_completion_progress_summary,
};

#[derive(Debug, Clone, Default)]
pub(super) struct CompletionPreparation {
    pub(super) request_was_stale: bool,
    pub(super) completion_prefix: Option<String>,
    pub(super) source_root_load_ms: u64,
    pub(super) completion_source_root_load_ms: u64,
    pub(super) namespace_completion_prime_ms: u64,
    pub(super) needs_resolved_session: bool,
    pub(super) ast_fast_path_matched: bool,
    pub(super) query_fast_path_check_ms: u64,
    pub(super) query_fast_path_matched: bool,
    pub(super) resolved_build_ms: Option<u64>,
    pub(super) built_resolved_tree: bool,
    pub(super) had_resolved_cache_before: bool,
}

#[derive(Debug, Clone, Copy, Default)]
struct CompletionStageTimings {
    source_root_load_ms: u64,
    completion_source_root_load_ms: u64,
    namespace_completion_prime_ms: u64,
}

#[derive(Debug, Clone, Default)]
struct CompletionSourceRootPreparation {
    request_was_stale: bool,
    completion_prefix: Option<String>,
    timings: CompletionStageTimings,
}

#[derive(Debug, Clone, Default)]
struct NamespaceCompletionPrimeResult {
    elapsed_ms: u64,
    detail: Option<String>,
}

#[derive(Debug, Clone, Copy)]
struct CompletionProgressContext<'a> {
    uri_path: &'a str,
    request_edit_epoch: u64,
    started: &'a Instant,
}

pub(super) struct CompletionTimingContext {
    pub(super) request_edit_epoch: u64,
    pub(super) uri: String,
    pub(super) semantic_layer: String,
    pub(super) completion_handler_ms: u64,
    pub(super) total_ms: u64,
    pub(super) class_name_count_after_ensure: usize,
    pub(super) session_cache_delta: SessionCacheStatsSnapshot,
}

impl<'a> CompletionProgressContext<'a> {
    fn log(
        self,
        stage: &str,
        status: &str,
        completion_prefix: Option<&str>,
        needs_resolved_session: Option<bool>,
        query_fast_path_matched: Option<bool>,
    ) {
        self.log_with_detail(
            stage,
            status,
            completion_prefix,
            needs_resolved_session,
            query_fast_path_matched,
            None,
        );
    }

    fn log_with_detail(
        self,
        stage: &str,
        status: &str,
        completion_prefix: Option<&str>,
        needs_resolved_session: Option<bool>,
        query_fast_path_matched: Option<bool>,
        detail: Option<&str>,
    ) {
        write_completion_progress_summary(
            &crate::completion_metrics::CompletionProgressSummary {
                requested_edit_epoch: self.request_edit_epoch,
                uri: self.uri_path.to_string(),
                stage: stage.to_string(),
                status: status.to_string(),
                elapsed_ms: self.started.elapsed().as_millis() as u64,
                completion_prefix: completion_prefix.map(ToString::to_string),
                needs_resolved_session,
                query_fast_path_matched,
                detail: detail.map(ToString::to_string),
            },
            None,
        );
    }
}

impl CompletionPreparation {
    fn stale(completion_prefix: Option<String>, timings: CompletionStageTimings) -> Self {
        Self {
            request_was_stale: true,
            completion_prefix,
            source_root_load_ms: timings.source_root_load_ms,
            completion_source_root_load_ms: timings.completion_source_root_load_ms,
            namespace_completion_prime_ms: timings.namespace_completion_prime_ms,
            needs_resolved_session: false,
            ast_fast_path_matched: false,
            query_fast_path_check_ms: 0,
            query_fast_path_matched: false,
            resolved_build_ms: None,
            built_resolved_tree: false,
            had_resolved_cache_before: false,
        }
    }

    fn ready(completion_prefix: Option<String>, timings: CompletionStageTimings) -> Self {
        Self {
            request_was_stale: false,
            completion_prefix,
            source_root_load_ms: timings.source_root_load_ms,
            completion_source_root_load_ms: timings.completion_source_root_load_ms,
            namespace_completion_prime_ms: timings.namespace_completion_prime_ms,
            needs_resolved_session: false,
            ast_fast_path_matched: false,
            query_fast_path_check_ms: 0,
            query_fast_path_matched: false,
            resolved_build_ms: None,
            built_resolved_tree: false,
            had_resolved_cache_before: false,
        }
    }
}

pub(super) fn build_completion_timing_summary(
    preparation: CompletionPreparation,
    context: CompletionTimingContext,
) -> crate::completion_metrics::CompletionTimingSummary {
    crate::completion_metrics::CompletionTimingSummary {
        requested_edit_epoch: context.request_edit_epoch,
        request_was_stale: preparation.request_was_stale,
        uri: context.uri,
        semantic_layer: context.semantic_layer,
        source_root_load_ms: preparation.source_root_load_ms,
        completion_source_root_load_ms: preparation.completion_source_root_load_ms,
        namespace_completion_prime_ms: preparation.namespace_completion_prime_ms,
        needs_resolved_session: preparation.needs_resolved_session,
        ast_fast_path_matched: preparation.ast_fast_path_matched,
        query_fast_path_check_ms: preparation.query_fast_path_check_ms,
        query_fast_path_matched: preparation.query_fast_path_matched,
        resolved_build_ms: preparation.resolved_build_ms,
        completion_handler_ms: context.completion_handler_ms,
        total_ms: context.total_ms,
        built_resolved_tree: preparation.built_resolved_tree,
        had_resolved_cache_before: preparation.had_resolved_cache_before,
        namespace_index_query_hits: context.session_cache_delta.namespace_index_query_hits,
        namespace_index_query_misses: context.session_cache_delta.namespace_index_query_misses,
        file_item_index_query_hits: context.session_cache_delta.file_item_index_query_hits,
        file_item_index_query_misses: context.session_cache_delta.file_item_index_query_misses,
        declaration_index_query_hits: context.session_cache_delta.declaration_index_query_hits,
        declaration_index_query_misses: context.session_cache_delta.declaration_index_query_misses,
        scope_query_hits: context.session_cache_delta.scope_query_hits,
        scope_query_misses: context.session_cache_delta.scope_query_misses,
        source_set_package_membership_query_hits: context
            .session_cache_delta
            .source_set_package_membership_query_hits,
        source_set_package_membership_query_misses: context
            .session_cache_delta
            .source_set_package_membership_query_misses,
        orphan_package_membership_query_hits: context
            .session_cache_delta
            .orphan_package_membership_query_hits,
        orphan_package_membership_query_misses: context
            .session_cache_delta
            .orphan_package_membership_query_misses,
        class_name_count_after_ensure: context.class_name_count_after_ensure,
        session_cache_delta: context.session_cache_delta,
    }
}

impl CompletionSourceRootPreparation {
    fn stale(self) -> CompletionPreparation {
        CompletionPreparation::stale(self.completion_prefix, self.timings)
    }
}

impl ModelicaLanguageServer {
    pub(super) async fn prepare_completion(
        &self,
        source: &str,
        pos: Position,
        uri_path: &str,
        request_edit_epoch: u64,
    ) -> CompletionPreparation {
        let prepare_started = Instant::now();
        let progress = CompletionProgressContext {
            uri_path,
            request_edit_epoch,
            started: &prepare_started,
        };
        if self.completion_request_is_stale(request_edit_epoch) {
            return CompletionPreparation::stale(
                extract_namespace_completion_prefix(source, pos),
                CompletionStageTimings::default(),
            );
        }

        let source_roots = self
            .prepare_completion_source_roots(source, pos, request_edit_epoch, progress)
            .await;
        if source_roots.request_was_stale {
            return source_roots.stale();
        }
        if self.completion_request_is_stale(request_edit_epoch) {
            return CompletionPreparation::stale(
                source_roots.completion_prefix,
                source_roots.timings,
            );
        }

        CompletionPreparation::ready(source_roots.completion_prefix, source_roots.timings)
    }

    async fn maybe_prime_namespace_completion_cache(
        &self,
        has_namespace_prefix: bool,
    ) -> NamespaceCompletionPrimeResult {
        if !has_namespace_prefix {
            return NamespaceCompletionPrimeResult::default();
        }
        self.wait_for_source_root_read_prewarm_if_pending().await;
        let started = Instant::now();
        let stats_before = session_cache_stats();
        let _indexing_guard = self.work_lanes.indexing.lock().await;
        let snapshot = self.session_snapshot().await;
        let prime = tokio::task::spawn_blocking(move || snapshot.namespace_index_query("")).await;
        if let Err(error) = prime {
            maybe_log_completion_debug(
                &self.client,
                format!("namespace completion worker failed: {error}"),
            )
            .await;
        } else if let Ok(Err(error)) = prime {
            maybe_log_completion_debug(
                &self.client,
                format!("failed to build namespace completion cache: {error}"),
            )
            .await;
        }
        let stats_after = session_cache_stats();
        let stats_delta = stats_after.delta_since(stats_before);
        NamespaceCompletionPrimeResult {
            elapsed_ms: started.elapsed().as_millis() as u64,
            detail: Some(namespace_completion_prime_detail(stats_delta)),
        }
    }

    async fn prepare_completion_source_roots(
        &self,
        source: &str,
        pos: Position,
        request_edit_epoch: u64,
        progress: CompletionProgressContext<'_>,
    ) -> CompletionSourceRootPreparation {
        let mut source_roots = CompletionSourceRootPreparation::default();
        let completion_prefix = extract_namespace_completion_prefix(source, pos);

        progress.log("source_root_load", "start", None, None, None);
        let source_root_load_started = Instant::now();
        let source_root_paths = self.source_root_paths.read().await.clone();
        self.ensure_source_roots_loaded_with_paths(source, progress.uri_path, &source_root_paths)
            .await;
        source_roots.timings.source_root_load_ms =
            source_root_load_started.elapsed().as_millis() as u64;
        progress.log("source_root_load", "end", None, None, None);
        if self.completion_request_is_stale(request_edit_epoch) {
            source_roots.request_was_stale = true;
            source_roots.completion_prefix = completion_prefix;
            return source_roots;
        }

        progress.log("completion_source_root_load", "start", None, None, None);
        let completion_source_root_load_started = Instant::now();
        self.ensure_completion_source_roots(source, pos, progress.uri_path)
            .await;
        source_roots.timings.completion_source_root_load_ms =
            completion_source_root_load_started.elapsed().as_millis() as u64;
        progress.log("completion_source_root_load", "end", None, None, None);
        if self.completion_request_is_stale(request_edit_epoch) {
            source_roots.request_was_stale = true;
            source_roots.completion_prefix = completion_prefix;
            return source_roots;
        }

        source_roots.completion_prefix = completion_prefix;
        progress.log(
            "namespace_completion_prime",
            "start",
            source_roots.completion_prefix.as_deref(),
            None,
            None,
        );
        let prime = self
            .maybe_prime_namespace_completion_cache(source_roots.completion_prefix.is_some())
            .await;
        source_roots.timings.namespace_completion_prime_ms = prime.elapsed_ms;
        progress.log_with_detail(
            "namespace_completion_prime",
            "end",
            source_roots.completion_prefix.as_deref(),
            None,
            None,
            prime.detail.as_deref(),
        );
        source_roots.request_was_stale = self.completion_request_is_stale(request_edit_epoch);
        source_roots
    }

    pub(super) fn cached_completion_class_name_count(
        snapshot: &SessionSnapshot,
        completion_prefix: Option<&str>,
    ) -> usize {
        if completion_prefix.is_none() {
            return snapshot.namespace_class_names_cached().len();
        }
        let namespace_class_names = cached_namespace_class_names(snapshot);
        if namespace_class_names.is_empty() {
            snapshot.namespace_class_names_cached().len()
        } else {
            namespace_class_names.len()
        }
    }
}

fn namespace_completion_prime_detail(delta: SessionCacheStatsSnapshot) -> String {
    format!(
        "decl={}/{} scope={}/{} pkg={}/{} orphan={}/{} ns={}/{} libCache={}/{} nsCollectMs={} nsBuildMs={} nsFinalizeMs={}",
        delta.declaration_index_query_hits,
        delta.declaration_index_query_misses,
        delta.scope_query_hits,
        delta.scope_query_misses,
        delta.source_set_package_membership_query_hits,
        delta.source_set_package_membership_query_misses,
        delta.orphan_package_membership_query_hits,
        delta.orphan_package_membership_query_misses,
        delta.namespace_index_query_hits,
        delta.namespace_index_query_misses,
        delta.namespace_completion_cache_hits,
        delta.namespace_completion_cache_misses,
        delta.namespace_refresh_collect_ms,
        delta.namespace_refresh_build_ms,
        delta.namespace_refresh_finalize_ms,
    )
}

fn cached_namespace_class_names(snapshot: &SessionSnapshot) -> Vec<String> {
    let class_names = snapshot
        .namespace_index_query("")
        .ok()
        .into_iter()
        .flat_map(|entries| entries.into_iter().map(|entry| entry.1))
        .collect::<Vec<_>>();
    if !class_names.is_empty() {
        return class_names;
    }

    snapshot.namespace_class_names_cached()
}

pub(super) fn code_lens_title_from_strict_report(mut report: StrictCompileReport) -> String {
    if !report.failures.is_empty() {
        return strict_compile_failure_title(&report);
    }

    match report.requested_result.take() {
        Some(PhaseResult::Success(result)) => balanced_code_lens_title(&result),
        Some(PhaseResult::NeedsInner { missing_inners }) => {
            format!("Needs inner ({})", missing_inners.join(", "))
        }
        Some(PhaseResult::Failed { phase, error, .. }) => {
            format!(
                "Compile failed ({}: {})",
                phase,
                truncate_code_lens_detail(&error)
            )
        }
        None => format!("Compile failed ({})", report.requested_model),
    }
}

fn strict_compile_failure_title(report: &StrictCompileReport) -> String {
    let failure = report
        .failures
        .iter()
        .find(|failure| failure.model_name == report.requested_model)
        .or_else(|| report.failures.first());
    let Some(failure) = failure else {
        return format!("Compile failed ({})", report.requested_model);
    };
    match failure.phase {
        Some(phase) => format!(
            "Compile failed ({}: {})",
            phase,
            truncate_code_lens_detail(&failure.error)
        ),
        None => format!(
            "Compile error ({})",
            truncate_code_lens_detail(&failure.error)
        ),
    }
}

fn balanced_code_lens_title(result: &rumoca_session::compile::CompilationResult) -> String {
    let detail = dae_balance_detail(&result.dae);
    let unknowns = detail.state_unknowns + detail.alg_unknowns + detail.output_unknowns;
    let balance = dae_balance(&result.dae);
    let equations = (unknowns as i64 + balance).max(0) as usize;
    if balance == 0 {
        format!("Balanced ({unknowns} unknowns, {equations} eqs)")
    } else {
        format!("Unbalanced ({unknowns} unknowns, {equations} eqs, Δ={balance})")
    }
}

fn truncate_code_lens_detail(message: &str) -> String {
    const MAX_CHARS: usize = 72;
    let mut chars = message.chars();
    let truncated: String = chars.by_ref().take(MAX_CHARS).collect();
    if chars.next().is_some() {
        format!("{truncated}...")
    } else {
        truncated
    }
}
