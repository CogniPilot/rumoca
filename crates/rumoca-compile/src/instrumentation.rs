use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use crate::source_root_cache::SourceRootCacheStatus;

macro_rules! session_cache_delta_fields {
    ($lhs:expr, $rhs:expr) => {
        Self {
            document_parse_calls: $lhs
                .document_parse_calls
                .saturating_sub($rhs.document_parse_calls),
            document_parse_total_nanos: $lhs
                .document_parse_total_nanos
                .saturating_sub($rhs.document_parse_total_nanos),
            document_parse_error_calls: $lhs
                .document_parse_error_calls
                .saturating_sub($rhs.document_parse_error_calls),
            parsed_file_parse_calls: $lhs
                .parsed_file_parse_calls
                .saturating_sub($rhs.parsed_file_parse_calls),
            parsed_file_parse_total_nanos: $lhs
                .parsed_file_parse_total_nanos
                .saturating_sub($rhs.parsed_file_parse_total_nanos),
            parsed_file_artifact_cache_hits: $lhs
                .parsed_file_artifact_cache_hits
                .saturating_sub($rhs.parsed_file_artifact_cache_hits),
            parsed_file_artifact_cache_misses: $lhs
                .parsed_file_artifact_cache_misses
                .saturating_sub($rhs.parsed_file_artifact_cache_misses),
            parsed_file_query_hits: $lhs
                .parsed_file_query_hits
                .saturating_sub($rhs.parsed_file_query_hits),
            parsed_file_query_misses: $lhs
                .parsed_file_query_misses
                .saturating_sub($rhs.parsed_file_query_misses),
            recovered_file_query_hits: $lhs
                .recovered_file_query_hits
                .saturating_sub($rhs.recovered_file_query_hits),
            recovered_file_query_misses: $lhs
                .recovered_file_query_misses
                .saturating_sub($rhs.recovered_file_query_misses),
            file_item_index_query_hits: $lhs
                .file_item_index_query_hits
                .saturating_sub($rhs.file_item_index_query_hits),
            file_item_index_query_misses: $lhs
                .file_item_index_query_misses
                .saturating_sub($rhs.file_item_index_query_misses),
            declaration_index_query_hits: $lhs
                .declaration_index_query_hits
                .saturating_sub($rhs.declaration_index_query_hits),
            declaration_index_query_misses: $lhs
                .declaration_index_query_misses
                .saturating_sub($rhs.declaration_index_query_misses),
            scope_query_hits: $lhs.scope_query_hits.saturating_sub($rhs.scope_query_hits),
            scope_query_misses: $lhs
                .scope_query_misses
                .saturating_sub($rhs.scope_query_misses),
            source_set_package_membership_query_hits: $lhs
                .source_set_package_membership_query_hits
                .saturating_sub($rhs.source_set_package_membership_query_hits),
            source_set_package_membership_query_misses: $lhs
                .source_set_package_membership_query_misses
                .saturating_sub($rhs.source_set_package_membership_query_misses),
            orphan_package_membership_query_hits: $lhs
                .orphan_package_membership_query_hits
                .saturating_sub($rhs.orphan_package_membership_query_hits),
            orphan_package_membership_query_misses: $lhs
                .orphan_package_membership_query_misses
                .saturating_sub($rhs.orphan_package_membership_query_misses),
            namespace_index_query_hits: $lhs
                .namespace_index_query_hits
                .saturating_sub($rhs.namespace_index_query_hits),
            namespace_index_query_misses: $lhs
                .namespace_index_query_misses
                .saturating_sub($rhs.namespace_index_query_misses),
            workspace_symbol_query_hits: $lhs
                .workspace_symbol_query_hits
                .saturating_sub($rhs.workspace_symbol_query_hits),
            workspace_symbol_query_misses: $lhs
                .workspace_symbol_query_misses
                .saturating_sub($rhs.workspace_symbol_query_misses),
            document_symbol_query_hits: $lhs
                .document_symbol_query_hits
                .saturating_sub($rhs.document_symbol_query_hits),
            document_symbol_query_misses: $lhs
                .document_symbol_query_misses
                .saturating_sub($rhs.document_symbol_query_misses),
            source_root_files_parsed: $lhs
                .source_root_files_parsed
                .saturating_sub($rhs.source_root_files_parsed),
            source_root_cache_hits: $lhs
                .source_root_cache_hits
                .saturating_sub($rhs.source_root_cache_hits),
            source_root_cache_misses: $lhs
                .source_root_cache_misses
                .saturating_sub($rhs.source_root_cache_misses),
            source_root_cache_disabled: $lhs
                .source_root_cache_disabled
                .saturating_sub($rhs.source_root_cache_disabled),
            standard_resolved_builds: $lhs
                .standard_resolved_builds
                .saturating_sub($rhs.standard_resolved_builds),
            standard_resolved_build_total_nanos: $lhs
                .standard_resolved_build_total_nanos
                .saturating_sub($rhs.standard_resolved_build_total_nanos),
            standard_resolved_cache_hits: $lhs
                .standard_resolved_cache_hits
                .saturating_sub($rhs.standard_resolved_cache_hits),
            strict_resolved_builds: $lhs
                .strict_resolved_builds
                .saturating_sub($rhs.strict_resolved_builds),
            strict_resolved_build_total_nanos: $lhs
                .strict_resolved_build_total_nanos
                .saturating_sub($rhs.strict_resolved_build_total_nanos),
            semantic_navigation_cache_hits: $lhs
                .semantic_navigation_cache_hits
                .saturating_sub($rhs.semantic_navigation_cache_hits),
            semantic_navigation_cache_misses: $lhs
                .semantic_navigation_cache_misses
                .saturating_sub($rhs.semantic_navigation_cache_misses),
            semantic_navigation_builds: $lhs
                .semantic_navigation_builds
                .saturating_sub($rhs.semantic_navigation_builds),
            interface_semantic_diagnostics_cache_hits: $lhs
                .interface_semantic_diagnostics_cache_hits
                .saturating_sub($rhs.interface_semantic_diagnostics_cache_hits),
            interface_semantic_diagnostics_cache_misses: $lhs
                .interface_semantic_diagnostics_cache_misses
                .saturating_sub($rhs.interface_semantic_diagnostics_cache_misses),
            interface_semantic_diagnostics_builds: $lhs
                .interface_semantic_diagnostics_builds
                .saturating_sub($rhs.interface_semantic_diagnostics_builds),
            body_semantic_diagnostics_cache_hits: $lhs
                .body_semantic_diagnostics_cache_hits
                .saturating_sub($rhs.body_semantic_diagnostics_cache_hits),
            body_semantic_diagnostics_cache_misses: $lhs
                .body_semantic_diagnostics_cache_misses
                .saturating_sub($rhs.body_semantic_diagnostics_cache_misses),
            body_semantic_diagnostics_builds: $lhs
                .body_semantic_diagnostics_builds
                .saturating_sub($rhs.body_semantic_diagnostics_builds),
            model_stage_semantic_diagnostics_cache_hits: $lhs
                .model_stage_semantic_diagnostics_cache_hits
                .saturating_sub($rhs.model_stage_semantic_diagnostics_cache_hits),
            model_stage_semantic_diagnostics_cache_misses: $lhs
                .model_stage_semantic_diagnostics_cache_misses
                .saturating_sub($rhs.model_stage_semantic_diagnostics_cache_misses),
            model_stage_semantic_diagnostics_builds: $lhs
                .model_stage_semantic_diagnostics_builds
                .saturating_sub($rhs.model_stage_semantic_diagnostics_builds),
            instantiated_model_cache_hits: $lhs
                .instantiated_model_cache_hits
                .saturating_sub($rhs.instantiated_model_cache_hits),
            instantiated_model_cache_misses: $lhs
                .instantiated_model_cache_misses
                .saturating_sub($rhs.instantiated_model_cache_misses),
            instantiated_model_builds: $lhs
                .instantiated_model_builds
                .saturating_sub($rhs.instantiated_model_builds),
            typed_model_cache_hits: $lhs
                .typed_model_cache_hits
                .saturating_sub($rhs.typed_model_cache_hits),
            typed_model_cache_misses: $lhs
                .typed_model_cache_misses
                .saturating_sub($rhs.typed_model_cache_misses),
            typed_model_builds: $lhs
                .typed_model_builds
                .saturating_sub($rhs.typed_model_builds),
            flat_model_cache_hits: $lhs
                .flat_model_cache_hits
                .saturating_sub($rhs.flat_model_cache_hits),
            flat_model_cache_misses: $lhs
                .flat_model_cache_misses
                .saturating_sub($rhs.flat_model_cache_misses),
            flat_model_builds: $lhs
                .flat_model_builds
                .saturating_sub($rhs.flat_model_builds),
            dae_model_cache_hits: $lhs
                .dae_model_cache_hits
                .saturating_sub($rhs.dae_model_cache_hits),
            dae_model_cache_misses: $lhs
                .dae_model_cache_misses
                .saturating_sub($rhs.dae_model_cache_misses),
            dae_model_builds: $lhs.dae_model_builds.saturating_sub($rhs.dae_model_builds),
            namespace_completion_cache_hits: $lhs
                .namespace_completion_cache_hits
                .saturating_sub($rhs.namespace_completion_cache_hits),
            namespace_completion_cache_misses: $lhs
                .namespace_completion_cache_misses
                .saturating_sub($rhs.namespace_completion_cache_misses),
            namespace_refresh_collect_ms: $lhs
                .namespace_refresh_collect_ms
                .saturating_sub($rhs.namespace_refresh_collect_ms),
            namespace_refresh_build_ms: $lhs
                .namespace_refresh_build_ms
                .saturating_sub($rhs.namespace_refresh_build_ms),
            namespace_refresh_finalize_ms: $lhs
                .namespace_refresh_finalize_ms
                .saturating_sub($rhs.namespace_refresh_finalize_ms),
            resolved_state_invalidations: $lhs
                .resolved_state_invalidations
                .saturating_sub($rhs.resolved_state_invalidations),
            strict_resolved_state_invalidations: $lhs
                .strict_resolved_state_invalidations
                .saturating_sub($rhs.strict_resolved_state_invalidations),
            namespace_completion_state_invalidations: $lhs
                .namespace_completion_state_invalidations
                .saturating_sub($rhs.namespace_completion_state_invalidations),
            document_mutation_invalidations: $lhs
                .document_mutation_invalidations
                .saturating_sub($rhs.document_mutation_invalidations),
            source_set_mutation_invalidations: $lhs
                .source_set_mutation_invalidations
                .saturating_sub($rhs.source_set_mutation_invalidations),
            document_removal_invalidations: $lhs
                .document_removal_invalidations
                .saturating_sub($rhs.document_removal_invalidations),
        }
    };
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Default)]
pub struct SessionCacheStatsSnapshot {
    pub document_parse_calls: u64,
    pub document_parse_total_nanos: u64,
    pub document_parse_error_calls: u64,
    pub parsed_file_parse_calls: u64,
    pub parsed_file_parse_total_nanos: u64,
    pub parsed_file_artifact_cache_hits: u64,
    pub parsed_file_artifact_cache_misses: u64,
    pub parsed_file_query_hits: u64,
    pub parsed_file_query_misses: u64,
    pub recovered_file_query_hits: u64,
    pub recovered_file_query_misses: u64,
    pub file_item_index_query_hits: u64,
    pub file_item_index_query_misses: u64,
    pub declaration_index_query_hits: u64,
    pub declaration_index_query_misses: u64,
    pub scope_query_hits: u64,
    pub scope_query_misses: u64,
    pub source_set_package_membership_query_hits: u64,
    pub source_set_package_membership_query_misses: u64,
    pub orphan_package_membership_query_hits: u64,
    pub orphan_package_membership_query_misses: u64,
    pub namespace_index_query_hits: u64,
    pub namespace_index_query_misses: u64,
    pub workspace_symbol_query_hits: u64,
    pub workspace_symbol_query_misses: u64,
    pub document_symbol_query_hits: u64,
    pub document_symbol_query_misses: u64,
    pub source_root_files_parsed: u64,
    pub source_root_cache_hits: u64,
    pub source_root_cache_misses: u64,
    pub source_root_cache_disabled: u64,
    pub standard_resolved_builds: u64,
    pub standard_resolved_build_total_nanos: u64,
    pub standard_resolved_cache_hits: u64,
    pub strict_resolved_builds: u64,
    pub strict_resolved_build_total_nanos: u64,
    pub semantic_navigation_cache_hits: u64,
    pub semantic_navigation_cache_misses: u64,
    pub semantic_navigation_builds: u64,
    pub interface_semantic_diagnostics_cache_hits: u64,
    pub interface_semantic_diagnostics_cache_misses: u64,
    pub interface_semantic_diagnostics_builds: u64,
    pub body_semantic_diagnostics_cache_hits: u64,
    pub body_semantic_diagnostics_cache_misses: u64,
    pub body_semantic_diagnostics_builds: u64,
    pub model_stage_semantic_diagnostics_cache_hits: u64,
    pub model_stage_semantic_diagnostics_cache_misses: u64,
    pub model_stage_semantic_diagnostics_builds: u64,
    pub instantiated_model_cache_hits: u64,
    pub instantiated_model_cache_misses: u64,
    pub instantiated_model_builds: u64,
    pub typed_model_cache_hits: u64,
    pub typed_model_cache_misses: u64,
    pub typed_model_builds: u64,
    pub flat_model_cache_hits: u64,
    pub flat_model_cache_misses: u64,
    pub flat_model_builds: u64,
    pub dae_model_cache_hits: u64,
    pub dae_model_cache_misses: u64,
    pub dae_model_builds: u64,
    pub namespace_completion_cache_hits: u64,
    pub namespace_completion_cache_misses: u64,
    pub namespace_refresh_collect_ms: u64,
    pub namespace_refresh_build_ms: u64,
    pub namespace_refresh_finalize_ms: u64,
    pub resolved_state_invalidations: u64,
    pub strict_resolved_state_invalidations: u64,
    pub namespace_completion_state_invalidations: u64,
    pub document_mutation_invalidations: u64,
    pub source_set_mutation_invalidations: u64,
    pub document_removal_invalidations: u64,
}

impl SessionCacheStatsSnapshot {
    pub fn delta_since(self, earlier: Self) -> Self {
        session_cache_delta_fields!(self, earlier)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CacheInvalidationCause {
    DocumentMutation,
    SourceSetMutation,
    DocumentRemoval,
}

static DOCUMENT_PARSE_CALLS: AtomicU64 = AtomicU64::new(0);
static DOCUMENT_PARSE_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static DOCUMENT_PARSE_ERROR_CALLS: AtomicU64 = AtomicU64::new(0);
static PARSED_FILE_PARSE_CALLS: AtomicU64 = AtomicU64::new(0);
static PARSED_FILE_PARSE_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static PARSED_FILE_ARTIFACT_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static PARSED_FILE_ARTIFACT_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static PARSED_FILE_QUERY_HITS: AtomicU64 = AtomicU64::new(0);
static PARSED_FILE_QUERY_MISSES: AtomicU64 = AtomicU64::new(0);
static RECOVERED_FILE_QUERY_HITS: AtomicU64 = AtomicU64::new(0);
static RECOVERED_FILE_QUERY_MISSES: AtomicU64 = AtomicU64::new(0);
static FILE_ITEM_INDEX_QUERY_HITS: AtomicU64 = AtomicU64::new(0);
static FILE_ITEM_INDEX_QUERY_MISSES: AtomicU64 = AtomicU64::new(0);
static DECLARATION_INDEX_QUERY_HITS: AtomicU64 = AtomicU64::new(0);
static DECLARATION_INDEX_QUERY_MISSES: AtomicU64 = AtomicU64::new(0);
static SCOPE_QUERY_HITS: AtomicU64 = AtomicU64::new(0);
static SCOPE_QUERY_MISSES: AtomicU64 = AtomicU64::new(0);
static SOURCE_SET_PACKAGE_MEMBERSHIP_QUERY_HITS: AtomicU64 = AtomicU64::new(0);
static SOURCE_SET_PACKAGE_MEMBERSHIP_QUERY_MISSES: AtomicU64 = AtomicU64::new(0);
static ORPHAN_PACKAGE_MEMBERSHIP_QUERY_HITS: AtomicU64 = AtomicU64::new(0);
static ORPHAN_PACKAGE_MEMBERSHIP_QUERY_MISSES: AtomicU64 = AtomicU64::new(0);
static NAMESPACE_INDEX_QUERY_HITS: AtomicU64 = AtomicU64::new(0);
static NAMESPACE_INDEX_QUERY_MISSES: AtomicU64 = AtomicU64::new(0);
static WORKSPACE_SYMBOL_QUERY_HITS: AtomicU64 = AtomicU64::new(0);
static WORKSPACE_SYMBOL_QUERY_MISSES: AtomicU64 = AtomicU64::new(0);
static DOCUMENT_SYMBOL_QUERY_HITS: AtomicU64 = AtomicU64::new(0);
static DOCUMENT_SYMBOL_QUERY_MISSES: AtomicU64 = AtomicU64::new(0);
static SOURCE_ROOT_FILES_PARSED: AtomicU64 = AtomicU64::new(0);
static SOURCE_ROOT_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static SOURCE_ROOT_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static SOURCE_ROOT_CACHE_DISABLED: AtomicU64 = AtomicU64::new(0);
static STANDARD_RESOLVED_BUILDS: AtomicU64 = AtomicU64::new(0);
static STANDARD_RESOLVED_BUILD_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static STANDARD_RESOLVED_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static STRICT_RESOLVED_BUILDS: AtomicU64 = AtomicU64::new(0);
static STRICT_RESOLVED_BUILD_TOTAL_NANOS: AtomicU64 = AtomicU64::new(0);
static SEMANTIC_NAVIGATION_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static SEMANTIC_NAVIGATION_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static SEMANTIC_NAVIGATION_BUILDS: AtomicU64 = AtomicU64::new(0);
static INTERFACE_SEMANTIC_DIAGNOSTICS_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static INTERFACE_SEMANTIC_DIAGNOSTICS_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static INTERFACE_SEMANTIC_DIAGNOSTICS_BUILDS: AtomicU64 = AtomicU64::new(0);
static BODY_SEMANTIC_DIAGNOSTICS_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static BODY_SEMANTIC_DIAGNOSTICS_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static BODY_SEMANTIC_DIAGNOSTICS_BUILDS: AtomicU64 = AtomicU64::new(0);
static MODEL_STAGE_SEMANTIC_DIAGNOSTICS_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static MODEL_STAGE_SEMANTIC_DIAGNOSTICS_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static MODEL_STAGE_SEMANTIC_DIAGNOSTICS_BUILDS: AtomicU64 = AtomicU64::new(0);
static INSTANTIATED_MODEL_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static INSTANTIATED_MODEL_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static INSTANTIATED_MODEL_BUILDS: AtomicU64 = AtomicU64::new(0);
static TYPED_MODEL_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static TYPED_MODEL_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static TYPED_MODEL_BUILDS: AtomicU64 = AtomicU64::new(0);
static FLAT_MODEL_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static FLAT_MODEL_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static FLAT_MODEL_BUILDS: AtomicU64 = AtomicU64::new(0);
static DAE_MODEL_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static DAE_MODEL_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static DAE_MODEL_BUILDS: AtomicU64 = AtomicU64::new(0);
static NAMESPACE_COMPLETION_CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static NAMESPACE_COMPLETION_CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static NAMESPACE_REFRESH_COLLECT_MS: AtomicU64 = AtomicU64::new(0);
static NAMESPACE_REFRESH_BUILD_MS: AtomicU64 = AtomicU64::new(0);
static NAMESPACE_REFRESH_FINALIZE_MS: AtomicU64 = AtomicU64::new(0);
static RESOLVED_STATE_INVALIDATIONS: AtomicU64 = AtomicU64::new(0);
static STRICT_RESOLVED_STATE_INVALIDATIONS: AtomicU64 = AtomicU64::new(0);
static NAMESPACE_COMPLETION_STATE_INVALIDATIONS: AtomicU64 = AtomicU64::new(0);
static DOCUMENT_MUTATION_INVALIDATIONS: AtomicU64 = AtomicU64::new(0);
static SOURCE_SET_MUTATION_INVALIDATIONS: AtomicU64 = AtomicU64::new(0);
static DOCUMENT_REMOVAL_INVALIDATIONS: AtomicU64 = AtomicU64::new(0);

fn load(counter: &AtomicU64) -> u64 {
    counter.load(Ordering::Relaxed)
}

fn add(counter: &AtomicU64, amount: u64) {
    counter.fetch_add(amount, Ordering::Relaxed);
}

fn reset(counter: &AtomicU64) {
    counter.store(0, Ordering::Relaxed);
}

fn record_invalidation_cause(cause: CacheInvalidationCause) {
    match cause {
        CacheInvalidationCause::DocumentMutation => add(&DOCUMENT_MUTATION_INVALIDATIONS, 1),
        CacheInvalidationCause::SourceSetMutation => add(&SOURCE_SET_MUTATION_INVALIDATIONS, 1),
        CacheInvalidationCause::DocumentRemoval => add(&DOCUMENT_REMOVAL_INVALIDATIONS, 1),
    }
}

pub(crate) fn record_document_parse() {
    add(&DOCUMENT_PARSE_CALLS, 1);
}

pub(crate) fn record_document_parse_duration(duration: Duration) {
    add(
        &DOCUMENT_PARSE_TOTAL_NANOS,
        duration.as_nanos().min(u128::from(u64::MAX)) as u64,
    );
}

pub(crate) fn record_document_parse_error() {
    add(&DOCUMENT_PARSE_ERROR_CALLS, 1);
}

pub(crate) fn record_parsed_file_parse() {
    add(&PARSED_FILE_PARSE_CALLS, 1);
}

pub(crate) fn record_parsed_file_parse_duration(duration: Duration) {
    add(
        &PARSED_FILE_PARSE_TOTAL_NANOS,
        duration.as_nanos().min(u128::from(u64::MAX)) as u64,
    );
}

pub(crate) fn record_parsed_file_artifact_cache_hit() {
    add(&PARSED_FILE_ARTIFACT_CACHE_HITS, 1);
}

pub(crate) fn record_parsed_file_artifact_cache_miss() {
    add(&PARSED_FILE_ARTIFACT_CACHE_MISSES, 1);
}

pub(crate) fn record_parsed_file_query_hit() {
    add(&PARSED_FILE_QUERY_HITS, 1);
}

pub(crate) fn record_parsed_file_query_miss() {
    add(&PARSED_FILE_QUERY_MISSES, 1);
}

pub(crate) fn record_recovered_file_query_hit() {
    add(&RECOVERED_FILE_QUERY_HITS, 1);
}

pub(crate) fn record_recovered_file_query_miss() {
    add(&RECOVERED_FILE_QUERY_MISSES, 1);
}

pub(crate) fn record_file_item_index_query_hit() {
    add(&FILE_ITEM_INDEX_QUERY_HITS, 1);
}

pub(crate) fn record_file_item_index_query_miss() {
    add(&FILE_ITEM_INDEX_QUERY_MISSES, 1);
}

pub(crate) fn record_declaration_index_query_hit() {
    add(&DECLARATION_INDEX_QUERY_HITS, 1);
}

pub(crate) fn record_declaration_index_query_miss() {
    add(&DECLARATION_INDEX_QUERY_MISSES, 1);
}

pub(crate) fn record_scope_query_hit() {
    add(&SCOPE_QUERY_HITS, 1);
}

pub(crate) fn record_scope_query_miss() {
    add(&SCOPE_QUERY_MISSES, 1);
}

pub(crate) fn record_source_set_package_membership_query_hit() {
    add(&SOURCE_SET_PACKAGE_MEMBERSHIP_QUERY_HITS, 1);
}

pub(crate) fn record_source_set_package_membership_query_miss() {
    add(&SOURCE_SET_PACKAGE_MEMBERSHIP_QUERY_MISSES, 1);
}

pub(crate) fn record_orphan_package_membership_query_hit() {
    add(&ORPHAN_PACKAGE_MEMBERSHIP_QUERY_HITS, 1);
}

pub(crate) fn record_orphan_package_membership_query_miss() {
    add(&ORPHAN_PACKAGE_MEMBERSHIP_QUERY_MISSES, 1);
}

pub(crate) fn record_namespace_index_query_hit() {
    add(&NAMESPACE_INDEX_QUERY_HITS, 1);
}

pub(crate) fn record_namespace_index_query_miss() {
    add(&NAMESPACE_INDEX_QUERY_MISSES, 1);
}

pub(crate) fn record_workspace_symbol_query_hit() {
    add(&WORKSPACE_SYMBOL_QUERY_HITS, 1);
}

pub(crate) fn record_workspace_symbol_query_miss() {
    add(&WORKSPACE_SYMBOL_QUERY_MISSES, 1);
}

pub(crate) fn record_document_symbol_query_hit() {
    add(&DOCUMENT_SYMBOL_QUERY_HITS, 1);
}

pub(crate) fn record_document_symbol_query_miss() {
    add(&DOCUMENT_SYMBOL_QUERY_MISSES, 1);
}

pub(crate) fn record_source_root_cache_result(status: SourceRootCacheStatus, parsed_files: usize) {
    if parsed_files > 0 {
        add(&SOURCE_ROOT_FILES_PARSED, parsed_files as u64);
    }
    match status {
        SourceRootCacheStatus::Hit => add(&SOURCE_ROOT_CACHE_HITS, 1),
        SourceRootCacheStatus::Miss => add(&SOURCE_ROOT_CACHE_MISSES, 1),
        SourceRootCacheStatus::Disabled => add(&SOURCE_ROOT_CACHE_DISABLED, 1),
    }
}

pub(crate) fn record_standard_resolved_cache_hit() {
    add(&STANDARD_RESOLVED_CACHE_HITS, 1);
}

pub(crate) fn record_standard_resolved_build(elapsed: Duration) {
    add(&STANDARD_RESOLVED_BUILDS, 1);
    add(
        &STANDARD_RESOLVED_BUILD_TOTAL_NANOS,
        elapsed.as_nanos() as u64,
    );
}

pub(crate) fn record_strict_resolved_build(elapsed: Duration) {
    add(&STRICT_RESOLVED_BUILDS, 1);
    add(
        &STRICT_RESOLVED_BUILD_TOTAL_NANOS,
        elapsed.as_nanos() as u64,
    );
}

pub(crate) fn record_semantic_navigation_cache_hit() {
    add(&SEMANTIC_NAVIGATION_CACHE_HITS, 1);
}

pub(crate) fn record_semantic_navigation_cache_miss() {
    add(&SEMANTIC_NAVIGATION_CACHE_MISSES, 1);
}

pub(crate) fn record_semantic_navigation_build() {
    add(&SEMANTIC_NAVIGATION_BUILDS, 1);
}

pub(crate) fn record_interface_semantic_diagnostics_cache_hit() {
    add(&INTERFACE_SEMANTIC_DIAGNOSTICS_CACHE_HITS, 1);
}

pub(crate) fn record_interface_semantic_diagnostics_cache_miss() {
    add(&INTERFACE_SEMANTIC_DIAGNOSTICS_CACHE_MISSES, 1);
}

pub(crate) fn record_interface_semantic_diagnostics_build() {
    add(&INTERFACE_SEMANTIC_DIAGNOSTICS_BUILDS, 1);
}

pub(crate) fn record_body_semantic_diagnostics_cache_hit() {
    add(&BODY_SEMANTIC_DIAGNOSTICS_CACHE_HITS, 1);
}

pub(crate) fn record_body_semantic_diagnostics_cache_miss() {
    add(&BODY_SEMANTIC_DIAGNOSTICS_CACHE_MISSES, 1);
}

pub(crate) fn record_body_semantic_diagnostics_build() {
    add(&BODY_SEMANTIC_DIAGNOSTICS_BUILDS, 1);
}

pub(crate) fn record_model_stage_semantic_diagnostics_cache_hit() {
    add(&MODEL_STAGE_SEMANTIC_DIAGNOSTICS_CACHE_HITS, 1);
}

pub(crate) fn record_model_stage_semantic_diagnostics_cache_miss() {
    add(&MODEL_STAGE_SEMANTIC_DIAGNOSTICS_CACHE_MISSES, 1);
}

pub(crate) fn record_model_stage_semantic_diagnostics_build() {
    add(&MODEL_STAGE_SEMANTIC_DIAGNOSTICS_BUILDS, 1);
}

pub(crate) fn record_instantiated_model_cache_hit() {
    add(&INSTANTIATED_MODEL_CACHE_HITS, 1);
}

pub(crate) fn record_instantiated_model_cache_miss() {
    add(&INSTANTIATED_MODEL_CACHE_MISSES, 1);
}

pub(crate) fn record_instantiated_model_build() {
    add(&INSTANTIATED_MODEL_BUILDS, 1);
}

pub(crate) fn record_typed_model_cache_hit() {
    add(&TYPED_MODEL_CACHE_HITS, 1);
}

pub(crate) fn record_typed_model_cache_miss() {
    add(&TYPED_MODEL_CACHE_MISSES, 1);
}

pub(crate) fn record_typed_model_build() {
    add(&TYPED_MODEL_BUILDS, 1);
}

pub(crate) fn record_flat_model_cache_hit() {
    add(&FLAT_MODEL_CACHE_HITS, 1);
}

pub(crate) fn record_flat_model_cache_miss() {
    add(&FLAT_MODEL_CACHE_MISSES, 1);
}

pub(crate) fn record_flat_model_build() {
    add(&FLAT_MODEL_BUILDS, 1);
}

pub(crate) fn record_dae_model_cache_hit() {
    add(&DAE_MODEL_CACHE_HITS, 1);
}

pub(crate) fn record_dae_model_cache_miss() {
    add(&DAE_MODEL_CACHE_MISSES, 1);
}

pub(crate) fn record_dae_model_build() {
    add(&DAE_MODEL_BUILDS, 1);
}

pub(crate) fn record_namespace_completion_cache_hit() {
    add(&NAMESPACE_COMPLETION_CACHE_HITS, 1);
}

pub(crate) fn record_namespace_completion_cache_miss() {
    add(&NAMESPACE_COMPLETION_CACHE_MISSES, 1);
}

pub(crate) fn record_namespace_refresh_collect(elapsed: Duration) {
    add(&NAMESPACE_REFRESH_COLLECT_MS, elapsed.as_millis() as u64);
}

pub(crate) fn record_namespace_refresh_build(elapsed: Duration) {
    add(&NAMESPACE_REFRESH_BUILD_MS, elapsed.as_millis() as u64);
}

pub(crate) fn record_namespace_refresh_finalize(elapsed: Duration) {
    add(&NAMESPACE_REFRESH_FINALIZE_MS, elapsed.as_millis() as u64);
}

pub(crate) fn record_resolved_state_invalidation(cause: CacheInvalidationCause) {
    add(&RESOLVED_STATE_INVALIDATIONS, 1);
    record_invalidation_cause(cause);
}

pub(crate) fn record_strict_resolved_state_invalidation(cause: CacheInvalidationCause) {
    add(&STRICT_RESOLVED_STATE_INVALIDATIONS, 1);
    record_invalidation_cause(cause);
}

pub(crate) fn record_namespace_completion_state_invalidation(cause: CacheInvalidationCause) {
    add(&NAMESPACE_COMPLETION_STATE_INVALIDATIONS, 1);
    record_invalidation_cause(cause);
}

pub fn reset_session_cache_stats() {
    reset(&DOCUMENT_PARSE_CALLS);
    reset(&DOCUMENT_PARSE_TOTAL_NANOS);
    reset(&DOCUMENT_PARSE_ERROR_CALLS);
    reset(&PARSED_FILE_PARSE_CALLS);
    reset(&PARSED_FILE_PARSE_TOTAL_NANOS);
    reset(&PARSED_FILE_ARTIFACT_CACHE_HITS);
    reset(&PARSED_FILE_ARTIFACT_CACHE_MISSES);
    reset(&PARSED_FILE_QUERY_HITS);
    reset(&PARSED_FILE_QUERY_MISSES);
    reset(&RECOVERED_FILE_QUERY_HITS);
    reset(&RECOVERED_FILE_QUERY_MISSES);
    reset(&FILE_ITEM_INDEX_QUERY_HITS);
    reset(&FILE_ITEM_INDEX_QUERY_MISSES);
    reset(&DECLARATION_INDEX_QUERY_HITS);
    reset(&DECLARATION_INDEX_QUERY_MISSES);
    reset(&SCOPE_QUERY_HITS);
    reset(&SCOPE_QUERY_MISSES);
    reset(&SOURCE_SET_PACKAGE_MEMBERSHIP_QUERY_HITS);
    reset(&SOURCE_SET_PACKAGE_MEMBERSHIP_QUERY_MISSES);
    reset(&ORPHAN_PACKAGE_MEMBERSHIP_QUERY_HITS);
    reset(&ORPHAN_PACKAGE_MEMBERSHIP_QUERY_MISSES);
    reset(&NAMESPACE_INDEX_QUERY_HITS);
    reset(&NAMESPACE_INDEX_QUERY_MISSES);
    reset(&WORKSPACE_SYMBOL_QUERY_HITS);
    reset(&WORKSPACE_SYMBOL_QUERY_MISSES);
    reset(&DOCUMENT_SYMBOL_QUERY_HITS);
    reset(&DOCUMENT_SYMBOL_QUERY_MISSES);
    reset(&SOURCE_ROOT_FILES_PARSED);
    reset(&SOURCE_ROOT_CACHE_HITS);
    reset(&SOURCE_ROOT_CACHE_MISSES);
    reset(&SOURCE_ROOT_CACHE_DISABLED);
    reset(&STANDARD_RESOLVED_BUILDS);
    reset(&STANDARD_RESOLVED_BUILD_TOTAL_NANOS);
    reset(&STANDARD_RESOLVED_CACHE_HITS);
    reset(&STRICT_RESOLVED_BUILDS);
    reset(&STRICT_RESOLVED_BUILD_TOTAL_NANOS);
    reset(&SEMANTIC_NAVIGATION_CACHE_HITS);
    reset(&SEMANTIC_NAVIGATION_CACHE_MISSES);
    reset(&SEMANTIC_NAVIGATION_BUILDS);
    reset(&INTERFACE_SEMANTIC_DIAGNOSTICS_CACHE_HITS);
    reset(&INTERFACE_SEMANTIC_DIAGNOSTICS_CACHE_MISSES);
    reset(&INTERFACE_SEMANTIC_DIAGNOSTICS_BUILDS);
    reset(&BODY_SEMANTIC_DIAGNOSTICS_CACHE_HITS);
    reset(&BODY_SEMANTIC_DIAGNOSTICS_CACHE_MISSES);
    reset(&BODY_SEMANTIC_DIAGNOSTICS_BUILDS);
    reset(&MODEL_STAGE_SEMANTIC_DIAGNOSTICS_CACHE_HITS);
    reset(&MODEL_STAGE_SEMANTIC_DIAGNOSTICS_CACHE_MISSES);
    reset(&MODEL_STAGE_SEMANTIC_DIAGNOSTICS_BUILDS);
    reset(&INSTANTIATED_MODEL_CACHE_HITS);
    reset(&INSTANTIATED_MODEL_CACHE_MISSES);
    reset(&INSTANTIATED_MODEL_BUILDS);
    reset(&TYPED_MODEL_CACHE_HITS);
    reset(&TYPED_MODEL_CACHE_MISSES);
    reset(&TYPED_MODEL_BUILDS);
    reset(&FLAT_MODEL_CACHE_HITS);
    reset(&FLAT_MODEL_CACHE_MISSES);
    reset(&FLAT_MODEL_BUILDS);
    reset(&DAE_MODEL_CACHE_HITS);
    reset(&DAE_MODEL_CACHE_MISSES);
    reset(&DAE_MODEL_BUILDS);
    reset(&NAMESPACE_COMPLETION_CACHE_HITS);
    reset(&NAMESPACE_COMPLETION_CACHE_MISSES);
    reset(&NAMESPACE_REFRESH_COLLECT_MS);
    reset(&NAMESPACE_REFRESH_BUILD_MS);
    reset(&NAMESPACE_REFRESH_FINALIZE_MS);
    reset(&RESOLVED_STATE_INVALIDATIONS);
    reset(&STRICT_RESOLVED_STATE_INVALIDATIONS);
    reset(&NAMESPACE_COMPLETION_STATE_INVALIDATIONS);
    reset(&DOCUMENT_MUTATION_INVALIDATIONS);
    reset(&SOURCE_SET_MUTATION_INVALIDATIONS);
    reset(&DOCUMENT_REMOVAL_INVALIDATIONS);
}

pub fn session_cache_stats() -> SessionCacheStatsSnapshot {
    SessionCacheStatsSnapshot {
        document_parse_calls: load(&DOCUMENT_PARSE_CALLS),
        document_parse_total_nanos: load(&DOCUMENT_PARSE_TOTAL_NANOS),
        document_parse_error_calls: load(&DOCUMENT_PARSE_ERROR_CALLS),
        parsed_file_parse_calls: load(&PARSED_FILE_PARSE_CALLS),
        parsed_file_parse_total_nanos: load(&PARSED_FILE_PARSE_TOTAL_NANOS),
        parsed_file_artifact_cache_hits: load(&PARSED_FILE_ARTIFACT_CACHE_HITS),
        parsed_file_artifact_cache_misses: load(&PARSED_FILE_ARTIFACT_CACHE_MISSES),
        parsed_file_query_hits: load(&PARSED_FILE_QUERY_HITS),
        parsed_file_query_misses: load(&PARSED_FILE_QUERY_MISSES),
        recovered_file_query_hits: load(&RECOVERED_FILE_QUERY_HITS),
        recovered_file_query_misses: load(&RECOVERED_FILE_QUERY_MISSES),
        file_item_index_query_hits: load(&FILE_ITEM_INDEX_QUERY_HITS),
        file_item_index_query_misses: load(&FILE_ITEM_INDEX_QUERY_MISSES),
        declaration_index_query_hits: load(&DECLARATION_INDEX_QUERY_HITS),
        declaration_index_query_misses: load(&DECLARATION_INDEX_QUERY_MISSES),
        scope_query_hits: load(&SCOPE_QUERY_HITS),
        scope_query_misses: load(&SCOPE_QUERY_MISSES),
        source_set_package_membership_query_hits: load(&SOURCE_SET_PACKAGE_MEMBERSHIP_QUERY_HITS),
        source_set_package_membership_query_misses: load(
            &SOURCE_SET_PACKAGE_MEMBERSHIP_QUERY_MISSES,
        ),
        orphan_package_membership_query_hits: load(&ORPHAN_PACKAGE_MEMBERSHIP_QUERY_HITS),
        orphan_package_membership_query_misses: load(&ORPHAN_PACKAGE_MEMBERSHIP_QUERY_MISSES),
        namespace_index_query_hits: load(&NAMESPACE_INDEX_QUERY_HITS),
        namespace_index_query_misses: load(&NAMESPACE_INDEX_QUERY_MISSES),
        workspace_symbol_query_hits: load(&WORKSPACE_SYMBOL_QUERY_HITS),
        workspace_symbol_query_misses: load(&WORKSPACE_SYMBOL_QUERY_MISSES),
        document_symbol_query_hits: load(&DOCUMENT_SYMBOL_QUERY_HITS),
        document_symbol_query_misses: load(&DOCUMENT_SYMBOL_QUERY_MISSES),
        source_root_files_parsed: load(&SOURCE_ROOT_FILES_PARSED),
        source_root_cache_hits: load(&SOURCE_ROOT_CACHE_HITS),
        source_root_cache_misses: load(&SOURCE_ROOT_CACHE_MISSES),
        source_root_cache_disabled: load(&SOURCE_ROOT_CACHE_DISABLED),
        standard_resolved_builds: load(&STANDARD_RESOLVED_BUILDS),
        standard_resolved_build_total_nanos: load(&STANDARD_RESOLVED_BUILD_TOTAL_NANOS),
        standard_resolved_cache_hits: load(&STANDARD_RESOLVED_CACHE_HITS),
        strict_resolved_builds: load(&STRICT_RESOLVED_BUILDS),
        strict_resolved_build_total_nanos: load(&STRICT_RESOLVED_BUILD_TOTAL_NANOS),
        semantic_navigation_cache_hits: load(&SEMANTIC_NAVIGATION_CACHE_HITS),
        semantic_navigation_cache_misses: load(&SEMANTIC_NAVIGATION_CACHE_MISSES),
        semantic_navigation_builds: load(&SEMANTIC_NAVIGATION_BUILDS),
        interface_semantic_diagnostics_cache_hits: load(&INTERFACE_SEMANTIC_DIAGNOSTICS_CACHE_HITS),
        interface_semantic_diagnostics_cache_misses: load(
            &INTERFACE_SEMANTIC_DIAGNOSTICS_CACHE_MISSES,
        ),
        interface_semantic_diagnostics_builds: load(&INTERFACE_SEMANTIC_DIAGNOSTICS_BUILDS),
        body_semantic_diagnostics_cache_hits: load(&BODY_SEMANTIC_DIAGNOSTICS_CACHE_HITS),
        body_semantic_diagnostics_cache_misses: load(&BODY_SEMANTIC_DIAGNOSTICS_CACHE_MISSES),
        body_semantic_diagnostics_builds: load(&BODY_SEMANTIC_DIAGNOSTICS_BUILDS),
        model_stage_semantic_diagnostics_cache_hits: load(
            &MODEL_STAGE_SEMANTIC_DIAGNOSTICS_CACHE_HITS,
        ),
        model_stage_semantic_diagnostics_cache_misses: load(
            &MODEL_STAGE_SEMANTIC_DIAGNOSTICS_CACHE_MISSES,
        ),
        model_stage_semantic_diagnostics_builds: load(&MODEL_STAGE_SEMANTIC_DIAGNOSTICS_BUILDS),
        instantiated_model_cache_hits: load(&INSTANTIATED_MODEL_CACHE_HITS),
        instantiated_model_cache_misses: load(&INSTANTIATED_MODEL_CACHE_MISSES),
        instantiated_model_builds: load(&INSTANTIATED_MODEL_BUILDS),
        typed_model_cache_hits: load(&TYPED_MODEL_CACHE_HITS),
        typed_model_cache_misses: load(&TYPED_MODEL_CACHE_MISSES),
        typed_model_builds: load(&TYPED_MODEL_BUILDS),
        flat_model_cache_hits: load(&FLAT_MODEL_CACHE_HITS),
        flat_model_cache_misses: load(&FLAT_MODEL_CACHE_MISSES),
        flat_model_builds: load(&FLAT_MODEL_BUILDS),
        dae_model_cache_hits: load(&DAE_MODEL_CACHE_HITS),
        dae_model_cache_misses: load(&DAE_MODEL_CACHE_MISSES),
        dae_model_builds: load(&DAE_MODEL_BUILDS),
        namespace_completion_cache_hits: load(&NAMESPACE_COMPLETION_CACHE_HITS),
        namespace_completion_cache_misses: load(&NAMESPACE_COMPLETION_CACHE_MISSES),
        namespace_refresh_collect_ms: load(&NAMESPACE_REFRESH_COLLECT_MS),
        namespace_refresh_build_ms: load(&NAMESPACE_REFRESH_BUILD_MS),
        namespace_refresh_finalize_ms: load(&NAMESPACE_REFRESH_FINALIZE_MS),
        resolved_state_invalidations: load(&RESOLVED_STATE_INVALIDATIONS),
        strict_resolved_state_invalidations: load(&STRICT_RESOLVED_STATE_INVALIDATIONS),
        namespace_completion_state_invalidations: load(&NAMESPACE_COMPLETION_STATE_INVALIDATIONS),
        document_mutation_invalidations: load(&DOCUMENT_MUTATION_INVALIDATIONS),
        source_set_mutation_invalidations: load(&SOURCE_SET_MUTATION_INVALIDATIONS),
        document_removal_invalidations: load(&DOCUMENT_REMOVAL_INVALIDATIONS),
    }
}
