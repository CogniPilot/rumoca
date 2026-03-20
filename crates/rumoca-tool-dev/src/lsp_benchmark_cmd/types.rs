use super::*;

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct LspCompletionProbeReport {
    pub(crate) probe_name: String,
    pub(crate) document_path: String,
    pub(crate) cold: CompletionMeasurementReport,
    pub(crate) warm: CompletionMeasurementReport,
    pub(crate) edited: CompletionMeasurementReport,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CompletionMeasurementReport {
    pub(crate) client_ms: u64,
    pub(crate) completion_count: usize,
    pub(crate) expected_completion_present: bool,
    pub(crate) preceding_edit_client_ms: Option<u64>,
    pub(crate) preceding_live_diagnostics_ms: Option<u64>,
    pub(crate) preceding_document_parse_ms: Option<u64>,
    pub(crate) lsp: CompletionTimingEntry,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct LspApiValidationReport {
    pub(crate) operations: Vec<LspApiValidationEntry>,
    pub(crate) warm_latency_snapshot: WarmLatencySnapshot,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct LspApiValidationEntry {
    pub(crate) operation: String,
    pub(crate) kind: String,
    pub(crate) ok: bool,
    pub(crate) client_ms: Option<u64>,
    pub(crate) detail: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct StartupTimingEntry {
    pub(crate) initial_library_paths: usize,
    pub(crate) library_paths_changed: bool,
    pub(crate) parse_init_options_ms: u64,
    pub(crate) workspace_root_ms: u64,
    pub(crate) reload_project_config_ms: u64,
    pub(crate) project_discover_ms: u64,
    pub(crate) resolve_library_paths_ms: u64,
    pub(crate) reset_session_ms: u64,
    pub(crate) durable_prewarm_ms: u64,
    #[serde(default)]
    pub(crate) durable_collect_files_ms: u64,
    #[serde(default)]
    pub(crate) durable_hash_inputs_ms: u64,
    #[serde(default)]
    pub(crate) durable_cache_lookup_ms: u64,
    #[serde(default)]
    pub(crate) durable_cache_deserialize_ms: u64,
    #[serde(default)]
    pub(crate) durable_parse_files_ms: u64,
    #[serde(default)]
    pub(crate) durable_validate_layout_ms: u64,
    #[serde(default)]
    pub(crate) durable_cache_write_ms: u64,
    #[serde(default)]
    pub(crate) durable_apply_ms: u64,
    pub(crate) workspace_symbol_prewarm_ms: u64,
    pub(crate) namespace_prewarm_spawn_ms: u64,
    pub(crate) total_ms: u64,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct WarmLatencySnapshot {
    pub(crate) measurements: Vec<WarmLatencyMeasurement>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, Default)]
#[serde(rename_all = "snake_case")]
pub(crate) enum NavigationRequestPath {
    #[default]
    Unknown,
    QueryOnly,
    FlatPreview,
}

impl NavigationRequestPath {
    pub(crate) fn label(self) -> &'static str {
        match self {
            Self::Unknown => "unknown",
            Self::QueryOnly => "query-only",
            Self::FlatPreview => "flat-preview",
        }
    }
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct WarmLatencyMeasurement {
    pub(crate) label: String,
    pub(crate) request_path: Option<NavigationRequestPath>,
    pub(crate) semantic_layer: Option<String>,
    pub(crate) samples: usize,
    pub(crate) p50_ms: u64,
    pub(crate) p95_ms: u64,
    pub(crate) budget_ms: u64,
    pub(crate) within_budget: bool,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct RuntimeSmokeReport {
    pub(crate) area: String,
    pub(crate) status: String,
    pub(crate) note: String,
    pub(crate) entries: Vec<RuntimeSmokeEntry>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct RuntimeSmokeEntry {
    pub(crate) operation: String,
    pub(crate) ok: bool,
    pub(crate) client_ms: Option<u64>,
    pub(crate) detail: String,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SurfaceCoverageReport {
    pub(crate) area: String,
    pub(crate) entries: Vec<SurfaceCoverageEntry>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SurfaceCoverageEntry {
    pub(crate) surface: String,
    pub(crate) kind: String,
    pub(crate) ok: bool,
    pub(crate) proof: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CompletionTimingEntry {
    #[serde(default)]
    pub(crate) requested_edit_epoch: u64,
    #[serde(default)]
    pub(crate) request_was_stale: bool,
    pub(crate) uri: String,
    #[serde(default)]
    pub(crate) semantic_layer: String,
    pub(crate) source_library_load_ms: u64,
    pub(crate) completion_library_load_ms: u64,
    #[serde(default)]
    pub(crate) library_completion_prime_ms: u64,
    #[serde(default)]
    pub(crate) needs_resolved_session: bool,
    #[serde(default)]
    pub(crate) ast_fast_path_matched: bool,
    #[serde(default)]
    pub(crate) query_fast_path_check_ms: u64,
    #[serde(default)]
    pub(crate) query_fast_path_matched: bool,
    pub(crate) resolved_build_ms: Option<u64>,
    pub(crate) completion_handler_ms: u64,
    pub(crate) total_ms: u64,
    pub(crate) built_resolved_tree: bool,
    pub(crate) had_resolved_cache_before: bool,
    #[serde(default)]
    pub(crate) namespace_index_query_hits: u64,
    #[serde(default)]
    pub(crate) namespace_index_query_misses: u64,
    #[serde(default)]
    pub(crate) file_item_index_query_hits: u64,
    #[serde(default)]
    pub(crate) file_item_index_query_misses: u64,
    #[serde(default)]
    pub(crate) declaration_index_query_hits: u64,
    #[serde(default)]
    pub(crate) declaration_index_query_misses: u64,
    #[serde(default)]
    pub(crate) scope_query_hits: u64,
    #[serde(default)]
    pub(crate) scope_query_misses: u64,
    #[serde(default)]
    pub(crate) source_set_package_membership_query_hits: u64,
    #[serde(default)]
    pub(crate) source_set_package_membership_query_misses: u64,
    #[serde(default)]
    pub(crate) orphan_package_membership_query_hits: u64,
    #[serde(default)]
    pub(crate) orphan_package_membership_query_misses: u64,
    #[serde(default)]
    pub(crate) class_name_count_after_ensure: usize,
    pub(crate) session_cache_delta: CompletionSessionCacheDelta,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CompletionProgressEntry {
    #[serde(default)]
    pub(crate) requested_edit_epoch: u64,
    pub(crate) uri: String,
    pub(crate) stage: String,
    pub(crate) status: String,
    pub(crate) elapsed_ms: u64,
    pub(crate) completion_prefix: Option<String>,
    pub(crate) needs_resolved_session: Option<bool>,
    pub(crate) query_fast_path_matched: Option<bool>,
    #[serde(default)]
    pub(crate) detail: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct DiagnosticsTimingEntry {
    #[serde(default)]
    pub(crate) requested_edit_epoch: u64,
    #[serde(default)]
    pub(crate) request_was_stale: bool,
    pub(crate) uri: String,
    pub(crate) trigger: String,
    #[serde(default)]
    pub(crate) semantic_layer: String,
    pub(crate) requested_library_load: bool,
    pub(crate) library_load_ms: u64,
    pub(crate) ran_compile: bool,
    pub(crate) diagnostics_compute_ms: u64,
    pub(crate) total_ms: u64,
    pub(crate) session_cache_delta: CompletionSessionCacheDelta,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct NavigationTimingEntry {
    #[serde(default)]
    pub(crate) requested_edit_epoch: u64,
    #[serde(default)]
    pub(crate) request_was_stale: bool,
    pub(crate) uri: String,
    pub(crate) request: String,
    #[serde(default)]
    pub(crate) request_path: NavigationRequestPath,
    #[serde(default)]
    pub(crate) semantic_layer: String,
    pub(crate) total_ms: u64,
    #[serde(default)]
    pub(crate) snapshot_ms: Option<u64>,
    #[serde(default)]
    pub(crate) snapshot_lock_ms: Option<u64>,
    #[serde(default)]
    pub(crate) snapshot_build_ms: Option<u64>,
    #[serde(default)]
    pub(crate) detail: Option<String>,
    #[serde(default)]
    pub(crate) query_ms: Option<u64>,
    #[serde(default)]
    pub(crate) format_ms: Option<u64>,
    pub(crate) built_resolved_tree: bool,
    pub(crate) had_resolved_cache_before: bool,
    pub(crate) session_cache_delta: CompletionSessionCacheDelta,
}

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
#[serde(rename_all = "camelCase")]
#[serde(default)]
pub(crate) struct CompletionSessionCacheDelta {
    #[serde(alias = "document_parse_calls")]
    pub(crate) document_parse_calls: u64,
    #[serde(alias = "document_parse_total_nanos")]
    pub(crate) document_parse_total_nanos: u64,
    #[serde(alias = "parsed_file_parse_calls")]
    pub(crate) parsed_file_parse_calls: u64,
    #[serde(alias = "parsed_file_parse_total_nanos")]
    pub(crate) parsed_file_parse_total_nanos: u64,
    #[serde(alias = "file_item_index_query_hits")]
    pub(crate) file_item_index_query_hits: u64,
    #[serde(alias = "file_item_index_query_misses")]
    pub(crate) file_item_index_query_misses: u64,
    #[serde(alias = "declaration_index_query_hits")]
    pub(crate) declaration_index_query_hits: u64,
    #[serde(alias = "declaration_index_query_misses")]
    pub(crate) declaration_index_query_misses: u64,
    #[serde(alias = "scope_query_hits")]
    pub(crate) scope_query_hits: u64,
    #[serde(alias = "scope_query_misses")]
    pub(crate) scope_query_misses: u64,
    #[serde(alias = "source_set_package_membership_query_hits")]
    pub(crate) source_set_package_membership_query_hits: u64,
    #[serde(alias = "source_set_package_membership_query_misses")]
    pub(crate) source_set_package_membership_query_misses: u64,
    #[serde(alias = "orphan_package_membership_query_hits")]
    pub(crate) orphan_package_membership_query_hits: u64,
    #[serde(alias = "orphan_package_membership_query_misses")]
    pub(crate) orphan_package_membership_query_misses: u64,
    #[serde(alias = "standard_resolved_builds")]
    pub(crate) standard_resolved_builds: u64,
    #[serde(alias = "strict_resolved_builds")]
    pub(crate) strict_resolved_builds: u64,
    #[serde(alias = "semantic_navigation_cache_hits")]
    pub(crate) semantic_navigation_cache_hits: u64,
    #[serde(alias = "semantic_navigation_cache_misses")]
    pub(crate) semantic_navigation_cache_misses: u64,
    #[serde(alias = "semantic_navigation_builds")]
    pub(crate) semantic_navigation_builds: u64,
    #[serde(alias = "library_completion_cache_hits")]
    pub(crate) library_completion_cache_hits: u64,
    #[serde(alias = "library_completion_cache_misses")]
    pub(crate) library_completion_cache_misses: u64,
    #[serde(alias = "interface_semantic_diagnostics_cache_hits")]
    pub(crate) interface_semantic_diagnostics_cache_hits: u64,
    #[serde(alias = "interface_semantic_diagnostics_cache_misses")]
    pub(crate) interface_semantic_diagnostics_cache_misses: u64,
    #[serde(alias = "interface_semantic_diagnostics_builds")]
    pub(crate) interface_semantic_diagnostics_builds: u64,
    #[serde(alias = "body_semantic_diagnostics_cache_hits")]
    pub(crate) body_semantic_diagnostics_cache_hits: u64,
    #[serde(alias = "body_semantic_diagnostics_cache_misses")]
    pub(crate) body_semantic_diagnostics_cache_misses: u64,
    #[serde(alias = "body_semantic_diagnostics_builds")]
    pub(crate) body_semantic_diagnostics_builds: u64,
    #[serde(alias = "model_stage_semantic_diagnostics_cache_hits")]
    pub(crate) model_stage_semantic_diagnostics_cache_hits: u64,
    #[serde(alias = "model_stage_semantic_diagnostics_cache_misses")]
    pub(crate) model_stage_semantic_diagnostics_cache_misses: u64,
    #[serde(alias = "model_stage_semantic_diagnostics_builds")]
    pub(crate) model_stage_semantic_diagnostics_builds: u64,
    #[serde(alias = "instantiated_model_cache_hits")]
    pub(crate) instantiated_model_cache_hits: u64,
    #[serde(alias = "instantiated_model_cache_misses")]
    pub(crate) instantiated_model_cache_misses: u64,
    #[serde(alias = "instantiated_model_builds")]
    pub(crate) instantiated_model_builds: u64,
    #[serde(alias = "typed_model_cache_hits")]
    pub(crate) typed_model_cache_hits: u64,
    #[serde(alias = "typed_model_cache_misses")]
    pub(crate) typed_model_cache_misses: u64,
    #[serde(alias = "typed_model_builds")]
    pub(crate) typed_model_builds: u64,
    #[serde(alias = "flat_model_cache_hits")]
    pub(crate) flat_model_cache_hits: u64,
    #[serde(alias = "flat_model_cache_misses")]
    pub(crate) flat_model_cache_misses: u64,
    #[serde(alias = "flat_model_builds")]
    pub(crate) flat_model_builds: u64,
    #[serde(alias = "dae_model_cache_hits")]
    pub(crate) dae_model_cache_hits: u64,
    #[serde(alias = "dae_model_cache_misses")]
    pub(crate) dae_model_cache_misses: u64,
    #[serde(alias = "dae_model_builds")]
    pub(crate) dae_model_builds: u64,
}

#[derive(Debug, Clone)]
pub(crate) struct LspTimingPaths {
    pub(crate) startup: Option<PathBuf>,
    pub(crate) completion: Option<PathBuf>,
    pub(crate) completion_progress: Option<PathBuf>,
    pub(crate) diagnostics: Option<PathBuf>,
    pub(crate) navigation: Option<PathBuf>,
}
