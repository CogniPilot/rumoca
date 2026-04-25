use lsp_types::Position;
use rumoca_compile::compile::SessionCacheStatsSnapshot;
use serde::{Deserialize, Serialize};

use crate::helpers::get_text_before_cursor;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionTimingSummary {
    pub requested_edit_epoch: u64,
    pub request_was_stale: bool,
    pub uri: String,
    pub semantic_layer: String,
    pub source_root_load_ms: u64,
    pub completion_source_root_load_ms: u64,
    pub namespace_completion_prime_ms: u64,
    pub needs_resolved_session: bool,
    pub ast_fast_path_matched: bool,
    pub query_fast_path_check_ms: u64,
    pub query_fast_path_matched: bool,
    pub resolved_build_ms: Option<u64>,
    pub completion_handler_ms: u64,
    pub total_ms: u64,
    pub built_resolved_tree: bool,
    pub had_resolved_cache_before: bool,
    pub namespace_index_query_hits: u64,
    pub namespace_index_query_misses: u64,
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
    pub class_name_count_after_ensure: usize,
    pub session_cache_delta: SessionCacheStatsSnapshot,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompletionProgressSummary {
    pub requested_edit_epoch: u64,
    pub uri: String,
    pub stage: String,
    pub status: String,
    pub elapsed_ms: u64,
    pub completion_prefix: Option<String>,
    pub needs_resolved_session: Option<bool>,
    pub query_fast_path_matched: Option<bool>,
    pub detail: Option<String>,
}

pub fn extract_import_completion_prefix(source: &str, position: Position) -> Option<String> {
    let line = source.lines().nth(position.line as usize)?;
    let line_prefix = line.get(..position.character as usize)?;
    let trimmed = line_prefix.trim_start();
    if !trimmed.starts_with("import ") {
        return None;
    }

    let after_import = trimmed["import".len()..].trim_start();
    let token: String = after_import
        .chars()
        .take_while(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '.')
        .collect();
    (!token.is_empty()).then_some(token)
}

pub fn extract_namespace_completion_prefix(source: &str, position: Position) -> Option<String> {
    if let Some(import_prefix) = extract_import_completion_prefix(source, position) {
        return Some(import_prefix);
    }

    let prefix = get_text_before_cursor(source, position).unwrap_or_default();
    let token: String = prefix
        .chars()
        .rev()
        .take_while(|c| c.is_ascii_alphanumeric() || *c == '_' || *c == '.')
        .collect::<String>()
        .chars()
        .rev()
        .collect();
    if !token.contains('.') {
        return None;
    }

    let head = token.split('.').next().unwrap_or(token.as_str());
    head.chars()
        .next()
        .filter(|c| c.is_ascii_uppercase())
        .map(|_| token)
}
