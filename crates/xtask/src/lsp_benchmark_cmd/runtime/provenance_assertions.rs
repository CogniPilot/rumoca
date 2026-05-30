use super::cache_assertions::ensure_no_model_query_activity;
use super::*;

pub(super) fn navigation_entries_since(
    path: &Path,
    start: usize,
    request: &str,
    uri: &str,
) -> Result<Vec<NavigationTimingEntry>> {
    let entries = read_navigation_timings(path)?;
    Ok(entries
        .into_iter()
        .skip(start)
        .filter(|entry| entry.request == request && entry.uri == uri)
        .collect())
}

pub(super) fn navigation_request_entries_since(
    path: &Path,
    start: usize,
    request: &str,
) -> Result<Vec<NavigationTimingEntry>> {
    let entries = read_navigation_timings(path)?;
    Ok(entries
        .into_iter()
        .skip(start)
        .filter(|entry| entry.request == request)
        .collect())
}

pub(super) fn diagnostics_entries_since(
    path: &Path,
    start: usize,
    trigger: &str,
    uri: &str,
) -> Result<Vec<DiagnosticsTimingEntry>> {
    let entries = read_diagnostics_timings(path)?;
    Ok(entries
        .into_iter()
        .skip(start)
        .filter(|entry| entry.trigger == trigger && entry.uri == uri)
        .collect())
}

pub(super) fn ensure_navigation_entry_count(
    request: &str,
    entries: &[NavigationTimingEntry],
    expected: usize,
) -> Result<()> {
    ensure!(
        entries.len() == expected,
        "expected {expected} navigation timing entries for {request}, got {}",
        entries.len()
    );
    Ok(())
}

pub(super) fn ensure_navigation_query_only_entries(
    request: &str,
    entries: &[NavigationTimingEntry],
    expected_layer: &str,
) -> Result<()> {
    for (idx, entry) in entries.iter().enumerate() {
        ensure!(
            !entry.request_was_stale,
            "{request} entry {idx} should not be marked stale"
        );
        ensure!(
            entry.request_path == NavigationRequestPath::QueryOnly,
            "{request} entry {idx} should be recorded as query-only, got {}",
            entry.request_path.label()
        );
        ensure!(
            entry.semantic_layer == expected_layer,
            "{request} entry {idx} should report semantic layer `{expected_layer}`, got `{}`",
            entry.semantic_layer
        );
        ensure!(
            entry.session_cache_delta.semantic_navigation_builds == 0,
            "{request} entry {idx} should not build semantic navigation on the query path"
        );
        ensure!(
            entry.session_cache_delta.semantic_navigation_cache_hits == 0,
            "{request} entry {idx} should not touch the navigation cache on the query path"
        );
        ensure!(
            entry.session_cache_delta.strict_resolved_builds == 0,
            "{request} entry {idx} should not build strict resolved state"
        );
        ensure!(
            entry.session_cache_delta.standard_resolved_builds == 0,
            "{request} entry {idx} should not build the standard resolved session"
        );
        ensure!(
            !entry.built_resolved_tree,
            "{request} entry {idx} should stay on the query-only path"
        );
        ensure_no_model_query_activity(
            &format!("{request} entry {idx}"),
            &entry.session_cache_delta,
        )?;
    }
    Ok(())
}

pub(super) fn ensure_single_navigation_fast_path(
    request: &str,
    entries: &[NavigationTimingEntry],
) -> Result<()> {
    ensure_navigation_entry_count(request, entries, 1)?;
    ensure_navigation_query_only_entries(request, entries, "class_interface")
}

pub(super) fn ensure_completion_entry_count(
    request: &str,
    entries: &[CompletionTimingEntry],
    expected: usize,
) -> Result<()> {
    ensure!(
        entries.len() == expected,
        "expected {expected} completion timing entries for {request}, got {}",
        entries.len()
    );
    Ok(())
}

pub(super) fn ensure_completion_entries_not_stale(
    request: &str,
    entries: &[CompletionTimingEntry],
) -> Result<()> {
    for (idx, entry) in entries.iter().enumerate() {
        ensure!(
            !entry.request_was_stale,
            "{request} entry {idx} should not be marked stale"
        );
    }
    Ok(())
}

pub(super) fn ensure_completion_entries_have_semantic_layer(
    request: &str,
    entries: &[CompletionTimingEntry],
    expected_layer: &str,
) -> Result<()> {
    for (idx, entry) in entries.iter().enumerate() {
        ensure!(
            entry.semantic_layer == expected_layer,
            "{request} entry {idx} should report semantic layer `{expected_layer}`, got `{}`",
            entry.semantic_layer
        );
    }
    Ok(())
}

pub(super) fn ensure_completion_query_fast_path_entries(
    request: &str,
    entries: &[CompletionTimingEntry],
    expected_layer: &str,
) -> Result<()> {
    ensure_completion_entries_not_stale(request, entries)?;
    ensure_completion_entries_have_semantic_layer(request, entries, expected_layer)?;
    for (idx, entry) in entries.iter().enumerate() {
        ensure!(
            !entry.built_resolved_tree,
            "{request} entry {idx} should stay off resolved-tree completion prep"
        );
        ensure!(
            entry.resolved_build_ms.is_none(),
            "{request} entry {idx} should not report resolved build timing"
        );
        ensure!(
            entry.session_cache_delta.semantic_navigation_builds == 0,
            "{request} entry {idx} should not build semantic navigation"
        );
        ensure!(
            entry.session_cache_delta.strict_resolved_builds == 0,
            "{request} entry {idx} should not build strict resolved state"
        );
        ensure!(
            entry.session_cache_delta.standard_resolved_builds == 0,
            "{request} entry {idx} should not build the standard resolved session"
        );
        ensure_no_model_query_activity(
            &format!("{request} entry {idx}"),
            &entry.session_cache_delta,
        )?;
    }
    Ok(())
}

pub(super) fn ensure_completion_query_fast_path(
    request: &str,
    entries: &[CompletionTimingEntry],
) -> Result<()> {
    ensure_completion_entry_count(request, entries, 1)?;
    ensure_completion_query_fast_path_entries(request, entries, "class_interface")
}

pub(super) fn ensure_diagnostics_entry_count(
    request: &str,
    entries: &[DiagnosticsTimingEntry],
    expected: usize,
) -> Result<()> {
    ensure!(
        entries.len() == expected,
        "expected {expected} diagnostics timing entries for {request}, got {}",
        entries.len()
    );
    Ok(())
}

pub(super) fn ensure_live_diagnostics_fast_path(
    request: &str,
    entries: &[DiagnosticsTimingEntry],
) -> Result<()> {
    for (idx, entry) in entries.iter().enumerate() {
        ensure!(
            !entry.request_was_stale,
            "{request} entry {idx} should not be marked stale"
        );
        ensure!(
            !entry.requested_source_root_load,
            "{request} entry {idx} should not request source-root loading"
        );
        ensure!(
            !entry.ran_compile,
            "{request} entry {idx} should stay off the strict compile path"
        );
        ensure!(
            entry.semantic_layer == "parse_only",
            "{request} entry {idx} should report parse_only diagnostics, got `{}`",
            entry.semantic_layer
        );
        ensure_no_model_query_activity(
            &format!("{request} entry {idx}"),
            &entry.session_cache_delta,
        )?;
    }
    Ok(())
}

fn is_compile_diagnostics_layer(layer: &str) -> bool {
    matches!(layer, "interface" | "body" | "model_stage")
}

pub(super) fn compile_diagnostics_semantic_layer<'a>(
    request: &str,
    entries: &'a [DiagnosticsTimingEntry],
    expected_layer: Option<&str>,
) -> Result<&'a str> {
    let semantic_layer = entries
        .first()
        .map(|entry| entry.semantic_layer.as_str())
        .context("missing diagnostics timing entries")?;
    if let Some(expected_layer) = expected_layer {
        ensure!(
            semantic_layer == expected_layer,
            "{request} should report {expected_layer} diagnostics, got `{semantic_layer}`"
        );
    } else {
        ensure!(
            is_compile_diagnostics_layer(semantic_layer),
            "{request} should report interface/body/model_stage diagnostics, got `{semantic_layer}`"
        );
    }
    for (idx, entry) in entries.iter().enumerate() {
        ensure!(
            entry.semantic_layer == semantic_layer,
            "{request} entry {idx} should preserve semantic layer `{semantic_layer}`, got `{}`",
            entry.semantic_layer
        );
    }
    Ok(semantic_layer)
}

pub(super) fn ensure_save_diagnostics_compile_path(
    request: &str,
    entries: &[DiagnosticsTimingEntry],
    expected_layer: Option<&str>,
) -> Result<()> {
    let semantic_layer = compile_diagnostics_semantic_layer(request, entries, expected_layer)?;
    for (idx, entry) in entries.iter().enumerate() {
        ensure!(
            !entry.request_was_stale,
            "{request} entry {idx} should not be marked stale"
        );
        ensure!(
            entry.requested_source_root_load,
            "{request} entry {idx} should request source-root loading"
        );
        ensure!(
            entry.ran_compile,
            "{request} entry {idx} should run the strict compile path"
        );
        ensure!(
            entry.semantic_layer == semantic_layer,
            "{request} entry {idx} should report `{semantic_layer}` diagnostics, got `{}`",
            entry.semantic_layer
        );
    }
    Ok(())
}

pub(super) fn ensure_diagnostics_validation_entries(
    entries: &[DiagnosticsTimingEntry],
) -> Result<()> {
    ensure_diagnostics_entry_count("didSave", entries, 2)?;
    ensure_save_diagnostics_compile_path("didSave", entries, None)?;
    for (idx, entry) in entries.iter().enumerate() {
        ensure!(
            entry.session_cache_delta.document_parse_calls == 0,
            "didSave entry {idx} should not reparse an already-open document",
        );
    }
    Ok(())
}
