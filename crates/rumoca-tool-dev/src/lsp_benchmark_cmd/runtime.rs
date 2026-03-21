use super::*;

use super::render::{
    append_tabled_section, render_completion_cache_stats_table,
    render_completion_edit_breakdown_table, render_completion_timing_breakdown_table,
    render_failure_details_table, render_lsp_api_validation_table, render_runtime_comparison_table,
    render_warm_latency_snapshot_table,
};

mod cache_assertions;
mod provenance_assertions;
use provenance_assertions::{
    compile_diagnostics_semantic_layer, diagnostics_entries_since,
    ensure_completion_entries_have_semantic_layer, ensure_completion_entries_not_stale,
    ensure_completion_entry_count, ensure_completion_query_fast_path,
    ensure_completion_query_fast_path_entries, ensure_diagnostics_entry_count,
    ensure_diagnostics_validation_entries, ensure_live_diagnostics_fast_path,
    ensure_navigation_entry_count, ensure_navigation_query_only_entries,
    ensure_save_diagnostics_compile_path, ensure_single_navigation_fast_path,
    navigation_entries_since, navigation_request_entries_since,
};

fn runtime_report_required() -> bool {
    matches!(
        env::var("RUMOCA_REQUIRE_EDITOR_RUNTIMES")
            .ok()
            .as_deref()
            .map(str::trim),
        Some("1" | "true" | "TRUE" | "yes" | "YES" | "on" | "ON")
    )
}

pub(crate) fn ensure_editor_runtime_optional(area: &str, detail: &str) -> Result<()> {
    ensure!(
        !runtime_report_required(),
        "{area} is required for this run but unavailable: {detail}"
    );
    Ok(())
}

pub(crate) fn skip_runtime_smoke_report(area: &str, note: &str) -> RuntimeSmokeReport {
    RuntimeSmokeReport {
        area: area.to_string(),
        status: "skip".to_string(),
        note: note.to_string(),
        entries: vec![],
    }
}

pub(crate) fn runtime_entry(
    operation: &str,
    ok: bool,
    client_ms: Option<u64>,
    detail: &str,
) -> RuntimeSmokeEntry {
    RuntimeSmokeEntry {
        operation: operation.to_string(),
        ok,
        client_ms,
        detail: detail.to_string(),
    }
}

struct ValidationWorkspace {
    _temp_dir: tempfile::TempDir,
    workspace_uri: Url,
    msl_uri: Url,
    msl_source: String,
    synthetic_uri: Url,
    formatting_uri: Url,
    broken_uri: Url,
    workspace_root: PathBuf,
}

struct FullMslValidationContext {
    msl_key: String,
    completion_position: ProbePosition,
    hover_position: ProbePosition,
    definition_position: ProbePosition,
}

struct SyntheticValidationContext {
    alias_position: ProbePosition,
    component_position: ProbePosition,
    completion_position: ProbePosition,
    signature_position: ProbePosition,
}

fn create_validation_workspace(msl_archive_root: &Path) -> Result<ValidationWorkspace> {
    let temp = tempdir().context("failed to create LSP validation workspace")?;
    let workspace_root = temp.path().to_path_buf();
    let synthetic_path = workspace_root.join("active.mo");
    let formatting_path = workspace_root.join("format.mo");
    let broken_path = workspace_root.join("broken.mo");
    fs::create_dir_all(workspace_root.join("Lib"))
        .with_context(|| format!("failed to create {}", workspace_root.join("Lib").display()))?;
    fs::write(
        workspace_root.join("Lib/package.mo"),
        "package Lib\nend Lib;\n",
    )
    .with_context(|| {
        format!(
            "failed to write {}",
            workspace_root.join("Lib/package.mo").display()
        )
    })?;
    fs::write(&synthetic_path, VALIDATION_SURFACE_SOURCE)
        .with_context(|| format!("failed to write {}", synthetic_path.display()))?;
    fs::write(&formatting_path, VALIDATION_FORMATTING_SOURCE)
        .with_context(|| format!("failed to write {}", formatting_path.display()))?;
    fs::write(&broken_path, VALIDATION_CODE_ACTION_SOURCE)
        .with_context(|| format!("failed to write {}", broken_path.display()))?;
    let msl_path = completion_probe_document_path(msl_archive_root, FULL_MSL_COMPLETION_PROBES[0])?;
    let msl_source = fs::read_to_string(&msl_path)
        .with_context(|| format!("failed to read {}", msl_path.display()))?;
    Ok(ValidationWorkspace {
        _temp_dir: temp,
        workspace_uri: directory_url(&workspace_root)?,
        msl_uri: file_url(&msl_path)?,
        msl_source,
        synthetic_uri: file_url(&synthetic_path)?,
        formatting_uri: file_url(&formatting_path)?,
        broken_uri: file_url(&broken_path)?,
        workspace_root,
    })
}

fn full_msl_validation_context(
    workspace: &ValidationWorkspace,
) -> Result<FullMslValidationContext> {
    Ok(FullMslValidationContext {
        msl_key: uri_path_string(&workspace.msl_uri)?,
        completion_position: completion_probe_position_from_source(&workspace.msl_source)?,
        hover_position: local_hover_probe_from_source(&workspace.msl_source)?,
        definition_position: local_definition_probe_from_source(&workspace.msl_source)?,
    })
}

fn synthetic_validation_context() -> SyntheticValidationContext {
    SyntheticValidationContext {
        alias_position: ProbePosition {
            line: 12,
            character: "  import Alias".len() as u32,
        },
        component_position: ProbePosition {
            line: 15,
            character: "  Alias helperInst".len() as u32,
        },
        completion_position: ProbePosition {
            line: 17,
            character: "  helperInst.".len() as u32,
        },
        signature_position: ProbePosition {
            line: 17,
            character: "  helperInst.y = sin(".len() as u32,
        },
    }
}

fn write_validation_file(workspace_root: &Path, filename: &str, source: &str) -> Result<Url> {
    let path = workspace_root.join(filename);
    fs::write(&path, source).with_context(|| format!("failed to write {}", path.display()))?;
    file_url(&path)
}

fn run_full_msl_lsp_validation(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    completion_timing_path: &Path,
    navigation_timing_path: &Path,
) -> Result<Vec<LspApiValidationEntry>> {
    let ctx = full_msl_validation_context(workspace)?;
    let mut entries =
        validate_full_msl_open_and_completion(client, workspace, &ctx, completion_timing_path)?;
    entries.extend(validate_full_msl_hover(
        client,
        workspace,
        &ctx,
        navigation_timing_path,
    )?);
    entries.extend(validate_full_msl_definition(
        client,
        workspace,
        &ctx,
        navigation_timing_path,
    )?);
    entries.extend(validate_full_msl_structure_requests(
        client,
        workspace,
        navigation_timing_path,
    )?);
    Ok(entries)
}

fn validate_full_msl_open_and_completion(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    ctx: &FullMslValidationContext,
    completion_timing_path: &Path,
) -> Result<Vec<LspApiValidationEntry>> {
    let opened = Instant::now();
    client.did_open(&workspace.msl_uri, &workspace.msl_source)?;
    let diagnostics =
        client.wait_for_publish_diagnostics(&workspace.msl_uri, DIAGNOSTICS_TIMEOUT)?;
    let open_ms = opened.elapsed().as_millis() as u64;
    let completion_start = read_completion_timings(completion_timing_path)?.len();
    let source_root_load_completion = client.completion(
        &workspace.msl_uri,
        ctx.completion_position,
        FULL_MSL_COMPLETION_PROBES[0].expected_completion_label,
    )?;
    let cold_completion = client.completion(
        &workspace.msl_uri,
        ctx.completion_position,
        FULL_MSL_COMPLETION_PROBES[0].expected_completion_label,
    )?;
    let warm_completion = client.completion(
        &workspace.msl_uri,
        ctx.completion_position,
        FULL_MSL_COMPLETION_PROBES[0].expected_completion_label,
    )?;
    ensure!(
        source_root_load_completion.expected_completion_present,
        "source-root-load completion probe missed expected label `{}`",
        FULL_MSL_COMPLETION_PROBES[0].expected_completion_label
    );
    ensure!(
        cold_completion.expected_completion_present,
        "full-MSL completion probe missed expected label `{}`",
        FULL_MSL_COMPLETION_PROBES[0].expected_completion_label
    );
    ensure!(
        warm_completion.expected_completion_present,
        "warm full-MSL completion probe missed expected label `{}`",
        FULL_MSL_COMPLETION_PROBES[0].expected_completion_label
    );
    let completion_timings =
        completion_entries_since(completion_timing_path, completion_start, &workspace.msl_uri)?;
    ensure!(
        completion_timings.len() == 3,
        "expected 3 completion timing entries for full-MSL validation, got {}",
        completion_timings.len()
    );
    ensure_completion_entries_have_semantic_layer(
        "full-MSL completion",
        &completion_timings,
        "package_def_map",
    )?;
    Ok(vec![
        ok_validation(
            "didOpen",
            "note",
            Some(open_ms),
            format!(
                "live-diag={}",
                diagnostics_from_notification(&diagnostics).len()
            ),
        ),
        ok_validation(
            "source-root-load",
            "req",
            Some(source_root_load_completion.client_ms),
            validation_completion_stage_detail(
                "msl",
                source_root_load_completion.completion_count,
                &completion_timings[0],
            ),
        ),
        ok_validation(
            "completion:cold",
            "req",
            Some(cold_completion.client_ms),
            validation_completion_stage_detail(
                "msl",
                cold_completion.completion_count,
                &completion_timings[1],
            ),
        ),
        ok_validation(
            "completion:warm",
            "req",
            Some(warm_completion.client_ms),
            validation_completion_stage_detail(
                "msl",
                warm_completion.completion_count,
                &completion_timings[2],
            ),
        ),
    ])
}

fn validate_full_msl_hover(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    ctx: &FullMslValidationContext,
    navigation_timing_path: &Path,
) -> Result<Vec<LspApiValidationEntry>> {
    let hover_params = json!({
        "textDocument": { "uri": workspace.msl_uri },
        "position": {
            "line": ctx.hover_position.line,
            "character": ctx.hover_position.character,
        }
    });
    let hover_start = read_navigation_timings(navigation_timing_path)?.len();
    let (hover_cold_ms, hover_cold_response) = client.request_timed(
        "textDocument/hover",
        hover_params.clone(),
        VALIDATION_TIMEOUT,
    )?;
    let (hover_warm_ms, hover_warm_response) =
        client.request_timed("textDocument/hover", hover_params, VALIDATION_TIMEOUT)?;
    let hover_payload = hover_text(&response_result(&hover_cold_response));
    ensure!(
        !hover_payload.trim().is_empty(),
        "full-MSL hover should return non-empty markdown content"
    );
    ensure!(
        hover_payload == hover_text(&response_result(&hover_warm_response)),
        "warm full-MSL hover should preserve the hover payload"
    );
    let hover_timings =
        navigation_entries_since(navigation_timing_path, hover_start, "hover", &ctx.msl_key)?;
    ensure_navigation_entry_count("hover", &hover_timings, 2)?;
    ensure_navigation_query_only_entries("hover", &hover_timings, "class_body_semantics")?;
    Ok(vec![
        ok_validation(
            "hover:cold",
            "req",
            Some(hover_cold_ms),
            format!(
                "full-msl chars={} {} layer={}={}ms",
                hover_payload.chars().count(),
                hover_timings[0].request_path.label(),
                hover_timings[0].semantic_layer,
                hover_timings[0].total_ms
            ),
        ),
        ok_validation(
            "hover:warm",
            "req",
            Some(hover_warm_ms),
            format!(
                "{} layer={}={}ms",
                hover_timings[1].request_path.label(),
                hover_timings[1].semantic_layer,
                hover_timings[1].total_ms
            ),
        ),
    ])
}

fn validate_full_msl_definition(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    ctx: &FullMslValidationContext,
    navigation_timing_path: &Path,
) -> Result<Vec<LspApiValidationEntry>> {
    let definition_params = json!({
        "textDocument": { "uri": workspace.msl_uri },
        "position": {
            "line": ctx.definition_position.line,
            "character": ctx.definition_position.character,
        }
    });
    let definition_start = read_navigation_timings(navigation_timing_path)?.len();
    let (definition_cold_ms, definition_cold_response) = client.request_timed(
        "textDocument/definition",
        definition_params.clone(),
        VALIDATION_TIMEOUT,
    )?;
    let (definition_warm_ms, definition_warm_response) = client.request_timed(
        "textDocument/definition",
        definition_params,
        VALIDATION_TIMEOUT,
    )?;
    let targets = definition_targets(&response_result(&definition_cold_response));
    ensure!(
        !targets.is_empty(),
        "full-MSL definition should return at least one target"
    );
    ensure!(
        targets == definition_targets(&response_result(&definition_warm_response)),
        "warm full-MSL definition should preserve the target set"
    );
    let definition_timings = navigation_entries_since(
        navigation_timing_path,
        definition_start,
        "definition",
        &ctx.msl_key,
    )?;
    ensure_navigation_entry_count("definition", &definition_timings, 2)?;
    ensure_navigation_query_only_entries(
        "definition",
        &definition_timings,
        "class_body_semantics",
    )?;
    Ok(vec![
        ok_validation(
            "definition:cold",
            "req",
            Some(definition_cold_ms),
            format!(
                "full-msl targets={} {} layer={}={}ms",
                targets.len(),
                definition_timings[0].request_path.label(),
                definition_timings[0].semantic_layer,
                definition_timings[0].total_ms
            ),
        ),
        ok_validation(
            "definition:warm",
            "req",
            Some(definition_warm_ms),
            format!(
                "{} layer={}={}ms",
                definition_timings[1].request_path.label(),
                definition_timings[1].semantic_layer,
                definition_timings[1].total_ms
            ),
        ),
    ])
}

fn validate_full_msl_structure_requests(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    navigation_timing_path: &Path,
) -> Result<Vec<LspApiValidationEntry>> {
    let (symbol_ms, symbol_response) = client.request_timed(
        "textDocument/documentSymbol",
        json!({ "textDocument": { "uri": workspace.msl_uri } }),
        VALIDATION_TIMEOUT,
    )?;
    let symbol_count = json_array_len(&response_result(&symbol_response));
    ensure!(
        symbol_count > 0,
        "full-MSL document symbols should not be empty"
    );

    let (tokens_ms, tokens_response) = client.request_timed(
        "textDocument/semanticTokens/full",
        json!({ "textDocument": { "uri": workspace.msl_uri } }),
        VALIDATION_TIMEOUT,
    )?;
    let token_count = semantic_token_count(&response_result(&tokens_response));
    ensure!(
        token_count > 0,
        "full-MSL semantic tokens should not be empty"
    );

    let workspace_start = read_navigation_timings(navigation_timing_path)?.len();
    let (workspace_ms, workspace_response) = client.request_timed(
        "workspace/symbol",
        json!({ "query": "Resistor" }),
        VALIDATION_TIMEOUT,
    )?;
    let workspace_hits = json_array_len(&response_result(&workspace_response));
    ensure!(
        workspace_hits > 0,
        "workspace/symbol should find Resistor in the open MSL document"
    );
    let workspace_timings = navigation_request_entries_since(
        navigation_timing_path,
        workspace_start,
        "workspace_symbol",
    )?;
    ensure_navigation_entry_count("workspace/symbol", &workspace_timings, 1)?;
    let workspace_entry = workspace_timings
        .last()
        .context("workspace/symbol timing entry should exist")?;

    let (lens_ms, lens_response) = client.request_timed(
        "textDocument/codeLens",
        json!({ "textDocument": { "uri": workspace.msl_uri } }),
        VALIDATION_TIMEOUT,
    )?;
    let lens_count = json_array_len(&response_result(&lens_response));
    ensure!(
        lens_count > 0,
        "code lens should produce at least one model result"
    );

    Ok(vec![
        ok_validation(
            "docSymbol",
            "req",
            Some(symbol_ms),
            format!("full-msl count={symbol_count}"),
        ),
        ok_validation(
            "semantic",
            "req",
            Some(tokens_ms),
            format!("full-msl data={token_count}"),
        ),
        ok_validation(
            "workspace/symbol",
            "req",
            Some(workspace_ms),
            workspace_symbol_detail("full-msl", workspace_hits, workspace_entry),
        ),
        ok_validation(
            "codeLens",
            "req",
            Some(lens_ms),
            format!("full-msl count={lens_count}"),
        ),
    ])
}

fn run_synthetic_lsp_validation(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    completion_timing_path: &Path,
    navigation_timing_path: &Path,
) -> Result<Vec<LspApiValidationEntry>> {
    let ctx = synthetic_validation_context();
    client.did_open(&workspace.synthetic_uri, VALIDATION_SURFACE_SOURCE)?;
    let _ = client.wait_for_publish_diagnostics(&workspace.synthetic_uri, DIAGNOSTICS_TIMEOUT)?;
    let mut entries = validate_synthetic_refactor_requests(client, workspace, &ctx)?;
    entries.extend(validate_synthetic_outline_requests(
        client, workspace, &ctx,
    )?);
    entries.extend(validate_synthetic_member_navigation(
        client,
        workspace,
        &ctx,
        completion_timing_path,
        navigation_timing_path,
    )?);
    Ok(entries)
}

fn validate_synthetic_refactor_requests(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    ctx: &SyntheticValidationContext,
) -> Result<Vec<LspApiValidationEntry>> {
    let (references_ms, references_response) = client.request_timed(
        "textDocument/references",
        json!({
            "textDocument": { "uri": workspace.synthetic_uri },
            "position": {
                "line": ctx.component_position.line,
                "character": ctx.component_position.character,
            },
            "context": { "includeDeclaration": true }
        }),
        VALIDATION_TIMEOUT,
    )?;
    let references_count = json_array_len(&response_result(&references_response));
    ensure!(
        references_count >= 2,
        "references should include declaration and usage sites"
    );

    let (prepare_ms, prepare_response) = client.request_timed(
        "textDocument/prepareRename",
        json!({
            "textDocument": { "uri": workspace.synthetic_uri },
            "position": {
                "line": ctx.component_position.line,
                "character": ctx.component_position.character,
            }
        }),
        VALIDATION_TIMEOUT,
    )?;
    ensure!(
        response_result(&prepare_response).get("start").is_some()
            || response_result(&prepare_response).get("range").is_some(),
        "prepareRename should return a range payload"
    );

    let (rename_ms, rename_response) = client.request_timed(
        "textDocument/rename",
        json!({
            "textDocument": { "uri": workspace.synthetic_uri },
            "position": {
                "line": ctx.component_position.line,
                "character": ctx.component_position.character,
            },
            "newName": "renamedInst"
        }),
        VALIDATION_TIMEOUT,
    )?;
    let rename_changes =
        workspace_edit_change_count(&response_result(&rename_response), &workspace.synthetic_uri);
    ensure!(
        rename_changes >= 2,
        "rename should edit declaration and use sites"
    );

    Ok(vec![
        ok_validation(
            "references",
            "req",
            Some(references_ms),
            format!("synthetic refs={references_count}"),
        ),
        ok_validation("prepareRename", "req", Some(prepare_ms), "synthetic range"),
        ok_validation(
            "rename",
            "req",
            Some(rename_ms),
            format!("synthetic edits={rename_changes}"),
        ),
    ])
}

fn validate_synthetic_outline_requests(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    ctx: &SyntheticValidationContext,
) -> Result<Vec<LspApiValidationEntry>> {
    let (signature_ms, signature_response) = client.request_timed(
        "textDocument/signatureHelp",
        json!({
            "textDocument": { "uri": workspace.synthetic_uri },
            "position": {
                "line": ctx.signature_position.line,
                "character": ctx.signature_position.character,
            }
        }),
        VALIDATION_TIMEOUT,
    )?;
    ensure!(
        response_result(&signature_response)
            .get("signatures")
            .and_then(Value::as_array)
            .is_some_and(|items| !items.is_empty()),
        "signatureHelp should expose builtin function signatures"
    );

    let (fold_ms, fold_response) = client.request_timed(
        "textDocument/foldingRange",
        json!({ "textDocument": { "uri": workspace.synthetic_uri } }),
        VALIDATION_TIMEOUT,
    )?;
    let fold_count = json_array_len(&response_result(&fold_response));
    ensure!(fold_count > 0, "foldingRange should find foldable regions");

    let (link_ms, link_response) = client.request_timed(
        "textDocument/documentLink",
        json!({ "textDocument": { "uri": workspace.synthetic_uri } }),
        VALIDATION_TIMEOUT,
    )?;
    let link_count = json_array_len(&response_result(&link_response));
    ensure!(
        link_count >= 2,
        "documentLink should expose URL and file targets"
    );

    Ok(vec![
        ok_validation(
            "signatureHelp",
            "req",
            Some(signature_ms),
            "synthetic builtin=sin",
        ),
        ok_validation(
            "foldingRange",
            "req",
            Some(fold_ms),
            format!("synthetic count={fold_count}"),
        ),
        ok_validation(
            "documentLink",
            "req",
            Some(link_ms),
            format!("synthetic count={link_count}"),
        ),
    ])
}

fn validate_synthetic_member_navigation(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    ctx: &SyntheticValidationContext,
    completion_timing_path: &Path,
    navigation_timing_path: &Path,
) -> Result<Vec<LspApiValidationEntry>> {
    let completion_start = read_completion_timings(completion_timing_path)?.len();
    let completion =
        client.completion(&workspace.synthetic_uri, ctx.completion_position, "gain")?;
    ensure!(
        completion.expected_completion_present,
        "synthetic completion should expose component members"
    );
    let completion_timings = completion_entries_since(
        completion_timing_path,
        completion_start,
        &workspace.synthetic_uri,
    )?;
    ensure_completion_query_fast_path("member completion", &completion_timings)?;

    let hover_start = read_navigation_timings(navigation_timing_path)?.len();
    let (hover_ms, hover_response) = client.request_timed(
        "textDocument/hover",
        json!({
            "textDocument": { "uri": workspace.synthetic_uri },
            "position": {
                "line": ctx.alias_position.line,
                "character": ctx.alias_position.character,
            }
        }),
        VALIDATION_TIMEOUT,
    )?;
    ensure!(
        hover_text(&response_result(&hover_response)).contains("Helper"),
        "synthetic hover should resolve the imported alias"
    );
    let hover_timings = navigation_entries_since(
        navigation_timing_path,
        hover_start,
        "hover",
        &uri_path_string(&workspace.synthetic_uri)?,
    )?;
    ensure_single_navigation_fast_path("alias hover", &hover_timings)?;

    Ok(vec![
        ok_validation(
            "memberCompletion",
            "req",
            Some(completion.client_ms),
            format!(
                "synthetic items={} layer={} ast={}ms",
                completion.completion_count,
                completion_timings[0].semantic_layer,
                completion_timings[0].total_ms
            ),
        ),
        ok_validation(
            "aliasHover",
            "req",
            Some(hover_ms),
            format!(
                "synthetic Helper {} layer={}={}ms",
                hover_timings[0].request_path.label(),
                hover_timings[0].semantic_layer,
                hover_timings[0].total_ms
            ),
        ),
    ])
}

fn run_mutation_lsp_validation(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    diagnostics_timing_path: &Path,
    navigation_timing_path: &Path,
) -> Result<Vec<LspApiValidationEntry>> {
    let save_path = workspace.workspace_root.join("Active.mo");
    fs::write(&save_path, VALIDATION_SAVE_SOURCE)
        .with_context(|| format!("failed to write {}", save_path.display()))?;
    let save_uri = file_url(&save_path)?;
    let save_key = uri_path_string(&save_uri)?;
    let mut entries = validate_mutation_rewrites(client, workspace)?;
    entries.extend(validate_mutation_save_path(
        client,
        workspace,
        &save_uri,
        &save_key,
        diagnostics_timing_path,
    )?);
    entries.extend(validate_mutation_change_and_close(
        client,
        workspace,
        navigation_timing_path,
    )?);
    Ok(entries)
}

fn validate_mutation_rewrites(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
) -> Result<Vec<LspApiValidationEntry>> {
    client.did_open(&workspace.formatting_uri, VALIDATION_FORMATTING_SOURCE)?;
    let _ = client.wait_for_publish_diagnostics(&workspace.formatting_uri, DIAGNOSTICS_TIMEOUT)?;
    let (format_ms, format_response) = client.request_timed(
        "textDocument/formatting",
        json!({
            "textDocument": { "uri": workspace.formatting_uri },
            "options": { "tabSize": 2, "insertSpaces": true }
        }),
        VALIDATION_TIMEOUT,
    )?;
    ensure!(
        json_array_len(&response_result(&format_response)) >= 1,
        "formatting should return a rewrite edit"
    );

    client.did_open(&workspace.broken_uri, VALIDATION_CODE_ACTION_SOURCE)?;
    let broken_diagnostics =
        client.wait_for_publish_diagnostics(&workspace.broken_uri, DIAGNOSTICS_TIMEOUT)?;
    let broken_items = diagnostics_from_notification(&broken_diagnostics);
    let action_range = expected_diagnostic_range(&broken_items, "EP001")
        .context("expected EP001 code-action diagnostic from synthetic broken source")?;
    let (action_ms, action_response) = client.request_timed(
        "textDocument/codeAction",
        json!({
            "textDocument": { "uri": workspace.broken_uri },
            "range": action_range,
            "context": {
                "diagnostics": broken_items,
                "triggerKind": 1
            }
        }),
        VALIDATION_TIMEOUT,
    )?;
    ensure!(
        json_array_len(&response_result(&action_response)) >= 1,
        "codeAction should return at least one fix"
    );
    client.did_close(&workspace.formatting_uri)?;
    client.did_close(&workspace.broken_uri)?;

    Ok(vec![
        ok_validation("formatting", "req", Some(format_ms), "synthetic rewrite"),
        ok_validation("codeAction", "req", Some(action_ms), "synthetic EP001"),
    ])
}

fn validate_mutation_save_path(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    save_uri: &Url,
    save_key: &str,
    diagnostics_timing_path: &Path,
) -> Result<Vec<LspApiValidationEntry>> {
    client.did_close(&workspace.synthetic_uri)?;
    client.did_close(&workspace.msl_uri)?;
    client.did_open(save_uri, VALIDATION_SAVE_SOURCE)?;
    let _ = client.wait_for_publish_diagnostics(save_uri, DIAGNOSTICS_TIMEOUT)?;
    let save_start = read_diagnostics_timings(diagnostics_timing_path)?.len();
    let save_cold_started = Instant::now();
    client.did_save(save_uri, VALIDATION_SAVE_SOURCE)?;
    let save_cold = client.wait_for_publish_diagnostics(save_uri, DIAGNOSTICS_TIMEOUT)?;
    let save_warm_started = Instant::now();
    client.did_save(save_uri, VALIDATION_SAVE_SOURCE)?;
    let _save_warm = client.wait_for_publish_diagnostics(save_uri, DIAGNOSTICS_TIMEOUT)?;
    let save_timings =
        diagnostics_entries_since(diagnostics_timing_path, save_start, "save", save_key)?;
    ensure_diagnostics_validation_entries(&save_timings)?;
    client.did_close(save_uri)?;
    client.did_open(&workspace.synthetic_uri, VALIDATION_SURFACE_SOURCE)?;
    let _ = client.wait_for_publish_diagnostics(&workspace.synthetic_uri, DIAGNOSTICS_TIMEOUT)?;
    client.did_open(&workspace.msl_uri, &workspace.msl_source)?;
    let _ = client.wait_for_publish_diagnostics(&workspace.msl_uri, DIAGNOSTICS_TIMEOUT)?;
    Ok(vec![
        ok_validation(
            "didSave:cold",
            "note",
            Some(save_cold_started.elapsed().as_millis() as u64),
            format!(
                "synthetic diag={}",
                diagnostics_from_notification(&save_cold).len()
            ),
        ),
        ok_validation(
            "didSave:warm",
            "note",
            Some(save_warm_started.elapsed().as_millis() as u64),
            format!("save={}ms compiled", save_timings[1].total_ms),
        ),
    ])
}

fn validate_mutation_change_and_close(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    navigation_timing_path: &Path,
) -> Result<Vec<LspApiValidationEntry>> {
    let change_started = Instant::now();
    client.did_change(&workspace.synthetic_uri, 2, VALIDATION_CHANGED_SOURCE)?;
    let _ = client.wait_for_publish_diagnostics(&workspace.synthetic_uri, DIAGNOSTICS_TIMEOUT)?;
    let workspace_start = read_navigation_timings(navigation_timing_path)?.len();
    let (symbol_ms, symbol_response) = client.request_timed(
        "workspace/symbol",
        json!({ "query": "Shifted" }),
        VALIDATION_TIMEOUT,
    )?;
    ensure!(
        json_array_len(&response_result(&symbol_response)) >= 1,
        "workspace/symbol should see the changed document"
    );
    let workspace_timings = navigation_request_entries_since(
        navigation_timing_path,
        workspace_start,
        "workspace_symbol",
    )?;
    ensure_navigation_entry_count("workspace/symbol*", &workspace_timings, 1)?;
    let workspace_entry = workspace_timings
        .last()
        .context("workspace/symbol* timing entry should exist")?;

    let close_started = Instant::now();
    client.did_close(&workspace.synthetic_uri)?;
    let (_, closed_symbol_response) = client.request_timed(
        "workspace/symbol",
        json!({ "query": "Shifted" }),
        VALIDATION_TIMEOUT,
    )?;
    ensure!(
        json_array_len(&response_result(&closed_symbol_response)) == 0,
        "didClose should remove the changed synthetic document from workspace symbols"
    );

    Ok(vec![
        ok_validation(
            "didChange",
            "note",
            Some(change_started.elapsed().as_millis() as u64),
            "synthetic Shifted",
        ),
        ok_validation(
            "workspace/symbol*",
            "req",
            Some(symbol_ms),
            workspace_symbol_detail("synthetic", 1, workspace_entry),
        ),
        ok_validation(
            "didClose",
            "note",
            Some(close_started.elapsed().as_millis() as u64),
            "synthetic closed",
        ),
    ])
}

fn workspace_symbol_detail(label: &str, hits: usize, entry: &NavigationTimingEntry) -> String {
    let mut detail = format!(
        "{label} hits={hits} snap={}ms lock={}ms build={}ms query={}ms fmt={}ms layer={}",
        entry
            .snapshot_ms
            .map(|value| value.to_string())
            .unwrap_or_else(|| "-".to_string()),
        entry
            .snapshot_lock_ms
            .map(|value| value.to_string())
            .unwrap_or_else(|| "-".to_string()),
        entry
            .snapshot_build_ms
            .map(|value| value.to_string())
            .unwrap_or_else(|| "-".to_string()),
        entry
            .query_ms
            .map(|value| value.to_string())
            .unwrap_or_else(|| "-".to_string()),
        entry
            .format_ms
            .map(|value| value.to_string())
            .unwrap_or_else(|| "-".to_string()),
        entry.semantic_layer,
    );
    if let Some(extra) = &entry.detail {
        detail.push(' ');
        detail.push_str(extra);
    }
    detail
}

fn capture_warm_latency_snapshot(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    timing_paths: &LspTimingPaths,
) -> Result<WarmLatencySnapshot> {
    let full_ctx = full_msl_validation_context(workspace)?;
    let synthetic_ctx = synthetic_validation_context();
    let completion_path = timing_paths
        .completion
        .as_deref()
        .context("missing completion timing path for warm latency snapshot")?;
    let diagnostics_path = timing_paths
        .diagnostics
        .as_deref()
        .context("missing diagnostics timing path for warm latency snapshot")?;
    let navigation_path = timing_paths
        .navigation
        .as_deref()
        .context("missing navigation timing path for warm latency snapshot")?;

    Ok(WarmLatencySnapshot {
        measurements: vec![
            capture_local_completion_snapshot(client, workspace, &synthetic_ctx, completion_path)?,
            capture_source_root_completion_snapshot(client, workspace, &full_ctx, completion_path)?,
            capture_hover_snapshot(client, workspace, &full_ctx, navigation_path)?,
            capture_definition_snapshot(client, workspace, &full_ctx, navigation_path)?,
            capture_live_diagnostics_snapshot(client, workspace, diagnostics_path)?,
            capture_save_diagnostics_snapshot(client, workspace, diagnostics_path)?,
        ],
    })
}

fn capture_local_completion_snapshot(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    ctx: &SyntheticValidationContext,
    completion_timing_path: &Path,
) -> Result<WarmLatencyMeasurement> {
    let uri = write_validation_file(
        &workspace.workspace_root,
        "snapshot-active.mo",
        VALIDATION_SURFACE_SOURCE,
    )?;
    client.did_open(&uri, VALIDATION_SURFACE_SOURCE)?;
    let _ = client.wait_for_publish_diagnostics(&uri, DIAGNOSTICS_TIMEOUT)?;
    let prime = client.completion(&uri, ctx.completion_position, "gain")?;
    ensure!(
        prime.expected_completion_present,
        "warm local completion snapshot priming request missed `gain`"
    );
    let start = read_completion_timings(completion_timing_path)?.len();
    let mut samples = Vec::with_capacity(WARM_LATENCY_SAMPLE_COUNT);
    for _ in 0..WARM_LATENCY_SAMPLE_COUNT {
        let completion = client.completion(&uri, ctx.completion_position, "gain")?;
        ensure!(
            completion.expected_completion_present,
            "warm local completion snapshot missed `gain`"
        );
        samples.push(completion.client_ms);
    }
    let entries = completion_entries_since(completion_timing_path, start, &uri)?;
    ensure_completion_entry_count("warm local completion", &entries, WARM_LATENCY_SAMPLE_COUNT)?;
    ensure_completion_query_fast_path_entries(
        "warm local completion",
        &entries,
        "class_interface",
    )?;
    build_warm_latency_measurement(
        "local completion",
        LOCAL_COMPLETION_TARGET_MS,
        &samples,
        None,
        Some("class_interface"),
    )
}

fn capture_source_root_completion_snapshot(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    ctx: &FullMslValidationContext,
    completion_timing_path: &Path,
) -> Result<WarmLatencyMeasurement> {
    let prime = client.completion(
        &workspace.msl_uri,
        ctx.completion_position,
        FULL_MSL_COMPLETION_PROBES[0].expected_completion_label,
    )?;
    ensure!(
        prime.expected_completion_present,
        "warm source-root completion snapshot priming request missed expected label"
    );
    let start = read_completion_timings(completion_timing_path)?.len();
    let mut samples = Vec::with_capacity(WARM_LATENCY_SAMPLE_COUNT);
    for _ in 0..WARM_LATENCY_SAMPLE_COUNT {
        let completion = client.completion(
            &workspace.msl_uri,
            ctx.completion_position,
            FULL_MSL_COMPLETION_PROBES[0].expected_completion_label,
        )?;
        ensure!(
            completion.expected_completion_present,
            "warm source-root completion snapshot missed expected label"
        );
        samples.push(completion.client_ms);
    }
    let entries = completion_entries_since(completion_timing_path, start, &workspace.msl_uri)?;
    ensure_completion_entry_count(
        "warm source-root completion",
        &entries,
        WARM_LATENCY_SAMPLE_COUNT,
    )?;
    ensure_completion_entries_not_stale("warm source-root completion", &entries)?;
    ensure_completion_entries_have_semantic_layer(
        "warm source-root completion",
        &entries,
        "package_def_map",
    )?;
    build_warm_latency_measurement(
        "source-root completion",
        SOURCE_ROOT_COMPLETION_TARGET_MS,
        &samples,
        None,
        Some("package_def_map"),
    )
}

fn capture_hover_snapshot(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    ctx: &FullMslValidationContext,
    navigation_timing_path: &Path,
) -> Result<WarmLatencyMeasurement> {
    let hover_params = json!({
        "textDocument": { "uri": workspace.msl_uri },
        "position": {
            "line": ctx.hover_position.line,
            "character": ctx.hover_position.character,
        }
    });
    let (_, prime_response) = client.request_timed(
        "textDocument/hover",
        hover_params.clone(),
        VALIDATION_TIMEOUT,
    )?;
    ensure!(
        !hover_text(&response_result(&prime_response))
            .trim()
            .is_empty(),
        "warm hover snapshot priming request should return markdown content"
    );
    let start = read_navigation_timings(navigation_timing_path)?.len();
    let mut samples = Vec::with_capacity(WARM_LATENCY_SAMPLE_COUNT);
    for _ in 0..WARM_LATENCY_SAMPLE_COUNT {
        let (client_ms, response) = client.request_timed(
            "textDocument/hover",
            hover_params.clone(),
            VALIDATION_TIMEOUT,
        )?;
        ensure!(
            !hover_text(&response_result(&response)).trim().is_empty(),
            "warm hover snapshot should return markdown content"
        );
        samples.push(client_ms);
    }
    let entries = navigation_entries_since(navigation_timing_path, start, "hover", &ctx.msl_key)?;
    ensure_navigation_entry_count("warm hover", &entries, WARM_LATENCY_SAMPLE_COUNT)?;
    ensure_navigation_query_only_entries("warm hover", &entries, "class_body_semantics")?;
    build_warm_latency_measurement(
        "hover",
        HOVER_TARGET_MS,
        &samples,
        Some(NavigationRequestPath::QueryOnly),
        Some("class_body_semantics"),
    )
}

fn capture_definition_snapshot(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    ctx: &FullMslValidationContext,
    navigation_timing_path: &Path,
) -> Result<WarmLatencyMeasurement> {
    let definition_params = json!({
        "textDocument": { "uri": workspace.msl_uri },
        "position": {
            "line": ctx.definition_position.line,
            "character": ctx.definition_position.character,
        }
    });
    let (_, prime_response) = client.request_timed(
        "textDocument/definition",
        definition_params.clone(),
        VALIDATION_TIMEOUT,
    )?;
    ensure!(
        !definition_targets(&response_result(&prime_response)).is_empty(),
        "warm definition snapshot priming request should return a target"
    );
    let start = read_navigation_timings(navigation_timing_path)?.len();
    let mut samples = Vec::with_capacity(WARM_LATENCY_SAMPLE_COUNT);
    for _ in 0..WARM_LATENCY_SAMPLE_COUNT {
        let (client_ms, response) = client.request_timed(
            "textDocument/definition",
            definition_params.clone(),
            VALIDATION_TIMEOUT,
        )?;
        ensure!(
            !definition_targets(&response_result(&response)).is_empty(),
            "warm definition snapshot should return a target"
        );
        samples.push(client_ms);
    }
    let entries =
        navigation_entries_since(navigation_timing_path, start, "definition", &ctx.msl_key)?;
    ensure_navigation_entry_count("warm definition", &entries, WARM_LATENCY_SAMPLE_COUNT)?;
    ensure_navigation_query_only_entries("warm definition", &entries, "class_body_semantics")?;
    build_warm_latency_measurement(
        "definition",
        DEFINITION_TARGET_MS,
        &samples,
        Some(NavigationRequestPath::QueryOnly),
        Some("class_body_semantics"),
    )
}

fn capture_live_diagnostics_snapshot(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    diagnostics_timing_path: &Path,
) -> Result<WarmLatencyMeasurement> {
    let uri = write_validation_file(
        &workspace.workspace_root,
        "snapshot-live.mo",
        VALIDATION_LIVE_DIAGNOSTICS_SOURCE_A,
    )?;
    let uri_key = uri_path_string(&uri)?;
    client.did_open(&uri, VALIDATION_LIVE_DIAGNOSTICS_SOURCE_A)?;
    let _ = client.wait_for_publish_diagnostics(&uri, DIAGNOSTICS_TIMEOUT)?;
    client.did_change(&uri, 2, VALIDATION_LIVE_DIAGNOSTICS_SOURCE_B)?;
    let _ = client.wait_for_publish_diagnostics(&uri, DIAGNOSTICS_TIMEOUT)?;

    let start = read_diagnostics_timings(diagnostics_timing_path)?.len();
    let mut samples = Vec::with_capacity(WARM_LATENCY_SAMPLE_COUNT);
    for (version, idx) in (3..).zip(0..WARM_LATENCY_SAMPLE_COUNT) {
        let source = if idx % 2 == 0 {
            VALIDATION_LIVE_DIAGNOSTICS_SOURCE_A
        } else {
            VALIDATION_LIVE_DIAGNOSTICS_SOURCE_B
        };
        let started = Instant::now();
        client.did_change(&uri, version, source)?;
        let _ = client.wait_for_publish_diagnostics(&uri, DIAGNOSTICS_TIMEOUT)?;
        samples.push(started.elapsed().as_millis() as u64);
    }
    let entries = diagnostics_entries_since(diagnostics_timing_path, start, "live", &uri_key)?;
    ensure_diagnostics_entry_count("warm live diagnostics", &entries, WARM_LATENCY_SAMPLE_COUNT)?;
    ensure_live_diagnostics_fast_path("warm live diagnostics", &entries)?;
    build_warm_latency_measurement(
        "live diagnostics",
        LIVE_DIAGNOSTICS_TARGET_MS,
        &samples,
        None,
        Some("parse_only"),
    )
}

fn capture_save_diagnostics_snapshot(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
    diagnostics_timing_path: &Path,
) -> Result<WarmLatencyMeasurement> {
    let uri = write_validation_file(
        &workspace.workspace_root,
        "Active.mo",
        VALIDATION_SAVE_SOURCE,
    )?;
    let uri_key = uri_path_string(&uri)?;
    client.did_open(&uri, VALIDATION_SAVE_SOURCE)?;
    let _ = client.wait_for_publish_diagnostics(&uri, DIAGNOSTICS_TIMEOUT)?;
    client.did_save(&uri, VALIDATION_SAVE_SOURCE)?;
    let _ = client.wait_for_publish_diagnostics(&uri, DIAGNOSTICS_TIMEOUT)?;

    let start = read_diagnostics_timings(diagnostics_timing_path)?.len();
    let mut samples = Vec::with_capacity(WARM_LATENCY_SAMPLE_COUNT);
    for _ in 0..WARM_LATENCY_SAMPLE_COUNT {
        let started = Instant::now();
        client.did_save(&uri, VALIDATION_SAVE_SOURCE)?;
        let _ = client.wait_for_publish_diagnostics(&uri, DIAGNOSTICS_TIMEOUT)?;
        samples.push(started.elapsed().as_millis() as u64);
    }
    let entries = diagnostics_entries_since(diagnostics_timing_path, start, "save", &uri_key)?;
    ensure_diagnostics_entry_count("warm save diagnostics", &entries, WARM_LATENCY_SAMPLE_COUNT)?;
    ensure_save_diagnostics_compile_path("warm save diagnostics", &entries, None)?;
    let semantic_layer =
        compile_diagnostics_semantic_layer("warm save diagnostics", &entries, None)?;
    build_warm_latency_measurement(
        "save diagnostics",
        SAVE_DIAGNOSTICS_TARGET_MS,
        &samples,
        None,
        Some(semantic_layer),
    )
}

fn run_execute_command_validation(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
) -> Result<Vec<LspApiValidationEntry>> {
    let sim_uri = prepare_execute_validation_document(client, workspace)?;
    let mut entries = validate_get_simulation_models_command(client, &sim_uri)?;
    entries.extend(validate_get_simulation_config_command(client, workspace)?);
    entries.extend(validate_execute_simulate_command(client, &sim_uri)?);
    entries.extend(validate_set_reset_simulation_config_commands(
        client, workspace,
    )?);
    entries.extend(validate_execute_visualization_commands(client, workspace)?);
    entries.extend(validate_execute_sidecar_commands(client, workspace)?);
    Ok(entries)
}

fn prepare_execute_validation_document(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
) -> Result<Url> {
    let sim_source = "model Decay\n  Real x(start=1);\nequation\n  der(x) = -x;\nend Decay;\n";
    let sim_path = workspace.workspace_root.join("simulate.mo");
    fs::write(&sim_path, sim_source)
        .with_context(|| format!("failed to write {}", sim_path.display()))?;
    let sim_uri = file_url(&sim_path)?;
    client.did_open(&sim_uri, sim_source)?;
    let _ = client.wait_for_publish_diagnostics(&sim_uri, DIAGNOSTICS_TIMEOUT)?;
    Ok(sim_uri)
}

fn execute_lsp_command(
    client: &mut LspStdioClient,
    command: &str,
    payload: Value,
) -> Result<(u64, Value)> {
    client.request_timed(
        "workspace/executeCommand",
        json!({
            "command": command,
            "arguments": [payload],
        }),
        COMPLETION_TIMEOUT,
    )
}

fn validate_get_simulation_models_command(
    client: &mut LspStdioClient,
    sim_uri: &Url,
) -> Result<Vec<LspApiValidationEntry>> {
    let (get_models_ms, get_models_response) = execute_lsp_command(
        client,
        "rumoca.project.getSimulationModels",
        json!({
            "uri": sim_uri,
            "defaultModel": "Decay",
        }),
    )?;
    ensure!(
        response_result(&get_models_response).get("selectedModel")
            == Some(&Value::String("Decay".to_string())),
        "getSimulationModels should select the default model"
    );

    Ok(vec![ok_validation(
        "exec:getSimModels",
        "req",
        Some(get_models_ms),
        "Decay selected",
    )])
}

fn validate_get_simulation_config_command(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
) -> Result<Vec<LspApiValidationEntry>> {
    let fallback = json!({
        "model": "Decay",
        "tEnd": 1.0,
        "dt": 0.1,
        "solver": "auto",
        "outputDir": "",
        "sourceRootPaths": [],
    });
    let workspace_root = workspace.workspace_root.display().to_string();

    let (get_sim_ms, get_sim_response) = execute_lsp_command(
        client,
        "rumoca.project.getSimulationConfig",
        json!({
            "workspaceRoot": workspace_root,
            "model": "Decay",
            "fallback": fallback,
        }),
    )?;
    ensure!(
        response_result(&get_sim_response)
            .get("effective")
            .is_some(),
        "getSimulationConfig should return an effective config payload"
    );

    Ok(vec![ok_validation(
        "exec:getSimCfg",
        "req",
        Some(get_sim_ms),
        "Decay effective",
    )])
}

fn validate_set_reset_simulation_config_commands(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
) -> Result<Vec<LspApiValidationEntry>> {
    let preset = json!({
        "tEnd": 2.0,
        "dt": 0.2,
        "solver": "bdf",
        "outputDir": "tmp",
        "sourceRootOverrides": [],
    });

    let (set_sim_ms, set_sim_response) = execute_lsp_command(
        client,
        "rumoca.project.setSimulationPreset",
        json!({
            "workspaceRoot": workspace.workspace_root.display().to_string(),
            "model": "Decay",
            "preset": preset,
        }),
    )?;
    ensure!(
        response_result(&set_sim_response) == json!({ "ok": true }),
        "setSimulationPreset should return ok"
    );

    let (reset_sim_ms, reset_sim_response) = execute_lsp_command(
        client,
        "rumoca.project.resetSimulationPreset",
        json!({
            "workspaceRoot": workspace.workspace_root.display().to_string(),
            "model": "Decay",
        }),
    )?;
    ensure!(
        response_result(&reset_sim_response) == json!({ "ok": true }),
        "resetSimulationPreset should return ok"
    );

    Ok(vec![
        ok_validation("exec:setSimPre", "req", Some(set_sim_ms), "ok"),
        ok_validation("exec:resetSim", "req", Some(reset_sim_ms), "ok"),
    ])
}

fn validate_execute_visualization_commands(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
) -> Result<Vec<LspApiValidationEntry>> {
    let (get_vis_ms, get_vis_response) = execute_lsp_command(
        client,
        "rumoca.project.getVisualizationConfig",
        json!({
            "workspaceRoot": workspace.workspace_root.display().to_string(),
            "model": "Decay",
        }),
    )?;
    ensure!(
        response_result(&get_vis_response).get("views").is_some(),
        "getVisualizationConfig should return views"
    );

    let (set_vis_ms, set_vis_response) = execute_lsp_command(
        client,
        "rumoca.project.setVisualizationConfig",
        json!({
            "workspaceRoot": workspace.workspace_root.display().to_string(),
            "model": "Decay",
            "views": [{
                "id": "states",
                "title": "States",
                "type": "timeseries",
                "x": "time",
                "y": ["x"],
            }],
        }),
    )?;
    ensure!(
        response_result(&set_vis_response) == json!({ "ok": true }),
        "setVisualizationConfig should return ok"
    );

    Ok(vec![
        ok_validation("exec:getVisCfg", "req", Some(get_vis_ms), "views"),
        ok_validation("exec:setVisCfg", "req", Some(set_vis_ms), "ok"),
    ])
}

fn validate_execute_sidecar_commands(
    client: &mut LspStdioClient,
    workspace: &ValidationWorkspace,
) -> Result<Vec<LspApiValidationEntry>> {
    let (resync_ms, resync_response) = execute_lsp_command(
        client,
        "rumoca.project.resyncSidecars",
        json!({
            "workspaceRoot": workspace.workspace_root.display().to_string(),
            "dryRun": true,
            "pruneOrphans": false,
            "reason": "benchmark",
        }),
    )?;
    ensure!(
        response_result(&resync_response).get("report").is_some(),
        "resyncSidecars should return a report payload"
    );

    let (files_moved_ms, files_moved_response) = execute_lsp_command(
        client,
        "rumoca.project.filesMoved",
        json!({
            "workspaceRoot": workspace.workspace_root.display().to_string(),
            "files": [{
                "oldPath": workspace.workspace_root.join("Old.mo").display().to_string(),
                "newPath": workspace.workspace_root.join("New.mo").display().to_string(),
            }],
        }),
    )?;
    ensure!(
        response_result(&files_moved_response).get("ok") == Some(&Value::Bool(true)),
        "filesMoved should return ok"
    );

    Ok(vec![
        ok_validation("exec:resync", "req", Some(resync_ms), "report"),
        ok_validation("exec:filesMv", "req", Some(files_moved_ms), "ok"),
    ])
}

fn validate_execute_simulate_command(
    client: &mut LspStdioClient,
    sim_uri: &Url,
) -> Result<Vec<LspApiValidationEntry>> {
    let (simulate_ms, simulate_response) = execute_lsp_command(
        client,
        "rumoca.project.simulate",
        json!({
            "uri": sim_uri,
            "model": "Decay",
            "settings": {
                "tEnd": 0.2,
                "dt": 0.1,
                "solver": "auto",
                "outputDir": "",
                "sourceRootPaths": [],
            }
        }),
    )?;
    let simulation = response_result(&simulate_response);
    ensure!(
        simulation.get("ok") == Some(&Value::Bool(true)),
        "simulate should return ok"
    );
    ensure!(
        simulation
            .get("payload")
            .and_then(|payload| payload.get("names"))
            .and_then(Value::as_array)
            .is_some_and(|names| !names.is_empty()),
        "simulate should return named result series"
    );
    let metrics = simulation.get("metrics");
    let compile_seconds = metrics
        .and_then(|value| value.get("compileSeconds"))
        .and_then(Value::as_f64)
        .unwrap_or(0.0);
    let simulate_seconds = metrics
        .and_then(|value| value.get("simulateSeconds"))
        .and_then(Value::as_f64)
        .unwrap_or(0.0);
    let compile_phase_seconds = metrics.and_then(|value| value.get("compilePhaseSeconds"));
    let prepare_context_seconds = compile_phase_seconds
        .and_then(|value| value.get("prepareContext"))
        .and_then(Value::as_f64)
        .unwrap_or(0.0);
    let build_snapshot_seconds = compile_phase_seconds
        .and_then(|value| value.get("buildSnapshot"))
        .and_then(Value::as_f64)
        .unwrap_or(0.0);
    let strict_compile_seconds = compile_phase_seconds
        .and_then(|value| value.get("strictCompile"))
        .and_then(Value::as_f64)
        .unwrap_or(0.0);
    let strict_resolve_seconds = compile_phase_seconds
        .and_then(|value| value.get("strictResolve"))
        .and_then(Value::as_f64)
        .unwrap_or(0.0);
    let instantiate_seconds = compile_phase_seconds
        .and_then(|value| value.get("instantiate"))
        .and_then(Value::as_f64)
        .unwrap_or(0.0);
    let typecheck_seconds = compile_phase_seconds
        .and_then(|value| value.get("typecheck"))
        .and_then(Value::as_f64)
        .unwrap_or(0.0);
    let flatten_seconds = compile_phase_seconds
        .and_then(|value| value.get("flatten"))
        .and_then(Value::as_f64)
        .unwrap_or(0.0);
    let todae_seconds = compile_phase_seconds
        .and_then(|value| value.get("todae"))
        .and_then(Value::as_f64)
        .unwrap_or(0.0);
    Ok(vec![ok_validation(
        "exec:simulate",
        "req",
        Some(simulate_ms),
        format!(
            "Decay ok compile={compile_seconds:.3}s sim={simulate_seconds:.3}s prep={prepare_context_seconds:.3}s snap={build_snapshot_seconds:.3}s strict={strict_compile_seconds:.3}s resolve={strict_resolve_seconds:.3}s inst={instantiate_seconds:.3}s type={typecheck_seconds:.3}s flat={flatten_seconds:.3}s dae={todae_seconds:.3}s"
        ),
    )])
}

pub(crate) fn run_lsp_api_validation(
    root: &Path,
    lsp_binary: &Path,
    modelica_paths: &[String],
    msl_archive_root: &Path,
    report_dir: &Path,
) -> Result<LspApiValidationReport> {
    let workspace = create_validation_workspace(msl_archive_root)?;
    let timing_paths = LspTimingPaths {
        startup: Some(unique_timing_path(report_dir, "lsp-api-startup")),
        completion: Some(unique_timing_path(report_dir, "lsp-api-validation")),
        completion_progress: Some(unique_timing_path(
            report_dir,
            "lsp-api-validation-progress",
        )),
        diagnostics: Some(unique_timing_path(report_dir, "lsp-api-diagnostics")),
        navigation: Some(unique_timing_path(report_dir, "lsp-api-navigation")),
    };
    let mut client = LspStdioClient::spawn(root, lsp_binary, modelica_paths, &timing_paths)?;
    let mut operations = Vec::new();
    let initialize_params = json!({
        "processId": std::process::id(),
        "rootUri": workspace.workspace_uri,
        "capabilities": {},
        "workspaceFolders": [{
            "uri": workspace.workspace_uri,
            "name": "lsp-api-validation",
        }],
        "initializationOptions": {
            "sourceRootPaths": modelica_paths,
        }
    });
    let (initialize_ms, initialize_response) =
        client.request_timed("initialize", initialize_params, VALIDATION_TIMEOUT)?;
    let initialize_detail =
        validate_initialize_response(&initialize_response, timing_paths.startup.as_deref())?;
    operations.push(ok_validation(
        "initialize",
        "req",
        Some(initialize_ms),
        initialize_detail,
    ));
    let initialized_started = Instant::now();
    client.notify("initialized", json!({}))?;
    operations.push(ok_validation(
        "initialized",
        "note",
        Some(initialized_started.elapsed().as_millis() as u64),
        "server ready",
    ));
    operations.extend(run_full_msl_lsp_validation(
        &mut client,
        &workspace,
        timing_paths
            .completion
            .as_deref()
            .context("missing LSP validation completion timing path")?,
        timing_paths
            .navigation
            .as_deref()
            .context("missing LSP validation navigation timing path")?,
    )?);
    operations.extend(run_synthetic_lsp_validation(
        &mut client,
        &workspace,
        timing_paths
            .completion
            .as_deref()
            .context("missing LSP validation completion timing path")?,
        timing_paths
            .navigation
            .as_deref()
            .context("missing LSP validation navigation timing path")?,
    )?);
    operations.extend(run_mutation_lsp_validation(
        &mut client,
        &workspace,
        timing_paths
            .diagnostics
            .as_deref()
            .context("missing LSP validation diagnostics timing path")?,
        timing_paths
            .navigation
            .as_deref()
            .context("missing LSP validation navigation timing path")?,
    )?);
    operations.extend(run_execute_command_validation(&mut client, &workspace)?);
    let warm_latency_snapshot =
        capture_warm_latency_snapshot(&mut client, &workspace, &timing_paths)?;
    let (shutdown_ms, _) = client.request_timed("shutdown", Value::Null, VALIDATION_TIMEOUT)?;
    operations.push(ok_validation(
        "shutdown",
        "req",
        Some(shutdown_ms),
        "clean exit",
    ));
    client.notify("exit", Value::Null)?;
    drop(client.stdin.take());
    wait_for_exit(&mut client.child, SHUTDOWN_TIMEOUT)?;
    ensure_required_lsp_validation_entries(&operations)?;
    cleanup_validation_timing_paths(&timing_paths);
    Ok(LspApiValidationReport {
        operations,
        warm_latency_snapshot,
    })
}

fn validate_initialize_response(
    response: &Value,
    startup_timing_path: Option<&Path>,
) -> Result<String> {
    let result = response_result(response);
    let capabilities = result
        .get("capabilities")
        .context("initialize response missing capabilities")?;
    for capability in [
        "textDocumentSync",
        "hoverProvider",
        "completionProvider",
        "documentSymbolProvider",
        "semanticTokensProvider",
        "definitionProvider",
        "referencesProvider",
        "renameProvider",
        "workspaceSymbolProvider",
        "signatureHelpProvider",
        "foldingRangeProvider",
        "documentFormattingProvider",
        "codeLensProvider",
        "codeActionProvider",
        "documentLinkProvider",
        "executeCommandProvider",
    ] {
        ensure!(
            capabilities.get(capability).is_some(),
            "initialize must advertise {capability}"
        );
    }
    ensure!(
        capabilities.get("inlayHintProvider").is_none(),
        "initialize should keep inlay hints disabled until special-case hints are re-enabled"
    );
    let commands = capabilities
        .get("executeCommandProvider")
        .and_then(|provider| provider.get("commands"))
        .and_then(Value::as_array)
        .context("initialize executeCommandProvider missing commands")?
        .iter()
        .filter_map(Value::as_str)
        .collect::<Vec<_>>();
    let expected_commands = [
        "rumoca.project.getSimulationConfig",
        "rumoca.project.setSimulationPreset",
        "rumoca.project.resetSimulationPreset",
        "rumoca.project.getVisualizationConfig",
        "rumoca.project.setVisualizationConfig",
        "rumoca.project.resyncSidecars",
        "rumoca.project.filesMoved",
        "rumoca.project.simulate",
        "rumoca.project.getSimulationModels",
        "rumoca.project.setSelectedSimulationModel",
        "rumoca.project.startSimulation",
        "rumoca.project.prepareSimulationModels",
    ];
    ensure!(
        commands == expected_commands,
        "initialize executeCommandProvider drifted: {:?}",
        commands
    );
    let base = format!("caps ok exec={} inlay=off", expected_commands.len());
    let Some(path) = startup_timing_path else {
        return Ok(base);
    };
    let timing = read_startup_timings(path)?
        .into_iter()
        .last()
        .context("missing startup timing entry")?;
    Ok(format!(
        "{base} init={}ms opts={}ms root={}ms reload={}ms cfg={}ms paths={}ms reset={}ms durable={}ms dscan={}ms dhash={}ms dlookup={}ms ddecode={}ms dparse={}ms dcheck={}ms dwrite={}ms dapply={}ms wsym={}ms ns={}ms libs={} changed={}",
        timing.total_ms,
        timing.parse_init_options_ms,
        timing.workspace_root_ms,
        timing.reload_project_config_ms,
        timing.project_discover_ms,
        timing.resolve_source_root_paths_ms,
        timing.reset_session_ms,
        timing.durable_prewarm_ms,
        timing.durable_collect_files_ms,
        timing.durable_hash_inputs_ms,
        timing.durable_cache_lookup_ms,
        timing.durable_cache_deserialize_ms,
        timing.durable_parse_files_ms,
        timing.durable_validate_layout_ms,
        timing.durable_cache_write_ms,
        timing.durable_apply_ms,
        timing.workspace_symbol_prewarm_ms,
        timing.source_root_read_prewarm_spawn_ms,
        timing.initial_source_root_paths,
        short_bool_label(timing.source_root_paths_changed),
    ))
}

fn ensure_required_lsp_validation_entries(entries: &[LspApiValidationEntry]) -> Result<()> {
    let present = entries
        .iter()
        .map(|entry| entry.operation.as_str())
        .collect::<std::collections::BTreeSet<_>>();
    for required in [
        "initialize",
        "initialized",
        "didOpen",
        "source-root-load",
        "completion:cold",
        "completion:warm",
        "hover:cold",
        "hover:warm",
        "definition:cold",
        "definition:warm",
        "docSymbol",
        "semantic",
        "workspace/symbol",
        "codeLens",
        "references",
        "prepareRename",
        "rename",
        "signatureHelp",
        "foldingRange",
        "documentLink",
        "memberCompletion",
        "aliasHover",
        "formatting",
        "codeAction",
        "didChange",
        "workspace/symbol*",
        "didSave:cold",
        "didSave:warm",
        "didClose",
        "exec:getSimCfg",
        "exec:setSimPre",
        "exec:resetSim",
        "exec:getVisCfg",
        "exec:setVisCfg",
        "exec:resync",
        "exec:filesMv",
        "exec:simulate",
        "shutdown",
    ] {
        ensure!(
            present.contains(required),
            "LSP API validation is missing required entry `{required}`"
        );
    }
    Ok(())
}

fn cleanup_validation_timing_paths(paths: &LspTimingPaths) {
    if std::env::var_os("RUMOCA_KEEP_LSP_VALIDATION_TIMINGS").is_some() {
        return;
    }
    for path in [
        paths.startup.as_deref(),
        paths.completion.as_deref(),
        paths.diagnostics.as_deref(),
        paths.navigation.as_deref(),
    ]
    .into_iter()
    .flatten()
    {
        let _ = fs::remove_file(path);
    }
}

fn should_stop_lsp_reader(tx: &mpsc::Sender<LspInbound>, inbound: LspInbound) -> bool {
    let done = !matches!(inbound, LspInbound::Json(_));
    tx.send(inbound).is_err() || done
}

pub(crate) fn forward_lsp_messages(stdout: impl Read, tx: mpsc::Sender<LspInbound>) {
    let mut reader = BufReader::new(stdout);
    loop {
        let inbound = match read_lsp_frame(&mut reader) {
            Ok(Some(message)) => LspInbound::Json(message),
            Ok(None) => LspInbound::Closed,
            Err(error) => LspInbound::ReadError(format!("{error:#}")),
        };
        if should_stop_lsp_reader(&tx, inbound) {
            return;
        }
    }
}

pub(crate) fn insert_params_field(payload: &mut Value, params: Value) {
    if params.is_null() {
        return;
    }
    if let Some(object) = payload.as_object_mut() {
        object.insert("params".to_string(), params);
    }
}

pub(crate) fn match_inbound_message<F>(
    inbound: LspInbound,
    predicate: &mut F,
) -> Result<Option<Value>>
where
    F: FnMut(&Value) -> bool,
{
    match inbound {
        LspInbound::Json(message) => Ok(predicate(&message).then_some(message)),
        LspInbound::Closed => bail!("rumoca-lsp closed stdout unexpectedly"),
        LspInbound::ReadError(error) => bail!("rumoca-lsp read failed: {error}"),
    }
}

fn read_lsp_frame(reader: &mut impl BufRead) -> Result<Option<Value>> {
    let mut content_length = None;
    loop {
        let mut line = String::new();
        let read = reader
            .read_line(&mut line)
            .context("failed to read LSP header line")?;
        if read == 0 {
            return Ok(None);
        }
        let trimmed = line.trim_end_matches(['\r', '\n']);
        if trimmed.is_empty() {
            break;
        }
        let lower = trimmed.to_ascii_lowercase();
        if let Some((_, value)) = lower.split_once("content-length:") {
            content_length = Some(
                value
                    .trim()
                    .parse::<usize>()
                    .context("invalid Content-Length header")?,
            );
        }
    }
    let length = content_length.context("missing Content-Length header")?;
    let mut body = vec![0_u8; length];
    reader
        .read_exact(&mut body)
        .context("failed to read LSP body")?;
    serde_json::from_slice(&body).context("failed to decode LSP JSON body")
}

pub(crate) fn wait_for_exit(child: &mut Child, timeout: Duration) -> Result<()> {
    let deadline = Instant::now() + timeout;
    loop {
        if let Some(status) = child
            .try_wait()
            .context("failed to query rumoca-lsp exit")?
        {
            ensure!(status.success(), "rumoca-lsp exited with status {status}");
            return Ok(());
        }
        if Instant::now() >= deadline {
            bail!("timed out waiting for rumoca-lsp to exit");
        }
        thread::sleep(Duration::from_millis(50));
    }
}

pub(crate) fn render_lsp_completion_table(report: &LspMslCompletionBenchmarkReport) -> String {
    let mut output = String::new();
    output.push_str(&render_runtime_comparison_table(report));
    append_tabled_section(
        &mut output,
        &render_warm_latency_snapshot_table(&report.lsp_api_validation.warm_latency_snapshot),
    );
    if let Some(validation) = render_lsp_api_validation_table(report) {
        append_tabled_section(&mut output, &validation);
    }
    append_tabled_section(
        &mut output,
        &render_completion_timing_breakdown_table(report),
    );
    append_tabled_section(&mut output, &render_completion_edit_breakdown_table(report));
    append_tabled_section(&mut output, &render_completion_cache_stats_table(report));
    if let Some(failures) = render_failure_details_table(report) {
        append_tabled_section(&mut output, &failures);
    }
    output
}
