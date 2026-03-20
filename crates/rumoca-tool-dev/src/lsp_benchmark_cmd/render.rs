use super::*;

pub(super) fn render_runtime_comparison_table(report: &LspMslCompletionBenchmarkReport) -> String {
    const STAGE_COL_WIDTH: usize = 15;
    const SETTING_COL_WIDTH: usize = 8;

    let mut output = String::new();
    output.push_str(&table_divider());
    let note_header = format!("{:<width$}", "note", width = RUNTIME_NOTE_COL_WIDTH,);
    output.push_str(&fixed_width_line(&format!(
        "{:<stage_width$} {:<setting_width$} {:<setting_width$} {:<setting_width$} {:<note_width$}",
        "stage [ms]",
        "lsp",
        "vscode",
        "wasm",
        note_header,
        stage_width = STAGE_COL_WIDTH,
        setting_width = SETTING_COL_WIDTH,
        note_width = RUNTIME_NOTE_COL_WIDTH,
    )));
    output.push_str(&table_divider());
    for row in build_runtime_matrix_rows(report) {
        output.push_str(&runtime_matrix_row(
            row.label,
            row.lsp_ms,
            row.vscode_ms,
            row.wasm_ms,
            &row.note,
        ));
    }
    output.push_str(&table_divider());
    output
}

pub(super) fn render_warm_latency_snapshot_table(snapshot: &WarmLatencySnapshot) -> String {
    const LABEL_COL_WIDTH: usize = 19;
    const ROUTE_COL_WIDTH: usize = 12;
    const LAYER_COL_WIDTH: usize = 15;
    const COUNT_COL_WIDTH: usize = 7;
    const PERCENTILE_COL_WIDTH: usize = 5;
    const BUDGET_COL_WIDTH: usize = 6;
    const STATUS_COL_WIDTH: usize = 2;

    let mut output = String::new();
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line("Warm latency snapshot [client ms]"));
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line(&format!(
        "{:<label_width$} {:<route_width$} {:<layer_width$} {:>count_width$} {:>percentile_width$} {:>percentile_width$} {:>budget_width$} {:>status_width$}",
        "path",
        "route",
        "layer",
        "samples",
        "p50",
        "p95",
        "budget",
        "ok",
        label_width = LABEL_COL_WIDTH,
        route_width = ROUTE_COL_WIDTH,
        layer_width = LAYER_COL_WIDTH,
        count_width = COUNT_COL_WIDTH,
        percentile_width = PERCENTILE_COL_WIDTH,
        budget_width = BUDGET_COL_WIDTH,
        status_width = STATUS_COL_WIDTH,
    )));
    output.push_str(&table_divider());
    for measurement in &snapshot.measurements {
        output.push_str(&fixed_width_line(&format!(
            "{:<label_width$} {:<route_width$} {:<layer_width$} {:>count_width$} {:>percentile_width$} {:>percentile_width$} {:>budget_width$} {:>status_width$}",
            fit_label(&measurement.label, LABEL_COL_WIDTH),
            measurement
                .request_path
                .map(NavigationRequestPath::label)
                .unwrap_or("-"),
            measurement.semantic_layer.as_deref().unwrap_or("-"),
            measurement.samples,
            measurement.p50_ms,
            measurement.p95_ms,
            measurement.budget_ms,
            short_bool_label(measurement.within_budget),
            label_width = LABEL_COL_WIDTH,
            route_width = ROUTE_COL_WIDTH,
            layer_width = LAYER_COL_WIDTH,
            count_width = COUNT_COL_WIDTH,
            percentile_width = PERCENTILE_COL_WIDTH,
            budget_width = BUDGET_COL_WIDTH,
            status_width = STATUS_COL_WIDTH,
        )));
    }
    output.push_str(&table_divider());
    output
}

pub(super) fn render_lsp_api_validation_table(
    report: &LspMslCompletionBenchmarkReport,
) -> Option<String> {
    const OP_COL_WIDTH: usize = 18;
    const KIND_COL_WIDTH: usize = 4;
    const MS_COL_WIDTH: usize = 5;
    const DETAIL_COL_WIDTH: usize = 47;

    if report.lsp_api_validation.operations.is_empty() {
        return None;
    }

    let mut output = String::new();
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line("LSP API validation [client ms]"));
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line(&format!(
        "{:<op_width$} {:<kind_width$} {:>ms_width$} {:<detail_width$}",
        "operation",
        "kind",
        "ms",
        "detail",
        op_width = OP_COL_WIDTH,
        kind_width = KIND_COL_WIDTH,
        ms_width = MS_COL_WIDTH,
        detail_width = DETAIL_COL_WIDTH,
    )));
    output.push_str(&table_divider());
    for entry in &report.lsp_api_validation.operations {
        output.push_str(&fixed_width_line(&format!(
            "{:<op_width$} {:<kind_width$} {:>ms_width$} {:<detail_width$}",
            fit_label(&entry.operation, OP_COL_WIDTH),
            fit_label(&entry.kind, KIND_COL_WIDTH),
            entry
                .client_ms
                .map(|ms| ms.to_string())
                .unwrap_or_else(|| "-".to_string()),
            fit_label(&entry.detail, DETAIL_COL_WIDTH),
            op_width = OP_COL_WIDTH,
            kind_width = KIND_COL_WIDTH,
            ms_width = MS_COL_WIDTH,
            detail_width = DETAIL_COL_WIDTH,
        )));
    }
    output.push_str(&table_divider());
    Some(output)
}

pub(super) fn render_completion_timing_breakdown_table(
    report: &LspMslCompletionBenchmarkReport,
) -> String {
    const PROBE_COL_WIDTH: usize = 16;
    const RUN_COL_WIDTH: usize = 4;
    const VALUE_COL_WIDTH: usize = 5;
    const ROUTE_COL_WIDTH: usize = 6;
    const LAYER_COL_WIDTH: usize = 16;

    let mut output = String::new();
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line("Completion timing breakdown [LSP ms]"));
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line(&format!(
        "{:<probe_width$} {:<run_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:<route_width$} {:<layer_width$}",
        "probe",
        "run",
        "cli",
        "tot",
        "src",
        "lib",
        "prime",
        "qchk",
        "hand",
        "route",
        "layer",
        probe_width = PROBE_COL_WIDTH,
        run_width = RUN_COL_WIDTH,
        value_width = VALUE_COL_WIDTH,
        route_width = ROUTE_COL_WIDTH,
        layer_width = LAYER_COL_WIDTH,
    )));
    output.push_str(&table_divider());
    for probe in &report.probe_results {
        for (run, measurement) in [
            ("cold", &probe.cold),
            ("warm", &probe.warm),
            ("edit", &probe.edited),
        ] {
            output.push_str(&fixed_width_line(&format!(
                "{:<probe_width$} {:<run_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:<route_width$} {:<layer_width$}",
                fit_label(&probe.probe_name, PROBE_COL_WIDTH),
                run,
                measurement.client_ms,
                measurement.lsp.total_ms,
                measurement.lsp.source_library_load_ms,
                measurement.lsp.completion_library_load_ms,
                measurement.lsp.library_completion_prime_ms,
                measurement.lsp.query_fast_path_check_ms,
                measurement.lsp.completion_handler_ms,
                completion_route_label(&measurement.lsp),
                fit_label(&measurement.lsp.semantic_layer, LAYER_COL_WIDTH),
                probe_width = PROBE_COL_WIDTH,
                run_width = RUN_COL_WIDTH,
                value_width = VALUE_COL_WIDTH,
                route_width = ROUTE_COL_WIDTH,
                layer_width = LAYER_COL_WIDTH,
            )));
        }
    }
    output.push_str(&table_divider());
    output
}

pub(super) fn render_completion_edit_breakdown_table(
    report: &LspMslCompletionBenchmarkReport,
) -> String {
    const PROBE_COL_WIDTH: usize = 16;
    const VALUE_COL_WIDTH: usize = 7;
    const ROUTE_COL_WIDTH: usize = 6;

    let mut output = String::new();
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line("Edited completion breakdown [ms]"));
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line(&format!(
        "{:<probe_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:<route_width$}",
        "probe",
        "edit",
        "live",
        "parse",
        "compl",
        "total",
        "route",
        probe_width = PROBE_COL_WIDTH,
        value_width = VALUE_COL_WIDTH,
        route_width = ROUTE_COL_WIDTH,
    )));
    output.push_str(&table_divider());
    for probe in &report.probe_results {
        let edited = &probe.edited;
        output.push_str(&fixed_width_line(&format!(
            "{:<probe_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:>value_width$} {:<route_width$}",
            fit_label(&probe.probe_name, PROBE_COL_WIDTH),
            edited
                .preceding_edit_client_ms
                .map(|value| value.to_string())
                .unwrap_or_else(|| "-".to_string()),
            edited
                .preceding_live_diagnostics_ms
                .map(|value| value.to_string())
                .unwrap_or_else(|| "-".to_string()),
            edited
                .preceding_document_parse_ms
                .map(|value| value.to_string())
                .unwrap_or_else(|| "-".to_string()),
            edited.client_ms,
            edited.lsp.total_ms,
            completion_route_label(&edited.lsp),
            probe_width = PROBE_COL_WIDTH,
            value_width = VALUE_COL_WIDTH,
            route_width = ROUTE_COL_WIDTH,
        )));
    }
    output.push_str(&table_divider());
    output
}

pub(super) fn render_completion_cache_stats_table(
    report: &LspMslCompletionBenchmarkReport,
) -> String {
    const PROBE_COL_WIDTH: usize = 16;
    const RUN_COL_WIDTH: usize = 4;
    const COUNT_COL_WIDTH: usize = 6;
    const MODEL_COL_WIDTH: usize = 4;

    let mut output = String::new();
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line("Completion query/cache stats [hit/miss]"));
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line(&format!(
        "{:<probe_width$} {:<run_width$} {:>count_width$} {:>count_width$} {:>count_width$} {:>count_width$} {:>count_width$} {:>count_width$} {:>count_width$} {:>model_width$}",
        "probe",
        "run",
        "decl",
        "scope",
        "pkg",
        "orph",
        "ns",
        "file",
        "lib",
        "mdl",
        probe_width = PROBE_COL_WIDTH,
        run_width = RUN_COL_WIDTH,
        count_width = COUNT_COL_WIDTH,
        model_width = MODEL_COL_WIDTH,
    )));
    output.push_str(&table_divider());
    for probe in &report.probe_results {
        for (run, measurement) in [
            ("cold", &probe.cold),
            ("warm", &probe.warm),
            ("edit", &probe.edited),
        ] {
            let entry = &measurement.lsp;
            output.push_str(&fixed_width_line(&format!(
                "{:<probe_width$} {:<run_width$} {:>count_width$} {:>count_width$} {:>count_width$} {:>count_width$} {:>count_width$} {:>count_width$} {:>count_width$} {:>model_width$}",
                fit_label(&probe.probe_name, PROBE_COL_WIDTH),
                run,
                hit_miss_cell(
                    entry.declaration_index_query_hits,
                    entry.declaration_index_query_misses,
                ),
                hit_miss_cell(entry.scope_query_hits, entry.scope_query_misses),
                hit_miss_cell(
                    entry.source_set_package_membership_query_hits,
                    entry.source_set_package_membership_query_misses,
                ),
                hit_miss_cell(
                    entry.orphan_package_membership_query_hits,
                    entry.orphan_package_membership_query_misses,
                ),
                hit_miss_cell(
                    entry.namespace_index_query_hits,
                    entry.namespace_index_query_misses,
                ),
                hit_miss_cell(
                    entry.file_item_index_query_hits,
                    entry.file_item_index_query_misses,
                ),
                hit_miss_cell(
                    entry.session_cache_delta.library_completion_cache_hits,
                    entry.session_cache_delta.library_completion_cache_misses,
                ),
                model_build_cell(&entry.session_cache_delta),
                probe_width = PROBE_COL_WIDTH,
                run_width = RUN_COL_WIDTH,
                count_width = COUNT_COL_WIDTH,
                model_width = MODEL_COL_WIDTH,
            )));
        }
    }
    output.push_str(&table_divider());
    output
}

struct RuntimeMatrixDisplayRow {
    label: &'static str,
    lsp_ms: Option<u64>,
    vscode_ms: Option<u64>,
    wasm_ms: Option<u64>,
    note: String,
}

fn build_runtime_matrix_rows(
    report: &LspMslCompletionBenchmarkReport,
) -> [RuntimeMatrixDisplayRow; 7] {
    let lsp_open = find_validation_entry(&report.lsp_api_validation, "didOpen");
    let vscode_open = find_runtime_entry(&report.vscode_runtime_smoke, "open");
    let wasm_archive = find_runtime_entry(&report.wasm_runtime_smoke, "archive-load");
    let wasm_open = find_runtime_entry(&report.wasm_runtime_smoke, "open");

    [
        total_runtime_row(
            "open",
            lsp_open.and_then(|entry| entry.client_ms),
            vscode_open.and_then(|entry| entry.client_ms),
            wasm_open.and_then(|entry| entry.client_ms),
            "doc ready",
        ),
        total_runtime_row(
            "download+unzip",
            None,
            None,
            wasm_archive.and_then(|entry| entry.client_ms),
            "browser archive prep",
        ),
        shared_runtime_row(
            "completion:1st",
            "completion",
            report,
            Some("library-load"),
            Some("library-load"),
            Some("library-load"),
        ),
        shared_runtime_row(
            "codeLens",
            "codeLens",
            report,
            Some("codeLens"),
            Some("codeLens"),
            Some("codeLens"),
        ),
        shared_runtime_row(
            "completion:warm",
            "completion",
            report,
            Some("completion:warm"),
            Some("completion:warm"),
            Some("completion:warm"),
        ),
        shared_runtime_row(
            "hover",
            "hover",
            report,
            Some("hover:warm"),
            Some("hover"),
            Some("hover"),
        ),
        shared_runtime_row(
            "definition",
            "definition",
            report,
            Some("definition:warm"),
            Some("definition"),
            Some("definition"),
        ),
    ]
}

fn total_runtime_row(
    label: &'static str,
    lsp_ms: Option<u64>,
    vscode_ms: Option<u64>,
    wasm_ms: Option<u64>,
    note: &'static str,
) -> RuntimeMatrixDisplayRow {
    RuntimeMatrixDisplayRow {
        label,
        lsp_ms,
        vscode_ms,
        wasm_ms,
        note: note.to_string(),
    }
}

fn shared_runtime_row(
    label: &'static str,
    note_kind: &'static str,
    report: &LspMslCompletionBenchmarkReport,
    lsp_op: Option<&'static str>,
    vscode_op: Option<&'static str>,
    wasm_op: Option<&'static str>,
) -> RuntimeMatrixDisplayRow {
    let lsp_entry = lsp_op.and_then(|op| find_validation_entry(&report.lsp_api_validation, op));
    let vscode_entry =
        vscode_op.and_then(|op| find_runtime_entry(&report.vscode_runtime_smoke, op));
    let wasm_entry = wasm_op.and_then(|op| find_runtime_entry(&report.wasm_runtime_smoke, op));
    let lsp_detail = lsp_entry.map(|entry| parse_runtime_detail(&entry.detail));
    let vscode_detail = vscode_entry.map(|entry| parse_runtime_detail(&entry.detail));
    let wasm_detail = wasm_entry.map(|entry| parse_runtime_detail(&entry.detail));
    RuntimeMatrixDisplayRow {
        label,
        lsp_ms: lsp_entry.and_then(|entry| entry.client_ms),
        vscode_ms: vscode_entry.and_then(|entry| entry.client_ms),
        wasm_ms: wasm_entry.and_then(|entry| entry.client_ms),
        note: comparison_note(
            note_kind,
            lsp_detail.as_ref(),
            vscode_detail.as_ref(),
            wasm_detail.as_ref(),
        ),
    }
}

fn runtime_matrix_row(
    label: &str,
    lsp_ms: Option<u64>,
    vscode_ms: Option<u64>,
    wasm_ms: Option<u64>,
    note: &str,
) -> String {
    const STAGE_COL_WIDTH: usize = 15;
    const SETTING_COL_WIDTH: usize = 8;

    fixed_width_line(&format!(
        "{:<stage_width$} {:<setting_width$} {:<setting_width$} {:<setting_width$} {:<note_width$}",
        fit_label(label, STAGE_COL_WIDTH),
        fit_label(&comparison_cell(lsp_ms), SETTING_COL_WIDTH),
        fit_label(&comparison_cell(vscode_ms), SETTING_COL_WIDTH),
        fit_label(&comparison_cell(wasm_ms), SETTING_COL_WIDTH),
        fit_label(note, RUNTIME_NOTE_COL_WIDTH),
        stage_width = STAGE_COL_WIDTH,
        setting_width = SETTING_COL_WIDTH,
        note_width = RUNTIME_NOTE_COL_WIDTH,
    ))
}

pub(super) fn render_failure_details_table(
    report: &LspMslCompletionBenchmarkReport,
) -> Option<String> {
    let mut failures = Vec::new();
    collect_runtime_failures(
        &mut failures,
        "vscode-runtime",
        &report.vscode_runtime_smoke,
    );
    collect_runtime_failures(&mut failures, "wasm-runtime", &report.wasm_runtime_smoke);
    collect_validation_failures(&mut failures, "lsp-api", &report.lsp_api_validation);
    collect_surface_failures(
        &mut failures,
        "vscode-surf",
        &report.vscode_surface_validation,
    );
    collect_surface_failures(&mut failures, "wasm-surf", &report.wasm_surface_validation);
    if failures.is_empty() {
        return None;
    }

    let mut output = String::new();
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line("Failures"));
    output.push_str(&table_divider());
    output.push_str(&fixed_width_line(&format!(
        "{:<14} {:<item_width$} {:<44}",
        "area",
        "item",
        "note",
        item_width = VALIDATION_ITEM_COL_WIDTH,
    )));
    output.push_str(&table_divider());
    for (area, item, note) in failures {
        output.push_str(&fixed_width_line(&format!(
            "{:<14} {:<item_width$} {:<44}",
            fit_label(&area, 14),
            fit_label(&item, VALIDATION_ITEM_COL_WIDTH),
            fit_label(&note, 44),
            item_width = VALIDATION_ITEM_COL_WIDTH,
        )));
    }
    output.push_str(&table_divider());
    Some(output)
}

fn find_validation_entry<'a>(
    report: &'a LspApiValidationReport,
    operation: &str,
) -> Option<&'a LspApiValidationEntry> {
    report
        .operations
        .iter()
        .find(|entry| entry.operation == operation)
}

fn find_runtime_entry<'a>(
    report: &'a RuntimeSmokeReport,
    operation: &str,
) -> Option<&'a RuntimeSmokeEntry> {
    report
        .entries
        .iter()
        .find(|entry| entry.operation == operation)
}

fn comparison_cell(client_ms: Option<u64>) -> String {
    client_ms
        .map(|value| value.to_string())
        .unwrap_or_else(|| "-".to_string())
}

fn comparison_note(
    note_kind: &str,
    lsp: Option<&RuntimeDetailMetrics>,
    vscode: Option<&RuntimeDetailMetrics>,
    wasm: Option<&RuntimeDetailMetrics>,
) -> String {
    match note_kind {
        "codeLens" => matrix_triplet_note("count", lsp, vscode, wasm, |metrics| metrics.count),
        "completion" => matrix_triplet_note("items", lsp, vscode, wasm, |metrics| metrics.items),
        "hover" => matrix_triplet_note("hits", lsp, vscode, wasm, |metrics| metrics.hits),
        "definition" => {
            let hits = matrix_triplet_note("hits", lsp, vscode, wasm, |metrics| {
                metrics.hits.or(metrics.targets)
            });
            let cross =
                matrix_triplet_bool_note("cross", lsp, vscode, wasm, |metrics| metrics.cross);
            join_matrix_notes([hits, cross])
        }
        _ => fallback_matrix_note(lsp, vscode, wasm),
    }
}

fn matrix_triplet_note<F>(
    label: &str,
    lsp: Option<&RuntimeDetailMetrics>,
    vscode: Option<&RuntimeDetailMetrics>,
    wasm: Option<&RuntimeDetailMetrics>,
    value: F,
) -> String
where
    F: Fn(&RuntimeDetailMetrics) -> Option<u64>,
{
    let values = [
        value_or_dash(lsp.and_then(&value)),
        value_or_dash(vscode.and_then(&value)),
        value_or_dash(wasm.and_then(&value)),
    ];
    if values.iter().all(|entry| entry == "-") {
        return fallback_matrix_note(lsp, vscode, wasm);
    }
    format!("{label}={}/{}/{}", values[0], values[1], values[2])
}

fn matrix_triplet_bool_note<F>(
    label: &str,
    lsp: Option<&RuntimeDetailMetrics>,
    vscode: Option<&RuntimeDetailMetrics>,
    wasm: Option<&RuntimeDetailMetrics>,
    value: F,
) -> String
where
    F: Fn(&RuntimeDetailMetrics) -> Option<bool>,
{
    let values = [
        bool_or_dash(lsp.and_then(&value)),
        bool_or_dash(vscode.and_then(&value)),
        bool_or_dash(wasm.and_then(&value)),
    ];
    if values.iter().all(|entry| entry == "-") {
        return String::new();
    }
    format!("{label}={}/{}/{}", values[0], values[1], values[2])
}

fn join_matrix_notes<const N: usize>(notes: [String; N]) -> String {
    notes
        .into_iter()
        .filter(|note| !note.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

fn fallback_matrix_note(
    lsp: Option<&RuntimeDetailMetrics>,
    vscode: Option<&RuntimeDetailMetrics>,
    wasm: Option<&RuntimeDetailMetrics>,
) -> String {
    for detail in [vscode, wasm, lsp].into_iter().flatten() {
        if runtime_detail_has_stage_metrics(detail) {
            if let Some(items) = detail.items {
                return format!("items={items}");
            }
            return "w/l/s/c".to_string();
        }
        if !detail.note.is_empty() {
            return detail.note.clone();
        }
    }
    String::new()
}

fn value_or_dash(value: Option<u64>) -> String {
    value
        .map(|value| value.to_string())
        .unwrap_or_else(|| "-".to_string())
}

fn bool_or_dash(value: Option<bool>) -> String {
    value.map(short_bool_label).unwrap_or("-").to_string()
}

fn completion_route_label(entry: &CompletionTimingEntry) -> &'static str {
    if entry.built_resolved_tree || entry.had_resolved_cache_before {
        "res"
    } else if entry.query_fast_path_matched {
        "qry"
    } else if entry.ast_fast_path_matched {
        "ast"
    } else if entry.needs_resolved_session {
        "need"
    } else if entry.session_cache_delta.library_completion_cache_hits > 0
        || entry.session_cache_delta.library_completion_cache_misses > 0
        || entry.namespace_index_query_hits > 0
        || entry.namespace_index_query_misses > 0
    {
        "ns"
    } else {
        "-"
    }
}

fn hit_miss_cell(hits: u64, misses: u64) -> String {
    if hits == 0 && misses == 0 {
        "-".to_string()
    } else {
        format!("{hits}/{misses}")
    }
}

fn model_build_cell(delta: &CompletionSessionCacheDelta) -> String {
    let builds = delta.instantiated_model_builds
        + delta.typed_model_builds
        + delta.flat_model_builds
        + delta.dae_model_builds;
    if builds == 0 {
        "-".to_string()
    } else {
        builds.to_string()
    }
}

fn runtime_detail_has_stage_metrics(parsed: &RuntimeDetailMetrics) -> bool {
    parsed.items.is_some()
        || parsed.lsp_ms.is_some()
        || parsed.source_ms.is_some()
        || parsed.completion_ms.is_some()
}

#[derive(Default)]
struct RuntimeDetailMetrics {
    items: Option<u64>,
    count: Option<u64>,
    hits: Option<u64>,
    targets: Option<u64>,
    cross: Option<bool>,
    lsp_ms: Option<u64>,
    source_ms: Option<u64>,
    completion_ms: Option<u64>,
    note: String,
}

fn parse_runtime_detail(detail: &str) -> RuntimeDetailMetrics {
    let mut metrics = RuntimeDetailMetrics::default();
    let mut note_tokens = Vec::new();
    for token in detail.split_whitespace() {
        match token.split_once('=') {
            Some(("i", value)) => metrics.items = value.parse().ok(),
            Some(("count", value)) => metrics.count = value.parse().ok(),
            Some(("hits", value)) => metrics.hits = value.parse().ok(),
            Some(("targets", value)) => metrics.targets = value.parse().ok(),
            Some(("cross", value)) => metrics.cross = Some(value == "Y"),
            Some(("l", value)) => metrics.lsp_ms = value.parse().ok(),
            Some(("s", value)) => metrics.source_ms = value.parse().ok(),
            Some(("c", value)) => metrics.completion_ms = value.parse().ok(),
            _ => note_tokens.push(token),
        }
    }
    if !note_tokens.is_empty() {
        metrics.note = note_tokens.join(" ");
    } else if !runtime_detail_has_stage_metrics(&metrics) {
        metrics.note = detail.to_string();
    }
    metrics
}

fn collect_runtime_failures(
    failures: &mut Vec<(String, String, String)>,
    area: &str,
    report: &RuntimeSmokeReport,
) {
    if report.status != "pass" {
        failures.push((
            area.to_string(),
            "status".to_string(),
            format!("{} {}", report.status, report.note),
        ));
    }
    for entry in &report.entries {
        if !entry.ok {
            failures.push((
                area.to_string(),
                entry.operation.clone(),
                entry.detail.clone(),
            ));
        }
    }
}

fn collect_validation_failures(
    failures: &mut Vec<(String, String, String)>,
    area: &str,
    report: &LspApiValidationReport,
) {
    for entry in &report.operations {
        if !entry.ok {
            failures.push((
                area.to_string(),
                entry.operation.clone(),
                entry.detail.clone(),
            ));
        }
    }
}

fn collect_surface_failures(
    failures: &mut Vec<(String, String, String)>,
    area: &str,
    report: &SurfaceCoverageReport,
) {
    for entry in &report.entries {
        if !entry.ok {
            failures.push((area.to_string(), entry.surface.clone(), entry.proof.clone()));
        }
    }
}

fn table_divider() -> String {
    format!("+{}+\n", "-".repeat(TABLE_CONTENT_WIDTH))
}

fn fixed_width_line(content: &str) -> String {
    let fitted = fit_label(content, TABLE_CONTENT_WIDTH);
    format!("|{fitted:<width$}|\n", width = TABLE_CONTENT_WIDTH)
}

pub(super) fn append_tabled_section(output: &mut String, section: &str) {
    let divider = table_divider();
    if output.ends_with(&divider)
        && let Some(rest) = section.strip_prefix(&divider)
    {
        output.push_str(rest);
    } else {
        output.push_str(section);
    }
}

fn fit_label(value: &str, width: usize) -> String {
    if value.len() <= width {
        return value.to_string();
    }
    if width <= 3 {
        return value[..width].to_string();
    }
    format!("{}...", &value[..width - 3])
}

pub(crate) fn short_bool_label(value: bool) -> &'static str {
    if value { "Y" } else { "N" }
}

pub(crate) fn display_output_path(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .display()
        .to_string()
}

#[cfg(test)]
mod tests {
    use super::{
        CompletionMeasurementReport, CompletionSessionCacheDelta, CompletionTimingEntry,
        LspApiValidationEntry, LspApiValidationReport, LspCompletionProbeReport,
        LspMslCompletionBenchmarkReport, NavigationRequestPath, RuntimeSmokeEntry,
        RuntimeSmokeReport, SurfaceCoverageEntry, SurfaceCoverageReport, TERMINAL_TABLE_WIDTH,
        WarmLatencyMeasurement, WarmLatencySnapshot, build_runtime_matrix_rows,
        completion_stage_metrics, extract_completion_metrics, hit_miss_cell, parse_runtime_detail,
        probe_position, render_lsp_completion_table, render_warm_latency_snapshot_table,
    };
    use serde_json::json;

    fn sample_report() -> LspMslCompletionBenchmarkReport {
        LspMslCompletionBenchmarkReport {
            lsp_binary: "/tmp/rumoca-lsp".to_string(),
            modelica_paths: vec!["/tmp/msl".to_string()],
            probe_results: vec![LspCompletionProbeReport {
                probe_name: "electrical-resistor".to_string(),
                document_path: "/tmp/Modelica 4.1.0/Electrical/Analog/Examples/Resistor.mo"
                    .to_string(),
                cold: sample_measurement(812, 17, Some(711), 31, 760, false),
                warm: sample_measurement(94, 17, None, 0, 82, true),
                edited: sample_edited_measurement(26, 19, 7, 6, 18),
            }],
            lsp_api_validation: sample_lsp_api_validation(),
            vscode_runtime_smoke: sample_vscode_runtime_smoke(),
            wasm_runtime_smoke: sample_wasm_runtime_smoke(),
            wasm_surface_validation: SurfaceCoverageReport {
                area: "WASM".to_string(),
                entries: vec![SurfaceCoverageEntry {
                    surface: "parse".to_string(),
                    kind: "check".to_string(),
                    ok: true,
                    proof: "bind-wasm:parse".to_string(),
                }],
            },
            vscode_surface_validation: SurfaceCoverageReport {
                area: "VS Code".to_string(),
                entries: vec![SurfaceCoverageEntry {
                    surface: "activate".to_string(),
                    kind: "life".to_string(),
                    ok: true,
                    proof: "vscode-smoke:activate".to_string(),
                }],
            },
        }
    }

    fn sample_lsp_api_validation_operations() -> Vec<LspApiValidationEntry> {
        vec![
            LspApiValidationEntry {
                operation: "initialize".to_string(),
                kind: "req".to_string(),
                ok: true,
                client_ms: Some(5),
                detail: "caps ok exec=8 inlay=off".to_string(),
            },
            LspApiValidationEntry {
                operation: "didOpen".to_string(),
                kind: "note".to_string(),
                ok: true,
                client_ms: Some(7),
                detail: "live-diag=0".to_string(),
            },
            LspApiValidationEntry {
                operation: "library-load".to_string(),
                kind: "req".to_string(),
                ok: true,
                client_ms: Some(18),
                detail: "msl i=17 l=760 s=31 c=0".to_string(),
            },
            LspApiValidationEntry {
                operation: "completion:cold".to_string(),
                kind: "req".to_string(),
                ok: true,
                client_ms: Some(12),
                detail: "msl i=17 l=760 s=31 c=0".to_string(),
            },
            LspApiValidationEntry {
                operation: "codeLens".to_string(),
                kind: "req".to_string(),
                ok: true,
                client_ms: Some(6),
                detail: "full-msl count=1".to_string(),
            },
            LspApiValidationEntry {
                operation: "completion:warm".to_string(),
                kind: "req".to_string(),
                ok: true,
                client_ms: Some(4),
                detail: "msl i=17 l=82 s=0 c=0".to_string(),
            },
            LspApiValidationEntry {
                operation: "hover:warm".to_string(),
                kind: "req".to_string(),
                ok: true,
                client_ms: Some(1),
                detail: "ast-local=0ms".to_string(),
            },
            LspApiValidationEntry {
                operation: "definition:warm".to_string(),
                kind: "req".to_string(),
                ok: true,
                client_ms: Some(2),
                detail: "ast-local=0ms".to_string(),
            },
            LspApiValidationEntry {
                operation: "didChange".to_string(),
                kind: "note".to_string(),
                ok: true,
                client_ms: Some(21),
                detail: "synthetic Shifted".to_string(),
            },
        ]
    }

    fn sample_warm_latency_measurements() -> Vec<WarmLatencyMeasurement> {
        vec![
            WarmLatencyMeasurement {
                label: "local completion".to_string(),
                request_path: None,
                semantic_layer: Some("class_interface".to_string()),
                samples: 20,
                p50_ms: 4,
                p95_ms: 9,
                budget_ms: 30,
                within_budget: true,
            },
            WarmLatencyMeasurement {
                label: "hover".to_string(),
                request_path: Some(NavigationRequestPath::QueryOnly),
                semantic_layer: Some("class_interface".to_string()),
                samples: 20,
                p50_ms: 3,
                p95_ms: 5,
                budget_ms: 30,
                within_budget: true,
            },
            WarmLatencyMeasurement {
                label: "save diagnostics".to_string(),
                request_path: None,
                semantic_layer: Some("model_stage".to_string()),
                samples: 20,
                p50_ms: 72,
                p95_ms: 108,
                budget_ms: 250,
                within_budget: true,
            },
        ]
    }

    fn sample_lsp_api_validation() -> LspApiValidationReport {
        LspApiValidationReport {
            operations: sample_lsp_api_validation_operations(),
            warm_latency_snapshot: WarmLatencySnapshot {
                measurements: sample_warm_latency_measurements(),
            },
        }
    }

    fn sample_vscode_runtime_smoke() -> RuntimeSmokeReport {
        RuntimeSmokeReport {
            area: "VS Code runtime smoke".to_string(),
            status: "pass".to_string(),
            note: "headless extension-host smoke against full MSL".to_string(),
            entries: vec![
                RuntimeSmokeEntry {
                    operation: "activate".to_string(),
                    ok: true,
                    client_ms: Some(41),
                    detail: "extension host".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "open".to_string(),
                    ok: true,
                    client_ms: Some(19),
                    detail: "full-MSL doc".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "library-load".to_string(),
                    ok: true,
                    client_ms: Some(28),
                    detail: "i=17 l=20 s=12 c=0".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "codeLens".to_string(),
                    ok: true,
                    client_ms: Some(15),
                    detail: "count=1".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "completion:warm".to_string(),
                    ok: true,
                    client_ms: Some(32),
                    detail: "i=17 l=20 s=12 c=0".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "hover".to_string(),
                    ok: true,
                    client_ms: Some(6),
                    detail: "hits=1".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "definition".to_string(),
                    ok: true,
                    client_ms: Some(7),
                    detail: "hits=1 cross=Y".to_string(),
                },
            ],
        }
    }

    fn sample_wasm_runtime_smoke() -> RuntimeSmokeReport {
        RuntimeSmokeReport {
            area: "WASM runtime smoke".to_string(),
            status: "pass".to_string(),
            note: "headless browser smoke against full MSL".to_string(),
            entries: vec![
                RuntimeSmokeEntry {
                    operation: "archive-load".to_string(),
                    ok: true,
                    client_ms: Some(101),
                    detail: "libs=1".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "open".to_string(),
                    ok: true,
                    client_ms: Some(11),
                    detail: "full-MSL doc".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "library-load".to_string(),
                    ok: true,
                    client_ms: Some(9),
                    detail: "i=17 l=1 s=0 c=0".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "codeLens".to_string(),
                    ok: true,
                    client_ms: Some(2),
                    detail: "count=1".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "completion:warm".to_string(),
                    ok: true,
                    client_ms: Some(18),
                    detail: "i=17 l=1 s=0 c=0".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "hover".to_string(),
                    ok: true,
                    client_ms: Some(3),
                    detail: "hits=1".to_string(),
                },
                RuntimeSmokeEntry {
                    operation: "definition".to_string(),
                    ok: true,
                    client_ms: Some(4),
                    detail: "hits=1 cross=Y".to_string(),
                },
            ],
        }
    }

    fn sample_measurement(
        client_ms: u64,
        completion_count: usize,
        resolved_build_ms: Option<u64>,
        library_load_ms: u64,
        total_ms: u64,
        warm_cache_hit: bool,
    ) -> CompletionMeasurementReport {
        CompletionMeasurementReport {
            client_ms,
            completion_count,
            expected_completion_present: true,
            preceding_edit_client_ms: None,
            preceding_live_diagnostics_ms: None,
            preceding_document_parse_ms: None,
            lsp: CompletionTimingEntry {
                requested_edit_epoch: 0,
                request_was_stale: false,
                uri: "/tmp/Modelica 4.1.0/Electrical/Analog/Examples/Resistor.mo".to_string(),
                semantic_layer: "package_def_map".to_string(),
                source_library_load_ms: library_load_ms,
                completion_library_load_ms: 0,
                library_completion_prime_ms: 9,
                needs_resolved_session: false,
                ast_fast_path_matched: false,
                query_fast_path_check_ms: 0,
                query_fast_path_matched: false,
                resolved_build_ms,
                completion_handler_ms: 14,
                total_ms,
                built_resolved_tree: resolved_build_ms.is_some(),
                had_resolved_cache_before: false,
                namespace_index_query_hits: u64::from(warm_cache_hit),
                namespace_index_query_misses: u64::from(!warm_cache_hit),
                file_item_index_query_hits: 0,
                file_item_index_query_misses: 0,
                declaration_index_query_hits: 3,
                declaration_index_query_misses: 0,
                scope_query_hits: 0,
                scope_query_misses: 0,
                source_set_package_membership_query_hits: 1,
                source_set_package_membership_query_misses: u64::from(!warm_cache_hit),
                orphan_package_membership_query_hits: 0,
                orphan_package_membership_query_misses: 0,
                class_name_count_after_ensure: 6519,
                session_cache_delta: CompletionSessionCacheDelta {
                    declaration_index_query_hits: 3,
                    source_set_package_membership_query_hits: 1,
                    library_completion_cache_hits: u64::from(warm_cache_hit),
                    library_completion_cache_misses: u64::from(!warm_cache_hit),
                    ..CompletionSessionCacheDelta::default()
                },
            },
        }
    }

    fn sample_edited_measurement(
        edit_client_ms: u64,
        live_diagnostics_ms: u64,
        parse_ms: u64,
        completion_client_ms: u64,
        completion_total_ms: u64,
    ) -> CompletionMeasurementReport {
        let mut measurement =
            sample_measurement(completion_client_ms, 17, None, 0, completion_total_ms, true);
        measurement.preceding_edit_client_ms = Some(edit_client_ms);
        measurement.preceding_live_diagnostics_ms = Some(live_diagnostics_ms);
        measurement.preceding_document_parse_ms = Some(parse_ms);
        measurement
    }

    #[test]
    fn probe_position_tracks_line_and_character() {
        let source = "model M\n  Modelica.\nend M;\n";
        let position = probe_position(source, "Modelica.").expect("probe position");
        assert_eq!(position.line, 1);
        assert_eq!(position.character, "  Modelica.".len() as u32);
    }

    #[test]
    fn completion_metrics_accept_array_and_completion_list_shapes() {
        let array_shape = json!([
            { "label": "Electrical" },
            { "label": "Blocks" }
        ]);
        let list_shape = json!({
            "items": [
                { "label": "Fluid" },
                { "label": "Mechanics" }
            ]
        });

        assert_eq!(
            extract_completion_metrics(&array_shape, "Electrical"),
            (2, true)
        );
        assert_eq!(
            extract_completion_metrics(&list_shape, "Clocked"),
            (2, false)
        );
    }

    #[test]
    fn completion_timing_entry_parses_snake_case_session_cache_delta() {
        let entry: CompletionTimingEntry = serde_json::from_value(json!({
            "uri": "/tmp/Resistor.mo",
            "sourceLibraryLoadMs": 12,
            "completionLibraryLoadMs": 0,
            "resolvedBuildMs": null,
            "completionHandlerMs": 4,
            "totalMs": 16,
            "builtResolvedTree": false,
            "hadResolvedCacheBefore": true,
            "sessionCacheDelta": {
                "standard_resolved_builds": 0,
                "semantic_navigation_cache_hits": 1,
                "semantic_navigation_cache_misses": 0,
                "semantic_navigation_builds": 0,
                "instantiated_model_cache_hits": 0,
                "instantiated_model_cache_misses": 1,
                "instantiated_model_builds": 1,
                "typed_model_cache_hits": 0,
                "typed_model_cache_misses": 1,
                "typed_model_builds": 1,
                "library_completion_cache_hits": 2,
                "library_completion_cache_misses": 0
            }
        }))
        .expect("completion timing entry should parse");

        assert_eq!(entry.session_cache_delta.semantic_navigation_cache_hits, 1);
        assert_eq!(entry.session_cache_delta.instantiated_model_cache_misses, 1);
        assert_eq!(entry.session_cache_delta.typed_model_builds, 1);
        assert_eq!(entry.session_cache_delta.library_completion_cache_hits, 2);
    }

    #[test]
    fn runtime_detail_parser_ignores_prefix_tokens_and_keeps_metrics() {
        let parsed = parse_runtime_detail("msl i=17 l=760 s=31 c=0");

        assert_eq!(parsed.items, Some(17));
        assert_eq!(parsed.lsp_ms, Some(760));
        assert_eq!(parsed.source_ms, Some(31));
        assert_eq!(parsed.completion_ms, Some(0));
        assert_eq!(parsed.note, "msl");
    }

    #[test]
    fn runtime_matrix_rows_show_exact_shared_values() {
        let report = sample_report();
        let rows = build_runtime_matrix_rows(&report);

        assert_eq!(rows[0].label, "open");
        assert_eq!(rows[0].lsp_ms, Some(7));
        assert_eq!(rows[0].vscode_ms, Some(19));
        assert_eq!(rows[0].wasm_ms, Some(11));
        assert_eq!(rows[0].note, "doc ready");

        assert_eq!(rows[1].label, "download+unzip");
        assert_eq!(rows[1].lsp_ms, None);
        assert_eq!(rows[1].vscode_ms, None);
        assert_eq!(rows[1].wasm_ms, Some(101));
        assert_eq!(rows[1].note, "browser archive prep");

        assert_eq!(rows[2].label, "completion:1st");
        assert_eq!(rows[2].lsp_ms, Some(18));
        assert_eq!(rows[2].vscode_ms, Some(28));
        assert_eq!(rows[2].wasm_ms, Some(9));
        assert_eq!(rows[2].note, "items=17/17/17");
        assert_eq!(rows[3].note, "count=1/1/1");
        assert_eq!(rows[4].note, "items=17/17/17");
        assert_eq!(rows[5].note, "hits=-/1/1");
        assert_eq!(rows[6].note, "hits=-/1/1 cross=-/Y/Y");
    }

    #[test]
    fn renders_direct_lsp_completion_table() {
        let report = sample_report();
        let table = render_lsp_completion_table(&report);
        assert!(!table.contains("Full-MSL probe summary"));
        assert!(!table.contains("Shared editor action matrix"));
        assert!(!table.contains("status lsp="));
        assert!(!table.contains("VS Code runtime smoke"));
        assert!(!table.contains("WASM runtime smoke"));
        assert!(!table.contains("surface validation"));
        assert!(!table.contains("validation lsp-api="));
        assert!(table.contains("open"));
        assert!(table.contains("download+unzip"));
        assert!(table.contains("completion:1st"));
        assert!(table.contains("completion:warm"));
        assert!(table.contains("stage [ms]"));
        assert!(table.contains("note"));
        assert!(table.contains("lsp"));
        assert!(table.contains("vscode"));
        assert!(table.contains("wasm"));
        assert!(table.contains("browser archive prep"));
        assert!(table.contains("Warm latency snapshot [client ms]"));
        assert!(table.contains("LSP API validation [client ms]"));
        assert!(table.contains("Completion timing breakdown [LSP ms]"));
        assert!(table.contains("Completion query/cache stats [hit/miss]"));
        assert!(table.contains("completion:warm"));
        assert!(table.contains("hover:warm"));
        assert!(table.contains("prime"));
        assert!(table.contains("qchk"));
        assert!(table.contains("route"));
        assert!(table.contains("decl"));
        assert!(table.contains("scope"));
        assert!(table.contains("pkg"));
        assert!(table.contains("orph"));
        assert!(table.contains("mdl"));
        assert!(table.contains("local completion"));
        assert!(table.contains("save diagnostics"));
        assert!(table.contains("count=1/1/1"));
        assert!(table.contains("hits=-/1/1"));
        assert!(!table.contains(
            "+------------------------------------------------------------------------------+\n+------------------------------------------------------------------------------+"
        ));
        assert!(!table.contains("probe            doc"));
        assert!(!table.contains("archive-load"));
        assert!(!table.contains("compile"));
        assert!(!table.contains("activate"));
        assert!(table.contains("library-load"));
        for line in table.lines() {
            assert_eq!(line.len(), TERMINAL_TABLE_WIDTH, "{line}");
            assert!(
                (line.starts_with('|') && line.ends_with('|'))
                    || (line.starts_with('+') && line.ends_with('+')),
                "{line}"
            );
        }
    }

    #[test]
    fn completion_stage_metrics_stay_compact() {
        assert_eq!(
            completion_stage_metrics(17, Some(980), Some(790), Some(85)),
            "i=17 l=980 s=790 c=85"
        );
    }

    #[test]
    fn renders_warm_latency_snapshot_table() {
        let snapshot = sample_lsp_api_validation().warm_latency_snapshot;
        let table = render_warm_latency_snapshot_table(&snapshot);

        assert!(table.contains("Warm latency snapshot [client ms]"));
        assert!(table.contains("path"));
        assert!(table.contains("route"));
        assert!(table.contains("layer"));
        assert!(table.contains("samples"));
        assert!(table.contains("p50"));
        assert!(table.contains("p95"));
        assert!(table.contains("budget"));
        assert!(table.contains("local completion"));
        assert!(table.contains("query-only"));
        assert!(table.contains("save diagnostics"));
        assert!(table.contains("20"));
        assert!(table.contains("250"));
    }

    #[test]
    fn runtime_detail_parser_extracts_completion_metrics() {
        let parsed = parse_runtime_detail("i=17 l=966 s=805 c=86");
        assert_eq!(parsed.items, Some(17));
        assert_eq!(parsed.lsp_ms, Some(966));
        assert_eq!(parsed.source_ms, Some(805));
        assert_eq!(parsed.completion_ms, Some(86));
        assert_eq!(parsed.note, "");

        let passthrough = parse_runtime_detail("hit=1 miss=0 parsed=0");
        assert_eq!(passthrough.items, None);
        assert_eq!(passthrough.note, "hit=1 miss=0 parsed=0");
    }

    #[test]
    fn runtime_detail_parser_extracts_shared_surface_counts() {
        let parsed = parse_runtime_detail("hits=1 cross=Y count=0");
        assert_eq!(parsed.hits, Some(1));
        assert_eq!(parsed.cross, Some(true));
        assert_eq!(parsed.count, Some(0));
    }

    #[test]
    fn hit_miss_cell_stays_compact() {
        assert_eq!(hit_miss_cell(0, 0), "-");
        assert_eq!(hit_miss_cell(3, 1), "3/1");
    }
}
