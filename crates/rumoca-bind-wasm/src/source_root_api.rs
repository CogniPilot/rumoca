use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use wasm_bindgen::prelude::*;

use rumoca_compile::Session;
use rumoca_compile::compile::{
    SourceRootKind, compile_phase_timing_stats, reset_compile_phase_timing_stats,
};
use rumoca_compile::parsing::{StoredDefinition, parse_source_to_ast};
#[cfg(not(target_arch = "wasm32"))]
use rumoca_compile::source_roots::resolve_source_root_cache_dir;

use super::{
    BUNDLED_SOURCE_ROOT_CACHE_BYTES, BUNDLED_SOURCE_ROOT_MANIFEST_JSON, BundledSourceRootManifest,
    SESSION, WASM_BUNDLED_SOURCE_ROOT_SET_ID, WASM_PROJECT_SOURCE_SET_ID,
    compile_source_in_session, wasm_elapsed_ms, wasm_timing_start,
};

#[derive(Debug, Clone, Copy, Default, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct WasmCompileBehaviorOptions {
    /// Compatibility option:
    /// allow `annotation(Evaluate=true)` on non-parameter/non-constant fields.
    ///
    /// Strict MLS behavior keeps this `false`.
    ///
    /// Example pattern (commonly used in PowerSystems records):
    /// `Boolean puUnits annotation(Evaluate=true, Dialog(...));`
    /// where `puUnits` is not explicitly declared `parameter` or `constant`.
    #[serde(default)]
    allow_non_param_evaluate_annotation: bool,
    /// Compatibility option:
    /// allow duplicate target assignment across separate `when` equations.
    ///
    /// Strict MLS behavior keeps this `false`.
    ///
    /// Note: valid distinct indexed targets (e.g. `t0[1]` and `t0[2]`) are
    /// accepted in strict mode and should not require this option.
    #[serde(default)]
    allow_multi_when_single_assign: bool,
}

fn parse_compile_behavior_options(
    options_json: &str,
) -> Result<WasmCompileBehaviorOptions, JsValue> {
    let trimmed = options_json.trim();
    if trimmed.is_empty() || trimmed == "{}" {
        return Ok(WasmCompileBehaviorOptions::default());
    }
    serde_json::from_str(trimmed)
        .map_err(|e| JsValue::from_str(&format!("Invalid compile options JSON: {}", e)))
}

fn semantic_strictness_from_options(options: WasmCompileBehaviorOptions) -> (bool, bool) {
    (
        !options.allow_non_param_evaluate_annotation,
        !options.allow_multi_when_single_assign,
    )
}

#[derive(Default)]
pub(crate) struct SourceLoadSummary {
    pub(crate) parsed_count: usize,
    pub(crate) inserted_count: usize,
    pub(crate) error_count: usize,
    pub(crate) skipped_files: Vec<String>,
}

struct ParsedProjectSourceLoad {
    definitions: Vec<(String, StoredDefinition)>,
    parsed_count: usize,
    skipped_files: Vec<String>,
}

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(message: &str);
}

fn report_parse_progress(scope: &str, current: usize, total: usize) {
    if total == 0 {
        return;
    }
    if current != 1 && current != total && !current.is_multiple_of(50) {
        return;
    }
    let percent = current.saturating_mul(100) / total;
    let message = format!("[WASM] {scope}: parsing {current}/{total} ({percent}%)");
    #[cfg(target_arch = "wasm32")]
    log(&message);
    #[cfg(not(target_arch = "wasm32"))]
    let _ = message;
}

fn parse_text_sources_json(sources_json: &str) -> Result<BTreeMap<String, String>, JsValue> {
    let trimmed = sources_json.trim();
    if trimmed.is_empty() {
        return Ok(BTreeMap::new());
    }
    serde_json::from_str(trimmed).map_err(|e| JsValue::from_str(&format!("Invalid JSON: {}", e)))
}

fn parse_binary_source_root_snapshot(
    bytes: &[u8],
) -> Result<Vec<(String, StoredDefinition)>, JsValue> {
    bincode::deserialize(bytes)
        .map_err(|e| JsValue::from_str(&format!("Invalid binary source-root cache: {}", e)))
}

fn bundled_source_root_manifest() -> Result<BundledSourceRootManifest, JsValue> {
    serde_json::from_str(BUNDLED_SOURCE_ROOT_MANIFEST_JSON)
        .map_err(|e| JsValue::from_str(&format!("Invalid bundled source-root manifest: {}", e)))
}

fn load_text_sources_in_session(
    session: &mut Session,
    source_set_id: &str,
    kind: SourceRootKind,
    sources_json: &str,
) -> Result<SourceLoadSummary, JsValue> {
    let cache_root = source_root_semantic_cache_root();
    load_text_sources_in_session_with_cache_root(
        session,
        source_set_id,
        kind,
        sources_json,
        cache_root.as_deref(),
    )
}

fn load_text_sources_in_session_with_cache_root(
    session: &mut Session,
    source_set_id: &str,
    kind: SourceRootKind,
    sources_json: &str,
    cache_root: Option<&Path>,
) -> Result<SourceLoadSummary, JsValue> {
    let sources = parse_text_sources_json(sources_json)?;
    let mut definitions: Vec<(String, StoredDefinition)> = Vec::with_capacity(sources.len());
    let mut skipped_files: Vec<String> = Vec::new();
    let total_sources = sources.len();

    for (index, (filename, source)) in sources.into_iter().enumerate() {
        report_parse_progress(source_set_id, index + 1, total_sources);
        match parse_source_to_ast(&source, &filename) {
            Ok(definition) => definitions.push((filename, definition)),
            Err(error) => skipped_files.push(format!("{filename}: {error}")),
        }
    }

    let parsed_count = definitions.len();
    if parsed_count == 0 && skipped_files.is_empty() {
        return Ok(SourceLoadSummary {
            parsed_count,
            inserted_count: 0,
            error_count: 0,
            skipped_files,
        });
    }
    let inserted_count =
        session.replace_parsed_source_set(source_set_id, kind, definitions, Some("input.mo"));
    if cache_root.is_some() {
        let source_root_path = synthetic_source_root_path(source_set_id);
        let _ = session.sync_source_root_semantic_summary_cache(
            source_set_id,
            &source_root_path,
            cache_root,
        );
    }

    Ok(SourceLoadSummary {
        parsed_count,
        inserted_count,
        error_count: skipped_files.len(),
        skipped_files,
    })
}

#[cfg(target_arch = "wasm32")]
fn source_root_semantic_cache_root() -> Option<PathBuf> {
    None
}

#[cfg(not(target_arch = "wasm32"))]
fn source_root_semantic_cache_root() -> Option<PathBuf> {
    resolve_source_root_cache_dir()
}

fn synthetic_source_root_path(source_set_id: &str) -> PathBuf {
    let sanitized = source_set_id.replace("::", "/");
    let path = Path::new(&sanitized);
    if path.as_os_str().is_empty() {
        PathBuf::from("source-root")
    } else {
        path.to_path_buf()
    }
}

fn load_source_root_sources_in_session(
    session: &mut Session,
    source_roots_json: &str,
) -> Result<SourceLoadSummary, JsValue> {
    load_text_sources_in_session(
        session,
        WASM_BUNDLED_SOURCE_ROOT_SET_ID,
        SourceRootKind::External,
        source_roots_json,
    )
}

pub(crate) fn load_project_sources_in_session(
    session: &mut Session,
    project_sources_json: &str,
) -> Result<SourceLoadSummary, JsValue> {
    let cache_root = source_root_semantic_cache_root();
    let parsed_roots = parse_project_source_roots(project_sources_json)?;
    sync_project_source_roots(session, parsed_roots, cache_root.as_deref())
}

pub fn load_project_sources_for_simulation(
    session: &mut Session,
    project_sources_json: &str,
) -> Result<(), JsValue> {
    load_project_sources_in_session(session, project_sources_json).map(|_| ())
}

#[cfg(test)]
pub(crate) fn sync_project_sources_with_cache_root_for_tests(
    project_sources_json: &str,
    cache_root: &Path,
) -> Result<String, JsValue> {
    super::with_singleton_session(|session| {
        let parsed_roots = parse_project_source_roots(project_sources_json)?;
        let summary = sync_project_source_roots(session, parsed_roots, Some(cache_root))?;
        source_load_summary_json(&summary)
    })
}

fn parse_project_source_roots(
    project_sources_json: &str,
) -> Result<ParsedProjectSourceLoad, JsValue> {
    let sources = parse_text_sources_json(project_sources_json)?;
    let mut parsed_count = 0usize;
    let mut skipped_files = Vec::new();
    let mut definitions = Vec::with_capacity(sources.len());
    let total_sources = sources.len();
    for (index, (filename, source)) in sources.into_iter().enumerate() {
        report_parse_progress(WASM_PROJECT_SOURCE_SET_ID, index + 1, total_sources);
        let normalized_filename = normalize_source_path(&filename);
        match parse_source_to_ast(&source, &normalized_filename) {
            Ok(definition) => {
                definitions.push((normalized_filename, definition));
                parsed_count += 1;
            }
            Err(error) => skipped_files.push(format!("{normalized_filename}: {error}")),
        }
    }

    Ok(ParsedProjectSourceLoad {
        definitions,
        parsed_count,
        skipped_files,
    })
}

fn sync_project_source_roots(
    session: &mut Session,
    parsed_load: ParsedProjectSourceLoad,
    cache_root: Option<&Path>,
) -> Result<SourceLoadSummary, JsValue> {
    let ParsedProjectSourceLoad {
        definitions,
        parsed_count,
        skipped_files,
    } = parsed_load;
    let inserted_count = session.sync_partitioned_source_root_family(
        WASM_PROJECT_SOURCE_SET_ID,
        SourceRootKind::Workspace,
        definitions,
        cache_root,
        Some("input.mo"),
    );

    Ok(SourceLoadSummary {
        parsed_count,
        inserted_count,
        error_count: skipped_files.len(),
        skipped_files,
    })
}

fn normalize_source_path(path: &str) -> String {
    path.replace('\\', "/")
}

fn source_load_summary_json(summary: &SourceLoadSummary) -> Result<String, JsValue> {
    let payload = serde_json::json!({
        "parsed_count": summary.parsed_count,
        "inserted_count": summary.inserted_count,
        "error_count": summary.error_count,
        "skipped_files": summary.skipped_files,
    });
    serde_json::to_string(&payload).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

#[wasm_bindgen]
pub fn compile_with_source_roots(
    source: &str,
    model_name: &str,
    source_roots_json: &str,
) -> Result<String, JsValue> {
    compile_with_source_roots_with_options(source, model_name, source_roots_json, "{}")
}

#[wasm_bindgen]
/// Compile using loaded source roots with explicit compile behavior options.
///
/// `compile_options_json` example:
/// `{"allowNonParamEvaluateAnnotation":true,"allowMultiWhenSingleAssign":false}`
///
/// Strict behavior remains default when this function is not used (or options are false).
pub fn compile_with_source_roots_with_options(
    source: &str,
    model_name: &str,
    source_roots_json: &str,
    compile_options_json: &str,
) -> Result<String, JsValue> {
    let compile_options = parse_compile_behavior_options(compile_options_json)?;
    super::with_singleton_session(|session| {
        let previous_strictness = session.semantic_strictness();
        let next_strictness = semantic_strictness_from_options(compile_options);
        session.set_semantic_strictness(next_strictness.0, next_strictness.1);
        let result = (|| {
            load_source_root_sources_in_session(session, source_roots_json)?;
            compile_source_in_session(session, source, model_name)
        })();
        session.set_semantic_strictness(previous_strictness.0, previous_strictness.1);
        result
    })
}

#[wasm_bindgen]
pub fn compile_check_with_source_roots(
    source: &str,
    model_name: &str,
    source_roots_json: &str,
) -> Result<String, JsValue> {
    compile_check_with_source_roots_with_options(source, model_name, source_roots_json, "{}")
}

#[wasm_bindgen]
/// Strict compile-check using loaded source roots with explicit behavior options.
///
/// This validates requested-model compilation without producing full DAE output.
/// Option semantics are identical to `compile_with_source_roots_with_options`.
pub fn compile_check_with_source_roots_with_options(
    source: &str,
    model_name: &str,
    source_roots_json: &str,
    compile_options_json: &str,
) -> Result<String, JsValue> {
    let compile_options = parse_compile_behavior_options(compile_options_json)?;
    super::with_singleton_session(|session| {
        let total_started = wasm_timing_start();
        let previous_strictness = session.semantic_strictness();
        let next_strictness = semantic_strictness_from_options(compile_options);
        session.set_semantic_strictness(next_strictness.0, next_strictness.1);
        let result = (|| {
            let load_started = wasm_timing_start();
            load_source_root_sources_in_session(session, source_roots_json)?;
            let load_ms = wasm_elapsed_ms(load_started);

            reset_compile_phase_timing_stats();

            let update_started = wasm_timing_start();
            session.update_document("input.mo", source);
            let update_ms = wasm_elapsed_ms(update_started);

            let qualify_started = wasm_timing_start();
            let requested_model = super::qualify_input_model_name(session, model_name);
            let qualify_ms = wasm_elapsed_ms(qualify_started);

            let check_started = wasm_timing_start();
            let strict_timing = session
                .check_model_strict_requested_only_with_timing(&requested_model)
                .map_err(|message| JsValue::from_str(&message))?;
            let check_ms = wasm_elapsed_ms(check_started);
            let total_ms = wasm_elapsed_ms(total_started);

            let timing = compile_phase_timing_stats();
            Ok(serde_json::json!({
                "status": "compiled",
                "model_name": requested_model,
                "__compile_check_timing": {
                    "load_source_roots_ms": load_ms,
                    "update_document_ms": update_ms,
                    "qualify_model_ms": qualify_ms,
                    "check_model_ms": check_ms,
                    "total_ms": total_ms,
                    "strict": {
                        "build_resolved_ms": strict_timing.build_resolved_ms,
                        "reachable_closure_ms": strict_timing.reachable_closure_ms,
                        "collect_parse_failures_ms": strict_timing.collect_parse_failures_ms,
                        "collect_resolve_failures_ms": strict_timing.collect_resolve_failures_ms,
                        "dae_phase_query_ms": strict_timing.dae_phase_query_ms,
                        "total_ms": strict_timing.total_ms,
                    }
                },
                "__compile_phase_timing": {
                    "instantiate": {
                        "calls": timing.instantiate.calls,
                        "total_nanos": timing.instantiate.total_nanos,
                        "total_ms": (timing.instantiate.total_nanos as f64) / 1_000_000.0,
                    },
                    "typecheck": {
                        "calls": timing.typecheck.calls,
                        "total_nanos": timing.typecheck.total_nanos,
                        "total_ms": (timing.typecheck.total_nanos as f64) / 1_000_000.0,
                    },
                    "flatten": {
                        "calls": timing.flatten.calls,
                        "total_nanos": timing.flatten.total_nanos,
                        "total_ms": (timing.flatten.total_nanos as f64) / 1_000_000.0,
                    },
                    "todae": {
                        "calls": timing.todae.calls,
                        "total_nanos": timing.todae.total_nanos,
                        "total_ms": (timing.todae.total_nanos as f64) / 1_000_000.0,
                    },
                }
            })
            .to_string())
        })();
        session.set_semantic_strictness(previous_strictness.0, previous_strictness.1);
        result
    })
}

#[wasm_bindgen]
pub fn compile_with_project_sources(
    source: &str,
    model_name: &str,
    project_sources_json: &str,
) -> Result<String, JsValue> {
    super::with_singleton_session(|session| {
        load_project_sources_in_session(session, project_sources_json)?;
        compile_source_in_session(session, source, model_name)
    })
}

#[wasm_bindgen]
pub fn sync_project_sources(project_sources_json: &str) -> Result<String, JsValue> {
    super::with_singleton_session(|session| {
        let summary = load_project_sources_in_session(session, project_sources_json)?;
        source_load_summary_json(&summary)
    })
}

#[wasm_bindgen]
pub fn get_source_root_statuses() -> Result<String, JsValue> {
    super::with_singleton_session(|session| {
        serde_json::to_string(&session.source_root_statuses())
            .map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
    })
}

#[wasm_bindgen]
pub fn load_source_roots(source_roots_json: &str) -> Result<String, JsValue> {
    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);
    let summary = load_source_root_sources_in_session(session, source_roots_json)?;

    let result = serde_json::json!({
        "parsed_count": summary.parsed_count,
        "inserted_count": summary.inserted_count,
        "error_count": summary.error_count,
        "source_root_names": [],
        "conflicts": [],
        "skipped_files": summary.skipped_files,
    });
    serde_json::to_string(&result).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

#[wasm_bindgen]
pub fn parse_source_root_file(source: &str, filename: &str) -> Result<String, JsValue> {
    let def = parse_source_to_ast(source, filename)
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    serde_json::to_string(&def)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

#[wasm_bindgen]
pub fn merge_parsed_source_roots(definitions_json: &str) -> Result<u32, JsValue> {
    let defs: Vec<(String, String)> = serde_json::from_str(definitions_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid JSON: {}", e)))?;

    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);
    let mut count = 0u32;

    for (filename, ast_json) in defs {
        if let Ok(def) = serde_json::from_str::<StoredDefinition>(&ast_json) {
            session.add_parsed(&filename, def);
            count += 1;
        }
    }

    Ok(count)
}

#[wasm_bindgen]
pub fn merge_parsed_source_roots_binary(bytes: &[u8]) -> Result<u32, JsValue> {
    let definitions = parse_binary_source_root_snapshot(bytes)?;
    let count = definitions.len() as u32;

    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);
    session.add_parsed_batch(definitions);
    Ok(count)
}

#[wasm_bindgen]
pub fn get_bundled_source_root_manifest() -> String {
    BUNDLED_SOURCE_ROOT_MANIFEST_JSON.to_string()
}

#[wasm_bindgen]
pub fn load_bundled_source_root_cache(archive_id: &str) -> Result<u32, JsValue> {
    let manifest = bundled_source_root_manifest()?;
    let Some(_entry) = manifest
        .archives
        .iter()
        .find(|entry| entry.archive_id == archive_id)
    else {
        return Err(JsValue::from_str(&format!(
            "Unknown bundled source-root archive: {}",
            archive_id
        )));
    };

    if BUNDLED_SOURCE_ROOT_CACHE_BYTES.is_empty() {
        return Ok(0);
    }

    merge_parsed_source_roots_binary(BUNDLED_SOURCE_ROOT_CACHE_BYTES)
}

#[wasm_bindgen]
pub fn export_parsed_source_roots_binary(uris_json: &str) -> Result<Vec<u8>, JsValue> {
    let requested_uris: Vec<String> = serde_json::from_str(uris_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid JSON: {}", e)))?;

    let lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let Some(session) = lock.as_ref() else {
        return Ok(Vec::new());
    };

    let mut definitions: Vec<(String, StoredDefinition)> = Vec::new();
    for uri in requested_uris {
        let Some(doc) = session.get_document(&uri) else {
            continue;
        };
        let Some(parsed) = doc.parsed().cloned() else {
            continue;
        };
        definitions.push((uri, parsed));
    }

    bincode::serialize(&definitions)
        .map_err(|e| JsValue::from_str(&format!("Binary cache serialization error: {}", e)))
}

#[wasm_bindgen]
pub fn clear_source_root_cache() {
    if let Ok(mut s) = SESSION.lock() {
        *s = None;
    }
}

#[wasm_bindgen]
pub fn get_source_root_document_count() -> u32 {
    SESSION
        .lock()
        .ok()
        .and_then(|s| s.as_ref().map(|sess| sess.document_uris().len() as u32))
        .unwrap_or(0)
}
