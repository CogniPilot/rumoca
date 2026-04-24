use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use wasm_bindgen::prelude::*;

use rumoca_session::Session;
use rumoca_session::compile::SourceRootKind;
use rumoca_session::parsing::{StoredDefinition, parse_source_to_ast};
#[cfg(not(target_arch = "wasm32"))]
use rumoca_session::source_roots::resolve_source_root_cache_dir;

use super::{
    BUNDLED_SOURCE_ROOT_CACHE_BYTES, BUNDLED_SOURCE_ROOT_MANIFEST_JSON, BundledSourceRootManifest,
    SESSION, WASM_BUNDLED_SOURCE_ROOT_SET_ID, WASM_PROJECT_SOURCE_SET_ID,
    compile_source_in_session,
};

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

    for (filename, source) in sources {
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
    for (filename, source) in sources {
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
    super::with_singleton_session(|session| {
        load_source_root_sources_in_session(session, source_roots_json)?;
        compile_source_in_session(session, source, model_name)
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
