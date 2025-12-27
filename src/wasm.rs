//! WASM bindings for Rumoca.
//!
//! This module provides WebAssembly bindings for using Rumoca in the browser,
//! including full LSP support for diagnostics, hover, completion, etc.
//!
//! ## Usage from JavaScript
//! ```js
//! import init, { wasm_init, compile_to_json, lsp_diagnostics } from './pkg/rumoca.js';
//!
//! await init();
//! await wasm_init(navigator.hardwareConcurrency || 4);
//!
//! // Compile a model
//! const json = compile_to_json("model M Real x; equation der(x) = 1; end M;", "M");
//!
//! // Get LSP diagnostics (errors/warnings)
//! const diagnostics = lsp_diagnostics("model M Real x; equation der(x) = ; end M;");
//! ```
//!
//! ## Important: Must be called from a Web Worker
//!
//! These functions use rayon's thread pool for parallel processing.
//! Since rayon uses `Atomics.wait` for synchronization, they **must be
//! called from a Web Worker**, not from the main browser thread.

use std::str::FromStr;
use std::sync::RwLock;
use wasm_bindgen::prelude::*;
use web_sys::console;
use web_time::Instant;

// Note: enable_cache/disable_cache are available from crate::ir::transform::flatten
// if needed for batch compilation with libraries
use crate::lsp::{
    WorkspaceState, compute_diagnostics, create_documents, get_semantic_token_legend,
    handle_completion_workspace, handle_document_symbols, handle_hover_workspace,
    handle_semantic_tokens,
};
use crate::{Compiler, parse_source, parse_source_simple};
use lsp_types::Uri;

// Persistent workspace state for WASM (maintains document cache, symbol index, library cache, etc.)
// This is the single source of truth - all library and document state lives here.
static WORKSPACE: RwLock<Option<WorkspaceState>> = RwLock::new(None);

fn log(msg: &str) {
    console::log_1(&msg.into());
}

/// Initialize the thread pool for parallel processing.
///
/// Call this once from JavaScript before using other functions.
/// The `num_threads` parameter specifies how many Web Workers to spawn.
///
/// Note: Requires SharedArrayBuffer support, which needs these HTTP headers:
/// - Cross-Origin-Opener-Policy: same-origin
/// - Cross-Origin-Embedder-Policy: require-corp
#[wasm_bindgen]
pub fn wasm_init(num_threads: usize) -> js_sys::Promise {
    // Limit thread count to 8 max to prevent hangs with large workloads
    let num_threads = num_threads.min(8);

    // Enable flatten cache for WASM - helps with repeated compiles
    // The cache uses def_hash as key, so it auto-invalidates when code changes
    crate::ir::transform::flatten::enable_cache();
    log(&format!(
        "[WASM] Flatten cache enabled, initializing thread pool with {} threads",
        num_threads
    ));

    wasm_bindgen_rayon::init_thread_pool(num_threads)
}

/// Get the Rumoca version string.
#[wasm_bindgen]
pub fn get_version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

/// Simple test function to verify worker communication works.
#[wasm_bindgen]
pub fn test_echo(input: &str) -> String {
    format!("Echo: {}", input)
}

/// Test grammar creation without parsing.
#[wasm_bindgen]
pub fn test_grammar() -> String {
    use crate::modelica_grammar::ModelicaGrammar;
    log("[WASM] test_grammar: creating grammar");
    let _grammar = ModelicaGrammar::new();
    log("[WASM] test_grammar: grammar created");
    "Grammar created successfully".to_string()
}

/// Parse Modelica source and return whether it's valid.
///
/// This uses rayon's thread pool for parallel processing.
/// Returns true if parsing succeeded, false otherwise.
///
/// **Note**: Must be called from a Web Worker, not the main thread.
#[wasm_bindgen]
pub fn parse_modelica(source: &str) -> bool {
    log("[WASM] parse_modelica: starting");
    log(&format!(
        "[WASM] parse_modelica: source length = {}",
        source.len()
    ));

    // Use catch_unwind to detect panics
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        log("[WASM] parse_modelica: calling parse_source_simple");
        let parsed = parse_source_simple(source, "<wasm>");
        log("[WASM] parse_modelica: parse_source_simple returned");
        parsed.is_some()
    }));

    match result {
        Ok(success) => {
            log(&format!("[WASM] parse_modelica: result = {}", success));
            success
        }
        Err(e) => {
            log(&format!(
                "[WASM] parse_modelica: PANIC: {:?}",
                e.downcast_ref::<&str>()
            ));
            false
        }
    }
}

/// Compile a Modelica model and return combined result (DAE + balance) as JSON.
///
/// This runs the full compilation pipeline using rayon's thread pool
/// for parallel processing where applicable.
///
/// Returns JSON with { dae: ..., balance: ... } on success, or throws an error.
///
/// **Note**: Must be called from a Web Worker, not the main thread.
#[wasm_bindgen]
pub fn compile_to_json(source: &str, model_name: &str) -> Result<String, JsError> {
    log("[WASM] compile_to_json: starting");
    log(&format!(
        "[WASM] compile_to_json: model = {}, source length = {}",
        model_name,
        source.len()
    ));
    // Log first 200 chars of source to verify correct content is being compiled
    let source_preview: String = source.chars().take(200).collect();
    log(&format!(
        "[WASM] compile_to_json: source preview: {}",
        source_preview
    ));

    log("[WASM] compile_to_json: creating compiler");
    let compiler = Compiler::new().model(model_name).cache(false);

    log("[WASM] compile_to_json: calling compile_str");
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        compiler.compile_str(source, "<wasm>")
    }));

    handle_compile_result(result)
}

/// Load and parse library sources into the cache using parallel parsing.
///
/// Call this once after fetching library files. The sources are parsed in
/// parallel using rayon's thread pool, then merged and indexed.
///
/// Returns the number of successfully parsed library files.
///
/// **Note**: Must be called from a Web Worker, not the main thread.
/// Result from load_libraries containing counts, library names, conflicts, and skipped files
#[derive(serde::Serialize)]
struct LoadLibrariesResult {
    parsed_count: usize,
    error_count: usize,
    library_names: Vec<String>,
    conflicts: Vec<String>,
    skipped_files: Vec<String>,
}

#[wasm_bindgen]
pub fn load_libraries(libraries_json: &str) -> Result<String, JsError> {
    log("[WASM] load_libraries: starting");

    // Parse the libraries JSON
    let libraries: std::collections::HashMap<String, String> = serde_json::from_str(libraries_json)
        .map_err(|e| JsError::new(&format!("Failed to parse libraries JSON: {}", e)))?;

    log(&format!(
        "[WASM] load_libraries: {} files to parse",
        libraries.len()
    ));

    let start = Instant::now();

    // Use WorkspaceState's parallel parsing method
    // This handles parsing, grouping, merging, and indexing - same logic as native LSP
    let mut workspace = WorkspaceState::new();
    let result = workspace.load_library_sources(libraries);

    let elapsed = start.elapsed();

    log(&format!(
        "[WASM] load_libraries: parsed {} files in {:.2}s ({} errors, {} skipped, libraries: {:?})",
        result.parsed_count,
        elapsed.as_secs_f64(),
        result.error_count,
        result.skipped_files.len(),
        result.library_names
    ));

    // Debug: log library contents
    for (lib_name, lib) in workspace.libraries() {
        let class_names: Vec<_> = lib.class_list.keys().take(10).collect();
        log(&format!(
            "[WASM] Library '{}' has {} classes, first 10: {:?}",
            lib_name,
            lib.class_list.len(),
            class_names
        ));
    }

    // Store workspace for LSP functions
    let mut ws_lock = WORKSPACE
        .write()
        .map_err(|e| JsError::new(&format!("Failed to acquire workspace lock: {}", e)))?;
    *ws_lock = Some(workspace);

    // Return JSON with all result info
    let json_result = LoadLibrariesResult {
        parsed_count: result.parsed_count,
        error_count: result.error_count,
        library_names: result.library_names,
        conflicts: result.conflicts,
        skipped_files: result.skipped_files,
    };
    serde_json::to_string(&json_result)
        .map_err(|e| JsError::new(&format!("Serialization error: {}", e)))
}

/// Parse a single library file and return serialized AST.
///
/// This is designed for JavaScript-based parallel parsing where multiple
/// Web Workers each parse a subset of files, then results are merged.
///
/// Returns JSON-serialized StoredDefinition on success.
#[wasm_bindgen]
pub fn parse_library_file(source: &str, filename: &str) -> Result<String, JsError> {
    match parse_source(source, filename) {
        Ok(def) => serde_json::to_string(&def)
            .map_err(|e| JsError::new(&format!("Serialization error: {}", e))),
        Err(e) => Err(JsError::new(&format!("Parse error: {}", e))),
    }
}

/// Merge pre-parsed library definitions into the workspace.
///
/// Takes a JSON array of [filename, ast_json] pairs where ast_json is the
/// output from parse_library_file. This allows JavaScript to parse files
/// in parallel using multiple Web Workers, then merge the results here.
///
/// Returns the number of successfully merged library packages.
#[wasm_bindgen]
pub fn merge_parsed_libraries(definitions_json: &str) -> Result<u32, JsError> {
    log("[WASM] merge_parsed_libraries: starting");

    // Parse the definitions array: [[filename, ast_json], ...]
    let definitions_array: Vec<(String, String)> = serde_json::from_str(definitions_json)
        .map_err(|e| JsError::new(&format!("Failed to parse definitions JSON: {}", e)))?;

    log(&format!(
        "[WASM] merge_parsed_libraries: {} definitions to merge",
        definitions_array.len()
    ));

    let start = Instant::now();

    // Deserialize each AST
    let mut parsed: Vec<(String, crate::ir::ast::StoredDefinition)> = Vec::new();
    let mut errors = 0;

    for (filename, ast_json) in definitions_array {
        match serde_json::from_str(&ast_json) {
            Ok(def) => parsed.push((filename, def)),
            Err(e) => {
                log(&format!(
                    "[WASM] merge_parsed_libraries: failed to deserialize {}: {}",
                    filename, e
                ));
                errors += 1;
            }
        }
    }

    log(&format!(
        "[WASM] merge_parsed_libraries: deserialized {} definitions ({} errors)",
        parsed.len(),
        errors
    ));

    // Use workspace to merge and index
    let mut workspace = WorkspaceState::new();
    let (library_names, conflicts) = workspace.load_library_definitions(parsed);

    let elapsed = start.elapsed();

    log(&format!(
        "[WASM] merge_parsed_libraries: merged {} libraries ({:?}) in {:.2}s{}",
        library_names.len(),
        library_names,
        elapsed.as_secs_f64(),
        if conflicts.is_empty() {
            String::new()
        } else {
            format!(" (conflicts: {:?})", conflicts)
        }
    ));

    // Store workspace for LSP functions
    let mut ws_lock = WORKSPACE
        .write()
        .map_err(|e| JsError::new(&format!("Failed to acquire workspace lock: {}", e)))?;
    *ws_lock = Some(workspace);

    Ok(library_names.len() as u32)
}

/// Clear the library cache and workspace.
///
/// Call this when you want to load a different set of libraries.
#[wasm_bindgen]
pub fn clear_library_cache() {
    log("[WASM] clear_library_cache: clearing");
    if let Ok(mut ws) = WORKSPACE.write() {
        *ws = None;
    }
}

/// Get the number of library packages currently cached in the workspace.
#[wasm_bindgen]
pub fn get_library_count() -> u32 {
    WORKSPACE
        .read()
        .ok()
        .and_then(|ws| ws.as_ref().map(|w| w.library_count() as u32))
        .unwrap_or(0)
}

/// Compile a Modelica model using cached libraries.
///
/// Uses the pre-parsed library cache from `load_libraries`. If no libraries
/// are cached, compiles without libraries.
///
/// **Note**: Must be called from a Web Worker, not the main thread.
#[wasm_bindgen]
pub fn compile_with_libraries(
    source: &str,
    model_name: &str,
    libraries_json: &str, // Used as fallback when no pre-parsed cache is available
) -> Result<String, JsError> {
    log("[WASM] compile_with_libraries: starting");

    // Check if we have cached libraries
    let cache_count = get_library_count();

    if cache_count > 0 {
        log(&format!(
            "[WASM] compile_with_libraries: using {} cached libraries",
            cache_count
        ));
        compile_with_cached_libraries(source, model_name)
    } else {
        log("[WASM] compile_with_libraries: no cache, parsing libraries");
        compile_with_libraries_uncached(source, model_name, libraries_json)
    }
}

/// Compile using the workspace's pre-built library dictionaries.
///
/// This delegates to the LSP workspace's compile method, which uses pre-built
/// ClassDicts for efficient compilation without re-parsing or re-merging libraries.
fn compile_with_cached_libraries(source: &str, model_name: &str) -> Result<String, JsError> {
    // Log first 200 chars of source to verify correct content is being compiled
    let source_preview: String = source.chars().take(200).collect();
    log(&format!(
        "[WASM] compile_with_cached_libraries: source preview: {}",
        source_preview
    ));

    // Parse the main source
    let main_def = parse_source(source, "<wasm>").map_err(|e| JsError::new(&e.to_string()))?;

    // Get workspace for compilation
    let ws_lock = WORKSPACE
        .read()
        .map_err(|e| JsError::new(&format!("Failed to acquire workspace lock: {}", e)))?;

    let workspace = ws_lock
        .as_ref()
        .ok_or_else(|| JsError::new("Workspace not initialized"))?;

    log(&format!(
        "[WASM] compile_with_cached_libraries: using workspace with {} libraries",
        workspace.library_count()
    ));

    // Use catch_unwind to capture panics
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        workspace.compile(&main_def, model_name)
    }));

    handle_compile_result(result)
}

/// Compile without using cache (fallback).
fn compile_with_libraries_uncached(
    source: &str,
    model_name: &str,
    libraries_json: &str,
) -> Result<String, JsError> {
    log(&format!(
        "[WASM] compile_with_libraries_uncached: model = {}, source length = {}, libraries length = {}",
        model_name,
        source.len(),
        libraries_json.len()
    ));

    // Parse the libraries JSON
    let libraries: std::collections::HashMap<String, String> = serde_json::from_str(libraries_json)
        .map_err(|e| JsError::new(&format!("Failed to parse libraries JSON: {}", e)))?;

    log(&format!(
        "[WASM] compile_with_libraries_uncached: {} library files",
        libraries.len()
    ));

    // Convert to the format expected by the compiler
    let lib_vec: Vec<(&str, &str)> = libraries
        .iter()
        .map(|(name, src)| (name.as_str(), src.as_str()))
        .collect();

    let compiler = Compiler::new().model(model_name).cache(false);

    // Use catch_unwind to capture panics
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        compiler.compile_str_with_sources(source, "<wasm>", lib_vec)
    }));

    handle_compile_result(result)
}

/// Combined compilation result for WASM (includes DAE, balance, and pretty-printed text)
#[derive(serde::Serialize)]
struct WasmCompileResult {
    /// DAE in DaeIR format (with ir_version, base_modelica_version, etc.)
    dae: serde_json::Value,
    /// DAE in native format (for template rendering - has rumoca_version, etc.)
    dae_native: serde_json::Value,
    balance: crate::dae::balance::BalanceResult,
    /// Human-readable pretty-printed DAE representation
    pretty: String,
}

/// Handle the result of a compilation, converting panics to errors.
fn handle_compile_result(
    result: std::result::Result<
        anyhow::Result<crate::CompilationResult>,
        Box<dyn std::any::Any + Send>,
    >,
) -> Result<String, JsError> {
    match result {
        Ok(Ok(r)) => {
            log("[WASM] compile: succeeded, getting JSON");
            match r.to_dae_ir_json() {
                Ok(dae_json) => {
                    // Parse DAE IR JSON to include in combined result
                    let dae: serde_json::Value = serde_json::from_str(&dae_json)
                        .map_err(|e| JsError::new(&format!("DAE JSON parse error: {}", e)))?;

                    // Serialize native DAE for template rendering
                    let dae_native: serde_json::Value = serde_json::to_value(&r.dae)
                        .map_err(|e| JsError::new(&format!("DAE native JSON error: {}", e)))?;

                    // Generate pretty-printed text from the DAE
                    let pretty = r.dae.to_pretty_string();

                    let combined = WasmCompileResult {
                        dae,
                        dae_native,
                        balance: r.balance,
                        pretty,
                    };

                    let json = serde_json::to_string(&combined)
                        .map_err(|e| JsError::new(&format!("JSON serialize error: {}", e)))?;

                    log(&format!("[WASM] compile: JSON length = {}", json.len()));
                    Ok(json)
                }
                Err(e) => {
                    log(&format!("[WASM] compile: JSON error: {}", e));
                    Err(JsError::new(&e.to_string()))
                }
            }
        }
        Ok(Err(e)) => {
            log(&format!("[WASM] compile: error: {}", e));
            Err(JsError::new(&e.to_string()))
        }
        Err(panic_info) => {
            let panic_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                s.clone()
            } else {
                "Unknown panic (likely stack overflow with large libraries)".to_string()
            };
            log(&format!("[WASM] compile: PANIC: {}", panic_msg));
            Err(JsError::new(&format!(
                "Compilation panicked: {}",
                panic_msg
            )))
        }
    }
}

// =============================================================================
// LSP Functions
// =============================================================================

/// Compute diagnostics (errors, warnings) for Modelica source code.
///
/// Returns a JSON array of LSP Diagnostic objects with:
/// - range: { start: {line, character}, end: {line, character} }
/// - severity: 1=Error, 2=Warning, 3=Information, 4=Hint
/// - message: The diagnostic message
/// - source: "rumoca"
///
/// This uses the same diagnostic logic as the native LSP server.
#[wasm_bindgen]
pub fn lsp_diagnostics(source: &str) -> Result<String, JsError> {
    let start = Instant::now();
    log("[WASM] lsp_diagnostics: starting");

    // Create a URI for the document
    let uri = Uri::from_str("file:///wasm/model.mo")
        .map_err(|e| JsError::new(&format!("Invalid URI: {}", e)))?;

    // Use persistent workspace if available (has library cache), otherwise create new
    let mut ws_lock = WORKSPACE
        .write()
        .map_err(|e| JsError::new(&format!("Failed to acquire workspace lock: {}", e)))?;

    let workspace = ws_lock.get_or_insert_with(WorkspaceState::new);

    // Update document in workspace
    workspace.add_document(uri.clone(), source.to_string());

    // Debug: log library state
    let lib_names: Vec<_> = workspace
        .libraries()
        .map(|(name, _)| name.as_str())
        .collect();
    log(&format!(
        "[WASM] lsp_diagnostics: setup took {:?}, has {} libraries: {:?}",
        start.elapsed(),
        workspace.library_count(),
        lib_names
    ));

    // Compute diagnostics using the existing LSP logic
    let diag_start = Instant::now();
    let diagnostics = compute_diagnostics(&uri, source, workspace);

    log(&format!(
        "[WASM] lsp_diagnostics: found {} diagnostics in {:?} (total {:?})",
        diagnostics.len(),
        diag_start.elapsed(),
        start.elapsed()
    ));

    // Serialize to JSON
    let json = serde_json::to_string(&diagnostics)
        .map_err(|e| JsError::new(&format!("JSON serialization error: {}", e)))?;

    Ok(json)
}

/// Get hover information for a position in the source code.
///
/// Returns a JSON object with markdown content, or null if no hover info available.
/// Uses the persistent workspace to resolve types from libraries.
#[wasm_bindgen]
pub fn lsp_hover(source: &str, line: u32, character: u32) -> Result<String, JsError> {
    use lsp_types::{HoverParams, Position, TextDocumentIdentifier, TextDocumentPositionParams};

    log(&format!(
        "[WASM] lsp_hover: line={}, char={}",
        line, character
    ));

    let uri = Uri::from_str("file:///wasm/model.mo")
        .map_err(|e| JsError::new(&format!("Invalid URI: {}", e)))?;

    // Create hover params
    let params = HoverParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position { line, character },
        },
        work_done_progress_params: Default::default(),
    };

    // Use persistent workspace if available, otherwise create an empty one
    let mut ws_lock = WORKSPACE
        .write()
        .map_err(|e| JsError::new(&format!("Failed to acquire workspace lock: {}", e)))?;

    let workspace = ws_lock.get_or_insert_with(WorkspaceState::new);

    // Update document in workspace
    workspace.add_document(uri, source.to_string());

    // Get hover info with workspace support
    let hover = handle_hover_workspace(workspace, params);

    // Serialize to JSON
    let json = serde_json::to_string(&hover)
        .map_err(|e| JsError::new(&format!("JSON serialization error: {}", e)))?;

    Ok(json)
}

/// Get code completion suggestions for a position in the source code.
///
/// Returns a JSON array of CompletionItem objects.
/// Uses the persistent workspace for type lookups and document caching.
#[wasm_bindgen]
pub fn lsp_completion(source: &str, line: u32, character: u32) -> Result<String, JsError> {
    use lsp_types::{
        CompletionParams, Position, TextDocumentIdentifier, TextDocumentPositionParams,
    };

    log(&format!(
        "[WASM] lsp_completion: line={}, char={}",
        line, character
    ));

    let uri = Uri::from_str("file:///wasm/model.mo")
        .map_err(|e| JsError::new(&format!("Invalid URI: {}", e)))?;

    // Create completion params
    let params = CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position { line, character },
        },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
        context: None,
    };

    // Use persistent workspace if available, otherwise create an empty one
    let mut ws_lock = WORKSPACE
        .write()
        .map_err(|e| JsError::new(&format!("Failed to acquire workspace lock: {}", e)))?;

    let workspace = ws_lock.get_or_insert_with(WorkspaceState::new);

    // Update document in workspace (this handles caching internally via reparse_document)
    workspace.add_document(uri.clone(), source.to_string());

    log(&format!(
        "[WASM] lsp_completion: workspace has cached_ast={}",
        workspace.get_cached_ast(&uri).is_some()
    ));

    // Get completions with workspace support
    let completions = handle_completion_workspace(workspace, params);

    // Serialize to JSON
    let json = serde_json::to_string(&completions)
        .map_err(|e| JsError::new(&format!("JSON serialization error: {}", e)))?;

    Ok(json)
}

/// Get document symbols (outline) for the source code.
///
/// Returns a JSON array of DocumentSymbol objects with name, kind, range, children.
#[wasm_bindgen]
pub fn lsp_document_symbols(source: &str) -> Result<String, JsError> {
    use lsp_types::{DocumentSymbolParams, TextDocumentIdentifier};

    log("[WASM] lsp_document_symbols: starting");

    let uri = Uri::from_str("file:///wasm/model.mo")
        .map_err(|e| JsError::new(&format!("Invalid URI: {}", e)))?;

    // Create document map
    let documents = create_documents(&uri, source);

    // Create params
    let params = DocumentSymbolParams {
        text_document: TextDocumentIdentifier { uri },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    // Get symbols
    let symbols = handle_document_symbols(&documents, params);

    // Serialize to JSON
    let json = serde_json::to_string(&symbols)
        .map_err(|e| JsError::new(&format!("JSON serialization error: {}", e)))?;

    Ok(json)
}

/// Get semantic tokens for syntax highlighting.
///
/// Returns a JSON object with the LSP SemanticTokens format.
#[wasm_bindgen]
pub fn lsp_semantic_tokens(source: &str) -> Result<String, JsError> {
    use lsp_types::{SemanticTokensParams, TextDocumentIdentifier};

    log("[WASM] lsp_semantic_tokens: starting");

    let uri = Uri::from_str("file:///wasm/model.mo")
        .map_err(|e| JsError::new(&format!("Invalid URI: {}", e)))?;

    // Create document map
    let documents = create_documents(&uri, source);

    // Create params
    let params = SemanticTokensParams {
        text_document: TextDocumentIdentifier { uri },
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    };

    // Get tokens
    let tokens = handle_semantic_tokens(&documents, params);

    // Serialize to JSON
    let json = serde_json::to_string(&tokens)
        .map_err(|e| JsError::new(&format!("JSON serialization error: {}", e)))?;

    Ok(json)
}

/// Get the semantic token legend (token types and modifiers).
///
/// Returns a JSON object with tokenTypes and tokenModifiers arrays.
#[wasm_bindgen]
pub fn lsp_semantic_token_legend() -> Result<String, JsError> {
    let legend = get_semantic_token_legend();

    let json = serde_json::to_string(&legend)
        .map_err(|e| JsError::new(&format!("JSON serialization error: {}", e)))?;

    Ok(json)
}

// =============================================================================
// Template Rendering
// =============================================================================

/// Render a Jinja template with the DAE data.
///
/// Takes the DAE JSON (from compile_to_json) and a Jinja template string.
/// Returns the rendered template output.
///
/// The template has access to `dae` variable containing all DAE fields:
/// - dae.model_name, dae.rumoca_version
/// - dae.x (states), dae.y (algebraics), dae.p (parameters), etc.
/// - dae.fx (continuous equations), dae.fz (algebraic equations), etc.
///
/// Example template:
/// ```jinja
/// Model: {{ dae.model_name }}
/// States: {% for name, comp in dae.x %}{{ name }}{% if not loop.last %}, {% endif %}{% endfor %}
/// ```
#[wasm_bindgen]
pub fn render_template(dae_json: &str, template: &str) -> Result<String, JsError> {
    log("[WASM] render_template: starting");

    // Deserialize the DAE JSON
    let dae: crate::dae::ast::Dae = serde_json::from_str(dae_json)
        .map_err(|e| JsError::new(&format!("Failed to parse DAE JSON: {}", e)))?;

    log(&format!(
        "[WASM] render_template: DAE model_name = {}",
        dae.model_name
    ));

    // Render the template
    let result = crate::dae::jinja::render_template_str(&dae, template)
        .map_err(|e| JsError::new(&format!("Template error: {}", e)))?;

    log(&format!(
        "[WASM] render_template: output length = {}",
        result.len()
    ));

    Ok(result)
}
