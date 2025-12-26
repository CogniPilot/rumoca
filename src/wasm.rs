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

use crate::ir::ast::StoredDefinition;
// Note: enable_cache/disable_cache are available from crate::ir::transform::flatten
// if needed for batch compilation with libraries
use crate::lsp::{
    WorkspaceState, compute_diagnostics, create_documents, get_semantic_token_legend,
    handle_completion_workspace, handle_document_symbols, handle_hover_workspace,
    handle_semantic_tokens,
};
use crate::{Compiler, parse_source, parse_source_simple};
use lsp_types::Uri;

// Global cache for parsed library definitions
static LIBRARY_CACHE: RwLock<Option<Vec<(String, StoredDefinition)>>> = RwLock::new(None);

// Persistent workspace state for WASM (maintains document cache, symbol index, etc.)
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

/// Load and parse library sources into the cache.
///
/// Call this once after fetching library files. The parsed ASTs are cached
/// and reused for subsequent compile calls, avoiding re-parsing on every compile.
///
/// Returns the number of successfully parsed library files.
///
/// **Note**: Must be called from a Web Worker, not the main thread.
#[wasm_bindgen]
pub fn load_libraries(libraries_json: &str) -> Result<u32, JsError> {
    log("[WASM] load_libraries: starting");

    // Parse the libraries JSON
    let libraries: std::collections::HashMap<String, String> = serde_json::from_str(libraries_json)
        .map_err(|e| JsError::new(&format!("Failed to parse libraries JSON: {}", e)))?;

    log(&format!(
        "[WASM] load_libraries: parsing {} library files...",
        libraries.len()
    ));

    let start = web_time::Instant::now();

    // Parse all library sources
    let mut parsed: Vec<(String, StoredDefinition)> = Vec::with_capacity(libraries.len());
    let mut errors: Vec<String> = Vec::new();

    for (lib_name, lib_source) in &libraries {
        match parse_source(lib_source, lib_name) {
            Ok(def) => {
                parsed.push((lib_name.clone(), def));
            }
            Err(e) => {
                errors.push(format!("{}: {}", lib_name, e));
            }
        }
    }

    let elapsed = start.elapsed();
    let count = parsed.len() as u32;

    log(&format!(
        "[WASM] load_libraries: parsed {} files in {:.2}s ({} errors)",
        count,
        elapsed.as_secs_f64(),
        errors.len()
    ));

    if !errors.is_empty() && errors.len() <= 5 {
        for err in &errors {
            log(&format!("[WASM] load_libraries error: {}", err));
        }
    }

    // Store in cache
    let mut cache = LIBRARY_CACHE
        .write()
        .map_err(|e| JsError::new(&format!("Failed to acquire cache lock: {}", e)))?;
    *cache = Some(parsed.clone());

    // Also initialize the persistent workspace with these libraries
    let library_refs: Vec<&StoredDefinition> = parsed.iter().map(|(_, ast)| ast).collect();
    let workspace = WorkspaceState::from_library_asts(&library_refs);

    let mut ws_lock = WORKSPACE
        .write()
        .map_err(|e| JsError::new(&format!("Failed to acquire workspace lock: {}", e)))?;
    *ws_lock = Some(workspace);

    log(&format!(
        "[WASM] load_libraries: workspace initialized with {} libraries",
        count
    ));

    Ok(count)
}

/// Clear the library cache and workspace.
///
/// Call this when you want to load a different set of libraries.
#[wasm_bindgen]
pub fn clear_library_cache() {
    log("[WASM] clear_library_cache: clearing");
    if let Ok(mut cache) = LIBRARY_CACHE.write() {
        *cache = None;
    }
    if let Ok(mut ws) = WORKSPACE.write() {
        *ws = None;
    }
}

/// Get the number of libraries currently cached.
#[wasm_bindgen]
pub fn get_library_count() -> u32 {
    LIBRARY_CACHE
        .read()
        .ok()
        .and_then(|cache| cache.as_ref().map(|v| v.len() as u32))
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

/// Compile using the cached library definitions.
fn compile_with_cached_libraries(source: &str, model_name: &str) -> Result<String, JsError> {
    // Log first 200 chars of source to verify correct content is being compiled
    let source_preview: String = source.chars().take(200).collect();
    log(&format!(
        "[WASM] compile_with_cached_libraries: source preview: {}",
        source_preview
    ));

    let compiler = Compiler::new().model(model_name).cache(false);

    // Parse the main source
    let main_def = parse_source(source, "<wasm>").map_err(|e| JsError::new(&e.to_string()))?;

    // Get cached libraries and clone them
    let cache = LIBRARY_CACHE
        .read()
        .map_err(|e| JsError::new(&format!("Failed to acquire cache lock: {}", e)))?;

    let mut all_definitions: Vec<(String, StoredDefinition)> =
        cache.as_ref().map(|libs| libs.clone()).unwrap_or_default();

    // Add main source last (so its classes take precedence)
    all_definitions.push(("<wasm>".to_string(), main_def));

    log(&format!(
        "[WASM] compile_with_cached_libraries: {} total definitions",
        all_definitions.len()
    ));

    // Use catch_unwind to capture panics
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        compiler.compile_definitions(all_definitions, source, "<wasm>")
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
    log("[WASM] lsp_diagnostics: starting");

    // Create a minimal workspace state for single-document analysis
    let mut workspace = WorkspaceState::new();

    // Create a URI for the document
    let uri = Uri::from_str("file:///wasm/model.mo")
        .map_err(|e| JsError::new(&format!("Invalid URI: {}", e)))?;

    // Compute diagnostics using the existing LSP logic
    let diagnostics = compute_diagnostics(&uri, source, &mut workspace);

    log(&format!(
        "[WASM] lsp_diagnostics: found {} diagnostics",
        diagnostics.len()
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
