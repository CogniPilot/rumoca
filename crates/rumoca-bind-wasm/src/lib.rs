//! WebAssembly bindings for Rumoca.
//!
//! Thin layer over `rumoca-session` and `rumoca-tool-lsp`. All heavy logic
//! lives in those crates; this module only provides WASM entry points.

mod class_browser_helpers;

use std::{collections::BTreeMap, sync::Mutex};

use lsp_types::{Diagnostic as LspDiagnostic, Position, Range, Url};
use wasm_bindgen::prelude::*;

use rumoca_session::Session;
use rumoca_session::compile::{CompilationMode, CompilationResult, FailedPhase, PhaseResult};
use rumoca_session::parsing::{
    Causality, ClassDef, Expression, OpBinary, StoredDefinition, Variability, parse_source_to_ast,
    validate_source_syntax,
};
use rumoca_session::runtime::{
    SimOptions, dae_balance, prepare_dae_for_template_codegen, render_dae_template_with_json,
    simulate_dae,
};
use rumoca_tool_lint::{LintOptions, lint as lint_source};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use crate::class_browser_helpers::{
    class_type_label, component_reference_to_path, expression_path, extract_string_literal,
    join_path, token_list_to_text,
};

/// Global compilation session containing both library and user documents.
static SESSION: Mutex<Option<Session>> = Mutex::new(None);
const WASM_LIBRARY_SOURCE_SET_ID: &str = "wasm::libraries";

// ==========================================================================
// Initialization
// ==========================================================================

/// Initialize panic hook for better error messages in console.
#[wasm_bindgen(start)]
pub fn init() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Initialize the thread pool (no-op, kept for worker API compatibility).
#[wasm_bindgen]
pub fn wasm_init(_num_threads: usize) {}

/// Get the Rumoca version string.
#[wasm_bindgen]
pub fn get_version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

/// Get the Git commit hash for this WASM build.
#[wasm_bindgen]
pub fn get_git_commit() -> String {
    option_env!("RUMOCA_GIT_COMMIT")
        .unwrap_or("unknown")
        .to_string()
}

/// Get the UTC build timestamp for this WASM build.
#[wasm_bindgen]
pub fn get_build_time_utc() -> String {
    option_env!("RUMOCA_BUILD_TIME_UTC")
        .unwrap_or("unknown")
        .to_string()
}

// ==========================================================================
// Parsing & Checking
// ==========================================================================

#[derive(Serialize, Deserialize)]
struct ParseResult {
    success: bool,
    error: Option<String>,
}

#[derive(Serialize)]
struct WasmClassTreeNode {
    name: String,
    qualified_name: String,
    class_type: String,
    partial: bool,
    children: Vec<WasmClassTreeNode>,
}

#[derive(Serialize)]
struct WasmClassTreeResponse {
    total_classes: usize,
    classes: Vec<WasmClassTreeNode>,
}

#[derive(Serialize)]
struct WasmClassComponentInfo {
    name: String,
    type_name: String,
    variability: String,
    causality: String,
    description: Option<String>,
}

#[derive(Serialize)]
struct WasmClassInfo {
    qualified_name: String,
    class_type: String,
    partial: bool,
    encapsulated: bool,
    description: Option<String>,
    documentation_html: Option<String>,
    documentation_revisions_html: Option<String>,
    component_count: usize,
    equation_count: usize,
    algorithm_count: usize,
    nested_class_count: usize,
    source_modelica: String,
    components: Vec<WasmClassComponentInfo>,
}

/// Parse Modelica source code and return whether it's valid.
#[wasm_bindgen]
pub fn parse(source: &str) -> JsValue {
    let result = match validate_source_syntax(source, "input.mo") {
        Ok(()) => ParseResult {
            success: true,
            error: None,
        },
        Err(e) => ParseResult {
            success: false,
            error: Some(e.to_string()),
        },
    };
    serde_wasm_bindgen::to_value(&result).unwrap_or(JsValue::NULL)
}

#[derive(Serialize, Deserialize)]
struct WasmLintMessage {
    rule: String,
    level: String,
    message: String,
    line: u32,
    column: u32,
    suggestion: Option<String>,
}

/// Lint Modelica source code and return messages.
#[wasm_bindgen]
pub fn lint(source: &str) -> JsValue {
    let options = LintOptions::default();
    let messages = lint_source(source, "input.mo", &options);
    let wasm_messages: Vec<WasmLintMessage> = messages
        .into_iter()
        .map(|m| WasmLintMessage {
            rule: m.rule.to_string(),
            level: m.level.to_string(),
            message: m.message,
            line: m.line,
            column: m.column,
            suggestion: m.suggestion,
        })
        .collect();
    serde_wasm_bindgen::to_value(&wasm_messages).unwrap_or(JsValue::NULL)
}

/// Check Modelica source code and return all diagnostics.
#[wasm_bindgen]
pub fn check(source: &str) -> JsValue {
    if let Err(e) = validate_source_syntax(source, "input.mo") {
        let error = WasmLintMessage {
            rule: "syntax-error".to_string(),
            level: "error".to_string(),
            message: e.to_string(),
            line: 1,
            column: 1,
            suggestion: None,
        };
        return serde_wasm_bindgen::to_value(&vec![error]).unwrap_or(JsValue::NULL);
    }
    lint(source)
}

// ==========================================================================
// Compilation
// ==========================================================================

fn as_object(value: &Value) -> Option<&Map<String, Value>> {
    value.as_object()
}

fn as_object_mut(value: &mut Value) -> Option<&mut Map<String, Value>> {
    value.as_object_mut()
}

fn expr_var_name(expr: &Value) -> Option<String> {
    let obj = as_object(expr)?;
    if let Some(var_ref) = obj.get("VarRef").and_then(Value::as_object) {
        return var_ref
            .get("name")
            .and_then(Value::as_str)
            .map(ToString::to_string);
    }
    obj.get("name")
        .and_then(Value::as_str)
        .map(ToString::to_string)
}

fn is_binary_sub(op: &Value) -> bool {
    if let Some(text) = op.as_str() {
        return text == "-" || text.eq_ignore_ascii_case("sub");
    }
    let Some(obj) = op.as_object() else {
        return false;
    };
    if obj.contains_key("Sub") {
        return true;
    }
    obj.get("token")
        .and_then(Value::as_object)
        .and_then(|tok| tok.get("text"))
        .and_then(Value::as_str)
        .is_some_and(|text| text == "-")
}

fn extract_residual_assignment_expr(residual_expr: &Value, target: &str) -> Option<Value> {
    let binary = residual_expr
        .as_object()
        .and_then(|obj| obj.get("Binary"))
        .and_then(Value::as_object)?;
    if !binary.get("op").is_some_and(is_binary_sub) {
        return None;
    }
    let lhs = binary.get("lhs")?;
    let rhs = binary.get("rhs")?;
    let lhs_name = expr_var_name(lhs);
    let rhs_name = expr_var_name(rhs);
    if lhs_name.as_deref() == Some(target) && rhs_name.as_deref() != Some(target) {
        return Some(rhs.clone());
    }
    if rhs_name.as_deref() == Some(target) && lhs_name.as_deref() != Some(target) {
        return Some(lhs.clone());
    }
    None
}

fn lhs_var_name(lhs: &Value) -> Option<String> {
    if let Some(s) = lhs.as_str() {
        return Some(s.to_string());
    }
    expr_var_name(lhs)
}

fn collect_observable_expr_candidates_from_native(native: &Value, target: &str) -> Vec<Value> {
    let Some(obj) = native.as_object() else {
        return Vec::new();
    };
    let mut out: Vec<Value> = Vec::new();

    for key in ["f_z", "f_m", "f_c"] {
        if let Some(rows) = obj.get(key).and_then(Value::as_array) {
            out.extend(
                rows.iter()
                    .filter_map(Value::as_object)
                    .filter(|row_obj| {
                        row_obj
                            .get("lhs")
                            .and_then(lhs_var_name)
                            .is_some_and(|name| name == target)
                    })
                    .filter_map(|row_obj| row_obj.get("rhs").cloned()),
            );
        }
    }

    for key in ["f_x", "fx"] {
        if let Some(rows) = obj.get(key).and_then(Value::as_array) {
            out.extend(
                rows.iter()
                    .filter_map(Value::as_object)
                    .filter_map(|row_obj| {
                        row_obj
                            .get("rhs")
                            .or_else(|| row_obj.get("residual"))
                            .and_then(|expr| extract_residual_assignment_expr(expr, target))
                    }),
            );
        }
    }

    out
}

fn expr_complexity(expr: &Value) -> usize {
    match expr {
        Value::Object(map) => {
            1 + map
                .values()
                .map(expr_complexity)
                .fold(0usize, |acc, n| acc.saturating_add(n))
        }
        Value::Array(items) => {
            1 + items
                .iter()
                .map(expr_complexity)
                .fold(0usize, |acc, n| acc.saturating_add(n))
        }
        _ => 1,
    }
}

fn is_simple_alias_expr(expr: &Value) -> bool {
    let is_var_like = |v: &Value| {
        v.as_object()
            .is_some_and(|m| m.contains_key("VarRef") || m.contains_key("ComponentReference"))
    };
    if is_var_like(expr) {
        return true;
    }
    let Some(obj) = expr.as_object() else {
        return false;
    };
    let Some(unary) = obj.get("Unary").and_then(Value::as_object) else {
        return false;
    };
    unary.get("arg").is_some_and(is_var_like)
}

fn find_observable_expr_from_native(native: &Value, target: &str) -> Option<Value> {
    let candidates = collect_observable_expr_candidates_from_native(native, target);
    if candidates.is_empty() {
        return None;
    }
    candidates.into_iter().max_by_key(|expr| {
        let non_alias = if is_simple_alias_expr(expr) {
            0usize
        } else {
            1usize
        };
        (non_alias, expr_complexity(expr))
    })
}

fn component_ref_name(expr: &Value) -> Option<String> {
    let obj = expr.as_object()?;
    let cr = obj.get("ComponentReference")?.as_object()?;
    let parts = cr.get("parts")?.as_array()?;
    let mut segs: Vec<String> = Vec::new();
    for part in parts {
        let part_obj = part.as_object()?;
        let ident = part_obj.get("ident")?.as_object()?;
        let text = ident.get("text")?.as_str()?;
        segs.push(text.to_string());
    }
    if segs.is_empty() {
        return None;
    }
    Some(segs.join("."))
}

fn collect_prepared_symbol_names(
    prepared_obj: &Map<String, Value>,
) -> std::collections::HashSet<String> {
    let mut out = std::collections::HashSet::new();
    for key in [
        "p",
        "constants",
        "cp",
        "x",
        "y",
        "z",
        "m",
        "w",
        "u",
        "x_dot_alias",
    ] {
        if let Some(map) = prepared_obj.get(key).and_then(Value::as_object) {
            out.extend(map.keys().cloned());
        }
    }
    out
}

fn rewrite_observable_expr_with_native_aliases(
    native_json: &Value,
    expr: &Value,
    prepared_symbols: &std::collections::HashSet<String>,
    visiting: &mut std::collections::HashSet<String>,
    depth: usize,
) -> Value {
    if depth > 24 {
        return expr.clone();
    }

    if let Some(obj) = expr.as_object() {
        if let Some(vr) = obj.get("VarRef").and_then(Value::as_object)
            && let Some(name) = vr.get("name").and_then(Value::as_str)
            && !prepared_symbols.contains(name)
            && !visiting.contains(name)
            && let Some(alias_expr) = find_observable_expr_from_native(native_json, name)
        {
            visiting.insert(name.to_string());
            let rewritten = rewrite_observable_expr_with_native_aliases(
                native_json,
                &alias_expr,
                prepared_symbols,
                visiting,
                depth + 1,
            );
            visiting.remove(name);
            return rewritten;
        }

        if let Some(name) = component_ref_name(expr)
            && !prepared_symbols.contains(&name)
            && !visiting.contains(&name)
            && let Some(alias_expr) = find_observable_expr_from_native(native_json, &name)
        {
            visiting.insert(name.clone());
            let rewritten = rewrite_observable_expr_with_native_aliases(
                native_json,
                &alias_expr,
                prepared_symbols,
                visiting,
                depth + 1,
            );
            visiting.remove(&name);
            return rewritten;
        }

        let mut out = Map::new();
        for (k, v) in obj {
            out.insert(
                k.clone(),
                rewrite_observable_expr_with_native_aliases(
                    native_json,
                    v,
                    prepared_symbols,
                    visiting,
                    depth + 1,
                ),
            );
        }
        return Value::Object(out);
    }

    if let Some(arr) = expr.as_array() {
        return Value::Array(
            arr.iter()
                .map(|v| {
                    rewrite_observable_expr_with_native_aliases(
                        native_json,
                        v,
                        prepared_symbols,
                        visiting,
                        depth + 1,
                    )
                })
                .collect(),
        );
    }

    expr.clone()
}

fn augment_prepared_with_native_observables(
    native_json: &Value,
    prepared_json: &mut Value,
) -> Option<usize> {
    let native_obj = native_json.as_object()?;
    let prepared_obj = as_object_mut(prepared_json)?;
    let native_y = native_obj.get("y").and_then(Value::as_object)?;
    let prepared_y = prepared_obj.get("y").and_then(Value::as_object);
    let prepared_symbols = collect_prepared_symbol_names(prepared_obj);

    let mut observables: Vec<Value> = Vec::new();
    for (name, comp) in native_y {
        if prepared_y.is_some_and(|m| m.contains_key(name)) {
            continue;
        }
        let Some(expr_raw) = find_observable_expr_from_native(native_json, name) else {
            continue;
        };
        let mut visiting = std::collections::HashSet::new();
        let expr = rewrite_observable_expr_with_native_aliases(
            native_json,
            &expr_raw,
            &prepared_symbols,
            &mut visiting,
            0,
        );
        let mut entry = Map::new();
        entry.insert("name".to_string(), Value::String(name.clone()));
        entry.insert("expr".to_string(), expr);
        if let Some(comp_obj) = comp.as_object() {
            if let Some(start) = comp_obj.get("start") {
                entry.insert("start".to_string(), start.clone());
            }
            if let Some(unit) = comp_obj
                .get("unit")
                .or_else(|| comp_obj.get("displayUnit"))
                .or_else(|| comp_obj.get("display_unit"))
            {
                entry.insert("unit".to_string(), unit.clone());
            }
        }
        observables.push(Value::Object(entry));
    }

    if observables.is_empty() {
        return Some(0);
    }
    let n = observables.len();
    prepared_obj.insert(
        "__rumoca_observables".to_string(),
        Value::Array(observables),
    );
    Some(n)
}

/// Build a rich compile response with DAE, balance info, and pretty output.
fn build_compile_response(result: &CompilationResult) -> Result<String, JsValue> {
    let dae = &result.dae;
    let dae_native_json = serde_json::to_value(dae).ok();
    let prepared = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        prepare_dae_for_template_codegen(dae, true)
    }));
    let (dae_prepared, dae_prepared_error) = match prepared {
        Ok(Ok(prepped)) => {
            let mut prepared_json = serde_json::to_value(prepped).ok();
            if let (Some(native), Some(prepared)) =
                (dae_native_json.as_ref(), prepared_json.as_mut())
            {
                let _ = augment_prepared_with_native_observables(native, prepared);
            }
            (prepared_json, None)
        }
        Ok(Err(err)) => (None, Some(err.to_string())),
        Err(panic_payload) => {
            let panic_msg = if let Some(msg) = panic_payload.downcast_ref::<&str>() {
                (*msg).to_string()
            } else if let Some(msg) = panic_payload.downcast_ref::<String>() {
                msg.clone()
            } else {
                "unknown panic payload".to_string()
            };
            (
                None,
                Some(format!(
                    "prepare_dae_for_template_codegen panicked: {}",
                    panic_msg
                )),
            )
        }
    };

    let num_eqs = dae.num_equations();
    let balance_val = dae_balance(dae);
    let num_unknowns = num_eqs as i64 - balance_val;
    let balance = serde_json::json!({
        "is_balanced": balance_val == 0,
        "num_equations": num_eqs,
        "num_unknowns": num_unknowns,
        "status": if balance_val == 0 { "Balanced" } else { "Unbalanced" },
    });

    let pretty = serde_json::to_string_pretty(dae).unwrap_or_default();

    let response = serde_json::json!({
        "dae": dae,
        "dae_native": dae,
        "dae_prepared": dae_prepared,
        "dae_prepared_error": dae_prepared_error,
        "balance": balance,
        "pretty": pretty,
    });

    serde_json::to_string(&response).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

fn compile_requested_model(
    session: &mut Session,
    model_name: &str,
) -> Result<CompilationResult, JsValue> {
    let mut report = session.compile_model_with_mode(
        model_name,
        CompilationMode::StrictReachableUncachedWithRecovery,
    );
    if !report.failures.is_empty() {
        return Err(JsValue::from_str(&format!(
            "Compilation error: {}",
            report.failure_summary(8)
        )));
    }
    match report.requested_result.take() {
        Some(PhaseResult::Success(result)) => Ok(*result),
        Some(PhaseResult::NeedsInner { missing_inners }) => Err(JsValue::from_str(&format!(
            "Compilation error: missing inner declarations: {}",
            missing_inners.join(", ")
        ))),
        Some(PhaseResult::Failed { phase, error, .. }) => {
            let phase_name = match phase {
                FailedPhase::Instantiate => "instantiate",
                FailedPhase::Typecheck => "typecheck",
                FailedPhase::Flatten => "flatten",
                FailedPhase::ToDae => "todae",
            };
            Err(JsValue::from_str(&format!(
                "Compilation error: {phase_name} failed: {error}"
            )))
        }
        None => Err(JsValue::from_str(&format!(
            "Compilation error: {}",
            report.failure_summary(8)
        ))),
    }
}

fn with_singleton_session<T>(
    f: impl FnOnce(&mut Session) -> Result<T, JsValue>,
) -> Result<T, JsValue> {
    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);
    f(session)
}

fn compile_source_in_session(
    session: &mut Session,
    source: &str,
    model_name: &str,
) -> Result<String, JsValue> {
    session.update_document("input.mo", source);
    let result = compile_requested_model(session, model_name)?;
    build_compile_response(&result)
}

#[derive(Default)]
struct LibraryLoadSummary {
    parsed_count: usize,
    inserted_count: usize,
    error_count: usize,
    skipped_files: Vec<String>,
}

fn parse_library_sources_json(libraries_json: &str) -> Result<BTreeMap<String, String>, JsValue> {
    serde_json::from_str(libraries_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid JSON: {}", e)))
}

fn load_library_sources_in_session(
    session: &mut Session,
    libraries_json: &str,
) -> Result<LibraryLoadSummary, JsValue> {
    let libraries = parse_library_sources_json(libraries_json)?;
    let mut definitions: Vec<(String, StoredDefinition)> = Vec::with_capacity(libraries.len());
    let mut skipped_files: Vec<String> = Vec::new();

    for (filename, source) in libraries {
        match parse_source_to_ast(&source, &filename) {
            Ok(definition) => definitions.push((filename, definition)),
            Err(error) => skipped_files.push(format!("{filename}: {error}")),
        }
    }

    let parsed_count = definitions.len();
    let inserted_count = session.replace_parsed_source_set(
        WASM_LIBRARY_SOURCE_SET_ID,
        definitions,
        Some("input.mo"),
    );

    Ok(LibraryLoadSummary {
        parsed_count,
        inserted_count,
        error_count: skipped_files.len(),
        skipped_files,
    })
}

/// Compile Modelica source code to DAE JSON.
#[wasm_bindgen]
pub fn compile(source: &str, model_name: &str) -> Result<String, JsValue> {
    with_singleton_session(|session| compile_source_in_session(session, source, model_name))
}

/// Compile Modelica source code to DAE JSON (alias for worker compatibility).
#[wasm_bindgen]
pub fn compile_to_json(source: &str, model_name: &str) -> Result<String, JsValue> {
    compile(source, model_name)
}

/// Compile using cached libraries if available.
#[wasm_bindgen]
pub fn compile_with_libraries(
    source: &str,
    model_name: &str,
    libraries_json: &str,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        if !libraries_json.trim().is_empty() {
            load_library_sources_in_session(session, libraries_json)?;
        }
        compile_source_in_session(session, source, model_name)
    })
}

// ==========================================================================
// Library Management
// ==========================================================================

/// Load and parse library sources into the session.
#[wasm_bindgen]
pub fn load_libraries(libraries_json: &str) -> Result<String, JsValue> {
    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);
    let summary = load_library_sources_in_session(session, libraries_json)?;

    let result = serde_json::json!({
        "parsed_count": summary.parsed_count,
        "inserted_count": summary.inserted_count,
        "error_count": summary.error_count,
        "library_names": [],
        "conflicts": [],
        "skipped_files": summary.skipped_files,
    });
    serde_json::to_string(&result).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Parse a single library file and return serialized AST.
#[wasm_bindgen]
pub fn parse_library_file(source: &str, filename: &str) -> Result<String, JsValue> {
    let def = parse_source_to_ast(source, filename)
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    serde_json::to_string(&def)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

/// Merge pre-parsed library definitions into the session.
#[wasm_bindgen]
pub fn merge_parsed_libraries(definitions_json: &str) -> Result<u32, JsValue> {
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

/// Clear the library cache.
#[wasm_bindgen]
pub fn clear_library_cache() {
    if let Ok(mut s) = SESSION.lock() {
        *s = None;
    }
}

/// Get the number of cached library documents.
#[wasm_bindgen]
pub fn get_library_count() -> u32 {
    SESSION
        .lock()
        .ok()
        .and_then(|s| s.as_ref().map(|sess| sess.document_uris().len() as u32))
        .unwrap_or(0)
}

#[derive(Default)]
struct DocumentationFields {
    info_html: Option<String>,
    revisions_html: Option<String>,
}

fn maybe_capture_documentation_field(path: &str, value: String, fields: &mut DocumentationFields) {
    let normalized = path.to_ascii_lowercase();
    if normalized.ends_with("documentation.info") {
        if fields.info_html.is_none() {
            fields.info_html = Some(value);
        }
    } else if normalized.ends_with("documentation.revisions") && fields.revisions_html.is_none() {
        fields.revisions_html = Some(value);
    }
}

fn collect_documentation_fields(
    expr: &Expression,
    context: Option<&str>,
    fields: &mut DocumentationFields,
) {
    match expr {
        Expression::ClassModification {
            target,
            modifications,
        } => {
            let next_context = join_path(context, &component_reference_to_path(target));
            for modification in modifications {
                collect_documentation_fields(modification, Some(&next_context), fields);
            }
        }
        Expression::FunctionCall { comp, args } => {
            let next_context = join_path(context, &component_reference_to_path(comp));
            for arg in args {
                collect_documentation_fields(arg, Some(&next_context), fields);
            }
        }
        Expression::NamedArgument { name, value } => {
            if let Some(text) = extract_string_literal(value) {
                let path = join_path(context, name.text.as_ref());
                maybe_capture_documentation_field(&path, text, fields);
            }
            collect_documentation_fields(value, context, fields);
        }
        Expression::Modification { target, value } => {
            let path = join_path(context, &component_reference_to_path(target));
            if let Some(text) = extract_string_literal(value) {
                maybe_capture_documentation_field(&path, text, fields);
            }
            collect_documentation_fields(value, Some(&path), fields);
        }
        Expression::Binary {
            op: OpBinary::Assign(_),
            lhs,
            rhs,
        } => {
            if let (Some(lhs_path), Some(text)) =
                (expression_path(lhs), extract_string_literal(rhs))
            {
                let full_path = join_path(context, &lhs_path);
                maybe_capture_documentation_field(&full_path, text, fields);
            }
            collect_documentation_fields(lhs, context, fields);
            collect_documentation_fields(rhs, context, fields);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            for element in elements {
                collect_documentation_fields(element, context, fields);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (_, branch_expr) in branches {
                collect_documentation_fields(branch_expr, context, fields);
            }
            collect_documentation_fields(else_branch, context, fields);
        }
        Expression::Parenthesized { inner } => collect_documentation_fields(inner, context, fields),
        _ => {}
    }
}

fn extract_documentation_fields(annotation: &[Expression]) -> DocumentationFields {
    let mut fields = DocumentationFields::default();
    for expr in annotation {
        collect_documentation_fields(expr, None, &mut fields);
    }
    fields
}

fn variability_label(variability: &Variability) -> String {
    match variability {
        Variability::Constant(_) => "constant".to_string(),
        Variability::Discrete(_) => "discrete".to_string(),
        Variability::Parameter(_) => "parameter".to_string(),
        Variability::Empty => "variable".to_string(),
    }
}

fn causality_label(causality: &Causality) -> String {
    match causality {
        Causality::Input(_) => "input".to_string(),
        Causality::Output(_) => "output".to_string(),
        Causality::Empty => "local".to_string(),
    }
}

fn build_class_tree_node(class: &ClassDef, parent_path: Option<&str>) -> WasmClassTreeNode {
    let name = class.name.text.to_string();
    let qualified_name = join_path(parent_path, &name);
    let mut children: Vec<&ClassDef> = class.classes.values().collect();
    children.sort_by(|a, b| a.name.text.cmp(&b.name.text));

    WasmClassTreeNode {
        name,
        qualified_name: qualified_name.clone(),
        class_type: class_type_label(&class.class_type),
        partial: class.partial,
        children: children
            .into_iter()
            .map(|child| build_class_tree_node(child, Some(&qualified_name)))
            .collect(),
    }
}

fn count_classes(node: &WasmClassTreeNode) -> usize {
    1 + node.children.iter().map(count_classes).sum::<usize>()
}

fn find_class_by_qualified_name<'a>(
    definitions: &'a StoredDefinition,
    qualified_name: &str,
) -> Option<&'a ClassDef> {
    let mut parts = qualified_name.split('.');
    let first = parts.next()?;
    let mut class = definitions.classes.get(first)?;
    for part in parts {
        class = class.classes.get(part)?;
    }
    Some(class)
}

fn list_classes_in_session(session: &mut Session) -> Result<String, JsValue> {
    let tree = session
        .tree()
        .map_err(|e| JsValue::from_str(&format!("Class tree error: {}", e)))?;

    let mut roots: Vec<&ClassDef> = tree.definitions.classes.values().collect();
    roots.sort_by(|a, b| a.name.text.cmp(&b.name.text));
    let classes: Vec<WasmClassTreeNode> = roots
        .into_iter()
        .map(|class| build_class_tree_node(class, None))
        .collect();
    let total_classes = classes.iter().map(count_classes).sum();

    let response = WasmClassTreeResponse {
        total_classes,
        classes,
    };
    serde_json::to_string(&response).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// List all loaded classes as a package/class hierarchy.
#[wasm_bindgen]
pub fn list_classes() -> Result<String, JsValue> {
    with_singleton_session(list_classes_in_session)
}

fn get_class_info_in_session(
    session: &mut Session,
    qualified_name: &str,
) -> Result<String, JsValue> {
    let tree = session
        .tree()
        .map_err(|e| JsValue::from_str(&format!("Class tree error: {}", e)))?;
    let class = find_class_by_qualified_name(&tree.definitions, qualified_name)
        .ok_or_else(|| JsValue::from_str(&format!("Class not found: {}", qualified_name)))?;
    let docs = extract_documentation_fields(&class.annotation);

    let mut components: Vec<WasmClassComponentInfo> = class
        .components
        .values()
        .map(|component| WasmClassComponentInfo {
            name: component.name.clone(),
            type_name: component.type_name.to_string(),
            variability: variability_label(&component.variability),
            causality: causality_label(&component.causality),
            description: token_list_to_text(&component.description),
        })
        .collect();
    components.sort_by(|a, b| a.name.cmp(&b.name));

    let info = WasmClassInfo {
        qualified_name: qualified_name.to_string(),
        class_type: class_type_label(&class.class_type),
        partial: class.partial,
        encapsulated: class.encapsulated,
        description: token_list_to_text(&class.description),
        documentation_html: docs.info_html,
        documentation_revisions_html: docs.revisions_html,
        component_count: class.components.len(),
        equation_count: class.equations.len() + class.initial_equations.len(),
        algorithm_count: class.algorithms.len() + class.initial_algorithms.len(),
        nested_class_count: class.classes.len(),
        source_modelica: class.to_modelica(""),
        components,
    };

    serde_json::to_string(&info).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get detailed class documentation and summary metadata.
#[wasm_bindgen]
pub fn get_class_info(qualified_name: &str) -> Result<String, JsValue> {
    with_singleton_session(|session| get_class_info_in_session(session, qualified_name))
}

// ==========================================================================
// Code Generation
// ==========================================================================

/// Render a Jinja template with DAE data.
#[wasm_bindgen]
pub fn render_template(dae_json: &str, template: &str) -> Result<String, JsValue> {
    let dae_value: serde_json::Value = serde_json::from_str(dae_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid DAE JSON: {}", e)))?;
    render_dae_template_with_json(&dae_value, template)
        .map_err(|e| JsValue::from_str(&format!("Template error: {}", e)))
}

// ==========================================================================
// LSP Functions — thin wrappers over rumoca-tool-lsp
// ==========================================================================

/// Compute diagnostics (syntax, lint, and compilation errors).
#[wasm_bindgen]
pub fn lsp_diagnostics(source: &str) -> Result<String, JsValue> {
    with_singleton_session(|session| lsp_diagnostics_in_session(session, source))
}

fn lsp_diagnostics_in_session(session: &mut Session, source: &str) -> Result<String, JsValue> {
    let diagnostics = rumoca_tool_lsp::compute_diagnostics(source, "input.mo", Some(session));
    serde_json::to_string(&diagnostics)
        .map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get hover information for a position.
#[wasm_bindgen]
pub fn lsp_hover(source: &str, line: u32, character: u32) -> Result<String, JsValue> {
    let ast = parse_source_to_ast(source, "input.mo").ok();
    let hover = rumoca_tool_lsp::handle_hover(source, ast.as_ref(), line, character);
    serde_json::to_string(&hover).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get code completion suggestions.
#[wasm_bindgen]
pub fn lsp_completion(source: &str, line: u32, character: u32) -> Result<String, JsValue> {
    let ast = parse_source_to_ast(source, "input.mo").ok();
    let lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&e.to_string()))?;
    let session = lock.as_ref();
    let items = rumoca_tool_lsp::handle_completion(source, ast.as_ref(), session, line, character);
    serde_json::to_string(&items).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get go-to-definition target(s) for a position.
#[wasm_bindgen]
pub fn lsp_definition(source: &str, line: u32, character: u32) -> Result<String, JsValue> {
    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&e.to_string()))?;
    let session = lock.get_or_insert_with(Session::default);
    session.update_document("input.mo", source);
    let Some(doc) = session.get_document("input.mo").cloned() else {
        return serde_json::to_string(&Option::<lsp_types::GotoDefinitionResponse>::None)
            .map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)));
    };
    let Some(ast) = doc.parsed.as_ref() else {
        return serde_json::to_string(&Option::<lsp_types::GotoDefinitionResponse>::None)
            .map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)));
    };
    let resolved = session.resolved().ok();
    let tree = resolved.as_ref().map(|resolved| &resolved.0);
    let uri = Url::parse("file:///input.mo")
        .map_err(|e| JsValue::from_str(&format!("Invalid URI: {}", e)))?;
    let response =
        rumoca_tool_lsp::handle_goto_definition(ast, tree, &doc.content, &uri, line, character);
    serde_json::to_string(&response).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get document symbols (outline).
#[wasm_bindgen]
pub fn lsp_document_symbols(source: &str) -> Result<String, JsValue> {
    let ast = parse_source_to_ast(source, "input.mo")
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    let symbols = rumoca_tool_lsp::handle_document_symbols(&ast);
    serde_json::to_string(&symbols).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get code actions (quick fixes) for diagnostics in a selected range.
#[wasm_bindgen]
pub fn lsp_code_actions(
    source: &str,
    range_start_line: u32,
    range_start_character: u32,
    range_end_line: u32,
    range_end_character: u32,
    diagnostics_json: &str,
) -> Result<String, JsValue> {
    let diagnostics: Vec<LspDiagnostic> = serde_json::from_str(diagnostics_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid diagnostics JSON: {}", e)))?;
    let range = Range {
        start: Position {
            line: range_start_line,
            character: range_start_character,
        },
        end: Position {
            line: range_end_line,
            character: range_end_character,
        },
    };
    let uri = Url::parse("file:///input.mo")
        .map_err(|e| JsValue::from_str(&format!("Invalid URI: {}", e)))?;
    let actions = rumoca_tool_lsp::handle_code_actions(&diagnostics, source, &range, Some(&uri));
    serde_json::to_string(&actions).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get semantic tokens for syntax highlighting.
#[wasm_bindgen]
pub fn lsp_semantic_tokens(source: &str) -> Result<String, JsValue> {
    let ast = parse_source_to_ast(source, "input.mo")
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    let tokens = rumoca_tool_lsp::handle_semantic_tokens(&ast);
    serde_json::to_string(&tokens).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get the semantic token legend.
#[wasm_bindgen]
pub fn lsp_semantic_token_legend() -> Result<String, JsValue> {
    let legend = rumoca_tool_lsp::get_semantic_token_legend();
    serde_json::to_string(&legend).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

// ==========================================================================
// Simulation
// ==========================================================================

/// Compile and simulate a Modelica model.
#[wasm_bindgen]
pub fn simulate_model(
    source: &str,
    model_name: &str,
    t_end: f64,
    dt: f64,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        simulate_model_in_session(session, source, model_name, t_end, dt)
    })
}

fn simulate_model_in_session(
    session: &mut Session,
    source: &str,
    model_name: &str,
    t_end: f64,
    dt: f64,
) -> Result<String, JsValue> {
    session.update_document("input.mo", source);
    let result = compile_requested_model(session, model_name)?;

    let dt_opt = if dt > 0.0 { Some(dt) } else { None };
    let opts = SimOptions {
        t_end,
        dt: dt_opt,
        ..SimOptions::default()
    };
    let sim = simulate_dae(&result.dae, &opts)
        .map_err(|e| JsValue::from_str(&format!("Simulation error: {}", e)))?;

    let output = serde_json::json!({
        "times": sim.times,
        "names": sim.names,
        "data": sim.data,
        "n_states": sim.n_states,
    });
    serde_json::to_string(&output).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

#[cfg(test)]
mod tests;
