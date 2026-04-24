//! WebAssembly bindings for Rumoca.
//!
//! Thin layer over `rumoca-session` and `rumoca-tool-lsp`. All heavy logic
//! lives in those crates; this module only provides WASM entry points.

mod class_browser_helpers;
#[cfg(any(feature = "sim-diffsol", feature = "sim-rk45"))]
mod simulation_api;
pub mod source_root_api;
#[cfg(feature = "stepper-diffsol")]
mod stepper_api;

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    sync::Mutex,
};

#[cfg(target_arch = "wasm32")]
use js_sys::Date;
use lsp_types::{Diagnostic as LspDiagnostic, Position, Range, Url};
use wasm_bindgen::prelude::*;
#[cfg(all(target_arch = "wasm32", feature = "wasm-rayon"))]
use wasm_bindgen_futures::JsFuture;
#[cfg(all(target_arch = "wasm32", feature = "wasm-rayon"))]
use wasm_bindgen_rayon::init_thread_pool;

use rumoca_session::Session;
use rumoca_session::codegen::render_dae_template_with_json;
use rumoca_session::codegen::templates as runtime_templates;
use rumoca_session::compile::{
    CompilationMode, CompilationResult, FailedPhase, PhaseResult, session_cache_stats,
};
use rumoca_session::parsing::ir_core as rumoca_ir_core;
use rumoca_session::parsing::{
    Causality, ClassDef, Expression, OpBinary, StoredDefinition, Variability, collect_model_names,
    parse_source_to_ast, validate_source_syntax,
};
use rumoca_tool_lint::{LintOptions, lint as lint_source};
use rumoca_tool_lsp::completion_metrics::{
    CompletionTimingSummary, extract_namespace_completion_prefix,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::class_browser_helpers::{
    class_type_label, component_reference_to_path, expression_path, extract_string_literal,
    join_path, token_list_to_text,
};
#[cfg(any(feature = "sim-diffsol", feature = "sim-rk45"))]
use crate::simulation_api::{simulate_model_impl, simulate_model_with_project_sources_impl};
pub use crate::source_root_api::{
    clear_source_root_cache, compile_with_project_sources, compile_with_source_roots,
    export_parsed_source_roots_binary, get_bundled_source_root_manifest,
    get_source_root_document_count, get_source_root_statuses, load_bundled_source_root_cache,
    load_source_roots, merge_parsed_source_roots, merge_parsed_source_roots_binary,
    parse_source_root_file, sync_project_sources,
};
#[cfg(feature = "stepper-diffsol")]
pub use crate::stepper_api::WasmStepper;

/// Global compilation session containing both bundled source-root and user documents.
static SESSION: Mutex<Option<Session>> = Mutex::new(None);
const WASM_BUNDLED_SOURCE_ROOT_SET_ID: &str = "wasm::bundled-source-roots";
const WASM_PROJECT_SOURCE_SET_ID: &str = "wasm::project";
const BUNDLED_SOURCE_ROOT_MANIFEST_JSON: &str = include_str!(concat!(
    env!("OUT_DIR"),
    "/bundled_source_root_manifest.json"
));
const BUNDLED_SOURCE_ROOT_CACHE_BYTES: &[u8] =
    include_bytes!(concat!(env!("OUT_DIR"), "/bundled_source_root_cache.bin"));

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TimedCompletionResponse {
    items: Vec<lsp_types::CompletionItem>,
    timing: CompletionTimingSummary,
}

#[cfg(target_arch = "wasm32")]
type WTimingStart = f64;
#[cfg(not(target_arch = "wasm32"))]
type WTimingStart = std::time::Instant;

#[cfg(target_arch = "wasm32")]
fn wasm_timing_start() -> WTimingStart {
    Date::now()
}

#[cfg(not(target_arch = "wasm32"))]
fn wasm_timing_start() -> WTimingStart {
    std::time::Instant::now()
}

#[cfg(target_arch = "wasm32")]
fn wasm_elapsed_ms(start: WTimingStart) -> u64 {
    (Date::now() - start).max(0.0).round() as u64
}

#[cfg(not(target_arch = "wasm32"))]
fn wasm_elapsed_ms(start: WTimingStart) -> u64 {
    start.elapsed().as_millis() as u64
}

// ==========================================================================
// Initialization
// ==========================================================================

/// Initialize panic hook for better error messages in console.
#[wasm_bindgen(start)]
pub fn init() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Initialize optional Rayon worker threads for wasm builds.
///
/// Returns `true` when the thread pool was initialized and `false` when threading
/// is unavailable in this build/runtime.
#[cfg(all(target_arch = "wasm32", feature = "wasm-rayon"))]
#[wasm_bindgen]
pub async fn wasm_init(num_threads: usize) -> Result<bool, JsValue> {
    if num_threads == 0 {
        return Ok(false);
    }
    JsFuture::from(init_thread_pool(num_threads)).await?;
    Ok(true)
}

/// Fallback thread-pool initializer for non-threaded builds.
#[cfg(not(all(target_arch = "wasm32", feature = "wasm-rayon")))]
#[wasm_bindgen]
pub fn wasm_init(_num_threads: usize) -> bool {
    false
}

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

/// Get the built-in codegen templates bundled with the WASM runtime.
#[wasm_bindgen]
pub fn get_builtin_templates() -> JsValue {
    let templates = vec![
        WasmBuiltinTemplate {
            id: "sympy.py.jinja",
            label: "SymPy (Python)",
            language: "python",
            source: runtime_templates::SYMPY,
        },
        WasmBuiltinTemplate {
            id: "jax.py.jinja",
            label: "JAX / Diffrax (Python)",
            language: "python",
            source: runtime_templates::JAX,
        },
        WasmBuiltinTemplate {
            id: "onnx.py.jinja",
            label: "ONNX (Python)",
            language: "python",
            source: runtime_templates::ONNX,
        },
        WasmBuiltinTemplate {
            id: "julia_mtk.jl.jinja",
            label: "Julia MTK",
            language: "julia",
            source: runtime_templates::JULIA_MTK,
        },
        WasmBuiltinTemplate {
            id: "casadi_sx.py.jinja",
            label: "CasADi SX (Python)",
            language: "python",
            source: runtime_templates::CASADI_SX,
        },
        WasmBuiltinTemplate {
            id: "casadi_mx.py.jinja",
            label: "CasADi MX (Python)",
            language: "python",
            source: runtime_templates::CASADI_MX,
        },
        WasmBuiltinTemplate {
            id: "embedded_c/model.h.jinja",
            label: "Embedded C Header",
            language: "c",
            source: runtime_templates::EMBEDDED_C_H,
        },
        WasmBuiltinTemplate {
            id: "embedded_c/model.c.jinja",
            label: "Embedded C Implementation",
            language: "c",
            source: runtime_templates::EMBEDDED_C_IMPL,
        },
        WasmBuiltinTemplate {
            id: "dae_modelica.mo.jinja",
            label: "DAE Modelica",
            language: "modelica",
            source: runtime_templates::DAE_MODELICA,
        },
        WasmBuiltinTemplate {
            id: "flat_modelica.mo.jinja",
            label: "Flat Modelica",
            language: "modelica",
            source: runtime_templates::FLAT_MODELICA,
        },
        WasmBuiltinTemplate {
            id: "fmi2/modelDescription.xml.jinja",
            label: "FMI 2.0 modelDescription.xml",
            language: "xml",
            source: runtime_templates::FMI2_MODEL_DESCRIPTION,
        },
        WasmBuiltinTemplate {
            id: "fmi2/model.c.jinja",
            label: "FMI 2.0 model.c",
            language: "c",
            source: runtime_templates::FMI2_MODEL,
        },
        WasmBuiltinTemplate {
            id: "fmi2/test_driver.c.jinja",
            label: "FMI 2.0 test driver",
            language: "c",
            source: runtime_templates::FMI2_TEST_DRIVER,
        },
        WasmBuiltinTemplate {
            id: "fmi3/modelDescription.xml.jinja",
            label: "FMI 3.0 modelDescription.xml",
            language: "xml",
            source: runtime_templates::FMI3_MODEL_DESCRIPTION,
        },
        WasmBuiltinTemplate {
            id: "fmi3/model.c.jinja",
            label: "FMI 3.0 model.c",
            language: "c",
            source: runtime_templates::FMI3_MODEL,
        },
        WasmBuiltinTemplate {
            id: "fmi3/test_driver.c.jinja",
            label: "FMI 3.0 test driver",
            language: "c",
            source: runtime_templates::FMI3_TEST_DRIVER,
        },
    ];
    serde_wasm_bindgen::to_value(&templates).unwrap_or(JsValue::NULL)
}

// ==========================================================================
// Parsing & Checking
// ==========================================================================

#[derive(Serialize, Deserialize)]
struct ParseResult {
    success: bool,
    error: Option<String>,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct WasmSimulationModelState {
    ok: bool,
    models: Vec<String>,
    selected_model: Option<String>,
    error: Option<String>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct WasmBuiltinTemplate {
    id: &'static str,
    label: &'static str,
    language: &'static str,
    source: &'static str,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BundledSourceRootManifest {
    archives: Vec<BundledSourceRootArchive>,
}

#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct BundledSourceRootArchive {
    archive_id: String,
    file_name: String,
    file_count: usize,
    source: String,
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

#[derive(Default)]
struct ParsedClassTreeNode {
    name: String,
    qualified_name: String,
    class_type: Option<String>,
    partial: bool,
    children: BTreeMap<String, ParsedClassTreeNode>,
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

fn attach_build_metadata(payload: &mut Value) {
    let Some(obj) = payload.as_object_mut() else {
        return;
    };
    let build = serde_json::json!({
        "version": env!("CARGO_PKG_VERSION"),
        "git_commit": option_env!("RUMOCA_GIT_COMMIT").unwrap_or("unknown"),
        "build_time_utc": option_env!("RUMOCA_BUILD_TIME_UTC").unwrap_or("unknown"),
    });
    obj.insert("__rumoca_build".to_string(), build);
}

/// Build a rich compile response with DAE, balance info, and pretty output.
fn build_compile_response(result: &CompilationResult) -> Result<String, JsValue> {
    let dae = &result.dae;
    let mut dae_native_json =
        serde_json::to_value(dae).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))?;
    attach_build_metadata(&mut dae_native_json);

    let num_eqs = dae.num_equations();
    let continuous_unknowns = dae.states.values().map(|v| v.size()).sum::<usize>()
        + dae.algebraics.values().map(|v| v.size()).sum::<usize>()
        + dae.outputs.values().map(|v| v.size()).sum::<usize>();
    let balance_val = num_eqs as i64 - continuous_unknowns as i64;
    let num_unknowns = num_eqs as i64 - balance_val;
    let balance = serde_json::json!({
        "is_balanced": balance_val == 0,
        "num_equations": num_eqs,
        "num_unknowns": num_unknowns,
        "status": if balance_val == 0 { "Balanced" } else { "Unbalanced" },
    });

    let pretty = serde_json::to_string_pretty(dae).unwrap_or_default();

    let response = serde_json::json!({
        "dae": dae_native_json.clone(),
        "dae_native": dae_native_json,
        "balance": balance,
        "pretty": pretty,
    });

    serde_json::to_string(&response).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

pub(crate) fn compile_requested_model(
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

pub(crate) fn with_singleton_session<T>(
    f: impl FnOnce(&mut Session) -> Result<T, JsValue>,
) -> Result<T, JsValue> {
    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);
    f(session)
}

pub(crate) fn qualify_input_model_name(session: &Session, model_name: &str) -> String {
    if model_name.contains('.') {
        return model_name.to_string();
    }

    let Some(doc) = session.get_document("input.mo") else {
        return model_name.to_string();
    };
    let Some(parsed) = doc.parsed().or(doc.recovered()) else {
        return model_name.to_string();
    };
    if !parsed.classes.contains_key(model_name) {
        return model_name.to_string();
    }

    let within = parsed
        .within
        .as_ref()
        .map(ToString::to_string)
        .filter(|prefix| !prefix.is_empty());
    within.map_or_else(
        || model_name.to_string(),
        |prefix| format!("{prefix}.{model_name}"),
    )
}

fn compile_source_in_session(
    session: &mut Session,
    source: &str,
    model_name: &str,
) -> Result<String, JsValue> {
    session.update_document("input.mo", source);
    let requested_model = qualify_input_model_name(session, model_name);
    let result = compile_requested_model(session, &requested_model)?;
    build_compile_response(&result)
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

/// Discover compilable simulation models in a source document.
#[wasm_bindgen]
pub fn get_simulation_models(source: &str, default_model: &str) -> Result<String, JsValue> {
    let parsed = parse_source_to_ast(source, "input.mo")
        .map_err(|error| JsValue::from_str(&error.to_string()))?;
    let models = collect_model_names(&parsed);
    let preferred = default_model.trim();
    let selected_model = if !preferred.is_empty() && models.iter().any(|model| model == preferred) {
        Some(preferred.to_string())
    } else {
        models.first().cloned()
    };
    serde_json::to_string(&WasmSimulationModelState {
        ok: true,
        models,
        selected_model,
        error: None,
    })
    .map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
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

fn parsed_definition_within(definitions: &StoredDefinition) -> Vec<String> {
    definitions
        .within
        .as_ref()
        .map(ToString::to_string)
        .unwrap_or_default()
        .split('.')
        .filter(|part| !part.is_empty())
        .map(ToString::to_string)
        .collect()
}

fn insert_parsed_class_tree(
    nodes: &mut BTreeMap<String, ParsedClassTreeNode>,
    parent_path: Option<&str>,
    within: &[String],
    class: &ClassDef,
) {
    let Some((segment, rest)) = within.split_first() else {
        let name = class.name.text.to_string();
        let qualified_name = join_path(parent_path, &name);
        let node = nodes
            .entry(name.clone())
            .or_insert_with(|| ParsedClassTreeNode {
                name,
                qualified_name: qualified_name.clone(),
                class_type: None,
                partial: false,
                children: BTreeMap::new(),
            });
        node.class_type = Some(class_type_label(&class.class_type));
        node.partial = class.partial;
        for child in class.classes.values() {
            insert_parsed_class_tree(&mut node.children, Some(&qualified_name), &[], child);
        }
        return;
    };

    let qualified_name = join_path(parent_path, segment);
    let node = nodes
        .entry(segment.clone())
        .or_insert_with(|| ParsedClassTreeNode {
            name: segment.clone(),
            qualified_name: qualified_name.clone(),
            class_type: Some("package".to_string()),
            partial: false,
            children: BTreeMap::new(),
        });
    insert_parsed_class_tree(&mut node.children, Some(&qualified_name), rest, class);
}

fn parsed_tree_node_to_wasm(node: ParsedClassTreeNode) -> WasmClassTreeNode {
    WasmClassTreeNode {
        name: node.name,
        qualified_name: node.qualified_name,
        class_type: node.class_type.unwrap_or_else(|| "package".to_string()),
        partial: node.partial,
        children: node
            .children
            .into_values()
            .map(parsed_tree_node_to_wasm)
            .collect(),
    }
}

fn parsed_class_tree(session: &Session) -> Vec<WasmClassTreeNode> {
    let mut roots: BTreeMap<String, ParsedClassTreeNode> = BTreeMap::new();
    for uri in session.document_uris() {
        let Some(doc) = session.get_document(uri) else {
            continue;
        };
        let Some(definitions) = doc.parsed().or(doc.recovered()) else {
            continue;
        };
        let within = parsed_definition_within(definitions);
        for class in definitions.classes.values() {
            insert_parsed_class_tree(&mut roots, None, &within, class);
        }
    }
    roots.into_values().map(parsed_tree_node_to_wasm).collect()
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

fn find_class_in_definition<'a>(
    definitions: &'a StoredDefinition,
    qualified_name: &str,
) -> Option<&'a ClassDef> {
    let relative = match definitions.within.as_ref().map(ToString::to_string) {
        Some(prefix) => {
            let suffix = qualified_name.strip_prefix(&prefix)?;
            let relative = suffix.strip_prefix('.').unwrap_or(suffix);
            if relative.is_empty() {
                return None;
            }
            relative
        }
        None => qualified_name,
    };
    find_class_by_qualified_name(definitions, relative)
}

fn find_class_in_session<'a>(session: &'a Session, qualified_name: &str) -> Option<&'a ClassDef> {
    for uri in session.document_uris() {
        let Some(doc) = session.get_document(uri) else {
            continue;
        };
        let Some(definitions) = doc.parsed().or(doc.recovered()) else {
            continue;
        };
        if let Some(class) = find_class_in_definition(definitions, qualified_name) {
            return Some(class);
        }
    }
    None
}

fn list_classes_in_session(session: &mut Session) -> Result<String, JsValue> {
    let classes = parsed_class_tree(session);
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
    let class = find_class_in_session(session, qualified_name)
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

fn wasm_cached_completion_class_name_count(
    session: &mut Session,
    completion_prefix: Option<&str>,
) -> usize {
    if completion_prefix.is_none() {
        if let Ok(entries) = session.namespace_index_query("") {
            return entries.len();
        }
        return session.namespace_class_names_cached().len();
    }
    let source_root_names = session
        .namespace_index_query("")
        .unwrap_or_default()
        .into_iter()
        .map(|(_, name, _)| name)
        .collect::<Vec<_>>();
    if source_root_names.is_empty() {
        session.namespace_class_names_cached().len()
    } else {
        source_root_names.len()
    }
}

fn timed_wasm_completion(
    session: &mut Session,
    source: &str,
    ast: Option<&StoredDefinition>,
    line: u32,
    character: u32,
) -> (Vec<lsp_types::CompletionItem>, CompletionTimingSummary) {
    let position = Position { line, character };
    let stats_before = session_cache_stats();
    let completion_started = wasm_timing_start();
    let completion_prefix = extract_namespace_completion_prefix(source, position);

    let completion_source_root_load_started = wasm_timing_start();
    if completion_prefix.is_some() {
        let _ = session.namespace_index_query("");
    }
    let completion_source_root_load_ms = wasm_elapsed_ms(completion_source_root_load_started);

    let class_name_count_after_ensure =
        wasm_cached_completion_class_name_count(session, completion_prefix.as_deref());
    let completion_handler_started = wasm_timing_start();
    let items = rumoca_tool_lsp::handle_completion(
        source,
        ast,
        Some(session),
        Some("input.mo"),
        line,
        character,
    );
    let completion_handler_ms = wasm_elapsed_ms(completion_handler_started);
    let total_ms = wasm_elapsed_ms(completion_started);
    let stats_after = session_cache_stats();
    let session_cache_delta = stats_after.delta_since(stats_before);
    let semantic_layer = if completion_prefix.is_some() {
        "package_def_map"
    } else if items.is_empty() {
        "syntax_fallback"
    } else {
        "class_interface"
    };

    (
        items,
        CompletionTimingSummary {
            requested_edit_epoch: 0,
            request_was_stale: false,
            uri: "file:///input.mo".to_string(),
            semantic_layer: semantic_layer.to_string(),
            source_root_load_ms: 0,
            completion_source_root_load_ms,
            namespace_completion_prime_ms: 0,
            needs_resolved_session: false,
            ast_fast_path_matched: false,
            query_fast_path_check_ms: 0,
            query_fast_path_matched: false,
            resolved_build_ms: None,
            completion_handler_ms,
            total_ms,
            built_resolved_tree: false,
            had_resolved_cache_before: false,
            namespace_index_query_hits: session_cache_delta.namespace_index_query_hits,
            namespace_index_query_misses: session_cache_delta.namespace_index_query_misses,
            file_item_index_query_hits: session_cache_delta.file_item_index_query_hits,
            file_item_index_query_misses: session_cache_delta.file_item_index_query_misses,
            declaration_index_query_hits: session_cache_delta.declaration_index_query_hits,
            declaration_index_query_misses: session_cache_delta.declaration_index_query_misses,
            scope_query_hits: session_cache_delta.scope_query_hits,
            scope_query_misses: session_cache_delta.scope_query_misses,
            source_set_package_membership_query_hits: session_cache_delta
                .source_set_package_membership_query_hits,
            source_set_package_membership_query_misses: session_cache_delta
                .source_set_package_membership_query_misses,
            orphan_package_membership_query_hits: session_cache_delta
                .orphan_package_membership_query_hits,
            orphan_package_membership_query_misses: session_cache_delta
                .orphan_package_membership_query_misses,
            class_name_count_after_ensure,
            session_cache_delta,
        },
    )
}

fn resolved_tree_for_navigation(
    session: &mut Session,
    ast: Option<&StoredDefinition>,
    line: u32,
) -> Option<rumoca_session::parsing::ast::ResolvedTree> {
    ast.and_then(|parsed| {
        rumoca_tool_lsp::helpers::find_enclosing_class_qualified_name(parsed, line)
    })
    .and_then(|active_model| {
        session
            .resolved_for_semantic_navigation(&active_model)
            .ok()
            .map(|resolved| resolved.as_ref().clone())
    })
    .or_else(|| session.resolved_cached())
}

fn local_component_hover(info: &rumoca_session::compile::LocalComponentInfo) -> lsp_types::Hover {
    let mut parts = Vec::new();
    if let Some(keyword_prefix) = &info.keyword_prefix {
        parts.push(keyword_prefix.clone());
    }
    parts.push(info.type_name.clone());
    let mut name = info.name.clone();
    if !info.shape.is_empty() {
        let dims = info
            .shape
            .iter()
            .map(|dim| dim.to_string())
            .collect::<Vec<_>>();
        name = format!("{name}[{}]", dims.join(", "));
    }
    parts.push(name);
    lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: format!("```modelica\n{}\n```", parts.join(" ")),
        }),
        range: None,
    }
}

fn class_target_hover(
    info: &rumoca_session::compile::NavigationClassTargetInfo,
) -> lsp_types::Hover {
    let mut value = format!(
        "```modelica\n{} {}\n```",
        class_type_keyword(&info.class_type),
        info.class_name
    );
    if let Some(description) = &info.description {
        value.push_str(&format!("\n\n{description}"));
    }
    if info.component_count > 0 || info.equation_count > 0 {
        value.push_str(&format!(
            "\n\n{} components, {} equations",
            info.component_count, info.equation_count
        ));
    }
    lsp_types::Hover {
        contents: lsp_types::HoverContents::Markup(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value,
        }),
        range: None,
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn url_from_session_document_uri(document_uri: &str) -> Option<Url> {
    Url::parse(document_uri)
        .ok()
        .or_else(|| Url::from_file_path(document_uri).ok())
}

#[cfg(target_arch = "wasm32")]
fn url_from_session_document_uri(document_uri: &str) -> Option<Url> {
    if let Ok(uri) = Url::parse(document_uri) {
        return Some(uri);
    }
    let mut normalized = document_uri.replace('\\', "/");
    if normalized.is_empty() {
        return None;
    }
    if !normalized.starts_with('/') {
        normalized.insert(0, '/');
    }
    Url::parse(&format!("file://{}", normalized)).ok()
}

fn class_target_definition(
    session: &Session,
    info: &rumoca_session::compile::NavigationClassTargetInfo,
    fallback_uri: &Url,
) -> lsp_types::GotoDefinitionResponse {
    let target_uri = resolve_session_target_uri(session, &info.target_uri, fallback_uri);
    lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
        uri: target_uri,
        range: rumoca_tool_lsp::helpers::location_to_range(&info.declaration_location),
    })
}

fn parsed_source_root_class_definition(
    session: &Session,
    ast: &StoredDefinition,
    tree: &rumoca_session::parsing::ast::ClassTree,
    source: &str,
    position: Position,
    fallback_uri: &Url,
) -> Option<lsp_types::GotoDefinitionResponse> {
    if let Some(qualified_name) =
        rumoca_tool_lsp::helpers::get_qualified_class_name_at_position(source, position)
        && let Some(def_id) = tree.get_def_id_by_name(&qualified_name)
    {
        return goto_response_for_def_id(session, tree, def_id, fallback_uri);
    }
    let word = rumoca_tool_lsp::helpers::get_word_at_position(source, position)?;
    let def_id = imported_def_id_in_definition(ast, tree, &word)?;
    goto_response_for_def_id(session, tree, def_id, fallback_uri)
}

fn local_component_definition(
    info: &rumoca_session::compile::LocalComponentInfo,
    uri: &Url,
) -> lsp_types::GotoDefinitionResponse {
    lsp_types::GotoDefinitionResponse::Scalar(lsp_types::Location {
        uri: uri.clone(),
        range: rumoca_tool_lsp::helpers::location_to_range(&info.declaration_location),
    })
}

fn imported_def_id_in_definition(
    ast: &StoredDefinition,
    tree: &rumoca_session::parsing::ast::ClassTree,
    name: &str,
) -> Option<rumoca_session::compile::core::DefId> {
    ast.classes
        .values()
        .find_map(|class| imported_def_id_in_class(class, tree, name))
}

fn imported_def_id_in_class(
    class: &ClassDef,
    tree: &rumoca_session::parsing::ast::ClassTree,
    name: &str,
) -> Option<rumoca_session::compile::core::DefId> {
    for import in &class.imports {
        if let Some(def_id) = rumoca_tool_lsp::helpers::imported_def_id(import, tree, name) {
            return Some(def_id);
        }
    }
    class
        .classes
        .values()
        .find_map(|nested| imported_def_id_in_class(nested, tree, name))
}

fn goto_response_for_def_id(
    session: &Session,
    tree: &rumoca_session::parsing::ast::ClassTree,
    def_id: rumoca_session::compile::core::DefId,
    fallback_uri: &Url,
) -> Option<lsp_types::GotoDefinitionResponse> {
    let class = tree.get_class_by_def_id(def_id)?;
    let loc = &class.name.location;
    let target_uri = target_uri_for_location(session, loc, fallback_uri);
    Some(lsp_types::GotoDefinitionResponse::Scalar(
        lsp_types::Location {
            uri: target_uri,
            range: rumoca_tool_lsp::helpers::location_to_range(loc),
        },
    ))
}

fn target_uri_for_location(
    session: &Session,
    loc: &rumoca_ir_core::Location,
    fallback_uri: &Url,
) -> Url {
    if loc.file_name.is_empty() {
        return fallback_uri.clone();
    }
    if !Path::new(loc.file_name.as_str()).is_absolute() {
        return resolve_session_target_uri(session, &loc.file_name, fallback_uri);
    }
    let path = Path::new(loc.file_name.as_str());
    if path.is_absolute()
        && let Some(uri) = url_from_file_path(path)
    {
        return uri;
    }
    if let Some(base_path) = file_path_from_url(fallback_uri)
        && let Some(parent) = base_path.parent()
    {
        let candidate = parent.join(path);
        if let Some(uri) = url_from_file_path(candidate) {
            return uri;
        }
    }
    fallback_uri.clone()
}

fn resolve_session_target_uri(session: &Session, target: &str, fallback_uri: &Url) -> Url {
    if let Some(uri) = url_from_session_document_uri(target) {
        return uri;
    }
    let normalized_target = target.replace('\\', "/");
    for document_uri in session.document_uris() {
        let document_uri = document_uri.to_string();
        let normalized_document = document_uri.replace('\\', "/");
        if normalized_document.ends_with(&normalized_target)
            && let Some(uri) = url_from_session_document_uri(&document_uri)
        {
            return uri;
        }
    }
    if !normalized_target.is_empty() {
        let relative = normalized_target.trim_start_matches('/');
        if let Ok(uri) = Url::parse(&format!("file:///{relative}")) {
            return uri;
        }
    }
    fallback_uri.clone()
}

#[cfg(not(target_arch = "wasm32"))]
fn file_path_from_url(uri: &Url) -> Option<PathBuf> {
    uri.to_file_path().ok()
}

#[cfg(target_arch = "wasm32")]
fn file_path_from_url(uri: &Url) -> Option<PathBuf> {
    if uri.scheme() != "file" {
        return None;
    }
    let path = uri.path();
    if path.is_empty() {
        return None;
    }
    Some(PathBuf::from(path))
}

#[cfg(not(target_arch = "wasm32"))]
fn url_from_file_path(path: impl AsRef<Path>) -> Option<Url> {
    Url::from_file_path(path).ok()
}

#[cfg(target_arch = "wasm32")]
fn url_from_file_path(path: impl AsRef<Path>) -> Option<Url> {
    let path = path.as_ref().to_string_lossy();
    let mut normalized = path.replace('\\', "/");
    if normalized.is_empty() {
        return None;
    }
    if !normalized.starts_with('/') {
        normalized.insert(0, '/');
    }
    Url::parse(&format!("file://{normalized}")).ok()
}

fn class_type_keyword(class_type: &rumoca_session::parsing::ast::ClassType) -> &'static str {
    match class_type {
        rumoca_session::parsing::ast::ClassType::Model => "model",
        rumoca_session::parsing::ast::ClassType::Block => "block",
        rumoca_session::parsing::ast::ClassType::Connector => "connector",
        rumoca_session::parsing::ast::ClassType::Record => "record",
        rumoca_session::parsing::ast::ClassType::Type => "type",
        rumoca_session::parsing::ast::ClassType::Package => "package",
        rumoca_session::parsing::ast::ClassType::Function => "function",
        rumoca_session::parsing::ast::ClassType::Class => "class",
        rumoca_session::parsing::ast::ClassType::Operator => "operator",
    }
}

/// Get hover information for a position.
#[wasm_bindgen]
pub fn lsp_hover(source: &str, line: u32, character: u32) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let Some(doc) = session.get_document("input.mo").cloned() else {
            return serde_json::to_string(&Option::<lsp_types::Hover>::None)
                .map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)));
        };
        let ast = doc.parsed();
        let hover = session
            .local_component_info_query("input.mo", line, character)
            .map(|info| local_component_hover(&info))
            .or_else(|| {
                session
                    .navigation_class_target_query("input.mo", line, character)
                    .map(|info| class_target_hover(&info))
            })
            .or_else(|| {
                let resolved = resolved_tree_for_navigation(session, ast, line);
                let tree = resolved.as_ref().map(|resolved| &resolved.0);
                rumoca_tool_lsp::handle_hover(&doc.content, ast, tree, line, character)
            });
        serde_json::to_string(&hover).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
    })
}

/// Get code completion suggestions.
#[wasm_bindgen]
pub fn lsp_completion(source: &str, line: u32, character: u32) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let doc = session.get_document("input.mo").cloned();
        let ast = doc.as_ref().and_then(|doc| doc.parsed());
        let (items, _) = timed_wasm_completion(session, source, ast, line, character);
        serde_json::to_string(&items).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
    })
}

/// Get code completion suggestions plus the completion timing breakdown used by the benchmark.
#[wasm_bindgen]
pub fn lsp_completion_with_timing(
    source: &str,
    line: u32,
    character: u32,
) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let doc = session.get_document("input.mo").cloned();
        let ast = doc.as_ref().and_then(|doc| doc.parsed());
        let (items, timing) = timed_wasm_completion(session, source, ast, line, character);
        let payload = TimedCompletionResponse { items, timing };
        serde_json::to_string(&payload)
            .map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
    })
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
    let Some(ast) = doc.parsed() else {
        return serde_json::to_string(&Option::<lsp_types::GotoDefinitionResponse>::None)
            .map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)));
    };
    let uri = Url::parse("file:///input.mo")
        .map_err(|e| JsValue::from_str(&format!("Invalid URI: {}", e)))?;
    let position = Position { line, character };
    let response = session
        .navigation_class_target_query("input.mo", line, character)
        .map(|info| class_target_definition(session, &info, &uri))
        .or_else(|| {
            session
                .local_component_info_query("input.mo", line, character)
                .map(|info| local_component_definition(&info, &uri))
        })
        .or_else(|| {
            let resolved = resolved_tree_for_navigation(session, Some(ast), line);
            let tree = resolved.as_ref().map(|resolved| &resolved.0);
            tree.and_then(|tree| {
                parsed_source_root_class_definition(
                    session,
                    ast,
                    tree,
                    &doc.content,
                    position,
                    &uri,
                )
            })
            .or_else(|| {
                rumoca_tool_lsp::handle_goto_definition(
                    ast,
                    tree,
                    &doc.content,
                    &uri,
                    line,
                    character,
                )
            })
        });
    serde_json::to_string(&response).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get document symbols (outline).
#[wasm_bindgen]
pub fn lsp_document_symbols(source: &str) -> Result<String, JsValue> {
    with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let symbols = session
            .document_symbol_query("input.mo")
            .map(rumoca_tool_lsp::handle_document_symbols);
        serde_json::to_string(&symbols)
            .map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
    })
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

/// Compile and simulate a Modelica model.
#[cfg(any(feature = "sim-diffsol", feature = "sim-rk45"))]
#[wasm_bindgen]
pub fn simulate_model(
    source: &str,
    model_name: &str,
    t_end: f64,
    dt: f64,
    solver: &str,
) -> Result<String, JsValue> {
    simulate_model_impl(source, model_name, t_end, dt, solver)
}

/// Compile with additional project-local sources and simulate a Modelica model.
#[cfg(any(feature = "sim-diffsol", feature = "sim-rk45"))]
#[wasm_bindgen]
pub fn simulate_model_with_project_sources(
    source: &str,
    model_name: &str,
    project_sources_json: &str,
    t_end: f64,
    dt: f64,
    solver: &str,
) -> Result<String, JsValue> {
    simulate_model_with_project_sources_impl(
        source,
        model_name,
        project_sources_json,
        t_end,
        dt,
        solver,
    )
}

#[cfg(test)]
mod tests;
