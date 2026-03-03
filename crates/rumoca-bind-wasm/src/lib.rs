//! WebAssembly bindings for Rumoca.
//!
//! Thin layer over `rumoca-session` and `rumoca-tool-lsp`. All heavy logic
//! lives in those crates; this module only provides WASM entry points.

use std::sync::Mutex;

use lsp_types::{Diagnostic as LspDiagnostic, Position, Range, Url};
use wasm_bindgen::prelude::*;

use rumoca_ir_ast::{
    Causality, ClassDef, ClassType, ComponentReference, Expression, OpBinary, StoredDefinition,
    TerminalType, Variability,
};
use rumoca_session::Session;
use serde::{Deserialize, Serialize};

/// Global compilation session containing both library and user documents.
static SESSION: Mutex<Option<Session>> = Mutex::new(None);

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
    let result = match rumoca_phase_parse::parse_string(source, "input.mo") {
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
    let options = rumoca_tool_lint::LintOptions::default();
    let messages = rumoca_tool_lint::lint(source, "input.mo", &options);
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
    if let Err(e) = rumoca_phase_parse::parse_string(source, "input.mo") {
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

/// Build a rich compile response with DAE, balance info, and pretty output.
fn build_compile_response(dae: &rumoca_session::Dae) -> Result<String, JsValue> {
    let num_eqs = dae.num_equations();
    let balance_val = dae.balance();
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
        "balance": balance,
        "pretty": pretty,
    });

    serde_json::to_string(&response).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

fn compile_requested_model(
    session: &mut Session,
    model_name: &str,
) -> Result<rumoca_session::CompilationResult, JsValue> {
    session
        .compile_model(model_name)
        .map_err(|e| JsValue::from_str(&format!("Compilation error: {}", e)))
}

/// Compile Modelica source code to DAE JSON.
#[wasm_bindgen]
pub fn compile(source: &str, model_name: &str) -> Result<String, JsValue> {
    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);
    session.update_document("input.mo", source);
    let result = compile_requested_model(session, model_name)?;
    build_compile_response(&result.dae)
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
    _libraries_json: &str,
) -> Result<String, JsValue> {
    compile(source, model_name)
}

// ==========================================================================
// Library Management
// ==========================================================================

/// Load and parse library sources into the session.
#[wasm_bindgen]
pub fn load_libraries(libraries_json: &str) -> Result<String, JsValue> {
    let libraries: std::collections::HashMap<String, String> = serde_json::from_str(libraries_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid JSON: {}", e)))?;

    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);

    let mut parsed_count = 0usize;
    let mut error_count = 0usize;
    let mut skipped: Vec<String> = Vec::new();

    for (filename, source) in &libraries {
        match session.add_document(filename, source) {
            Ok(()) => parsed_count += 1,
            Err(e) => {
                error_count += 1;
                skipped.push(format!("{}: {}", filename, e));
            }
        }
    }

    let result = serde_json::json!({
        "parsed_count": parsed_count,
        "error_count": error_count,
        "library_names": [],
        "conflicts": [],
        "skipped_files": skipped,
    });
    serde_json::to_string(&result).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Parse a single library file and return serialized AST.
#[wasm_bindgen]
pub fn parse_library_file(source: &str, filename: &str) -> Result<String, JsValue> {
    let def = rumoca_phase_parse::parse_to_ast(source, filename)
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

fn class_type_label(class_type: &ClassType) -> String {
    class_type.as_str().to_string()
}

fn token_list_to_text(tokens: &[rumoca_ir_ast::Token]) -> Option<String> {
    let text = tokens
        .iter()
        .map(|tok| tok.text.as_ref())
        .collect::<Vec<_>>()
        .join("");
    let trimmed = text.trim();
    if trimmed.is_empty() {
        None
    } else {
        Some(trimmed.to_string())
    }
}

fn component_reference_to_path(comp: &ComponentReference) -> String {
    comp.parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".")
}

fn expression_path(expr: &Expression) -> Option<String> {
    match expr {
        Expression::ComponentReference(comp) => Some(component_reference_to_path(comp)),
        Expression::FieldAccess { base, field } => {
            expression_path(base).map(|base_path| format!("{base_path}.{field}"))
        }
        Expression::Parenthesized { inner } => expression_path(inner),
        _ => None,
    }
}

fn extract_string_literal(expr: &Expression) -> Option<String> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::String,
            token,
        } => Some(token.text.to_string()),
        Expression::Parenthesized { inner } => extract_string_literal(inner),
        Expression::Binary {
            op: OpBinary::Add(_) | OpBinary::AddElem(_),
            lhs,
            rhs,
        } => {
            let lhs = extract_string_literal(lhs)?;
            let rhs = extract_string_literal(rhs)?;
            Some(format!("{lhs}{rhs}"))
        }
        _ => None,
    }
}

fn join_path(context: Option<&str>, tail: &str) -> String {
    match context {
        Some(prefix) if !prefix.is_empty() => format!("{prefix}.{tail}"),
        _ => tail.to_string(),
    }
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

/// List all loaded classes as a package/class hierarchy.
#[wasm_bindgen]
pub fn list_classes() -> Result<String, JsValue> {
    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);
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

/// Get detailed class documentation and summary metadata.
#[wasm_bindgen]
pub fn get_class_info(qualified_name: &str) -> Result<String, JsValue> {
    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);
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

// ==========================================================================
// Code Generation
// ==========================================================================

/// Load a DAE from JSON and generate code for a target.
#[wasm_bindgen]
pub fn generate_code(dae_json: &str, target: &str) -> Result<String, JsValue> {
    use rumoca_phase_codegen::templates;
    use rumoca_session::Dae;

    let dae: Dae = serde_json::from_str(dae_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid DAE JSON: {}", e)))?;

    let template = match target {
        "casadi" => templates::CASADI_SX,
        "cyecca" => templates::CYECCA,
        "julia" => templates::JULIA_MTK,
        "c" => templates::C_CODE,
        "jax" => templates::JAX,
        "onnx" => templates::ONNX,
        _ => return Err(JsValue::from_str(&format!("Unknown target: {}", target))),
    };

    rumoca_phase_codegen::render_template(&dae, template)
        .map_err(|e| JsValue::from_str(&format!("Code generation error: {}", e)))
}

/// Render a Jinja template with DAE data.
#[wasm_bindgen]
pub fn render_template(dae_json: &str, template: &str) -> Result<String, JsValue> {
    use rumoca_session::Dae;

    let dae: Dae = serde_json::from_str(dae_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid DAE JSON: {}", e)))?;
    rumoca_phase_codegen::render_template(&dae, template)
        .map_err(|e| JsValue::from_str(&format!("Template error: {}", e)))
}

// ==========================================================================
// LSP Functions — thin wrappers over rumoca-tool-lsp
// ==========================================================================

/// Compute diagnostics (syntax, lint, and compilation errors).
#[wasm_bindgen]
pub fn lsp_diagnostics(source: &str) -> Result<String, JsValue> {
    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&e.to_string()))?;
    let session = lock.get_or_insert_with(Session::default);
    let diagnostics = rumoca_tool_lsp::compute_diagnostics(source, "input.mo", Some(session));
    serde_json::to_string(&diagnostics)
        .map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get hover information for a position.
#[wasm_bindgen]
pub fn lsp_hover(source: &str, line: u32, character: u32) -> Result<String, JsValue> {
    let ast = rumoca_phase_parse::parse_to_ast(source, "input.mo").ok();
    let hover = rumoca_tool_lsp::handle_hover(source, ast.as_ref(), line, character);
    serde_json::to_string(&hover).map_err(|e| JsValue::from_str(&format!("JSON error: {}", e)))
}

/// Get code completion suggestions.
#[wasm_bindgen]
pub fn lsp_completion(source: &str, line: u32, character: u32) -> Result<String, JsValue> {
    let ast = rumoca_phase_parse::parse_to_ast(source, "input.mo").ok();
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
    let ast = rumoca_phase_parse::parse_to_ast(source, "input.mo")
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
    let ast = rumoca_phase_parse::parse_to_ast(source, "input.mo")
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
    use rumoca_sim_diffsol::{SimOptions, simulate};

    let mut lock = SESSION
        .lock()
        .map_err(|e| JsValue::from_str(&format!("Lock error: {}", e)))?;
    let session = lock.get_or_insert_with(Session::default);
    session.update_document("input.mo", source);
    let result = compile_requested_model(session, model_name)?;

    let dt_opt = if dt > 0.0 { Some(dt) } else { None };
    let opts = SimOptions {
        t_end,
        dt: dt_opt,
        ..SimOptions::default()
    };
    let sim = simulate(&result.dae, &opts)
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
mod tests {
    use super::*;

    #[test]
    fn test_get_version() {
        let version = get_version();
        assert!(!version.is_empty());
    }

    #[test]
    fn test_parse_valid() {
        let result = rumoca_phase_parse::parse_string("model M Real x; end M;", "test.mo");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_invalid() {
        let result = rumoca_phase_parse::parse_string("model M Real x end M;", "test.mo");
        assert!(result.is_err());
    }

    #[test]
    fn test_list_classes_includes_nested_packages() {
        clear_library_cache();
        let source = r#"
        package Lib
          package Nested
            model Probe
              Real x;
            equation
              x = 1.0;
            end Probe;
          end Nested;
        end Lib;
        "#;

        let mut lock = SESSION.lock().expect("session lock");
        let session = lock.get_or_insert_with(Session::default);
        session.update_document("input.mo", source);
        drop(lock);

        let json = list_classes().expect("list_classes should succeed");
        let tree: serde_json::Value = serde_json::from_str(&json).expect("valid JSON");
        assert_eq!(
            tree.get("total_classes")
                .and_then(|v| v.as_u64())
                .unwrap_or_default(),
            3
        );
        let classes = tree
            .get("classes")
            .and_then(|v| v.as_array())
            .expect("classes array");
        assert!(
            classes
                .iter()
                .any(|node| { node.get("qualified_name").and_then(|v| v.as_str()) == Some("Lib") }),
            "expected top-level package Lib in class tree: {tree:?}"
        );
    }

    const DOC_MODEL_SOURCE: &str = r#"
        model DocModel "Short description"
          Real x "State";
        equation
          der(x) = -x;
          annotation(
            Documentation(
              info = "<html><p>Detailed docs</p></html>",
              revisions = "<html><ul><li>r1</li></ul></html>"
            )
          );
        end DocModel;
        "#;

    #[cfg(target_arch = "wasm32")]
    #[test]
    fn test_get_class_info_extracts_documentation_annotation() {
        clear_library_cache();
        let mut lock = SESSION.lock().expect("session lock");
        let session = lock.get_or_insert_with(Session::default);
        session.update_document("input.mo", DOC_MODEL_SOURCE);
        drop(lock);

        let json = get_class_info("DocModel").expect("get_class_info should succeed");
        let info: serde_json::Value = serde_json::from_str(&json).expect("valid JSON");
        assert_eq!(
            info.get("class_type").and_then(|v| v.as_str()),
            Some("model"),
            "unexpected class info payload: {info:?}"
        );
        assert!(
            info.get("documentation_html")
                .and_then(|v| v.as_str())
                .is_some_and(|s| s.contains("Detailed docs")),
            "expected Documentation(info=...) to be extracted: {info:?}"
        );
        assert!(
            info.get("documentation_revisions_html")
                .and_then(|v| v.as_str())
                .is_some_and(|s| s.contains("<li>r1</li>")),
            "expected Documentation(revisions=...) to be extracted: {info:?}"
        );
        assert!(
            info.get("source_modelica")
                .and_then(|v| v.as_str())
                .is_some_and(|s| s.contains("model DocModel")),
            "expected reconstructed Modelica source in class info: {info:?}"
        );
    }

    #[cfg(not(target_arch = "wasm32"))]
    #[test]
    fn test_extract_documentation_annotation_fields_native() {
        let parsed = rumoca_phase_parse::parse_to_ast(DOC_MODEL_SOURCE, "input.mo")
            .expect("parse should succeed");
        let class =
            find_class_by_qualified_name(&parsed, "DocModel").expect("DocModel should be present");
        let docs = extract_documentation_fields(&class.annotation);
        assert!(
            docs.info_html
                .as_deref()
                .is_some_and(|s| s.contains("Detailed docs")),
            "expected Documentation(info=...) to be extracted, got: {:?}",
            docs.info_html
        );
        assert!(
            docs.revisions_html
                .as_deref()
                .is_some_and(|s| s.contains("<li>r1</li>")),
            "expected Documentation(revisions=...) to be extracted, got: {:?}",
            docs.revisions_html
        );
        assert!(
            class.to_modelica("").contains("model DocModel"),
            "expected reconstructed Modelica source to contain model header"
        );
    }

    #[test]
    fn test_compile_to_json_valid_model() {
        clear_library_cache();
        let source = r#"
        model Ball
          Real x(start=0);
          Real v(start=1);
        equation
          der(x) = v;
          der(v) = -9.81;
        end Ball;
        "#;

        let json = compile_to_json(source, "Ball").expect("compile_to_json should succeed");
        let result: serde_json::Value =
            serde_json::from_str(&json).expect("compile_to_json should return valid JSON");
        let balance = result
            .get("balance")
            .expect("compile output should include balance section");
        assert!(
            balance
                .get("is_balanced")
                .and_then(|v| v.as_bool())
                .unwrap_or(false),
            "expected Ball to be balanced, got: {balance:?}"
        );
        assert_eq!(
            balance
                .get("num_equations")
                .and_then(|v| v.as_u64())
                .unwrap_or_default(),
            2
        );
        assert_eq!(
            balance
                .get("num_unknowns")
                .and_then(|v| v.as_i64())
                .unwrap_or_default(),
            2
        );
    }

    #[test]
    fn test_compile_to_json_recovers_after_syntax_diagnostics() {
        clear_library_cache();
        let invalid = r#"
        model Ball
          Real x(start=0);
          Real v(start=1)
        equation
          der(x) = v;
          der(v) = -9.81;
        end Ball;
        "#;
        let valid = r#"
        model Ball
          Real x(start=0);
          Real v(start=1);
        equation
          der(x) = v;
          der(v) = -9.81;
        end Ball;
        "#;

        let diags_json =
            lsp_diagnostics(invalid).expect("diagnostics should still return syntax errors");
        let diags: Vec<serde_json::Value> =
            serde_json::from_str(&diags_json).expect("diagnostics payload should be valid JSON");
        assert!(
            diags.iter().any(|d| {
                d.get("code")
                    .and_then(|c| c.as_str())
                    .is_some_and(|code| code.starts_with("EP"))
            }),
            "expected syntax diagnostics for invalid source, got: {diags:?}"
        );

        let json = compile_to_json(valid, "Ball")
            .expect("compile_to_json should recover after diagnostics");
        let result: serde_json::Value =
            serde_json::from_str(&json).expect("compile_to_json should return valid JSON");
        assert!(
            result
                .get("balance")
                .and_then(|b| b.get("is_balanced"))
                .and_then(|v| v.as_bool())
                .unwrap_or(false),
            "expected recovered compile to be balanced, got: {result:?}"
        );
    }

    #[test]
    fn test_lsp_diagnostics_reports_unknown_builtin_modifier_with_multiple_classes() {
        clear_library_cache();
        let source = r#"
        package Lib
          model Helper
            Real y;
          equation
            y = 1.0;
          end Helper;
        end Lib;

        model M
          Real x(startd = 1.0);
        equation
          der(x) = -x;
        end M;
        "#;

        let json = lsp_diagnostics(source).expect("diagnostics should serialize");
        let diagnostics: Vec<serde_json::Value> =
            serde_json::from_str(&json).expect("diagnostics should be valid JSON");

        assert!(
            diagnostics.iter().any(|d| {
                d.get("code").and_then(|c| c.as_str()) == Some("ET001")
                    && d.get("message")
                        .and_then(|m| m.as_str())
                        .is_some_and(|m| m.contains("unknown modifier `startd`"))
            }),
            "expected ET001 unknown-modifier diagnostic, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn test_lsp_diagnostics_reports_unknown_builtin_modifier_startdt() {
        clear_library_cache();
        let source = r#"
        model M
          Real x(startdt = 1.0);
        equation
          der(x) = -x;
        end M;
        "#;

        let json = lsp_diagnostics(source).expect("diagnostics should serialize");
        let diagnostics: Vec<serde_json::Value> =
            serde_json::from_str(&json).expect("diagnostics should be valid JSON");

        assert!(
            diagnostics.iter().any(|d| {
                d.get("code").and_then(|c| c.as_str()) == Some("ET001")
                    && d.get("message")
                        .and_then(|m| m.as_str())
                        .is_some_and(|m| m.contains("unknown modifier `startdt`"))
            }),
            "expected ET001 unknown-modifier diagnostic, got: {:?}",
            diagnostics
        );
    }

    #[test]
    fn test_lsp_code_actions_returns_unknown_modifier_fix() {
        clear_library_cache();
        let source = r#"
        model M
          Real x(startdt = 1.0);
        equation
          der(x) = -x;
        end M;
        "#;

        let diag_json = lsp_diagnostics(source).expect("diagnostics should serialize");
        let diagnostics: Vec<serde_json::Value> =
            serde_json::from_str(&diag_json).expect("diagnostics should be valid JSON");
        let et001 = diagnostics
            .iter()
            .find(|d| d.get("code").and_then(|c| c.as_str()) == Some("ET001"))
            .expect("expected ET001 diagnostic");

        let range = et001.get("range").expect("diagnostic range");
        let start = range
            .get("start")
            .expect("range.start should exist")
            .as_object()
            .expect("range.start should be object");
        let end = range
            .get("end")
            .expect("range.end should exist")
            .as_object()
            .expect("range.end should be object");
        let start_line = start
            .get("line")
            .and_then(|v| v.as_u64())
            .unwrap_or_default() as u32;
        let start_character = start
            .get("character")
            .and_then(|v| v.as_u64())
            .unwrap_or_default() as u32;
        let end_line = end.get("line").and_then(|v| v.as_u64()).unwrap_or_default() as u32;
        let end_character = end
            .get("character")
            .and_then(|v| v.as_u64())
            .unwrap_or_default() as u32;

        let actions_json = lsp_code_actions(
            source,
            start_line,
            start_character,
            end_line,
            end_character,
            &serde_json::to_string(&vec![et001]).expect("serialize diagnostics"),
        )
        .expect("code actions should serialize");
        let actions: Vec<serde_json::Value> =
            serde_json::from_str(&actions_json).expect("actions should be valid JSON");
        assert!(
            actions.iter().any(|action| {
                action
                    .get("title")
                    .and_then(|t| t.as_str())
                    .is_some_and(|title| title.contains("Replace `startdt` with `start`"))
            }),
            "expected unknown-modifier quick-fix action, got: {actions:?}"
        );
    }

    #[test]
    fn test_lsp_code_actions_returns_missing_semicolon_fix() {
        clear_library_cache();
        let source = r#"
        model M
          Real x(start=0)
        equation
          der(x) = -x;
        end M;
        "#;

        let diag_json = lsp_diagnostics(source).expect("diagnostics should serialize");
        let diagnostics: Vec<serde_json::Value> =
            serde_json::from_str(&diag_json).expect("diagnostics should be valid JSON");
        let missing_semicolon_diag = diagnostics
            .iter()
            .find(|d| {
                d.get("code").and_then(|c| c.as_str()) == Some("EP001")
                    && d.get("message").and_then(|m| m.as_str()).is_some_and(|m| {
                        m.contains("missing `;`") || m.contains("unexpected `equation`")
                    })
            })
            .unwrap_or_else(|| {
                panic!("expected missing-semicolon diagnostic, got: {diagnostics:?}")
            });

        let range = missing_semicolon_diag
            .get("range")
            .expect("diagnostic range");
        let start = range
            .get("start")
            .expect("range.start should exist")
            .as_object()
            .expect("range.start should be object");
        let end = range
            .get("end")
            .expect("range.end should exist")
            .as_object()
            .expect("range.end should be object");
        let start_line = start
            .get("line")
            .and_then(|v| v.as_u64())
            .unwrap_or_default() as u32;
        let start_character = start
            .get("character")
            .and_then(|v| v.as_u64())
            .unwrap_or_default() as u32;
        let end_line = end.get("line").and_then(|v| v.as_u64()).unwrap_or_default() as u32;
        let end_character = end
            .get("character")
            .and_then(|v| v.as_u64())
            .unwrap_or_default() as u32;

        let actions_json = lsp_code_actions(
            source,
            start_line,
            start_character,
            end_line,
            end_character,
            &serde_json::to_string(&vec![missing_semicolon_diag]).expect("serialize diagnostics"),
        )
        .expect("code actions should serialize");
        let actions: Vec<serde_json::Value> =
            serde_json::from_str(&actions_json).expect("actions should be valid JSON");
        assert!(
            actions.iter().any(|action| {
                action
                    .get("title")
                    .and_then(|t| t.as_str())
                    .is_some_and(|title| title.contains("Insert missing `;`"))
            }),
            "expected missing-semicolon quick-fix action, got: {actions:?}"
        );
    }

    #[test]
    fn test_lsp_code_actions_returns_did_you_mean_type_fix() {
        clear_library_cache();
        let source = r#"
        model Ball
          Real x(start=0);
          Readl v(start=1);
        equation
          der(x) = v;
          der(v) = -9.81;
        end Ball;
        "#;

        let diag_json = lsp_diagnostics(source).expect("diagnostics should serialize");
        let diagnostics: Vec<serde_json::Value> =
            serde_json::from_str(&diag_json).expect("diagnostics should be valid JSON");
        let unresolved_type_diag = diagnostics
            .iter()
            .find(|d| {
                d.get("code").and_then(|c| c.as_str()) == Some("ER002")
                    && d.get("message")
                        .and_then(|m| m.as_str())
                        .is_some_and(|m| m.contains("unresolved type reference"))
            })
            .unwrap_or_else(|| panic!("expected unresolved-type diagnostic, got: {diagnostics:?}"));

        let range = unresolved_type_diag.get("range").expect("diagnostic range");
        let start = range
            .get("start")
            .expect("range.start should exist")
            .as_object()
            .expect("range.start should be object");
        let end = range
            .get("end")
            .expect("range.end should exist")
            .as_object()
            .expect("range.end should be object");
        let start_line = start
            .get("line")
            .and_then(|v| v.as_u64())
            .unwrap_or_default() as u32;
        let start_character = start
            .get("character")
            .and_then(|v| v.as_u64())
            .unwrap_or_default() as u32;
        let end_line = end.get("line").and_then(|v| v.as_u64()).unwrap_or_default() as u32;
        let end_character = end
            .get("character")
            .and_then(|v| v.as_u64())
            .unwrap_or_default() as u32;

        let actions_json = lsp_code_actions(
            source,
            start_line,
            start_character,
            end_line,
            end_character,
            &serde_json::to_string(&vec![unresolved_type_diag]).expect("serialize diagnostics"),
        )
        .expect("code actions should serialize");
        let actions: Vec<serde_json::Value> =
            serde_json::from_str(&actions_json).expect("actions should be valid JSON");
        assert!(
            actions.iter().any(|action| {
                action
                    .get("title")
                    .and_then(|t| t.as_str())
                    .is_some_and(|title| title.contains("Replace with `Real`"))
            }),
            "expected did-you-mean quick-fix action, got: {actions:?}"
        );
    }

    #[test]
    fn test_lsp_diagnostics_reports_builtin_modifier_type_mismatch() {
        clear_library_cache();
        let source = r#"
        model M
          Boolean df = true;
          Real v(start = df);
        equation
          der(v) = -v;
        end M;
        "#;

        let json = lsp_diagnostics(source).expect("diagnostics should serialize");
        let diagnostics: Vec<serde_json::Value> =
            serde_json::from_str(&json).expect("diagnostics should be valid JSON");

        assert!(
            diagnostics.iter().any(|d| {
                d.get("code").and_then(|c| c.as_str()) == Some("ET002")
                    && d.get("message").and_then(|m| m.as_str()).is_some_and(|m| {
                        m.contains("modifier `start`")
                            && m.contains("expects `Real`, found `Boolean`")
                    })
            }),
            "expected ET002 modifier type mismatch diagnostic, got: {:?}",
            diagnostics
        );
    }
}
