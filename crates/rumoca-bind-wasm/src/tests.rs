use super::*;
use rumoca_session::compile::{reset_session_cache_stats, session_cache_stats};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::source_root_api::sync_project_sources_with_cache_root_for_tests;

mod lsp_diagnostics_tests;
mod source_root_api_tests;

static SESSION_TEST_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

const MINI_MODELICA_LIBRARY: &str = r#"
    within ;
    package Modelica
      package Blocks
        package Sources
          model Constant
            parameter Real k = 1.0;
            output Real y;
          equation
            y = k;
          end Constant;
        end Sources;
      end Blocks;
    end Modelica;
    "#;

const USES_MODELICA_SOURCE: &str = r#"
    model UsesModelica
      import Modelica.Blocks.Sources.Constant;
      Constant c(k = 2.0);
      Real y;
    equation
      y = c.y;
    end UsesModelica;
    "#;

const WORKSPACE_PACKAGE_MO: &str = r#"
    within ;
    package NewFolder
    end NewFolder;
    "#;

const WORKSPACE_PACKAGE_TEST_MODEL: &str = r#"
    within NewFolder;
    model Test
      Real x;
    equation
      der(x) = 1;
    end Test;
    "#;

const USES_WORKSPACE_PACKAGE_SOURCE: &str = r#"
    model UsesWorkspacePackage
      import NewFolder.Test;
      Test test;
      Real y(start = 0);
    equation
      der(y) = test.x;
    end UsesWorkspacePackage;
    "#;

fn mini_modelica_source_root_json() -> String {
    serde_json::json!({
        "Modelica/package.mo": MINI_MODELICA_LIBRARY,
    })
    .to_string()
}

fn workspace_package_sources_json() -> String {
    serde_json::json!({
        "NewFolder/package.mo": WORKSPACE_PACKAGE_MO,
        "NewFolder/Test.mo": WORKSPACE_PACKAGE_TEST_MODEL,
    })
    .to_string()
}

fn session_test_guard() -> std::sync::MutexGuard<'static, ()> {
    SESSION_TEST_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner)
}

fn completion_labels(json: &str) -> Vec<String> {
    serde_json::from_str::<serde_json::Value>(json)
        .expect("completion JSON should be valid")
        .as_array()
        .expect("completion JSON should be an array")
        .iter()
        .filter_map(|item| {
            item.get("label")
                .and_then(|label| label.as_str())
                .map(ToOwned::to_owned)
        })
        .collect()
}

fn hover_markdown(json: &str) -> Option<String> {
    let hover: Option<lsp_types::Hover> =
        serde_json::from_str(json).expect("hover JSON should decode");
    let contents = hover?.contents;
    match contents {
        lsp_types::HoverContents::Markup(markup) => Some(markup.value),
        lsp_types::HoverContents::Scalar(lsp_types::MarkedString::String(text)) => Some(text),
        lsp_types::HoverContents::Scalar(lsp_types::MarkedString::LanguageString(text)) => {
            Some(text.value)
        }
        lsp_types::HoverContents::Array(parts) => Some(
            parts
                .into_iter()
                .map(|part| match part {
                    lsp_types::MarkedString::String(text) => text,
                    lsp_types::MarkedString::LanguageString(text) => text.value,
                })
                .collect::<Vec<_>>()
                .join("\n"),
        ),
    }
}

#[cfg(target_arch = "wasm32")]
fn decode_wasm_value<T: serde::de::DeserializeOwned>(value: JsValue) -> T {
    serde_wasm_bindgen::from_value(value).expect("wrapper payload should decode")
}

fn with_singleton_document(source: &str) {
    let mut lock = SESSION.lock().expect("session lock");
    let session = lock.get_or_insert_with(Session::default);
    session.update_document("input.mo", source);
}

fn singleton_session_has_standard_resolved_cached() -> bool {
    let lock = SESSION.lock().expect("session lock");
    let Some(session) = lock.as_ref() else {
        return false;
    };
    session.has_standard_resolved_cached()
}

fn decode_semantic_tokens(tokens: &[lsp_types::SemanticToken]) -> Vec<(u32, u32, u32, u32)> {
    let mut decoded = Vec::with_capacity(tokens.len());
    let mut line = 0u32;
    let mut col = 0u32;
    for token in tokens {
        line += token.delta_line;
        col = if token.delta_line == 0 {
            col + token.delta_start
        } else {
            token.delta_start
        };
        decoded.push((line, col, token.length, token.token_type));
    }
    decoded
}

fn lexeme_at(source: &str, line: u32, col: u32, len: u32) -> String {
    source
        .lines()
        .nth(line as usize)
        .unwrap_or_default()
        .chars()
        .skip(col as usize)
        .take(len as usize)
        .collect()
}

#[cfg(not(target_arch = "wasm32"))]
fn unique_test_cache_root() -> std::path::PathBuf {
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    std::env::temp_dir().join(format!(
        "rumoca-bind-wasm-source-root-cache-{}-{nonce}",
        std::process::id()
    ))
}

#[test]
fn test_get_version() {
    let version = get_version();
    assert!(!version.is_empty());
}

#[test]
fn test_get_git_commit() {
    let commit = get_git_commit();
    assert!(!commit.is_empty());
}

#[test]
fn test_get_build_time_utc() {
    let build_time = get_build_time_utc();
    assert!(!build_time.is_empty());
}

#[test]
fn test_init_start_hook_is_safe_to_call() {
    init();
}

#[test]
fn test_wasm_init_is_a_noop() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    wasm_init(4);
    assert_eq!(get_source_root_document_count(), 0);
}

#[test]
#[cfg(target_arch = "wasm32")]
fn test_parse_wrapper_serializes_success_and_error_shape() {
    let valid: ParseResult = decode_wasm_value(parse("model M\n  Real x;\nend M;\n"));
    assert!(valid.success, "expected successful parse wrapper payload");
    assert_eq!(valid.error, None);

    let invalid: ParseResult = decode_wasm_value(parse("model M Real x end M;"));
    assert!(
        !invalid.success,
        "expected invalid Modelica source to fail parse wrapper"
    );
    assert!(
        invalid.error.is_some(),
        "expected parse wrapper to include an error message"
    );
}

#[test]
#[cfg(not(target_arch = "wasm32"))]
fn test_parse_validation_still_distinguishes_valid_and_invalid_sources_on_native() {
    assert!(validate_source_syntax("model M\n  Real x;\nend M;\n", "input.mo").is_ok());
    assert!(validate_source_syntax("model M Real x end M;", "input.mo").is_err());
}

#[test]
#[cfg(target_arch = "wasm32")]
fn test_lint_wrapper_returns_naming_convention_message_shape() {
    let messages: Vec<WasmLintMessage> = decode_wasm_value(lint("model m Real x; end m;"));
    let message = messages
        .iter()
        .find(|message| message.rule == "naming-convention")
        .expect("expected lowercase model name lint warning");
    assert_eq!(message.level, "warning");
    assert!(
        message
            .message
            .contains("should start with uppercase (PascalCase)"),
        "unexpected lint message text: {}",
        message.message
    );
    assert!(
        message
            .suggestion
            .as_deref()
            .is_some_and(|suggestion| suggestion.contains("Rename to 'M'")),
        "expected rename suggestion in lint wrapper payload, got: {:?}",
        message.suggestion
    );
    assert!(message.line >= 1);
    assert!(message.column >= 1);
}

#[test]
#[cfg(target_arch = "wasm32")]
fn test_check_wrapper_reports_syntax_errors_and_valid_lint_messages() {
    let syntax_messages: Vec<WasmLintMessage> = decode_wasm_value(check("model M Real x end M;"));
    assert_eq!(
        syntax_messages.len(),
        1,
        "syntax errors should short-circuit to one wrapper payload"
    );
    let syntax_message = &syntax_messages[0];
    assert_eq!(syntax_message.rule, "syntax-error");
    assert_eq!(syntax_message.level, "error");
    assert!(
        !syntax_message.message.is_empty(),
        "expected syntax error text in check wrapper payload"
    );

    let lint_messages: Vec<WasmLintMessage> = decode_wasm_value(check("model m Real x; end m;"));
    assert!(
        lint_messages
            .iter()
            .any(|message| message.rule == "naming-convention"),
        "expected check wrapper to delegate to lint for valid source"
    );
}

#[test]
fn test_list_classes_includes_nested_packages() {
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

    let mut session = Session::default();
    session.update_document("input.mo", source);
    let json = list_classes_in_session(&mut session).expect("list_classes should succeed");
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
    let mut session = Session::default();
    session.update_document("input.mo", DOC_MODEL_SOURCE);
    let json =
        get_class_info_in_session(&mut session, "DocModel").expect("get_class_info should succeed");
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
    let parsed = parse_source_to_ast(DOC_MODEL_SOURCE, "input.mo").expect("parse should succeed");
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
    let mut session = Session::default();
    let source = r#"
    model Ball
      Real x(start=0);
      Real v(start=1);
    equation
      der(x) = v;
      der(v) = -9.81;
    end Ball;
    "#;

    let json =
        compile_source_in_session(&mut session, source, "Ball").expect("compile should succeed");
    let result: serde_json::Value =
        serde_json::from_str(&json).expect("compile should return valid JSON");
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
fn test_compile_to_json_matches_compile_wrapper_output() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let source = r#"
    model Ball
      Real x(start=0);
      Real v(start=1);
    equation
      der(x) = v;
      der(v) = -9.81;
    end Ball;
    "#;

    let compiled = compile(source, "Ball").expect("compile should succeed");
    let compiled_to_json = compile_to_json(source, "Ball").expect("compile_to_json should succeed");
    let compiled_value: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");
    let compiled_to_json_value: serde_json::Value =
        serde_json::from_str(&compiled_to_json).expect("compile_to_json should return valid JSON");
    assert_eq!(
        compiled_value, compiled_to_json_value,
        "compile_to_json should remain an exact alias of compile"
    );

    clear_source_root_cache();
}

#[test]
fn test_compile_to_json_qualifies_unqualified_within_model_name() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let source = r#"
    within Outer;
    model Example
      Real x;
    equation
      x = 1.0;
    end Example;
    "#;

    let compiled = compile(source, "Example").expect("compile should qualify within prefix");
    let compiled_result: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");
    assert!(
        compiled_result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected within-qualified model to compile successfully, got: {compiled_result:?}"
    );

    clear_source_root_cache();
}

#[test]
fn test_load_source_roots_creates_usable_source_root_source_set() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let result_json = load_source_roots(&mini_modelica_source_root_json())
        .expect("load_source_roots should succeed");
    let result: serde_json::Value =
        serde_json::from_str(&result_json).expect("load_source_roots should return JSON");

    assert_eq!(
        result
            .get("parsed_count")
            .and_then(|value| value.as_u64())
            .unwrap_or_default(),
        1
    );
    assert_eq!(
        result
            .get("inserted_count")
            .and_then(|value| value.as_u64())
            .unwrap_or_default(),
        1
    );
    assert_eq!(
        result
            .get("error_count")
            .and_then(|value| value.as_u64())
            .unwrap_or_default(),
        0
    );
    assert_eq!(get_source_root_document_count(), 1);

    let compiled = compile(USES_MODELICA_SOURCE, "UsesModelica")
        .expect("compile should succeed with preloaded source root");
    let compiled_result: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");
    assert!(
        compiled_result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected source-root-backed model to compile successfully, got: {compiled_result:?}"
    );

    clear_source_root_cache();
}

#[test]
fn test_compile_with_source_roots_uses_supplied_source_root_sources() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let compiled = compile_with_source_roots(
        USES_MODELICA_SOURCE,
        "UsesModelica",
        &mini_modelica_source_root_json(),
    )
    .expect("compile_with_source_roots should succeed");
    let compiled_result: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");

    assert!(
        compiled_result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected compile_with_source_roots to honor supplied source roots, got: {compiled_result:?}"
    );
    assert!(
        get_source_root_document_count() >= 1,
        "expected compile_with_source_roots to populate at least one cached source-root document"
    );

    clear_source_root_cache();
}

#[test]
fn test_compile_with_source_roots_preserves_cached_source_roots_when_given_empty_object() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    load_source_roots(&mini_modelica_source_root_json()).expect("load_source_roots should succeed");
    let compiled = compile_with_source_roots(USES_MODELICA_SOURCE, "UsesModelica", "{}")
        .expect("compile_with_source_roots should reuse cached source roots");
    let compiled_result: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");

    assert!(
        compiled_result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected compile_with_source_roots to preserve cached source roots for '{{}}', got: {compiled_result:?}"
    );
    assert!(
        get_source_root_document_count() >= 1,
        "expected cached source-root documents to remain available after compile_with_source_roots('{{}}')"
    );

    clear_source_root_cache();
}

#[test]
fn test_parse_source_root_file_and_merge_parsed_source_roots_support_compilation() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let ast_json = parse_source_root_file(MINI_MODELICA_LIBRARY, "Modelica/package.mo")
        .expect("parse_source_root_file should serialize an AST");
    let parsed: rumoca_session::parsing::ast::StoredDefinition =
        serde_json::from_str(&ast_json).expect("parse_source_root_file should return AST JSON");
    assert!(
        parsed.classes.contains_key("Modelica"),
        "expected parsed source-root AST to include the top-level package"
    );

    let definitions_json = serde_json::to_string(&vec![("Modelica/package.mo", ast_json)])
        .expect("serialize parsed source-root definitions");
    let merged = merge_parsed_source_roots(&definitions_json)
        .expect("merge_parsed_source_roots should succeed");
    assert_eq!(
        merged, 1,
        "expected one parsed source-root definition to merge"
    );
    assert_eq!(get_source_root_document_count(), 1);

    let compiled = compile(USES_MODELICA_SOURCE, "UsesModelica")
        .expect("merged parsed source roots should be visible to compile");
    let compiled_result: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");
    assert!(
        compiled_result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected merged source-root definitions to support successful compilation, got: {compiled_result:?}"
    );

    clear_source_root_cache();
}

#[test]
fn test_clear_source_root_cache_clears_the_singleton_session() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    load_source_roots(&mini_modelica_source_root_json()).expect("load_source_roots should succeed");
    assert_eq!(get_source_root_document_count(), 1);

    clear_source_root_cache();

    assert_eq!(
        get_source_root_document_count(),
        0,
        "clear_source_root_cache should remove loaded source-root documents"
    );
}

#[test]
fn test_compile_with_source_roots_ignores_unrelated_session_parse_errors() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    {
        let mut lock = SESSION.lock().expect("session lock");
        let session = lock.get_or_insert_with(Session::default);
        session.update_document("Broken.mo", "model Broken\n  Real x\nend Broken;\n");
    }

    let compiled = compile_with_source_roots(
        USES_MODELICA_SOURCE,
        "UsesModelica",
        &mini_modelica_source_root_json(),
    )
    .expect("focused compile should ignore unrelated session parse errors");
    let compiled_result: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");

    assert!(
        compiled_result
            .get("balance")
            .and_then(|b| b.get("is_balanced"))
            .and_then(|v| v.as_bool())
            .unwrap_or(false),
        "expected compile_with_source_roots to ignore unrelated parse errors, got: {compiled_result:?}"
    );

    clear_source_root_cache();
}

#[test]
fn test_lsp_completion_uses_loaded_source_root_completion_cache() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    reset_session_cache_stats();

    load_source_roots(&mini_modelica_source_root_json()).expect("load_source_roots should succeed");
    let source = "model UsesModelica\n  Modelica.\nend UsesModelica;\n";
    let line = 1;
    let character = "  Modelica.".len() as u32;

    let before = session_cache_stats();
    let first = lsp_completion(source, line, character).expect("cold completion should succeed");
    let after_first = session_cache_stats();
    let first_delta = after_first.delta_since(before);
    let first_items: Vec<lsp_types::CompletionItem> =
        serde_json::from_str(&first).expect("completion JSON should decode");
    assert!(
        first_items.iter().any(|item| item.label == "Blocks"),
        "expected source-root namespace completion items, got: {first_items:?}"
    );
    {
        let lock = SESSION.lock().expect("session lock");
        let session = lock.as_ref().expect("singleton session should exist");
        assert!(
            session.namespace_fingerprint_cached("Modelica.").is_some(),
            "cold completion should populate the source-root namespace cache"
        );
    }
    assert_eq!(
        first_delta.semantic_navigation_builds, 0,
        "source-root namespace completion should avoid semantic navigation"
    );
    assert_eq!(
        first_delta.strict_resolved_builds, 0,
        "source-root namespace completion should avoid strict resolved state"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "source-root namespace completion should avoid populating the standard resolved session"
    );

    let second = lsp_completion(source, line, character).expect("warm completion should succeed");
    let second_delta = session_cache_stats().delta_since(after_first);
    let second_items: Vec<lsp_types::CompletionItem> =
        serde_json::from_str(&second).expect("completion JSON should decode");
    assert!(
        second_items.iter().any(|item| item.label == "Blocks"),
        "warm completion should still expose source-root namespace members"
    );
    assert_eq!(
        second_delta.semantic_navigation_builds, 0,
        "warm source-root namespace completion should avoid semantic navigation"
    );
    assert_eq!(
        second_delta.strict_resolved_builds, 0,
        "warm source-root namespace completion should avoid strict resolved state"
    );
    assert!(
        second_delta.namespace_completion_cache_hits >= 1,
        "warm source-root namespace completion should hit the source-root completion cache"
    );

    clear_source_root_cache();
}

#[test]
fn test_lsp_completion_with_timing_reports_cache_breakdown() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    reset_session_cache_stats();

    load_source_roots(&mini_modelica_source_root_json()).expect("load_source_roots should succeed");
    let source = "model UsesModelica\n  Modelica.\nend UsesModelica;\n";
    let line = 1;
    let character = "  Modelica.".len() as u32;

    let first_json = lsp_completion_with_timing(source, line, character)
        .expect("cold timed completion should succeed");
    let first: TimedCompletionResponse =
        serde_json::from_str(&first_json).expect("timed completion JSON should decode");
    assert!(
        first.items.iter().any(|item| item.label == "Blocks"),
        "expected timed completion items to include Blocks, got: {:?}",
        first.items
    );
    assert_eq!(first.timing.source_root_load_ms, 0);
    assert_eq!(first.timing.resolved_build_ms, None);
    assert_eq!(
        first.timing.session_cache_delta.semantic_navigation_builds,
        0
    );

    let second_json = lsp_completion_with_timing(source, line, character)
        .expect("warm timed completion should succeed");
    let second: TimedCompletionResponse =
        serde_json::from_str(&second_json).expect("timed completion JSON should decode");
    assert!(
        second
            .timing
            .session_cache_delta
            .namespace_completion_cache_hits
            >= 1,
        "warm timed completion should report a source-root completion cache hit"
    );
    assert_eq!(
        second.timing.session_cache_delta.semantic_navigation_builds,
        0
    );

    clear_source_root_cache();
}

#[test]
fn test_lsp_completion_keeps_local_member_lookup_on_ast_fast_path() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let source = r#"model Plane
  Real x, y, theta;
equation
  der(x) = cos(theta);
  der(y) = sin(theta);
  der(theta) = 1;
end Plane;

model Sim
  Plane p1, p2;
equation
  p1.x = 1;
end Sim;
"#;
    let line = 11;
    let character = "  p1.".len() as u32;

    let first = lsp_completion(source, line, character).expect("cold completion should succeed");
    let first_items: Vec<lsp_types::CompletionItem> =
        serde_json::from_str(&first).expect("completion JSON should decode");
    assert!(
        first_items.iter().any(|item| item.label == "x"),
        "expected semantic member completion items, got: {first_items:?}"
    );
    {
        let lock = SESSION.lock().expect("session lock");
        let session = lock.as_ref().expect("singleton session should exist");
        assert!(
            !session.has_semantic_navigation_cached("Sim"),
            "local member completion should stay on the AST fast path"
        );
    }

    let second = lsp_completion(source, line, character).expect("warm completion should succeed");
    let second_items: Vec<lsp_types::CompletionItem> =
        serde_json::from_str(&second).expect("completion JSON should decode");
    assert!(
        second_items.iter().any(|item| item.label == "x"),
        "warm completion should still expose local semantic members"
    );

    clear_source_root_cache();
}

#[test]
fn test_lsp_diagnostics_reuses_semantic_diagnostics_cache() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let source = "model Active\n  Real x;\nequation\n  der(x) = -x;\nend Active;\n";

    let first = lsp_diagnostics(source).expect("cold diagnostics should succeed");
    let first_diags: Vec<lsp_types::Diagnostic> =
        serde_json::from_str(&first).expect("diagnostics JSON should decode");
    assert!(
        first_diags.is_empty(),
        "expected a clean model to produce no diagnostics, got: {first_diags:?}"
    );
    {
        let lock = SESSION.lock().expect("session lock");
        let session = lock.as_ref().expect("singleton session should exist");
        assert!(
            session.has_semantic_diagnostics_cached("Active"),
            "cold diagnostics should populate the semantic diagnostics cache"
        );
    }

    let second = lsp_diagnostics(source).expect("warm diagnostics should succeed");
    let second_diags: Vec<lsp_types::Diagnostic> =
        serde_json::from_str(&second).expect("diagnostics JSON should decode");
    assert!(
        second_diags.is_empty(),
        "warm diagnostics should reuse cached semantic diagnostics for clean models"
    );

    clear_source_root_cache();
}

#[test]
fn test_wasm_lsp_hover_uses_parsed_source_root_fast_path_for_imported_class() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    reset_session_cache_stats();

    let source_roots = serde_json::json!({
        "Lib/package.mo": "package Lib\n  block Target\n    Real y;\n  equation\n    y = 1;\n  end Target;\nend Lib;\n",
    })
    .to_string();
    load_source_roots(&source_roots).expect("load_source_roots should succeed");

    let source = r#"model M
  import Lib.Target;
  Target target;
equation
  target.y = 1;
end M;
"#;
    let import_line = source.lines().nth(1).expect("import line");
    let char_pos = import_line.find("Target").expect("Target token") as u32 + 1;

    let before = session_cache_stats();
    let first_json = lsp_hover(source, 1, char_pos).expect("cold hover");
    let after_first = session_cache_stats();
    let first_delta = after_first.delta_since(before);
    let first_hover = hover_markdown(&first_json).expect("hover should resolve imported class");
    assert!(
        first_hover.contains("block Target"),
        "expected hover to resolve the imported class, got: {first_hover}"
    );
    assert_eq!(
        first_delta.semantic_navigation_builds, 0,
        "import-line hover should stay off semantic navigation"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "hover should avoid populating the standard resolved session"
    );

    let second_json = lsp_hover(source, 1, char_pos).expect("warm hover");
    let second_delta = session_cache_stats().delta_since(after_first);
    let second_hover = hover_markdown(&second_json).expect("warm hover should still resolve");
    assert_eq!(
        first_hover, second_hover,
        "warm hover should preserve the hover payload"
    );
    assert_eq!(
        second_delta.semantic_navigation_builds, 0,
        "warm hover should keep using the parsed-source-root fast path"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "warm hover should continue avoiding the standard resolved session"
    );

    clear_source_root_cache();
}

#[test]
fn test_wasm_lsp_hover_uses_parsed_source_root_fast_path_for_qualified_class() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    reset_session_cache_stats();
    load_source_roots(&mini_modelica_source_root_json()).expect("load_source_roots should succeed");

    let source = r#"model UsesModelica
  Modelica.Blocks.Sources.Constant c;
end UsesModelica;
"#;
    let line = 1;
    let character = "  Modelica.Blocks.Sources.".len() as u32 + 1;

    let before = session_cache_stats();
    let hover_json = lsp_hover(source, line, character).expect("qualified hover should succeed");
    let delta = session_cache_stats().delta_since(before);
    let hover = hover_markdown(&hover_json).expect("hover should resolve the qualified class");
    assert!(
        hover.contains("model Constant"),
        "expected hover to describe the qualified source-root class, got: {hover}"
    );
    assert_eq!(
        delta.semantic_navigation_builds, 0,
        "qualified source-root hover should stay off semantic navigation"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "qualified source-root hover should avoid populating the standard resolved session"
    );

    clear_source_root_cache();
}

#[test]
fn test_wasm_lsp_definition_uses_parsed_source_root_fast_path_for_imported_class() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    reset_session_cache_stats();

    let source_roots = serde_json::json!({
        "Lib/package.mo": "package Lib\n  block Target\n    Real y;\n  equation\n    y = 1;\n  end Target;\nend Lib;\n",
    })
    .to_string();
    load_source_roots(&source_roots).expect("load_source_roots should succeed");

    let source = r#"model M
  import Lib.Target;
  Target target;
equation
  target.y = 1;
end M;
"#;
    let import_line = source.lines().nth(1).expect("import line");
    let char_pos = import_line.find("Target").expect("Target token") as u32 + 1;

    let before = session_cache_stats();
    let first_json = lsp_definition(source, 1, char_pos).expect("cold definition");
    let after_first = session_cache_stats();
    let first_delta = after_first.delta_since(before);
    let first_definition: Option<lsp_types::GotoDefinitionResponse> =
        serde_json::from_str(&first_json).expect("definition payload should decode");
    let Some(lsp_types::GotoDefinitionResponse::Scalar(first_location)) = first_definition.as_ref()
    else {
        panic!("expected scalar goto-definition response for imported class");
    };
    assert_eq!(
        first_location.range.start.line, 1,
        "expected goto-definition to jump to `Target`, got: {first_location:?}"
    );
    assert!(
        first_location.uri.to_string().contains("Lib/package.mo"),
        "expected goto-definition to point at the loaded source-root, got: {}",
        first_location.uri
    );
    assert_eq!(
        first_delta.semantic_navigation_builds, 0,
        "import-line goto-definition should stay off semantic navigation"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "goto-definition should avoid populating the standard resolved session"
    );

    let second_json = lsp_definition(source, 1, char_pos).expect("warm definition");
    let second_delta = session_cache_stats().delta_since(after_first);
    let second_definition: Option<lsp_types::GotoDefinitionResponse> =
        serde_json::from_str(&second_json).expect("definition payload should decode");
    assert_eq!(
        first_definition, second_definition,
        "warm goto-definition should preserve the target"
    );
    assert_eq!(
        second_delta.semantic_navigation_builds, 0,
        "warm goto-definition should keep using the parsed-source-root fast path"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "warm goto-definition should continue avoiding the standard resolved session"
    );

    clear_source_root_cache();
}

#[test]
fn test_wasm_lsp_definition_uses_parsed_source_root_fast_path_for_qualified_class() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    reset_session_cache_stats();
    load_source_roots(&mini_modelica_source_root_json()).expect("load_source_roots should succeed");

    let source = r#"model UsesModelica
  Modelica.Blocks.Sources.Constant c;
end UsesModelica;
"#;
    let line = 1;
    let character = "  Modelica.Blocks.Sources.".len() as u32 + 1;

    let before = session_cache_stats();
    let definition_json =
        lsp_definition(source, line, character).expect("qualified definition should succeed");
    let delta = session_cache_stats().delta_since(before);
    let definition: Option<lsp_types::GotoDefinitionResponse> =
        serde_json::from_str(&definition_json).expect("definition payload should decode");
    let Some(lsp_types::GotoDefinitionResponse::Scalar(location)) = definition else {
        panic!("expected scalar goto-definition response for qualified class");
    };
    assert!(
        location.uri.to_string().contains("Modelica/package.mo"),
        "expected goto-definition to resolve into the loaded source-root, got: {}",
        location.uri
    );
    assert_eq!(
        delta.semantic_navigation_builds, 0,
        "qualified source-root goto-definition should stay off semantic navigation"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "qualified source-root goto-definition should avoid populating the standard resolved session"
    );

    clear_source_root_cache();
}

#[test]
fn test_lsp_completion_rebuilds_ast_local_members_after_source_edit() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let source_v1 = r#"model Plane
  Real x, y, theta;
equation
  der(x) = cos(theta);
  der(y) = sin(theta);
  der(theta) = 1;
end Plane;

model Sim
  Plane p1, p2;
equation
  p1.x = 1;
end Sim;
"#;
    let source_v2 = r#"model Plane
  Real z, y, theta;
equation
  der(z) = cos(theta);
  der(y) = sin(theta);
  der(theta) = 1;
end Plane;

model Sim
  Plane p1, p2;
equation
  p1.z = 1;
end Sim;
"#;
    let line = 11;
    let character = "  p1.".len() as u32;

    let first = lsp_completion(source_v1, line, character).expect("first completion should work");
    let first_labels = completion_labels(&first);
    assert!(
        first_labels.iter().any(|label| label == "x"),
        "expected semantic member completion for x, got: {first_labels:?}"
    );
    {
        let lock = SESSION.lock().expect("session lock");
        let session = lock.as_ref().expect("singleton session");
        assert!(
            !session.has_semantic_navigation_cached("Sim"),
            "local member completion should stay on the AST fast path"
        );
    }

    let second = lsp_completion(source_v2, line, character).expect("edited completion should work");
    let second_labels = completion_labels(&second);
    assert!(
        second_labels.iter().any(|label| label == "z"),
        "edited completion should rebuild local members for z, got: {second_labels:?}"
    );
    assert!(
        !second_labels.iter().any(|label| label == "x"),
        "edited completion must not reuse stale AST-local members: {second_labels:?}"
    );

    clear_source_root_cache();
}

#[test]
fn test_list_classes_wrapper_serializes_singleton_class_tree() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    with_singleton_document(
        r#"
        package Lib
          package Nested
            model Probe
              Real x;
            equation
              x = 1.0;
            end Probe;
          end Nested;
        end Lib;
        "#,
    );

    let json = list_classes().expect("list_classes should succeed");
    let tree: serde_json::Value = serde_json::from_str(&json).expect("valid class tree JSON");
    assert_eq!(
        tree.get("total_classes").and_then(|value| value.as_u64()),
        Some(3)
    );
    let classes = tree
        .get("classes")
        .and_then(|value| value.as_array())
        .expect("class tree should include classes array");
    assert!(
        classes.iter().any(|node| {
            node.get("qualified_name").and_then(|value| value.as_str()) == Some("Lib")
        }),
        "expected top-level package in list_classes payload: {tree:?}"
    );

    clear_source_root_cache();
}

#[test]
fn test_list_classes_wrapper_tolerates_resolve_failures() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    with_singleton_document(
        r#"
        package BrokenLib
          model Broken
            Real x = Missing.value;
          end Broken;
        end BrokenLib;
        "#,
    );

    let json = list_classes().expect("list_classes should use parsed class structure");
    let tree: serde_json::Value = serde_json::from_str(&json).expect("valid class tree JSON");
    assert_eq!(
        tree.get("total_classes").and_then(|value| value.as_u64()),
        Some(2)
    );
    let classes = tree
        .get("classes")
        .and_then(|value| value.as_array())
        .expect("class tree should include classes array");
    let broken_lib = classes
        .iter()
        .find(|node| {
            node.get("qualified_name").and_then(|value| value.as_str()) == Some("BrokenLib")
        })
        .expect("expected top-level BrokenLib package");
    let children = broken_lib
        .get("children")
        .and_then(|value| value.as_array())
        .expect("BrokenLib should include nested classes");
    assert!(
        children.iter().any(|node| {
            node.get("qualified_name").and_then(|value| value.as_str()) == Some("BrokenLib.Broken")
        }),
        "expected parsed class tree to include BrokenLib.Broken despite resolve errors: {tree:?}"
    );

    clear_source_root_cache();
}

#[test]
fn test_get_class_info_wrapper_serializes_documentation_and_components() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    with_singleton_document(DOC_MODEL_SOURCE);

    let json = get_class_info("DocModel").expect("get_class_info should succeed");
    let info: serde_json::Value = serde_json::from_str(&json).expect("valid class info JSON");
    assert_eq!(
        info.get("qualified_name").and_then(|value| value.as_str()),
        Some("DocModel")
    );
    assert_eq!(
        info.get("class_type").and_then(|value| value.as_str()),
        Some("model")
    );
    assert_eq!(
        info.get("component_count").and_then(|value| value.as_u64()),
        Some(1)
    );
    assert!(
        info.get("documentation_html")
            .and_then(|value| value.as_str())
            .is_some_and(|html| html.contains("Detailed docs")),
        "expected documentation_html in get_class_info payload: {info:?}"
    );
    assert!(
        info.get("documentation_revisions_html")
            .and_then(|value| value.as_str())
            .is_some_and(|html| html.contains("<li>r1</li>")),
        "expected documentation_revisions_html in get_class_info payload: {info:?}"
    );

    clear_source_root_cache();
}

#[test]
fn test_get_class_info_wrapper_uses_parsed_within_source_root_docs() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let source_roots = serde_json::json!({
        "Lib/package.mo": "within ;\npackage Lib\nend Lib;\n",
        "Lib/Sub.mo": "within Lib;\npackage Sub\n  model Broken \"Doc\"\n    Real x = Missing.value;\n  end Broken;\nend Sub;\n",
    })
    .to_string();
    load_source_roots(&source_roots).expect("load_source_roots should succeed");

    let json = get_class_info("Lib.Sub.Broken").expect("get_class_info should read parsed docs");
    let info: serde_json::Value = serde_json::from_str(&json).expect("valid class info JSON");
    assert_eq!(
        info.get("qualified_name").and_then(|value| value.as_str()),
        Some("Lib.Sub.Broken")
    );
    assert_eq!(
        info.get("component_count").and_then(|value| value.as_u64()),
        Some(1)
    );
    assert!(
        info.get("source_modelica")
            .and_then(|value| value.as_str())
            .is_some_and(|source| source.contains("model Broken")),
        "expected parsed class source for within-loaded source-root class: {info:?}"
    );

    clear_source_root_cache();
}

#[test]
fn test_lsp_completion_reuses_loaded_source_root_namespace_cache_after_local_edit() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    load_source_roots(&mini_modelica_source_root_json()).expect("load_source_roots should succeed");

    let source_v1 = "model UsesModelica\n  Modelica.\nend UsesModelica;\n";
    let source_v2 = "model UsesModelica\n  Real localX;\n  Modelica.\nend UsesModelica;\n";

    let first = lsp_completion(source_v1, 1, "  Modelica.".len() as u32)
        .expect("first source-root completion should work");
    let first_labels = completion_labels(&first);
    assert!(
        first_labels.iter().any(|label| label == "Blocks"),
        "expected loaded source-root namespace completion to include Blocks, got: {first_labels:?}"
    );
    let fingerprint_before = {
        let lock = SESSION.lock().expect("session lock");
        let session = lock.as_ref().expect("singleton session");
        session
            .namespace_fingerprint_cached("Modelica.")
            .expect("Modelica namespace fingerprint should be cached")
    };

    let second = lsp_completion(source_v2, 2, "  Modelica.".len() as u32)
        .expect("warm source-root completion should work");
    let second_labels = completion_labels(&second);
    assert!(
        second_labels.iter().any(|label| label == "Blocks"),
        "local edits should preserve loaded source-root namespace completion, got: {second_labels:?}"
    );
    {
        let lock = SESSION.lock().expect("session lock");
        let session = lock.as_ref().expect("singleton session");
        assert_eq!(
            session.namespace_fingerprint_cached("Modelica."),
            Some(fingerprint_before),
            "unrelated local edits should preserve the loaded source-root namespace cache"
        );
    }

    clear_source_root_cache();
}

#[test]
fn test_compile_to_json_prepared_retains_observables_from_native_orbit_model() {
    let mut session = Session::default();
    let source = r#"
    model SatelliteOrbit2D
      parameter Real mu = 398600.4418;
      parameter Real r0 = 7000;
      parameter Real v0 = sqrt(mu / r0);
      Real rx(start = r0, fixed = true);
      Real ry(start = 0, fixed = true);
      Real vx(start = 0, fixed = true);
      Real vy(start = v0, fixed = true);
      Real inv_r;
      Real inv_v2;
      Real inv_h;
      Real inv_energy;
      Real inv_a;
      Real inv_rv;
      Real inv_ex;
      Real inv_ey;
      Real inv_ecc;
    equation
      der(rx) = vx;
      der(ry) = vy;
      inv_r = sqrt(rx * rx + ry * ry);
      inv_v2 = vx * vx + vy * vy;
      inv_h = rx * vy - ry * vx;
      inv_energy = 0.5 * inv_v2 - mu / inv_r;
      inv_a = 1 / (2 / inv_r - inv_v2 / mu);
      inv_rv = rx * vx + ry * vy;
      inv_ex = ((inv_v2 - mu / inv_r) * rx - inv_rv * vx) / mu;
      inv_ey = ((inv_v2 - mu / inv_r) * ry - inv_rv * vy) / mu;
      inv_ecc = sqrt(inv_ex * inv_ex + inv_ey * inv_ey);
      der(vx) = -mu * rx / (inv_r ^ 3);
      der(vy) = -mu * ry / (inv_r ^ 3);
    end SatelliteOrbit2D;
    "#;

    let json = compile_source_in_session(&mut session, source, "SatelliteOrbit2D")
        .expect("compile should succeed for orbit model");
    let result: serde_json::Value =
        serde_json::from_str(&json).expect("compile should return valid JSON");

    let native_y = result
        .get("dae_native")
        .and_then(|d| d.get("y"))
        .and_then(|y| y.as_object())
        .expect("dae_native.y should exist for orbit model");
    assert!(
        native_y.contains_key("inv_r"),
        "native dae should include algebraic variable inv_r, got keys: {:?}",
        native_y.keys().collect::<Vec<_>>()
    );

    let prepared = result
        .get("dae_prepared")
        .and_then(|d| d.as_object())
        .expect("dae_prepared should exist");
    let observables = prepared
        .get("__rumoca_observables")
        .and_then(|v| v.as_array())
        .expect("dae_prepared.__rumoca_observables should exist");
    assert!(
        !observables.is_empty(),
        "expected prepared dae to retain at least one observable"
    );

    let observable_names: std::collections::HashSet<String> = observables
        .iter()
        .filter_map(|v| v.get("name").and_then(|n| n.as_str()).map(str::to_string))
        .collect();
    for expected in [
        "inv_r",
        "inv_v2",
        "inv_h",
        "inv_energy",
        "inv_a",
        "inv_rv",
        "inv_ex",
        "inv_ey",
        "inv_ecc",
    ] {
        assert!(
            observable_names.contains(expected),
            "missing expected retained observable `{expected}`; got: {:?}",
            observable_names
        );
    }
}

#[test]
fn test_render_template_preserves_prepared_observables_from_json_context() {
    let mut session = Session::default();
    let source = r#"
    model SatelliteOrbit2D
      parameter Real mu = 398600.4418;
      parameter Real r0 = 7000;
      parameter Real v0 = sqrt(mu / r0);
      Real rx(start = r0, fixed = true);
      Real ry(start = 0, fixed = true);
      Real vx(start = 0, fixed = true);
      Real vy(start = v0, fixed = true);
      Real inv_r;
      Real inv_v2;
      Real inv_h;
      Real inv_energy;
      Real inv_a;
      Real inv_rv;
      Real inv_ex;
      Real inv_ey;
      Real inv_ecc;
    equation
      der(rx) = vx;
      der(ry) = vy;
      inv_r = sqrt(rx * rx + ry * ry);
      inv_v2 = vx * vx + vy * vy;
      inv_h = rx * vy - ry * vx;
      inv_energy = 0.5 * inv_v2 - mu / inv_r;
      inv_a = 1 / (2 / inv_r - inv_v2 / mu);
      inv_rv = rx * vx + ry * vy;
      inv_ex = ((inv_v2 - mu / inv_r) * rx - inv_rv * vx) / mu;
      inv_ey = ((inv_v2 - mu / inv_r) * ry - inv_rv * vy) / mu;
      inv_ecc = sqrt(inv_ex * inv_ex + inv_ey * inv_ey);
      der(vx) = -mu * rx / (inv_r ^ 3);
      der(vy) = -mu * ry / (inv_r ^ 3);
    end SatelliteOrbit2D;
    "#;

    let compiled = compile_source_in_session(&mut session, source, "SatelliteOrbit2D")
        .expect("compile should succeed for orbit model");
    let parsed: serde_json::Value =
        serde_json::from_str(&compiled).expect("compile should return valid JSON");
    let prepared = parsed
        .get("dae_prepared")
        .expect("compile response should contain dae_prepared");

    let rendered = render_template(
        &prepared.to_string(),
        "{% for o in dae.__rumoca_observables %}{{ o.name }}\n{% endfor %}",
    )
    .expect("render_template should succeed with JSON context");

    for expected in [
        "inv_r",
        "inv_v2",
        "inv_h",
        "inv_energy",
        "inv_a",
        "inv_rv",
        "inv_ex",
        "inv_ey",
        "inv_ecc",
    ] {
        assert!(
            rendered.lines().any(|line| line.trim() == expected),
            "expected rendered observables to contain `{expected}`, got:\n{rendered}"
        );
    }
}

#[test]
fn test_lsp_document_symbols_wrapper_returns_nested_outline() {
    let source = r#"
model Outline
  parameter Real k = 1;
  Real x;
equation
  der(x) = -k * x;
end Outline;
"#;

    let symbols_json =
        lsp_document_symbols(source).expect("document symbols wrapper should serialize");
    let symbols: Option<lsp_types::DocumentSymbolResponse> =
        serde_json::from_str(&symbols_json).expect("document symbols JSON should decode");
    let Some(lsp_types::DocumentSymbolResponse::Nested(top_level)) = symbols else {
        panic!("expected nested document symbols response");
    };
    let outline = top_level
        .iter()
        .find(|symbol| symbol.name == "Outline")
        .expect("expected top-level model symbol");
    let children = outline
        .children
        .as_ref()
        .expect("expected grouped child symbols");
    assert!(
        children.iter().any(|symbol| symbol.name == "Parameters"),
        "expected grouped parameter symbols in outline: {children:?}"
    );
    assert!(
        children.iter().any(|symbol| symbol.name == "Variables"),
        "expected grouped variable symbols in outline: {children:?}"
    );
    assert!(
        children.iter().any(|symbol| symbol.name == "Equations"),
        "expected equations summary in outline: {children:?}"
    );
}

#[test]
fn test_lsp_semantic_token_legend_wrapper_exposes_expected_entries() {
    let legend_json =
        lsp_semantic_token_legend().expect("semantic token legend wrapper should serialize");
    let legend: lsp_types::SemanticTokensLegend =
        serde_json::from_str(&legend_json).expect("semantic token legend JSON should decode");
    assert!(
        legend
            .token_types
            .contains(&lsp_types::SemanticTokenType::KEYWORD),
        "expected keyword token type in semantic token legend"
    );
    assert!(
        legend
            .token_types
            .contains(&lsp_types::SemanticTokenType::FUNCTION),
        "expected function token type in semantic token legend"
    );
    assert!(
        legend
            .token_modifiers
            .contains(&lsp_types::SemanticTokenModifier::DECLARATION),
        "expected declaration modifier in semantic token legend"
    );
}

#[test]
fn test_lsp_semantic_tokens_wrapper_highlights_keywords_and_functions() {
    let source = r#"
model Ball
  Real x(start=1);
  Real v(start=0);
equation
  der(x) = v;
  der(v) = -9.81;
  x = sin(v);
  when x < 0 then
    reinit(v, -0.6 * pre(v));
  end when;
end Ball;
"#;

    let legend_json =
        lsp_semantic_token_legend().expect("semantic token legend wrapper should serialize");
    let legend: lsp_types::SemanticTokensLegend =
        serde_json::from_str(&legend_json).expect("semantic token legend JSON should decode");
    let keyword_type = legend
        .token_types
        .iter()
        .position(|token_type| *token_type == lsp_types::SemanticTokenType::KEYWORD)
        .expect("keyword token type should be present") as u32;
    let function_type = legend
        .token_types
        .iter()
        .position(|token_type| *token_type == lsp_types::SemanticTokenType::FUNCTION)
        .expect("function token type should be present") as u32;

    let tokens_json =
        lsp_semantic_tokens(source).expect("semantic tokens wrapper should serialize");
    let tokens: Option<lsp_types::SemanticTokensResult> =
        serde_json::from_str(&tokens_json).expect("semantic tokens JSON should decode");
    let Some(lsp_types::SemanticTokensResult::Tokens(tokens)) = tokens else {
        panic!("expected full semantic tokens payload");
    };
    let decoded = decode_semantic_tokens(&tokens.data);
    assert!(
        decoded.iter().any(|(line, col, len, token_type)| {
            *token_type == keyword_type && lexeme_at(source, *line, *col, *len) == "reinit"
        }),
        "expected `reinit` to be classified as a keyword semantic token"
    );
    assert!(
        decoded.iter().any(|(line, col, len, token_type)| {
            *token_type == function_type && lexeme_at(source, *line, *col, *len) == "sin"
        }),
        "expected `sin` to be classified as a function semantic token"
    );
}

#[test]
fn test_compile_to_json_recovers_after_syntax_diagnostics() {
    let mut session = Session::default();
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

    let diags_json = lsp_diagnostics_in_session(&mut session, invalid)
        .expect("diagnostics should still return syntax errors");
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

    let json = compile_source_in_session(&mut session, valid, "Ball")
        .expect("compile should recover after diagnostics");
    let result: serde_json::Value =
        serde_json::from_str(&json).expect("compile should return valid JSON");
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
fn test_simulate_model_wrapper_returns_time_series_payload() {
    let _guard = session_test_guard();
    clear_source_root_cache();

    let source = r#"
    model Decay
      Real x(start=1, fixed=true);
    equation
      der(x) = -x;
    end Decay;
    "#;

    let json = simulate_model(source, "Decay", 0.2, 0.1, "auto")
        .expect("simulate_model wrapper should return simulation output");
    let simulation: serde_json::Value =
        serde_json::from_str(&json).expect("simulation payload should be valid JSON");
    let payload = simulation
        .get("payload")
        .expect("simulation output should include nested payload");
    let names = payload
        .get("names")
        .and_then(|value| value.as_array())
        .expect("simulation payload should include names");
    let all_data = payload
        .get("allData")
        .and_then(|value| value.as_array())
        .expect("simulation payload should include allData columns");
    let times = all_data
        .first()
        .and_then(|value| value.as_array())
        .expect("simulation payload should include time samples in allData[0]");
    let data = &all_data[1..];

    assert!(
        !times.is_empty(),
        "expected simulation payload to include sampled times"
    );
    assert!(
        names.iter().any(|value| value.as_str() == Some("x")),
        "expected simulation payload to include state name `x`: {simulation:?}"
    );
    assert_eq!(
        data.len(),
        names.len(),
        "expected one data column per variable after the time column"
    );
    assert!(
        data.iter().all(|series| {
            series
                .as_array()
                .is_some_and(|samples| samples.len() == times.len())
        }),
        "expected each data series to align with the sampled times"
    );
    assert_eq!(
        payload.get("nStates").and_then(|value| value.as_u64()),
        Some(1)
    );

    clear_source_root_cache();
}

#[test]
fn test_wasm_lsp_completion_keeps_local_members_on_ast_fast_path() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    reset_session_cache_stats();

    let source = r#"
    model Plane
      Real x, y, theta;
    equation
      der(x) = cos(theta);
      der(y) = sin(theta);
      der(theta) = 1;
    end Plane;

    model Sim
      Plane p1, p2;
    equation
      p1.x = 1;
    end Sim;
    "#;

    let before = session_cache_stats();
    let first_json = lsp_completion(source, 12, "      p1.".len() as u32).expect("cold completion");
    let after_first = session_cache_stats();
    let first_delta = after_first.delta_since(before);
    let first_items: Vec<serde_json::Value> =
        serde_json::from_str(&first_json).expect("completion payload should be valid JSON");
    assert!(
        first_items
            .iter()
            .any(|item| item.get("label").and_then(|v| v.as_str()) == Some("x")),
        "expected member completion for x, got: {first_items:?}"
    );
    assert_eq!(
        first_delta.semantic_navigation_builds, 0,
        "local member completion should stay off semantic navigation"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "completion should avoid populating the standard resolved session"
    );

    let second_json =
        lsp_completion(source, 12, "      p1.".len() as u32).expect("warm completion");
    let second_delta = session_cache_stats().delta_since(after_first);
    let second_items: Vec<serde_json::Value> =
        serde_json::from_str(&second_json).expect("completion payload should be valid JSON");
    assert_eq!(
        first_items, second_items,
        "warm completion should preserve completion results"
    );
    assert_eq!(
        second_delta.semantic_navigation_builds, 0,
        "warm completion should keep using the AST fast path"
    );
    assert!(
        !singleton_session_has_standard_resolved_cached(),
        "warm completion should still avoid the standard resolved session"
    );

    clear_source_root_cache();
}

#[test]
fn test_wasm_lsp_diagnostics_reuse_semantic_diagnostics_cache() {
    let _guard = session_test_guard();
    clear_source_root_cache();
    reset_session_cache_stats();

    let source = r#"
    model M
      Real x(start=0);
    equation
      der(x) = -x;
    end M;
    "#;

    let before = session_cache_stats();
    let first_json = lsp_diagnostics(source).expect("cold diagnostics");
    let after_first = session_cache_stats();
    let first_delta = after_first.delta_since(before);
    let first_diagnostics: Vec<serde_json::Value> =
        serde_json::from_str(&first_json).expect("diagnostics payload should be valid JSON");
    assert!(
        first_diagnostics.is_empty(),
        "expected clean diagnostics for valid model, got: {first_diagnostics:?}"
    );
    assert!(
        first_delta.interface_semantic_diagnostics_builds >= 1,
        "cold diagnostics should build interface-stage diagnostics artifacts"
    );
    assert!(
        first_delta.body_semantic_diagnostics_builds >= 1,
        "cold diagnostics should build body-stage diagnostics artifacts"
    );
    assert!(
        first_delta.model_stage_semantic_diagnostics_builds >= 1,
        "cold diagnostics should build model-stage diagnostics artifacts"
    );

    let second_json = lsp_diagnostics(source).expect("warm diagnostics");
    let second_delta = session_cache_stats().delta_since(after_first);
    let second_diagnostics: Vec<serde_json::Value> =
        serde_json::from_str(&second_json).expect("diagnostics payload should be valid JSON");
    assert_eq!(
        first_diagnostics, second_diagnostics,
        "warm diagnostics should preserve the diagnostics payload"
    );
    assert_eq!(
        second_delta.interface_semantic_diagnostics_builds, 0,
        "warm diagnostics should not rebuild interface-stage diagnostics artifacts"
    );
    assert_eq!(
        second_delta.body_semantic_diagnostics_builds, 0,
        "warm diagnostics should not rebuild body-stage diagnostics artifacts"
    );
    assert_eq!(
        second_delta.model_stage_semantic_diagnostics_builds, 0,
        "warm diagnostics should not rebuild model-stage diagnostics artifacts"
    );
    assert!(
        second_delta.interface_semantic_diagnostics_cache_hits >= 1,
        "warm diagnostics should reuse the interface-stage diagnostics cache"
    );
    assert!(
        second_delta.body_semantic_diagnostics_cache_hits >= 1,
        "warm diagnostics should reuse the body-stage diagnostics cache"
    );
    assert!(
        second_delta.model_stage_semantic_diagnostics_cache_hits >= 1,
        "warm diagnostics should reuse the model-stage diagnostics cache"
    );

    clear_source_root_cache();
}
