//! An emitted capture marker keeps the source-selected child entry.
use super::assert_runtime_call_retained;
use rumoca::{Compiler, CompilerError};
const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/SelectedNearest.mo");
#[test]
fn nested_selected_nearest_keeps_its_definition_before_the_marker_owned_child() {
    let compiled = Compiler::new()
        .model("SelectedNearest")
        .compile_str(SOURCE, "SelectedNearest.mo")
        .expect("the source-selected entry owns the preceding nearest definition");
    assert_runtime_call_retained(&compiled.dae);
}
#[test]
fn empty_selected_child_preserves_its_preceding_nearest_definition() {
    let source = SOURCE.replace("for word in 1:2 loop", "for word in 1:0 loop");
    assert_ne!(source, SOURCE);
    Compiler::new()
        .model("SelectedNearest")
        .compile_str(&source, "EmptySelectedNearest.mo")
        .expect("zero child iterations preserve the actual preceding nearest value");
}
#[test]
fn a_missing_selected_nearest_definition_is_source_refused() {
    let source = SOURCE.replace("nearest := 5;", "if accepted then nearest := 5; end if;");
    assert_ne!(source, SOURCE);
    refused(&source);
}
#[test]
fn changing_a_guard_cannot_supply_an_absent_nearest_definition() {
    let source = SOURCE.replace(
        "nearest := 5;",
        "gate := accepted; if gate then nearest := 5; end if; gate := true;",
    );
    assert_ne!(source, SOURCE);
    refused(&source);
}
fn refused(source: &str) {
    let error = Compiler::new()
        .model("SelectedNearest")
        .compile_str(source, "UnsafeSelectedNearest.mo")
        .expect_err("the reached incoming nearest read is undefined");
    let CompilerError::CompileDiagnosticsError {
        failures,
        source_map,
        ..
    } = &error
    else {
        panic!("expected typed source refusal: {error}")
    };
    let failure = failures
        .iter()
        .find(|failure| failure.model_name == "SelectedNearest")
        .unwrap();
    assert_eq!(
        failure
            .phase
            .as_ref()
            .map(|phase| phase.to_string())
            .as_deref(),
        Some("ToDae")
    );
    assert!(
        failure
            .error_code
            .as_deref()
            .is_some_and(|code| code.ends_with("ED019")),
        "{error}"
    );
    assert!(
        failure.error.contains("SelectedVocabulary.Scan") && failure.error.contains("nearest"),
        "{error}"
    );
    let label = failure
        .primary_label
        .as_ref()
        .expect("the undefined read retains its source owner");
    assert!(label.primary);
    let (_, issued) = source_map
        .as_ref()
        .unwrap()
        .get_source(label.span.source)
        .unwrap();
    assert_eq!(issued, source);
    assert!(
        source[label.span.start.0..label.span.end.0].contains("nearest"),
        "{error}"
    );
}
