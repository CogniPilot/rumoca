//! MLS §11.2.1.1 checks old-state arguments independently of receiving slots.
use super::{assert_runtime_call_retained, undefined_source};
use rumoca::{Compiler, CompilerError};
const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/GuardedTupleReads.mo");
#[test]
fn selected_tuple_arguments_remain_readable_in_body_fold_and_record_assembly() {
    let compiled = Compiler::new()
        .model("GuardedTupleReads")
        .compile_str(SOURCE, "GuardedTupleReads.mo")
        .expect("selected old-state array arguments retain their source read proof");
    assert_runtime_call_retained(&compiled.dae);
}
#[test]
fn an_empty_tuple_fold_preserves_its_original_array_and_score() {
    let source = SOURCE.replace("for pass in 1:2 loop", "for pass in 1:0 loop");
    assert_ne!(source, SOURCE);
    Compiler::new()
        .model("GuardedTupleReads")
        .compile_str(&source, "EmptyTupleFold.mo")
        .expect("no tuple call runs in an empty fold");
}
#[test]
fn a_changed_guard_cannot_authorize_a_tuple_argument() {
    let source = SOURCE.replace(
        "    if child > 1 then",
        "    child := 2;\n    if child > 1 then",
    );
    assert_ne!(source, SOURCE);
    refused(&source, "GuardedTupleReads", "GuardedTuple.Body");
}
#[test]
fn omitted_receivers_still_require_a_complete_old_state_argument() {
    let source = SOURCE.replace("scratch := values;", "scratch[1,:] := values[1,:];");
    assert_ne!(source, SOURCE);
    refused(&source, "TupleOmitted", "GuardedTuple.Omitted");
}
#[test]
fn completely_omitted_tuple_results_keep_the_existing_statement_refusal() {
    let source = SOURCE.replace("(,score) := Pair(scratch);", "(,) := Pair(scratch);");
    assert_ne!(source, SOURCE);
    let error = Compiler::new()
        .model("TupleOmitted")
        .compile_str(&source, "NoTupleResults.mo")
        .expect_err("effect-only calls retain their existing unsupported source owner");
    undefined_source::assert_refusal(
        &error,
        &source,
        "TupleOmitted",
        "function call statement",
        "`GuardedTuple.Omitted` calls `Pair` as a statement without reading a result, which the canonical DAE has no owner for",
    );
}
fn refused(source: &str, model: &str, function: &str) {
    let error = Compiler::new()
        .model(model)
        .compile_str(source, "UnsafeTupleArgument.mo")
        .expect_err("a reached undefined array argument is source-refused with an omitted slot");
    let CompilerError::CompileDiagnosticsError { failures, .. } = &error else {
        panic!("expected typed source refusal: {error}")
    };
    let failure = failures
        .iter()
        .find(|failure| failure.model_name == model)
        .unwrap();
    assert!(
        failure.error.contains(function) && failure.error.contains("scratch"),
        "{error}"
    );
    let (owner, detail) = failure
        .error
        .strip_prefix("unsupported Flat semantic owner `")
        .unwrap()
        .split_once("`: ")
        .unwrap();
    assert!(
        matches!(
            owner,
            "function conditional" | "function element assignment"
        ),
        "{error}"
    );
    assert!(
        detail.contains("only some branches") || detail.contains("do not all have a definition"),
        "{error}"
    );
    undefined_source::assert_refusal(&error, source, model, owner, detail);
}
