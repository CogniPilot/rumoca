//! The source-owned whole record is the selected loop's actual carried value.
use super::assert_runtime_call_retained;
use rumoca::{Compiler, CompilerError};
const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/RecordLoopEntry.mo");
#[test]
fn conditional_whole_record_fields_remain_readable_as_loop_carried_entries() {
    let compiled = Compiler::new()
        .model("RecordLoopEntry")
        .compile_str(SOURCE, "RecordLoopEntry.mo")
        .expect("each selected loop entry reads its actual preceding whole record");
    assert_runtime_call_retained(&compiled.dae);
}
#[test]
fn empty_record_loop_preserves_the_preceding_selected_whole_value() {
    let source = SOURCE.replace("1:min(3,max(0,limit))", "1:0");
    assert_ne!(source, SOURCE);
    Compiler::new()
        .model("RecordLoopEntry")
        .compile_str(&source, "EmptyRecordLoopEntry.mo")
        .expect("the empty transition leaves the prior whole record available to its suffix");
}
#[test]
fn partial_record_elements_cannot_supply_a_whole_carried_entry() {
    let source = SOURCE.replace("working := State({Edge(flags[1],weights[1]),Edge(flags[2],weights[2]),Edge(flags[3],weights[3])});", "working.edges[1] := Edge(flags[1],weights[1]);");
    assert_ne!(source, SOURCE);
    source_refused(&source, "PartialRecordLoopEntry.mo");
}
#[test]
fn a_reenabled_guard_cannot_read_an_absent_record_loop_entry() {
    let source = SOURCE.replace(
        "    end if;\n    if valid then\n      for slot",
        "    end if;\n    valid := true;\n    if valid then\n      for slot",
    );
    assert_ne!(source, SOURCE);
    source_refused(&source, "UnsafeRecordLoopEntry.mo");
}
fn source_refused(source: &str, name: &str) {
    let error = Compiler::new()
        .model("RecordLoopEntry")
        .compile_str(source, name)
        .expect_err("an undefined carried record is source-refused");
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
        .find(|failure| failure.model_name == "RecordLoopEntry")
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
        failure.error.contains("CarriedRecord.Scan") && failure.error.contains("working"),
        "{error}"
    );
    let label = failure
        .primary_label
        .as_ref()
        .expect("the reached undefined source has an owner");
    assert!(label.primary);
    let (_, issued) = source_map
        .as_ref()
        .unwrap()
        .get_source(label.span.source)
        .unwrap();
    assert_eq!(issued, source);
    let read = &source[label.span.start.0..label.span.end.0];
    assert!(
        read.contains("working")
            || source[..label.span.start.0]
                .lines()
                .next_back()
                .unwrap_or("")
                .contains("working"),
        "{error}"
    );
}
