//! A later accepted path cannot bypass the earlier record producer.

use super::assert_runtime_call_retained;
use rumoca::Compiler;

const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/RecordGatedFlow.mo");

#[test]
fn later_valid_checks_preserve_the_earlier_whole_record_definition() {
    let compiled = Compiler::new()
        .model("RecordGatedFlow")
        .compile_str(SOURCE, "RecordGatedFlow.mo")
        .expect("later checks execute only after the whole record producer");
    assert_runtime_call_retained(&compiled.dae);
}

#[test]
fn reenabled_valid_cannot_read_a_skipped_whole_record_producer() {
    let source = SOURCE.replace(
        "    end if;\n    if valid then\n      valid := accepted;",
        "    end if;\n    valid := accepted;\n    if valid then\n      valid := accepted;",
    );
    assert_ne!(source, SOURCE);
    let error = Compiler::new()
        .model("RecordGatedFlow")
        .compile_str(&source, "ReenabledRecordGatedFlow.mo")
        .expect_err("requested=false and accepted=true reaches an undefined record field");
    let rumoca::CompilerError::CompileDiagnosticsError {
        failures,
        source_map,
        ..
    } = &error
    else {
        panic!("expected checked source diagnostics: {error}");
    };
    let failure = failures
        .iter()
        .find(|failure| failure.model_name == "RecordGatedFlow")
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
        failure.error.contains("RecordGate.Correct")
            && failure.error.contains("candidate__referencePosition"),
        "{error}"
    );
    let label = failure
        .primary_label
        .as_ref()
        .expect("the reached read has a source owner");
    assert!(label.primary);
    let (_, issued) = source_map
        .as_ref()
        .unwrap()
        .get_source(label.span.source)
        .unwrap();
    assert_eq!(issued, source);
    let line = "valid := valid and abs(candidate.referencePosition[i]-candidate.position[i]) <= 1;";
    let start = source.find(line).unwrap();
    assert!(
        label.span.start.0 >= start && label.span.end.0 <= start + line.len(),
        "{error}"
    );
}
