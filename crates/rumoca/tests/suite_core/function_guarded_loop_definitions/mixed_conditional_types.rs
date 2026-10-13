//! Whole conditional element type remains an obligation before selected writes.
use rumoca::Compiler;

fn source(destination: &str, selector: &str) -> String {
    format!(
        "function MixedConditional\n input Boolean flag;\n output {destination} y;\nalgorithm\n y := if flag then 1 else 2.0;\nend MixedConditional;\nmodel MixedConditionalModel\n input Boolean requested = true;\n {destination} y = MixedConditional({selector});\nend MixedConditionalModel;\n"
    )
}

fn refuse_integer(selector: &str) {
    let error = Compiler::new()
        .model("MixedConditionalModel")
        .compile_str(&source("Integer", selector), "MixedConditional.mo")
        .expect_err("a Real common conditional result cannot be assigned to Integer");
    let rumoca::CompilerError::CompileDiagnosticsError {
        failures,
        source_map,
        ..
    } = &error
    else {
        panic!("expected typed construction diagnostics: {error}");
    };
    let failure = failures
        .iter()
        .find(|failure| failure.model_name == "MixedConditionalModel")
        .unwrap();
    assert_eq!(
        failure
            .phase
            .as_ref()
            .map(|phase| phase.to_string())
            .as_deref(),
        Some("ToDae")
    );
    assert_eq!(failure.error_code.as_deref(), Some("ED020"));
    assert_eq!(
        failure.error,
        "canonical DAE construction rejected an invalid operation: expression shape mismatch"
    );
    let label = failure
        .primary_label
        .as_ref()
        .expect("canonical type refusal retains its source provenance");
    assert!(label.primary);
    let (_, issued) = source_map
        .as_ref()
        .unwrap()
        .get_source(label.span.source)
        .unwrap();
    assert_eq!(issued, source("Integer", selector));
    assert!(label.span.start.0 < label.span.end.0);
    assert!(issued.get(label.span.start.0..label.span.end.0).is_some());
}

#[test]
fn mixed_conditional_integer_destination_refuses_runtime_selector() {
    refuse_integer("requested");
}

#[test]
fn mixed_conditional_integer_destination_refuses_constant_true_selector() {
    refuse_integer("true");
}

#[test]
fn mixed_conditional_integer_destination_refuses_constant_false_selector() {
    refuse_integer("false");
}

#[test]
fn mixed_conditional_real_destination_retains_its_runtime_call() {
    let compiled = Compiler::new()
        .model("MixedConditionalModel")
        .compile_str(&source("Real", "requested"), "MixedConditional.mo")
        .unwrap_or_else(|error| panic!("Real accepts the common numeric result: {error}"));
    super::assert_runtime_call_retained(&compiled.dae);
}
