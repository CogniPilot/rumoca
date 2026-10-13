//! Normalized exhaustive tuple/scalar definitions need no incoming scratch.

use super::assert_runtime_call_retained;
use rumoca::{Compiler, CompilerError};

const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/ExhaustiveLoopFlow.mo");

#[test]
fn exhaustive_tuple_and_loop_arm_definitions_need_no_entry_value() {
    let compiled = Compiler::new()
        .model("ExhaustiveLoopFlow")
        .compile_str(SOURCE, "ExhaustiveLoopFlow.mo")
        .expect("both selected arms define the scratch before its reached read");
    assert_runtime_call_retained(&compiled.dae);
}

#[test]
fn a_missing_exhaustive_arm_does_not_define_the_scratch() {
    let source = SOURCE.replace("            residualValid := true;\n", "");
    assert_ne!(source, SOURCE);
    assert_undefined(&source);
}

#[test]
fn a_changed_mutable_guard_cannot_reuse_a_previous_selected_definition() {
    let source = SOURCE
        .replace("    Boolean residualValid;", "    Boolean residualValid;\n    Boolean chosen;")
        .replace("    y := 0;", "    y := 0;\n    chosen := usePair;")
        .replace("          if usePair then", "          if chosen then")
        .replace("            residualValid := true;\n", "")
        .replace("          if residualValid then", "          chosen := not chosen;\n          if chosen then\n            if residualValid then")
        .replace("            y := y + squaredResidual;\n          end if;", "            y := y + squaredResidual;\n            end if;\n          end if;");
    assert_ne!(source, SOURCE);
    assert_undefined(&source);
}

#[test]
fn a_child_binder_selected_write_cannot_define_its_reached_entry_read() {
    let source = SOURCE
        .replace("            squaredResidual := 0;\n", "")
        .replace(
            "            for axis in 1:2 loop\n",
            "            for axis in 1:2 loop\n              if axis == 2 then\n                squaredResidual := 0;\n              end if;\n",
        );
    assert_ne!(source, SOURCE);
    assert_undefined_target(&source, "squaredResidual");
}

fn assert_undefined(source: &str) {
    assert_undefined_target(source, "residualValid");
}

fn assert_undefined_target(source: &str, target: &str) {
    let error = Compiler::new()
        .model("ExhaustiveLoopFlow")
        .compile_str(source, "UndefinedExhaustiveLoopFlow.mo")
        .expect_err("a reached read without an assigned arm must remain refused");
    let CompilerError::CompileDiagnosticsError {
        failures,
        source_map,
        ..
    } = &error
    else {
        panic!("expected checked source diagnostics: {error}");
    };
    let failure = failures
        .iter()
        .find(|failure| failure.model_name == "ExhaustiveLoopFlow")
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
        failure.error.contains("ExhaustiveFlow.Search") && failure.error.contains(target),
        "{error}"
    );
    let label = failure
        .primary_label
        .as_ref()
        .expect("the source read owns its label");
    assert!(label.primary);
    let (_, issued) = source_map
        .as_ref()
        .unwrap()
        .get_source(label.span.source)
        .unwrap();
    assert_eq!(issued, source);
    assert!(
        source
            .get(label.span.start.0..label.span.end.0)
            .unwrap()
            .contains(target),
        "{error}"
    );
}
