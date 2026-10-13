//! Whole definitions dominate each selected per-iteration slice-update base.

use super::assert_runtime_call_retained;
use rumoca::{Compiler, CompilerError};

const SOURCE: &str =
    include_str!("../../fixtures/function_guarded_loops/WholeDefinedSliceScratch.mo");

#[test]
fn a_whole_defined_loop_local_accepts_its_later_partial_slice_writes() {
    let compiled = Compiler::new()
        .model("WholeDefinedSliceScratch")
        .compile_str(SOURCE, "WholeDefinedSliceScratch.mo")
        .expect("each reached slice base follows a whole definition in the same iteration");
    assert_runtime_call_retained(&compiled.dae);
}

#[test]
fn a_partial_loop_local_refuses_a_reached_undefined_element_read() {
    let source = SOURCE
        .replace("        J := zeros(2,3);\n", "")
        .replace("{values[i],2*values[i]}", "{values[i]+J[1,3],2*values[i]}");
    assert_ne!(source, SOURCE);
    let error = Compiler::new()
        .model("WholeDefinedSliceScratch")
        .compile_str(&source, "UndefinedSliceScratch.mo")
        .expect_err("the third column is read before any source definition");
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
        .find(|failure| failure.model_name == "WholeDefinedSliceScratch")
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
        failure.error.contains("SliceScratch.Sum") && failure.error.contains("J"),
        "{error}"
    );
    let label = failure
        .primary_label
        .as_ref()
        .expect("the undefined source read has a primary owner");
    assert!(label.primary);
    let (_, issued) = source_map
        .as_ref()
        .unwrap()
        .get_source(label.span.source)
        .unwrap();
    assert_eq!(issued, source);
    let line = "J[:,3] := {values[i]+J[1,3],2*values[i]};";
    let start = source.find(line).unwrap();
    assert!(
        label.span.start.0 >= start && label.span.end.0 <= start + line.len(),
        "{error}"
    );
}

#[test]
fn an_empty_slice_scratch_domain_needs_no_loop_entry_value() {
    let source = format!(
        "{SOURCE}\nmodel EmptySliceScratch\n  input Boolean requested = true;\n  Real y = SliceScratch.Sum(zeros(0),requested);\nend EmptySliceScratch;\n"
    );
    Compiler::new()
        .model("EmptySliceScratch")
        .compile_str(&source, "EmptySliceScratch.mo")
        .expect("an empty loop does not read or manufacture a scratch entry");
}

#[test]
fn whole_defined_scratch_accepts_a_partial_multi_output_receiver() {
    let source = SOURCE.replace(
        "  function Sum\n",
        "  function Pair\n    input Real x;\n    output Real column[2];\n    output Real unused;\n  algorithm\n    column := {x,2*x};\n    unused := x;\n  end Pair;\n  function Sum\n",
    ).replace(
        "        J[:,3] := {values[i],2*values[i]};",
        "        (J[:,3], ) := SliceScratch.Pair(values[i]);",
    );
    assert_ne!(source, SOURCE);
    let compiled = Compiler::new()
        .model("WholeDefinedSliceScratch")
        .compile_str(&source, "WholeDefinedTupleSliceScratch.mo")
        .expect("the partial receiving slot follows the same whole-definition proof");
    assert_runtime_call_retained(&compiled.dae);
}
