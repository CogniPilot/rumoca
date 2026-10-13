//! Nested capture leaves retain the selected source read owner.
use rumoca::Compiler;
const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/SelectedCapture.mo");

#[test]
fn nested_generated_capture_reads_a_selected_split_record_scalar() {
    let compiled = Compiler::new()
        .model("SelectedCapture")
        .compile_str(SOURCE, "SelectedCapture.mo")
        .unwrap_or_else(|error| panic!("selected capture source admission: {error}"));
    super::assert_runtime_call_retained(&compiled.dae);
}
