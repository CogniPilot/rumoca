//! Source-loop captures retain whole slot definitions behind their source guard.
use rumoca::Compiler;
const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/SelectedSlots.mo");

#[test]
fn selected_nested_slot_subscript_retains_both_original_definitions() {
    let compiled = Compiler::new()
        .model("SelectedSlots")
        .compile_str(SOURCE, "SelectedSlots.mo")
        .unwrap_or_else(|error| panic!("selected slot source admission: {error}"));
    super::assert_runtime_call_retained(&compiled.dae);
}

#[test]
fn refined_validity_preserves_selected_slot_definitions_at_nested_entry() {
    let source = include_str!("../../fixtures/function_guarded_loops/RefinedSelectedSlots.mo");
    let compiled = Compiler::new()
        .model("RefinedSelectedSlots")
        .compile_str(source, "RefinedSelectedSlots.mo")
        .unwrap_or_else(|error| panic!("refined selected slot source admission: {error}"));
    super::assert_runtime_call_retained(&compiled.dae);
}
