//! A selected partial update reads its original whole tensor base.
use rumoca::Compiler;
const SOURCE: &str = include_str!("../../fixtures/function_guarded_loops/PartialBase.mo");

#[test]
fn selected_partial_body_update_retains_its_whole_base() {
    compile("PartialBaseBody");
}

#[test]
fn selected_partial_fold_update_retains_its_whole_base() {
    compile("PartialBaseFold");
}

fn compile(model: &str) {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "PartialBase.mo")
        .unwrap_or_else(|error| panic!("selected partial update source admission: {error}"));
    super::assert_runtime_call_retained(&compiled.dae);
}
