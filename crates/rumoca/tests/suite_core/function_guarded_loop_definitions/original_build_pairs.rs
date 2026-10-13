//! Exact original record and function bodies, with the original capacities.
use rumoca::Compiler;

#[test]
fn original_build_pairs_record_retains_checked_loop_exit_payloads() {
    let source = include_str!("../../fixtures/function_guarded_loops/OriginalBuildPairs.mo");
    let compiled = Compiler::new()
        .model("OriginalBuildPairs")
        .compile_str(source, "OriginalBuildPairs.mo")
        .unwrap_or_else(|error| panic!("original BuildPairs source admission: {error}"));
    super::assert_runtime_call_retained(&compiled.dae);
    rumoca_phase_solve::lower_solve_problem(&compiled.dae)
        .unwrap_or_else(|error| panic!("original BuildPairs Solve admission: {error}"));
}
