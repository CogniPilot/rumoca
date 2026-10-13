//! Serial guarded scalar states remain owned by their original source regions.
use rumoca::Compiler;

#[test]
fn serial_guarded_state_crosses_later_selected_source_regions() {
    let source = include_str!("../../fixtures/function_guarded_loops/SerialGuardedState.mo");
    let compiled = Compiler::new()
        .model("SerialGuardedState")
        .compile_str(source, "SerialGuardedState.mo")
        .unwrap_or_else(|error| panic!("serial guarded source admission: {error}"));
    super::assert_runtime_call_retained(&compiled.dae);
    rumoca_phase_solve::lower_solve_problem(&compiled.dae)
        .unwrap_or_else(|error| panic!("serial guarded Solve admission: {error}"));
}
