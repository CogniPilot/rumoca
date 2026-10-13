//! Original nested-loop scale flow retains the checked loop-exit payload.
use rumoca::Compiler;

#[test]
fn assigned_loop_exit_scale_remains_available_to_later_source_regions() {
    let source = include_str!("../../fixtures/function_guarded_loops/AssignedLoopExitScale.mo");
    let compiled = Compiler::new()
        .model("AssignedLoopExitScale")
        .compile_str(source, "AssignedLoopExitScale.mo")
        .unwrap_or_else(|error| panic!("assigned loop-exit source admission: {error}"));
    super::assert_runtime_call_retained(&compiled.dae);
    rumoca_phase_solve::lower_solve_problem(&compiled.dae)
        .unwrap_or_else(|error| panic!("assigned loop-exit Solve admission: {error}"));
}
