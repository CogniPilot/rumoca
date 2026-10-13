//! Actual source-issued native calls preserve checked loop-exit scale values.
const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/AssignedLoopExitScale.mo"
);

#[test]
fn assigned_loop_exit_scale_executes_actual_native_wasm() {
    super::record_root_reads::run_cases(
        SOURCE,
        "AssignedLoopExitScale",
        &["y"],
        &[
            (true, [1.0, 2.0], &[8.0]),
            (false, [1.0, 2.0], &[0.0]),
            (true, [0.0, 0.0], &[4.0]),
            (true, [4.0, 5.0], &[20.0]),
            (true, [1.0, 2.0], &[8.0]),
        ],
    );
}
