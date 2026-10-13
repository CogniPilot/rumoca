//! Exact original full-capacity function closure executes emitted native WASM.
const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/OriginalBuildPairs.mo"
);

#[test]
fn original_build_pairs_executes_actual_native_wasm() {
    super::record_root_reads::run_cases(
        SOURCE,
        "OriginalBuildPairs",
        &["y", "activeCount", "validValue"],
        &[
            (true, [1.0, 2.0], &[774.0, 2.0, 1.0]),
            (false, [1.0, 2.0], &[768.0, 0.0, 1.0]),
            (true, [0.0, 0.0], &[0.0, 0.0, 0.0]),
            (true, [4.0, 5.0], &[810.0, 2.0, 1.0]),
            (true, [1.0, 2.0], &[774.0, 2.0, 1.0]),
        ],
    );
}
