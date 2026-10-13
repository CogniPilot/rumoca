//! Actual emitted calls retain selected generated capture leaf reads.
const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/SelectedCapture.mo"
);

#[test]
fn nested_generated_capture_executes_actual_native_wasm() {
    super::record_root_reads::run_cases(
        SOURCE,
        "SelectedCapture",
        &["y"],
        &[
            (true, [1.0, 2.0], &[3.0]),
            (false, [1.0, 2.0], &[0.0]),
            (true, [4.0, 5.0], &[9.0]),
            (true, [1.0, 2.0], &[3.0]),
        ],
    );
}
