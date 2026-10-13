//! Partial tensor updates preserve their original selected whole base.
const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/PartialBase.mo"
);

#[test]
fn selected_partial_body_updates_execute_actual_native_wasm() {
    super::record_root_reads::run_cases(
        SOURCE,
        "PartialBaseBody",
        &["y"],
        &[
            (true, [1.0, 2.0], &[9.0]),
            (false, [1.0, 2.0], &[0.0]),
            (true, [4.0, 5.0], &[12.0]),
            (true, [1.0, 2.0], &[9.0]),
        ],
    );
}

#[test]
fn selected_partial_fold_updates_execute_actual_native_wasm() {
    super::record_root_reads::run_cases(
        SOURCE,
        "PartialBaseFold",
        &["y"],
        &[
            (true, [1.0, 2.0], &[18.0]),
            (false, [1.0, 2.0], &[0.0]),
            (true, [4.0, 5.0], &[24.0]),
            (true, [1.0, 2.0], &[18.0]),
        ],
    );
}
