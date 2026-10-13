//! Repeated record-array leaves exercise actual native call emission and execution.
use super::*;

const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/CompactCallTransfer.mo"
);

#[test]
fn repeated_record_array_integer_leaves_execute_actual_native_wasm() {
    record_root_reads::run_source_cases(
        SOURCE,
        "CompactCallTransfer",
        &["score"],
        &[
            record_root_reads::SourceCase {
                requested: Some(true),
                values: [1.0, 2.0],
                slots: Some([(1_i64 << 53) + 1, -(1_i64 << 53) - 1]),
                expected: &[3.0],
            },
            record_root_reads::SourceCase {
                requested: Some(false),
                values: [1.0, 2.0],
                slots: Some([i64::MAX, i64::MIN]),
                expected: &[-1.0],
            },
            record_root_reads::SourceCase {
                requested: Some(true),
                values: [4.0, 5.0],
                slots: Some([7, 8]),
                expected: &[9.0],
            },
            record_root_reads::SourceCase {
                requested: Some(true),
                values: [1.0, 2.0],
                slots: Some([(1_i64 << 53) + 1, -(1_i64 << 53) - 1]),
                expected: &[3.0],
            },
        ],
    );
}
