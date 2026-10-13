//! Same source-bound native product executes selected Integer slot subscripts.
use super::record_root_reads::{SourceCase, run_source_cases};
const SOURCE: &str = include_str!(
    "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/SelectedSlots.mo"
);

#[test]
fn selected_nested_slot_subscripts_execute_actual_native_wasm() {
    let cases = [
        (true, [1.0, 2.0], [1, 2], 6.0),
        (false, [1.0, 2.0], [1, 2], 0.0),
        (false, [1.0, 2.0], [0, 99], 0.0),
        (true, [4.0, 5.0], [2, 1], 12.0),
        (true, [1.0, 2.0], [1, 2], 6.0),
    ];
    run_slot_cases(SOURCE, "SelectedSlots", &cases);
}

fn run_slot_cases(source: &str, model: &str, cases: &[(bool, [f64; 2], [i64; 2], f64)]) {
    let cases = cases
        .iter()
        .map(|(requested, values, slots, expected)| SourceCase {
            requested: Some(*requested),
            values: *values,
            slots: Some(*slots),
            expected: std::slice::from_ref(expected),
        })
        .collect::<Vec<_>>();
    run_source_cases(source, model, &["y"], &cases);
}

#[test]
fn refined_validity_slot_subscripts_execute_actual_native_wasm() {
    let source = include_str!(
        "../../../../../../../../crates/rumoca/tests/fixtures/function_guarded_loops/RefinedSelectedSlots.mo"
    );
    let cases = [
        (true, [1.0, 2.0], [1, 2], 6.0),
        (false, [1.0, 2.0], [1, 2], 0.0),
        (false, [1.0, 2.0], [0, 99], 0.0),
        (true, [4.0, 5.0], [1, 2], 12.0),
        (true, [-1.0, 2.0], [0, 99], 0.0),
        (true, [11.0, 2.0], [0, 99], 0.0),
        (true, [1.0, 2.0], [1, 2], 6.0),
    ];
    run_slot_cases(source, "RefinedSelectedSlots", &cases);
}
