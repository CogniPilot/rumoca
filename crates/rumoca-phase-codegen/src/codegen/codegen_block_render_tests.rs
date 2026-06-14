//! Tests for the multi-output scalar block renderer (`render_solve_block_*`).
//! Split out of `codegen_tests.rs` to keep that file under the SPEC_0021
//! file-size limit.

use super::*;

#[test]
fn test_render_solve_block_c_emits_multi_output_and_shares_registers() {
    // One program with two outputs that both read register 0 (a shared operand).
    // The block renderer must (a) write BOTH out[0] and out[1], and (b) compute
    // the shared register once as a temporary rather than re-expanding it.
    let programs = serde_json::json!([[
        {"LoadP": {"dst": 0, "index": 0}},
        {"Const": {"dst": 1, "value": 2.0}},
        {"Binary": {"dst": 2, "op": "Mul", "lhs": 0, "rhs": 1}},
        {"Binary": {"dst": 3, "op": "Add", "lhs": 0, "rhs": 2}},
        {"StoreOutput": {"src": 2}},
        {"StoreOutput": {"src": 3}}
    ]]);
    let template = r#"{{ render_solve_block_c(dae.programs, {"time": "m->time", "y": "Y({})", "p": "P({})"}, "out[{}] = {}") }}"#;
    let rendered =
        render_template_with_dae_json(&serde_json::json!({ "programs": programs }), template)
            .unwrap();

    assert!(rendered.contains("out[0] ="), "missing first output: {rendered}");
    assert!(rendered.contains("out[1] ="), "missing second output: {rendered}");
    // Register 0 is read twice, so it must be materialized once as a temp.
    assert!(
        rendered.contains("double __r"),
        "shared register should become a temporary: {rendered}"
    );
    assert_eq!(
        rendered.matches("P(0)").count(),
        1,
        "shared operand must be computed exactly once: {rendered}"
    );
}
