//! Render-time refusals of the ME projection view.

use rumoca_eval_solve::TargetIsolationProgram;

use super::block::isolation_kind;

/// An isolation the evaluator answers but no scalar program reproduces
/// refuses its block before any byte, and never interns an isolator.
///
/// `TargetIsolationProgram::Unrepresentable` arises only when an assignment
/// shape's materialization does not fit its row (a register range overflow or
/// a tensor-affine lane outside its source range), which no shape derived from
/// a lowered model produces today; this pins the refusal the renderer applies
/// if one ever does.
#[test]
fn an_unrepresentable_row_isolation_refuses_its_block() {
    let mut interned = 0;
    let mut intern = || {
        interned += 1;
        3
    };
    let error = isolation_kind(7, &TargetIsolationProgram::Unrepresentable, &mut intern)
        .expect_err("an unrepresentable isolation must refuse the block");
    assert_eq!(
        isolation_kind(
            7,
            &TargetIsolationProgram::Isolator(Vec::new()),
            &mut intern
        )
        .ok(),
        Some((2, 3)),
        "a materialized isolator is interned exactly once"
    );
    let message = error.to_string();
    assert!(
        message.contains("unsupported-feature:algebraic_projection")
            && message.contains("projection block 7")
            && message.contains("row isolation no scalar program reproduces"),
        "{message}"
    );
    for (program, kind) in [
        (TargetIsolationProgram::Unavailable, 0),
        (TargetIsolationProgram::OutputValue, 1),
    ] {
        assert_eq!(
            isolation_kind(7, &program, &mut intern).ok(),
            Some((kind, 0))
        );
    }
    assert_eq!(
        interned, 1,
        "only the materialized isolator is interned; the refused isolation interns nothing"
    );
}
