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

/// Lane seeds are checked in their own units: a `LoadSeed` index is a
/// position among `seed_len * lanes`, a tensor load's seed range is in seeds.
#[test]
fn lane_seed_loads_are_checked_in_their_own_units() {
    use rumoca_ir_solve::{LinearOp, TensorInputKind};

    use super::block::check_seed_loads;

    let (seed_len, lanes) = (4, 3);
    let load = |index| vec![LinearOp::LoadSeed { dst: 0, index }];
    let tensor = |seed_start| {
        vec![LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: Some(seed_start),
            lanes: lanes + 1,
        }]
    };
    assert!(check_seed_loads(0, &load(seed_len * lanes - 1), seed_len, lanes).is_ok());
    assert!(check_seed_loads(0, &load(seed_len * lanes), seed_len, lanes).is_err());
    assert!(check_seed_loads(0, &tensor(seed_len - 2), seed_len, lanes).is_ok());
    // Within `seed_len * lanes` positions but past the last seed.
    assert!(check_seed_loads(0, &tensor(seed_len - 1), seed_len, lanes).is_err());
}
