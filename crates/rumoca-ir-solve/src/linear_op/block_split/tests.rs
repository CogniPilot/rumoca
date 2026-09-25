use super::*;
use LinearOp as L;

/// `r = sin(y2) * p0 + y0` with block unknown y0: the sine of the fixed
/// coordinate and its product hoist, the unknown's load and the sum do not.
fn product_row() -> Vec<LinearOp> {
    vec![
        L::LoadY { dst: 0, index: 2 },
        L::Unary {
            dst: 1,
            op: UnaryOp::Sin,
            arg: 0,
        },
        L::LoadP { dst: 2, index: 0 },
        L::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        L::LoadY { dst: 4, index: 0 },
        L::Binary {
            dst: 5,
            op: BinaryOp::Add,
            lhs: 3,
            rhs: 4,
        },
        L::StoreOutput { src: 5 },
    ]
}

#[test]
fn operations_reading_no_unknown_hoist_and_their_consumers_receive_them() {
    let program = product_row();
    let split = BlockResidualSplit::derive(&program, &[0]).expect("a split");
    assert_eq!(split.invariant(), &program[..4]);
    assert_eq!(split.dependent(), &program[4..]);
    assert_eq!(split.live_out(), &[3]);
    split.check(&program, &[0]).expect("the split checks");
    // With y2 an unknown too, the sine and product read it: only the
    // parameter load hoists, and the product receives it.
    let split = BlockResidualSplit::derive(&program, &[0, 2]).expect("a split");
    assert_eq!(split.invariant(), &program[2..3]);
    assert_eq!(split.live_out(), &[2]);
}

#[test]
fn a_condition_reading_an_unknown_keeps_its_select_dependent() {
    let program = vec![
        L::LoadY { dst: 0, index: 0 },
        L::LoadP { dst: 1, index: 0 },
        L::LoadP { dst: 2, index: 1 },
        L::Select {
            dst: 3,
            cond: 0,
            if_true: 1,
            if_false: 2,
        },
        L::StoreOutput { src: 3 },
    ];
    let split = BlockResidualSplit::derive(&program, &[0]).expect("a split");
    assert!(
        split.dependent().contains(&program[3]),
        "the select reads the unknown's condition"
    );
    assert_eq!(split.live_out(), &[1, 2]);
}

#[test]
fn a_register_written_twice_never_hoists() {
    let program = vec![
        L::LoadP { dst: 0, index: 0 },
        L::Unary {
            dst: 1,
            op: UnaryOp::Exp,
            arg: 0,
        },
        L::LoadY { dst: 2, index: 0 },
        L::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        L::Unary {
            dst: 1,
            op: UnaryOp::Cos,
            arg: 0,
        },
        L::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 3,
            rhs: 1,
        },
        L::StoreOutput { src: 4 },
    ];
    let split = BlockResidualSplit::derive(&program, &[0]).expect("a split");
    assert_eq!(
        split.invariant(),
        &program[..1],
        "only the parameter load hoists"
    );
    split.check(&program, &[0]).expect("the split checks");
}

#[test]
fn a_preset_the_dependent_part_writes_is_rejected() {
    let program = product_row();
    let split = BlockResidualSplit::derive(&program, &[0]).expect("a split");
    let mut clobbering = split.clone();
    let mut dependent = clobbering.dependent.to_vec();
    dependent.insert(0, L::Const { dst: 3, value: 1.0 });
    clobbering.dependent = dependent.into_boxed_slice();
    assert_eq!(
        clobbering.check(&program, &[0]),
        Err(BlockResidualSplitError::ClobberedPreset { register: 3 })
    );
    let mut missing = split.clone();
    missing.live_out = Box::new([]);
    assert_eq!(
        missing.check(&program, &[0]),
        Err(BlockResidualSplitError::UndefinedRegister { register: 3 })
    );
    let mut widened = split;
    widened.invariant = program[..3].to_vec().into_boxed_slice();
    assert!(widened.check(&program, &[0]).is_err());
}

#[test]
fn a_program_without_invariant_work_does_not_split() {
    let program = vec![L::LoadY { dst: 0, index: 0 }, L::StoreOutput { src: 0 }];
    assert!(BlockResidualSplit::derive(&program, &[0]).is_none());
}
