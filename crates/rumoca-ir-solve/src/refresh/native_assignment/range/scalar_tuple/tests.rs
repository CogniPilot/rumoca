use super::*;

#[test]
fn packed_tuple_private_register_overflow_and_empty_values_leave_prefix_unchanged() {
    for destination in [Reg::MAX - 1, Reg::MAX] {
        let mut operations = vec![LinearOp::Const {
            dst: destination,
            value: -0.,
        }];
        let before = operations.clone();
        assert!(pack(&mut operations, &[0, 1]).is_err());
        assert!(operations_match(&operations, &before));
    }
    let mut operations = vec![LinearOp::Const { dst: 0, value: 0. }];
    let before = operations.clone();
    assert!(pack(&mut operations, &[]).is_err());
    assert!(operations_match(&operations, &before));
}
