use super::*;

fn checked(values: &[f64], op: LinearOp) -> ScalarProgramBlock {
    let mut row = values
        .iter()
        .enumerate()
        .map(|(index, value)| LinearOp::Const {
            dst: Reg::try_from(index).unwrap(),
            value: *value,
        })
        .collect::<Vec<_>>();
    row.push(op);
    ScalarProgramBlock::with_program_spans(vec![row], vec![crate::fixture_span()]).unwrap()
}

#[test]
fn native_tensor_staging_reuses_storage_and_leaves_disjoint_strided_gaps_unallocated() {
    let matrix = LinearOp::MatrixMultiply {
        dst_start: 0,
        lhs_start: 0,
        rhs_start: 4,
        rows: 2,
        inner: 2,
        columns: 2,
        lanes: 1,
    };
    let values = [1., 2., 3., 4., 5., 6., 7., 8.];
    let block = checked(&values, matrix);
    let op = block.program(0).unwrap().last().unwrap();
    let mut results = Vec::new();
    let mut regs = values.to_vec();
    eval(&mut regs, &mut results, op).unwrap();
    assert_eq!(&regs[..4], &[19., 22., 43., 50.]);
    let (pointer, capacity) = (results.as_ptr(), results.capacity());
    regs.copy_from_slice(&values);
    eval(&mut regs, &mut results, op).unwrap();
    assert_eq!(&regs[..4], &[19., 22., 43., 50.]);
    assert_eq!((results.as_ptr(), results.capacity()), (pointer, capacity));

    let mut regs = vec![0.0; 28];
    for (start, values) in [
        (0, [1., 10.]),
        (20, [2., 20.]),
        (24, [3., 30.]),
        (26, [4., 40.]),
    ] {
        regs[start..start + 2].copy_from_slice(&values);
    }
    let binary = LinearOp::TensorBinary {
        dst_start: 2,
        op: BinaryOp::Add,
        lhs_start: 0,
        rhs_start: 24,
        count: 2,
        lanes: 2,
        lhs_stride: 10,
        rhs_stride: 1,
    };
    let block = checked(&regs, binary);
    let op = block.program(0).unwrap().last().unwrap();
    let mut unused = Vec::new();
    eval(&mut regs, &mut unused, op).unwrap();
    assert_eq!(&regs[2..6], &[4., 40., 6., 60.]);
    assert_eq!(unused.capacity(), 0, "a gap is not an operand read");
}
