use super::*;

fn fixture_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_tensor_staging.mo"),
        1,
        2,
    )
}

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
    row.push(LinearOp::StoreOutput { src: 0 });
    ScalarProgramBlock::with_program_spans(vec![row], vec![fixture_span()]).unwrap()
}

fn tensor_operation(block: &ScalarProgramBlock) -> &LinearOp {
    block
        .program(0)
        .unwrap()
        .iter()
        .find(|op| {
            matches!(
                op,
                LinearOp::MatrixMultiply { .. }
                    | LinearOp::TensorBinary { .. }
                    | LinearOp::TensorConcatenate { .. }
                    | LinearOp::TensorUpdate { .. }
                    | LinearOp::TensorFill { .. }
            )
        })
        .expect("checked fixture contains its tensor operation")
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
    let op = tensor_operation(&block);
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
    let op = tensor_operation(&block);
    let mut unused = Vec::new();
    eval(&mut regs, &mut unused, op).unwrap();
    assert_eq!(&regs[2..6], &[4., 40., 6., 60.]);
    assert_eq!(unused.capacity(), 0, "a gap is not an operand read");
}

fn assert_copy_storage(values: &[f64], operation: LinearOp, expected: &[f64], destination: usize) {
    let block = checked(values, operation);
    let op = tensor_operation(&block);
    let mut results = Vec::new();
    let mut regs = values.to_vec();
    eval(&mut regs, &mut results, op).unwrap();
    assert_eq!(&regs[destination..destination + expected.len()], expected);
    if destination != 0 {
        assert_eq!(
            results.capacity(),
            0,
            "disjoint copy leaves scratch unallocated"
        );
        return;
    }
    assert_eq!(results.len(), expected.len());
    let (pointer, capacity) = (results.as_ptr(), results.capacity());
    regs.copy_from_slice(values);
    eval(&mut regs, &mut results, op).unwrap();
    assert_eq!(&regs[..expected.len()], expected);
    assert_eq!((results.as_ptr(), results.capacity()), (pointer, capacity));
}

#[test]
fn native_tensor_staging_copy_operations_reuse_storage_and_leave_disjoint_destinations_unallocated()
{
    for dst_start in [0, 8] {
        assert_copy_storage(
            &[1., 2., 3., 4., 0., 0., 0., 0., 0., 0., 0., 0.],
            LinearOp::TensorConcatenate {
                dst_start,
                sources: vec![
                    rumoca_ir_solve::TensorConcatenateSource {
                        start: 2,
                        dimensions: vec![2].into(),
                    },
                    rumoca_ir_solve::TensorConcatenateSource {
                        start: 0,
                        dimensions: vec![2].into(),
                    },
                ]
                .into(),
                dimensions: vec![4].into(),
                axis: 0,
                lanes: 1,
            },
            &[3., 4., 1., 2.],
            dst_start as usize,
        );
        assert_copy_storage(
            &[1., 2., 3., 2., 3., 0., 0., 0., 0., 0., 0., 0.],
            LinearOp::TensorUpdate {
                dst_start,
                base_start: 0,
                value_start: 0,
                dimensions: vec![3].into(),
                subscripts: vec![rumoca_ir_solve::TensorUpdateSubscript::Slice {
                    start: 3,
                    dimensions: vec![2].into(),
                }]
                .into(),
                lanes: 1,
            },
            &[1., 1., 2.],
            dst_start as usize,
        );
        assert_copy_storage(
            &[0., 1., 2., 0., 0., 0., 0., 0., 0., 0., 0., 0.],
            LinearOp::TensorFill {
                dst_start,
                value_start: 1,
                count: 2,
                lanes: 2,
            },
            &[1., 2., 1., 2.],
            dst_start as usize,
        );
    }
}

#[test]
fn native_matrix_product_starts_each_output_from_its_first_term() {
    // [-1 -1 -1] * [0; 0; 0]: every primal product is -0.0, so the dot is
    // -0.0 as in the scalar expansion; a +0.0 term makes it +0.0.
    let primal = [-1.0, -1.0, -1.0, 0.0, 0.0, 0.0];
    let read = |cells: &[f64]| {
        let cells = cells.to_vec();
        move |reg: Reg| Ok::<_, EvalSolveError>(cells[reg as usize])
    };
    let matrix = |lanes: usize| LinearOp::MatrixMultiply {
        dst_start: 0,
        lhs_start: 0,
        rhs_start: (3 * lanes) as Reg,
        rows: 1,
        inner: 3,
        columns: 1,
        lanes,
    };
    let value = value_at(&matrix(1), &mut read(&primal), 0).unwrap();
    assert_eq!(value.to_bits(), (-0.0f64).to_bits());
    let mixed = [-1.0, -1.0, 1.0, 0.0, 0.0, 0.0];
    let value = value_at(&matrix(1), &mut read(&mixed), 0).unwrap();
    assert_eq!(value.to_bits(), 0.0f64.to_bits());

    // Dual lanes `(re, du)`: with lhs (-1, -1) and rhs (0, 0) each tangent
    // term is -0.0 + -0.0, so the tangent is -0.0 as well.
    let dual = [
        -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
    ];
    for lane in 0..2 {
        let value = value_at(&matrix(2), &mut read(&dual), lane).unwrap();
        assert_eq!(value.to_bits(), (-0.0f64).to_bits(), "lane {lane}");
    }

    // Checked execution reaches the same arithmetic.
    let block = checked(&primal, matrix(1));
    let mut regs = primal.to_vec();
    eval(&mut regs, &mut Vec::new(), tensor_operation(&block)).unwrap();
    assert_eq!(regs[0].to_bits(), (-0.0f64).to_bits());

    // An empty dot has no first term and stays +0.0.
    let empty = LinearOp::MatrixMultiply {
        dst_start: 0,
        lhs_start: 0,
        rhs_start: 0,
        rows: 1,
        inner: 0,
        columns: 1,
        lanes: 1,
    };
    let value = value_at(&empty, &mut read(&[-0.0]), 0).unwrap();
    assert_eq!(value.to_bits(), 0.0f64.to_bits());
}
