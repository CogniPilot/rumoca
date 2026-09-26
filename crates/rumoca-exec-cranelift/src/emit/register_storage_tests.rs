use super::*;

fn allocates_register_tape(row: &[LinearOp]) -> bool {
    let mut context = cranelift_codegen::Context::new();
    let mut frontend = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut context.func, &mut frontend);
    let entry = builder.create_block();
    builder.switch_to_block(entry);
    builder.seal_block(entry);
    let allocated = create_row_register_tape(&mut builder, types::I64, row)
        .expect("native register storage")
        .is_some();
    builder.ins().return_(&[]);
    builder.finalize();
    allocated
}

fn transposed_dual_product(rows: usize) -> Vec<LinearOp> {
    let matrix_cells = (rows * 3 * 2) as u32;
    let vector = 3 * matrix_cells;
    let output = vector + 6;
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 0,
            count: rows * 3,
            seed_start: Some(0),
            lanes: 2,
        },
        LinearOp::TensorTranspose {
            dst_start: matrix_cells,
            src_start: 0,
            rows: 3,
            columns: rows,
            element_width: 1,
            lanes: 2,
        },
        LinearOp::TensorTranspose {
            dst_start: 2 * matrix_cells,
            src_start: matrix_cells,
            rows,
            columns: 3,
            element_width: 1,
            lanes: 2,
        },
        LinearOp::TensorLoad {
            dst_start: vector,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: rows * 3,
            count: 3,
            seed_start: Some(rows * 3),
            lanes: 2,
        },
        LinearOp::MatrixMultiply {
            dst_start: output,
            lhs_start: 2 * matrix_cells,
            rhs_start: vector,
            rows,
            inner: 3,
            columns: 1,
            lanes: 2,
        },
        LinearOp::StoreOutputRange {
            start: output,
            count: rows * 2,
            stride: 1,
        },
    ]
}

#[test]
fn small_tensor_rows_avoid_a_tape_and_preserve_ordered_dual_arithmetic() {
    for rows in [3, 11] {
        let row = transposed_dual_product(rows);
        assert_eq!(allocates_register_tape(&row), rows == 11);
        let plan = plan_row(&row).unwrap();
        let compiled = compile_jacobian_rows(&[row]).unwrap();
        let mut y = [1e16, 1.0, -1e16].repeat(rows);
        y.extend([1.0, 1.0, 1.0]);
        for scale in [0.0, 1.0, -2.0] {
            let seed = (0..y.len())
                .map(|index| scale * (index as f64 - 2.0))
                .collect::<Vec<_>>();
            let mut actual = vec![0.0; rows * 2];
            let mut expected = actual.clone();
            execute_row(
                &plan,
                &mut Vec::new(),
                RowInputs {
                    y: &y,
                    p: &[],
                    t: 0.0,
                    seed: Some(&seed),
                    external_tables: &[],
                },
                &mut expected,
            )
            .unwrap();
            compiled.call(&y, &[], 0.0, &seed, &mut actual).unwrap();
            assert_eq!(actual[0], 0.0, "ordered cancellation must be retained");
            assert_eq!(
                actual
                    .iter()
                    .map(|value| value.to_bits())
                    .collect::<Vec<_>>(),
                expected
                    .iter()
                    .map(|value| value.to_bits())
                    .collect::<Vec<_>>()
            );
        }
    }
}

#[test]
fn large_tensor_rows_keep_loop_storage_and_reload_changed_inputs() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::TensorFill {
            dst_start: 1,
            value_start: 0,
            count: 4096,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 1 },
        LinearOp::StoreOutput { src: 4096 },
    ];
    assert!(allocates_register_tape(&row));
    let compiled = compile_residual_rows(&[row]).unwrap();
    for value in [1.5, -0.0, -7.0] {
        let mut out = [0.0; 2];
        compiled.call(&[value], &[], 0.0, &mut out).unwrap();
        assert!(out.iter().all(|result| result.to_bits() == value.to_bits()));
    }
}

/// An identity tensor lowers to constant registers in a small row and to a
/// loop over the register tape in a large one; both store one on the
/// diagonal and zero elsewhere.
#[test]
fn identity_tensors_lower_statically_and_through_the_tape() {
    for (size, tape) in [(3, false), (64, true)] {
        let last = size * size;
        let row = vec![
            LinearOp::TensorIdentity {
                dst_start: 1,
                size,
                lanes: 1,
            },
            LinearOp::StoreOutput { src: 1 },
            LinearOp::StoreOutput { src: 2 },
            LinearOp::StoreOutput {
                src: (size + 2) as u32,
            },
            LinearOp::StoreOutput { src: last as u32 },
        ];
        assert_eq!(allocates_register_tape(&row), tape, "size {size}");
        let compiled = compile_residual_rows(&[row]).unwrap();
        let mut out = [f64::NAN; 4];
        compiled.call(&[], &[], 0.0, &mut out).unwrap();
        assert_eq!(out, [1.0, 0.0, 1.0, 1.0], "size {size}");
    }
}
