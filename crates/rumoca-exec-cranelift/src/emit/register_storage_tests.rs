use super::*;

fn allocates_register_tape(row: &[LinearOp]) -> bool {
    let mut context = cranelift_codegen::Context::new();
    context
        .func
        .signature
        .params
        .push(AbiParam::new(types::I64));
    context
        .func
        .signature
        .returns
        .push(AbiParam::new(types::I8));
    let mut frontend = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut context.func, &mut frontend);
    let entry = builder.create_block();
    builder.append_block_params_for_function_params(entry);
    builder.switch_to_block(entry);
    builder.seal_block(entry);
    scratch::enter(&mut builder);
    let allocated = create_row_register_tape(&mut builder, types::I64, row)
        .expect("native register storage")
        .is_some();
    status::succeed(&mut builder);
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

#[test]
fn full_width_tapes_execute_assignments_residual_batches_and_jvp_on_default_stack() {
    let count = 512 * 512;
    let row = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::P,
            input_start: 0,
            count,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutput {
            src: count as u32 - 1,
        },
    ];
    let assignments = compile_assignment_schedule(std::slice::from_ref(&row), &[0, 1]).unwrap();
    let residuals = compile_residual_rows(&[row.clone(), row.clone()]).unwrap();
    let jacobian = compile_jacobian_rows(std::slice::from_ref(&row)).unwrap();
    for value in [-0.0_f64, 1.25, -7.0] {
        let mut parameters = vec![value; count];
        parameters[count - 1] = -value;
        let expected = [value.to_bits(), (-value).to_bits()];
        let mut y = [f64::NAN; 2];
        assignments.call(&mut y, &parameters, 0.0).unwrap();
        assert_eq!(y.map(f64::to_bits), expected);
        let mut residual = [f64::NAN; 4];
        residuals
            .call(&[], &parameters, 0.0, &mut residual)
            .unwrap();
        assert_eq!(
            residual.map(f64::to_bits),
            [expected[0], expected[1], expected[0], expected[1]]
        );
        let mut direction = [f64::NAN; 2];
        jacobian
            .call(&[], &parameters, 0.0, &[], &mut direction)
            .unwrap();
        assert_eq!(direction.map(f64::to_bits), expected);
    }
}

#[test]
fn large_contiguous_output_ranges_compile_and_preserve_batch_offsets_and_bits() {
    let count = 270_347;
    let row = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::P,
            input_start: 0,
            count,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::StoreOutput {
            src: count as u32 - 1,
        },
        LinearOp::StoreOutputRange {
            start: 1,
            count: count - 2,
            stride: 1,
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    let compiled = compile_residual_rows(&[row.clone(), row]).unwrap();
    for scale in [-0.0_f64, 1.25, -7.0] {
        let mut parameters = vec![scale; count];
        parameters[0] = -scale;
        parameters[count - 1] = scale * 2.0;
        let mut expected = parameters.clone();
        expected.swap(0, count - 1);
        let mut output = vec![f64::NAN; 2 * count];
        compiled.call(&[], &parameters, 0.0, &mut output).unwrap();
        for batch in output.chunks_exact(count) {
            assert!(
                batch
                    .iter()
                    .zip(&expected)
                    .all(|(actual, expected)| { actual.to_bits() == expected.to_bits() })
            );
        }
    }
}

/// `[-1 … -1] · [0 … 0]`: every product, and every tangent term
/// `du_l·r + l·du_r` for seeds `(-1, 0)`, is `-0.0`, so the first-term
/// convention gives `-0.0` where a `+0.0` start would give `+0.0`.
fn signed_zero_dot(inner: usize, lanes: usize) -> Vec<LinearOp> {
    let width = (2 * inner * lanes) as u32;
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 0,
            count: 2 * inner,
            seed_start: (lanes == 2).then_some(0),
            lanes,
        },
        LinearOp::MatrixMultiply {
            dst_start: width,
            lhs_start: 0,
            rhs_start: (inner * lanes) as u32,
            rows: 1,
            inner,
            columns: 1,
            lanes,
        },
        LinearOp::StoreOutputRange {
            start: width,
            count: lanes,
            stride: 1,
        },
    ]
}

/// The interpreter's and the JIT's bits for [`signed_zero_dot`], for a static
/// product and one looped over the register tape, primal and dual.
/// A case label with its interpreted and native result bits.
type SignedZeroDotResult = (String, Vec<u64>, Vec<u64>);

fn signed_zero_dot_results() -> Vec<SignedZeroDotResult> {
    let bits = |values: &[f64]| values.iter().map(|v| v.to_bits()).collect::<Vec<_>>();
    let mut results = Vec::new();
    for inner in [2, 600] {
        let mut y = vec![-1.0; inner];
        y.extend(vec![0.0; inner]);
        let seed = y.clone();
        for lanes in [1, 2] {
            let row = signed_zero_dot(inner, lanes);
            assert_eq!(allocates_register_tape(&row), inner == 600, "inner {inner}");
            let plan = plan_row(&row).unwrap();
            let mut interpreted = vec![f64::NAN; lanes];
            execute_row(
                &plan,
                &mut Vec::new(),
                RowInputs {
                    y: &y,
                    p: &[],
                    t: 0.0,
                    seed: (lanes == 2).then_some(&seed[..]),
                    external_tables: &[],
                },
                &mut interpreted,
            )
            .unwrap();
            let mut native = vec![f64::NAN; lanes];
            if lanes == 2 {
                let compiled = compile_jacobian_rows(&[row]).unwrap();
                compiled.call(&y, &[], 0.0, &seed, &mut native).unwrap();
            } else {
                let compiled = compile_residual_rows(&[row]).unwrap();
                compiled.call(&y, &[], 0.0, &mut native).unwrap();
            }
            results.push((
                format!("inner {inner} lanes {lanes}"),
                bits(&interpreted),
                bits(&native),
            ));
        }
    }
    results
}

/// Every case whose `pick`ed bits are not all `-0.0`.
fn signed_zero_dot_mismatches(pick: fn(&SignedZeroDotResult) -> &Vec<u64>) -> Vec<String> {
    signed_zero_dot_results()
        .iter()
        .filter(|result| pick(result).iter().any(|&b| b != (-0.0f64).to_bits()))
        .map(|result| format!("{}: {:x?}", result.0, pick(result)))
        .collect()
}

#[test]
fn native_linear_matrix_products_start_each_output_from_its_first_term() {
    let mismatches = signed_zero_dot_mismatches(|result| &result.2);
    assert!(mismatches.is_empty(), "{mismatches:#?}");
}

#[test]
fn interpreted_linear_matrix_products_start_each_output_from_its_first_term() {
    let mismatches = signed_zero_dot_mismatches(|result| &result.1);
    assert!(mismatches.is_empty(), "{mismatches:#?}");
}
