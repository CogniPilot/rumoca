use super::*;

fn source_span() -> rumoca_core::ProvenanceSpan {
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add("inactive.mo", "parameter Real p[n]; p .* p;");
    rumoca_core::Span::from_offsets(source, 0, 27)
        .require_provenance("inactive tensor fixture")
        .unwrap()
}

fn lower_dual(program: Vec<LinearOp>, seed_mode: SeedMode) -> ScalarProgramBlock {
    let mut builder = AdBuilder::new_with_span(seed_mode, source_span().span());
    builder.store_output_mode = StoreOutputMode::Dual;
    for operation in program {
        builder.lower_op(operation).unwrap();
    }
    ScalarProgramBlock::with_source_span(vec![builder.ops], source_span()).unwrap()
}

#[test]
fn parameter_tensor_arithmetic_needs_no_tangent_program() {
    for count in [3, 4096] {
        let count_reg = Reg::try_from(count).unwrap();
        let program = vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start: 0,
                count,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorBinary {
                dst_start: count_reg,
                op: BinaryOp::Mul,
                lhs_start: 0,
                rhs_start: 0,
                count,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: count_reg,
                count,
                stride: 1,
            },
        ];
        let mut builder = AdBuilder::default();
        for operation in program {
            builder.lower_op(operation).unwrap();
        }
        assert!(
            !builder.ops.iter().any(|op| matches!(
                op,
                LinearOp::TensorBinary { lanes: 2, .. } | LinearOp::TensorLoad { lanes: 2, .. }
            )),
            "parameter-only tensor arithmetic must remain primal-only"
        );
        let rows = ScalarProgramBlock::with_source_span(vec![builder.ops], source_span()).unwrap();
        let mut output = vec![1.0; count];
        rumoca_eval_solve::eval_scalar_program_block(
            &rows,
            &[],
            &vec![2.0; count],
            0.0,
            Some(&[]),
            &mut output,
        )
        .unwrap();
        assert_eq!(output, vec![0.0; count]);
    }
}

#[test]
fn inactive_tangent_storage_and_broadcast_products_remain_compact() {
    for count in [3, 4096] {
        for active in [false, true] {
            let count_reg = Reg::try_from(count).unwrap();
            let input = if active {
                rumoca_ir_solve::TensorInputKind::Y
            } else {
                rumoca_ir_solve::TensorInputKind::P
            };
            let rows = lower_dual(
                vec![
                    LinearOp::TensorLoad {
                        dst_start: 0,
                        input,
                        input_start: 0,
                        count,
                        seed_start: None,
                        lanes: 1,
                    },
                    LinearOp::LoadP {
                        dst: count_reg,
                        index: 0,
                    },
                    LinearOp::TensorBinary {
                        dst_start: count_reg + 1,
                        op: BinaryOp::Mul,
                        lhs_start: 0,
                        rhs_start: count_reg,
                        count,
                        lhs_stride: 1,
                        rhs_stride: 0,
                        lanes: 1,
                    },
                    LinearOp::StoreOutputRange {
                        start: count_reg + 1,
                        count,
                        stride: 1,
                    },
                ],
                SeedMode::SolverYOnly,
            );
            assert!(
                rows.programs()[0].len() <= 15,
                "tensor extent {count} must not become scalar packing operations"
            );
            let mut output = vec![0.0; 2 * count];
            rumoca_eval_solve::eval_scalar_program_block(
                &rows,
                &vec![3.0; count],
                &vec![2.0; count],
                0.0,
                Some(&vec![5.0; count]),
                &mut output,
            )
            .unwrap();
            let expected = if active { [6.0, 10.0] } else { [4.0, 0.0] };
            assert!(output.chunks_exact(2).all(|pair| pair == expected));
        }
    }
}

#[test]
fn parameter_tensors_retain_initialization_and_sensitivity_seeds() {
    let rows = lower_dual(
        vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start: 0,
                count: 3,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorBinary {
                dst_start: 3,
                op: BinaryOp::Mul,
                lhs_start: 0,
                rhs_start: 0,
                count: 3,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 3,
                count: 3,
                stride: 1,
            },
        ],
        SeedMode::SolverYAndP { p_seed_offset: 1 },
    );
    let mut output = [0.0; 6];
    rumoca_eval_solve::eval_scalar_program_block(
        &rows,
        &[0.0],
        &[0.0, 2.0, -3.0],
        0.0,
        Some(&[0.0, 1.0, 2.0, 3.0]),
        &mut output,
    )
    .unwrap();
    assert_eq!(output, [0.0, 0.0, 4.0, 8.0, 9.0, -18.0]);
}

#[test]
fn matrix_product_with_one_inactive_factor_uses_only_one_tangent_product() {
    let rows = lower_dual(
        vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start: 0,
                count: 4,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: 4,
                input: rumoca_ir_solve::TensorInputKind::Y,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::MatrixMultiply {
                dst_start: 6,
                lhs_start: 0,
                rhs_start: 4,
                rows: 2,
                inner: 2,
                columns: 1,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 6,
                count: 2,
                stride: 1,
            },
        ],
        SeedMode::SolverYOnly,
    );
    assert!(
        !rows.programs()[0]
            .iter()
            .any(|op| matches!(op, LinearOp::MatrixMultiply { lanes: 2, .. })),
        "a zero factor tangent must not produce a second tangent product"
    );
    let mut output = [0.0; 4];
    rumoca_eval_solve::eval_scalar_program_block(
        &rows,
        &[0.0, 0.0],
        &[2.0, 3.0, 5.0, 7.0],
        0.0,
        Some(&[11.0, 13.0]),
        &mut output,
    )
    .unwrap();
    assert_eq!(output, [0.0, 61.0, 0.0, 146.0]);
}

#[test]
fn zero_primal_value_does_not_make_its_tangent_inactive() {
    let rows = lower_dual(
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Sin,
                arg: 0,
            },
            LinearOp::StoreOutput { src: 1 },
        ],
        SeedMode::SolverYOnly,
    );
    let mut output = [0.0; 2];
    rumoca_eval_solve::eval_scalar_program_block(
        &rows,
        &[0.0],
        &[],
        0.0,
        Some(&[2.0]),
        &mut output,
    )
    .unwrap();
    assert_eq!(output, [0.0, 2.0]);
}

#[test]
fn zero_tangent_does_not_freeze_a_changing_primal() {
    let rows = lower_dual(
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Floor,
                arg: 0,
            },
            LinearOp::Unary {
                dst: 2,
                op: UnaryOp::Sin,
                arg: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
        SeedMode::SolverYOnly,
    );
    for y in [0.25_f64, 1.25, -1.25] {
        let mut output = [0.0; 2];
        rumoca_eval_solve::eval_scalar_program_block(
            &rows,
            &[y],
            &[],
            0.0,
            Some(&[2.0]),
            &mut output,
        )
        .unwrap();
        assert_eq!(output, [y.floor().sin(), 0.0]);
    }
}

fn checked_square_with_assertion() -> rumoca_ir_solve::SolvePureCallTable {
    use rumoca_ir_solve as solve;
    let span = source_span().span();
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![
                solve::SolvePureCallOutput::result(real),
                solve::SolvePureCallOutput::assertion_predicate(),
            ],
            span,
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span)?;
                let square =
                    builder.binary(solve::SolveBinaryOperator::Multiply, input, input, span)?;
                builder.store(outputs[0], square, span)?;
                let zero = builder.constant(solve::SolveValue::real(arithmetic, 0.0), span)?;
                let predicate =
                    builder.compare(solve::SolveCompareOperator::Greater, input, zero, span)?;
                builder.store(outputs[1], predicate, span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn inactive_call_keeps_primal_results_and_assertion_predicates() {
    let table = checked_square_with_assertion();
    let rows = lower_dual(
        vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::PureCall {
                dst_start: 1,
                input_starts: Box::new([0]),
                site: table.owners()[0].call_site(),
            },
            LinearOp::StoreOutputRange {
                start: 1,
                count: 2,
                stride: 1,
            },
        ],
        SeedMode::SolverYOnly,
    );
    assert!(
        rows.programs()[0]
            .iter()
            .any(|op| matches!(op, LinearOp::PureCall { .. }))
    );
    assert!(
        !rows.programs()[0]
            .iter()
            .any(|op| matches!(op, LinearOp::PureCallDirectional { .. }))
    );
    for p in [2.0, -3.0, 0.0] {
        let mut output = [0.0; 4];
        rumoca_eval_solve::eval_scalar_program_block_with_context(
            &rows,
            &[],
            &[p],
            0.0,
            rumoca_eval_solve::RowEvalContext {
                pure_calls: Some(&table),
                seed: Some(&[]),
                ..Default::default()
            },
            &mut output,
        )
        .unwrap();
        assert_eq!(output, [p * p, 0.0, if p > 0.0 { 1.0 } else { 0.0 }, 0.0]);
    }
}

#[test]
fn inactive_tangent_does_not_replace_invalid_primal_arithmetic() {
    let rows = lower_dual(
        vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Sqrt,
                arg: 0,
            },
            LinearOp::StoreOutput { src: 1 },
        ],
        SeedMode::SolverYOnly,
    );
    let mut output = [0.0; 2];
    rumoca_eval_solve::eval_scalar_program_block(&rows, &[], &[-1.0], 0.0, Some(&[]), &mut output)
        .unwrap();
    assert!(output[0].is_nan());
    assert_eq!(output[1], 0.0);
}

#[test]
fn bilinear_tangents_match_primal_finite_differences_for_either_active_factor() {
    for kind in 0..3 {
        for swap in [false, true] {
            for parameter_seeds in [false, true] {
                check_bilinear_tangents(kind, swap, parameter_seeds);
            }
        }
    }
}

fn bilinear_fixture(kind: usize, swap: bool) -> (usize, Vec<LinearOp>) {
    let count = if kind == 2 { 4 } else { 3 };
    let width = Reg::try_from(count).unwrap();
    let (lhs, rhs) = if swap { (width, 0) } else { (0, width) };
    let mut program = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::P,
            input_start: 0,
            count,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: width,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 0,
            count,
            seed_start: None,
            lanes: 1,
        },
    ];
    program.push(match kind {
        0 => LinearOp::TensorCross {
            dst_start: 2 * width,
            lhs_start: lhs,
            rhs_start: rhs,
            lanes: 1,
        },
        1 => LinearOp::TensorBinary {
            dst_start: 2 * width,
            op: BinaryOp::Mul,
            lhs_start: lhs,
            rhs_start: rhs,
            count,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        _ => LinearOp::MatrixMultiply {
            dst_start: 2 * width,
            lhs_start: lhs,
            rhs_start: rhs,
            rows: 2,
            inner: 2,
            columns: 2,
            lanes: 1,
        },
    });
    program.push(LinearOp::StoreOutputRange {
        start: 2 * width,
        count,
        stride: 1,
    });
    (count, program)
}

fn check_bilinear_tangents(kind: usize, swap: bool, parameter_seeds: bool) {
    let (count, program) = bilinear_fixture(kind, swap);
    let primal =
        ScalarProgramBlock::with_source_span(vec![program.clone()], source_span()).unwrap();
    let mode = if parameter_seeds {
        SeedMode::SolverYAndP {
            p_seed_offset: count,
        }
    } else {
        SeedMode::SolverYOnly
    };
    let dual = lower_dual(program, mode);
    let y = [0.7, -0.9, 1.1, 2.0];
    let p = [1.5, -0.3, 0.2, 2.4];
    let dy = [0.2, 0.5, -0.7, 1.2];
    let dp = [0.4, -0.8, 0.6, 0.9].map(|v| if parameter_seeds { v } else { 0.0 });
    let seed = dy[..count]
        .iter()
        .chain(&dp[..count])
        .copied()
        .collect::<Vec<_>>();
    let mut actual = vec![0.0; 2 * count];
    rumoca_eval_solve::eval_scalar_program_block(
        &dual,
        &y[..count],
        &p[..count],
        0.0,
        Some(&seed),
        &mut actual,
    )
    .unwrap();
    let evaluate = |step: f64| {
        let y = std::array::from_fn::<_, 4, _>(|i| y[i] + step * dy[i]);
        let p = std::array::from_fn::<_, 4, _>(|i| p[i] + step * dp[i]);
        let mut out = vec![0.0; count];
        rumoca_eval_solve::eval_scalar_program_block(
            &primal,
            &y[..count],
            &p[..count],
            0.0,
            None,
            &mut out,
        )
        .unwrap();
        out
    };
    let expected = evaluate(0.0);
    let plus = evaluate(1e-6);
    let minus = evaluate(-1e-6);
    for i in 0..count {
        assert_eq!(actual[2 * i], expected[i]);
        let slope = (plus[i] - minus[i]) / 2e-6;
        assert!(
            (actual[2 * i + 1] - slope).abs() < 1e-8,
            "kind={kind} swap={swap} parameters={parameter_seeds} component={i}"
        );
    }
}
