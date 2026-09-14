use super::*;
use rumoca_ir_solve::TensorInputKind;

fn span() -> rumoca_core::ProvenanceSpan {
    let source = rumoca_core::SourceId::from_source_name("block_seed_domain.mo");
    rumoca_core::Span::from_offsets(source, 0, 1)
        .require_provenance("seed domain fixture")
        .unwrap()
}

fn dual(operations: &[LinearOp], mode: SeedMode<'_>) -> ScalarProgramBlock {
    let mut builder = AdBuilder::new_with_span(mode, span().span());
    builder.store_output_mode = StoreOutputMode::Dual;
    for operation in operations {
        builder.lower_op(operation.clone()).unwrap();
    }
    ScalarProgramBlock::with_source_span(vec![builder.ops], span()).unwrap()
}

fn evaluate(block: &ScalarProgramBlock, y: &[f64], p: &[f64], t: f64, seed: &[f64]) -> Vec<f64> {
    let mut output = vec![0.0; block.output_indices().len()];
    rumoca_eval_solve::eval_scalar_program_block(block, y, p, t, Some(seed), &mut output).unwrap();
    output
}

#[test]
fn block_domain_keeps_fixed_orientation_primal_and_omits_its_tangent_product() {
    let source = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 9,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 9,
            input: TensorInputKind::Y,
            input_start: 9,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::MatrixMultiply {
            dst_start: 12,
            lhs_start: 0,
            rhs_start: 9,
            rows: 3,
            inner: 3,
            columns: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 12,
            count: 3,
            stride: 1,
        },
    ];
    let active = [9, 10, 11];
    let original = dual(&source, SeedMode::SolverYOnly);
    let reduced = dual(&source, SeedMode::SolverYSubset { active: &active });
    let products = |block: &ScalarProgramBlock| {
        block.programs()[0]
            .iter()
            .map(|op| match op {
                LinearOp::MatrixMultiply { lanes: 2, .. } => 3,
                LinearOp::MatrixMultiply { .. } => 1,
                _ => 0,
            })
            .sum::<usize>()
    };
    assert!(products(&reduced) < products(&original));
    for shift in [0.0, 0.5, -2.0] {
        let y = (0..12)
            .map(|i| f64::from(i) / 10.0 + shift)
            .collect::<Vec<_>>();
        let mut seed = vec![0.0; 12];
        seed[9..].copy_from_slice(&[2.0, -1.0, 3.0]);
        assert_eq!(
            evaluate(&reduced, &y, &[], 0.0, &seed),
            evaluate(&original, &y, &[], 0.0, &seed)
        );
    }
}

#[test]
fn partially_active_tensor_loads_split_at_domain_boundaries_without_scalar_operations() {
    for count in [9, 4096] {
        let source = vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 3,
                count,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count,
                stride: 1,
            },
        ];
        let active = [4, 5, 8];
        let block = dual(&source, SeedMode::SolverYSubset { active: &active });
        let loads = block.programs()[0]
            .iter()
            .filter(|op| matches!(op, LinearOp::TensorLoad { .. }))
            .count();
        assert_eq!(loads, 5);
        assert!(
            block.programs()[0].len() < 40,
            "tensor packing must follow domain runs, not scalar extent"
        );
        assert!(
            !block.programs()[0]
                .iter()
                .any(|op| matches!(op, LinearOp::LoadY { .. } | LinearOp::LoadSeed { .. }))
        );
        let y = (0..count + 3).map(|i| i as f64).collect::<Vec<_>>();
        let seed = vec![2.0; count + 3];
        let output = evaluate(&block, &y, &[], 0.0, &seed);
        for (offset, pair) in output.chunks_exact(2).enumerate() {
            let coordinate = offset + 3;
            assert_eq!(pair[0], y[coordinate]);
            assert_eq!(
                pair[1],
                if active.contains(&coordinate) {
                    2.0
                } else {
                    0.0
                }
            );
        }
    }
}

#[test]
fn projection_seed_domain_does_not_replace_general_or_parameter_directions() {
    let source = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Sin,
            arg: 0,
        },
        LinearOp::LoadY { dst: 2, index: 1 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::LoadP { dst: 4, index: 0 },
        LinearOp::LoadTime { dst: 5 },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Mul,
            lhs: 4,
            rhs: 5,
        },
        LinearOp::Binary {
            dst: 7,
            op: BinaryOp::Add,
            lhs: 3,
            rhs: 6,
        },
        LinearOp::StoreOutput { src: 7 },
    ];
    let domain = dual(&source, SeedMode::SolverYSubset { active: &[1] });
    let general = dual(&source, SeedMode::SolverYOnly);
    let full = dual(&source, SeedMode::SolverYAndP { p_seed_offset: 2 });
    let y = [0.5, 2.0];
    assert_eq!(
        evaluate(&domain, &y, &[3.0], 4.0, &[0.0, 1.0]),
        evaluate(&general, &y, &[3.0], 4.0, &[0.0, 1.0])
    );
    assert_eq!(
        evaluate(&general, &y, &[3.0], 4.0, &[1.0, 0.0])[1],
        2.0 * 0.5_f64.cos()
    );
    assert_eq!(evaluate(&full, &y, &[3.0], 4.0, &[0.0, 0.0, 1.0])[1], 4.0);
}

#[test]
fn fixed_coordinate_arithmetic_still_executes_in_a_block_jacobian() {
    let source = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Sqrt,
            arg: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let block = dual(&source, SeedMode::SolverYSubset { active: &[1] });
    let output = evaluate(&block, &[-1.0, 0.0], &[], 0.0, &[0.0, 1.0]);
    assert!(output[0].is_nan());
    assert_eq!(output[1], 0.0);
}
