use super::*;
use rumoca_solver::CompiledSolveExpression;
mod mixed_faults;
mod regions;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("portable_dense.mo"), 13, 57)
}

fn source(n: usize, seed_rhs: bool) -> ScalarProgramBlock {
    let cells = n * n;
    let mut operations = vec![
        LinearOp::Const {
            dst: 0,
            value: -0.0,
        },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::TensorLoad {
            dst_start: 1,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 0,
            count: cells,
            seed_start: None,
            lanes: 1,
        },
    ];
    for index in 0..n {
        let dst = (1 + cells + index) as u32;
        operations.push(if seed_rhs {
            LinearOp::LoadSeed { dst, index }
        } else {
            LinearOp::LoadY {
                dst,
                index: cells + index,
            }
        });
    }
    for component in 0..n {
        let dst = (1 + cells + n + component) as u32;
        operations.extend([
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start: 1,
                rhs_start: (1 + cells) as u32,
                n,
                component,
            },
            LinearOp::StoreOutput { src: dst },
        ]);
    }
    ScalarProgramBlock::with_program_spans(vec![operations], vec![span()]).unwrap()
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

fn reference(source: &ScalarProgramBlock, y: &[f64], seed: Option<&[f64]>) -> Vec<f64> {
    let mut expected = vec![0.0; source.output_count()];
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        source,
        y,
        &[],
        0.0,
        rumoca_eval_solve::RowEvalContext {
            seed,
            ..Default::default()
        },
        &mut expected,
    )
    .unwrap();
    expected
}

#[test]
fn emitted_dense_solve_matches_original_gauss_jordan_order_and_last_pivot_ties() {
    for (n, y) in [
        (1, vec![3.0, -0.0]),
        (2, vec![0.0, 3.0, 7.0, 2.0, 5.0, 11.0]),
        (2, vec![3.0, 1.7, -3.0, 2.3, 0.7, 9.1]),
        (
            3,
            vec![1.1, 3.2, 7.3, -1.1, 4.4, 1.8, 1.1, 2.7, 8.3, 6.1, -2.8, 3.9],
        ),
    ] {
        let source = source(n, false);
        let layout = VarLayout::from_parts(Default::default(), y.len(), 0);
        let code = WasmExpression::compile(&source, &layout, &Default::default()).unwrap();
        let before = bits(&y);
        let expected = reference(&source, &y, None);
        let mut output = vec![19.0; n + 1];
        for _ in 0..2 {
            code.call(&y, &[], 0.0, &[], &mut output).unwrap();
            assert_eq!(bits(&output), bits(&expected), "n={n}, y={y:?}");
            assert_eq!(bits(&y), before);
        }
    }
}

#[test]
fn emitted_dense_faults_keep_original_span_and_commit_no_prefix_or_partial_solution() {
    let source = source(2, false);
    let code = WasmExpression::compile(
        &source,
        &VarLayout::from_parts(Default::default(), 6, 0),
        &Default::default(),
    )
    .unwrap();
    let valid = [1.0, 2.0, 3.0, 7.0, 5.0, 11.0];
    for y in [
        [0.0, 0.0, -0.0, 0.0, 1.0, 2.0],
        [1.0, 2.0, 2.0, 4.0, 5.0, 11.0],
        [f64::NAN, 2.0, 3.0, 7.0, 5.0, 11.0],
        [1.0, 2.0, f64::INFINITY, 7.0, 5.0, 11.0],
        [1.0, 2.0, 3.0, 7.0, f64::NAN, 11.0],
        [1.0, 2.0, 3.0, 7.0, 5.0, f64::INFINITY],
        [1e-308, 1e-309, 1e-309, 2e-308, f64::MAX, 1.0],
    ] {
        let mut expected = [0.0; 3];
        let reference = rumoca_eval_solve::eval_scalar_program_block(
            &source,
            &y,
            &[],
            0.0,
            None,
            &mut expected,
        )
        .unwrap_err();
        let mut output = [19.0, -0.0, 23.0];
        let before = bits(&output);
        let input_before = bits(&y);
        let error = code.call(&y, &[], 0.0, &[], &mut output).unwrap_err();
        assert_eq!(error.source_span(), Some(span()));
        assert!(error.to_string().contains("singular matrix"), "{error}");
        assert!(
            reference.to_string().contains("singular matrix"),
            "{reference}"
        );
        assert_eq!(bits(&output), before);
        assert_eq!(bits(&y), input_before);
        code.call(&valid, &[], 0.0, &[], &mut output).unwrap();
        assert_eq!(bits(&output), bits(&self::reference(&source, &valid, None)));
    }
}

#[test]
fn emitted_directional_dense_rhs_preserves_changed_seed_bits() {
    let source = source(2, true);
    let y = [3.0, 1.7, -3.0, 2.3];
    let code = WasmExpression::compile_with_seed(
        &source,
        &VarLayout::from_parts(Default::default(), 4, 0),
        &Default::default(),
        Some(2),
    )
    .unwrap();
    let mut actual = [19.0; 3];
    for seed in [[0.0, -0.0], [-0.0, 0.0], [2.7, -3.1], [-7.9, 11.3]] {
        code.call_with_seed(
            rumoca_eval_solve::JacobianEvalInputs {
                y: &y,
                p: &[],
                t: 0.0,
                seed: &seed,
            },
            &[],
            &mut actual,
        )
        .unwrap();
        assert_eq!(bits(&actual), bits(&reference(&source, &y, Some(&seed))));
    }
    let before = bits(&actual);
    assert!(
        code.call_with_seed(
            rumoca_eval_solve::JacobianEvalInputs {
                y: &y,
                p: &[],
                t: 0.0,
                seed: &[1.0]
            },
            &[],
            &mut actual,
        )
        .is_err()
    );
    assert_eq!(bits(&actual), before);
}

#[test]
fn dense_augmented_workspace_is_cold_bounded_by_the_private_capacity_policy() {
    let source = source(2048, false);
    let layout = VarLayout::from_parts(Default::default(), 2048 * 2049, 0);
    let error = WasmExpression::compile(&source, &layout, &Default::default())
        .err()
        .expect("augmented workspace must exceed the private capacity");
    assert!(
        error.contains("private register arena exceeds 64 MiB"),
        "{error}"
    );
}
