use super::*;
use rumoca_ir_solve as solve;

pub(super) fn selection(
    source: &ScalarProgramBlock,
    rows: Vec<usize>,
) -> solve::ProjectionOutputSelection {
    let n = rows.len();
    let pattern = solve::StructuralPattern::from_row_dependencies(
        n,
        n,
        &vec![(0..n).collect(); n],
        solve::PatternProvenance::derived(
            solve::PatternDerivation::DependencyPropagation,
            fixture_span(),
        )
        .unwrap(),
    )
    .unwrap();
    let plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows,
            y_indices: (0..n).collect(),
            tearing: None,
            guarded_tearing: None,
            alternate_charts: vec![],
        }],
    };
    let artifacts = solve::ContinuousStructuralArtifacts::derived(
        None,
        vec![pattern],
        vec![false],
        None,
        vec![],
        None,
    )
    .with_algebraic_output_evaluations(&plan, source, source, source);
    artifacts.algebraic_projection()[0]
        .residual_output_evaluation()
        .unwrap()
        .clone()
}

fn nonlinear_and_table() -> ScalarProgramBlock {
    use LinearOp as L;
    let original = aggregate_and_table();
    let nonlinear = vec![
        L::LoadY { dst: 0, index: 0 },
        L::LoadP { dst: 1, index: 1 },
        L::LoadTime { dst: 2 },
        L::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        L::Binary {
            dst: 4,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        L::Binary {
            dst: 5,
            op: BinaryOp::Add,
            lhs: 3,
            rhs: 4,
        },
        L::StoreOutput { src: 5 },
    ];
    ScalarProgramBlock::with_output_indices(
        vec![
            original.programs()[0].clone(),
            nonlinear,
            original.programs()[1].clone(),
        ],
        vec![fixture_span(); 3],
        vec![7, 3, 17, 11],
    )
    .unwrap()
}

fn tables(offset: f64) -> Vec<ExternalTableData> {
    vec![ExternalTableData {
        id: 42,
        data: vec![vec![0.0, offset], vec![2.0, offset + 4.0]],
        columns: vec![2],
        smoothness: 1,
        extrapolation: 1,
    }]
}

#[test]
fn batch_preserves_nonlinear_residuals_fresh_tables_and_permuted_outputs() {
    check_nonlinear_batch(vec![17, 11, 3, 7], &[0, 1, 2, 3]);
    check_nonlinear_batch(vec![17, 11, 7], &[0, 1, 3]);
}

fn check_nonlinear_batch(rows: Vec<usize>, offsets: &[usize]) {
    let source = nonlinear_and_table();
    let selection = selection(&source, rows);
    let compiled = compile_selectable_expression_scalar_program_block(&source, None).unwrap();
    assert_eq!(
        selection
            .programs()
            .iter()
            .map(|p| p.program())
            .collect::<Vec<_>>(),
        [1, 2, 0]
    );
    for (x, p, t, offset) in [(3.0, [2.0, 4.0], 0.5, 10.0), (-2.0, [-1.0, 3.0], 2.0, -5.0)] {
        let tables = tables(offset);
        let mut expected = vec![99.0; selection.output_len()];
        for program in selection.programs() {
            let mut values = Vec::new();
            compiled
                .call_program_outputs(program.program(), &[x], &p, t, &tables, &mut values)
                .unwrap();
            for &(from, to) in program.placements() {
                expected[to] = values[from];
            }
        }
        let before = compiled.jit.jit_call_count();
        let mut actual = vec![99.0; selection.output_len()];
        assert!(
            compiled
                .call_projection_outputs(&selection, &[x], &p, t, &tables, &mut actual)
                .unwrap()
        );
        assert_eq!(
            actual.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            expected.iter().map(|v| v.to_bits()).collect::<Vec<_>>()
        );
        let analytic = [x * x + p[1] * t, offset + 2.0, x + t, x * p[0]];
        assert_eq!(
            actual,
            offsets.iter().map(|&i| analytic[i]).collect::<Vec<_>>()
        );
        assert_eq!(compiled.jit.jit_call_count() - before, 3);
    }
}

#[test]
fn batch_preserves_first_failure_identity_order_and_rolls_back_output() {
    let source = nonlinear_and_table();
    let selection = selection(&source, vec![3, 17, 11, 7]);
    let compiled = compile_selectable_expression_scalar_program_block(&source, None).unwrap();
    for (p, tables, successful_prefix, failing_program) in [
        (vec![2.0], tables(10.0), 1, 1),
        (vec![2.0, 4.0], vec![], 2, 2),
    ] {
        let reference = compiled
            .call_program_outputs(failing_program, &[3.0], &p, 0.5, &tables, &mut vec![])
            .unwrap_err();
        let before = compiled.jit.jit_call_count();
        let mut out = [99.0; 4];
        let error = compiled
            .call_projection_outputs(&selection, &[3.0], &p, 0.5, &tables, &mut out)
            .unwrap_err();
        assert_eq!(error.to_string(), reference.to_string());
        assert_eq!(out, [99.0; 4]);
        assert_eq!(compiled.jit.jit_call_count() - before, successful_prefix);
    }
    let mut recovered = [99.0; 4];
    compiled
        .call_projection_outputs(
            &selection,
            &[3.0],
            &[2.0, 4.0],
            0.5,
            &tables(20.0),
            &mut recovered,
        )
        .unwrap();
    assert_eq!(recovered, [3.5, 11.0, 22.0, 6.0]);
}

#[test]
fn batch_checks_extents_and_declines_nonselectable_products_without_execution() {
    let source = nonlinear_and_table();
    let selection = selection(&source, vec![17, 11, 3, 7]);
    let compiled = compile_selectable_expression_scalar_program_block(&source, None).unwrap();
    for (y, p, len) in [
        (vec![], vec![2.0, 4.0], 4),
        (vec![3.0], vec![], 4),
        (vec![3.0], vec![2.0, 4.0], 3),
    ] {
        let mut out = vec![99.0; len];
        assert!(
            compiled
                .call_projection_outputs(&selection, &y, &p, 0.5, &tables(10.0), &mut out)
                .is_err()
        );
        assert_eq!(out, vec![99.0; len]);
        assert_eq!(compiled.jit.jit_call_count(), 0);
    }
    let nonselectable = compile_expression_scalar_program_block(&source).unwrap();
    let mut out = [99.0; 4];
    assert!(
        !nonselectable
            .call_projection_outputs(
                &selection,
                &[3.0],
                &[2.0, 4.0],
                0.5,
                &tables(10.0),
                &mut out
            )
            .unwrap()
    );
    assert_eq!(out, [99.0; 4]);
    assert_eq!(nonselectable.jit.jit_call_count(), 0);
}

#[test]
fn distinct_batch_preserves_early_error_and_nonfinite_then_later_error() {
    let source = nonlinear_and_table();
    let selection = selection(&source, vec![17, 11, 7]);
    assert_eq!(
        selection
            .programs()
            .iter()
            .map(|p| p.program())
            .collect::<Vec<_>>(),
        [1, 2, 0]
    );
    assert!(
        selection
            .programs()
            .iter()
            .all(|p| p.placements().len() == 1)
    );
    let compiled = compile_selectable_expression_scalar_program_block(&source, None).unwrap();
    for (x, p, failing_program, prefix) in [
        (3.0, vec![2.0], 1, 0),
        (f64::INFINITY, vec![2.0, 4.0], 2, 1),
    ] {
        if prefix == 1 {
            let mut value = Vec::new();
            assert!(
                compiled
                    .call_program_outputs(1, &[x], &p, 0.5, &[], &mut value)
                    .unwrap()
            );
            assert!(
                value[0].is_infinite(),
                "nonfinite arithmetic is a value, not an execution error"
            );
        }
        let before_error = compiled.jit.jit_call_count();
        let reference = compiled
            .call_program_outputs(failing_program, &[x], &p, 0.5, &[], &mut vec![])
            .unwrap_err();
        let error_calls = compiled.jit.jit_call_count() - before_error;
        let before = compiled.jit.jit_call_count();
        let mut out = [99.0; 3];
        let error = compiled
            .call_projection_outputs(&selection, &[x], &p, 0.5, &[], &mut out)
            .unwrap_err();
        assert_eq!(error.to_string(), reference.to_string());
        assert_eq!(
            compiled.jit.jit_call_count() - before,
            prefix + error_calls,
            "later programs never execute or replay after error"
        );
        assert_eq!(out, [99.0; 3]);
    }
}
