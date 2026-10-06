use super::*;
use rumoca_core::{SourceId, Span, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    AffineStencilIndexStrideTerm, AffineStencilLoadStride, AlgebraicProjectionBlock, BinaryOp,
    LinearOp, PatternDerivation, PatternProvenance, ScalarProgramBlock, TensorOutputMap,
};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("reverse_invalidation.mo"), 7, 19)
}

fn full(rows: usize, columns: usize) -> StructuralPattern {
    StructuralPattern::full(
        rows,
        columns,
        PatternProvenance::derived(PatternDerivation::ConservativeFull, span()).unwrap(),
    )
    .unwrap()
}

fn seed_pattern(matrix: &[Vec<bool>], columns: usize) -> StructuralPattern {
    let programs = matrix.iter().map(|row| seed_row(row)).collect();
    let source =
        ScalarProgramBlock::with_program_spans(programs, vec![span(); matrix.len()]).unwrap();
    StructuralPattern::derive_from_scalar_jvp(&source, matrix.len(), columns, span()).unwrap()
}

fn seed_row(row: &[bool]) -> Vec<LinearOp> {
    let mut ops = vec![LinearOp::Const { dst: 0, value: 0.0 }];
    for (index, _) in row.iter().enumerate().filter(|(_, present)| **present) {
        ops.push(LinearOp::LoadSeed { dst: 1, index });
        ops.push(LinearOp::Binary {
            dst: 0,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        });
    }
    ops.push(LinearOp::StoreOutput { src: 0 });
    ops
}

fn block(rows: &[usize], columns: &[usize]) -> AlgebraicProjectionBlock {
    AlgebraicProjectionBlock {
        rows: rows.to_vec(),
        y_indices: columns.to_vec(),
        ..Default::default()
    }
}

/// Independent dense relation, supplied by each fixture's mathematical inputs.
/// It neither walks the optimized accumulator nor asks the pattern for edges.
fn dense_oracle(matrix: &[Vec<bool>], plan: &AlgebraicProjectionPlan) -> Vec<bool> {
    let mut prior = vec![false; matrix.len()];
    plan.blocks
        .iter()
        .map(|block| {
            let result = block.y_indices.iter().any(|&column| {
                matrix
                    .iter()
                    .zip(&prior)
                    .any(|(row, &included)| included && row[column])
            });
            for &row in &block.rows {
                prior[row] = true;
            }
            result
        })
        .collect()
}

/// Keep the previous exhaustive consumer as a small independent oracle,
/// including its validation order and exact source-span diagnostics.
fn original_column_rows_oracle(
    source: &StructuralPattern,
    plan: &AlgebraicProjectionPlan,
) -> Result<Vec<bool>, EvalSolveError> {
    let column_rows = source.column_rows();
    let mut earlier_rows = vec![false; source.rows() as usize];
    let mut invalidations = Vec::with_capacity(plan.blocks.len());
    for block in &plan.blocks {
        let invalidates =
            block
                .y_indices
                .iter()
                .copied()
                .try_fold(false, |invalidates, column| {
                    let affected = column_rows.get(column).ok_or_else(|| {
                        sparsity_error(
                            format!(
                                "projection invalidation column {column} is outside 0..{}",
                                source.columns()
                            ),
                            Some(source.provenance().span()),
                        )
                    })?;
                    Ok::<_, EvalSolveError>(
                        invalidates || affected.iter().any(|&row| earlier_rows[row]),
                    )
                })?;
        invalidations.push(invalidates);
        for &row in &block.rows {
            let row_count = earlier_rows.len();
            let Some(earlier) = earlier_rows.get_mut(row) else {
                return Err(sparsity_error(
                    format!("projection invalidation row {row} is outside 0..{row_count}"),
                    Some(source.provenance().span()),
                ));
            };
            *earlier = true;
        }
    }
    Ok(invalidations)
}

fn compare(pattern: &StructuralPattern, matrix: &[Vec<bool>], plan: &AlgebraicProjectionPlan) {
    let actual = derive_algebraic_reverse_invalidations(Some(pattern), plan).unwrap();
    assert_eq!(actual, dense_oracle(matrix, plan));
    assert_eq!(actual, original_column_rows_oracle(pattern, plan).unwrap());
}

fn permutations(pattern: &StructuralPattern, matrix: &[Vec<bool>]) {
    let rows = pattern.rows() as usize;
    let columns = pattern.columns() as usize;
    for offset in 0..rows {
        let plan = AlgebraicProjectionPlan {
            blocks: (0..rows)
                .map(|index| {
                    let row = (index + offset) % rows;
                    block(&[row, row], &[row % columns, (row + 2) % columns])
                })
                .collect(),
        };
        compare(pattern, matrix, &plan);
    }
}

#[test]
fn full_empty_diagonal_and_csr_preserve_ordered_dense_results() {
    let full_matrix = vec![vec![true; 7]; 5];
    permutations(&full(5, 7), &full_matrix);
    let empty = vec![vec![false; 7]; 5];
    let empty_pattern = seed_pattern(&empty, 7);
    assert!(matches!(empty_pattern.view(), StructuralPatternView::Empty));
    permutations(&empty_pattern, &empty);
    let diagonal = (0..5)
        .map(|row| (0..7).map(|column| column == row).collect())
        .collect::<Vec<_>>();
    let diagonal_pattern = seed_pattern(&diagonal, 7);
    assert!(matches!(
        diagonal_pattern.view(),
        StructuralPatternView::Diagonal
    ));
    permutations(&diagonal_pattern, &diagonal);
    let sparse = vec![
        vec![false, true, false, false, true, false, false],
        vec![true, false, true, false, false, false, false],
        vec![false; 7],
        vec![true, false, false, false, true, false, false],
        vec![false, true, false, true, false, false, false],
    ];
    let sparse_pattern = seed_pattern(&sparse, 7);
    assert!(matches!(
        sparse_pattern.view(),
        StructuralPatternView::Csr { .. }
    ));
    permutations(&sparse_pattern, &sparse);
}

#[test]
fn checked_banded_representation_preserves_its_dense_relation() {
    // Checked wire fixture only: production consumes the existing authority.
    let mut wire = serde_json::to_value(full(5, 7)).unwrap();
    wire["representation"] = serde_json::json!({
        "banded": { "lower_bandwidth": 1, "upper_bandwidth": 2 }
    });
    let pattern: StructuralPattern = serde_json::from_value(wire).unwrap();
    assert!(matches!(
        pattern.view(),
        StructuralPatternView::Banded { .. }
    ));
    let matrix = (0usize..5)
        .map(|row| {
            (0usize..7)
                .map(|column| column >= row.saturating_sub(1) && column <= row + 2)
                .collect()
        })
        .collect::<Vec<_>>();
    permutations(&pattern, &matrix);
}

#[test]
fn affine_domains_keep_shifted_rows_and_signed_strides() {
    for stride in [-2, 2] {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: 0,
                display_name: "i".into(),
                lower: 9,
                upper: 1,
                step: -2,
            }],
        };
        let start = if stride < 0 { 10 } else { 2 };
        let pattern = StructuralPattern::derive_from_affine_jvp(
            &domain,
            &TensorOutputMap::dense_contiguous(1, &domain).unwrap(),
            &[
                LinearOp::LoadSeed {
                    dst: 0,
                    index: start,
                },
                LinearOp::StoreOutput { src: 0 },
            ],
            &[AffineStencilLoadStride {
                op_position: 0,
                terms: vec![AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride,
                }],
            }],
            6,
            12,
            span(),
        )
        .unwrap();
        assert!(matches!(
            pattern.view(),
            StructuralPatternView::Affine { .. }
        ));
        let mut matrix = vec![vec![false; 12]; 6];
        for ordinal in 0..5 {
            matrix[ordinal + 1][(start as isize + ordinal as isize * stride) as usize] = true;
        }
        permutations(&pattern, &matrix);
    }
}

#[test]
fn overlapping_rows_and_target_ranges_commit_only_after_the_current_block() {
    let matrix = vec![vec![true; 4]; 3];
    let plan = AlgebraicProjectionPlan {
        blocks: vec![
            block(&[], &[0, 1]),
            block(&[0, 1], &[2, 3]),
            block(&[1, 2], &[]),
            block(&[2], &[3, 2, 1, 0]),
        ],
    };
    compare(&full(3, 4), &matrix, &plan);
    assert_eq!(
        dense_oracle(&matrix, &plan),
        vec![false, false, false, true]
    );
}

#[test]
fn invalid_targets_and_rows_keep_exact_refusals_after_a_full_intersection() {
    let pattern = full(3, 4);
    for invalid in [
        block(&[1], &[0, 4]),
        block(&[0, 3], &[0]),
        block(&[3], &[]),
        block(&[1], &[usize::MAX]),
    ] {
        let plan = AlgebraicProjectionPlan {
            blocks: vec![block(&[0], &[0]), invalid],
        };
        let actual = derive_algebraic_reverse_invalidations(Some(&pattern), &plan).unwrap_err();
        let oracle = original_column_rows_oracle(&pattern, &plan).unwrap_err();
        assert_eq!(actual.to_string(), oracle.to_string());
        assert_eq!(actual.source_span(), oracle.source_span());
        assert_eq!(actual.source_span(), Some(span()));
    }
}

#[test]
fn missing_source_and_zero_extents_preserve_the_original_results() {
    let invalid = AlgebraicProjectionPlan {
        blocks: vec![block(&[usize::MAX], &[usize::MAX])],
    };
    assert_eq!(
        derive_algebraic_reverse_invalidations(None, &invalid).unwrap(),
        Vec::<bool>::new()
    );
    for (rows, columns, plan) in [
        (
            0,
            3,
            AlgebraicProjectionPlan {
                blocks: vec![block(&[], &[0, 2])],
            },
        ),
        (
            3,
            0,
            AlgebraicProjectionPlan {
                blocks: vec![block(&[0, 2], &[])],
            },
        ),
        (0, 0, AlgebraicProjectionPlan::default()),
    ] {
        let pattern = full(rows, columns);
        assert_eq!(
            derive_algebraic_reverse_invalidations(Some(&pattern), &plan).unwrap(),
            original_column_rows_oracle(&pattern, &plan).unwrap()
        );
    }
}

#[test]
fn unchanged_28800_row_full_relation_and_empty_plan_do_not_expand_adjacency() {
    let pattern = full(28800, 28800);
    assert_eq!(
        derive_algebraic_reverse_invalidations(Some(&pattern), &Default::default()).unwrap(),
        Vec::<bool>::new()
    );
    let plan = AlgebraicProjectionPlan {
        blocks: (0..28800).map(|row| block(&[row], &[row])).collect(),
    };
    let result = derive_algebraic_reverse_invalidations(Some(&pattern), &plan).unwrap();
    assert_eq!(result.len(), 28800);
    assert!(!result[0]);
    assert!(result[1..].iter().all(|invalidates| *invalidates));
}
