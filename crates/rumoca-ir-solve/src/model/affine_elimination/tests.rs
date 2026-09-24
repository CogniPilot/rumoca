use super::*;
use crate::{BlockTearing, CausalStep, PatternDerivation, PatternProvenance};
use rumoca_core::{SourceId, Span};

fn fixture() -> (AlgebraicProjectionBlock, StructuralPattern) {
    let block = AlgebraicProjectionBlock {
        rows: vec![30, 10, 20],
        y_indices: vec![12, 11, 10],
        tearing: Some(BlockTearing {
            tear_y_indices: vec![10],
            residual_rows: vec![20],
            causal_steps: vec![
                CausalStep {
                    row: 30,
                    y_index: 12,
                },
                CausalStep {
                    row: 10,
                    y_index: 11,
                },
            ],
        }),
        guarded_tearing: None,
        alternate_charts: Vec::new(),
    };
    let pattern = StructuralPattern::from_row_dependencies(
        3,
        3,
        &[vec![0, 2], vec![0, 1], vec![1, 2]],
        PatternProvenance::derived(
            PatternDerivation::DependencyPropagation,
            Span::from_offsets(SourceId::from_source_name("CausalPartition.mo"), 0, 1),
        )
        .unwrap(),
    )
    .unwrap();
    (block, pattern)
}

#[test]
fn local_layout_retains_source_order_and_complete_permutations() {
    let (block, pattern) = fixture();
    let layout = AffineEliminationLayout::derive(&block, &pattern).unwrap();
    assert_eq!(layout.causal(), &[(0, 0), (1, 1)]);
    assert_eq!(layout.residuals(), &[2]);
    assert_eq!(layout.tears(), &[2]);
}

#[test]
fn reversing_a_causal_dependency_requires_an_exact_zero_guard() {
    let (mut block, pattern) = fixture();
    block.tearing.as_mut().unwrap().causal_steps.reverse();
    let layout = AffineEliminationLayout::derive(&block, &pattern).unwrap();
    assert_eq!(layout.zero_guards(), &[(1, 0)]);
}

#[test]
fn duplicate_missing_and_foreign_members_cannot_issue_a_layout() {
    for mutation in 0..4 {
        let (mut block, pattern) = fixture();
        let tearing = block.tearing.as_mut().unwrap();
        match mutation {
            0 => tearing.residual_rows[0] = 30,
            1 => tearing.tear_y_indices[0] = 11,
            2 => {
                tearing.causal_steps.pop();
            }
            _ => tearing.causal_steps[0].y_index = 999,
        }
        assert!(AffineEliminationLayout::derive(&block, &pattern).is_none());
    }
}

#[test]
fn a_pattern_without_the_selected_diagonal_cannot_issue_a_layout() {
    let (block, pattern) = fixture();
    let mut rows = vec![vec![2], vec![0, 1], vec![1, 2]];
    let missing =
        StructuralPattern::from_row_dependencies(3, 3, &rows, pattern.provenance()).unwrap();
    assert!(AffineEliminationLayout::derive(&block, &missing).is_none());
    rows[0] = vec![0, 1, 2];
    let future =
        StructuralPattern::from_row_dependencies(3, 3, &rows, pattern.provenance()).unwrap();
    let layout = AffineEliminationLayout::derive(&block, &future).unwrap();
    assert_eq!(layout.zero_guards(), &[(0, 1)]);
}
