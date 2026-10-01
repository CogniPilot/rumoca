use super::*;
use crate::{AlgebraicProjectionBlock, AlgebraicProjectionPlan};

fn provenance() -> PatternProvenance {
    PatternProvenance::derived(
        PatternDerivation::DependencyPropagation,
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("state_jacobian.mo"),
            0,
            1,
        ),
    )
    .expect("fixture provenance")
}

fn plan_solving(y_indices: Vec<usize>) -> AlgebraicProjectionPlan {
    AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices,
            ..AlgebraicProjectionBlock::default()
        }],
    }
}

/// Two states and one algebraic `z` (solver column 2) solved by one block
/// whose row reads state 0 and `z` itself.
fn implicit() -> StructuralPattern {
    StructuralPattern::csr(1, 3, [0, 2], [0, 2], provenance()).expect("implicit relation")
}

#[test]
fn state_jacobian_replaces_each_algebraic_by_the_states_its_block_reads() {
    // der(x0) reads z; der(x1) reads x1.
    let derivative =
        StructuralPattern::csr(2, 3, [0, 1, 2], [2, 1], provenance()).expect("derivative");
    let jacobian = StructuralPattern::derive_state_jacobian(
        &derivative,
        Some(&implicit()),
        &plan_solving(vec![2]),
        2,
        3,
    )
    .expect("state Jacobian");

    assert!(jacobian.contains(0, 0));
    assert!(!jacobian.contains(0, 1));
    assert!(jacobian.contains(1, 1));
    assert!(!jacobian.contains(1, 0));
}

#[test]
fn projection_block_solving_a_non_algebraic_column_is_refused() {
    let derivative =
        StructuralPattern::csr(2, 3, [0, 1, 2], [2, 1], provenance()).expect("derivative");
    for column in [0, 3] {
        let refused = StructuralPattern::derive_state_jacobian(
            &derivative,
            Some(&implicit()),
            &plan_solving(vec![column]),
            2,
            3,
        );
        let expected =
            format!("a projection block solves solver column {column}, not an algebraic");
        assert!(
            matches!(
                &refused,
                Err(StructuralPatternError::DependencyContract { message, span: Some(_) })
                    if *message == expected
            ),
            "{refused:?}"
        );
    }
}
