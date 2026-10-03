use super::*;
use rumoca_ir_solve::{PatternDerivation, PatternProvenance, StructuralPattern};

fn layout() -> Arc<CompactPatternLayout> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("block_jacobian.mo"),
        0,
        1,
    );
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, span).unwrap();
    let pattern = StructuralPattern::from_row_dependencies(
        3,
        3,
        &[vec![0, 2], vec![1], vec![0, 2]],
        provenance,
    )
    .unwrap();
    Arc::new(CompactPatternLayout::of(&pattern))
}

#[test]
fn compact_storage_reads_and_multiplies_as_its_dense_form() {
    let mut compact = BlockJacobian::compact(&layout());
    assert_eq!(
        compact.storage_mut().len(),
        5,
        "one value per pattern entry"
    );
    for (row, column, value) in [
        (0, 0, 2.0),
        (0, 2, -1.0),
        (1, 1, 3.0),
        (2, 0, 0.5),
        (2, 2, 4.0),
    ] {
        compact[(row, column)] = value;
    }
    let dense = compact.as_dense().into_owned();
    let reference = BlockJacobian::dense(dense.clone());
    for row in 0..3 {
        for column in 0..3 {
            assert_eq!(compact[(row, column)], reference[(row, column)]);
        }
    }
    assert_eq!(
        compact[(1, 0)],
        0.0,
        "an entry outside the pattern reads zero"
    );
    let vector = DVector::from_vec(vec![1.0, -2.0, 0.25]);
    assert_eq!(compact.mul_vector(&vector), &dense * &vector);
    compact.clear();
    assert!(compact.storage_mut().iter().all(|&value| value == 0.0));
    assert!(compact.is_stored_in(&layout()));
    assert!(!reference.is_stored_in(&layout()));
}

#[test]
#[should_panic(expected = "pattern entries")]
fn a_compact_write_outside_the_pattern_is_a_construction_defect() {
    let mut compact = BlockJacobian::compact(&layout());
    compact[(1, 0)] = 1.0;
}
