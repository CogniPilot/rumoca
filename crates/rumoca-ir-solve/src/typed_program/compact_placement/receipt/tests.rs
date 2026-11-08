use super::*;
use crate::StructuralPatternView;
use rumoca_core::SourceId;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("PlacementReceipt.mo"), 1, 2)
}

#[test]
fn issued_identity_survives_clone_but_not_reconstruction() {
    let placement =
        CompactJacobianPlacement::new(vec![8], (0, vec![1]), vec![(0, vec![1])], 8).unwrap();
    let clone = placement.clone();
    let foreign =
        CompactJacobianPlacement::new(vec![8], (0, vec![1]), vec![(0, vec![1])], 8).unwrap();
    assert_eq!(placement, foreign);
    placement
        .with_dependency_receipt(span(), |receipt| {
            assert_eq!(receipt.state_count(), 8);
            assert!(receipt.belongs_to(&placement));
            assert!(receipt.belongs_to(&clone));
            assert!(!receipt.belongs_to(&foreign));
        })
        .unwrap();
}

#[test]
fn reversed_and_strided_rows_keep_exact_coordinate_relation() {
    let placement = CompactJacobianPlacement::new(
        vec![2, 3],
        (10, vec![-6, -2]),
        vec![(0, vec![3, 1]), (6, vec![-3, -1])],
        12,
    )
    .unwrap();
    placement
        .with_dependency_receipt(span(), |receipt| {
            assert!(matches!(
                receipt.relation().view(),
                StructuralPatternView::Stacked { segments: 1 }
            ));
            for row in 0..12 {
                for column in 0..12 {
                    let expected = expected_coordinate(row, column);
                    assert_eq!(
                        receipt.relation().contains(row, column),
                        expected,
                        "{row},{column}"
                    );
                }
            }
        })
        .unwrap();
}

fn expected_coordinate(row: u32, column: u32) -> bool {
    (0..2).any(|i| {
        (0..3)
            .any(|j| row == 10 - 6 * i - 2 * j && (column == 3 * i + j || column == 6 - 3 * i - j))
    })
}

#[test]
fn row_alias_and_nonnested_geometry_refuse_receipt_only() {
    for strides in [vec![0, 1], vec![2, 1]] {
        let placement =
            CompactJacobianPlacement::new(vec![2, 3], (0, strides), vec![(0, vec![0, 0])], 8)
                .unwrap();
        assert!(placement.with_dependency_receipt(span(), |_| ()).is_err());
    }
}

fn entries(value: &serde_json::Value) -> usize {
    match value {
        serde_json::Value::Array(values) => {
            values.len() + values.iter().map(entries).sum::<usize>()
        }
        serde_json::Value::Object(values) => {
            values.len() + values.values().map(entries).sum::<usize>()
        }
        _ => 1,
    }
}

#[test]
fn actual_affine_stacked_metadata_entries_do_not_scale_with_points() {
    let measured = [8, 4096].map(|count| {
        let placement = CompactJacobianPlacement::new(
            vec![count],
            (0, vec![2]),
            vec![(0, vec![1])],
            (count * 2) as usize,
        )
        .unwrap();
        placement
            .with_dependency_receipt(span(), |receipt| {
                let serialized = serde_json::to_value(receipt.relation()).unwrap();
                let count_entries = entries(&serialized);
                assert!(count_entries < 128);
                assert!(receipt.relation().contains((count - 1) * 2, count - 1));
                assert!(!receipt.relation().contains((count - 1) * 2 + 1, count - 1));
                count_entries
            })
            .unwrap()
    });
    assert_eq!(measured[0], measured[1]);
}

#[test]
fn empty_directions_preserve_placement_without_any_relation_edges() {
    let placement = CompactJacobianPlacement::new(vec![8], (0, vec![1]), vec![], 8).unwrap();
    placement
        .with_dependency_receipt(span(), |receipt| {
            assert_eq!(receipt.state_count(), 8);
            assert_eq!(receipt.relation().nonzero_upper_bound(), Some(0));
        })
        .unwrap();
}
