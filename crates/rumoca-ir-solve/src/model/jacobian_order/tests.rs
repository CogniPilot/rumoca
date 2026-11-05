use super::*;
use crate::{SolveLayout, SolveModel, SolveProblem, VarLayout, scalar_slot_p, scalar_slot_y};
use rumoca_core::{SourceId, Span};

fn model() -> SolveModel {
    let span = Span::from_offsets(SourceId::from_source_name("Ordered.mo"), 1, 2);
    SolveModel {
        problem: SolveProblem {
            layout: VarLayout::from_parts_with_shapes_and_spans(
                [
                    ("x".into(), scalar_slot_y(0)),
                    ("p".into(), scalar_slot_p(0)),
                ]
                .into(),
                [("x".into(), vec![3])].into(),
                [("x".into(), span)].into(),
                3,
                1,
            )
            .unwrap(),
            solve_layout: SolveLayout {
                state_scalar_count: 3,
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![1.0].into(),
        ..Default::default()
    }
}

#[test]
fn stored_order_issues_independent_complete_axes_bound_to_exact_model() {
    let model = model();
    let canonical = StoredStateJacobianOutputOrder::canonical(&model).unwrap();
    let labels = canonical.input_labels();
    let rows = vec![labels[2].clone(), labels[0].clone(), labels[1].clone()];
    let columns = vec![labels[1].clone(), labels[2].clone(), labels[0].clone()];
    let order = StoredStateJacobianOutputOrder::construct(&model, &rows, &columns).unwrap();
    assert!(std::ptr::eq(order.source_model(), &model));
    assert_eq!(order.input_labels(), labels);
    assert_eq!(order.row_labels(), rows);
    assert_eq!(order.column_labels(), columns);
    assert_eq!(order.row_coordinates(), &[2, 0, 1]);
    assert_eq!(order.column_coordinates(), &[1, 2, 0]);
    assert_eq!(order.cells(), 9);
}

#[test]
fn stored_order_rejects_wrong_length_duplicate_foreign_and_parameter_axes() {
    let model = model();
    let canonical = StoredStateJacobianOutputOrder::canonical(&model).unwrap();
    let labels = canonical.input_labels();
    for bad in [
        labels[..2].to_vec(),
        vec![labels[0].clone(); 3],
        vec![labels[0].clone(), labels[1].clone(), "foreign".into()],
        vec![labels[0].clone(), labels[1].clone(), "p".into()],
    ] {
        assert!(StoredStateJacobianOutputOrder::construct(&model, &bad, labels).is_err());
        assert!(StoredStateJacobianOutputOrder::construct(&model, labels, &bad).is_err());
    }
}

#[test]
fn stored_order_refuses_an_unnamed_canonical_coordinate() {
    let mut model = model();
    model.problem.solve_layout.state_scalar_count = 4;
    assert!(matches!(
        StoredStateJacobianOutputOrder::canonical(&model),
        Err(StoredStateJacobianOrderError::MissingCanonicalLabel { index: 3 })
    ));
}
