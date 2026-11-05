use super::*;

fn assert_ordered_cells_bitwise(
    actual: &[f64],
    analytic: &[[f64; 2]; 2],
    rows: [usize; 2],
    columns: [usize; 2],
) {
    for r in 0..2 {
        for c in 0..2 {
            assert_eq!(
                actual[r * 2 + c].to_bits(),
                analytic[rows[r]][columns[c]].to_bits()
            );
        }
    }
}

#[test]
fn source_issued_output_axes_match_every_independent_row_column_permutation() {
    // Reversed authored placements deliberately differ from canonical row order.
    let model = model(vec![1, 0]);
    let canonical = ModelStateJacobian::construct(&model).unwrap();
    let labels = canonical.state_labels();
    for rows in [[0, 1], [1, 0]] {
        for columns in [[0, 1], [1, 0]] {
            let row_names = rows.map(|i| labels[i].clone());
            let column_names = columns.map(|i| labels[i].clone());
            let order =
                StoredStateJacobianOutputOrder::construct(&model, &row_names, &column_names)
                    .unwrap();
            let graph = ModelStateJacobian::construct_with_output_order(order).unwrap();
            assert_eq!(graph.state_labels(), labels);
            for x in [[0.25, -0.5], [-1.5, 0.75], [2.0, 1.25]] {
                let actual = evaluate(&graph, x);
                let analytic = [[x[1], x[0]], [2.0 * x[0], x[1].cos()]];
                assert_ordered_cells_bitwise(&actual, &analytic, rows, columns);
            }
        }
    }
}

#[test]
fn canonical_output_request_preserves_the_entire_issued_table() {
    let model = model(vec![0, 1]);
    let canonical = ModelStateJacobian::construct(&model).unwrap();
    let order = StoredStateJacobianOutputOrder::construct(
        &model,
        canonical.state_labels(),
        canonical.state_labels(),
    )
    .unwrap();
    let requested = ModelStateJacobian::construct_with_output_order(order).unwrap();
    assert_eq!(
        serde_json::to_value(canonical.table()).unwrap(),
        serde_json::to_value(requested.table()).unwrap()
    );
}

#[test]
fn valid_axis_descriptor_does_not_bypass_jacobian_domain_admission() {
    let mut model = model(vec![0, 1]);
    model.problem.events.has_terminal_event = true;
    let order = StoredStateJacobianOutputOrder::canonical(&model).unwrap();
    assert!(matches!(
        ModelStateJacobian::construct_with_output_order(order),
        Err(ModelStateJacobianError::Unsupported { .. })
    ));
}
