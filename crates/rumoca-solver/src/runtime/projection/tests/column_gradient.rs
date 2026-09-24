//! A column-restricted residual gradient agrees with the complete row.
//!
//! Block reverse gradients request only the block's own columns. A model
//! without a cached gradient answers through its complete row, so the
//! restricted request must report the same availability and, when a gradient
//! exists, the same entries at the requested columns.

use super::*;

fn assert_columns_match_row(label: &str, model: &dyn ImplicitProjectionModel, values: &[f64]) {
    let columns = [0, 1];
    let mut row = vec![f64::NAN; values.len()];
    let mut restricted = vec![f64::NAN; values.len()];
    let complete = model
        .eval_implicit_jacobian_row(0, values, &[], 0.0, &mut row)
        .expect("the complete row gradient evaluates");
    let partial = model
        .eval_implicit_jacobian_row_columns(0, values, &[], 0.0, &columns, &mut restricted)
        .expect("the column-restricted gradient evaluates");
    assert_eq!(partial, complete, "{label}: same gradient availability");
    if complete {
        for column in columns {
            assert_eq!(
                restricted[column].to_bits(),
                row[column].to_bits(),
                "{label}: column {column}"
            );
        }
    }
}

fn combined<M: AlgebraicProjectionModel>(
    model: &M,
) -> CombinedInitializationProjectionModel<'_, M> {
    CombinedInitializationProjectionModel {
        model,
        y_len: 2,
        parameter_scales: Vec::new(),
    }
}

#[test]
fn column_restricted_gradients_match_the_complete_row() {
    let values = [0.25, -1.5];
    assert_columns_match_row("rect", &RectInitialProjectionModel, &values);
    assert_columns_match_row("targeted", &TargetedInitialProjectionModel, &values);
    assert_columns_match_row(
        "combined coupled",
        &combined(&CoupledTargetedInitialProjectionModel),
        &values,
    );
    assert_columns_match_row(
        "combined scaled",
        &combined(&ScaledResidualProjectionModel),
        &values,
    );
}
