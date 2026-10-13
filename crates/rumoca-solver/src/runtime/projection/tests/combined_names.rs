//! The combined Y/P view names its unknowns through the wrapped model, and
//! names only the solver coordinates the wrapped model owns.

use super::*;

#[test]
fn the_combined_view_forwards_row_targets_and_names_only_solver_coordinates() {
    let combined = CombinedInitializationProjectionModel {
        model: &TargetedInitialProjectionModel,
        y_len: 2,
        parameter_scales: Vec::new(),
    };
    assert_eq!(combined.target_name_for_row(0).as_deref(), Some("unknown0"));
    assert_eq!(combined.variable_name_for_y_index(1).as_deref(), Some("y1"));
    assert_eq!(
        combined.variable_name_for_y_index(2),
        None,
        "a parameter coordinate is not a solver variable"
    );

    let unnamed = CombinedInitializationProjectionModel {
        model: &RectInitialProjectionModel,
        y_len: 2,
        parameter_scales: Vec::new(),
    };
    assert_eq!(unnamed.target_name_for_row(0), None);
    assert_eq!(
        unnamed.variable_name_for_y_index(0),
        None,
        "a wrapped model without names stays unnamed"
    );
}
