use super::*;
use crate::{SolveDeclarationStart, SolveExactInitialRun, SolveExactInitialValues};

pub(super) fn fixture(value: i64) -> (SolveModel, FmiVariableInput) {
    let (mut model, mut input) = super::max_step_duration_local::delay_bearing_model_with_one_run();
    model.problem.events.delays = crate::SolveDelayPartition::default();
    model.problem.solve_layout.variable_storage_runs[0].value_kind =
        SolveVariableValueKind::Integer;
    model.problem.solve_layout.variable_declarations = vec![SolveVariableDeclaration::new(
        SolveVariableStorageRole::Parameter,
        SolveVariableValueKind::Integer,
    )];
    input.value_kind = SolveVariableValueKind::Integer;
    input.start = SolveDeclarationStart::Integer(
        SolveExactInitialValues::construct(
            1,
            vec![SolveExactInitialRun::Repeat {
                start: 0,
                value,
                count: 1,
            }],
        )
        .unwrap(),
    );
    (model, input)
}

#[test]
fn metadata_keeps_integer_bits_and_owns_fmi_width() {
    for value in [9_007_199_254_740_993, i64::MAX] {
        let (model, input) = fixture(value);
        let component = FmiComponent::construct(model, vec![input]).unwrap();
        let variable = &component.variables()[0];
        let SolveDeclarationStart::Integer(values) = variable.declaration_start() else {
            panic!("exact Integer declaration authority")
        };
        assert_eq!(values.value(0), Some(&value));
        assert!(std::ptr::eq(
            variable.declaration_start(),
            variable.typed_start().unwrap()
        ));
        assert_eq!(variable.fmi3_type(), "Int64");
        assert!(!variable.fmi2_integer_admitted());
        assert!(
            variable.start().is_none(),
            "no Float64 reconstruction exists"
        );
        let json = serde_json::to_value(super::super::metadata_projection::SerializedFmiVariable(
            variable,
        ))
        .unwrap();
        assert_eq!(json["start_fmi3"][0].as_i64(), Some(value));
    }
}

#[test]
fn typed_start_cannot_transfer_to_a_foreign_declaration_extent() {
    let (model, mut input) = fixture(7);
    input.dimensions = vec![2];
    input.start = SolveDeclarationStart::Integer(
        SolveExactInitialValues::construct(
            2,
            vec![SolveExactInitialRun::Repeat {
                start: 0,
                value: 7,
                count: 2,
            }],
        )
        .unwrap(),
    );
    assert!(matches!(
        FmiComponent::construct(model, vec![input]),
        Err(FmiComponentError::StorageTypeMismatch { .. })
    ));
}

#[test]
fn c_refuses_exact_integer_starts_outside_its_numerical_profile() {
    let (model, input) = fixture(i64::MAX);
    let error = FmiComponent::construct(model, vec![input])
        .unwrap()
        .into_codegen_view()
        .try_c()
        .unwrap_err();
    assert!(error.to_string().contains("exact Integer initialization"));
}
