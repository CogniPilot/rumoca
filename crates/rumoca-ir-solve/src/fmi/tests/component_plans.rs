use super::*;
use crate::{
    SolveDeclarationStart, SolveInitialValues, SolveVariableDeclaration, SolveVariableValueKind,
};

fn fixture_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_component_plans.mo"),
        0,
        1,
    )
}

fn component() -> FmiComponent {
    FmiComponent::construct(SolveModel::default(), Vec::new())
        .expect("empty checked Solve model has an empty FMI inventory")
}

#[test]
fn every_component_steps_communication_intervals_to_the_standard_tolerance() {
    let component = component();
    assert_eq!(component.co_simulation().default_tolerance(), 1.0e-6);
    let deployment = component
        .into_codegen_view()
        .try_deployment(FmiDeploymentCapabilities::default())
        .unwrap();
    assert_eq!(deployment.co_simulation().default_tolerance(), 1.0e-6);
}

fn integer_fixture() -> (SolveModel, FmiVariableInput) {
    super::exact_starts::fixture(7)
}

fn admitted_with(minimum: Option<SolveInitialValues>, maximum: Option<SolveInitialValues>) -> bool {
    let (model, mut input) = integer_fixture();
    input.minimum = minimum;
    input.maximum = maximum;
    let component = FmiComponent::construct(model, vec![input]).unwrap();
    component.variables()[0].fmi2_integer_admitted()
}

#[test]
fn fmi2_integer_bounds_must_be_whole_numbers_inside_the_32_bit_range() {
    let repeated = |value: f64| Some(SolveInitialValues::repeat(value, 1).unwrap());
    let literal = |value: f64| Some(SolveInitialValues::from(vec![value]));
    assert!(admitted_with(None, None));
    assert!(admitted_with(
        repeated(-2_147_483_648.0),
        repeated(2_147_483_647.0)
    ));
    assert!(admitted_with(literal(-5.0), literal(12.0)));
    assert!(!admitted_with(repeated(-2_147_483_649.0), None));
    assert!(!admitted_with(None, repeated(2_147_483_648.0)));
    assert!(!admitted_with(None, repeated(0.5)));
    assert!(!admitted_with(literal(1.5), None));
}

mod input_storage {
    use super::*;
    use crate::{SolveVariableStorageRole as Role, VarLayout};

    /// One Real external input stored in the second parameter cell.
    fn input_model(default: Option<f64>, start: f64) -> (SolveModel, FmiVariableInput) {
        let (mut model, mut input) =
            super::super::max_step_duration_local::delay_bearing_model_with_one_run();
        model.problem.events.delays = crate::SolveDelayPartition::default();
        model.problem.solve_layout.variable_storage_runs[0].role = Role::ExternalInput;
        model.problem.solve_layout.variable_declarations = vec![SolveVariableDeclaration::new(
            Role::ExternalInput,
            SolveVariableValueKind::Real,
        )];
        input.role = Role::ExternalInput;
        input.causality = FmiCausality::Input;
        input.variability = FmiVariability::Continuous;
        input.start = vec![start].into();
        input.effective_input_default = default.map(|value| FmiInputDefaultInput {
            declaration: 0,
            values: SolveDeclarationStart::Real(vec![value].into()),
            origins: vec![FmiInputDefaultOriginRun {
                start: 0,
                count: 1,
                origin: FmiInputDefaultOrigin::Supplied,
                span: fixture_span(),
            }],
            fallback_minimum: None,
            fallback_maximum: None,
            fallback_minimum_span: None,
            fallback_maximum_span: None,
        });
        (model, input)
    }

    #[test]
    fn a_default_equal_to_its_stored_value_is_admitted_for_publication() {
        let (model, input) = input_model(Some(0.0), 0.0);
        let component = FmiComponent::construct(model, vec![input]).unwrap();
        component
            .into_codegen_view()
            .try_deployment(FmiDeploymentCapabilities::default())
            .expect("the checked default is the stored value");
    }

    #[test]
    fn a_default_that_differs_from_the_stored_value_is_refused() {
        let (model, input) = input_model(Some(5.0), 0.0);
        assert!(matches!(
            FmiComponent::construct(model, vec![input]),
            Err(FmiComponentError::InputDefault {
                message: "default disagrees with original instantiation storage",
                ..
            })
        ));
    }

    #[test]
    fn a_default_whose_storage_lies_outside_the_initial_values_is_refused() {
        let (mut model, input) = input_model(Some(0.0), 0.0);
        model.parameters = SolveInitialValues::repeat(0.0, 1).unwrap();
        model.problem.layout = VarLayout::from_parts(Default::default(), 0, 2);
        let Err(FmiComponentError::InvalidSolve(message)) =
            FmiComponent::construct(model, vec![input])
        else {
            panic!("the stored cell is not an initial value");
        };
        assert!(message.contains("outside runtime storage"), "{message}");
    }

    #[test]
    fn an_input_start_the_storage_does_not_hold_leaves_the_publication_incomplete() {
        let (model, input) = input_model(None, 3.0);
        let component = FmiComponent::construct(model, vec![input]).unwrap();
        assert!(
            component
                .into_codegen_view()
                .try_deployment(FmiDeploymentCapabilities::default())
                .is_err()
        );
    }
}
