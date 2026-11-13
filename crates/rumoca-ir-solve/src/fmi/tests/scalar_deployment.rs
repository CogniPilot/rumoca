//! The scalar Co-Simulation view admits a stateful root the native profile
//! refuses, keeps its complete metadata, and advertises Co-Simulation only.

use super::super::{FmiComponent, FmiInterfaceProfile, FmiScalarDeploymentView};
use super::max_step_duration_local::delay_bearing_model_with_one_state;

#[test]
fn a_stateful_root_is_refused_natively_and_admitted_as_scalar_co_simulation() {
    let (model, run) = delay_bearing_model_with_one_state();
    let component = FmiComponent::construct(model, vec![run])
        .expect("one state run plus one delay partition is a complete inventory");
    let root = component.runtime_view().retain_assertion_root();
    assert!(
        root.native_preparation().is_err(),
        "the stateless native profile keeps refusing a stateful root"
    );

    let view = FmiScalarDeploymentView::from_root(&root)
        .expect("the scalar view admits the stateful root");
    assert_eq!(
        view.capabilities().interfaces,
        FmiInterfaceProfile::CoSimulation
    );
    assert!(!view.capabilities().fmu_state);
    // The complete checked inventory survives, the storage-less maximum step
    // duration included; nothing is dropped to fit the profile.
    assert_eq!(view.variables().len(), component.variables().len());
    assert!(root.metadata().max_step_duration().is_some());
    assert_eq!(
        root.metadata().state_variable_indices(),
        component.state_variable_indices()
    );
}
