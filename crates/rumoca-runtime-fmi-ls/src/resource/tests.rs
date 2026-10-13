//! The resource drives one real retained component through the FMI lifecycle.

use super::*;
use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_ir_solve::SolveVariableValueKind;
use rumoca_solver::fmi_me::{MeModelSource, MeOutputCursor};

const PLANT: &str = r#"
model Plant
  input Real gain = 2;
  input Integer count = 3;
  input Boolean enabled = true;
  output Real y;
equation
  y = gain * time;
end Plant;
"#;

struct Fixture {
    resource: FmiLsResource,
    gain: u32,
    count: u32,
    enabled: u32,
}

fn reference(variables: &[rumoca_ir_solve::fmi::FmiVariable], name: &str) -> u32 {
    variables
        .iter()
        .find(|variable| variable.name() == name)
        .unwrap_or_else(|| panic!("{name} is published"))
        .value_reference_fmi3()
}

fn fixture() -> Fixture {
    let mut session = Session::new(SessionConfig::default());
    session.add_document("Plant.mo", PLANT).unwrap();
    let dae = session.compile_model("Plant").unwrap().dae;
    let component = rumoca_sim::lower_fmi_export_component(&dae).unwrap();
    let root = component.runtime_view().retain_assertion_root();
    let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        root.native_preparation().unwrap(),
    )
    .unwrap();
    let variables = root.metadata().variables();
    let (gain, count, enabled) = (
        reference(variables, "gain"),
        reference(variables, "count"),
        reference(variables, "enabled"),
    );
    let retained =
        rumoca_solver::fmi_me::session::MeRetainedComponent::prepare_retained_native_wasm(
            MeModelSource::new(&component),
            &code,
        )
        .unwrap();
    Fixture {
        resource: FmiLsResource::from_retained(retained),
        gain,
        count,
        enabled,
    }
}

fn config(start: f64, stop: f64) -> MeInstanceConfig {
    MeInstanceConfig::new("plant", 1e-6, start, stop).unwrap()
}

fn session_options() -> rumoca_solver::fmi_me::MeSessionOptions {
    rumoca_solver::fmi_me::driver::live_session_options(
        &rumoca_ir_solve::fmi::RootLocationPlan::STANDARD,
        0.0,
        1e-6,
        1e-6,
        1e-10,
        None,
    )
    .unwrap()
}

fn read_gain(fixture: &Fixture) -> f64 {
    let mut value = [f64::NAN];
    fixture
        .resource
        .get_native_float64(&[fixture.gain], &mut value)
        .unwrap();
    value[0]
}

#[test]
fn unaddressable_float64_result_refuses_without_panicking_or_allocating() {
    assert!(matches!(
        result_buffer::<f64>(usize::MAX),
        Err(FmiLsResourceError::ResultBuffer(_))
    ));
}

#[test]
fn typed_values_round_trip_before_initialization_and_count_by_kind() {
    let mut f = fixture();
    f.resource.set_native_float64(&[f.gain], &[4.5]).unwrap();
    f.resource.set_native_integer(&[f.count], &[7]).unwrap();
    f.resource.set_boolean(&[f.enabled], &[false]).unwrap();
    let (mut real, mut integer, mut boolean) = ([0.0], [0], [true]);
    f.resource.get_native_float64(&[f.gain], &mut real).unwrap();
    f.resource
        .get_native_integer(&[f.count], &mut integer)
        .unwrap();
    f.resource.get_boolean(&[f.enabled], &mut boolean).unwrap();
    assert_eq!((real, integer, boolean), ([4.5], [7], [false]));
    assert_eq!(
        f.resource
            .value_count(&[f.gain, f.gain], SolveVariableValueKind::Real)
            .unwrap(),
        2
    );
    assert!(
        f.resource
            .value_count(&[f.gain], SolveVariableValueKind::Integer)
            .is_err()
    );
    assert!(f.resource.set_boolean(&[f.gain], &[true]).is_err());
    assert_eq!(f.resource.diagnostics_since(0).0.len(), 0);
}

#[test]
fn a_retained_component_refuses_session_only_operations() {
    let mut f = fixture();
    assert!(f.resource.do_step(0.0, 0.1).is_err());
    assert!(
        f.resource
            .advance_to(0.1, &mut MeOutputCursor::empty())
            .is_err()
    );
    assert_eq!(read_gain(&f), 2.0);
}

#[test]
fn reset_restores_the_original_source_and_termination_is_final() {
    let mut f = fixture();
    f.resource.set_native_float64(&[f.gain], &[9.0]).unwrap();
    f.resource.reset().unwrap();
    assert_eq!(read_gain(&f), 2.0);
    f.resource.terminate().unwrap();
    assert!(f.resource.terminate().is_err());
}

#[test]
fn a_hosted_engine_runs_the_lifecycle_through_do_step_and_a_session_reset() {
    let mut f = fixture();
    f.resource.set_native_float64(&[f.gain], &[3.0]).unwrap();
    f.resource
        .enter_initialization_mode(&config(0.0, 1.0))
        .unwrap();
    f.resource.complete_initialization().unwrap();

    // The session now owns the component; the retained-only entries refuse.
    assert!(f.resource.complete_initialization().is_err());
    assert!(
        f.resource
            .enter_initialization_mode(&config(0.0, 1.0))
            .is_err()
    );
    assert!(
        f.resource
            .exit_initialization_mode(session_options())
            .is_err()
    );

    let step = f.resource.do_step(0.0, 0.25).unwrap();
    assert_eq!(step.last_successful_time, 0.25);
    assert!(!step.terminate_simulation && !step.early_return);
    assert!(
        f.resource.do_step(0.0, 0.25).is_err(),
        "a step must start at the actual communication point"
    );
    assert!(f.resource.do_step(0.25, 0.0).is_err());
    assert!(
        f.resource.do_step(0.25, 2.0).is_err(),
        "a step past the horizon is refused"
    );
    f.resource
        .advance_to(0.5, &mut MeOutputCursor::empty())
        .unwrap();

    // Typed inputs are written and read through the session.
    f.resource.set_native_float64(&[f.gain], &[5.0]).unwrap();
    f.resource.set_native_integer(&[f.count], &[8]).unwrap();
    f.resource.set_boolean(&[f.enabled], &[false]).unwrap();
    let (mut real, mut integer, mut boolean) = ([0.0], [0], [true]);
    f.resource.get_native_float64(&[f.gain], &mut real).unwrap();
    f.resource
        .get_native_integer(&[f.count], &mut integer)
        .unwrap();
    f.resource.get_boolean(&[f.enabled], &mut boolean).unwrap();
    assert_eq!((real, integer, boolean), ([5.0], [8], [false]));
    assert_eq!(
        f.resource
            .value_count(&[f.gain], SolveVariableValueKind::Real)
            .unwrap(),
        1
    );
    assert!(f.resource.set_native_float64(&[f.count], &[1.0]).is_err());

    // Reset releases the session and restores the source defaults.
    f.resource.reset().unwrap();
    assert_eq!(read_gain(&f), 2.0);
    assert!(f.resource.do_step(0.0, 0.1).is_err());

    // The next run takes its own start time.
    f.resource
        .enter_initialization_mode(&config(1.0, 2.0))
        .unwrap();
    f.resource.complete_initialization().unwrap();
    assert_eq!(
        f.resource.do_step(1.0, 0.5).unwrap().last_successful_time,
        1.5
    );
    f.resource.terminate().unwrap();
}
