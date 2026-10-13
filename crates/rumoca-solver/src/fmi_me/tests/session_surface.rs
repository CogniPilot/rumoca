//! What a session and its owners answer without advancing: horizon, output
//! metadata, new diagnostics, native request counts and termination.

use rumoca_ir_solve as solve;

use super::{block, fixture_instance_config, harmonic_oscillator, refresh_owned};
use crate::WARNING_ASSERTION_CODE;
use crate::fmi_me::{
    MeModelSource, MeRetainedComponent,
    integrator::conformance::{HermiteStepIntegrator, SamplerQuality},
    session::{MeSessionOptions, MeSessionOptionsInput},
};

fn with_warning(mut model: solve::SolveModel) -> solve::SolveModel {
    model.problem.events.action_conditions = block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_session_warning_condition.mo",
    );
    model.problem.events.actions = vec![solve::SolveEventAction {
        kind: solve::SolveEventActionKind::Warning,
        message: solve::SolveEventMessage {
            parts: vec![solve::SolveEventMessagePart::Text("check the input".into())],
        },
        span: rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fmi_me_session_warning.mo"),
            1,
            2,
        ),
        origin: "Fixture.check".into(),
        clock_owner: None,
        assertion_projection: None,
    }];
    refresh_owned(model)
}

/// The oscillator with declared metadata for both visible states.
fn described_oscillator() -> solve::SolveModel {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_session_metadata.mo"),
        1,
        2,
    );
    let meta = |name: &str| solve::SolveVariableMeta {
        name: name.into(),
        source_span: span,
        role: "state".into(),
        is_state: true,
        value_type: Some("Real".into()),
        variability: None,
        time_domain: None,
        unit: Some("m".into()),
        start: None,
        min: None,
        max: None,
        nominal: None,
        fixed: None,
        description: None,
        state_coordinate: None,
        phasor: None,
    };
    crate::fixture_layout::with_visible_expressions(
        harmonic_oscillator(),
        vec!["x".to_string(), "v".to_string()],
        solve::ScalarProgramBlock::default(),
        vec![meta("x"), meta("v")],
    )
}

fn options(stop_time: Option<f64>) -> MeSessionOptions {
    MeSessionOptions::new(MeSessionOptionsInput {
        start_time: 0.0,
        stop_time,
        relative_tolerance: 1.0e-8,
        absolute_tolerance: 1.0e-8,
        output_interval: 0.1,
        root_scan_resolution: 0.05,
        root_location_tolerance: 1.0e-10,
        max_wall_seconds: None,
        records_trace: true,
    })
    .expect("the fixture options are admissible")
}

fn retained(model: &solve::SolveModel) -> MeRetainedComponent {
    MeRetainedComponent::instantiate(
        MeModelSource::fixture(model),
        &fixture_instance_config(),
        None,
    )
    .expect("the fixture component instantiates")
}

fn names(meta: &[crate::SimVariableMeta]) -> Vec<&str> {
    meta.iter().map(|variable| variable.name.as_str()).collect()
}

#[test]
fn a_session_reports_horizon_metadata_and_only_new_diagnostics() {
    let model = with_warning(described_oscillator());
    let mut component = retained(&model);
    let host = component.lease(options(Some(1.0))).expect("the sole lease");
    let host_meta = host.output_meta().expect("the host reads its metadata");
    assert_eq!(names(&host_meta), ["x", "v"]);

    let mut session = host
        .into_session(Some(Box::new(HermiteStepIntegrator::new(
            2,
            SamplerQuality::Native,
        ))))
        .expect("a state-carrying component admits a state-carrying plugin");
    assert_eq!(session.stop_time(), Some(1.0));
    let session_meta = session.output_meta().unwrap();
    assert_eq!(names(&session_meta), names(&host_meta));
    assert!(
        session_meta
            .iter()
            .all(|variable| variable.is_state && variable.unit.as_deref() == Some("m"))
    );
    // Attaching observes the initial point, which reports the warning site once.
    let (reported, cursor) = session.diagnostics_since(0);
    assert_eq!(cursor, 1);
    assert_eq!(reported[0].code, WARNING_ASSERTION_CODE);
    assert_eq!(reported[0].message, "check the input");
    session
        .output_values()
        .expect("a reported site stays reported");
    assert_eq!(session.diagnostics_since(cursor), (Vec::new(), 1));

    #[cfg(feature = "native-wasm")]
    assert!(
        session
            .native_value_count(&[0], solve::SolveVariableValueKind::Real)
            .is_err(),
        "a non-native component has no native request to count"
    );
    session
        .terminate()
        .expect("the session terminates its component");
    assert!(session.is_terminated());
    assert_eq!(session.finish().diagnostics.len(), 1);
}

#[test]
fn a_retained_component_reports_diagnostics_and_terminates_after_its_session() {
    let model = with_warning(solve::SolveModel::default());
    let component = retained(&model);
    assert_eq!(component.diagnostics_since(0), (Vec::new(), 0));

    let session = match component.into_session_retaining(options(None), None) {
        Ok(session) => session,
        Err(_) => panic!("a zero-state component attaches the time-only driver"),
    };
    session.output_values().expect("the outputs observe");
    let mut component = match session.into_retained_component() {
        Ok(component) => component,
        Err(_) => panic!("an owned session returns its component"),
    };
    let (reported, cursor) = component.diagnostics_since(0);
    assert_eq!((reported.len(), cursor), (1, 1));
    #[cfg(feature = "native-wasm")]
    assert!(
        component
            .native_value_count(&[0], solve::SolveVariableValueKind::Real)
            .is_err()
    );
    component
        .terminate()
        .expect("the retained component terminates");
}
