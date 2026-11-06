//! The optional observer must not change typed failure priority or unwind recovery.

use super::*;
use me::numerical_observation::{
    AdapterWriter, FailureCategory, IncompleteReason, ObservationRoute, RecordKind,
    SettingsCollector,
};

#[derive(Clone, Copy)]
enum InitializeExit {
    Typed,
    Component,
    Panic,
}

/// Only a test plugin: the first initialization is ordinary, the second makes a
/// real trial excursion and then fails. It does not impersonate a BDF readback.
struct FailingReinitialization {
    inner: FaultyPlugin,
    exit: InitializeExit,
    _observation: Option<AdapterWriter>,
}

impl me::MeIntegratorBackend for FailingReinitialization {
    fn initialize(
        &mut self,
        point: &me::MeContinuousPoint,
        derivatives: me::MeDerivativeHandle,
    ) -> Result<(), me::MeIntegrationError> {
        if self.inner.initializations.get() == 0 {
            return self.inner.initialize(point, derivatives);
        }
        let trial = match self.exit {
            InitializeExit::Component => -1.0,
            InitializeExit::Typed | InitializeExit::Panic => point.states()[0] + 1.0,
        };
        let evaluated = derivatives
            .derivatives(point.time() + 0.01, &[trial])
            .is_ok();
        if matches!(self.exit, InitializeExit::Panic) {
            std::panic::panic_any(HostilePanic::AfterATrial { evaluated });
        }
        Err(me::MeIntegrationError::numerical(
            "observer-fixture",
            me::MeNumericalFailure::LinearAlgebra,
            "the original plugin error",
        ))
    }

    fn advance(
        &mut self,
        request: &me::MeAdvanceRequest,
    ) -> Result<me::MeStepCandidate, me::MeIntegrationError> {
        self.inner.advance(request)
    }

    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), me::MeIntegrationError> {
        self.inner.sample(time, states)
    }

    fn truncate_reset(
        &mut self,
        point: &me::MeContinuousPoint,
    ) -> Result<(), me::MeIntegrationError> {
        self.inner.truncate_reset(point)
    }
}

fn observe_failure(
    exit: InitializeExit,
    observed: bool,
) -> Option<me::numerical_observation::SettingsSnapshot> {
    let model = algebraic_branch_component(false);
    let mut retained = retained_branch_component(&model);
    let collector = observed.then(|| SettingsCollector::new(64, 64).unwrap());
    let endpoints = collector
        .as_ref()
        .and_then(|collector| collector.associate(ObservationRoute::ModelExchangeSession));
    let (host_writer, adapter_writer) = match endpoints {
        Some((host, adapter)) => {
            adapter.unavailable();
            (Some(host), Some(adapter))
        }
        None => (None, None),
    };
    let plugin = FailingReinitialization {
        inner: FaultyPlugin::new(PluginFault::Healthy),
        exit,
        _observation: adapter_writer,
    };
    let options = me::session::MeSessionOptions::new(me::session::MeSessionOptionsInput {
        start_time: 0.0,
        stop_time: Some(1.0),
        relative_tolerance: 1.0e-8,
        absolute_tolerance: 1.0e-8,
        output_interval: 0.1,
        root_scan_resolution: 0.04,
        root_location_tolerance: 1.0e-10,
        max_wall_seconds: None,
        records_trace: true,
    })
    .unwrap();
    let mut session = retained
        .lease(options)
        .unwrap()
        .into_session_with_numerical_observation(Some(Box::new(plugin)), host_writer)
        .unwrap();
    let before = session.verification_session_point();
    assert_failure_exit(&mut session, exit);
    assert!(!session.verification_capability_is_active());
    assert_eq!(session.verification_session_point(), before);
    assert_eq!(session.verification_component_point(), before);
    let loss = match exit {
        InitializeExit::Panic => me::session::MeSessionLoss::NumericalStep,
        InitializeExit::Typed | InitializeExit::Component => me::session::MeSessionLoss::Restart,
    };
    assert_split_session_is_refused(&mut session, loss);
    drop(session);
    collector.map(SettingsCollector::finish)
}

fn assert_failure_exit(
    session: &mut me::session::MeSimulationSession<'_, 'static>,
    exit: InitializeExit,
) {
    if matches!(exit, InitializeExit::Panic) {
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| session.reset(0.0)))
            .expect_err("the exact plugin panic must reach the embedding");
        assert_eq!(
            resumed_payload(caught),
            HostilePanic::AfterATrial { evaluated: true }
        );
        return;
    }
    let failure = session.reset(0.0).expect_err("the plugin must fail");
    match exit {
        InitializeExit::Typed => assert!(matches!(failure,
            me::session::MeSessionError::Integration(me::MeIntegrationError::Numerical {
                method: "observer-fixture", category: me::MeNumericalFailure::LinearAlgebra, ref detail
            }) if detail == "the original plugin error")),
        InitializeExit::Component => assert!(
            matches!(failure, me::session::MeSessionError::Component(_)),
            "the typed component cause must outrank the plugin error: {failure}"
        ),
        InitializeExit::Panic => unreachable!(),
    }
}

#[test]
fn observation_preserves_typed_plugin_error_and_real_trial_restoration() {
    assert!(observe_failure(InitializeExit::Typed, false).is_none());
    let snapshot = observe_failure(InitializeExit::Typed, true).unwrap();
    assert_eq!(
        snapshot.incomplete,
        IncompleteReason::UnsupportedObservation as u16
    );
    let outcomes: Vec<_> = snapshot
        .records
        .iter()
        .filter_map(|record| match record.kind {
            RecordKind::OperationFinished { success } => Some((record.operation, success)),
            _ => None,
        })
        .collect();
    assert_eq!(outcomes, [(Some(1), true), (Some(2), false)]);
    assert!(
        snapshot
            .records
            .iter()
            .any(|record| record.operation == Some(2)
                && record.kind
                    == RecordKind::OperationFailure(FailureCategory::Numerical(
                        me::MeNumericalFailure::LinearAlgebra
                    )))
    );
    assert!(
        !snapshot
            .records
            .iter()
            .any(|record| matches!(record.kind, RecordKind::Parameters(_)))
    );
}

#[test]
fn observation_cannot_replace_a_latched_component_failure_with_plugin_error() {
    assert!(observe_failure(InitializeExit::Component, false).is_none());
    let snapshot = observe_failure(InitializeExit::Component, true).unwrap();
    assert_eq!(
        snapshot.incomplete,
        IncompleteReason::UnsupportedObservation as u16
    );
    assert!(
        snapshot
            .records
            .iter()
            .any(|record| record.operation == Some(2)
                && record.kind == RecordKind::OperationFailure(FailureCategory::Component))
    );
    assert!(
        snapshot
            .records
            .iter()
            .any(|record| record.operation == Some(2)
                && record.kind == RecordKind::OperationFinished { success: false })
    );
}

#[test]
fn observation_preserves_original_panic_restoration_and_marks_unfinished_call() {
    assert!(observe_failure(InitializeExit::Panic, false).is_none());
    let snapshot = observe_failure(InitializeExit::Panic, true).unwrap();
    assert!(snapshot.has(IncompleteReason::UnsupportedObservation));
    assert!(snapshot.has(IncompleteReason::UnfinishedOperation));
    assert!(!snapshot.has(IncompleteReason::WritersStillLive));
    assert!(
        !snapshot
            .records
            .iter()
            .any(|record| record.operation == Some(2)
                && matches!(record.kind, RecordKind::OperationFinished { .. }))
    );
}

#[test]
fn reinitialization_fixture_forwards_step_sampling_and_reset_contracts() {
    use me::MeIntegratorBackend;

    let mut plugin = FailingReinitialization {
        inner: FaultyPlugin::new(PluginFault::Healthy),
        exit: InitializeExit::Typed,
        _observation: None,
    };
    let mut states = [f64::NAN];
    let missing = plugin.sample(0.0, &mut states).unwrap_err();
    assert!(missing.to_string().contains("no accepted interval"));
    assert!(states[0].is_nan());
    let point = me::MeContinuousPoint::new(0.0, vec![4.0], 1).unwrap();
    plugin.truncate_reset(&point).unwrap();
    plugin.sample(0.0, &mut states).unwrap();
    assert_eq!(states, [4.0]);
    let outside = plugin.sample(0.1, &mut states).unwrap_err();
    assert!(outside.to_string().contains("outside"));
    assert_eq!(states, [4.0]);
    let request = me::MeAdvanceRequest::new(point, None, 0.1, None, None).unwrap();
    let missing_handle = plugin.advance(&request).unwrap_err();
    assert!(
        missing_handle
            .to_string()
            .contains("never issued a derivative handle")
    );
    plugin.sample(0.0, &mut states).unwrap();
    assert_eq!(
        states,
        [4.0],
        "failed advance must preserve the reset point"
    );
}
