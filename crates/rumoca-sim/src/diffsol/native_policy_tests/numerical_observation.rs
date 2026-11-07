//! Real BDF composition controls; definitions only until separately qualified.

use super::{sim_opts, state_fixture, zero_state_fixture};
use crate::diffsol::{build_simulation_artifact, simulate_prepared_with_observation};
use rumoca_solver::fmi_me::numerical_observation::{
    IncompleteReason, RecordKind, SettingsCollector, SettingsStage,
};
use rumoca_solver::{SimExecutionPolicy, SimResult};

fn assert_same_result(actual: &SimResult, expected: &SimResult) {
    assert_eq!(actual.names, expected.names);
    assert_eq!(actual.n_states, expected.n_states);
    assert_eq!(actual.times.len(), expected.times.len());
    assert_eq!(actual.data.len(), expected.data.len());
    assert!(
        actual
            .times
            .iter()
            .zip(&expected.times)
            .all(|(a, b)| a.to_bits() == b.to_bits())
    );
    for (actual, expected) in actual.data.iter().zip(&expected.data) {
        assert_eq!(actual.len(), expected.len());
        assert!(
            actual
                .iter()
                .zip(expected)
                .all(|(a, b)| a.to_bits() == b.to_bits())
        );
    }
    assert_eq!(
        format!("{:?}", actual.termination),
        format!("{:?}", expected.termination)
    );
    assert_eq!(
        format!("{:?}", actual.diagnostics),
        format!("{:?}", expected.diagnostics)
    );
    assert_eq!(
        format!("{:?}", actual.variable_meta),
        format!("{:?}", expected.variable_meta)
    );
}

#[test]
fn observation_preserves_bdf_results_and_separates_warmup_from_run() {
    let opts = sim_opts(SimExecutionPolicy::Interpreter);
    let model = state_fixture(&opts);
    let plain = build_simulation_artifact(model.artifact(), &opts, None, None)
        .unwrap()
        .run()
        .unwrap();
    let collector = SettingsCollector::new(512, 4096).unwrap();
    let prepared =
        build_simulation_artifact(model.artifact(), &opts, None, Some(&collector)).unwrap();
    let actual = simulate_prepared_with_observation(&prepared, Some(&collector)).unwrap();
    drop(prepared);
    assert_same_result(&actual, &plain);
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    let instances: Vec<_> = snapshot
        .records
        .iter()
        .filter(|r| matches!(r.kind, RecordKind::InstanceStarted(_)))
        .collect();
    assert_eq!(instances.len(), 2);
    assert_ne!(instances[0].instance, instances[1].instance);
    for instance in instances {
        let records: Vec<_> = snapshot
            .records
            .iter()
            .filter(|r| r.instance == instance.instance)
            .collect();
        assert!(
            records
                .iter()
                .any(|r| matches!(r.kind, RecordKind::IssuedSetup { .. }))
        );
        for stage in [
            SettingsStage::Requested,
            SettingsStage::ConstructionStarted,
            SettingsStage::Constructed,
        ] {
            let record = records
                .iter()
                .find(|r| {
                    r.kind
                        == RecordKind::BackendValues {
                            stage,
                            backend: rumoca_solver::NumericalBackend::Bdf,
                            tag: 1,
                        }
                })
                .unwrap();
            assert!(record.operation.is_some());
            assert_eq!(snapshot.cells[record.cells.start], opts.rtol.to_bits());
            assert_eq!(record.cells.len(), 2 + model.state_scalar_count());
        }
        assert!(records.iter().any(|r| r.kind == RecordKind::Initialized));
    }
}

#[test]
fn exhausted_recording_does_not_change_the_real_bdf_result() {
    let opts = sim_opts(SimExecutionPolicy::Interpreter);
    let model = state_fixture(&opts);
    let plain = build_simulation_artifact(model.artifact(), &opts, None, None)
        .unwrap()
        .run()
        .unwrap();
    let collector = SettingsCollector::new(1, 0).unwrap();
    let prepared =
        build_simulation_artifact(model.artifact(), &opts, None, Some(&collector)).unwrap();
    let actual = simulate_prepared_with_observation(&prepared, Some(&collector)).unwrap();
    drop(prepared);
    assert_same_result(&actual, &plain);
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::Capacity));
    assert_eq!(snapshot.records.len(), 1);
    assert!(snapshot.cells.is_empty());
}

#[test]
fn time_only_route_reports_no_backend_as_a_complete_negative_fact() {
    let opts = sim_opts(SimExecutionPolicy::Interpreter);
    let model = zero_state_fixture(&opts);
    let plain = build_simulation_artifact(model.artifact(), &opts, None, None)
        .unwrap()
        .run()
        .unwrap();
    let collector = SettingsCollector::new(64, 64).unwrap();
    let prepared =
        build_simulation_artifact(model.artifact(), &opts, None, Some(&collector)).unwrap();
    let actual = simulate_prepared_with_observation(&prepared, Some(&collector)).unwrap();
    drop(prepared);
    assert_same_result(&actual, &plain);
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    assert!(snapshot.records.iter().any(|r| r.kind
        == RecordKind::NoNumericalBackend(
            rumoca_solver::fmi_me::numerical_observation::ObservationRoute::ModelExchangeSession
        )));
    assert!(!snapshot.records.iter().any(|r| matches!(
        r.kind,
        RecordKind::BackendValues {
            stage: _,
            backend: rumoca_solver::NumericalBackend::Bdf,
            tag: 1
        }
    )));
}

#[test]
fn actual_bdf_reinitialization_keeps_one_instance_without_reissuing_setup() {
    let opts = sim_opts(SimExecutionPolicy::Interpreter);
    let model = state_fixture(&opts);
    let artifact = model.artifact();
    let retained = rumoca_solver::fmi_me::session::MeRetainedComponent::instantiate(
        artifact.source(),
        &crate::me_backend::instance_config("observer-reset", &opts)
            .unwrap()
            .with_model_state_jacobian_request(super::super::BDF_INTEGRATOR.model_jacobian_request),
        None,
    )
    .unwrap();
    let host = retained
        .into_lease(crate::me_backend::batch_options(&artifact.root_location(), &opts).unwrap())
        .unwrap();
    let collector = SettingsCollector::new(128, 1024).unwrap();
    let (plugin, writer) = crate::me_backend::plugin_for_host_with_observation(
        &host,
        &opts,
        super::super::BDF_INTEGRATOR,
        Some(&collector),
        rumoca_solver::fmi_me::numerical_observation::ConstructionPurpose::SelectedRun,
    )
    .unwrap();
    let mut session = host
        .into_session_with_numerical_observation(plugin, writer)
        .unwrap();
    let before: Vec<_> = session
        .output_values()
        .unwrap()
        .iter()
        .map(|v| v.to_bits())
        .collect();
    session.reset(opts.t_start).unwrap();
    let after: Vec<_> = session
        .output_values()
        .unwrap()
        .iter()
        .map(|v| v.to_bits())
        .collect();
    assert_eq!(before, after);
    drop(session);
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    assert_eq!(
        snapshot
            .records
            .iter()
            .filter(|r| matches!(r.kind, RecordKind::InstanceStarted(_)))
            .count(),
        1
    );
    assert_eq!(
        snapshot
            .records
            .iter()
            .filter(|r| matches!(r.kind, RecordKind::IssuedSetup { .. }))
            .count(),
        1
    );
    let stored: Vec<_> = snapshot
        .records
        .iter()
        .filter(|r| {
            r.kind
                == RecordKind::BackendValues {
                    stage: SettingsStage::Constructed,
                    backend: rumoca_solver::NumericalBackend::Bdf,
                    tag: 1,
                }
        })
        .collect();
    assert_eq!(stored.len(), 2);
    assert_eq!(
        (stored[0].operation, stored[1].operation),
        (Some(1), Some(2))
    );
    assert_eq!(stored[0].instance, stored[1].instance);
    assert_eq!(
        snapshot.cells[stored[0].cells.clone()],
        snapshot.cells[stored[1].cells.clone()]
    );
    assert_eq!(
        snapshot
            .records
            .iter()
            .filter(|r| r.kind == RecordKind::Initialized)
            .count(),
        2
    );
    assert!(
        !snapshot.records.iter().any(|r| r.kind
            == RecordKind::BackendValues {
                stage: SettingsStage::RetainedAfterReset,
                backend: rumoca_solver::NumericalBackend::Bdf,
                tag: 1
            }),
        "whole reinitialization is not an in-place reset readback"
    );
}

#[test]
fn located_root_reset_reports_retained_problem_before_history_reinitialization() {
    let mut opts = sim_opts(SimExecutionPolicy::Interpreter);
    opts.t_end = 0.12;
    let model = super::lower(
        "model ObservedRootReset\n  Real x(start = 0, fixed = true, nominal = 2);\nequation\n  der(x) = 1;\n  when x > 0.05 then\n    reinit(x, 0);\n  end when;\nend ObservedRootReset;\n",
        "ObservedRootReset",
        &opts,
    );
    let plain = build_simulation_artifact(model.artifact(), &opts, None, None)
        .unwrap()
        .run()
        .unwrap();
    let collector = SettingsCollector::new(512, 4096).unwrap();
    let prepared =
        build_simulation_artifact(model.artifact(), &opts, None, Some(&collector)).unwrap();
    let actual = simulate_prepared_with_observation(&prepared, Some(&collector)).unwrap();
    drop(prepared);
    assert_same_result(&actual, &plain);
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    let resets: Vec<_> = snapshot
        .records
        .iter()
        .filter(|r| {
            r.kind
                == RecordKind::OperationStarted(
                    rumoca_solver::fmi_me::numerical_observation::LifecycleCall::TruncateReset,
                )
        })
        .collect();
    assert!(
        !resets.is_empty(),
        "the fixture must reach a real in-place reset"
    );
    for reset in resets {
        let stored = snapshot
            .records
            .iter()
            .find(|r| {
                r.instance == reset.instance
                    && r.kind
                        == RecordKind::BackendValues {
                            stage: SettingsStage::Constructed,
                            backend: rumoca_solver::NumericalBackend::Bdf,
                            tag: 1,
                        }
            })
            .unwrap();
        let reset_stored = snapshot
            .records
            .iter()
            .find(|r| {
                r.instance == reset.instance
                    && r.operation == reset.operation
                    && r.kind
                        == RecordKind::BackendValues {
                            stage: SettingsStage::RetainedAfterReset,
                            backend: rumoca_solver::NumericalBackend::Bdf,
                            tag: 1,
                        }
            })
            .unwrap();
        assert_eq!(
            snapshot.cells[stored.cells.clone()],
            snapshot.cells[reset_stored.cells.clone()]
        );
        assert!(snapshot.records.iter().any(|r| r.instance == reset.instance
            && r.operation == reset.operation
            && r.kind == RecordKind::OperationFinished { success: true }));
        assert!(
            !snapshot.records.iter().any(|r| r.instance == reset.instance
                && r.operation == reset.operation
                && matches!(
                    r.kind,
                    RecordKind::BackendValues {
                        stage: SettingsStage::Requested
                            | SettingsStage::ConstructionStarted
                            | SettingsStage::Constructed,
                        backend: rumoca_solver::NumericalBackend::Bdf,
                        tag: 1
                    } | RecordKind::Initialized
                )),
            "an in-place reset does not rebuild the problem"
        );
        assert!(
            snapshot.records.iter().any(|r| r.instance == reset.instance
                && r.operation > reset.operation
                && r.kind == RecordKind::Initialized),
            "the root event subsequently reinitializes history under a later operation"
        );
    }
}

#[test]
fn public_observed_batch_facade_runs_the_same_bdf_result() {
    let mut compiler =
        rumoca_compile::compile::Session::new(rumoca_compile::compile::SessionConfig::default());
    compiler
        .add_document(
            "observed_facade.mo",
            "model ObservedFacade
 Real x(start=1, fixed=true);
equation
 der(x)=-x;
end ObservedFacade;",
        )
        .unwrap();
    let dae = compiler.compile_model("ObservedFacade").unwrap().dae;
    let opts = sim_opts(SimExecutionPolicy::Interpreter);
    let plain = crate::diffsol::build_simulation(&dae, &opts)
        .unwrap()
        .run()
        .unwrap();
    let collector = SettingsCollector::new(512, 4096).unwrap();
    let observed = crate::simulate_with_numerical_observation(&dae, &opts, &collector).unwrap();
    assert_same_result(&observed, &plain);
    assert_eq!(observed.n_states, 1);
    assert!(observed.times.len() > 1);
    assert!(observed.times.last().unwrap() > observed.times.first().unwrap());
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    assert_eq!(
        snapshot
            .records
            .iter()
            .filter(|record| record.kind == RecordKind::Initialized)
            .count(),
        2,
    );
    assert!(snapshot.records.iter().any(|record| {
        record.kind
            == RecordKind::BackendValues {
                stage: SettingsStage::Constructed,
                backend: rumoca_solver::NumericalBackend::Bdf,
                tag: 1,
            }
    }));
}
