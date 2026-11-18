//! Generic facade controls, including builds with only the RK plugin enabled.

use super::*;
use rumoca_compile::compile::{Session, SessionConfig};
use rumoca_solver::fmi_me::numerical_observation::{
    ConstructionPurpose, IncompleteReason, RecordKind, SettingsCollector, SettingsStage,
};

pub(crate) fn model() -> std::sync::Arc<dae::Dae> {
    let mut compiler = Session::new(SessionConfig::default());
    compiler.add_document(
        "observed.mo",
        "model Observed Real x(start=1, fixed=true, nominal=3); equation der(x)=-x; end Observed;",
    ).unwrap();
    compiler.compile_model("Observed").unwrap().dae
}

fn options(mode: SimSolverMode) -> SimOptions {
    SimOptions {
        solver_mode: mode,
        execution_policy: SimExecutionPolicy::Interpreter,
        t_end: 0.02,
        dt: Some(0.01),
        ..Default::default()
    }
}

fn assert_equal(actual: &SimResult, expected: &SimResult) {
    assert_eq!(actual.names, expected.names);
    assert_eq!(actual.n_states, expected.n_states);
    assert_eq!(
        actual.times.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
        expected
            .times
            .iter()
            .map(|v| v.to_bits())
            .collect::<Vec<_>>()
    );
    assert_eq!(actual.data.len(), expected.data.len());
    for (actual, expected) in actual.data.iter().zip(&expected.data) {
        assert_eq!(
            actual.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            expected.iter().map(|v| v.to_bits()).collect::<Vec<_>>()
        );
    }
    assert_eq!(
        format!("{:?}", actual.variable_meta),
        format!("{:?}", expected.variable_meta)
    );
    assert_eq!(
        format!("{:?}", actual.termination),
        format!("{:?}", expected.termination)
    );
    assert_eq!(
        format!("{:?}", actual.diagnostics),
        format!("{:?}", expected.diagnostics)
    );
}

#[test]
fn public_rk_observation_keeps_results_and_reports_actual_configuration() {
    let model = model();
    let opts = options(SimSolverMode::RkLike);
    let expected = simulate_with_diagnostics(&model, &opts).unwrap();
    let collector = SettingsCollector::new(128, 1024).unwrap();
    let actual = simulate_with_numerical_observation(&model, &opts, &collector).unwrap();
    assert_equal(&actual, &expected);
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    assert!(!snapshot.has(IncompleteReason::WritersStillLive));
    assert!(!snapshot.has(IncompleteReason::OperationContext));
    assert!(snapshot.records.iter().any(|record| record.kind
        == RecordKind::BackendConstruction {
            backend: NumericalBackend::Rk45,
            purpose: ConstructionPurpose::SelectedRun,
            stage: SettingsStage::Constructed,
        }));
    assert!(!snapshot.records.iter().any(|record| matches!(
        record.kind,
        RecordKind::BackendConstruction {
            backend: NumericalBackend::Bdf,
            ..
        }
    )));
    use rumoca_solver::fmi_me::numerical_observation::{SettingAvailability, SettingField};
    for (stage, field, expected) in [
        (
            SettingsStage::Requested,
            SettingField::RelativeTolerance,
            opts.rtol,
        ),
        (
            SettingsStage::Requested,
            SettingField::ScalarAbsoluteTolerance,
            opts.atol,
        ),
        (SettingsStage::Requested, SettingField::StateNominals, 3.0),
        (SettingsStage::Constructed, SettingField::InitialStep, 0.01),
        (
            SettingsStage::Constructed,
            SettingField::StateAbsoluteTolerances,
            opts.atol * 3.0,
        ),
    ] {
        let record = snapshot
            .records
            .iter()
            .find(|record| {
                record.kind
                    == RecordKind::Setting {
                        stage,
                        field,
                        availability: SettingAvailability::Known,
                    }
            })
            .unwrap();
        assert_eq!(snapshot.cells[record.cells.clone()], [expected.to_bits()]);
    }
}

#[cfg(feature = "solver-velimplicit")]
#[test]
fn unsupported_readback_keeps_the_selected_non_bdf_numerical_path() {
    let model = model();
    let opts = options(SimSolverMode::VelImplicit);
    let expected = simulate_with_diagnostics(&model, &opts).unwrap();
    let collector = SettingsCollector::new(128, 1024).unwrap();
    let actual = simulate_with_numerical_observation(&model, &opts, &collector).unwrap();
    assert_equal(&actual, &expected);
    let snapshot = collector.finish();
    assert_eq!(
        snapshot.incomplete,
        IncompleteReason::UnsupportedObservation as u16
    );
    assert!(snapshot.records.iter().any(|record| record.kind
        == RecordKind::BackendConstruction {
            backend: NumericalBackend::VelImplicit,
            purpose: ConstructionPurpose::SelectedRun,
            stage: SettingsStage::Constructed,
        }));
    assert!(!snapshot.records.iter().any(|record| matches!(
        record.kind,
        RecordKind::BackendConstruction {
            backend: NumericalBackend::Bdf,
            ..
        }
    )));
}

#[test]
fn exhausted_diagnostic_storage_does_not_change_public_numerical_results() {
    let model = model();
    let opts = options(SimSolverMode::RkLike);
    let expected = simulate_with_diagnostics(&model, &opts).unwrap();
    let collector = SettingsCollector::new(0, 0).unwrap();
    let actual = simulate_with_numerical_observation(&model, &opts, &collector).unwrap();
    assert_equal(&actual, &expected);
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, IncompleteReason::Capacity as u16);
    assert!(snapshot.records.is_empty());
    assert!(snapshot.cells.is_empty());
}

#[cfg(not(feature = "solver-diffsol"))]
#[test]
fn rk_only_auto_observation_never_constructs_or_probes_bdf() {
    let model = model();
    let opts = options(SimSolverMode::Auto);
    let expected = simulate_with_diagnostics(&model, &opts).unwrap();
    let collector = SettingsCollector::new(128, 1024).unwrap();
    let actual = simulate_with_numerical_observation(&model, &opts, &collector).unwrap();
    assert_equal(&actual, &expected);
    let snapshot = collector.finish();
    let constructed: Vec<_> = snapshot
        .records
        .iter()
        .filter_map(|record| match record.kind {
            RecordKind::BackendConstruction {
                backend,
                purpose,
                stage: SettingsStage::Constructed,
            } => Some((backend, purpose)),
            _ => None,
        })
        .collect();
    assert_eq!(
        constructed,
        [(NumericalBackend::Rk45, ConstructionPurpose::SelectedRun)]
    );
}

#[cfg(feature = "solver-diffsol")]
#[test]
fn generic_auto_observation_labels_actual_probe_and_selected_bdf_separately() {
    let model = model();
    let opts = options(SimSolverMode::Auto);
    let expected = simulate_with_diagnostics(&model, &opts).unwrap();
    let collector = SettingsCollector::new(1024, 8192).unwrap();
    let actual = simulate_with_numerical_observation(&model, &opts, &collector).unwrap();
    assert_equal(&actual, &expected);
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    let constructed: Vec<_> = snapshot
        .records
        .iter()
        .filter_map(|record| match record.kind {
            RecordKind::BackendConstruction {
                backend,
                purpose,
                stage: SettingsStage::Constructed,
            } => Some((backend, purpose)),
            _ => None,
        })
        .collect();
    assert_eq!(
        constructed,
        [
            (
                NumericalBackend::Bdf,
                ConstructionPurpose::AutoCapabilityProbe
            ),
            (NumericalBackend::Bdf, ConstructionPurpose::SelectedRun),
        ]
    );
}

#[test]
fn invalid_typed_options_refuse_before_lowering_or_recording_on_both_facades() {
    let model = model();
    let mut opts = options(SimSolverMode::RkLike);
    opts.backend_options = Some(BackendOptions::new(NumericalBackend::Rk45, 99_u32));
    // This would be a distinct lowering/override failure if request admission
    // happened after compiler/model work.
    opts.param_overrides.push(("not_a_parameter".into(), 3.0));
    let collector = SettingsCollector::new(128, 1024).unwrap();
    for error in [
        simulate_with_diagnostics(&model, &opts).unwrap_err(),
        simulate_with_numerical_observation(&model, &opts, &collector).unwrap_err(),
    ] {
        assert!(matches!(
            error,
            SimulationDiagnosticError::BackendOptions(BackendOptionsError::ConcreteType {
                backend: NumericalBackend::Rk45
            })
        ));
        let cause = std::error::Error::source(&error).unwrap();
        assert!(cause.downcast_ref::<BackendOptionsError>().is_some());
        assert_eq!(error.diagnostic_code(), "EX002");
    }
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    assert!(snapshot.records.is_empty());
}

#[cfg(not(feature = "solver-diffsol"))]
#[test]
fn unavailable_bdf_refuses_before_lowering_or_observation_on_both_facades() {
    let model = model();
    let mut opts = options(SimSolverMode::Bdf);
    opts.backend_options = Some(BackendOptions::new(NumericalBackend::Bdf, 99_u32));
    // Availability must refuse even before concrete-payload or model checks.
    opts.param_overrides.push(("not_a_parameter".into(), 3.0));
    let collector = SettingsCollector::new(128, 1024).unwrap();
    for error in [
        simulate_with_diagnostics(&model, &opts).unwrap_err(),
        simulate_with_numerical_observation(&model, &opts, &collector).unwrap_err(),
    ] {
        assert!(matches!(
            error,
            SimulationDiagnosticError::BackendOptions(BackendOptionsError::BackendUnavailable {
                backend: NumericalBackend::Bdf
            })
        ));
        assert_eq!(error.diagnostic_code(), "EX002");
        assert!(
            std::error::Error::source(&error)
                .unwrap()
                .downcast_ref::<BackendOptionsError>()
                .is_some()
        );
    }
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    assert!(snapshot.records.is_empty());
}

fn failing_model() -> std::sync::Arc<dae::Dae> {
    let mut compiler = Session::new(SessionConfig::default());
    compiler
        .add_document(
            "observed_failure.mo",
            "model ObservedFailure Real x(start=1, fixed=true); \
             equation der(x)=-x; \
             assert(time < 0.01, \"typed observer failure\"); \
             end ObservedFailure;",
        )
        .unwrap();
    compiler.compile_model("ObservedFailure").unwrap().dae
}

fn assert_typed_component_cause(error: &SimulationDiagnosticError) {
    use rumoca_solver::fmi_me::{MeError, MeStage, session::MeSessionError};

    let SimulationDiagnosticError::Simulation(simulation) = error else {
        panic!("expected the actual numerical cause, got {error:?}");
    };
    let SimError::ModelExchangeSession(MeSessionError::Component(component)) = simulation.kind()
    else {
        panic!("expected the actual ME component cause, got {simulation:?}");
    };
    assert!(matches!(
        component.kind(),
        MeError::Assertion { time, message }
            if *time >= 0.01 && message == "typed observer failure"
    ));
    assert_eq!(component.stage(), Some(MeStage::EventIteration));
    assert_eq!(error.diagnostic_code(), "EX001");
    assert!(
        std::error::Error::source(error)
            .unwrap()
            .downcast_ref::<SimError>()
            .is_some()
    );
}

#[test]
fn public_observation_preserves_a_real_typed_model_failure_and_stage() {
    let model = failing_model();
    let opts = options(SimSolverMode::RkLike);
    let plain = simulate_with_diagnostics(&model, &opts).unwrap_err();
    let collector = SettingsCollector::new(128, 1024).unwrap();
    let observed = simulate_with_numerical_observation(&model, &opts, &collector).unwrap_err();
    assert_typed_component_cause(&plain);
    assert_typed_component_cause(&observed);
    assert_eq!(plain.to_string(), observed.to_string());
    let snapshot = collector.finish();
    assert!(!snapshot.has(IncompleteReason::WritersStillLive));
    assert!(!snapshot.has(IncompleteReason::OperationContext));
    assert!(!snapshot.has(IncompleteReason::UnfinishedOperation));
}
