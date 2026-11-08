use super::*;

#[test]
fn factory_metadata_separates_auto_probe_preparation_and_selected_run() {
    let collector = SettingsCollector::new(32, 0).unwrap();
    for purpose in [
        ConstructionPurpose::AutoCapabilityProbe,
        ConstructionPurpose::PreparationCheck,
        ConstructionPurpose::SelectedRun,
    ] {
        let (host, adapter) = collector
            .associate(ObservationRoute::ModelExchangeSession)
            .unwrap();
        host.construction(
            crate::NumericalBackend::Bdf,
            purpose,
            SettingsStage::ConstructionStarted,
        );
        host.construction(
            crate::NumericalBackend::Bdf,
            purpose,
            SettingsStage::Constructed,
        );
        drop((host, adapter));
    }
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    let actual: Vec<_> = snapshot
        .records
        .iter()
        .filter_map(|record| match record.kind {
            RecordKind::BackendConstruction {
                backend,
                purpose,
                stage,
            } => {
                assert_eq!(backend, crate::NumericalBackend::Bdf);
                assert_eq!(record.operation, None);
                assert!(record.cells.is_empty());
                Some((record.instance, purpose, stage))
            }
            _ => None,
        })
        .collect();
    assert_eq!(
        actual,
        [
            (
                0,
                ConstructionPurpose::AutoCapabilityProbe,
                SettingsStage::ConstructionStarted
            ),
            (
                0,
                ConstructionPurpose::AutoCapabilityProbe,
                SettingsStage::Constructed
            ),
            (
                1,
                ConstructionPurpose::PreparationCheck,
                SettingsStage::ConstructionStarted
            ),
            (
                1,
                ConstructionPurpose::PreparationCheck,
                SettingsStage::Constructed
            ),
            (
                2,
                ConstructionPurpose::SelectedRun,
                SettingsStage::ConstructionStarted
            ),
            (
                2,
                ConstructionPurpose::SelectedRun,
                SettingsStage::Constructed
            ),
        ]
    );
}

#[test]
fn no_numerical_backend_is_a_complete_negative_fact_with_no_live_endpoints() {
    let collector = SettingsCollector::new(1, 0).unwrap();
    collector.no_numerical_backend(ObservationRoute::ModelExchangeSession);
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    assert!(snapshot.cells.is_empty());
    assert_eq!(snapshot.records.len(), 1);
    assert_eq!(
        snapshot.records[0].kind,
        RecordKind::NoNumericalBackend(ObservationRoute::ModelExchangeSession)
    );

    let collector = SettingsCollector::new(0, 0).unwrap();
    collector.no_numerical_backend(ObservationRoute::StandaloneOde);
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::Capacity));
    assert!(snapshot.records.is_empty());
}

fn setup() -> crate::fmi_me::MeNumericalSetup {
    crate::fmi_me::MeNumericalSetup::new(1.0e-6, 1.0e-10, vec![2.0, 3.0], 2, Some(0.01)).unwrap()
}

#[test]
fn equal_numeric_setup_clones_do_not_share_construction_associations() {
    let collector = SettingsCollector::new(32, 64).unwrap();
    let numerical = setup();
    let copied = numerical.clone();
    let (first, first_adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    let (second, second_adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    first.issued_setup(&numerical);
    second.issued_setup(&copied);
    drop((first, first_adapter, second, second_adapter));
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    let issued: Vec<_> = snapshot
        .records
        .iter()
        .filter(|record| matches!(record.kind, RecordKind::IssuedSetup { .. }))
        .collect();
    assert_eq!(issued.len(), 2);
    assert_ne!(issued[0].instance, issued[1].instance);
    assert_eq!(
        snapshot.cells[issued[0].cells.clone()],
        snapshot.cells[issued[1].cells.clone()]
    );
}

#[test]
fn adapter_reports_only_under_its_own_host_operation() {
    let collector = SettingsCollector::new(32, 64).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    let (other_host, other_adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    let operation = host.begin(LifecycleCall::Initialize).unwrap();
    other_adapter.backend_values(
        SettingsStage::Requested,
        crate::NumericalBackend::Bdf,
        1,
        &[1.0e-6, 0.01],
        &[1.0e-10],
    );
    adapter.backend_values(
        SettingsStage::Requested,
        crate::NumericalBackend::Bdf,
        1,
        &[1.0e-6, 0.01],
        &[1.0e-10],
    );
    operation.finish(true);
    adapter.initialized();
    drop((host, adapter, other_host, other_adapter));
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::OperationContext));
    let parameters: Vec<_> = snapshot
        .records
        .iter()
        .filter(|record| {
            matches!(
                record.kind,
                RecordKind::BackendValues {
                    stage: _,
                    backend: crate::NumericalBackend::Bdf,
                    tag: 1
                }
            )
        })
        .collect();
    assert_eq!(parameters.len(), 1);
    assert_eq!(parameters[0].instance, 0);
    assert_eq!(parameters[0].operation, Some(1));
    assert!(
        !snapshot
            .records
            .iter()
            .any(|record| record.kind == RecordKind::Initialized)
    );
}

#[test]
fn nested_context_cannot_be_mistaken_for_the_outer_operation() {
    let collector = SettingsCollector::new(32, 64).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    let outer = host.begin(LifecycleCall::Initialize).unwrap();
    assert!(host.begin(LifecycleCall::TruncateReset).is_none());
    adapter.backend_values(
        SettingsStage::Constructed,
        crate::NumericalBackend::Bdf,
        1,
        &[1.0e-6, 0.01],
        &[1.0e-10],
    );
    outer.finish(true);
    drop((host, adapter));
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::OperationContext));
    assert!(snapshot.has(IncompleteReason::UnfinishedOperation));
    assert!(!snapshot.records.iter().any(|record| matches!(
        record.kind,
        RecordKind::BackendValues {
            stage: _,
            backend: crate::NumericalBackend::Bdf,
            tag: 1
        }
    )));
}

#[test]
fn abandoned_operation_is_incomplete_without_a_success_record() {
    let collector = SettingsCollector::new(16, 16).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    drop(host.begin(LifecycleCall::Initialize));
    drop((host, adapter));
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::UnfinishedOperation));
    assert!(
        !snapshot
            .records
            .iter()
            .any(|record| matches!(record.kind, RecordKind::OperationFinished { .. }))
    );
}

#[test]
fn record_and_raw_cell_exhaustion_are_sticky_and_whole_record_atomic() {
    let collector = SettingsCollector::new(16, 2).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    let operation = host.begin(LifecycleCall::Initialize).unwrap();
    adapter.backend_values(
        SettingsStage::Requested,
        crate::NumericalBackend::Bdf,
        1,
        &[1.0e-6, 0.01],
        &[1.0e-10],
    );
    operation.finish(false);
    drop((host, adapter));
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::Capacity));
    assert!(snapshot.cells.is_empty());
    assert!(!snapshot.records.iter().any(|record| matches!(
        record.kind,
        RecordKind::BackendValues {
            stage: _,
            backend: crate::NumericalBackend::Bdf,
            tag: 1
        }
    )));
    assert!(
        snapshot
            .records
            .iter()
            .any(|record| record.kind == RecordKind::OperationFinished { success: false })
    );

    let collector = SettingsCollector::new(1, 64).unwrap();
    let endpoints = collector
        .associate(ObservationRoute::StandaloneOde)
        .unwrap();
    assert!(
        collector
            .associate(ObservationRoute::StandaloneOde)
            .is_none()
    );
    drop(endpoints);
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::Capacity));
    assert_eq!(snapshot.records.len(), 1);
}

#[test]
fn all_scalar_and_vector_bits_survive_without_coordinate_or_finiteness_inference() {
    let collector = SettingsCollector::new(16, 64).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::StandaloneOde)
        .unwrap();
    let operation = host.begin(LifecycleCall::Initialize).unwrap();
    let values = [f64::from_bits(0x7ff8_0000_0000_0042), -0.0, f64::INFINITY];
    adapter.backend_values(
        SettingsStage::Constructed,
        crate::NumericalBackend::Bdf,
        1,
        &[-0.0, f64::NEG_INFINITY],
        &values,
    );
    operation.finish(true);
    drop((host, adapter));
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    let record = snapshot
        .records
        .iter()
        .find(|record| {
            record.kind
                == RecordKind::BackendValues {
                    stage: SettingsStage::Constructed,
                    backend: crate::NumericalBackend::Bdf,
                    tag: 1,
                }
        })
        .unwrap();
    assert_eq!(
        snapshot.cells[record.cells.clone()],
        [-0.0_f64, f64::NEG_INFINITY]
            .iter()
            .chain(&values)
            .map(|v| v.to_bits())
            .collect::<Vec<_>>()
    );
}

#[test]
fn borrow_conflict_returns_to_the_ordinary_path_without_waiting_or_panicking() {
    let collector = SettingsCollector::new(16, 64).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    let retained_borrow = collector.shared.storage.borrow_mut();
    assert!(host.begin(LifecycleCall::Initialize).is_none());
    host.issued_setup(&setup());
    drop(retained_borrow);
    drop((host, adapter));
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::BorrowConflict));
}

#[test]
fn collection_with_live_endpoints_and_unsupported_routes_is_not_complete() {
    let collector = SettingsCollector::new(16, 16).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::StandaloneOde)
        .unwrap();
    adapter.unavailable();
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::UnsupportedObservation));
    assert!(snapshot.has(IncompleteReason::WritersStillLive));
    drop((host, adapter));
}

#[test]
fn prepared_builder_stored_and_initialized_are_distinct_ordered_records() {
    let collector = SettingsCollector::new(32, 64).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    host.issued_setup(&setup());
    let first = host.begin(LifecycleCall::Initialize).unwrap();
    adapter.backend_values(
        SettingsStage::Requested,
        crate::NumericalBackend::Bdf,
        1,
        &[1.0e-6, 0.01],
        &[2.0e-10, 3.0e-10],
    );
    first.finish(false);
    let second = host.begin(LifecycleCall::Initialize).unwrap();
    adapter.backend_values(
        SettingsStage::Requested,
        crate::NumericalBackend::Bdf,
        1,
        &[1.0e-6, 0.01],
        &[2.0e-10, 3.0e-10],
    );
    adapter.backend_values(
        SettingsStage::ConstructionStarted,
        crate::NumericalBackend::Bdf,
        1,
        &[1.0e-6, 0.01],
        &[2.0e-10, 3.0e-10],
    );
    adapter.backend_values(
        SettingsStage::Constructed,
        crate::NumericalBackend::Bdf,
        1,
        &[1.0e-6, 0.01],
        &[2.0e-10, 3.0e-10],
    );
    adapter.initialized();
    second.finish(true);
    drop((host, adapter));
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    assert_eq!(
        snapshot
            .records
            .iter()
            .filter(|record| record.operation == Some(1))
            .count(),
        4
    );
    assert_eq!(
        snapshot
            .records
            .iter()
            .filter(|record| record.operation == Some(2))
            .count(),
        6
    );
}

#[test]
fn changed_host_policy_does_not_relabel_retained_adapter_values() {
    let collector = SettingsCollector::new(32, 64).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    host.issued_setup(&setup());
    host.current_policy(1.0e-6, 1.0e-10, &[23.0, 42.0]);
    let operation = host.begin(LifecycleCall::TruncateReset).unwrap();
    adapter.backend_values(
        SettingsStage::RetainedAfterReset,
        crate::NumericalBackend::Bdf,
        1,
        &[1.0e-6, 0.01],
        &[2.0e-10, 3.0e-10],
    );
    operation.finish(true);
    drop((host, adapter));
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    let policy = snapshot
        .records
        .iter()
        .find(|r| matches!(r.kind, RecordKind::HostPolicy { .. }))
        .unwrap();
    let retained = snapshot
        .records
        .iter()
        .find(|r| {
            r.kind
                == RecordKind::BackendValues {
                    stage: SettingsStage::RetainedAfterReset,
                    backend: crate::NumericalBackend::Bdf,
                    tag: 1,
                }
        })
        .unwrap();
    assert_eq!(policy.operation, None);
    assert_eq!(retained.operation, Some(1));
    assert_eq!(
        snapshot.cells[policy.cells.clone()],
        [1.0e-6, 1.0e-10, 23.0, 42.0].map(f64::to_bits)
    );
    assert_eq!(
        snapshot.cells[retained.cells.clone()],
        [1.0e-6, 0.01, 2.0e-10, 3.0e-10].map(f64::to_bits)
    );
}

#[test]
fn impossible_record_or_cell_capacity_refuses_before_any_endpoint_exists() {
    assert!(SettingsCollector::new(usize::MAX, 0).is_err());
    assert!(SettingsCollector::new(0, usize::MAX).is_err());
    let collector = SettingsCollector::new(0, 0).unwrap();
    assert!(
        collector
            .associate(ObservationRoute::StandaloneOde)
            .is_none()
    );
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::Capacity));
    assert!(snapshot.records.is_empty());
    assert!(snapshot.cells.is_empty());
}

#[test]
fn unavailable_host_policy_does_not_invent_parameters_or_replace_operation_outcome() {
    let collector = SettingsCollector::new(16, 16).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    host.unavailable();
    let operation = host.begin(LifecycleCall::Initialize).unwrap();
    operation.finish(true);
    drop((host, adapter));
    let snapshot = collector.finish();
    assert_eq!(
        snapshot.incomplete,
        IncompleteReason::UnsupportedObservation as u16
    );
    assert!(snapshot.cells.is_empty());
    assert_eq!(
        snapshot
            .records
            .iter()
            .map(|record| record.kind)
            .collect::<Vec<_>>(),
        [
            RecordKind::InstanceStarted(ObservationRoute::ModelExchangeSession),
            RecordKind::OperationStarted(LifecycleCall::Initialize),
            RecordKind::OperationFinished { success: true },
            RecordKind::HostClosed,
            RecordKind::AdapterClosed,
        ],
    );
}

#[test]
fn neutral_fields_distinguish_raw_known_negative_and_unsupported_facts() {
    let collector = SettingsCollector::new(32, 32).unwrap();
    let (host, adapter) = collector
        .associate(ObservationRoute::ModelExchangeSession)
        .unwrap();
    let raw = [f64::from_bits(0x7ff8_0000_0000_0042), -0.0];
    adapter.setting(
        SettingsStage::Requested,
        SettingField::StateNominals,
        SettingValue::Known(&raw),
    );
    adapter.setting(
        SettingsStage::Constructed,
        SettingField::InitialStep,
        SettingValue::NotApplicable,
    );
    adapter.setting(
        SettingsStage::Constructed,
        SettingField::ScalarAbsoluteTolerance,
        SettingValue::Unsupported,
    );
    drop((adapter, host));
    let snapshot = collector.finish();
    assert_ne!(
        snapshot.incomplete & IncompleteReason::UnsupportedObservation as u16,
        0
    );
    let fields: Vec<_> = snapshot
        .records
        .iter()
        .filter(|r| matches!(r.kind, RecordKind::Setting { .. }))
        .collect();
    assert_eq!(fields.len(), 3);
    assert_eq!(
        &snapshot.cells[fields[0].cells.clone()],
        &[raw[0].to_bits(), raw[1].to_bits()]
    );
    assert!(fields[1].cells.is_empty());
    assert!(fields[2].cells.is_empty());
    assert!(matches!(
        fields[1].kind,
        RecordKind::Setting {
            availability: SettingAvailability::NotApplicable,
            ..
        }
    ));
    assert!(matches!(
        fields[2].kind,
        RecordKind::Setting {
            availability: SettingAvailability::Unsupported,
            ..
        }
    ));
}
