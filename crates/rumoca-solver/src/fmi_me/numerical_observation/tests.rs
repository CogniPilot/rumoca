use super::*;

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
    other_adapter.parameters(ParameterStage::Prepared, 1.0e-6, 0.01, &[1.0e-10]);
    adapter.parameters(ParameterStage::Prepared, 1.0e-6, 0.01, &[1.0e-10]);
    operation.finish(true);
    adapter.initialized();
    drop((host, adapter, other_host, other_adapter));
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::OperationContext));
    let parameters: Vec<_> = snapshot
        .records
        .iter()
        .filter(|record| matches!(record.kind, RecordKind::Parameters(_)))
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
    adapter.parameters(ParameterStage::ProblemStored, 1.0e-6, 0.01, &[1.0e-10]);
    outer.finish(true);
    drop((host, adapter));
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::OperationContext));
    assert!(snapshot.has(IncompleteReason::UnfinishedOperation));
    assert!(
        !snapshot
            .records
            .iter()
            .any(|record| matches!(record.kind, RecordKind::Parameters(_)))
    );
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
    adapter.parameters(ParameterStage::Prepared, 1.0e-6, 0.01, &[1.0e-10]);
    operation.finish(false);
    drop((host, adapter));
    let snapshot = collector.finish();
    assert!(snapshot.has(IncompleteReason::Capacity));
    assert!(snapshot.cells.is_empty());
    assert!(
        !snapshot
            .records
            .iter()
            .any(|record| matches!(record.kind, RecordKind::Parameters(_)))
    );
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
    adapter.parameters(
        ParameterStage::ProblemStored,
        -0.0,
        f64::NEG_INFINITY,
        &values,
    );
    operation.finish(true);
    drop((host, adapter));
    let snapshot = collector.finish();
    assert_eq!(snapshot.incomplete, 0);
    let record = snapshot
        .records
        .iter()
        .find(|record| record.kind == RecordKind::Parameters(ParameterStage::ProblemStored))
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
    adapter.parameters(ParameterStage::Prepared, 1.0e-6, 0.01, &[2.0e-10, 3.0e-10]);
    first.finish(false);
    let second = host.begin(LifecycleCall::Initialize).unwrap();
    adapter.parameters(ParameterStage::Prepared, 1.0e-6, 0.01, &[2.0e-10, 3.0e-10]);
    adapter.parameters(
        ParameterStage::BuilderInvoked,
        1.0e-6,
        0.01,
        &[2.0e-10, 3.0e-10],
    );
    adapter.parameters(
        ParameterStage::ProblemStored,
        1.0e-6,
        0.01,
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
    adapter.parameters(
        ParameterStage::ResetStored,
        1.0e-6,
        0.01,
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
        .find(|r| r.kind == RecordKind::Parameters(ParameterStage::ResetStored))
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
