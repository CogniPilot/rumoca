//! A real pre-builder refusal through the public standalone ODE driver.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use rumoca_solver::fmi_me::{
    MeIntegrationError, MeNumericalFailure,
    numerical_observation::{
        FailureCategory, LifecycleCall, ObservationRoute, ParameterStage, RecordKind,
        SettingsCollector, SettingsSnapshot,
    },
    ode_driver::{ContinuousOde, OdeDriveError, OdeRun, integrate_ode},
};

struct DerivativeOnly(Rc<Cell<usize>>);

impl ContinuousOde for DerivativeOnly {
    fn width(&self) -> usize {
        2
    }

    fn derivatives_into(&self, _time: f64, state: &[f64], out: &mut [f64]) -> Result<(), String> {
        self.0.set(self.0.get() + 1);
        out.copy_from_slice(state);
        Ok(())
    }
}

fn pre_builder_refusal(observed: bool) -> (String, Option<SettingsSnapshot>) {
    pre_builder_refusal_with_run(observed, &ordinary_run())
}

fn ordinary_run() -> OdeRun {
    OdeRun {
        relative_tolerance: 1.0e-6,
        absolute_tolerance: 1.0e-10,
        nominals: vec![2.0, 3.0],
        initial_step: Some(0.01),
        t_start: 0.0,
        t_end: 0.1,
        initial_state: vec![1.0, 2.0],
        breakpoints: Vec::new(),
        extension_order_limit: None,
    }
}

fn pre_builder_refusal_with_run(
    observed: bool,
    run: &OdeRun,
) -> (String, Option<SettingsSnapshot>) {
    let collector = observed.then(|| SettingsCollector::new(32, 64).unwrap());
    let endpoints = collector
        .as_ref()
        .and_then(|c| c.associate(ObservationRoute::StandaloneOde));
    let (host, adapter) = match endpoints {
        Some((host, adapter)) => (Some(host), Some(adapter)),
        None => (None, None),
    };
    let adapter = RefCell::new(adapter);
    let operation = host
        .as_ref()
        .and_then(|w| w.begin(LifecycleCall::Initialize));
    let derivatives = Rc::new(Cell::new(0));
    let intervals = Cell::new(0);
    let failure = integrate_ode(
        Rc::new(DerivativeOnly(Rc::clone(&derivatives))),
        run,
        &|setup| {
            if let Some(writer) = &host {
                writer.issued_setup(&setup);
            }
            super::model_exchange_integrator_with_observation(setup, adapter.borrow_mut().take())
        },
        |_| {
            intervals.set(intervals.get() + 1);
            Ok(())
        },
    )
    .expect_err("a derived ODE provides no certified state-Jacobian relation for BDF");
    if let Some(operation) = operation {
        operation.finish_failure(FailureCategory::Numerical(MeNumericalFailure::Construction));
    }
    assert!(matches!(
        &failure,
        OdeDriveError::Integration(MeIntegrationError::Numerical {
            category: MeNumericalFailure::Construction,
            ..
        })
    ));
    assert!(
        failure
            .to_string()
            .contains("no certified state-Jacobian relation")
    );
    assert_eq!(
        derivatives.get(),
        0,
        "refusal precedes numerical evaluation"
    );
    assert_eq!(intervals.get(), 0, "no accepted interval is reported");
    drop((host, adapter));
    (
        failure.to_string(),
        collector.map(SettingsCollector::finish),
    )
}

#[test]
fn prepared_scaling_reports_underflow_and_overflow_clamps_without_inventing_a_problem() {
    for (atol, nominals, expected) in [
        (
            f64::MIN_POSITIVE,
            vec![f64::MIN_POSITIVE, 2.0],
            [f64::MIN_POSITIVE, f64::MIN_POSITIVE * 2.0],
        ),
        (f64::MAX, vec![2.0, 0.5], [f64::MAX, f64::MAX * 0.5]),
    ] {
        let mut run = ordinary_run();
        run.absolute_tolerance = atol;
        run.nominals = nominals;
        let (plain, _) = pre_builder_refusal_with_run(false, &run);
        let (observed, snapshot) = pre_builder_refusal_with_run(true, &run);
        assert_eq!(plain, observed);
        let snapshot = snapshot.unwrap();
        assert_eq!(snapshot.incomplete, 0);
        let prepared = snapshot
            .records
            .iter()
            .find(|r| r.kind == RecordKind::Parameters(ParameterStage::Prepared))
            .unwrap();
        assert_eq!(
            snapshot.cells[prepared.cells.clone()],
            [run.relative_tolerance, 0.01, expected[0], expected[1]].map(f64::to_bits)
        );
        assert!(!snapshot.records.iter().any(|r| matches!(
            r.kind,
            RecordKind::Parameters(
                ParameterStage::BuilderInvoked
                    | ParameterStage::ProblemStored
                    | ParameterStage::ResetStored
            ) | RecordKind::Initialized
        )));
    }
}

fn invalid_setup_refusal(observed: bool, run: &OdeRun) -> String {
    let collector = observed.then(|| SettingsCollector::new(32, 64).unwrap());
    let builders = Cell::new(0);
    let derivatives = Rc::new(Cell::new(0));
    let intervals = Cell::new(0);
    let failure = integrate_ode(
        Rc::new(DerivativeOnly(Rc::clone(&derivatives))),
        run,
        &|setup| {
            builders.set(builders.get() + 1);
            let writer = collector.as_ref().and_then(|c| {
                c.associate(ObservationRoute::StandaloneOde)
                    .map(|(host, adapter)| {
                        host.issued_setup(&setup);
                        adapter
                    })
            });
            super::model_exchange_integrator_with_observation(setup, writer)
        },
        |_| {
            intervals.set(intervals.get() + 1);
            Ok(())
        },
    )
    .expect_err("invalid setup must refuse before plugin construction");
    assert!(matches!(
        &failure,
        OdeDriveError::Integration(MeIntegrationError::Contract { .. })
    ));
    assert_eq!(builders.get(), 0);
    assert_eq!(derivatives.get(), 0);
    assert_eq!(intervals.get(), 0);
    if let Some(collector) = collector {
        let snapshot = collector.finish();
        assert!(snapshot.records.is_empty());
        assert!(snapshot.cells.is_empty());
        // Empty diagnostics are not an issued-setup or successful-construction certificate.
        assert_eq!(snapshot.incomplete, 0);
    }
    failure.to_string()
}

#[test]
fn invalid_numerical_setup_refuses_before_observed_factory_or_numerical_evaluation() {
    let mut cases = Vec::new();
    for value in [0.0, -1.0, f64::INFINITY, f64::NAN] {
        let mut run = ordinary_run();
        run.relative_tolerance = value;
        cases.push(run);
        let mut run = ordinary_run();
        run.absolute_tolerance = value;
        cases.push(run);
        let mut run = ordinary_run();
        run.nominals[1] = value;
        cases.push(run);
        let mut run = ordinary_run();
        run.initial_step = Some(value);
        cases.push(run);
    }
    let mut wrong_width = ordinary_run();
    wrong_width.nominals.pop();
    cases.push(wrong_width);
    for run in cases {
        assert_eq!(
            invalid_setup_refusal(false, &run),
            invalid_setup_refusal(true, &run)
        );
    }
}

fn invalid_point_after_factory(observed: bool, run: &OdeRun) -> String {
    let collector = observed.then(|| SettingsCollector::new(32, 64).unwrap());
    let host = RefCell::new(None);
    let builders = Cell::new(0);
    let derivatives = Rc::new(Cell::new(0));
    let intervals = Cell::new(0);
    let failure = integrate_ode(
        Rc::new(DerivativeOnly(Rc::clone(&derivatives))),
        run,
        &|setup| {
            builders.set(builders.get() + 1);
            let adapter = collector.as_ref().and_then(|collector| {
                collector
                    .associate(ObservationRoute::StandaloneOde)
                    .map(|(writer, adapter)| {
                        writer.issued_setup(&setup);
                        *host.borrow_mut() = Some(writer);
                        adapter
                    })
            });
            super::model_exchange_integrator_with_observation(setup, adapter)
        },
        |_| {
            intervals.set(intervals.get() + 1);
            Ok(())
        },
    )
    .expect_err("the checked initial point refuses before initialize");
    assert!(matches!(
        &failure,
        OdeDriveError::Integration(MeIntegrationError::Contract { .. })
    ));
    assert_eq!(builders.get(), 1);
    assert_eq!(derivatives.get(), 0);
    assert_eq!(intervals.get(), 0);
    drop(host);
    if let Some(collector) = collector {
        let snapshot = collector.finish();
        assert_eq!(snapshot.incomplete, 0);
        assert_eq!(
            snapshot
                .records
                .iter()
                .filter(|record| matches!(record.kind, RecordKind::IssuedSetup { .. }))
                .count(),
            1
        );
        assert!(snapshot.records.iter().all(|record| matches!(
            record.kind,
            RecordKind::InstanceStarted(ObservationRoute::StandaloneOde)
                | RecordKind::IssuedSetup { .. }
                | RecordKind::HostClosed
                | RecordKind::AdapterClosed
        )));
        assert!(
            snapshot
                .records
                .iter()
                .all(|record| record.operation.is_none())
        );
    }
    failure.to_string()
}

#[test]
fn invalid_initial_point_keeps_issued_setup_distinct_from_unstarted_initialization() {
    let mut nonfinite_time = ordinary_run();
    nonfinite_time.t_start = f64::INFINITY;
    let mut nonfinite_state = ordinary_run();
    nonfinite_state.initial_state[0] = f64::NAN;
    let mut wrong_width = ordinary_run();
    wrong_width.initial_state.pop();
    for run in [nonfinite_time, nonfinite_state, wrong_width] {
        assert_eq!(
            invalid_point_after_factory(false, &run),
            invalid_point_after_factory(true, &run)
        );
    }
}

#[test]
fn real_pre_builder_refusal_records_preparation_without_inventing_builder_or_solver() {
    let (plain, absent) = pre_builder_refusal(false);
    assert!(absent.is_none());
    let (observed, snapshot) = pre_builder_refusal(true);
    assert_eq!(observed, plain);
    let snapshot = snapshot.unwrap();
    assert_eq!(snapshot.incomplete, 0);
    let prepared = snapshot
        .records
        .iter()
        .find(|r| r.kind == RecordKind::Parameters(ParameterStage::Prepared))
        .unwrap();
    assert_eq!(prepared.operation, Some(1));
    assert_eq!(
        snapshot.cells[prepared.cells.clone()],
        [1.0e-6, 0.01, 1.0e-10 * 2.0, 1.0e-10 * 3.0].map(f64::to_bits)
    );
    assert!(
        snapshot
            .records
            .iter()
            .any(|r| r.kind == RecordKind::InstanceStarted(ObservationRoute::StandaloneOde))
    );
    assert!(
        snapshot
            .records
            .iter()
            .any(|r| r.kind == RecordKind::OperationFinished { success: false })
    );
    assert!(!snapshot.records.iter().any(|r| matches!(
        r.kind,
        RecordKind::Parameters(
            ParameterStage::BuilderInvoked
                | ParameterStage::ProblemStored
                | ParameterStage::ResetStored
        ) | RecordKind::Initialized
    )));
}
