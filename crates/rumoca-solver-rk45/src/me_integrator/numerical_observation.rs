//! Passive readback of the existing RK configuration, not adaptive-step policy.

use super::{MeNumericalSetup, Rk45Integrator};
use rumoca_solver::fmi_me::numerical_observation::{
    AdapterWriter, SettingField, SettingValue, SettingsStage,
};

pub(super) fn record_requested(writer: &AdapterWriter, setup: &MeNumericalSetup) {
    let stage = SettingsStage::Requested;
    writer.setting(
        stage,
        SettingField::RelativeTolerance,
        SettingValue::Known(&[setup.relative_tolerance()]),
    );
    writer.setting(
        stage,
        SettingField::ScalarAbsoluteTolerance,
        SettingValue::Known(&[setup.absolute_tolerance()]),
    );
    writer.setting(
        stage,
        SettingField::StateNominals,
        SettingValue::Known(setup.state_nominals()),
    );
    match setup.initial_step_hint() {
        Some(value) => writer.setting(
            stage,
            SettingField::InitialStep,
            SettingValue::Known(&[value]),
        ),
        None => writer.setting(
            stage,
            SettingField::InitialStep,
            SettingValue::NotApplicable,
        ),
    }
    // The request supplies a scalar plus nominals, not a per-state vector.
    writer.setting(
        stage,
        SettingField::StateAbsoluteTolerances,
        SettingValue::NotApplicable,
    );
}

impl Rk45Integrator {
    pub(super) fn record_settings(&self, stage: SettingsStage) {
        let Some(writer) = &self.observation else {
            return;
        };
        writer.setting(
            stage,
            SettingField::RelativeTolerance,
            SettingValue::Known(&[self.relative_tolerance]),
        );
        writer.setting(
            stage,
            SettingField::InitialStep,
            SettingValue::Known(&[self.initial_step]),
        );
        writer.setting(
            stage,
            SettingField::StateAbsoluteTolerances,
            SettingValue::Known(&self.state_absolute_tolerances),
        );
        // Only the scaled vector is retained. The complete scalar and nominal
        // requests remain in the distinct Requested/IssuedSetup records.
        for field in [
            SettingField::ScalarAbsoluteTolerance,
            SettingField::StateNominals,
        ] {
            writer.setting(stage, field, SettingValue::NotApplicable);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_solver::fmi_me::{
        MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeIntegrationError,
        MeIntegratorBackend, MeStepCandidate,
        numerical_observation::{
            LifecycleCall, ObservationRoute, RecordKind, SettingAvailability, SettingsCollector,
            SettingsSnapshot,
        },
        ode_driver::{ContinuousOde, OdeRun, integrate_ode},
    };
    use std::{cell::RefCell, rc::Rc};

    struct ConstantPair;

    impl ContinuousOde for ConstantPair {
        fn width(&self) -> usize {
            2
        }

        fn derivatives_into(
            &self,
            _time: f64,
            _state: &[f64],
            out: &mut [f64],
        ) -> Result<(), String> {
            out.fill(0.0);
            Ok(())
        }
    }

    // Obtain a checked point from the real public host, without widening the
    // private constructor. All numerical work remains in the ordinary plugin.
    struct CapturePoint {
        inner: Box<dyn MeIntegratorBackend>,
        point: Rc<RefCell<Option<MeContinuousPoint>>>,
    }

    impl MeIntegratorBackend for CapturePoint {
        fn initialize(
            &mut self,
            point: &MeContinuousPoint,
            derivatives: MeDerivativeHandle,
        ) -> Result<(), MeIntegrationError> {
            *self.point.borrow_mut() = Some(point.clone());
            self.inner.initialize(point, derivatives)?;
            self.truncate_reset(point)
        }

        fn advance(
            &mut self,
            request: &MeAdvanceRequest,
        ) -> Result<MeStepCandidate, MeIntegrationError> {
            self.inner.advance(request)
        }

        fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
            self.inner.sample(time, states)
        }

        fn continuous_extension_order(&self) -> Option<u32> {
            self.inner.continuous_extension_order()
        }

        fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
            self.inner.truncate_reset(point)
        }
    }

    fn host_issued_point() -> MeContinuousPoint {
        let point = Rc::new(RefCell::new(None));
        let run = OdeRun {
            relative_tolerance: 1.0e-6,
            absolute_tolerance: 1.0e-10,
            nominals: vec![2.0, 3.0],
            initial_step: None,
            t_start: 0.0,
            t_end: 1.0e-3,
            initial_state: vec![1.0, 2.0],
            breakpoints: Vec::new(),
            extension_order_limit: None,
        };
        integrate_ode(
            Rc::new(ConstantPair),
            &run,
            &|setup| {
                Box::new(CapturePoint {
                    inner: super::super::model_exchange_integrator(setup),
                    point: Rc::clone(&point),
                })
            },
            |interval| {
                let mut sampled = [f64::NAN; 2];
                interval.sample(interval.end_time(), &mut sampled).unwrap();
                assert_eq!(sampled, [1.0, 2.0]);
                Ok(())
            },
        )
        .unwrap();
        point.borrow_mut().take().unwrap()
    }

    fn assert_setting(
        snapshot: &SettingsSnapshot,
        stage: SettingsStage,
        field: SettingField,
        expected: &[f64],
    ) {
        let kind = RecordKind::Setting {
            stage,
            field,
            availability: SettingAvailability::Known,
        };
        let record = snapshot
            .records
            .iter()
            .find(|record| record.kind == kind)
            .unwrap();
        assert_eq!(
            snapshot.cells[record.cells.clone()],
            expected
                .iter()
                .copied()
                .map(f64::to_bits)
                .collect::<Vec<_>>()
        );
        assert_eq!(
            record.operation.is_some(),
            stage == SettingsStage::RetainedAfterReset
        );
    }

    #[test]
    fn absent_hint_and_derived_vector_have_distinct_requested_and_retained_facts() {
        let collector = SettingsCollector::new(64, 128).unwrap();
        let (host, adapter) = collector
            .associate(ObservationRoute::ModelExchangeSession)
            .unwrap();
        let setup = MeNumericalSetup::new(1.0e-6, 1.0e-10, vec![2.0, 3.0], 2, None).unwrap();
        let mut plugin =
            super::super::model_exchange_integrator_with_observation(setup, Some(adapter));
        let operation = host.begin(LifecycleCall::TruncateReset).unwrap();
        let point = host_issued_point();
        plugin.truncate_reset(&point).unwrap();
        operation.finish(true);
        drop((plugin, host));
        let snapshot = collector.finish();
        assert_eq!(snapshot.incomplete, 0);
        assert!(snapshot.records.iter().any(|record| record.kind
            == RecordKind::Setting {
                stage: SettingsStage::Requested,
                field: SettingField::InitialStep,
                availability: SettingAvailability::NotApplicable,
            }));
        for stage in [
            SettingsStage::Constructed,
            SettingsStage::RetainedAfterReset,
        ] {
            for (field, expected) in [
                (SettingField::InitialStep, vec![1.0e-3]),
                (
                    SettingField::StateAbsoluteTolerances,
                    vec![2.0e-10, 3.0e-10],
                ),
            ] {
                assert_setting(&snapshot, stage, field, &expected);
            }
        }
    }
}
