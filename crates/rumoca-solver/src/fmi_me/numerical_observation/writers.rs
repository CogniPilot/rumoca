//! Distinct append-only endpoints. Reading/finalization stay collector-private.

use super::storage::{Context, Shared};
use super::*;
use crate::fmi_me::MeNumericalSetup;

/// Host/session write endpoint. It cannot read records or model data.
///
/// The endpoint cannot finalize the outer collector:
/// ```compile_fail
/// use rumoca_solver::fmi_me::numerical_observation::HostWriter;
/// fn finalize(writer: HostWriter) { let _ = writer.finish(); }
/// ```
/// It also cannot be duplicated into a second operation owner:
/// ```compile_fail
/// use rumoca_solver::fmi_me::numerical_observation::HostWriter;
/// fn duplicate(writer: HostWriter) { let _ = writer.clone(); }
/// ```
pub struct HostWriter {
    shared: Rc<Shared>,
    instance: usize,
}

impl HostWriter {
    pub(super) fn new(shared: Rc<Shared>, instance: usize) -> Self {
        Self { shared, instance }
    }

    /// Borrow the setup actually issued at composition, without cloning it.
    pub fn issued_setup(&self, setup: &MeNumericalSetup) {
        self.setup(setup);
    }

    /// Observe the existing factory boundary without invoking the plugin.
    pub fn construction(
        &self,
        backend: crate::NumericalBackend,
        purpose: ConstructionPurpose,
        stage: SettingsStage,
    ) {
        if let Some(mut storage) = self.shared.borrow() {
            storage.append(
                &self.shared,
                self.instance,
                None,
                RecordKind::BackendConstruction {
                    backend,
                    purpose,
                    stage,
                },
                &[],
                &[],
            );
        }
    }

    /// Current policy is distinct from parameters retained by a numerical plugin.
    pub fn current_policy(&self, rtol: f64, atol: f64, nominals: &[f64]) {
        if let Some(mut storage) = self.shared.borrow() {
            storage.append(
                &self.shared,
                self.instance,
                None,
                RecordKind::HostPolicy { has_step: false },
                &[rtol, atol],
                nominals,
            );
        }
    }

    /// The host could not supply a policy observation; numerical flow continues.
    pub fn unavailable(&self) {
        self.shared.mark(IncompleteReason::UnsupportedObservation);
    }

    fn setup(&self, setup: &MeNumericalSetup) {
        let has_step = setup.initial_step_hint().is_some();
        let values = [
            setup.relative_tolerance(),
            setup.absolute_tolerance(),
            setup.initial_step_hint().unwrap_or(0.0),
        ];
        let scalars = &values[..if has_step { 3 } else { 2 }];
        if let Some(mut storage) = self.shared.borrow() {
            storage.append(
                &self.shared,
                self.instance,
                None,
                RecordKind::IssuedSetup { has_step },
                scalars,
                setup.state_nominals(),
            );
        }
    }

    /// Bracket an existing call. Context failure only spoils diagnostics.
    #[must_use]
    pub fn begin(&self, kind: LifecycleCall) -> Option<OperationGuard> {
        let mut storage = self.shared.borrow()?;
        let instance = &mut storage.instances[self.instance];
        if !instance.context.is_idle() {
            instance.context = Context::Invalid;
            self.shared.mark(IncompleteReason::OperationContext);
            return None;
        }
        let Some(ordinal) = instance.next_operation.checked_add(1) else {
            self.shared.mark(IncompleteReason::OperationContext);
            return None;
        };
        instance.next_operation = ordinal;
        instance.context = Context::Active(ordinal);
        storage.append(
            &self.shared,
            self.instance,
            Some(ordinal),
            RecordKind::OperationStarted(kind),
            &[],
            &[],
        );
        Some(OperationGuard {
            shared: Rc::clone(&self.shared),
            instance: self.instance,
            ordinal,
            finished: false,
        })
    }
}

impl Drop for HostWriter {
    fn drop(&mut self) {
        if let Some(mut storage) = self.shared.borrow() {
            if !storage.instances[self.instance].context.is_idle() {
                self.shared.mark(IncompleteReason::UnfinishedOperation);
            }
            storage.append(
                &self.shared,
                self.instance,
                None,
                RecordKind::HostClosed,
                &[],
                &[],
            );
        }
    }
}

/// Adapter write endpoint. It cannot choose host operation kinds or read records.
///
/// An adapter cannot open a host lifecycle window:
/// ```compile_fail
/// use rumoca_solver::fmi_me::numerical_observation::{AdapterWriter, LifecycleCall};
/// fn enter(writer: AdapterWriter) { let _ = writer.begin(LifecycleCall::Initialize); }
/// ```
/// Nor can it inspect the collector through private storage:
/// ```compile_fail
/// use rumoca_solver::fmi_me::numerical_observation::AdapterWriter;
/// fn inspect(writer: AdapterWriter) { let _ = writer.shared; }
/// ```
pub struct AdapterWriter {
    shared: Rc<Shared>,
    instance: usize,
}

impl AdapterWriter {
    pub(super) fn new(shared: Rc<Shared>, instance: usize) -> Self {
        Self { shared, instance }
    }

    /// Report a backend-owned bounded schema without prescribing its fields.
    pub fn backend_values(
        &self,
        stage: SettingsStage,
        backend: crate::NumericalBackend,
        tag: u16,
        scalars: &[f64],
        vector: &[f64],
    ) {
        self.record(
            RecordKind::BackendValues {
                stage,
                backend,
                tag,
            },
            scalars,
            vector,
        );
    }

    /// Record a common field, including explicit unavailable/negative readback.
    pub fn setting(&self, stage: SettingsStage, field: SettingField, value: SettingValue<'_>) {
        let (availability, values) = match value {
            SettingValue::Known(values) => (SettingAvailability::Known, values),
            SettingValue::NotApplicable => (SettingAvailability::NotApplicable, &[][..]),
            SettingValue::Unsupported => {
                self.shared.mark(IncompleteReason::UnsupportedObservation);
                (SettingAvailability::Unsupported, &[][..])
            }
        };
        let Some(mut storage) = self.shared.borrow() else {
            return;
        };
        let operation = match storage.instances[self.instance].context {
            Context::Active(operation) => Some(operation),
            Context::Idle => None,
            Context::Invalid => {
                self.shared.mark(IncompleteReason::OperationContext);
                return;
            }
        };
        storage.append(
            &self.shared,
            self.instance,
            operation,
            RecordKind::Setting {
                stage,
                field,
                availability,
            },
            &[],
            values,
        );
    }

    /// Report only after the ordinary adapter initialization checks succeed.
    pub fn initialized(&self) {
        self.record(RecordKind::Initialized, &[], &[]);
    }

    /// Explicitly report unavailable coverage; never invent a parameter record.
    pub fn unavailable(&self) {
        self.shared.mark(IncompleteReason::UnsupportedObservation);
        if let Some(mut storage) = self.shared.borrow() {
            storage.append(
                &self.shared,
                self.instance,
                None,
                RecordKind::ObservationUnavailable,
                &[],
                &[],
            );
        }
    }

    fn record(&self, kind: RecordKind, scalars: &[f64], vector: &[f64]) {
        let Some(mut storage) = self.shared.borrow() else {
            return;
        };
        let Context::Active(operation) = storage.instances[self.instance].context else {
            self.shared.mark(IncompleteReason::OperationContext);
            return;
        };
        storage.append(
            &self.shared,
            self.instance,
            Some(operation),
            kind,
            scalars,
            vector,
        );
    }
}

impl Drop for AdapterWriter {
    fn drop(&mut self) {
        if let Some(mut storage) = self.shared.borrow() {
            storage.append(
                &self.shared,
                self.instance,
                None,
                RecordKind::AdapterClosed,
                &[],
                &[],
            );
        }
    }
}

/// Closes the diagnostic bracket of one real host call, including unwind exits.
#[must_use]
pub struct OperationGuard {
    shared: Rc<Shared>,
    instance: usize,
    ordinal: u64,
    finished: bool,
}

impl OperationGuard {
    /// Observe success/error of the existing call; original typed errors are untouched.
    /// A caller without a typed failure category records it as unavailable.
    pub fn finish(self, success: bool) {
        self.finish_outcome((!success).then_some(FailureCategory::Unavailable));
    }

    /// Observe an original typed failure by a bounded category, without formatting it.
    pub fn finish_failure(self, category: FailureCategory) {
        self.finish_outcome(Some(category));
    }

    fn finish_outcome(mut self, failure: Option<FailureCategory>) {
        self.finished = true;
        let Some(mut storage) = self.shared.borrow() else {
            return;
        };
        let context = &mut storage.instances[self.instance].context;
        if !matches!(context, Context::Active(ordinal) if *ordinal == self.ordinal) {
            self.shared.mark(IncompleteReason::OperationContext);
            *context = Context::Invalid;
            return;
        }
        *context = Context::Idle;
        if let Some(category) = failure {
            storage.append(
                &self.shared,
                self.instance,
                Some(self.ordinal),
                RecordKind::OperationFailure(category),
                &[],
                &[],
            );
        }
        storage.append(
            &self.shared,
            self.instance,
            Some(self.ordinal),
            RecordKind::OperationFinished {
                success: failure.is_none(),
            },
            &[],
            &[],
        );
    }
}

impl Drop for OperationGuard {
    fn drop(&mut self) {
        if self.finished {
            return;
        }
        self.shared.mark(IncompleteReason::UnfinishedOperation);
        if let Some(mut storage) = self.shared.borrow() {
            storage.instances[self.instance].context = Context::Invalid;
        }
    }
}
