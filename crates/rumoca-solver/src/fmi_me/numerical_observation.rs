//! Bounded, optional numerical-settings diagnostics, independent of ME authority.
//!
//! Only the outer collector reads records. The host and adapter receive distinct
//! write endpoints. Recording never invokes a model or changes a numerical result.
//! A missing record is an incomplete diagnostic, never evidence of success.

mod storage;
#[cfg(test)]
mod tests;
mod writers;

pub use writers::{AdapterWriter, HostWriter, OperationGuard};

use std::cell::{Cell, RefCell};
use std::collections::TryReserveError;
use std::rc::Rc;

use storage::{Instance, Shared, Storage};

/// The actual composition route, supplied by the outer recording owner.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObservationRoute {
    /// An ME session invokes the plugin.
    ModelExchangeSession,
    /// A standalone ODE driver invokes the plugin, without an ME session.
    StandaloneOde,
}

/// Why the importer is constructing this numerical plugin. This is supplied
/// by the existing factory owner, never queried from the numerical trait.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConstructionPurpose {
    /// The plugin drives the selected numerical run.
    SelectedRun,
    /// Existing automatic selection checks a candidate, then drops its plugin.
    AutoCapabilityProbe,
    /// Existing preparation checks initialization before a subsequent run.
    PreparationCheck,
}

/// A host-owned lifecycle call, not an accepted-step certificate.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LifecycleCall {
    /// The existing initialize call, including history rebuilds.
    Initialize,
    /// The existing in-place truncate/reset call.
    TruncateReset,
}

/// Solver-neutral construction and retained-state readback stages.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SettingsStage {
    Requested,
    ConstructionStarted,
    Constructed,
    RetainedAfterReset,
}

/// Common importer/solver settings, distinct from backend-specific fields.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SettingField {
    RelativeTolerance,
    ScalarAbsoluteTolerance,
    StateAbsoluteTolerances,
    /// Configured initial hint/start step, never an accepted adaptive step.
    InitialStep,
    StateNominals,
}

/// A negative fact is explicit; unsupported coverage is never fabricated zero.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SettingAvailability {
    Known,
    NotApplicable,
    Unsupported,
}

/// Borrowed values copied only into the collector's pre-reserved raw cells.
#[derive(Debug, Clone, Copy)]
pub enum SettingValue<'a> {
    Known(&'a [f64]),
    NotApplicable,
    Unsupported,
}

/// A bounded diagnostic category; the original typed failure remains authoritative.
/// No rendered messages, model queries or cloned errors are captured here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FailureCategory {
    /// The recording caller has no typed category available.
    Unavailable,
    /// The component's own evaluation failed.
    Component,
    /// The numerical method reported its existing typed category.
    Numerical(super::MeNumericalFailure),
    /// The component discarded a trial evaluation.
    ComponentDiscard,
    /// The numerical method could not represent a progressing step.
    StepSizeUnderflow,
    /// The retained derivative capability refused or was used out of context.
    DerivativeCapability,
    /// An original host or numerical allocation failed.
    Allocation,
    /// The original host budget expired.
    Timeout,
    /// An original checked option, shape, arity or contract failed.
    Contract,
    /// A host lifecycle, root, event, restoration or usability operation failed.
    HostLifecycle,
}

/// A diagnostic record's meaning. No backend error or private solver state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RecordKind {
    /// A fresh actual plugin construction, even when setup values are equal.
    InstanceStarted(ObservationRoute),
    /// Identity and purpose of the actual importer-owned factory call.
    BackendConstruction {
        backend: crate::NumericalBackend,
        purpose: ConstructionPurpose,
        stage: SettingsStage,
    },
    /// The zero-state host requires no numerical plugin; this is complete.
    NoNumericalBackend(ObservationRoute),
    /// Validated host setup: rtol, atol, optional step, then all nominals.
    IssuedSetup { has_step: bool },
    /// Current host policy, which need not have reached a retained adapter.
    HostPolicy { has_step: bool },
    /// Host entered an existing lifecycle call.
    OperationStarted(LifecycleCall),
    /// Original numerical call returned, without reclassifying its error.
    OperationFinished { success: bool },
    /// Broad category of the original failure, paired with a failed terminal.
    OperationFailure(FailureCategory),
    /// One neutral setting; the cells contain its complete known value.
    Setting {
        stage: SettingsStage,
        field: SettingField,
        availability: SettingAvailability,
    },
    /// Backend-owned bounded schema tag; only that backend defines cell meaning.
    BackendValues {
        stage: SettingsStage,
        backend: crate::NumericalBackend,
        tag: u16,
    },
    /// Initialization completed after the adapter's ordinary checks.
    Initialized,
    /// This adapter/route cannot report its parameters.
    ObservationUnavailable,
    /// The actual endpoint was dropped.
    HostClosed,
    /// The actual endpoint was dropped.
    AdapterClosed,
}

/// One header and a range into the snapshot's exact Binary64-bit cells.
#[derive(Debug)]
pub struct SettingsRecord {
    /// Collector-local instance ordinal, not numerical/provenance authority.
    pub instance: usize,
    /// Host lifecycle ordinal, or absent for setup/construction/closure records.
    pub operation: Option<u64>,
    /// The observed source stage.
    pub kind: RecordKind,
    /// Exact values in the declared order, including signed zeros and NaN bits.
    pub cells: std::ops::Range<usize>,
}

/// An explicit reason that a collection cannot be treated as complete.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum IncompleteReason {
    /// A configured header, instance or raw-cell bound was exceeded.
    Capacity = 1,
    /// A recording borrow conflicted; the numerical call was not blocked.
    BorrowConflict = 2,
    /// Adapter reporting had no unique live host operation.
    OperationContext = 4,
    /// Collection happened before all endpoints were closed.
    WritersStillLive = 8,
    /// A lifecycle call left without its terminal diagnostic.
    UnfinishedOperation = 16,
    /// A route/adapter explicitly cannot supply this observation.
    UnsupportedObservation = 32,
    /// A recording operation was attempted after collection ended.
    Finalized = 64,
}

/// Outer-owner result. Serialization and its independent output cap live outside.
#[derive(Debug)]
pub struct SettingsSnapshot {
    /// Headers in append order, not a reordered numerical history.
    pub records: Vec<SettingsRecord>,
    /// Original IEEE Binary64 bits; no normalization or coordinate inference.
    pub cells: Vec<u64>,
    /// Sticky incompleteness flags, zero only when no recorded failure occurred.
    pub incomplete: u16,
}

impl SettingsSnapshot {
    /// Test a named diagnostic failure without interpreting numerical outcomes.
    #[must_use]
    pub fn has(&self, reason: IncompleteReason) -> bool {
        self.incomplete & reason as u16 != 0
    }
}

/// Sole reader/finalizer. Construct only for an explicitly requested observation.
///
/// Its outer allocation precedes numerical construction. All record/instance/cell
/// storage is fallibly reserved here; writes never grow any allocation.
pub struct SettingsCollector {
    shared: Rc<Shared>,
}

impl SettingsCollector {
    /// Reserve the caller's explicit raw diagnostic limits, not solver budgets.
    pub fn new(record_limit: usize, cell_limit: usize) -> Result<Self, TryReserveError> {
        let mut storage = Storage::default();
        storage.records.try_reserve_exact(record_limit)?;
        storage.cells.try_reserve_exact(cell_limit)?;
        storage.instances.try_reserve_exact(record_limit)?;
        Ok(Self {
            shared: Rc::new(Shared {
                storage: RefCell::new(storage),
                incomplete: Cell::new(0),
                finalized: Cell::new(false),
                record_limit,
                cell_limit,
            }),
        })
    }

    /// Associate one actual construction. Endpoints are neither cloneable nor readers.
    /// Exhaustion returns no endpoints and leaves the ordinary numerical path free.
    pub fn associate(&self, route: ObservationRoute) -> Option<(HostWriter, AdapterWriter)> {
        let mut storage = self.shared.borrow()?;
        if storage.instances.len() >= self.shared.record_limit {
            self.shared.mark(IncompleteReason::Capacity);
            return None;
        }
        let instance = storage.instances.len();
        storage.instances.push(Instance::default());
        storage.append(
            &self.shared,
            instance,
            None,
            RecordKind::InstanceStarted(route),
            &[],
            &[],
        );
        Some((
            HostWriter::new(Rc::clone(&self.shared), instance),
            AdapterWriter::new(Rc::clone(&self.shared), instance),
        ))
    }

    /// Record an actual zero-state composition without inventing a plugin or
    /// marking a known negative fact as unsupported observation.
    pub fn no_numerical_backend(&self, route: ObservationRoute) {
        let Some(mut storage) = self.shared.borrow() else {
            return;
        };
        if storage.instances.len() >= self.shared.record_limit {
            self.shared.mark(IncompleteReason::Capacity);
            return;
        }
        let instance = storage.instances.len();
        storage.instances.push(Instance::default());
        storage.append(
            &self.shared,
            instance,
            None,
            RecordKind::NoNumericalBackend(route),
            &[],
            &[],
        );
    }

    /// Consume the sole reader. Live writers or unfinished calls are incomplete.
    #[must_use]
    pub fn finish(self) -> SettingsSnapshot {
        if Rc::strong_count(&self.shared) != 1 {
            self.shared.mark(IncompleteReason::WritersStillLive);
        }
        self.shared.finalized.set(true);
        let Ok(mut storage) = self.shared.storage.try_borrow_mut() else {
            self.shared.mark(IncompleteReason::BorrowConflict);
            return SettingsSnapshot {
                records: Vec::new(),
                cells: Vec::new(),
                incomplete: self.shared.incomplete.get(),
            };
        };
        if storage
            .instances
            .iter()
            .any(|instance| !instance.context.is_idle())
        {
            self.shared.mark(IncompleteReason::UnfinishedOperation);
        }
        SettingsSnapshot {
            records: std::mem::take(&mut storage.records),
            cells: std::mem::take(&mut storage.cells),
            incomplete: self.shared.incomplete.get(),
        }
    }
}
