//! The event-free type-state of the correlated codegen view.
//!
//! This view admits only components without semantic event partitions. The
//! separate [`super::FmiCCodegenView`] also admits parameter-dependent assertions
//! after proving their dependency and lifecycle requirements.
//!
//! Neither the entry shape nor the empty event domain is a property of every
//! checked component. The maximum-step-duration local of
//! [`super::max_step_duration`] is evaluated by the component rather than
//! backed by a run, and FMI forbids it a `start`; and a checked kernel may own
//! discrete/event partitions, root or scheduled events, runtime delay or
//! terminal events, or clocks that no such template describes.
//!
//! Component construction already makes the inventory shape valid: ordinary
//! entries are storage-backed, with `start` present only where FMI permits it.
//! The derived maximum-step-duration entry belongs to a delay-bearing kernel.
//! Narrowing therefore has
//! one proof to issue rather than auditing a finalized aggregate again:
//! [`super::FmiCodegenView`] is consumed into [`FmiEventFreeCodegenView`]
//! exactly when its kernel carries no semantic event class. That narrowed view
//! has a whole-inventory encoding, as does the checked C profile.
//!
//! The event domain is read through [`crate::solve_event_class`], the single
//! owner `rumoca-compile`'s target-capability gate also calls, so this
//! narrowing cannot recognise a different set of event classes than the public
//! manifest validation does.
//!
//! What is *not* proved here is stated exactly, because the type's name is a
//! claim: this narrowing says nothing about initialization owners, residual
//! algebraic systems, or the tensor/DAE-level facts a target manifest also
//! declares. Public target selection checks the complete manifest, and each
//! renderer constructor checks the capabilities required by its own templates
//! before a render context exists.
//!
//! The narrowing moves the one checked [`FmiMetadata`] and the one correlated
//! kernel handle. Nothing is copied, re-counted, or re-indexed, and the
//! unrestricted [`super::FmiCodegenView`] remains available to a future
//! event-capable renderer under ME-EVENT-002's later gate.

use super::metadata::{FmiVariable, SerializedFmiVariables};
use super::{FmiCodegenView, FmiMetadata};
use crate::{SolveEventClass, SolveModel, solve_event_class};
use serde::Serialize;
use serde::ser::{SerializeMap, Serializer};
use std::sync::Arc;

/// Why one correlated view has no event-free type-state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum FmiEventFreeError {
    #[error("FMI component kernel owns {class:?} semantic events")]
    EventBearingKernel { class: SolveEventClass },
}

/// One correlated codegen view whose kernel is proved event-free.
///
/// The proof is carried by the type; the checked inventory and the kernel it
/// was checked against are moved in whole, so this remains the single owner of
/// order, identity, and attributes rather than a second description of them.
/// A renderer may impose a narrower capability domain in its own checked
/// constructor.
///
/// Deliberately not `Clone`, and with no constructor of its own: the only
/// producer is [`FmiCodegenView::try_event_free`].
///
/// Together with the checked C profile, this gates whole-inventory encoding. `FmiVariable`,
/// [`FmiMetadata`], [`super::FmiComponent`], and [`FmiCodegenView`] are
/// deliberately not `Serialize`, so an unproved inventory cannot be handed to a
/// template through any public type:
///
/// ```compile_fail
/// fn requires_serialize<T: serde::Serialize>(_: &T) {}
/// fn escape(entry: &rumoca_ir_solve::fmi::FmiVariable) {
///     requires_serialize(entry);
/// }
/// ```
///
/// ```compile_fail
/// fn requires_serialize<T: serde::Serialize>(_: &T) {}
/// fn escape(view: &rumoca_ir_solve::fmi::FmiCodegenView) {
///     requires_serialize(view);
/// }
/// ```
///
/// while the proved type-state is encodable:
///
/// ```
/// fn requires_serialize<T: serde::Serialize>(_: &T) {}
/// fn encode(view: &rumoca_ir_solve::fmi::FmiEventFreeCodegenView) {
///     requires_serialize(view);
/// }
/// ```
#[derive(Debug)]
pub struct FmiEventFreeCodegenView {
    pub(super) metadata: FmiMetadata,
    pub(super) model: Arc<SolveModel>,
}

impl FmiCodegenView {
    /// Consume this correlated view into the event-free type-state, or name
    /// the semantic event class that prevents it.
    ///
    /// The built-in FMI targets declare `events`, `runtime_events`, and
    /// `clocks` false and fail closed at their capability gate long before
    /// this. This rejection keeps the boundary true rather than incidental if
    /// a target ever forgets, and it is the whole refusal for a caller that
    /// reaches the typed API directly.
    pub fn try_event_free(self) -> Result<FmiEventFreeCodegenView, FmiEventFreeError> {
        if let Some(class) = solve_event_class(&self.model.problem) {
            return Err(FmiEventFreeError::EventBearingKernel { class });
        }
        Ok(FmiEventFreeCodegenView {
            metadata: self.metadata,
            model: self.model,
        })
    }
}

impl FmiEventFreeCodegenView {
    /// The moved inventory, in its one value-reference order.
    #[must_use]
    pub fn variables(&self) -> &[FmiVariable] {
        self.metadata.variables()
    }

    #[must_use]
    pub const fn derivative_value_reference_base_fmi3(&self) -> u32 {
        self.metadata.derivative_value_reference_base_fmi3()
    }

    #[must_use]
    pub fn problem(&self) -> &crate::SolveProblem {
        &self.model.problem
    }

    #[must_use]
    pub fn artifacts(&self) -> &crate::SolveArtifacts {
        &self.model.artifacts
    }

    #[must_use]
    pub fn pure_calls(&self) -> &crate::SolvePureCallTable {
        &self.model.pure_calls
    }
}

/// The one whole-inventory encoding a render context can hold.
///
/// Each entry is encoded through the module-private borrowed serializer, so
/// the template keys come from the single owned inventory in place: nothing is
/// cloned, re-indexed, or stored a second time.
impl Serialize for FmiEventFreeCodegenView {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut entries = serializer.serialize_map(Some(3))?;
        entries.serialize_entry(
            "variables",
            &SerializedFmiVariables::borrowing(self.metadata.variables()),
        )?;
        entries.serialize_entry(
            "state_variable_indices",
            self.metadata.state_variable_indices(),
        )?;
        entries.serialize_entry(
            "derivative_value_reference_base_fmi3",
            &self.metadata.derivative_value_reference_base_fmi3(),
        )?;
        entries.end()
    }
}
