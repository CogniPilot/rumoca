//! The typed failure and usability vocabulary of the one master algorithm
//! (SPEC_0044 §6 ME-INT-003, review findings \[373\], \[382\]).
//!
//! Split out of [`super`] so the master algorithm stays readable inside the
//! SPEC_0021 file limits. It is not a second owner: every value here is
//! produced by the session module and by nothing else.

use crate::{
    fmi_me::{MeError, MeStage, integrator::MeIntegrationError, trace::MeTraceViolation},
    runtime::timeout::TimeoutExceeded,
};

/// Typed session failures (SPEC_0044 §6, ME-INT-003).
///
/// Component, integrator, timeout, allocation, discard, root-application, and
/// trace failures stay distinct all the way to a facade client's own error
/// enum; nothing is rendered into a neighbouring variant's prose. Standard
/// termination is not a failure at all: it is a successful [`SimTermination`].
#[derive(Debug, thiserror::Error)]
pub enum MeSessionError {
    /// A standard component operation failed.
    #[error(transparent)]
    Component(#[from] MeError),

    /// The numerical plugin failed.
    #[error(transparent)]
    Integration(#[from] MeIntegrationError),

    /// The wall-clock budget expired.
    #[error("timeout after {seconds:.3}s")]
    Timeout { seconds: f64 },

    /// The **host** could not reserve its own storage.
    ///
    /// ME-INT-003 lists allocation as a category of its own. A host buffer or
    /// trace row that cannot be reserved is a host storage failure, so it is
    /// never attributed to the FMI component (review finding \[370\]§3).
    #[error("the host could not reserve {entries} entries for {context}")]
    Allocation {
        context: &'static str,
        entries: usize,
    },

    /// A host option, cursor, or policy input was not admissible.
    #[error("{reason}")]
    Options { reason: String },

    /// The host used its own contract incorrectly, or the component produced a
    /// vector contradicting its own model description.
    #[error("{reason}")]
    Contract { reason: String },

    /// Root search found a domain change it cannot turn into an application.
    ///
    /// The host does not enter Event Mode, clamp an indicator, or repair
    /// evidence in this case.
    #[error("root application is unavailable at t={time}: {reason}")]
    RootApplicationUnavailable { time: f64, reason: String },

    /// The promised scan resolution cannot be represented over this interval.
    ///
    /// SPEC_0044 §6 guarantees every adjacent sampled interval is bounded by
    /// the checked resolution, so the host reports a typed resource failure
    /// rather than silently scanning coarser (review finding \[336\]§1).
    #[error(
        "a scan of [{start}, {end}] at resolution {resolution} needs more coordinates than the \
         host can represent"
    )]
    RootScanUnrepresentable {
        start: f64,
        end: f64,
        resolution: f64,
    },

    /// A discrete iteration at one coordinate did not settle.
    #[error("event iteration did not settle at t={time} within {limit} updates")]
    EventIterationDiverged { time: f64, limit: usize },

    /// A stateless model was handed a state-carrying numerical plugin, or the
    /// reverse.
    ///
    /// The diagnostic names the arity rule that was broken, never who broke it:
    /// SPEC_0044 §6 ME-INT-001 admits exactly four plugin operations and no
    /// backend-identity one, so a solver label cannot be asked for here, and a
    /// common enum of solver families would put the same identity back into the
    /// common crate through the back door (review finding \[399\]).
    #[error("a component with {state_count} continuous states {mismatch}")]
    PluginArity {
        state_count: usize,
        mismatch: MePluginArity,
    },

    /// The component could not be returned to the session's accepted point.
    ///
    /// SPEC_0044 §6 and ME-BUF-001 make the session's accepted time/state and
    /// the component's coordinate one fact. When an off-point excursion cannot
    /// be closed, that fact is gone, so this failure takes precedence over
    /// whatever the excursion was attempting: a getter, indicator, sampler,
    /// budget, or backend failure is carried here as typed data rather than
    /// rendered into prose or dropped (review finding \[373\]).
    #[error("the component could not be restored to the accepted point t={time}: {restoration}")]
    AcceptedPointLost {
        time: f64,
        restoration: Box<MeError>,
        /// What the excursion was attempting when restoration failed, if it had
        /// already failed too.
        attempted: Option<Box<MeSessionError>>,
    },

    /// A mutating or evaluating call on a session that stopped being usable.
    ///
    /// A mutating step that failed after one correlated owner had already moved
    /// is terminal: nothing here can re-establish the correlation it destroyed,
    /// so the session is an explicit non-reusable failed session rather than an
    /// apparently live one (review findings \[373\], \[382\]).
    #[error("the session is not reusable: {loss}")]
    SessionNotReusable { loss: MeSessionLoss },
}

impl MeSessionError {
    /// Whether this is the common host's wall-clock timeout category.
    ///
    /// Facades use this producer-owned discriminator to preserve timeout as a
    /// distinct result bucket without parsing the transparent error text.
    #[must_use]
    pub const fn is_timeout(&self) -> bool {
        matches!(self, Self::Timeout { .. })
    }

    /// Whether the numerical plugin itself owns the failure.
    ///
    /// A failed restoration or an unusable session remains host-owned even
    /// when its nested attempted operation was numerical: the correlation
    /// loss is the failure that escaped the common master algorithm.
    #[must_use]
    pub const fn is_integrator_failure(&self) -> bool {
        matches!(self, Self::Integration(_))
    }
}

/// Which half of ME-ZERO-001's arity rule a plugin attachment broke.
///
/// A category, never a solver name: a host or a worker bucket switches on the
/// *rule*, which is identical for every backend, and the common crate stays
/// free of backend identity (review finding \[399\]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MePluginArity {
    /// Zero continuous states, yet a numerical plugin was supplied. ME-ZERO-001
    /// selects the host's own time-only advance for that component.
    RejectsANumericalPlugin,
    /// Continuous states to integrate, yet no numerical plugin was supplied.
    RequiresANumericalPlugin,
}

impl MePluginArity {
    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::RejectsANumericalPlugin => {
                "admits only the host's time-only advance, so no numerical plugin may be attached"
            }
            Self::RequiresANumericalPlugin => "requires a numerical plugin, and none was supplied",
        }
    }
}

impl std::fmt::Display for MePluginArity {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.label())
    }
}

/// Why a session stopped being usable.
///
/// A category, never prose: a host, a worker bucket, or a census switches on
/// this rather than on a rendered message, and a lifecycle, policy, or plugin
/// loss is never mislabelled as a coordinate loss (review finding \[382\]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeSessionLoss {
    /// The component could not be returned to the session's accepted point.
    /// The originating [`MeSessionError::AcceptedPointLost`] carries the
    /// dual-failure detail.
    AcceptedPoint,
    /// Event Mode's continuous-state, nominal, or plugin-history refresh did
    /// not complete, so those owners may name different states.
    EventRefresh,
    /// An input write did not become visible in every correlated owner.
    InputApplication,
    /// A restart did not rebuild a complete lifecycle.
    Restart,
    /// One accepted numerical step did not complete.
    ///
    /// [`MeIntegratorBackend`] promises no rollback of a plugin's private
    /// history, so once the host has asked for an advance or a truncate/reset
    /// the plugin, the component, the host, the output cursor, and the
    /// published evidence can be at different points and restoring the
    /// component's coordinate alone does not restore the rest
    /// (review finding \[387\]).
    ///
    /// [`MeIntegratorBackend`]: crate::fmi_me::MeIntegratorBackend
    NumericalStep,
}

impl MeSessionLoss {
    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::AcceptedPoint => "the component left the accepted point",
            Self::EventRefresh => "an Event Mode refresh did not complete",
            Self::InputApplication => "an input did not reach every correlated owner",
            Self::Restart => "a restart did not rebuild the lifecycle",
            Self::NumericalStep => "an accepted numerical step did not complete",
        }
    }
}

impl std::fmt::Display for MeSessionLoss {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(self.label())
    }
}

/// Map a latched capability failure onto the public categories.
///
/// A component evaluation additionally records the Continuous-Time Mode stage
/// that raised it (review finding \[340\]§4); an inactive-capability misuse is
/// the plugin's own typed contract failure and keeps that identity.
pub(super) fn latched_failure(error: MeIntegrationError) -> MeSessionError {
    match error {
        MeIntegrationError::Component(component) => {
            MeSessionError::Component(component.at_stage(MeStage::Integration))
        }
        other => MeSessionError::Integration(other),
    }
}

impl From<TimeoutExceeded> for MeSessionError {
    fn from(value: TimeoutExceeded) -> Self {
        Self::Timeout {
            seconds: value.seconds,
        }
    }
}

/// Map the host-private recorder's failure onto the public categories.
///
/// SPEC_0044 §6 ME-INT-003 requires allocation failures to stay typed and
/// distinguishable; every other recorder violation is the host breaking its own
/// trace contract. Neither needs the recorder's concrete shape on the public
/// API (review finding \[366\]§3). The recorder is host storage, so its
/// allocation failure maps to the host's own allocation category rather than to
/// the component's (review finding \[370\]§3).
impl From<MeTraceViolation> for MeSessionError {
    fn from(value: MeTraceViolation) -> Self {
        match value {
            MeTraceViolation::Allocation { context, entries } => {
                Self::Allocation { context, entries }
            }
            other => Self::Contract {
                reason: other.to_string(),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// ME-INT-003 keeps allocation a category of its own, and the recorder is
    /// *host* storage. Attributing its exhaustion to the FMI component would
    /// both lose the category and blame the wrong party
    /// (review finding \[370\]§3).
    #[test]
    fn a_recorder_allocation_failure_is_a_host_allocation_not_a_component_failure() {
        let violation = MeTraceViolation::Allocation {
            context: "trace rows",
            entries: 17,
        };
        match MeSessionError::from(violation) {
            MeSessionError::Allocation { context, entries } => {
                assert_eq!(context, "trace rows");
                assert_eq!(entries, 17);
            }
            other => panic!("recorder allocation must stay a host allocation failure: {other}"),
        }
    }
}
