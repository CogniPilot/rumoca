//! MSL quality-gate rules shared by the quality gate and xtask.
//!
//! The ratchet judges a run by what happened to each model rather than by
//! aggregate counts: an aggregate cannot tell a model that got worse from one
//! that newly compiles and then fails, or from one that is newly scored. A
//! quality snapshot therefore records each sim target's [`ModelState`], and
//! [`transitions`] compares two snapshots model by model.
//!
//! This crate depends on no workspace crate, so xtask can share the rules
//! without building the compiler.

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

/// Which agreement band the comparator placed a model in, or `Absent` when the
/// model is not in the compared set at all.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BandLabel {
    /// Strict-high agreement: the only quotable parity band.
    High,
    /// Minor ("near") agreement.
    Near,
    /// Measured deviation.
    Deviation,
    /// Compared, but a projection block of the run fell back above the policy
    /// rate (SPEC_0044 ME-PROJ-003); never quoted as strict-high parity,
    /// whatever its channels.
    Fallback,
    /// Not compared in this run; the row carries an exit reason.
    Absent,
}

impl BandLabel {
    /// Stable wire name, matching the serialized form.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::High => "high",
            Self::Near => "near",
            Self::Deviation => "deviation",
            Self::Fallback => "fallback",
            Self::Absent => "absent",
        }
    }

    /// Whether this model is in the compared set.
    pub fn is_compared(self) -> bool {
        !matches!(self, Self::Absent)
    }

    /// Agreement rank of a compared band; higher is better. A fallback band is
    /// compared but never strict-high, so it ranks with minor agreement.
    fn rank(self) -> u8 {
        match self {
            Self::High => 3,
            Self::Near | Self::Fallback => 2,
            Self::Deviation => 1,
            Self::Absent => 0,
        }
    }
}

/// The compile phases in pipeline order: the one statement of that order.
///
/// A model result's `phase_reached` names the phase that *failed*, or
/// `Success`, which ranks after all of them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CompilePhase {
    Parse,
    Resolve,
    NeedsInner,
    Instantiate,
    Typecheck,
    Flatten,
    ToDae,
}

impl CompilePhase {
    /// Every phase, in pipeline order.
    pub const ALL: [Self; 7] = [
        Self::Parse,
        Self::Resolve,
        Self::NeedsInner,
        Self::Instantiate,
        Self::Typecheck,
        Self::Flatten,
        Self::ToDae,
    ];

    /// The name a model result's `phase_reached` uses for this phase.
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Parse => "Parse",
            Self::Resolve => "Resolve",
            Self::NeedsInner => "NeedsInner",
            Self::Instantiate => "Instantiate",
            Self::Typecheck => "Typecheck",
            Self::Flatten => "Flatten",
            Self::ToDae => "ToDae",
        }
    }
}

/// The `phase_reached` of a model that compiled.
pub const COMPILE_SUCCESS: &str = "Success";

/// Read a model result's `phase_reached`: `Ok(None)` for a model that
/// compiled, the phase it failed in otherwise. An unknown name is an error,
/// never a guess, so a renamed or new phase cannot be mis-ranked.
pub fn failed_compile_phase(phase_reached: &str) -> Result<Option<CompilePhase>, String> {
    if phase_reached == COMPILE_SUCCESS {
        return Ok(None);
    }
    CompilePhase::ALL
        .into_iter()
        .find(|phase| phase.as_str() == phase_reached)
        .map(Some)
        .ok_or_else(|| format!("unknown compile phase `{phase_reached}`"))
}

/// The rank of a `phase_reached` in pipeline order (`Success` after every
/// phase), or `None` for a name that is not a compile phase.
pub fn compile_phase_rank(phase_reached: &str) -> Option<usize> {
    match failed_compile_phase(phase_reached) {
        Ok(Some(phase)) => Some(phase as usize),
        Ok(None) => Some(CompilePhase::ALL.len()),
        Err(_) => None,
    }
}

/// How far a model got, in pipeline order. A compile failure records the
/// phase it failed in, ranked by [`CompilePhase`]; a compiled model is refined
/// by its initialization and simulation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ModelStage {
    FailedAt(CompilePhase),
    /// Compiled; not initialized (initialization failed, timed out or was not
    /// attempted).
    Compiled,
    /// Initialized; the simulation did not complete.
    Initialized,
    Simulated,
}

/// Why a model that simulates is not held to strict-high pointwise parity
/// (SPEC_0033 typed trace exceptions). There is no other kind: a model that
/// simulates without one of these must reach the strict-high band.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TraceExceptionKind {
    /// A random or impure source: traces differ by construction, so the
    /// obligation is statistical, not pointwise.
    ImpureSource,
    /// The OMC reference does not resolve the model: it is not converged or
    /// not accurate enough for the observables compared.
    ReferenceFailure,
    /// A documented property of the model, such as chaos, a discontinuity
    /// wrapped in `noEvent` by design, or an observable the model leaves
    /// underdetermined, makes one trajectory non-identifying.
    ModelIssue,
    /// A reviewed proof that pointwise comparison cannot identify the model.
    Nonidentifiable,
    /// The comparator, not the model or the reference, cannot identify the
    /// channels; the row names the comparator improvement that retires it.
    ComparatorLimitation,
}

impl TraceExceptionKind {
    /// Every kind, with its wire name.
    const ALL: [Self; 5] = [
        Self::ImpureSource,
        Self::ReferenceFailure,
        Self::ModelIssue,
        Self::Nonidentifiable,
        Self::ComparatorLimitation,
    ];

    pub fn parse(value: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|kind| kind.as_str() == value)
    }

    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::ImpureSource => "impure_source",
            Self::ReferenceFailure => "reference_failure",
            Self::ModelIssue => "model_issue",
            Self::Nonidentifiable => "nonidentifiable",
            Self::ComparatorLimitation => "comparator_limitation",
        }
    }
}

/// Why a model's trace was not scored. Who caused it decides how the ratchet
/// treats a model that leaves scoring.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum UnscoredCause {
    /// Excluded by a reviewed row of the tracked exclusion policy, of this kind.
    PolicyExcluded(TraceExceptionKind),
    /// The OMC reference failed or is missing: not rumoca's doing.
    OmcFailed,
    /// Rumoca reported a simulation but produced no usable trace.
    RumocaTraceMissing,
    /// The comparison of rumoca's trace failed or had nothing to compare.
    RumocaTraceError,
    /// Rumoca's own trace evidence says pointwise comparison cannot certify it.
    TraceNonidentifiable,
    /// Not simulated by rumoca in this run; the stage says why.
    NotSimulated,
}

/// A model's trace judgement: a band, or a typed reason it was not scored.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TraceState {
    Scored(BandLabel),
    Unscored(UnscoredCause),
}

/// Elapsed time of each watchdog stage, the times its budgets are judged on.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct StageSeconds {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub build: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub initialization: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub run: Option<f64>,
    /// The stage the watchdog killed, when one was killed, by the worker's
    /// own name for it. The crate depends on no workspace crate, so the
    /// worker's phase type is not available here; a reader parses this with
    /// that type and refuses a name it does not know.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub timeout_phase: Option<String>,
}

/// One sim target's state in a quality snapshot.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModelState {
    pub stage: ModelStage,
    /// The failure status behind `stage` (an error code, or the initialization
    /// or simulation status), when the model did not simulate.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub outcome: Option<String>,
    pub trace: TraceState,
    pub compared_channels: usize,
    pub certified: bool,
    pub roster: bool,
    pub stage_seconds: StageSeconds,
}

/// Every sim target's state, by model name.
pub type ModelStates = BTreeMap<String, ModelState>;

/// What a reviewed reference boundary allows to change between two snapshots.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ReviewedChanges {
    /// Sim targets the boundary removes.
    pub removed_targets: BTreeSet<String>,
    /// Models the boundary newly excludes by policy.
    pub newly_excluded: BTreeSet<String>,
    /// Models whose compared channel set the boundary changes (for example a
    /// compiler rename).
    pub channel_set_changes: BTreeSet<String>,
}

/// Why a model is worse than its own previous state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Regression {
    /// It is no longer a sim target, and no reviewed boundary removes it.
    LeftTargets,
    /// It stopped at an earlier stage, not on a watchdog kill.
    StageDropped { from: ModelStage, to: ModelStage },
    /// It is scored in both, at a lower band.
    BandDropped { from: BandLabel, to: BandLabel },
    /// It is scored in both, over fewer channels, without a reviewed change.
    ChannelsDropped { from: usize, to: usize },
    /// It left scoring for a cause that is rumoca's, or by a policy exclusion
    /// no reviewed boundary makes.
    LeftScoring(UnscoredCause),
    /// It was certified and is not.
    Uncertified,
    /// It stays excluded, but under another kind of exception, without a
    /// reviewed boundary.
    ExclusionKindChanged {
        from: TraceExceptionKind,
        to: TraceExceptionKind,
    },
}

/// What happened to one model between two snapshots.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Transition {
    /// Worse than its own previous state: fails the ratchet.
    Regressed(Regression),
    /// Cannot be judged from this run (a watchdog kill, or the OMC reference
    /// failed): re-measured, never a pass.
    Inconclusive(&'static str),
    /// Better than before: reached a later stage, or is newly scored.
    Progressed,
    /// A sim target the previous snapshot did not have.
    New,
    /// A change a reviewed boundary allows.
    Reviewed,
    Unchanged,
}

/// The ratchet's verdict over every transition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Verdict {
    Pass,
    /// Nothing regressed, but some models must be re-measured first.
    Inconclusive,
    Fail,
}

/// Compare `after` with `before` model by model; see the crate docs.
pub fn transitions(
    before: &ModelStates,
    after: &ModelStates,
    reviewed: &ReviewedChanges,
) -> BTreeMap<String, Transition> {
    let mut out = BTreeMap::new();
    for (model, previous) in before {
        let transition = match after.get(model) {
            None if reviewed.removed_targets.contains(model) => Transition::Reviewed,
            None => Transition::Regressed(Regression::LeftTargets),
            Some(current) => transition(model, previous, current, reviewed),
        };
        out.insert(model.clone(), transition);
    }
    for model in after.keys() {
        if !before.contains_key(model) {
            out.insert(model.clone(), Transition::New);
        }
    }
    out
}

fn transition(
    model: &str,
    previous: &ModelState,
    current: &ModelState,
    reviewed: &ReviewedChanges,
) -> Transition {
    if current.stage < previous.stage {
        return if current.stage_seconds.timeout_phase.is_some() {
            Transition::Inconclusive("watchdog kill")
        } else {
            Transition::Regressed(Regression::StageDropped {
                from: previous.stage,
                to: current.stage,
            })
        };
    }
    let trace = match (previous.trace, current.trace) {
        (TraceState::Scored(from), TraceState::Scored(to)) if to.rank() < from.rank() => {
            Some(Transition::Regressed(Regression::BandDropped { from, to }))
        }
        (TraceState::Scored(_), TraceState::Scored(_))
            if current.compared_channels < previous.compared_channels
                && !reviewed.channel_set_changes.contains(model) =>
        {
            Some(Transition::Regressed(Regression::ChannelsDropped {
                from: previous.compared_channels,
                to: current.compared_channels,
            }))
        }
        (TraceState::Scored(_), TraceState::Unscored(cause)) => Some(match cause {
            UnscoredCause::OmcFailed => Transition::Inconclusive("OMC reference failed"),
            UnscoredCause::PolicyExcluded(_) if reviewed.newly_excluded.contains(model) => {
                Transition::Reviewed
            }
            cause => Transition::Regressed(Regression::LeftScoring(cause)),
        }),
        (TraceState::Unscored(_), TraceState::Scored(_)) => Some(Transition::Progressed),
        (
            TraceState::Unscored(UnscoredCause::PolicyExcluded(from)),
            TraceState::Unscored(UnscoredCause::PolicyExcluded(to)),
        ) if from != to && !reviewed.newly_excluded.contains(model) => {
            Some(Transition::Regressed(Regression::ExclusionKindChanged {
                from,
                to,
            }))
        }
        _ => None,
    };
    if let Some(transition) = trace {
        return transition;
    }
    if previous.certified && !current.certified {
        return Transition::Regressed(Regression::Uncertified);
    }
    if current.stage > previous.stage {
        Transition::Progressed
    } else {
        Transition::Unchanged
    }
}

/// Fail on any regression; otherwise inconclusive while anything must be
/// re-measured; otherwise pass.
pub fn verdict(transitions: &BTreeMap<String, Transition>) -> Verdict {
    let mut verdict = Verdict::Pass;
    for transition in transitions.values() {
        match transition {
            Transition::Regressed(_) => return Verdict::Fail,
            Transition::Inconclusive(_) => verdict = Verdict::Inconclusive,
            _ => {}
        }
    }
    verdict
}

#[cfg(test)]
mod tests;
