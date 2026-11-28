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

pub mod snapshot;
pub mod speed;

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

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
    /// The shard speed the stage times were divided by (rule B), when the
    /// gauge was trusted.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub speed: Option<f64>,
    /// Why the speed gauge could not be trusted, when it could not: then the
    /// model's certification is inconclusive, never judged on absolute time.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub timing_inconclusive: Option<String>,
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
    /// The comparator's violation mass over the compared channels, when the
    /// model is scored.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub trace_violation_mass: Option<f64>,
    /// The mean of the compared channels' bounded normalized L1 errors, when
    /// the model is scored: a continuous accuracy measure that moves inside a
    /// band, where violation mass (deviation channels only) stays zero.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub mean_channel_bounded_l1: Option<f64>,
    pub certified: bool,
    pub roster: bool,
    pub stage_seconds: StageSeconds,
}

/// Every sim target's state, by model name.
pub type ModelStates = BTreeMap<String, ModelState>;

/// What a reviewed reference boundary allows to change between two snapshots:
/// a boundary's `reviewed_changes`, which is reviewed with the boundary chain
/// it sits in (the ratchet requires the snapshot's chain to equal the checked-in
/// one).
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default, deny_unknown_fields)]
pub struct ReviewedChanges {
    /// Sim targets the boundary removes.
    pub removed_targets: BTreeSet<String>,
    /// Models the boundary newly excludes by policy.
    pub newly_excluded: BTreeSet<String>,
    /// Models whose compared channel set the boundary changes (for example a
    /// compiler rename).
    pub channel_set_changes: BTreeSet<String>,
}

impl ReviewedChanges {
    /// The changes of every boundary a promotion crosses, read from each
    /// link's `reviewed_changes`.
    pub fn from_links(links: &[&serde_json::Value]) -> Result<Self, String> {
        let mut all = Self::default();
        for link in links {
            let Some(changes) = link.get("reviewed_changes") else {
                continue;
            };
            let changes: Self = serde_json::from_value(changes.clone())
                .map_err(|error| format!("a boundary's reviewed_changes: {error}"))?;
            all.removed_targets.extend(changes.removed_targets);
            all.newly_excluded.extend(changes.newly_excluded);
            all.channel_set_changes.extend(changes.channel_set_changes);
        }
        Ok(all)
    }
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

impl Regression {
    /// Whether an OMC context change can explain this regression: what it
    /// measures depends on the OMC reference (the band, the compared channel
    /// names, the timing margin, which is relative to OMC time, and an OMC
    /// failure). A stage drop, a target that leaves, a rumoca trace failure and
    /// an exclusion kind change are rumoca's or policy's, and are never
    /// excused by one.
    pub fn depends_on_omc(&self) -> bool {
        match self {
            Self::BandDropped { .. }
            | Self::ChannelsDropped { .. }
            | Self::Uncertified
            | Self::LeftScoring(UnscoredCause::OmcFailed) => true,
            Self::LeftTargets
            | Self::StageDropped { .. }
            | Self::LeftScoring(_)
            | Self::ExclusionKindChanged { .. } => false,
        }
    }
}

impl fmt::Display for Regression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LeftTargets => write!(f, "left the sim targets without a reviewed removal"),
            Self::StageDropped { from, to } => write!(f, "stage dropped: {from:?} -> {to:?}"),
            Self::BandDropped { from, to } => {
                write!(f, "band dropped: {} -> {}", from.as_str(), to.as_str())
            }
            Self::ChannelsDropped { from, to } => {
                write!(f, "compared channels dropped: {from} -> {to}")
            }
            Self::LeftScoring(cause) => write!(f, "left scoring: {cause:?}"),
            Self::Uncertified => write!(f, "no longer certified"),
            Self::ExclusionKindChanged { from, to } => write!(
                f,
                "exclusion kind changed without a boundary: {} -> {}",
                from.as_str(),
                to.as_str()
            ),
        }
    }
}

/// The schema of an [`EvidenceStates`] file.
pub const EVIDENCE_STATES_SCHEMA_VERSION: u32 = 1;

/// Model states measured for the old side of a crossing whose promoted
/// baseline predates `model_states`, pinned by a reviewed boundary.
#[derive(Debug, Clone, PartialEq, Deserialize)]
pub struct EvidenceStates {
    pub schema_version: u32,
    /// The run the states were derived from.
    pub source_run: String,
    pub source_commit: String,
    /// What a reader must know about that run before trusting a comparison.
    pub caveats: Vec<String>,
    pub model_states: ModelStates,
}

/// Parse an evidence file, refusing one of another schema.
pub fn parse_evidence_states(text: &str) -> Result<EvidenceStates, String> {
    let evidence: EvidenceStates = serde_json::from_str(text)
        .map_err(|error| format!("parse the evidence model states: {error}"))?;
    if evidence.schema_version != EVIDENCE_STATES_SCHEMA_VERSION {
        return Err(format!(
            "evidence model states schema {} is not the supported {}",
            evidence.schema_version, EVIDENCE_STATES_SCHEMA_VERSION
        ));
    }
    Ok(evidence)
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
        if current.stage_seconds.timing_inconclusive.is_some() {
            return Transition::Inconclusive("certification timing gauge inconclusive");
        }
        return Transition::Regressed(Regression::Uncertified);
    }
    if current.stage > previous.stage {
        Transition::Progressed
    } else {
        Transition::Unchanged
    }
}

/// The continuous trace measures over the models **scored on both sides**: a
/// newly scored model cannot move them, so they stay floors without counting
/// progress as regression. They catch an accuracy loss that stays inside every
/// model's band, which band transitions alone cannot see.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SharedFloors {
    pub models: usize,
    pub violation_mass: (f64, f64),
    pub high_or_near: (usize, usize),
    /// Sum of the models' mean channel bounded L1 errors, before and after.
    pub mean_channel_bounded_l1: (f64, f64),
}

/// Relative tolerance of the violation-mass floor, the ratchet's float rule.
const VIOLATION_MASS_EPSILON: f64 = 1.0e-9;

pub fn shared_floors(before: &ModelStates, after: &ModelStates) -> SharedFloors {
    let high_or_near = |state: &ModelState| {
        matches!(
            state.trace,
            TraceState::Scored(BandLabel::High | BandLabel::Near)
        )
    };
    let mut floors = SharedFloors {
        models: 0,
        violation_mass: (0.0, 0.0),
        high_or_near: (0, 0),
        mean_channel_bounded_l1: (0.0, 0.0),
    };
    for (model, previous) in before {
        let Some(current) = after.get(model) else {
            continue;
        };
        let (Some(was), Some(is)) = (previous.trace_violation_mass, current.trace_violation_mass)
        else {
            continue;
        };
        floors.models += 1;
        floors.violation_mass.0 += was;
        floors.violation_mass.1 += is;
        floors.high_or_near.0 += usize::from(high_or_near(previous));
        floors.high_or_near.1 += usize::from(high_or_near(current));
        floors.mean_channel_bounded_l1.0 += previous.mean_channel_bounded_l1.unwrap_or(0.0);
        floors.mean_channel_bounded_l1.1 += current.mean_channel_bounded_l1.unwrap_or(0.0);
    }
    floors
}

impl SharedFloors {
    /// The rise of the shared models' summed mean channel error, as a sentence,
    /// when it rose at all. Simulations are deterministic, so on the same
    /// comparator unchanged accuracy is bit-equal and any rise is real.
    pub fn accuracy_change(&self) -> Option<String> {
        let (was, is) = self.mean_channel_bounded_l1;
        (is > was).then(|| {
            format!(
                "mean channel bounded L1 over {} models scored in both: {was:.6e} -> {is:.6e}",
                self.models
            )
        })
    }

    /// Each floor this comparison lowers, as a sentence.
    pub fn regressions(&self) -> Vec<String> {
        let mut out = Vec::new();
        let (was, is) = self.violation_mass;
        if is > was + VIOLATION_MASS_EPSILON * was.abs().max(1.0) {
            out.push(format!(
                "trace violation mass over {} models scored in both: {was:.6e} -> {is:.6e}",
                self.models
            ));
        }
        let (was, is) = self.high_or_near;
        if is < was {
            out.push(format!(
                "high+near trace agreement over {} models scored in both: {was} -> {is}",
                self.models
            ));
        }
        out
    }
}

/// One model's mean channel bounded L1 error before and after.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ModelAccuracy {
    pub model: String,
    pub before: f64,
    pub after: f64,
}

/// Every model scored on both sides whose mean channel bounded L1 error rose.
///
/// The unit is the model, not the sum: a sum can fall while one model gets
/// worse. A rise is never refused on its size, since a legitimate change can
/// trade some models' accuracy for others'. It must instead be **stated** by the
/// generated, checked-in [`AccuracyStatement`] keyed to the baseline, and the
/// statement must equal the measured list exactly: every rising model, with its values. Simulations and
/// the comparator are deterministic (bit-equal across runners), so a stale,
/// incomplete or guessed statement cannot match, and an unstated rise is
/// refused. The summed error is reported, not ruled on.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AccuracyChange {
    /// The rising models, by name.
    pub rose: Vec<ModelAccuracy>,
}

/// The models scored on both sides whose mean channel error rose, or `None`
/// when none did.
pub fn accuracy_change(before: &ModelStates, after: &ModelStates) -> Option<AccuracyChange> {
    let rose = before
        .iter()
        .filter_map(|(model, previous)| {
            let current = after.get(model)?;
            let (was, is) = (
                previous.mean_channel_bounded_l1?,
                current.mean_channel_bounded_l1?,
            );
            (is > was).then(|| ModelAccuracy {
                model: model.clone(),
                before: was,
                after: is,
            })
        })
        .collect::<Vec<_>>();
    (!rose.is_empty()).then_some(AccuracyChange { rose })
}

/// Read an evidence file whose bytes a reviewed boundary pins by sha256,
/// refusing bytes that do not match the pin or a file of another schema.
pub fn verify_evidence(bytes: &[u8], pinned_sha256: &str) -> Result<EvidenceStates, String> {
    use sha2::{Digest, Sha256};
    let actual = format!("{:x}", Sha256::digest(bytes));
    if actual != pinned_sha256 {
        return Err(format!(
            "evidence model states have sha256 {actual}, the reviewed boundary pins {pinned_sha256}"
        ));
    }
    let text =
        std::str::from_utf8(bytes).map_err(|error| format!("evidence is not UTF-8: {error}"))?;
    parse_evidence_states(text)
}

/// The checked-in accuracy statement, relative to the workspace root.
pub const ACCURACY_STATEMENT_FILE: &str =
    "crates/rumoca-test-msl/tests/msl_tests/msl_accuracy_changes.json";

/// The schema of an [`AccuracyStatement`] file.
pub const ACCURACY_STATEMENT_SCHEMA_VERSION: u32 = 1;

/// The checked-in, generated statement of every model whose mean channel error
/// rose against one baseline, keyed to that baseline's sha256. Once another
/// baseline is promoted the key no longer matches and the file is refused as
/// stale, so a statement is used against exactly one baseline.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AccuracyStatement {
    pub schema_version: u32,
    /// The sha256 of the baseline file the rises are measured against.
    pub baseline_sha256: String,
    pub rose: Vec<ModelAccuracy>,
}

/// The lowercase hex sha256 of `bytes`.
pub fn sha256_hex(bytes: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    format!("{:x}", Sha256::digest(bytes))
}

/// The accuracy change a statement file states against the baseline whose
/// sha256 is `baseline_sha256`; `None` when there is no file. A file keyed to
/// another baseline, of another schema, or stating no rise is refused.
pub fn stated_accuracy(
    statement: Option<&[u8]>,
    baseline_sha256: &str,
) -> Result<Option<AccuracyChange>, String> {
    let Some(bytes) = statement else {
        return Ok(None);
    };
    let statement: AccuracyStatement = serde_json::from_slice(bytes)
        .map_err(|error| format!("invalid accuracy statement: {error}"))?;
    if statement.schema_version != ACCURACY_STATEMENT_SCHEMA_VERSION {
        return Err(format!(
            "accuracy statement schema {} is not {ACCURACY_STATEMENT_SCHEMA_VERSION}",
            statement.schema_version
        ));
    }
    if statement.baseline_sha256 != baseline_sha256 {
        return Err(format!(
            "stale accuracy statement: it is keyed to baseline {}, the run is judged against \
             {baseline_sha256}; regenerate it with `cargo xtask verify msl-accuracy-changes` (it deletes the file \
             when no model's error rose)",
            statement.baseline_sha256
        ));
    }
    if statement.rose.is_empty() {
        return Err("the accuracy statement states no rise; delete it".to_string());
    }
    Ok(Some(AccuracyChange {
        rose: statement.rose,
    }))
}

/// The statement file for `measured` against the baseline `baseline_sha256`,
/// or `None` when no model's error rose (then there must be no file).
pub fn accuracy_statement(
    baseline_sha256: &str,
    measured: Option<&AccuracyChange>,
) -> Option<String> {
    let measured = measured?;
    let statement = AccuracyStatement {
        schema_version: ACCURACY_STATEMENT_SCHEMA_VERSION,
        baseline_sha256: baseline_sha256.to_string(),
        rose: measured.rose.clone(),
    };
    let mut text = serde_json::to_string_pretty(&statement)
        .expect("an accuracy statement of strings and finite floats serializes");
    text.push('\n');
    Some(text)
}

/// The per-model rule's decision, as lines.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct PerModelVerdict {
    pub regressions: Vec<String>,
    /// Must be re-measured before anything can pass; never a pass.
    pub inconclusive: Vec<String>,
    pub improvements: Vec<String>,
    /// Printed, not ruled on.
    pub report: Vec<String>,
}

/// Judge `after` against `before` model by model: the transition table (with
/// the changes the crossed boundaries review), the continuous floors over the
/// models scored in both, and the accuracy rule: every model whose mean channel
/// error rose is listed, largest first, and the list must equal `stated`
/// exactly. Across an OMC context change only the regressions an OMC change
/// cannot explain count ([`Regression::depends_on_omc`]), and the trace
/// measures are not compared.
pub fn judge(
    before: &ModelStates,
    after: &ModelStates,
    reviewed: &ReviewedChanges,
    stated: Option<&AccuracyChange>,
    omc_context_changed: bool,
) -> PerModelVerdict {
    let mut verdict = PerModelVerdict::default();
    for (model, transition) in transitions(before, after, reviewed) {
        match transition {
            Transition::Regressed(why) => {
                if !(omc_context_changed && why.depends_on_omc()) {
                    verdict.regressions.push(format!("{model}: {why}"));
                }
            }
            Transition::Inconclusive(why) => verdict.inconclusive.push(format!("{model}: {why}")),
            Transition::Progressed => verdict.improvements.push(format!("{model}: progressed")),
            Transition::New => verdict
                .improvements
                .push(format!("{model}: new sim target")),
            Transition::Reviewed => verdict
                .report
                .push(format!("{model}: changed as a crossed boundary reviews")),
            Transition::Unchanged => {}
        }
    }
    if omc_context_changed {
        return verdict;
    }
    let floors = shared_floors(before, after);
    verdict.regressions.extend(floors.regressions());
    let (was, is) = floors.mean_channel_bounded_l1;
    verdict.report.push(format!(
        "mean channel bounded L1 over {} models scored in both: {was:.6e} -> {is:.6e} (reported)",
        floors.models
    ));
    let measured = accuracy_change(before, after);
    if let Some(change) = &measured {
        let mut ranked = change.rose.clone();
        ranked.sort_by(|a, b| (b.after - b.before).total_cmp(&(a.after - a.before)));
        verdict.report.push(format!(
            "{} models' mean channel error rose, largest first:",
            ranked.len()
        ));
        verdict.report.extend(
            ranked
                .iter()
                .map(|model| format!("  {} {:e} -> {:e}", model.model, model.before, model.after)),
        );
    }
    if measured.as_ref() != stated {
        verdict.regressions.push(match &measured {
            Some(change) => format!(
                "{} models' mean channel error rose and {ACCURACY_STATEMENT_FILE} does not state \
                 exactly that; regenerate it with `cargo xtask verify msl-accuracy-changes`",
                change.rose.len()
            ),
            None => format!(
                "{ACCURACY_STATEMENT_FILE} states an accuracy change, but no model's mean channel \
                 error rose; delete it"
            ),
        });
    }
    verdict
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
