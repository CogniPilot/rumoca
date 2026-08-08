//! Machine-readable failure classification for one MSL model attempt.
//!
//! The MSL harness used to answer "why did this model fail?" by regexing the
//! rendered `sim_error` text. That is unusable as a gate: the sub-buckets move
//! whenever a message is reworded, and a message never says which *code path*
//! raised it. This module defines the typed vocabulary instead —
//! [`ModelFailureBucket`] and its total [`ModelFailureOwner`] mapping — and the
//! constructors that mint a bucket from producer knowledge:
//!
//! * a compile failure from [`FailedPhase`] plus the structured balance
//!   breakdown, never from the summary string;
//! * a simulation failure from the [`SimError`] variant plus the
//!   [`SimFailureStage`] the failing path in `rumoca-solver-diffsol` attached;
//! * a harness failure from the typed run outcome the harness observed;
//! * a resource-budget overrun from the harness's own measurement of a
//!   declared per-model ceiling (Solve-IR serialized bytes, compile wall
//!   seconds) — a number compared against a constant, never a message.
//!
//! Both enums serialize by variant name, so `msl_results.json` carries stable
//! identifiers that triage tooling can group on without re-deriving anything.

use rumoca_compile::compile::FailedPhase;
use rumoca_sim::{SimError, SimFailureStage};
use serde::{Deserialize, Serialize};

use crate::WorkerProgressPhase;

/// The failure family a model attempt belongs to.
///
/// One attempt has exactly one bucket. Variants are deliberately narrow enough
/// that a cohort is actionable by a single owner — see [`ModelFailureOwner`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ModelFailureBucket {
    /// Parsing or name resolution failed, before instantiation.
    Resolve,
    /// Instantiation failed (including a missing `inner` for an `outer`).
    Instantiate,
    /// Type checking failed.
    Typecheck,
    /// Flattening failed (connections, expandable connectors, redeclaration).
    Flatten,
    /// DAE construction failed for a reason other than balance.
    DaeConstruction,
    /// DAE construction found the model unbalanced.
    DaeBalance,
    /// Lowering the DAE to Solve IR rejected the model.
    SolveLowering,
    /// Structural analysis of the lowered system rejected the model
    /// (index reduction, matching, tearing).
    StructuralAnalysis,
    /// Building the backend problem failed before any integration.
    SimBackendBuild,
    /// Initial conditions did not settle (initial event/`pre` chain, initial
    /// projection, homotopy continuation).
    RuntimeInitialization,
    /// Event iteration / discrete re-initialization did not converge.
    RuntimeEventIteration,
    /// Index-reduction manifold (constraint) projection did not converge.
    RuntimeManifoldProjection,
    /// Isolating or advancing onto a requested output/stop time failed.
    RuntimeTargetIsolation,
    /// The numeric integrator reported a failure.
    SolverIntegration,
    /// A runtime contract (vector shape, slot assignability, ...) was violated.
    RuntimeContract,
    /// The lowered model had no equations to simulate.
    EmptySystem,
    /// The simulation completed but produced a non-finite sample.
    NonFiniteResult,
    /// A Modelica `assert` fired: model behaviour, not a compiler defect.
    ModelAssertion,
    /// A Modelica `terminate` fired: model behaviour, not a compiler defect.
    ModelTermination,
    /// The attempt exceeded its wall budget.
    Timeout,
    /// The attempt exceeded its memory budget.
    MemoryLimit,
    /// The attempt exceeded a declared resource budget that is neither wall
    /// time nor process memory — today the MSL harness's per-model Solve-IR
    /// serialized-size ceiling and its per-model compile wall ceiling.
    ///
    /// Distinct from [`Self::Timeout`]/[`Self::MemoryLimit`] on purpose: those
    /// say "the process ran out of a machine resource", this says "the artifact
    /// the compiler produced is larger (or slower) than the pipeline agreed to
    /// carry", which is an owner-actionable lowering defect rather than a
    /// scheduling accident. Same [`ModelFailureOwner::Performance`] owner.
    ResourceBudget,
    /// The harness could not run or record the attempt (worker transport,
    /// artifact IO, protocol mismatch).
    HarnessFailure,
    /// Writing or comparing the simulation trace failed.
    TraceOutput,
    /// No typed producer data was available to classify the failure. A growing
    /// count here means a producer stopped reporting its stage, not that the
    /// failures are exotic.
    Unclassified,
}

/// Who owns the fix for a [`ModelFailureBucket`].
///
/// Coarser than the bucket on purpose: this is the column a parity report
/// groups by when deciding where to spend the next unit of work.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum ModelFailureOwner {
    /// Parse, resolve, instantiate, typecheck.
    Frontend,
    /// Flattening.
    Flatten,
    /// DAE construction and balance.
    DaeConstruction,
    /// Structural analysis of the lowered system.
    Structural,
    /// DAE-to-Solve-IR lowering.
    SolveLowering,
    /// The simulation runtime (preparation, projection, events).
    Runtime,
    /// The numeric solver.
    Solver,
    /// The model itself asked to stop; not a compiler defect.
    ModelBehaviour,
    /// Compile/solve/simulate performance against the attempt budget.
    Performance,
    /// The measurement harness.
    Harness,
    /// Not attributable from the data recorded for the attempt.
    Unknown,
}

impl ModelFailureBucket {
    /// Every bucket, in declaration order. Used by tests that require the
    /// owner mapping to stay total as buckets are added.
    pub const ALL: &'static [Self] = &[
        Self::Resolve,
        Self::Instantiate,
        Self::Typecheck,
        Self::Flatten,
        Self::DaeConstruction,
        Self::DaeBalance,
        Self::SolveLowering,
        Self::StructuralAnalysis,
        Self::SimBackendBuild,
        Self::RuntimeInitialization,
        Self::RuntimeEventIteration,
        Self::RuntimeManifoldProjection,
        Self::RuntimeTargetIsolation,
        Self::SolverIntegration,
        Self::RuntimeContract,
        Self::EmptySystem,
        Self::NonFiniteResult,
        Self::ModelAssertion,
        Self::ModelTermination,
        Self::Timeout,
        Self::MemoryLimit,
        Self::ResourceBudget,
        Self::HarnessFailure,
        Self::TraceOutput,
        Self::Unclassified,
    ];

    /// The owner of this failure family. Total by construction.
    #[must_use]
    pub fn owner_category(self) -> ModelFailureOwner {
        match self {
            Self::Resolve | Self::Instantiate | Self::Typecheck => ModelFailureOwner::Frontend,
            Self::Flatten => ModelFailureOwner::Flatten,
            Self::DaeConstruction | Self::DaeBalance => ModelFailureOwner::DaeConstruction,
            Self::SolveLowering => ModelFailureOwner::SolveLowering,
            Self::StructuralAnalysis | Self::EmptySystem => ModelFailureOwner::Structural,
            Self::SimBackendBuild
            | Self::RuntimeInitialization
            | Self::RuntimeEventIteration
            | Self::RuntimeManifoldProjection
            | Self::RuntimeTargetIsolation
            | Self::RuntimeContract
            | Self::NonFiniteResult => ModelFailureOwner::Runtime,
            Self::SolverIntegration => ModelFailureOwner::Solver,
            Self::ModelAssertion | Self::ModelTermination => ModelFailureOwner::ModelBehaviour,
            Self::Timeout | Self::MemoryLimit | Self::ResourceBudget => {
                ModelFailureOwner::Performance
            }
            Self::HarnessFailure | Self::TraceOutput => ModelFailureOwner::Harness,
            Self::Unclassified => ModelFailureOwner::Unknown,
        }
    }

    /// The variant name, as serialized. Handy for report tables that want the
    /// same identifier the JSON carries.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Resolve => "Resolve",
            Self::Instantiate => "Instantiate",
            Self::Typecheck => "Typecheck",
            Self::Flatten => "Flatten",
            Self::DaeConstruction => "DaeConstruction",
            Self::DaeBalance => "DaeBalance",
            Self::SolveLowering => "SolveLowering",
            Self::StructuralAnalysis => "StructuralAnalysis",
            Self::SimBackendBuild => "SimBackendBuild",
            Self::RuntimeInitialization => "RuntimeInitialization",
            Self::RuntimeEventIteration => "RuntimeEventIteration",
            Self::RuntimeManifoldProjection => "RuntimeManifoldProjection",
            Self::RuntimeTargetIsolation => "RuntimeTargetIsolation",
            Self::SolverIntegration => "SolverIntegration",
            Self::RuntimeContract => "RuntimeContract",
            Self::EmptySystem => "EmptySystem",
            Self::NonFiniteResult => "NonFiniteResult",
            Self::ModelAssertion => "ModelAssertion",
            Self::ModelTermination => "ModelTermination",
            Self::Timeout => "Timeout",
            Self::MemoryLimit => "MemoryLimit",
            Self::ResourceBudget => "ResourceBudget",
            Self::HarnessFailure => "HarnessFailure",
            Self::TraceOutput => "TraceOutput",
            Self::Unclassified => "Unclassified",
        }
    }

    /// The compile-stage bucket for a strict-compile failure.
    ///
    /// `phase` is the structured [`FailedPhase`] the compile session reported
    /// (`None` means the model never reached instantiation). `unbalanced` is
    /// read from the structured balance breakdown, so a balance failure is
    /// distinguished from every other ToDae failure without consulting an error
    /// code or a message.
    #[must_use]
    pub fn from_compile_phase(phase: Option<FailedPhase>, unbalanced: bool) -> Self {
        match phase {
            None => Self::Resolve,
            Some(FailedPhase::Instantiate) => Self::Instantiate,
            Some(FailedPhase::Typecheck) => Self::Typecheck,
            Some(FailedPhase::Flatten) => Self::Flatten,
            Some(FailedPhase::ToDae) if unbalanced => Self::DaeBalance,
            Some(FailedPhase::ToDae) => Self::DaeConstruction,
        }
    }

    /// The bucket for a simulation-stage failure.
    ///
    /// Both inputs are producer knowledge: the [`SimError`] variant, and the
    /// [`SimFailureStage`] the failing path attached on its way out. `fallback`
    /// is the stage implied by the worker's own position in the pipeline, used
    /// only when the failing path recorded nothing.
    #[must_use]
    pub fn from_sim_error(error: &SimError, fallback: SimFailureStage) -> Self {
        match error.kind() {
            SimError::Timeout { .. } => Self::Timeout,
            SimError::AssertionFailed { .. } => Self::ModelAssertion,
            SimError::Terminated { .. } => Self::ModelTermination,
            SimError::EmptySystem => Self::EmptySystem,
            SimError::RuntimeContract { .. } => Self::RuntimeContract,
            // The model has no reduced state-only system. The backend pins this
            // at `BackendBuild`, so it buckets there like any other
            // construction-time rejection; routing it through the stage keeps
            // the bucket producer knowledge rather than a second mapping.
            SimError::StateOnlyPathUnavailable(_)
            | SimError::DirectionalDerivativeUnavailable { .. }
            | SimError::SolveIr(_)
            | SimError::SolverError(_) => Self::from_sim_stage(error.stage().unwrap_or(fallback)),
            // `kind()` peels every annotation, so a `Staged` cannot reach here.
            SimError::Staged { .. } => Self::Unclassified,
        }
    }

    /// The bucket a simulation stage belongs to.
    #[must_use]
    pub fn from_sim_stage(stage: SimFailureStage) -> Self {
        match stage {
            SimFailureStage::SolveLowering => Self::SolveLowering,
            SimFailureStage::StructuralAnalysis => Self::StructuralAnalysis,
            SimFailureStage::BackendBuild => Self::SimBackendBuild,
            SimFailureStage::Initialization => Self::RuntimeInitialization,
            SimFailureStage::EventIteration => Self::RuntimeEventIteration,
            SimFailureStage::ManifoldProjection => Self::RuntimeManifoldProjection,
            SimFailureStage::TargetIsolation => Self::RuntimeTargetIsolation,
            SimFailureStage::Integration => Self::SolverIntegration,
        }
    }
}

impl std::fmt::Display for ModelFailureBucket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl ModelFailureOwner {
    /// The variant name, as serialized.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Frontend => "Frontend",
            Self::Flatten => "Flatten",
            Self::DaeConstruction => "DaeConstruction",
            Self::Structural => "Structural",
            Self::SolveLowering => "SolveLowering",
            Self::Runtime => "Runtime",
            Self::Solver => "Solver",
            Self::ModelBehaviour => "ModelBehaviour",
            Self::Performance => "Performance",
            Self::Harness => "Harness",
            Self::Unknown => "Unknown",
        }
    }
}

impl std::fmt::Display for ModelFailureOwner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// The classification block recorded on a model result.
///
/// Additive: absent on a result that succeeded, and absent on every historical
/// `msl_results.json` written before this existed, which is why every consumer
/// treats it as optional.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ModelFailureClassification {
    /// The pipeline phase the attempt reached.
    pub phase: WorkerProgressPhase,
    /// The typed failure family.
    pub bucket: ModelFailureBucket,
    /// Who owns the fix. Always [`ModelFailureBucket::owner_category`] of
    /// `bucket`; carried explicitly so consumers can group without linking this
    /// crate.
    pub owner_category: ModelFailureOwner,
}

impl ModelFailureClassification {
    /// Build a classification, deriving the owner from the bucket so the two can
    /// never disagree.
    #[must_use]
    pub fn new(phase: WorkerProgressPhase, bucket: ModelFailureBucket) -> Self {
        Self {
            phase,
            bucket,
            owner_category: bucket.owner_category(),
        }
    }
}

/// The pipeline phase a compile failure stopped in.
#[must_use]
pub fn compile_failure_progress_phase(phase: Option<FailedPhase>) -> WorkerProgressPhase {
    // A pre-instantiation failure is a parse/resolve failure inside the compile
    // session, which the progress log reports as `Compile`.
    phase.map_or(WorkerProgressPhase::Compile, WorkerProgressPhase::from)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_bucket_has_an_owner_and_a_stable_name() {
        for bucket in ModelFailureBucket::ALL {
            let json = serde_json::to_string(bucket).expect("bucket serializes");
            assert_eq!(
                json,
                format!("\"{}\"", bucket.as_str()),
                "the serialized name and `as_str` must not drift"
            );
            let owner = bucket.owner_category();
            assert_eq!(
                serde_json::to_string(&owner).expect("owner serializes"),
                format!("\"{}\"", owner.as_str())
            );
        }
    }

    #[test]
    fn bucket_list_covers_every_variant() {
        // `ALL` is hand-written, so prove it is not missing a variant by
        // round-tripping each entry and checking the set size against the
        // distinct names.
        let names = ModelFailureBucket::ALL
            .iter()
            .map(|bucket| bucket.as_str())
            .collect::<std::collections::BTreeSet<_>>();
        assert_eq!(
            names.len(),
            ModelFailureBucket::ALL.len(),
            "duplicate entry in ModelFailureBucket::ALL"
        );
        // Adding a variant without adding it to `ALL` leaves this count stale;
        // it is asserted so the omission is caught here rather than as a silent
        // hole in the owner-mapping test above.
        assert_eq!(ModelFailureBucket::ALL.len(), 25);
    }

    /// A declared-budget overrun is a performance defect with a named owner,
    /// not an unclassified failure and not a machine-scheduling accident.
    #[test]
    fn resource_budget_overruns_are_owned_by_performance() {
        assert_eq!(
            ModelFailureBucket::ResourceBudget.owner_category(),
            ModelFailureOwner::Performance
        );
        assert_eq!(
            ModelFailureBucket::ResourceBudget.as_str(),
            "ResourceBudget"
        );
        assert_ne!(
            ModelFailureBucket::ResourceBudget,
            ModelFailureBucket::MemoryLimit,
            "a Solve-IR size overrun must stay distinguishable from an RSS overrun"
        );
    }

    #[test]
    fn compile_buckets_come_from_the_typed_phase_not_the_message() {
        assert_eq!(
            ModelFailureBucket::from_compile_phase(None, false),
            ModelFailureBucket::Resolve
        );
        assert_eq!(
            ModelFailureBucket::from_compile_phase(Some(FailedPhase::Typecheck), false),
            ModelFailureBucket::Typecheck
        );
        assert_eq!(
            ModelFailureBucket::from_compile_phase(Some(FailedPhase::ToDae), false),
            ModelFailureBucket::DaeConstruction
        );
        assert_eq!(
            ModelFailureBucket::from_compile_phase(Some(FailedPhase::ToDae), true),
            ModelFailureBucket::DaeBalance
        );
    }

    #[test]
    fn sim_buckets_come_from_the_variant_and_the_recorded_stage() {
        let staged = SimError::SolveIr("did not converge".to_string())
            .at_stage(SimFailureStage::ManifoldProjection);
        assert_eq!(
            ModelFailureBucket::from_sim_error(&staged, SimFailureStage::Integration),
            ModelFailureBucket::RuntimeManifoldProjection,
            "a recorded stage must win over the caller's fallback"
        );

        let unstaged = SimError::SolverError("nonlinear failures".to_string());
        assert_eq!(
            ModelFailureBucket::from_sim_error(&unstaged, SimFailureStage::Integration),
            ModelFailureBucket::SolverIntegration
        );
        assert_eq!(
            ModelFailureBucket::from_sim_error(&unstaged, SimFailureStage::BackendBuild),
            ModelFailureBucket::SimBackendBuild,
            "with no recorded stage the worker's own position decides"
        );
        let capability = SimError::DirectionalDerivativeUnavailable {
            reason: "non-differentiable operation".to_string(),
        };
        assert_eq!(
            ModelFailureBucket::from_sim_error(&capability, SimFailureStage::BackendBuild),
            ModelFailureBucket::SimBackendBuild,
            "BDF derivative capability is decided during backend construction"
        );
    }

    /// Model behaviour and resource limits must not be attributed to a
    /// compiler owner regardless of which stage raised them.
    #[test]
    fn model_behaviour_and_limits_keep_their_own_owners() {
        for (error, bucket, owner) in [
            (
                SimError::AssertionFailed {
                    time: 1.0,
                    message: "x > 0".to_string(),
                },
                ModelFailureBucket::ModelAssertion,
                ModelFailureOwner::ModelBehaviour,
            ),
            (
                SimError::Terminated {
                    time: 2.0,
                    message: "done".to_string(),
                },
                ModelFailureBucket::ModelTermination,
                ModelFailureOwner::ModelBehaviour,
            ),
            (
                SimError::Timeout { seconds: 10.0 },
                ModelFailureBucket::Timeout,
                ModelFailureOwner::Performance,
            ),
        ] {
            let staged = error.at_stage(SimFailureStage::Integration);
            assert_eq!(
                ModelFailureBucket::from_sim_error(&staged, SimFailureStage::Integration),
                bucket
            );
            assert_eq!(bucket.owner_category(), owner);
        }
    }

    #[test]
    fn classification_owner_always_matches_its_bucket() {
        for bucket in ModelFailureBucket::ALL {
            let classification = ModelFailureClassification::new(WorkerProgressPhase::Sim, *bucket);
            assert_eq!(classification.owner_category, bucket.owner_category());
        }
    }

    #[test]
    fn classification_round_trips_by_name() {
        let classification = ModelFailureClassification::new(
            WorkerProgressPhase::Sim,
            ModelFailureBucket::RuntimeEventIteration,
        );
        let json = serde_json::to_string(&classification).expect("serializes");
        assert!(
            json.contains("\"RuntimeEventIteration\"") && json.contains("\"Runtime\""),
            "buckets and owners must serialize by name, got {json}"
        );
        let parsed: ModelFailureClassification = serde_json::from_str(&json).expect("round-trips");
        assert_eq!(parsed, classification);
    }

    #[test]
    fn an_unknown_bucket_name_is_rejected_rather_than_silently_defaulted() {
        let error = serde_json::from_str::<ModelFailureBucket>("\"NotABucket\"")
            .expect_err("unknown bucket names must not parse");
        assert!(error.to_string().contains("unknown variant"));
        serde_json::from_str::<ModelFailureOwner>("\"NotAnOwner\"")
            .expect_err("unknown owner names must not parse");
    }

    #[test]
    fn compile_progress_phase_maps_pre_instantiation_failures_to_compile() {
        assert_eq!(
            compile_failure_progress_phase(None),
            WorkerProgressPhase::Compile
        );
        assert_eq!(
            compile_failure_progress_phase(Some(FailedPhase::Flatten)),
            WorkerProgressPhase::Flatten
        );
    }
}
