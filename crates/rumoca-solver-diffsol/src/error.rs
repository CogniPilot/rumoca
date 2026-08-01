use rumoca_solver::RuntimeSolveError;

/// The simulation sub-stage that raised a failure.
///
/// This is *producer knowledge*: the code path that fails attaches the stage it
/// was running, so downstream classification never has to re-derive a sub-bucket
/// by pattern-matching a rendered message. The MSL harness records the mapped
/// bucket in `msl_results.json`, which is why the split has to be minted here
/// rather than reconstructed from text later.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SimFailureStage {
    /// Lowering the DAE to Solve IR (a `rumoca-phase-solve` defect surfaced by
    /// the simulation entry point).
    SolveLowering,
    /// Structural analysis of the lowered system (index reduction, matching,
    /// tearing) rejected the model.
    StructuralAnalysis,
    /// Constructing the backend problem, before any integration happens.
    BackendBuild,
    /// Settling initial conditions: the initial event/`pre` chain, the initial
    /// variable projection, and homotopy continuation.
    Initialization,
    /// Event iteration / discrete re-initialization at an event instant.
    EventIteration,
    /// Index-reduction manifold (algebraic constraint) projection.
    ManifoldProjection,
    /// Isolating or advancing the solver onto a requested output/stop time.
    TargetIsolation,
    /// A time-integration step taken by the numeric solver.
    Integration,
}

impl std::fmt::Display for SimFailureStage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Self::SolveLowering => "SolveLowering",
            Self::StructuralAnalysis => "StructuralAnalysis",
            Self::BackendBuild => "BackendBuild",
            Self::Initialization => "Initialization",
            Self::EventIteration => "EventIteration",
            Self::ManifoldProjection => "ManifoldProjection",
            Self::TargetIsolation => "TargetIsolation",
            Self::Integration => "Integration",
        };
        f.write_str(name)
    }
}

/// Why a state-carrying model has no reduced state-only system to integrate.
///
/// SPEC 0008 acceptance contract, stated positively first: the reduced
/// state-only path *accepts* a model whose state-derivative rows read only
/// continuous states, or whose non-state reads are — transitively — all
/// produced by the algebraic projection plan. Every other shape is rejected
/// here, naming both the construct that failed and the solver coordinate that
/// failed it.
///
/// The rejection is terminal by design. Until SPEC 0038 this predicate returned
/// a plain `false` and the backend quietly built a second, general/implicit DAE
/// system instead. That fallback made an unprojectable
/// coordinate — which is a `rumoca-phase-solve` defect — look like a solver
/// preference, and it would have absorbed a lowering regression by switching
/// integrators with no diagnostic at all.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum StateOnlyRejection {
    /// The continuous system does not present one derivative row per state, so
    /// there is no `y' = f(y, t)` to reduce to in the first place.
    #[error(
        "the model declares {state_count} continuous state(s) but lowers to {derivative_rows} \
         state-derivative row(s), so no reduced state-only ODE exists"
    )]
    DerivativeRowCount {
        derivative_rows: usize,
        state_count: usize,
    },

    /// A state derivative reads a non-state solver coordinate that no algebraic
    /// projection block produces, so a reduced step cannot recover its value.
    #[error(
        "der({state_name}) reads solver coordinate '{name}' (y[{y_index}]), which no algebraic \
         projection block produces, so a reduced state-only step cannot recover it"
    )]
    UnproducedDerivativeDependency {
        y_index: usize,
        name: String,
        state_name: String,
    },

    /// The projection plan names a producing residual row that carries no
    /// evaluable scalar program.
    #[error(
        "solver coordinate '{name}' (y[{y_index}]) is projected from implicit residual output \
         {output_index}, which carries no evaluable scalar program"
    )]
    ProducerWithoutScalarProgram {
        y_index: usize,
        name: String,
        output_index: usize,
    },

    /// The projection plan indexes a residual row the implicit residual does
    /// not have.
    #[error(
        "the algebraic projection plan references implicit residual row {row}, but the implicit \
         residual emits only {implicit_output_count} output(s)"
    )]
    ProjectionRowOutOfRange {
        row: usize,
        implicit_output_count: usize,
    },

    /// Two projection rows claim the same coordinate, so its producer — and
    /// therefore the reduced system — is not well defined.
    #[error(
        "solver coordinate '{name}' (y[{y_index}]) is claimed by two algebraic projection rows \
         ({first_row} and {second_row}), so its producer is ambiguous"
    )]
    ConflictingProjectionProducers {
        y_index: usize,
        name: String,
        first_row: usize,
        second_row: usize,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum SimError {
    #[error("empty system: no equations to simulate")]
    EmptySystem,

    /// The model does not satisfy the state-only integration contract.
    ///
    /// This is the one place the retirement is user-visible: the general /
    /// implicit DAE path that used to absorb these models is gone, so the
    /// backend reports which coordinate has no projection producer instead of
    /// silently integrating a different system.
    #[error(
        "{0}; the general/implicit DAE fallback that used to absorb this was retired \
         (SPEC 0038), so this is a hard error rather than a silent switch \
         to a different integrator"
    )]
    StateOnlyPathUnavailable(StateOnlyRejection),

    #[error("solver error: {0}")]
    SolverError(String),

    #[error("solve-IR evaluation failed: {0}")]
    SolveIr(String),

    #[error("diffsol runtime contract violation: {reason}")]
    RuntimeContract { reason: String },

    #[error("Modelica assert failed at t={time:.9}: {message}")]
    AssertionFailed { time: f64, message: String },

    #[error("Modelica terminate requested at t={time:.9}: {message}")]
    Terminated { time: f64, message: String },

    #[error("timeout after {seconds:.3}s")]
    Timeout { seconds: f64 },

    /// A failure annotated with the stage that raised it.
    ///
    /// The rendered form is exactly the inner failure's, so annotating a path
    /// never changes any user-visible message or recorded `sim_error` text; the
    /// stage is carried alongside for machine consumers.
    #[error("{inner}")]
    Staged {
        stage: SimFailureStage,
        inner: Box<SimError>,
    },
}

impl SimError {
    pub fn source_span(&self) -> Option<()> {
        None
    }

    /// The failure itself, with every stage annotation peeled off.
    ///
    /// Consumers that switch on the failure *variant* must go through this so an
    /// annotated path keeps behaving exactly like the unannotated one.
    #[must_use]
    pub fn kind(&self) -> &SimError {
        match self {
            Self::Staged { inner, .. } => inner.kind(),
            other => other,
        }
    }

    /// The stage the raising path recorded, if any.
    #[must_use]
    pub fn stage(&self) -> Option<SimFailureStage> {
        match self {
            Self::Staged { stage, .. } => Some(*stage),
            _ => None,
        }
    }

    /// Annotate with the stage that raised this failure.
    ///
    /// The innermost annotation wins: an outer boundary sees failures from every
    /// stage nested under it, and relabeling them with its own coarser stage
    /// would destroy the precision this type exists to carry.
    ///
    /// [`Self::Terminated`] is never annotated. A Modelica `terminate()` is a
    /// *successful* early stop that the run path matches on by value to replay
    /// its semantics, not a failure with a stage; wrapping it would silently
    /// turn a completed simulation into a reported error.
    #[must_use]
    pub fn at_stage(self, stage: SimFailureStage) -> Self {
        match self {
            already @ (Self::Staged { .. } | Self::Terminated { .. }) => already,
            other => Self::Staged {
                stage,
                inner: Box::new(other),
            },
        }
    }
}

impl From<RuntimeSolveError> for SimError {
    fn from(value: RuntimeSolveError) -> Self {
        match value {
            RuntimeSolveError::SolveIr { message, span } => {
                let message = match span {
                    Some(span) => format!("{message} @ {span:?}"),
                    None => message,
                };
                Self::SolveIr(message)
            }
            RuntimeSolveError::UnsupportedModel { reason } => Self::SolveIr(reason),
            unassignable @ RuntimeSolveError::RefreshTargetUnassignable { .. } => {
                Self::SolveIr(unassignable.to_string())
            }
            RuntimeSolveError::NonFiniteDerivative { state_name } => Self::SolveIr(format!(
                "non-finite derivative evaluation for state '{state_name}'"
            )),
            non_finite @ RuntimeSolveError::NonFiniteValue { .. } => {
                Self::SolveIr(non_finite.to_string())
            }
        }
    }
}

impl From<rumoca_eval_solve::EvalSolveError> for SimError {
    fn from(value: rumoca_eval_solve::EvalSolveError) -> Self {
        Self::SolveIr(value.to_string())
    }
}

impl From<rumoca_eval_solve::ScalarizeError> for SimError {
    fn from(value: rumoca_eval_solve::ScalarizeError) -> Self {
        Self::SolveIr(value.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn staged_annotation_preserves_the_rendered_message() {
        let raw = SimError::SolveIr("projection did not converge".to_string());
        let rendered = raw.to_string();
        let staged = raw.at_stage(SimFailureStage::ManifoldProjection);
        assert_eq!(
            staged.to_string(),
            rendered,
            "annotating must not perturb recorded sim_error text"
        );
    }

    #[test]
    fn kind_peels_annotations_so_variant_matching_is_unchanged() {
        let staged = SimError::Timeout { seconds: 10.0 }
            .at_stage(SimFailureStage::Integration)
            .at_stage(SimFailureStage::TargetIsolation);
        assert!(matches!(staged.kind(), SimError::Timeout { .. }));
    }

    #[test]
    fn innermost_stage_wins_over_a_coarser_outer_boundary() {
        let staged = SimError::SolveIr("event update".to_string())
            .at_stage(SimFailureStage::EventIteration)
            .at_stage(SimFailureStage::Integration);
        assert_eq!(staged.stage(), Some(SimFailureStage::EventIteration));
    }

    /// `terminate()` is a normal end of simulation that the run path matches on
    /// by value. Annotating it would make a completed run look like a failure.
    #[test]
    fn a_terminate_outcome_is_never_annotated() {
        let terminated = SimError::Terminated {
            time: 0.25,
            message: "threshold reached".to_string(),
        }
        .at_stage(SimFailureStage::Integration);
        assert!(matches!(terminated, SimError::Terminated { .. }));
        assert_eq!(terminated.stage(), None);
    }

    #[test]
    fn an_unannotated_failure_reports_no_stage() {
        assert_eq!(SimError::EmptySystem.stage(), None);
        assert!(matches!(
            SimError::EmptySystem.kind(),
            SimError::EmptySystem
        ));
    }
}
