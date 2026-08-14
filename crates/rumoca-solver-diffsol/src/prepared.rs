use rumoca_solver::{
    SimBackend, SimOptions, SimResult,
    fmi_me::{MeFmuState, MeModelArtifact},
};

use crate::{SimError, SimFailureStage, me::DiffsolMeHost, run_prepared_simulation};

pub struct PreparedSimulation {
    pub(crate) model: MeModelArtifact,
    pub(crate) opts: SimOptions,
    pub(crate) state: PreparedSimulationState,
}

/// Which system a prepared simulation integrates.
///
/// A state-carrying model is always the reduced state-only ODE. The `General`
/// (full-solver-vector implicit DAE) variant was retired in SPEC 0038: no model
/// in the 566-model MSL cohort ever constructed it, and as
/// a silent fallback it would have absorbed a `rumoca-phase-solve` regression
/// by switching integrators without a diagnostic. Models that do not satisfy
/// the reduced-system contract are now rejected by name at build time — see
/// [`crate::StateOnlyRejection`].
pub(crate) enum PreparedSimulationState {
    NoState,
    StateOnly(Box<PreparedComponent>),
}

/// The retained, reusable ME component of a prepared state-only simulation.
pub(crate) struct PreparedComponent {
    /// The ONE instantiated ME component for this prepared simulation.
    ///
    /// Its runtime owns every compiled execution artifact issued for this
    /// exact model instance — the eager compiles from instantiation plus
    /// the lazily compiled owners warmed by the build-time preflight — so
    /// hot `run()` iterations reuse them instead of re-instantiating a
    /// fresh component and recompiling per run.
    host: DiffsolMeHost,
    /// Pristine post-instantiate component state (`fmi3GetFMUState`).
    ///
    /// Each `run()`/`check_initialization()` rewinds the component to this
    /// snapshot (`fmi3Reset` + `fmi3SetFMUState`) before evaluating. The
    /// snapshot is brand-checked by the kernel against the exact instance
    /// that minted it, and it restores ALL mutable evaluation state —
    /// lifecycle, time, states, parameters, discrete/relation memory,
    /// event and stop-schedule bookkeeping, delay buffers, and evaluator
    /// caches — while compiled callables (Rc-held, absent from the
    /// snapshot by design) persist across runs.
    pristine: MeFmuState,
}

impl PreparedComponent {
    pub(crate) fn new(host: DiffsolMeHost, pristine: MeFmuState) -> Self {
        Self { host, pristine }
    }
}

impl PreparedSimulation {
    pub fn backend(&self) -> SimBackend {
        SimBackend::Diffsol
    }

    pub fn run(&self) -> Result<SimResult, SimError> {
        run_prepared_simulation(self)
    }

    pub fn check_initialization(&self) -> Result<(), SimError> {
        match self.state.fresh_run_component()? {
            // The zero-state path instantiates no integrator component; it
            // settles through the (backend-free) no-state session exactly as
            // the one-shot entry does.
            None => crate::check_initialization(self.model.clone(), &self.opts),
            Some(host) => crate::me_bdf::check_initialization_with_host(host, &self.opts)
                .map_err(|error| error.at_stage(SimFailureStage::Initialization)),
        }
    }
}

impl PreparedSimulationState {
    /// The explicit fresh-run contract for the retained ME component
    /// (SPEC_0038 §Internal Solver Boundary; SPEC_0041 §4 hot-run reuse).
    ///
    /// SHARED across runs — same instance, root-scoped, sound by
    /// construction: the Solve model and its prepared plans/program blocks;
    /// every compiled callable (Rc-held); the lazily compiled program caches
    /// (exact-assignment schedules, discrete/guarded/root/visible/action
    /// specializations, failed-row memos) — each is memoized per issued owner
    /// WITHIN this exact model instance and warmed by the build preflight, so
    /// no cross-root reuse is possible; bit-validated numeric caches
    /// (parameter-static gradients, Newton factor caches) and scratch
    /// buffers, which self-validate or are overwritten before use.
    ///
    /// FRESH per run — restored by the brand-checked pristine rewind
    /// (`fmi3Reset` + `fmi3SetFMUState`; the kernel rejects a snapshot from
    /// any other instance): lifecycle, time, continuous states, the parameter
    /// vector (which carries discrete slots and relation memory), stop
    /// schedule and pending/last event bookkeeping, pre-event vectors and
    /// frozen-event seeds, solver-y guess and value caches
    /// (derivative/root/linearization), delay buffers and delay scratch, the
    /// static refresh cache, host callback/event bookkeeping, and the impure
    /// random streams — the evaluator snapshot copies stream CONTENTS by
    /// value (`SimulationRuntimeState::snapshot/restore`), so each run
    /// restarts from the pristine pre-preflight streams, i.e. the declared
    /// initial seed policy, never a shared mutable handle.
    pub(crate) fn fresh_run_component(&self) -> Result<Option<&DiffsolMeHost>, SimError> {
        match self {
            Self::NoState => Ok(None),
            Self::StateOnly(component) => {
                component.host.reset_to_fmu_state(&component.pristine)?;
                Ok(Some(&component.host))
            }
        }
    }
}
