use rumoca_solver::{SimBackend, SimOptions, SimResult, fmi_me::MeModelArtifact};

use crate::{SimError, check_initialization, run_prepared_simulation};

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
    StateOnly,
}

impl PreparedSimulation {
    pub fn backend(&self) -> SimBackend {
        SimBackend::Diffsol
    }

    pub fn run(&self) -> Result<SimResult, SimError> {
        run_prepared_simulation(self)
    }

    pub fn check_initialization(&self) -> Result<(), SimError> {
        check_initialization(self.model.clone(), &self.opts)
    }
}
