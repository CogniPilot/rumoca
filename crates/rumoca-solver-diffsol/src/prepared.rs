use std::sync::Arc;

use rumoca_eval_solve::SolveRuntime;
use rumoca_ir_solve as solve;
use rumoca_solver::{SimBackend, SimOptions, SimResult};

use crate::{OdeModel, SimError, check_initialization, run_prepared_simulation};

pub struct PreparedSimulation {
    pub(crate) model: solve::SolveModel,
    pub(crate) opts: SimOptions,
    pub(crate) state: PreparedSimulationState,
}

pub(crate) enum PreparedSimulationState {
    NoState,
    StateOnly {
        equilibrium_model: Arc<OdeModel>,
        runtime: Arc<SolveRuntime>,
    },
    General {
        equilibrium_model: Arc<OdeModel>,
        runtime: Arc<SolveRuntime>,
    },
}

impl PreparedSimulation {
    pub fn backend(&self) -> SimBackend {
        SimBackend::Diffsol
    }

    pub fn run(&self) -> Result<SimResult, SimError> {
        run_prepared_simulation(self)
    }

    pub fn check_initialization(&self) -> Result<(), SimError> {
        check_initialization(&self.model, &self.opts)
    }

    pub fn model(&self) -> &solve::SolveModel {
        &self.model
    }
}
