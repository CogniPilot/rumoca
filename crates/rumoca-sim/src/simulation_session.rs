use indexmap::IndexMap;
use rumoca_ir_dae as dae;

#[cfg(feature = "scheduled-sim")]
use crate::SimulationSessionApi;
use crate::{SimSolverMode, SimulationDiagnosticError};

#[derive(Debug, Clone)]
pub struct SessionState {
    pub time: f64,
    pub values: IndexMap<String, f64>,
}

pub struct SimulationSession {
    inner: SimulationSessionInner,
}

enum SimulationSessionInner {
    #[cfg(feature = "solver-diffsol")]
    Diffsol(Box<crate::diffsol::SimulationSession>),
    #[cfg(feature = "solver-rk45")]
    RkLike(Box<crate::rk45::SimulationSession>),
}

impl SimulationSession {
    #[cfg(all(test, feature = "solver-diffsol", feature = "scheduled-sim"))]
    pub(crate) fn verification_from_diffsol(session: crate::diffsol::SimulationSession) -> Self {
        Self {
            inner: SimulationSessionInner::Diffsol(Box::new(session)),
        }
    }

    #[cfg(all(test, feature = "solver-rk45", feature = "scheduled-sim"))]
    pub(crate) fn verification_from_rk_like(session: crate::rk45::SimulationSession) -> Self {
        Self {
            inner: SimulationSessionInner::RkLike(Box::new(session)),
        }
    }

    pub fn new(
        dae_model: &dae::Dae,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        match opts.solver_mode {
            SimSolverMode::Auto => new_auto_session(dae_model, opts),
            SimSolverMode::Bdf => new_bdf_session(dae_model, opts),
            SimSolverMode::RkLike => new_rk_like_session(dae_model, opts),
        }
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .set_input(name, value)
                .map_err(SimulationDiagnosticError::from),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .set_input(name, value)
                .map_err(SimulationDiagnosticError::from),
        }
    }

    pub fn reset(&mut self) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => {
                session.reset().map_err(SimulationDiagnosticError::from)
            }
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => {
                session.reset().map_err(SimulationDiagnosticError::from)
            }
        }
    }

    /// Explicitly replay the pristine state at a new start coordinate.
    pub fn retime(&mut self, t_start: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .retime(t_start)
                .map_err(SimulationDiagnosticError::from),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .retime(t_start)
                .map_err(SimulationDiagnosticError::from),
        }
    }

    pub fn advance_to(&mut self, target_time: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .advance_to(target_time)
                .map_err(SimulationDiagnosticError::from),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .advance_to(target_time)
                .map_err(SimulationDiagnosticError::from),
        }
    }

    pub fn step(&mut self, dt: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => {
                session.step(dt).map_err(SimulationDiagnosticError::from)
            }
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => {
                session.step(dt).map_err(SimulationDiagnosticError::from)
            }
        }
    }

    pub fn time(&self) -> f64 {
        match &self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.time(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.time(),
        }
    }

    pub fn get(&self, name: &str) -> Result<Option<f64>, SimulationDiagnosticError> {
        match &self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => {
                session.get(name).map_err(SimulationDiagnosticError::from)
            }
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => {
                session.get(name).map_err(SimulationDiagnosticError::from)
            }
        }
    }

    pub fn state(&self) -> Result<SessionState, SimulationDiagnosticError> {
        match &self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => {
                let state = session.state().map_err(SimulationDiagnosticError::from)?;
                Ok(SessionState {
                    time: state.time,
                    values: state.values,
                })
            }
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => {
                let state = session.state().map_err(SimulationDiagnosticError::from)?;
                Ok(SessionState {
                    time: state.time,
                    values: state.values,
                })
            }
        }
    }

    pub fn input_names(&self) -> &[String] {
        match &self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.input_names(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.input_names(),
        }
    }

    pub fn variable_names(&self) -> &[String] {
        match &self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.variable_names(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.variable_names(),
        }
    }
}

#[cfg(feature = "scheduled-sim")]
impl SimulationSessionApi for SimulationSession {
    type Error = SimulationDiagnosticError;

    fn retime(&mut self, t_start: f64) -> Result<(), Self::Error> {
        Self::retime(self, t_start)
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), Self::Error> {
        Self::set_input(self, name, value)
    }

    fn advance_to(&mut self, target_time: f64) -> Result<(), Self::Error> {
        Self::advance_to(self, target_time)
    }

    fn time(&self) -> f64 {
        Self::time(self)
    }

    fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, Self::Error> {
        match &self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .values_for(names)
                .map_err(SimulationDiagnosticError::from),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .values_for(names)
                .map_err(SimulationDiagnosticError::from),
        }
    }

    fn max_schedule_advance_dt(&self) -> Option<f64> {
        match &self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.max_schedule_advance_dt(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(_) => None,
        }
    }
}

/// Lower the DAE and apply simulation overrides exactly once before handing the
/// solve model to the selected simulation solver backend.
fn lower_for_simulation_session(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<
    (
        rumoca_solver::fmi_me::MeModelArtifact,
        Option<rumoca_solver::fmi_me::MeExecutionBackend>,
    ),
    SimulationDiagnosticError,
> {
    crate::solve_lowering::lower_runtime_fmi_artifact(dae_model, opts)
}

fn new_auto_session(
    dae_model: &dae::Dae,
    opts: rumoca_solver::SimOptions,
) -> Result<SimulationSession, SimulationDiagnosticError> {
    let (artifact, execution_backend) = lower_for_simulation_session(dae_model, &opts)?;
    #[cfg(all(feature = "solver-diffsol", feature = "solver-rk45"))]
    {
        match crate::prepared_simulation::select_retained_component(
            artifact,
            &opts,
            execution_backend,
        )
        .map_err(SimulationDiagnosticError::from)?
        {
            crate::prepared_simulation::SelectedRetainedComponent::Bdf(retained) => {
                crate::diffsol::SimulationSession::from_retained(retained, opts).map(|session| {
                    SimulationSession {
                        inner: SimulationSessionInner::Diffsol(Box::new(session)),
                    }
                })
            }
            crate::prepared_simulation::SelectedRetainedComponent::RkLike(retained) => {
                crate::rk45::SimulationSession::from_selected_retained(retained, opts).map(
                    |session| SimulationSession {
                        inner: SimulationSessionInner::RkLike(Box::new(session)),
                    },
                )
            }
        }
    }
    #[cfg(all(feature = "solver-diffsol", not(feature = "solver-rk45")))]
    {
        crate::diffsol::SimulationSession::from_artifact(artifact, opts, execution_backend).map(
            |session| SimulationSession {
                inner: SimulationSessionInner::Diffsol(Box::new(session)),
            },
        )
    }
    #[cfg(all(not(feature = "solver-diffsol"), feature = "solver-rk45"))]
    {
        crate::rk45::SimulationSession::from_selected_artifact(artifact, opts, execution_backend)
            .map(|session| SimulationSession {
                inner: SimulationSessionInner::RkLike(Box::new(session)),
            })
    }
    #[cfg(not(any(feature = "solver-diffsol", feature = "solver-rk45")))]
    {
        let _ = (artifact, execution_backend, opts);
        Err(SimulationDiagnosticError::Solver(
            "no simulation solver backend is enabled".to_string(),
        ))
    }
}

fn new_bdf_session(
    dae_model: &dae::Dae,
    opts: rumoca_solver::SimOptions,
) -> Result<SimulationSession, SimulationDiagnosticError> {
    #[cfg(feature = "solver-diffsol")]
    {
        let (artifact, execution_backend) = lower_for_simulation_session(dae_model, &opts)?;
        crate::diffsol::SimulationSession::from_artifact(artifact, opts, execution_backend).map(
            |session| SimulationSession {
                inner: SimulationSessionInner::Diffsol(Box::new(session)),
            },
        )
    }
    #[cfg(not(feature = "solver-diffsol"))]
    {
        let _ = (dae_model, opts);
        Err(SimulationDiagnosticError::Solver(
            "bdf solver requested, but this build does not include the diffsol backend".to_string(),
        ))
    }
}

fn new_rk_like_session(
    dae_model: &dae::Dae,
    opts: rumoca_solver::SimOptions,
) -> Result<SimulationSession, SimulationDiagnosticError> {
    let (artifact, execution_backend) = lower_for_simulation_session(dae_model, &opts)?;
    #[cfg(feature = "solver-rk45")]
    {
        crate::rk45::SimulationSession::from_selected_artifact(artifact, opts, execution_backend)
            .map(|session| SimulationSession {
                inner: SimulationSessionInner::RkLike(Box::new(session)),
            })
    }
    #[cfg(not(feature = "solver-rk45"))]
    {
        let _ = (artifact, execution_backend, opts);
        Err(SimulationDiagnosticError::Solver(
            "rk-like solver requested, but this build does not include the rk45 backend"
                .to_string(),
        ))
    }
}
