use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

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
    Discrete(Box<crate::discrete_stepper::SimStepper>),
    #[cfg(feature = "solver-diffsol")]
    Diffsol(Box<crate::diffsol::SimulationSession>),
    #[cfg(feature = "solver-rk45")]
    RkLike(Box<crate::rk45::SimulationSession>),
}

impl SimulationSession {
    pub fn new(
        dae_model: &dae::Dae,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        Self::new_with_diagnostics(dae_model, opts)
    }

    pub fn new_with_diagnostics(
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
            SimulationSessionInner::Discrete(stepper) => stepper.set_input(name, value),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .set_input(name, value)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .set_input(name, value)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }

    pub fn reset(&mut self, t_start: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            SimulationSessionInner::Discrete(stepper) => stepper.reset(t_start),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .reset(t_start)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .reset(t_start)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }

    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimulationDiagnosticError> {
        for (name, value) in inputs {
            self.set_input(name, *value)?;
        }
        Ok(())
    }

    pub fn advance_to(&mut self, target_time: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            SimulationSessionInner::Discrete(stepper) => stepper.step(target_time - stepper.time()),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .advance_to(target_time)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .advance_to(target_time)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }

    pub fn step(&mut self, dt: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            SimulationSessionInner::Discrete(stepper) => stepper.step(dt),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .step(dt)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .step(dt)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }

    pub fn time(&self) -> f64 {
        match &self.inner {
            SimulationSessionInner::Discrete(stepper) => stepper.time(),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.time(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.time(),
        }
    }

    pub fn get(&self, name: &str) -> Result<Option<f64>, SimulationDiagnosticError> {
        match &self.inner {
            SimulationSessionInner::Discrete(stepper) => stepper.get(name),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .get(name)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .get(name)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }

    pub fn state(&self) -> Result<SessionState, SimulationDiagnosticError> {
        match &self.inner {
            SimulationSessionInner::Discrete(stepper) => {
                let state = stepper.state()?;
                Ok(SessionState {
                    time: state.time,
                    values: state.values,
                })
            }
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => {
                let state = session
                    .state()
                    .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
                Ok(SessionState {
                    time: state.time,
                    values: state.values,
                })
            }
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => {
                let state = session
                    .state()
                    .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
                Ok(SessionState {
                    time: state.time,
                    values: state.values,
                })
            }
        }
    }

    pub fn input_names(&self) -> &[String] {
        match &self.inner {
            SimulationSessionInner::Discrete(stepper) => stepper.input_names(),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.input_names(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.input_names(),
        }
    }

    pub fn variable_names(&self) -> &[String] {
        match &self.inner {
            SimulationSessionInner::Discrete(stepper) => stepper.variable_names(),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.variable_names(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.variable_names(),
        }
    }

    pub fn values_for(
        &self,
        names: &[String],
    ) -> Result<Option<IndexMap<String, f64>>, SimulationDiagnosticError> {
        match &self.inner {
            SimulationSessionInner::Discrete(stepper) => stepper.values_for(names).map(Some),
            #[cfg(all(feature = "solver-diffsol", feature = "scheduled-sim"))]
            SimulationSessionInner::Diffsol(session) => session
                .values_for(names)
                .map(Some)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(all(feature = "solver-diffsol", not(feature = "scheduled-sim")))]
            SimulationSessionInner::Diffsol(session) => {
                let state = session
                    .state()
                    .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
                Ok(Some(
                    names
                        .iter()
                        .filter_map(|name| {
                            state.values.get(name).map(|value| (name.clone(), *value))
                        })
                        .collect(),
                ))
            }
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .values_for(names)
                .map(Some)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }
}

#[cfg(feature = "scheduled-sim")]
impl SimulationSessionApi for SimulationSession {
    type Error = SimulationDiagnosticError;

    fn reset(&mut self, t_start: f64) -> Result<(), Self::Error> {
        Self::reset(self, t_start)
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

    fn get(&self, name: &str) -> Result<Option<f64>, Self::Error> {
        Self::get(self, name)
    }

    fn values_for(&self, names: &[String]) -> Result<Option<IndexMap<String, f64>>, Self::Error> {
        Self::values_for(self, names)
    }

    fn max_schedule_advance_dt(&self) -> Option<f64> {
        match &self.inner {
            SimulationSessionInner::Discrete(_) => None,
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
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    crate::solve_lowering::lower_for_simulation_with_overrides(dae_model, opts)
}

fn discrete_session_from_solve_model(
    solve_model: solve::SolveModel,
) -> Result<SimulationSession, SimulationDiagnosticError> {
    let stepper = crate::discrete_stepper::SimStepper::from_solve_model(solve_model)?;
    Ok(SimulationSession {
        inner: SimulationSessionInner::Discrete(Box::new(stepper)),
    })
}

fn new_auto_session(
    dae_model: &dae::Dae,
    opts: rumoca_solver::SimOptions,
) -> Result<SimulationSession, SimulationDiagnosticError> {
    let solve_model = lower_for_simulation_session(dae_model, &opts)?;
    if solve_model.state_scalar_count() == 0 {
        return discrete_session_from_solve_model(solve_model);
    }
    #[cfg(feature = "solver-diffsol")]
    {
        crate::diffsol::SimulationSession::from_solve_model(solve_model, opts).map(|session| {
            SimulationSession {
                inner: SimulationSessionInner::Diffsol(Box::new(session)),
            }
        })
    }
    #[cfg(all(not(feature = "solver-diffsol"), feature = "solver-rk45"))]
    {
        crate::rk45::SimulationSession::from_solve_model(solve_model, opts).map(|session| {
            SimulationSession {
                inner: SimulationSessionInner::RkLike(Box::new(session)),
            }
        })
    }
    #[cfg(not(any(feature = "solver-diffsol", feature = "solver-rk45")))]
    {
        let _ = (solve_model, opts);
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
        let solve_model = lower_for_simulation_session(dae_model, &opts)?;
        crate::diffsol::SimulationSession::from_solve_model(solve_model, opts).map(|session| {
            SimulationSession {
                inner: SimulationSessionInner::Diffsol(Box::new(session)),
            }
        })
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
    let solve_model = lower_for_simulation_session(dae_model, &opts)?;
    if solve_model.state_scalar_count() == 0 {
        return discrete_session_from_solve_model(solve_model);
    }
    #[cfg(feature = "solver-rk45")]
    {
        crate::rk45::SimulationSession::from_solve_model(solve_model, opts).map(|session| {
            SimulationSession {
                inner: SimulationSessionInner::RkLike(Box::new(session)),
            }
        })
    }
    #[cfg(not(feature = "solver-rk45"))]
    {
        let _ = (solve_model, opts);
        Err(SimulationDiagnosticError::Solver(
            "rk-like solver requested, but this build does not include the rk45 backend"
                .to_string(),
        ))
    }
}
