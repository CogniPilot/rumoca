use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::{InteractiveStepper, SimOptions, SimSolverMode, SimulationDiagnosticError};

#[derive(Debug, Clone)]
pub struct StepperState {
    pub time: f64,
    pub values: IndexMap<String, f64>,
}

pub struct SimStepper {
    inner: SimStepperInner,
}

enum SimStepperInner {
    Discrete(Box<crate::discrete_stepper::SimStepper>),
    #[cfg(feature = "solver-diffsol")]
    Bdf(Box<crate::diffsol::SimStepper>),
    #[cfg(feature = "solver-rk45")]
    RkLike(Box<crate::rk45::SimStepper>),
}

impl SimStepper {
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
            SimSolverMode::Auto => new_auto_stepper(dae_model, opts),
            SimSolverMode::Bdf => new_bdf_stepper(dae_model, opts),
            SimSolverMode::RkLike => new_rk_like_stepper(dae_model, opts),
        }
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            SimStepperInner::Discrete(stepper) => stepper.set_input(name, value),
            #[cfg(feature = "solver-diffsol")]
            SimStepperInner::Bdf(stepper) => stepper
                .set_input(name, value)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimStepperInner::RkLike(stepper) => stepper
                .set_input(name, value)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }

    pub fn reset(&mut self, t_start: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            SimStepperInner::Discrete(stepper) => stepper.reset(t_start),
            #[cfg(feature = "solver-diffsol")]
            SimStepperInner::Bdf(stepper) => stepper
                .reset(t_start)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimStepperInner::RkLike(stepper) => stepper
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

    pub fn step(&mut self, dt: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            SimStepperInner::Discrete(stepper) => stepper.step(dt),
            #[cfg(feature = "solver-diffsol")]
            SimStepperInner::Bdf(stepper) => stepper
                .step(dt)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimStepperInner::RkLike(stepper) => stepper
                .step(dt)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }

    pub fn time(&self) -> f64 {
        match &self.inner {
            SimStepperInner::Discrete(stepper) => stepper.time(),
            #[cfg(feature = "solver-diffsol")]
            SimStepperInner::Bdf(stepper) => stepper.time(),
            #[cfg(feature = "solver-rk45")]
            SimStepperInner::RkLike(stepper) => stepper.time(),
        }
    }

    pub fn get(&self, name: &str) -> Result<Option<f64>, SimulationDiagnosticError> {
        match &self.inner {
            SimStepperInner::Discrete(stepper) => stepper.get(name),
            #[cfg(feature = "solver-diffsol")]
            SimStepperInner::Bdf(stepper) => stepper
                .get(name)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimStepperInner::RkLike(stepper) => stepper
                .get(name)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }

    pub fn state(&self) -> Result<StepperState, SimulationDiagnosticError> {
        match &self.inner {
            SimStepperInner::Discrete(stepper) => {
                let state = stepper.state()?;
                Ok(StepperState {
                    time: state.time,
                    values: state.values,
                })
            }
            #[cfg(feature = "solver-diffsol")]
            SimStepperInner::Bdf(stepper) => {
                let state = stepper
                    .state()
                    .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
                Ok(StepperState {
                    time: state.time,
                    values: state.values,
                })
            }
            #[cfg(feature = "solver-rk45")]
            SimStepperInner::RkLike(stepper) => {
                let state = stepper
                    .state()
                    .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
                Ok(StepperState {
                    time: state.time,
                    values: state.values,
                })
            }
        }
    }

    pub fn input_names(&self) -> &[String] {
        match &self.inner {
            SimStepperInner::Discrete(stepper) => stepper.input_names(),
            #[cfg(feature = "solver-diffsol")]
            SimStepperInner::Bdf(stepper) => stepper.input_names(),
            #[cfg(feature = "solver-rk45")]
            SimStepperInner::RkLike(stepper) => stepper.input_names(),
        }
    }

    pub fn variable_names(&self) -> &[String] {
        match &self.inner {
            SimStepperInner::Discrete(stepper) => stepper.variable_names(),
            #[cfg(feature = "solver-diffsol")]
            SimStepperInner::Bdf(stepper) => stepper.variable_names(),
            #[cfg(feature = "solver-rk45")]
            SimStepperInner::RkLike(stepper) => stepper.variable_names(),
        }
    }
}

impl InteractiveStepper for SimStepper {
    type Error = SimulationDiagnosticError;

    fn new_from_dae(dae_model: &dae::Dae, opts: SimOptions) -> Result<Self, Self::Error> {
        Self::new_with_diagnostics(dae_model, opts)
    }

    fn reset(&mut self, t_start: f64) -> Result<(), Self::Error> {
        Self::reset(self, t_start)
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), Self::Error> {
        Self::set_input(self, name, value)
    }

    fn step(&mut self, dt: f64) -> Result<(), Self::Error> {
        Self::step(self, dt)
    }

    fn time(&self) -> f64 {
        Self::time(self)
    }

    fn get(&self, name: &str) -> Result<Option<f64>, Self::Error> {
        Self::get(self, name)
    }

    fn input_names(&self) -> &[String] {
        Self::input_names(self)
    }

    fn values_for(&self, names: &[String]) -> Result<Option<IndexMap<String, f64>>, Self::Error> {
        match &self.inner {
            SimStepperInner::Discrete(stepper) => stepper.values_for(names).map(Some),
            #[cfg(feature = "solver-diffsol")]
            SimStepperInner::Bdf(stepper) => stepper
                .values_for(names)
                .map(Some)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimStepperInner::RkLike(stepper) => stepper
                .values_for(names)
                .map(Some)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }

    fn max_runner_step_dt(&self) -> Option<f64> {
        match &self.inner {
            SimStepperInner::Discrete(_) => None,
            #[cfg(feature = "solver-diffsol")]
            SimStepperInner::Bdf(_) => Some(0.002),
            #[cfg(feature = "solver-rk45")]
            SimStepperInner::RkLike(_) => None,
        }
    }
}

/// Lower the DAE and apply simulation overrides exactly once. The interactive
/// steppers share this result: the pure-discrete probe and the ODE backend it
/// falls through to both consume the same solve model, so a model is never
/// lowered twice per stepper construction.
fn lower_for_interactive_stepper(
    dae_model: &dae::Dae,
    opts: &rumoca_solver::SimOptions,
) -> Result<solve::SolveModel, SimulationDiagnosticError> {
    let mut solve_model = crate::solve_lowering::lower_dae_for_simulation(dae_model, opts)
        .map_err(SimulationDiagnosticError::SolveLowering)?;
    crate::solve_lowering::apply_simulation_overrides(&mut solve_model, dae_model, opts, true)?;
    Ok(solve_model)
}

fn discrete_stepper_from_solve_model(
    solve_model: solve::SolveModel,
) -> Result<SimStepper, SimulationDiagnosticError> {
    let stepper = crate::discrete_stepper::SimStepper::from_solve_model(solve_model)?;
    Ok(SimStepper {
        inner: SimStepperInner::Discrete(Box::new(stepper)),
    })
}

fn new_auto_stepper(
    dae_model: &dae::Dae,
    opts: rumoca_solver::SimOptions,
) -> Result<SimStepper, SimulationDiagnosticError> {
    let solve_model = lower_for_interactive_stepper(dae_model, &opts)?;
    // A model with no continuous states integrates nothing: run it on the
    // discrete stepper instead of spinning up an ODE solver.
    if solve_model.state_scalar_count() == 0 {
        return discrete_stepper_from_solve_model(solve_model);
    }
    #[cfg(feature = "solver-diffsol")]
    {
        crate::diffsol::SimStepper::from_solve_model(solve_model, opts).map(|stepper| SimStepper {
            inner: SimStepperInner::Bdf(Box::new(stepper)),
        })
    }
    #[cfg(all(not(feature = "solver-diffsol"), feature = "solver-rk45"))]
    {
        crate::rk45::SimStepper::from_solve_model(solve_model, opts).map(|stepper| SimStepper {
            inner: SimStepperInner::RkLike(Box::new(stepper)),
        })
    }
    #[cfg(not(any(feature = "solver-diffsol", feature = "solver-rk45")))]
    {
        let _ = (solve_model, opts);
        Err(SimulationDiagnosticError::Solver(
            "no interactive solver backend is enabled".to_string(),
        ))
    }
}

fn new_bdf_stepper(
    dae_model: &dae::Dae,
    opts: rumoca_solver::SimOptions,
) -> Result<SimStepper, SimulationDiagnosticError> {
    #[cfg(feature = "solver-diffsol")]
    {
        crate::diffsol::SimStepper::new_with_diagnostics(dae_model, opts).map(|stepper| {
            SimStepper {
                inner: SimStepperInner::Bdf(Box::new(stepper)),
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

fn new_rk_like_stepper(
    dae_model: &dae::Dae,
    opts: rumoca_solver::SimOptions,
) -> Result<SimStepper, SimulationDiagnosticError> {
    let solve_model = lower_for_interactive_stepper(dae_model, &opts)?;
    if solve_model.state_scalar_count() == 0 {
        return discrete_stepper_from_solve_model(solve_model);
    }
    #[cfg(feature = "solver-rk45")]
    {
        crate::rk45::SimStepper::from_solve_model(solve_model, opts).map(|stepper| SimStepper {
            inner: SimStepperInner::RkLike(Box::new(stepper)),
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
