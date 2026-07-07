use indexmap::IndexMap;
use rumoca_eval_solve::{self as solve_eval, SolveRuntime};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

#[cfg(feature = "runner")]
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
    RuntimeOnly(Box<RuntimeOnlySession>),
    #[cfg(feature = "solver-diffsol")]
    Diffsol(Box<crate::diffsol::SimulationSession>),
    #[cfg(feature = "solver-rk45")]
    RkLike(Box<crate::rk45::SimulationSession>),
}

struct RuntimeOnlySession {
    _runtime_context: solve_eval::SimulationContext,
    model: solve::SolveModel,
    opts: rumoca_solver::SimOptions,
    runtime: SolveRuntime,
    params: Vec<f64>,
    current_y: Vec<f64>,
    current_t: f64,
    input_values: IndexMap<String, f64>,
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
            SimulationSessionInner::RuntimeOnly(session) => session.set_input(name, value),
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
            SimulationSessionInner::RuntimeOnly(session) => session.reset(t_start),
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
            SimulationSessionInner::RuntimeOnly(session) => session.advance_to(target_time),
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
            SimulationSessionInner::RuntimeOnly(session) => session.step(dt),
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
            SimulationSessionInner::RuntimeOnly(session) => session.time(),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.time(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.time(),
        }
    }

    pub fn get(&self, name: &str) -> Result<Option<f64>, SimulationDiagnosticError> {
        match &self.inner {
            SimulationSessionInner::RuntimeOnly(session) => session.get(name),
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
            SimulationSessionInner::RuntimeOnly(session) => session.state(),
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
            SimulationSessionInner::RuntimeOnly(session) => session.input_names(),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.input_names(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.input_names(),
        }
    }

    pub fn variable_names(&self) -> &[String] {
        match &self.inner {
            SimulationSessionInner::RuntimeOnly(session) => session.variable_names(),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.variable_names(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.variable_names(),
        }
    }
}

#[cfg(feature = "runner")]
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
        match &self.inner {
            SimulationSessionInner::RuntimeOnly(session) => session
                .values_for(names)
                .map(Some)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .values_for(names)
                .map(Some)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .values_for(names)
                .map(Some)
                .map_err(|err| SimulationDiagnosticError::Solver(err.to_string())),
        }
    }

    fn max_runner_advance_dt(&self) -> Option<f64> {
        match &self.inner {
            SimulationSessionInner::RuntimeOnly(_) => None,
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.max_runner_advance_dt(),
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

fn new_auto_session(
    dae_model: &dae::Dae,
    opts: rumoca_solver::SimOptions,
) -> Result<SimulationSession, SimulationDiagnosticError> {
    let solve_model = lower_for_simulation_session(dae_model, &opts)?;
    if solve_model.state_scalar_count() == 0 {
        return new_runtime_only_session(solve_model, opts);
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

impl RuntimeOnlySession {
    fn new(
        model: solve::SolveModel,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        if model.state_scalar_count() != 0 {
            return Err(SimulationDiagnosticError::Solver(
                "runtime-only session requires a model with zero continuous states".to_string(),
            ));
        }
        let runtime_context = solve_eval::SimulationContext::new();
        runtime_context.hydrate_solve_model(&model);
        let runtime = SolveRuntime::new(&model)
            .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
        let mut session = Self {
            _runtime_context: runtime_context,
            params: model.parameters.clone(),
            current_y: model.initial_y.clone(),
            current_t: opts.t_start,
            model,
            opts,
            runtime,
            input_values: IndexMap::new(),
        };
        session.initialize_runtime()?;
        Ok(session)
    }

    fn initialize_runtime(&mut self) -> Result<(), SimulationDiagnosticError> {
        let tol = self.tol();
        self.runtime.set_initial_event_flag(&mut self.params, true);
        self.refresh_visible_runtime_values()?;
        self.runtime.set_initial_event_flag(&mut self.params, false);
        rumoca_solver::commit_pre_params_after_event(
            &self.model,
            &self.current_y,
            &mut self.params,
            tol,
        );
        Ok(())
    }

    fn tol(&self) -> f64 {
        self.opts.atol.max(1.0e-10)
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimulationDiagnosticError> {
        let Some(param_idx) = self.model.problem.solve_layout.input_parameter_index(name) else {
            return Err(SimulationDiagnosticError::Solver(format!(
                "unknown input '{name}'"
            )));
        };
        self.input_values.insert(name.to_string(), value);
        if let Some(slot) = self.params.get_mut(param_idx) {
            *slot = value;
        }
        Ok(())
    }

    fn advance_to(&mut self, target_time: f64) -> Result<(), SimulationDiagnosticError> {
        if target_time <= self.current_t {
            return Ok(());
        }
        self.step(target_time - self.current_t)
    }

    fn step(&mut self, dt: f64) -> Result<(), SimulationDiagnosticError> {
        if !dt.is_finite() || dt < 0.0 {
            return Err(SimulationDiagnosticError::Solver(format!(
                "invalid runtime-only step dt {dt}"
            )));
        }
        if dt == 0.0 {
            return Ok(());
        }
        let tol = self.tol();
        self.current_t = (self.current_t + dt).min(self.opts.t_end);
        let values = self
            .runtime
            .eval_scalar_program_block(
                &self.model.problem.discrete.rhs,
                &self.current_y,
                &self.params,
                self.current_t,
            )
            .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
        solve_eval::apply_discrete_slot_values(
            &self.model.problem.discrete.update_targets,
            &values,
            &mut self.current_y,
            &mut self.params,
            tol,
        )
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
        self.runtime
            .apply_runtime_assignments_once(&mut self.current_y, &mut self.params, self.current_t)
            .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
        self.refresh_visible_runtime_values()?;
        rumoca_solver::commit_pre_params_after_event(
            &self.model,
            &self.current_y,
            &mut self.params,
            tol,
        );
        Ok(())
    }

    fn reset(&mut self, t_start: f64) -> Result<(), SimulationDiagnosticError> {
        self.input_values.clear();
        self.params.clone_from(&self.model.parameters);
        self.current_y.clone_from(&self.model.initial_y);
        self.current_t = t_start;
        self.opts.t_start = t_start;
        self.initialize_runtime()
    }

    fn time(&self) -> f64 {
        self.current_t
    }

    fn get(&self, name: &str) -> Result<Option<f64>, SimulationDiagnosticError> {
        if let Some(value) = self.input_values.get(name).copied() {
            return Ok(Some(value));
        }
        let values = self.session_visible_values()?;
        Ok(values.get(name).copied())
    }

    fn state(&self) -> Result<SessionState, SimulationDiagnosticError> {
        Ok(SessionState {
            time: self.time(),
            values: self.session_visible_values()?,
        })
    }

    #[cfg(feature = "runner")]
    fn values_for(
        &self,
        names: &[String],
    ) -> Result<IndexMap<String, f64>, SimulationDiagnosticError> {
        let visible_values = self.session_visible_values()?;
        let mut values = IndexMap::with_capacity(names.len());
        for name in names {
            if let Some(value) = visible_values.get(name).copied() {
                values.insert(name.clone(), value);
            }
        }
        Ok(values)
    }

    fn input_names(&self) -> &[String] {
        self.model.problem.solve_layout.input_scalar_names()
    }

    fn variable_names(&self) -> &[String] {
        &self.model.visible_names
    }

    fn session_visible_values(&self) -> Result<IndexMap<String, f64>, SimulationDiagnosticError> {
        let visible_values = self
            .runtime
            .visible_values(&self.current_y, &self.params, self.current_t)
            .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))?;
        let mut values = IndexMap::with_capacity(self.model.visible_names.len());
        for (idx, name) in self.model.visible_names.iter().enumerate() {
            let Some(value) = visible_values.get(idx).copied() else {
                return Err(SimulationDiagnosticError::Solver(format!(
                    "visible value '{name}' resolved to index {idx}, but runtime returned {} values",
                    visible_values.len()
                )));
            };
            values.insert(name.clone(), value);
        }
        values.extend(
            self.input_values
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        Ok(values)
    }

    fn refresh_visible_runtime_values(&mut self) -> Result<(), SimulationDiagnosticError> {
        let tol = self.tol();
        self.runtime
            .refresh_algebraic_and_output_slots(
                self.current_t,
                &mut self.current_y,
                &self.params,
                tol,
                256,
            )
            .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))
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
        return new_runtime_only_session(solve_model, opts);
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

fn new_runtime_only_session(
    solve_model: solve::SolveModel,
    opts: rumoca_solver::SimOptions,
) -> Result<SimulationSession, SimulationDiagnosticError> {
    RuntimeOnlySession::new(solve_model, opts).map(|session| SimulationSession {
        inner: SimulationSessionInner::RuntimeOnly(Box::new(session)),
    })
}
