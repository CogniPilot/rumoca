use indexmap::IndexMap;
use rumoca_ir_dae as dae;

scheduled_executor_items! {
    use crate::SimulationSessionApi;
}
use crate::{SimSolverMode, SimulationDiagnosticError};

#[derive(Debug, Clone)]
pub struct SessionState {
    pub time: f64,
    pub values: IndexMap<String, f64>,
}

pub struct SimulationSession {
    inner: SimulationSessionInner,
}

/// A fresh checked FMI component and the source-diagnostic outcome of its lowering.
pub struct PreparedSimulationComponent {
    component: rumoca_ir_solve::fmi::FmiComponent,
    replayable: bool,
}

impl PreparedSimulationComponent {
    /// Retain an immutable wire root only if replay can reproduce preparation without
    /// suppressing source diagnostics from state selection or event guards.
    pub fn replay_root(&self) -> Option<rumoca_ir_solve::RetainedAssertionRoot> {
        self.replayable
            .then(|| self.component.runtime_view().retain_assertion_root())
    }

    pub fn into_component(self) -> rumoca_ir_solve::fmi::FmiComponent {
        self.component
    }
}

enum SimulationSessionInner {
    #[cfg(feature = "solver-diffsol")]
    Diffsol(Box<crate::diffsol::SimulationSession>),
    #[cfg(feature = "solver-rk45")]
    RkLike(Box<crate::rk45::SimulationSession>),
}

impl SimulationSession {
    #[cfg(all(
        feature = "precompiled-runtime",
        feature = "solver-rk45",
        target_arch = "wasm32"
    ))]
    pub(crate) fn from_prepared_rk(
        artifact: rumoca_solver::fmi_me::MeModelArtifact,
        opts: rumoca_solver::SimOptions,
        backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
    ) -> Result<Self, SimulationDiagnosticError> {
        crate::backend_options::validate(&opts)?;
        if !crate::precompiled_runtime::admits_solver_profile(opts.solver_mode) {
            return Err(SimulationDiagnosticError::Solver(
                "precompiled browser product requires its declared rk-like or fixed-step profile"
                    .into(),
            ));
        }
        crate::rk45::SimulationSession::from_artifact(artifact, opts, backend).map(|session| Self {
            inner: SimulationSessionInner::RkLike(Box::new(session)),
        })
    }

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
        crate::backend_options::validate(&opts)?;
        let artifact = crate::solve_lowering::lower_runtime_fmi_artifact(dae_model, &opts)?;
        Self::from_runtime_artifact(artifact, opts)
    }

    /// Construct the immutable component for this DAE and its exact initialization options.
    /// The returned component holds no numerical session history.
    pub fn prepare_component(
        dae: &dae::Dae,
        options: &rumoca_solver::SimOptions,
    ) -> Result<PreparedSimulationComponent, SimulationDiagnosticError> {
        crate::backend_options::validate(options)?;
        let lowered =
            crate::solve_lowering::lower_correlated_for_simulation_with_overrides(dae, options)?;
        let replayable = lowered.withheld_state_preferences().is_empty()
            && lowered
                .model()
                .problem
                .continuous
                .unlocalizable_guards
                .is_empty();
        Ok(PreparedSimulationComponent {
            component: crate::solve_lowering::finish_component(lowered)?,
            replayable,
        })
    }

    /// Start a fresh solver from a checked component prepared with the same initialization
    /// inputs, parameters and state starts. Persistent callers must verify their artifact key.
    pub fn from_prepared_component(
        component: rumoca_ir_solve::fmi::FmiComponent,
        options: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        crate::backend_options::validate(&options)?;
        let artifact = crate::solve_lowering::admit_runtime_component(component, &options)?;
        Self::from_runtime_artifact(artifact, options)
    }

    fn from_runtime_artifact(
        artifact: RuntimeArtifact,
        opts: rumoca_solver::SimOptions,
    ) -> Result<Self, SimulationDiagnosticError> {
        match opts.solver_mode {
            SimSolverMode::Auto => new_auto_session(artifact, opts),
            SimSolverMode::Bdf => new_bdf_session(artifact, opts),
            SimSolverMode::RkLike | SimSolverMode::VelImplicit | SimSolverMode::FixedStep => {
                new_rk_like_session(artifact, opts)
            }
        }
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimulationDiagnosticError> {
        self.set_inputs(&[(name, value)])
    }

    /// Apply one atomic component input batch, restarting changed history once.
    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .set_inputs(inputs)
                .map_err(SimulationDiagnosticError::from),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .set_inputs(inputs)
                .map_err(SimulationDiagnosticError::from),
        }
    }

    pub fn reset(&mut self, t_start: f64) -> Result<(), SimulationDiagnosticError> {
        match &mut self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .reset(t_start)
                .map_err(SimulationDiagnosticError::from),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .reset(t_start)
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

    /// Ensure the finite solver horizon includes `target_time`.
    pub fn ensure_end_time(&mut self, target_time: f64) {
        match &mut self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.ensure_end_time(target_time),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.ensure_end_time(target_time),
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

    /// The engine the session selected for its Solve programs and, when
    /// compiled execution was refused for an admissible request, why.
    pub fn execution_receipt(&self) -> rumoca_solver::SimExecutionReceipt {
        match &self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session.execution_receipt(),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session.execution_receipt(),
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

    /// The requested variables at the current point, in request order; `None`
    /// for a name that is not a variable.
    pub fn selected_values(
        &self,
        names: &[String],
    ) -> Result<Vec<Option<f64>>, SimulationDiagnosticError> {
        match &self.inner {
            #[cfg(feature = "solver-diffsol")]
            SimulationSessionInner::Diffsol(session) => session
                .selected_values(names)
                .map_err(SimulationDiagnosticError::from),
            #[cfg(feature = "solver-rk45")]
            SimulationSessionInner::RkLike(session) => session
                .selected_values(names)
                .map_err(SimulationDiagnosticError::from),
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

scheduled_executor_items! {
    impl SimulationSessionApi for SimulationSession {
        type Error = SimulationDiagnosticError;

        fn reset(&mut self, t_start: f64) -> Result<(), Self::Error> {
            Self::reset(self, t_start)
        }

        fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), Self::Error> {
            Self::set_inputs(self, inputs)
        }

        fn ensure_end_time(&mut self, target_time: f64) {
            Self::ensure_end_time(self, target_time);
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
                #[cfg(feature = "solver-diffsol")]
                SimulationSessionInner::Diffsol(session) => session
                    .values_for(names)
                    .map(Some)
                    .map_err(SimulationDiagnosticError::from),
                #[cfg(feature = "solver-rk45")]
                SimulationSessionInner::RkLike(session) => session
                    .values_for(names)
                    .map(Some)
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
}

type RuntimeArtifact = (
    rumoca_solver::fmi_me::MeModelArtifact,
    Option<rumoca_solver::fmi_me::MeExecutionBackend>,
);

fn new_auto_session(
    (artifact, execution_backend): RuntimeArtifact,
    opts: rumoca_solver::SimOptions,
) -> Result<SimulationSession, SimulationDiagnosticError> {
    #[cfg(all(feature = "solver-diffsol", feature = "solver-rk45"))]
    {
        match crate::diffsol::select_auto_integrator(&artifact, &opts, execution_backend.clone())
            .map_err(SimulationDiagnosticError::from)?
        {
            crate::diffsol::SelectedAutoIntegrator::Bdf(_) => {
                crate::diffsol::SimulationSession::from_artifact(artifact, opts, execution_backend)
                    .map(|session| SimulationSession {
                        inner: SimulationSessionInner::Diffsol(Box::new(session)),
                    })
            }
            crate::diffsol::SelectedAutoIntegrator::RkLike => {
                crate::rk45::SimulationSession::from_artifact(artifact, opts, execution_backend)
                    .map(|session| SimulationSession {
                        inner: SimulationSessionInner::RkLike(Box::new(session)),
                    })
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
        crate::rk45::SimulationSession::from_artifact(artifact, opts, execution_backend).map(
            |session| SimulationSession {
                inner: SimulationSessionInner::RkLike(Box::new(session)),
            },
        )
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
    (artifact, execution_backend): RuntimeArtifact,
    opts: rumoca_solver::SimOptions,
) -> Result<SimulationSession, SimulationDiagnosticError> {
    #[cfg(feature = "solver-diffsol")]
    {
        crate::diffsol::SimulationSession::from_artifact(artifact, opts, execution_backend).map(
            |session| SimulationSession {
                inner: SimulationSessionInner::Diffsol(Box::new(session)),
            },
        )
    }
    #[cfg(not(feature = "solver-diffsol"))]
    {
        let _ = (artifact, execution_backend, opts);
        Err(SimulationDiagnosticError::Solver(
            "bdf solver requested, but this build does not include the diffsol backend".to_string(),
        ))
    }
}

fn new_rk_like_session(
    (artifact, execution_backend): RuntimeArtifact,
    opts: rumoca_solver::SimOptions,
) -> Result<SimulationSession, SimulationDiagnosticError> {
    #[cfg(feature = "solver-rk45")]
    {
        crate::rk45::SimulationSession::from_artifact(artifact, opts, execution_backend).map(
            |session| SimulationSession {
                inner: SimulationSessionInner::RkLike(Box::new(session)),
            },
        )
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
