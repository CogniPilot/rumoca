//! Trajectory sensitivities and objective gradients of a model over a run.
//!
//! These are the probes behind `rumoca sim --inspect trajectory-sensitivity`
//! and the trajectory mode of `--inspect objective-gradient`. What may be
//! differentiated is decided by the Solve construction
//! (`rumoca_ir_solve::SensitivityProblem`, SOLVE-C74); the variational and
//! adjoint systems are advanced by the same Dormand-Prince plugin that
//! simulates the model. This module lowers once, applies the request to the
//! construction, and names the result.

use std::rc::Rc;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_solver::{
    ForwardSensitivityTrajectory, ObjectiveGradient, SimOptions, SimResult, SimVariableMeta,
    SolveRuntime, TrajectoryConfig, TrajectoryError, TrajectoryObjective, TrajectoryProblem,
    adjoint_objective_gradient, forward_objective_gradient, forward_sensitivity_trajectory,
};

use crate::SimulationDiagnosticError;
use crate::me_backend::{default_output_dt, default_step_size};
use crate::prepared_vectors::settle_prepared_vectors;
use crate::solve_lowering::{
    ExcludedParameter, ParameterClassification, lower_for_simulation_with_overrides,
    select_sensitivity_parameters,
};

impl From<TrajectoryError> for SimulationDiagnosticError {
    fn from(error: TrajectoryError) -> Self {
        let message = error.to_string();
        match error {
            TrajectoryError::InvalidRequest { .. } => Self::InvalidOverride { message },
            TrajectoryError::Refused(refusal) if refusal.is_request_error() => {
                Self::InvalidOverride { message }
            }
            TrajectoryError::Refused(refusal) => Self::RuntimePreparation {
                message,
                span: refusal.span(),
            },
            TrajectoryError::Initialization { .. } => Self::RuntimePreparation {
                message,
                span: None,
            },
            TrajectoryError::LinearSolve { .. }
            | TrajectoryError::Runtime(_)
            | TrajectoryError::Integration(_) => Self::Solver(message),
        }
    }
}

/// The numerical plugin a trajectory system is advanced with.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TrajectoryPlugin {
    /// The explicit Dormand-Prince plugin, the default.
    #[default]
    Rk45,
    /// The implicit BDF plugin, which declares no continuous-extension order, so
    /// the adjoint checkpoint contract cannot be proved for it.
    #[cfg(feature = "solver-diffsol")]
    Bdf,
}

impl TrajectoryPlugin {
    fn build(
        self,
        setup: rumoca_solver::fmi_me::MeNumericalSetup,
    ) -> Box<dyn rumoca_solver::fmi_me::MeIntegratorBackend> {
        match self {
            Self::Rk45 => (crate::rk45::integrator_factory().build)(setup),
            #[cfg(feature = "solver-diffsol")]
            Self::Bdf => rumoca_solver_diffsol::model_exchange_integrator(setup),
        }
    }
}

fn build_plugin(
    setup: rumoca_solver::fmi_me::MeNumericalSetup,
) -> Box<dyn rumoca_solver::fmi_me::MeIntegratorBackend> {
    TrajectoryPlugin::Rk45.build(setup)
}

/// A model lowered once and prepared for trajectory sensitivities.
///
/// The session owns the lowering, the proved sensitivity construction, and the
/// settled initial point; a different parameter point re-settles the initial
/// point on the same runtime and never lowers again.
pub struct TrajectorySession {
    problem: Rc<TrajectoryProblem>,
    names: Vec<String>,
    excluded: Vec<ExcludedParameter>,
    opts: SimOptions,
}

impl TrajectorySession {
    /// Lower `model` and prove the sensitivity construction over the
    /// parameters `requested` (every independent tunable parameter some Solve
    /// program reads when empty).
    pub fn new(
        model: &dae::Dae,
        opts: &SimOptions,
        requested: &[String],
    ) -> Result<Self, SimulationDiagnosticError> {
        let solve_model = lower_for_simulation_with_overrides(model, opts)?;
        let classification = select_sensitivity_parameters(model, &solve_model.problem);
        let names = requested_names(&classification, requested);
        let construction =
            solve::SensitivityProblem::construct(&solve_model.problem, &names, &classification)
                .map_err(TrajectoryError::from)?;
        let runtime = Rc::new(SolveRuntime::new(&solve_model).map_err(|error| {
            SimulationDiagnosticError::RuntimePreparation {
                message: error.to_string(),
                span: None,
            }
        })?);
        let (y0, params) =
            settle_prepared_vectors(&runtime, opts.t_start, solve_model.parameters.to_vec())?;
        let config = TrajectoryConfig {
            t_start: opts.t_start,
            t_end: opts.t_end,
            relative_tolerance: opts.rtol,
            absolute_tolerance: opts.atol,
            initial_step: Some(default_step_size(opts)),
            checkpoint_budget_bytes: opts.checkpoint_budget_bytes,
        };
        let problem = Rc::new(TrajectoryProblem::new(
            runtime,
            y0,
            params,
            construction,
            config,
        )?);
        Ok(Self {
            problem,
            names,
            excluded: classification.excluded().to_vec(),
            opts: opts.clone(),
        })
    }

    /// The parameters differentiated, in request order.
    #[must_use]
    pub fn parameter_names(&self) -> &[String] {
        &self.names
    }

    /// Parameters the model declares but that cannot be differentiated, each
    /// with the reason.
    #[must_use]
    pub fn excluded(&self) -> &[ExcludedParameter] {
        &self.excluded
    }

    /// Requested parameters that sit exactly on the switching value of an
    /// admitted relation at this session's point: the sensitivity there is
    /// that of one side of the switch.
    #[must_use]
    pub fn switching_value_notes(&self) -> Vec<solve::SwitchingValueNote> {
        self.problem.switching_value_notes()
    }

    /// The same session at new values of the differentiated parameters,
    /// re-settled from the already lowered runtime.
    pub fn with_parameter_values(&self, values: &[f64]) -> Result<Self, SimulationDiagnosticError> {
        let mut params = self.problem.base_parameters().to_vec();
        for (slot, value) in self.problem.parameter_slots().zip(values) {
            params[slot] = *value;
        }
        let (y0, params) =
            settle_prepared_vectors(self.problem.runtime(), self.opts.t_start, params)?;
        Ok(Self {
            problem: Rc::new(self.problem.at_point(y0, params)?),
            names: self.names.clone(),
            excluded: self.excluded.clone(),
            opts: self.opts.clone(),
        })
    }

    /// The trajectory of every solver variable and its sensitivity `d(v)/d(p)`
    /// to every differentiated parameter, sampled on the run's output grid.
    pub fn sensitivity(&self) -> Result<SimResult, SimulationDiagnosticError> {
        let times = rumoca_solver::timeline::try_build_output_times(
            self.opts.t_start,
            self.opts.t_end,
            default_output_dt(&self.opts),
        )
        .map_err(|error| SimulationDiagnosticError::Solver(format!("output times: {error:?}")))?;
        let trajectory =
            forward_sensitivity_trajectory(&self.problem, &build_plugin, &times, None)?;
        Ok(sensitivity_result(&self.problem, &trajectory))
    }

    /// Gradient `dJ/dp` of a trajectory objective, by forward sensitivity or by
    /// the adjoint system.
    pub fn gradient(
        &self,
        objective: &TrajectoryObjective,
        adjoint: bool,
    ) -> Result<ObjectiveGradient, SimulationDiagnosticError> {
        self.gradient_with(objective, adjoint, TrajectoryPlugin::Rk45)
    }

    /// The same gradient advanced by `plugin`. A plugin that declares no
    /// continuous-extension order is refused by the adjoint before any step.
    pub fn gradient_with(
        &self,
        objective: &TrajectoryObjective,
        adjoint: bool,
        plugin: TrajectoryPlugin,
    ) -> Result<ObjectiveGradient, SimulationDiagnosticError> {
        let build = |setup| plugin.build(setup);
        let gradient = if adjoint {
            adjoint_objective_gradient(&self.problem, &build, objective)
        } else {
            forward_objective_gradient(&self.problem, &build, objective)
        }?;
        Ok(gradient)
    }
}

/// The parameters to differentiate: the request when given, otherwise every
/// selected parameter. A requested name is judged by the construction against
/// the same classification, which refuses it by name with its reason.
fn requested_names(classification: &ParameterClassification, requested: &[String]) -> Vec<String> {
    if requested.is_empty() {
        classification.selected().to_vec()
    } else {
        requested.to_vec()
    }
}

/// The trajectory of every solver variable and its sensitivity to every
/// selected parameter, as one result whose sensitivity columns follow the
/// variables like any trace output.
pub fn trajectory_sensitivity_for_dae(
    model: &dae::Dae,
    opts: &SimOptions,
    parameters: &[String],
) -> Result<SimResult, SimulationDiagnosticError> {
    TrajectorySession::new(model, opts, parameters)?.sensitivity()
}

/// Gradient `dJ/dp` of a trajectory objective, by forward sensitivity or by
/// the adjoint system.
pub fn trajectory_objective_gradient_for_dae(
    model: &dae::Dae,
    opts: &SimOptions,
    parameters: &[String],
    objective: &TrajectoryObjective,
    adjoint: bool,
) -> Result<ObjectiveGradient, SimulationDiagnosticError> {
    TrajectorySession::new(model, opts, parameters)?.gradient(objective, adjoint)
}

fn sensitivity_result(
    problem: &TrajectoryProblem,
    trajectory: &ForwardSensitivityTrajectory,
) -> SimResult {
    let m = trajectory.parameters.len();
    let variables = &trajectory.variables;
    let mut names = variables.clone();
    let mut meta: Vec<SimVariableMeta> = variables
        .iter()
        .enumerate()
        .map(|(index, name)| column_meta(name, index < problem.state_count(), "solver variable"))
        .collect();
    let mut data: Vec<Vec<f64>> = (0..variables.len())
        .map(|index| {
            trajectory
                .values
                .iter()
                .map(|row| positive_zero(row[index]))
                .collect()
        })
        .collect();
    for (index, variable) in variables.iter().enumerate() {
        for (j, parameter) in trajectory.parameters.iter().enumerate() {
            let name = format!("d({variable})/d({parameter})");
            meta.push(column_meta(&name, false, "sensitivity"));
            data.push(
                trajectory
                    .sensitivities
                    .iter()
                    .map(|row| positive_zero(row[index * m + j]))
                    .collect(),
            );
            names.push(name);
        }
    }
    SimResult {
        times: trajectory.times.clone(),
        names,
        data,
        n_states: problem.state_count(),
        variable_meta: meta,
        termination: None,
        diagnostics: Vec::new(),
    }
}

/// `-0.0` is `0.0` in a result column.
fn positive_zero(value: f64) -> f64 {
    if value == 0.0 { 0.0 } else { value }
}

fn column_meta(name: &str, is_state: bool, role: &str) -> SimVariableMeta {
    SimVariableMeta {
        name: name.to_string(),
        role: role.to_string(),
        is_state,
        value_type: Some("Real".to_string()),
        variability: None,
        time_domain: None,
        unit: None,
        start: None,
        min: None,
        max: None,
        nominal: None,
        fixed: None,
        description: None,
        state_coordinate: None,
        phasor: None,
    }
}
