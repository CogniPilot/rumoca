//! Trajectory sensitivities of a lowered Solve model.
//!
//! The reduced ODE of a model is `x' = F(x, p, t) = f(x, z(x, p, t), p, t)`,
//! where the algebraic projection recovers `z` from `g(x, z, p, t) = 0`. The
//! forward sensitivity `S = dx/dp` obeys the variational equation
//!
//! ```text
//!   S' = (∂F/∂x) S + ∂F/∂p,    S(t0) = dx(t0)/dp
//! ```
//!
//! Both terms of its right-hand side are one directional derivative of the
//! Solve derivative rows seeded with `[S_j | e_pj]`; the seed's algebraic
//! entries are filled by the projection's own forward sensitivity, which is
//! the consistency condition `g_z dz = -(g_x S + g_p)` of the index-reduced
//! rows. Every derivative is the automatic directional derivative the Solve
//! lowering issues, and the augmented system `[x; S; quadratures]` is advanced
//! by the unchanged numerical plugin through [`crate::fmi_me::ode_driver`].
//!
//! What may be differentiated, and how the augmented system is laid out, is
//! decided once by [`rumoca_ir_solve::SensitivityProblem`] (SOLVE-C74); this
//! module evaluates the rows that proof admits.
//!
//! The consistent initial sensitivity `S(t0)` is the implicit-function
//! derivative of the initialization system: the residual rows of the checked
//! initialization projection, differentiated along the projection's own
//! unknowns and along each parameter.
//!
//! # References
//!
//! The variational equations and their consistent initialization for
//! differential-algebraic systems: S. Li and L. Petzold, "Software and
//! algorithms for sensitivity analysis of large-scale differential algebraic
//! systems", Journal of Computational and Applied Mathematics 125(1-2):131-145,
//! 2000, doi:10.1016/S0377-0427(00)00465-9.

use std::rc::Rc;

use rumoca_ir_solve as solve;
use rumoca_ir_solve::SensitivityLayout;
use rumoca_ir_solve::SensitivityParameter;

use crate::fmi_me::MeIntegratorBackend;
use crate::fmi_me::MeNumericalSetup;
use crate::fmi_me::ode_driver::{ContinuousOde, OdeDriveError, OdeRun, integrate_ode};
use crate::runtime::solve_ops::RuntimeSolveError;
use crate::runtime::solve_runtime::{AlgebraicLinearization, AlgebraicSettle, SolveRuntime};
use crate::runtime::trajectory_initial::initial_state_sensitivity;

/// Why a trajectory sensitivity could not be constructed or evaluated.
#[derive(Debug, thiserror::Error)]
pub enum TrajectoryError {
    /// The construction proof refused the model or the requested parameters.
    #[error("trajectory sensitivity is not defined for this request: {0}")]
    Refused(#[from] solve::SensitivityRefusal),
    /// A requested objective variable, data series, or horizon is invalid.
    #[error("{reason}")]
    InvalidRequest { reason: String },
    /// The consistent initial sensitivity could not be solved.
    #[error("the initial sensitivity is undefined: {reason}")]
    Initialization { reason: String },
    /// A matrix-free cotangent solve did not converge.
    #[error("{reason}")]
    LinearSolve { reason: String },
    /// A Solve row evaluation failed.
    #[error("{0}")]
    Runtime(#[from] RuntimeSolveError),
    /// The numerical plugin failed.
    #[error("{0}")]
    Integration(#[from] OdeDriveError),
}

pub(crate) fn invalid_request(reason: impl Into<String>) -> TrajectoryError {
    TrajectoryError::InvalidRequest {
        reason: reason.into(),
    }
}

/// Numerical settings of one trajectory computation.
#[derive(Debug, Clone, Copy)]
pub struct TrajectoryConfig {
    pub t_start: f64,
    pub t_end: f64,
    pub relative_tolerance: f64,
    pub absolute_tolerance: f64,
    pub initial_step: Option<f64>,
    /// Memory the adjoint may spend storing the forward path, in bytes.
    pub checkpoint_budget_bytes: u64,
}

/// The integrator constructor the caller supplies, as the common host does.
pub type PluginBuilder<'a> = &'a dyn Fn(MeNumericalSetup) -> Box<dyn MeIntegratorBackend>;

/// A lowered model prepared for trajectory sensitivities: the runtime, its
/// settled initial point, the proved construction, and the consistent initial
/// sensitivity of the states.
pub struct TrajectoryProblem {
    pub(crate) runtime: Rc<SolveRuntime>,
    pub(crate) params: Vec<f64>,
    pub(crate) construction: solve::SensitivityProblem,
    pub(crate) y0: Vec<f64>,
    /// `initial_state_sensitivity[j][i] = dx_i(t0)/dp_j`.
    pub(crate) initial_state_sensitivity: Vec<Vec<f64>>,
    pub(crate) config: TrajectoryConfig,
    /// The algebraic settle the construction decided.
    pub(crate) settle: AlgebraicSettle,
}

impl TrajectoryProblem {
    /// Prepare `runtime` at the settled initial point `(y0, params)` for the
    /// proved `construction`.
    pub fn new(
        runtime: Rc<SolveRuntime>,
        y0: Vec<f64>,
        params: Vec<f64>,
        construction: solve::SensitivityProblem,
        config: TrajectoryConfig,
    ) -> Result<Self, TrajectoryError> {
        if !(config.t_start.is_finite()
            && config.t_end.is_finite()
            && config.t_end > config.t_start)
        {
            return Err(invalid_request(format!(
                "the horizon [{}, {}] is not a finite interval of positive length",
                config.t_start, config.t_end
            )));
        }
        if y0.len() != runtime.solver_count {
            return Err(invalid_request(format!(
                "the initial point has {} entries for {} solver variables",
                y0.len(),
                runtime.solver_count
            )));
        }
        let settle = construction.settle();
        let settle = AlgebraicSettle {
            tol: settle.tolerance,
            max_iters: settle.max_iterations,
        };
        let initial_state_sensitivity = initial_state_sensitivity(
            &runtime,
            (&y0, &params, config.t_start),
            &construction,
            settle,
        )?;
        Ok(Self {
            runtime,
            params,
            construction,
            y0,
            initial_state_sensitivity,
            config,
            settle,
        })
    }

    /// The same model and construction at new parameter values and the
    /// correspondingly re-settled initial point; nothing is lowered again.
    pub fn at_point(&self, y0: Vec<f64>, params: Vec<f64>) -> Result<Self, TrajectoryError> {
        Self::new(
            Rc::clone(&self.runtime),
            y0,
            params,
            self.construction.clone(),
            self.config,
        )
    }

    #[must_use]
    pub fn runtime(&self) -> &SolveRuntime {
        &self.runtime
    }

    /// The parameter vector of the settled initial point.
    #[must_use]
    pub fn base_parameters(&self) -> &[f64] {
        &self.params
    }

    /// The runtime slot of each differentiated parameter, in request order.
    pub fn parameter_slots(&self) -> impl Iterator<Item = usize> + '_ {
        self.parameters().iter().map(|parameter| parameter.slot)
    }

    pub(crate) fn parameters(&self) -> &[SensitivityParameter] {
        self.construction.parameters()
    }

    /// Solver-variable names in solver order (states first).
    #[must_use]
    pub fn variable_names(&self) -> &[String] {
        &self.runtime.model.problem.solve_layout.solver_maps.names
    }

    #[must_use]
    pub fn state_count(&self) -> usize {
        self.runtime.state_count
    }

    pub(crate) fn linearization(&self, t: f64) -> AlgebraicLinearization<'_> {
        AlgebraicLinearization {
            t,
            params: &self.params,
            settle: self.settle,
        }
    }

    pub(crate) fn layout(&self, quadratures: usize) -> SensitivityLayout {
        self.construction.layout(quadratures)
    }

    /// Plugin tolerances scaled by each state's own nominal; the construction
    /// decides how a sensitivity entry and a quadrature scale from them.
    pub(crate) fn nominals(&self, quadrature_count: usize) -> Vec<f64> {
        let model = &self.runtime.model;
        let state_scales: Vec<f64> = (0..self.runtime.state_count)
            .map(|index| model.solver_variable_scale(index))
            .collect();
        self.construction
            .nominals(&state_scales, &self.params, quadrature_count)
    }

    /// Requested parameters that sit exactly on the switching value of an
    /// admitted relation at this point, where the sensitivity is one-sided.
    #[must_use]
    pub fn switching_value_notes(&self) -> Vec<solve::SwitchingValueNote> {
        self.construction.switching_value_notes(&self.params)
    }

    pub(crate) fn run(
        &self,
        initial_state: Vec<f64>,
        nominals: Vec<f64>,
        breakpoints: Vec<f64>,
    ) -> OdeRun {
        OdeRun {
            relative_tolerance: self.config.relative_tolerance,
            absolute_tolerance: self.config.absolute_tolerance,
            nominals,
            initial_step: self.config.initial_step,
            t_start: self.config.t_start,
            t_end: self.config.t_end,
            initial_state,
            breakpoints,
            extension_order_limit: None,
        }
    }
}

/// A running or terminal objective term evaluated on the solver vector.
pub trait QuadratureIntegrand {
    /// Number of quadrature states the integrand adds.
    fn quadrature_count(&self) -> usize;

    /// Times inside `(t_start, t_end)`, ascending, at which the integrand
    /// changes form (a data knot): the integration grid contains each, so the
    /// quadrature is exact per segment for the declared interpolation.
    fn breakpoints(&self, t_start: f64, t_end: f64) -> Vec<f64>;

    /// Write each quadrature's rate at `(t, solver_y)` and its sensitivity
    /// rates given the solver-vector tangent of every parameter.
    fn rates_into(
        &self,
        t: f64,
        solver_y: &[f64],
        tangents: &[Vec<f64>],
        out: &mut [f64],
    ) -> Result<(), TrajectoryError>;
}

/// The forward variational system as a driven ODE.
struct ForwardOde {
    problem: Rc<TrajectoryProblem>,
    layout: SensitivityLayout,
    /// Runtime slot of each requested parameter.
    slots: Vec<usize>,
    integrand: Option<Rc<dyn QuadratureIntegrand>>,
}

impl ContinuousOde for ForwardOde {
    fn width(&self) -> usize {
        self.layout.width()
    }

    fn derivatives_into(&self, time: f64, state: &[f64], out: &mut [f64]) -> Result<(), String> {
        self.evaluate(time, state, out)
            .map_err(|error| error.to_string())
    }
}

impl ForwardOde {
    fn evaluate(&self, t: f64, state: &[f64], out: &mut [f64]) -> Result<(), TrajectoryError> {
        let problem = &self.problem;
        let runtime = &problem.runtime;
        let n = self.layout.states;
        let x = &state[..n];
        let lin = problem.linearization(t);
        runtime.eval_derivatives_and_sensitivities_into(
            lin,
            state,
            self.layout,
            &self.slots,
            out,
        )?;
        if let Some(integrand) = &self.integrand {
            let solver_y = runtime.full_solver_y(
                t,
                x,
                &problem.params,
                lin.settle.tol,
                lin.settle.max_iters,
            )?;
            let tangents = self.solver_tangents(t, state)?;
            integrand.rates_into(
                t,
                &solver_y,
                &tangents,
                &mut out[self.layout.quadrature_start()..],
            )?;
        }
        Ok(())
    }

    fn solver_tangents(&self, t: f64, state: &[f64]) -> Result<Vec<Vec<f64>>, TrajectoryError> {
        solver_tangents(&self.problem, t, &state[..self.layout.states], |j| {
            &state[self.layout.sensitivity(j)]
        })
    }
}

/// The full solver-vector tangent `dy/dp_j` (states and algebraics) of every
/// parameter at `(t, x)`, given the state sensitivity columns.
pub(crate) fn solver_tangents<'a>(
    problem: &TrajectoryProblem,
    t: f64,
    x: &[f64],
    sensitivity: impl Fn(usize) -> &'a [f64],
) -> Result<Vec<Vec<f64>>, TrajectoryError> {
    let lin = problem.linearization(t);
    problem
        .parameters()
        .iter()
        .enumerate()
        .map(|(j, parameter)| {
            let mut tangent = vec![0.0; problem.runtime.solver_count];
            problem.runtime.project_state_sensitivity_to_solver_y(
                lin,
                x,
                sensitivity(j),
                parameter.slot,
                &mut tangent,
            )?;
            Ok(tangent)
        })
        .collect()
}

/// A forward-sensitivity trajectory sampled on the requested output times.
#[derive(Debug, Clone)]
pub struct ForwardSensitivityTrajectory {
    pub times: Vec<f64>,
    /// Solver-variable names, states first.
    pub variables: Vec<String>,
    pub parameters: Vec<String>,
    /// `values[time][variable]`.
    pub values: Vec<Vec<f64>>,
    /// `sensitivities[time][variable * parameters + parameter] = dy/dp`.
    pub sensitivities: Vec<Vec<f64>>,
    /// Objective value and gradient when a quadrature integrand was supplied.
    pub quadratures: Vec<Vec<f64>>,
}

/// Integrate the forward variational system and sample it at `times`.
///
/// `times` must be sorted, inside the horizon, and distinct. Algebraic
/// variables and their sensitivities are recovered at every sample from the
/// state and its sensitivity through the projection, so the output covers every
/// solver variable and not only the integrated states.
pub fn forward_sensitivity_trajectory(
    problem: &Rc<TrajectoryProblem>,
    build: PluginBuilder<'_>,
    times: &[f64],
    integrand: Option<Rc<dyn QuadratureIntegrand>>,
) -> Result<ForwardSensitivityTrajectory, TrajectoryError> {
    let layout = problem.layout(
        integrand
            .as_ref()
            .map_or(0, |value| value.quadrature_count()),
    );
    let mut initial = vec![0.0; layout.width()];
    initial[..layout.states].copy_from_slice(&problem.y0[..layout.states]);
    for (j, column) in problem.initial_state_sensitivity.iter().enumerate() {
        initial[layout.sensitivity(j)].copy_from_slice(column);
    }
    let system = Rc::new(ForwardOde {
        problem: Rc::clone(problem),
        layout,
        slots: problem
            .parameters()
            .iter()
            .map(|parameter| parameter.slot)
            .collect(),
        integrand,
    });
    let breakpoints = system.integrand.as_ref().map_or_else(Vec::new, |value| {
        value.breakpoints(problem.config.t_start, problem.config.t_end)
    });
    let run = problem.run(
        initial.clone(),
        problem.nominals(layout.quadratures),
        breakpoints,
    );
    let mut trajectory = ForwardSensitivityTrajectory {
        times: Vec::with_capacity(times.len()),
        variables: problem.variable_names().to_vec(),
        parameters: problem
            .parameters()
            .iter()
            .map(|p| p.name.clone())
            .collect(),
        values: Vec::new(),
        sensitivities: Vec::new(),
        quadratures: Vec::new(),
    };
    let mut next = 0usize;
    let mut sampled = vec![0.0; layout.width()];
    let record = |trajectory: &mut ForwardSensitivityTrajectory,
                  t: f64,
                  state: &[f64]|
     -> Result<(), String> {
        record_sample(problem, layout, trajectory, t, state).map_err(|error| error.to_string())
    };
    if times
        .first()
        .is_some_and(|first| *first <= problem.config.t_start)
    {
        record(&mut trajectory, problem.config.t_start, &initial)
            .map_err(|reason| TrajectoryError::Integration(OdeDriveError::Observer(reason)))?;
        next = 1;
    }
    integrate_ode(system, &run, build, |interval| {
        while next < times.len() && times[next] <= interval.end_time() {
            let t = times[next];
            if t >= interval.end_time() {
                sampled.copy_from_slice(interval.end_state());
            } else {
                interval
                    .sample(t, &mut sampled)
                    .map_err(|error| error.to_string())?;
            }
            record(&mut trajectory, t, &sampled)?;
            next += 1;
        }
        Ok(())
    })?;
    Ok(trajectory)
}

fn record_sample(
    problem: &Rc<TrajectoryProblem>,
    layout: SensitivityLayout,
    trajectory: &mut ForwardSensitivityTrajectory,
    t: f64,
    state: &[f64],
) -> Result<(), TrajectoryError> {
    let runtime = &problem.runtime;
    let settle = problem.settle;
    let x = &state[..layout.states];
    let solver_y = runtime.full_solver_y(t, x, &problem.params, settle.tol, settle.max_iters)?;
    let tangents = solver_tangents(problem, t, x, |j| &state[layout.sensitivity(j)])?;
    let m = layout.parameters;
    let mut flat = vec![0.0; solver_y.len() * m];
    for (j, tangent) in tangents.iter().enumerate() {
        for (i, value) in tangent.iter().enumerate() {
            flat[i * m + j] = *value;
        }
    }
    trajectory.times.push(t);
    trajectory.values.push(solver_y);
    trajectory.sensitivities.push(flat);
    trajectory
        .quadratures
        .push(state[layout.quadrature_start()..].to_vec());
    Ok(())
}
