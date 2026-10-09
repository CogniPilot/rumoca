//! Gradients of trajectory objectives by forward and adjoint sensitivity.
//!
//! The objective is `J = ∫ g(y, t) dt + h(y(T))` over solver variables `y`
//! (states and algebraics). Two routes give the same `dJ/dp`:
//!
//! * forward: the variational equations of [`super::trajectory`] plus one
//!   quadrature per parameter, `dJ/dp_j = ∫ (∂g/∂y) Y_j dt + (∂h/∂y) Y_j(T)`,
//!   where `Y_j` is the solver-vector tangent of parameter `j`;
//! * adjoint: one forward solve that stores the trajectory, then one backward
//!   solve of
//!
//!   ```text
//!     λ' = -(F_x)ᵀ λ - (G_x)ᵀ,   λ(T) = (H_x)ᵀ,
//!     dJ/dp = ∫ (F_p)ᵀ λ + (G_p)ᵀ dt + (H_p)ᵀ + λ(t0)ᵀ dx(t0)/dp
//!   ```
//!
//!   whose cost does not grow with the parameter count. `F`, `G`, `H` are
//!   total derivatives through the algebraic projection: the cotangent of an
//!   algebraic variable is carried back by solving `g_zᵀ μ = -(f_zᵀ λ + c_z)`
//!   matrix-free, with the reverse sweeps of the derivative rows and the
//!   algebraic constraint rows.
//!
//! Memory and time: the adjoint stores the checkpoint policy's nodes (five for the
//! Dormand-Prince extension) of the state per accepted
//! forward step (the plugin's degree-four continuous extension is reproduced
//! exactly by interpolation through them), `5 n` values per step, against a
//! backward solve of `n + m` states. Storing every step is the simplest policy
//! with no recomputation; a hard bound refuses a trajectory too large to hold
//! rather than degrading silently.
//!
//! # References
//!
//! The adjoint system for differential-algebraic equations and its quadrature
//! for the parameter gradient: Y. Cao, S. Li, L. Petzold and R. Serban,
//! "Adjoint sensitivity analysis for differential-algebraic equations: the
//! adjoint DAE system and its numerical solution", SIAM Journal on Scientific
//! Computing 24(3):1076-1089, 2003, doi:10.1137/S1064827501380630.

use std::cell::RefCell;
use std::rc::Rc;

use rumoca_ir_solve as solve;

use crate::fmi_me::ode_driver::{ContinuousOde, OdeDriveError, OdeInterval, integrate_ode};
use crate::runtime::iterative_solve::{GmresConfig, gmres};
use crate::runtime::solve_ops::RuntimeSolveError;
use crate::runtime::solve_runtime::SolveRuntime;
use crate::runtime::trajectory::{
    PluginBuilder, QuadratureIntegrand, TrajectoryError, TrajectoryProblem,
    forward_sensitivity_trajectory, invalid_request,
};

/// Measured data, linearly interpolated between samples.
#[derive(Debug, Clone, PartialEq)]
pub struct DataSeries {
    times: Vec<f64>,
    values: Vec<f64>,
}

impl DataSeries {
    /// A series needs two or more finite samples at strictly increasing times.
    pub fn new(times: Vec<f64>, values: Vec<f64>) -> Result<Self, TrajectoryError> {
        if times.len() != values.len() || times.len() < 2 {
            return Err(invalid_request(
                "a data series needs at least two samples with one value per time",
            ));
        }
        if times.iter().chain(&values).any(|value| !value.is_finite()) {
            return Err(invalid_request("a data series holds a non-finite value"));
        }
        if times.windows(2).any(|pair| pair[1] <= pair[0]) {
            return Err(invalid_request("data times must be strictly increasing"));
        }
        Ok(Self { times, values })
    }

    /// The sample times.
    #[must_use]
    pub fn knots(&self) -> &[f64] {
        &self.times
    }

    #[must_use]
    pub fn first_time(&self) -> f64 {
        self.times[0]
    }

    #[must_use]
    pub fn last_time(&self) -> f64 {
        self.times[self.times.len() - 1]
    }

    /// The interpolant at `t`, which the caller proved lies inside the data.
    #[must_use]
    pub fn value_at(&self, t: f64) -> f64 {
        let upper = self
            .times
            .partition_point(|time| *time <= t)
            .clamp(1, self.times.len() - 1);
        let (t0, t1) = (self.times[upper - 1], self.times[upper]);
        let weight = ((t - t0) / (t1 - t0)).clamp(0.0, 1.0);
        self.values[upper - 1] + weight * (self.values[upper] - self.values[upper - 1])
    }
}

/// What a running term integrates.
#[derive(Debug, Clone, PartialEq)]
pub enum RunningKind {
    /// `weight * y(t)`.
    Value,
    /// `weight * (y(t) - data(t))^2`.
    SquaredError(DataSeries),
}

/// One running term `∫ ... dt` over a named solver variable.
#[derive(Debug, Clone, PartialEq)]
pub struct RunningTerm {
    pub variable: String,
    pub weight: f64,
    pub kind: RunningKind,
}

/// One terminal term `weight * y(T)` over a named solver variable.
#[derive(Debug, Clone, PartialEq)]
pub struct TerminalTerm {
    pub variable: String,
    pub weight: f64,
}

/// An objective `J = Σ running + Σ terminal`, by variable name.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct TrajectoryObjective {
    pub running: Vec<RunningTerm>,
    pub terminal: Vec<TerminalTerm>,
}

enum ResolvedKind {
    Value,
    SquaredError(DataSeries),
}

/// An objective whose variables are solver-vector indices.
pub struct ResolvedObjective {
    running: Vec<(usize, f64, ResolvedKind)>,
    terminal: Vec<(usize, f64)>,
    solver_count: usize,
    /// Parameters a forward gradient is accumulated for (0 for a value-only run).
    gradient_parameters: usize,
}

impl ResolvedObjective {
    /// Bind `objective` to the solver variables of `problem`.
    ///
    /// A variable that is not a solver variable (for example one eliminated as
    /// an alias and reconstructed after the solve) is refused, as are data that
    /// do not cover the horizon and a weight that is not finite.
    pub fn resolve(
        problem: &TrajectoryProblem,
        objective: &TrajectoryObjective,
        gradient_parameters: usize,
    ) -> Result<Self, TrajectoryError> {
        if objective.running.is_empty() && objective.terminal.is_empty() {
            return Err(invalid_request("the objective has no term"));
        }
        let runtime = problem.runtime();
        let index_of = |name: &str| {
            runtime.solver_variable_index(name).ok_or_else(|| {
                invalid_request(format!(
                    "objective variable `{name}` is not a solver variable (a state or solver \
                     algebraic); reference the underlying state or algebraic"
                ))
            })
        };
        let finite = |weight: f64| {
            weight
                .is_finite()
                .then_some(weight)
                .ok_or_else(|| invalid_request("an objective weight is not finite"))
        };
        let (t0, t1) = (problem.config.t_start, problem.config.t_end);
        let mut running = Vec::with_capacity(objective.running.len());
        for term in &objective.running {
            let kind = resolve_kind(term, (t0, t1))?;
            running.push((index_of(&term.variable)?, finite(term.weight)?, kind));
        }
        let mut terminal = Vec::with_capacity(objective.terminal.len());
        for term in &objective.terminal {
            terminal.push((index_of(&term.variable)?, finite(term.weight)?));
        }
        Ok(Self {
            running,
            terminal,
            solver_count: runtime.solver_count,
            gradient_parameters,
        })
    }

    /// The data knots strictly inside `(t0, t1)`, ascending: the integrand is
    /// smooth between them, so a grid that contains each integrates it exactly
    /// for the declared piecewise-linear data.
    fn data_knots(&self, t0: f64, t1: f64) -> Vec<f64> {
        let mut knots: Vec<f64> = self
            .running
            .iter()
            .filter_map(|(_, _, kind)| match kind {
                ResolvedKind::SquaredError(data) => Some(data.knots().iter().copied()),
                ResolvedKind::Value => None,
            })
            .flatten()
            .filter(|knot| *knot > t0 && *knot < t1)
            .collect();
        knots.sort_by(f64::total_cmp);
        knots.dedup();
        knots
    }

    /// `g(y, t)`.
    fn running_value(&self, t: f64, y: &[f64]) -> f64 {
        self.running
            .iter()
            .map(|(index, weight, kind)| match kind {
                ResolvedKind::Value => weight * y[*index],
                ResolvedKind::SquaredError(data) => {
                    let error = y[*index] - data.value_at(t);
                    weight * error * error
                }
            })
            .sum()
    }

    /// `∂g/∂y` at `(t, y)`.
    fn running_gradient(&self, t: f64, y: &[f64]) -> Vec<f64> {
        let mut gradient = vec![0.0; self.solver_count];
        for (index, weight, kind) in &self.running {
            gradient[*index] += match kind {
                ResolvedKind::Value => *weight,
                ResolvedKind::SquaredError(data) => 2.0 * weight * (y[*index] - data.value_at(t)),
            };
        }
        gradient
    }

    /// `h(y)`.
    fn terminal_value(&self, y: &[f64]) -> f64 {
        self.terminal
            .iter()
            .map(|(index, weight)| weight * y[*index])
            .sum()
    }

    /// `∂h/∂y`.
    fn terminal_gradient(&self) -> Vec<f64> {
        let mut gradient = vec![0.0; self.solver_count];
        for (index, weight) in &self.terminal {
            gradient[*index] += weight;
        }
        gradient
    }
}

impl QuadratureIntegrand for ResolvedObjective {
    fn quadrature_count(&self) -> usize {
        1 + self.gradient_parameters
    }

    fn breakpoints(&self, t_start: f64, t_end: f64) -> Vec<f64> {
        self.data_knots(t_start, t_end)
    }

    fn rates_into(
        &self,
        t: f64,
        solver_y: &[f64],
        tangents: &[Vec<f64>],
        out: &mut [f64],
    ) -> Result<(), TrajectoryError> {
        out[0] = self.running_value(t, solver_y);
        if tangents.is_empty() {
            return Ok(());
        }
        let gradient = self.running_gradient(t, solver_y);
        for (slot, tangent) in out[1..].iter_mut().zip(tangents) {
            *slot = gradient.iter().zip(tangent).map(|(c, y)| c * y).sum();
        }
        Ok(())
    }
}

/// An objective value and its gradient with respect to the parameters.
#[derive(Debug, Clone)]
pub struct ObjectiveGradient {
    pub value: f64,
    pub parameters: Vec<String>,
    pub gradient: Vec<f64>,
}

/// Forward-sensitivity gradient: the variational system plus one quadrature
/// per parameter, read at the end of the horizon.
pub fn forward_objective_gradient(
    problem: &Rc<TrajectoryProblem>,
    build: PluginBuilder<'_>,
    objective: &TrajectoryObjective,
) -> Result<ObjectiveGradient, TrajectoryError> {
    let m = problem.parameters().len();
    let resolved = Rc::new(ResolvedObjective::resolve(problem, objective, m)?);
    let trajectory = forward_sensitivity_trajectory(
        problem,
        build,
        &[problem.config.t_end],
        Some(Rc::clone(&resolved) as Rc<dyn QuadratureIntegrand>),
    )?;
    let (Some(quadratures), Some(solver_y), Some(sensitivities)) = (
        trajectory.quadratures.last(),
        trajectory.values.last(),
        trajectory.sensitivities.last(),
    ) else {
        return Err(invalid_request("the run produced no end-of-horizon sample"));
    };
    let terminal = resolved.terminal_gradient();
    let gradient = (0..m)
        .map(|j| {
            let terminal_rate: f64 = terminal
                .iter()
                .enumerate()
                .map(|(i, c)| c * sensitivities[i * m + j])
                .sum();
            quadratures[1 + j] + terminal_rate
        })
        .collect();
    Ok(ObjectiveGradient {
        value: quadratures[0] + resolved.terminal_value(solver_y),
        parameters: trajectory.parameters.clone(),
        gradient,
    })
}

/// The accepted forward trajectory: the initial state, then the later nodes of
/// every step (node 0 of a step is the end node of the previous one).
struct Checkpoints {
    states: usize,
    /// Step breakpoints `t_0 < t_1 < ... < t_K`.
    times: Vec<f64>,
    /// The state at `t_0`.
    initial: Vec<f64>,
    /// `samples[(k * (NODES - 1) + node - 1) * states ..][..states]` is node
    /// `node >= 1` of step `k`.
    samples: Vec<f64>,
    budget_values: usize,
    /// The checkpoint contract of the construction (SOLVE-C76).
    policy: solve::CheckpointPolicy,
}

impl Checkpoints {
    fn new(
        initial: &[f64],
        t_start: f64,
        budget_bytes: u64,
        policy: solve::CheckpointPolicy,
    ) -> Self {
        Self {
            policy,
            states: initial.len(),
            times: vec![t_start],
            initial: initial.to_vec(),
            samples: Vec::new(),
            budget_values: solve::CheckpointPolicy::budget_values(budget_bytes),
        }
    }

    /// Refuse a step the memory budget cannot hold.
    fn reserve_step(&self) -> Result<(), TrajectoryError> {
        let needed =
            self.initial.len() + self.samples.len() + (self.policy.nodes - 1) * self.states;
        solve::CheckpointPolicy::admit_stored_values(needed, self.budget_values)?;
        Ok(())
    }

    /// Node `node` of step `step`.
    fn node(&self, step: usize, node: usize) -> &[f64] {
        if node == 0 {
            return match step {
                0 => &self.initial,
                _ => self.node(step - 1, self.policy.nodes - 1),
            };
        }
        let start = (step * (self.policy.nodes - 1) + node - 1) * self.states;
        &self.samples[start..start + self.states]
    }

    /// The state at `t`, interpolated through the stored nodes.
    fn state_at(&self, t: f64, out: &mut [f64]) {
        let steps = self.times.len() - 1;
        let step = (self.times.partition_point(|time| *time < t).max(1) - 1).min(steps - 1);
        let (t0, t1) = (self.times[step], self.times[step + 1]);
        let theta = ((t - t0) / (t1 - t0)).clamp(0.0, 1.0);
        out.fill(0.0);
        for node in 0..self.policy.nodes {
            let weight = lagrange_weight(self.policy, node, theta);
            for (slot, value) in out.iter_mut().zip(self.node(step, node)) {
                *slot += weight * value;
            }
        }
    }
}

/// The Lagrange basis polynomial of node `k` of `policy`, evaluated at `theta`.
fn lagrange_weight(policy: solve::CheckpointPolicy, k: usize, theta: f64) -> f64 {
    (0..policy.nodes)
        .filter(|j| *j != k)
        .map(|j| {
            let (nk, nj) = (policy.node_fraction(k), policy.node_fraction(j));
            (theta - nj) / (nk - nj)
        })
        .product()
}

/// The states and the objective value, integrated forward to store the path.
struct StateOde {
    problem: Rc<TrajectoryProblem>,
    objective: Rc<ResolvedObjective>,
}

impl ContinuousOde for StateOde {
    fn width(&self) -> usize {
        self.problem.runtime.state_count + 1
    }

    fn derivatives_into(&self, t: f64, state: &[f64], out: &mut [f64]) -> Result<(), String> {
        self.evaluate(t, state, out)
            .map_err(|error| error.to_string())
    }
}

impl StateOde {
    fn evaluate(&self, t: f64, state: &[f64], out: &mut [f64]) -> Result<(), RuntimeSolveError> {
        let n = self.problem.runtime.state_count;
        let settle = self.problem.settle;
        let runtime = &self.problem.runtime;
        runtime.eval_state_derivatives_into(
            t,
            &state[..n],
            &self.problem.params,
            settle.tol,
            settle.max_iters,
            &mut out[..n],
        )?;
        let y = runtime.full_solver_y(
            t,
            &state[..n],
            &self.problem.params,
            settle.tol,
            settle.max_iters,
        )?;
        out[n] = self.objective.running_value(t, &y);
        Ok(())
    }
}

/// The adjoint system integrated in reversed time `τ = T - t`.
struct AdjointOde {
    problem: Rc<TrajectoryProblem>,
    objective: Rc<ResolvedObjective>,
    path: Rc<Checkpoints>,
    scratch: RefCell<Vec<f64>>,
}

impl ContinuousOde for AdjointOde {
    fn width(&self) -> usize {
        self.problem.runtime.state_count + self.problem.parameters().len()
    }

    fn derivatives_into(&self, tau: f64, state: &[f64], out: &mut [f64]) -> Result<(), String> {
        self.evaluate(tau, state, out)
            .map_err(|error| error.to_string())
    }
}

impl AdjointOde {
    fn evaluate(&self, tau: f64, state: &[f64], out: &mut [f64]) -> Result<(), TrajectoryError> {
        let problem = &self.problem;
        let n = problem.runtime.state_count;
        let t = (problem.config.t_end - tau).clamp(problem.config.t_start, problem.config.t_end);
        let mut x = self.scratch.borrow_mut();
        self.path.state_at(t, &mut x);
        let settle = problem.settle;
        let y =
            problem
                .runtime
                .full_solver_y(t, &x, &problem.params, settle.tol, settle.max_iters)?;
        let c = self.objective.running_gradient(t, &y);
        let (state_cotangent, parameter_cotangent) =
            total_cotangent(&problem.runtime, t, &y, &problem.params, &state[..n], &c)?;
        out[..n].copy_from_slice(&state_cotangent);
        for (slot, parameter) in out[n..].iter_mut().zip(problem.parameters()) {
            *slot = parameter_cotangent[parameter.slot];
        }
        Ok(())
    }
}

/// The total cotangent `(F_xᵀ λ + c_x, F_pᵀ λ + c_p)` of the reduced ODE.
///
/// `c` is a cotangent over the full solver vector (states and algebraics). The
/// algebraic part of `[λ; μ]` is chosen so that the algebraic block of the
/// transposed residual Jacobian vanishes, `f_zᵀ λ + g_zᵀ μ + c_z = 0`; the
/// remaining state and parameter blocks are then the total derivatives through
/// the projection.
pub(crate) fn total_cotangent(
    runtime: &SolveRuntime,
    t: f64,
    y: &[f64],
    params: &[f64],
    lambda: &[f64],
    c: &[f64],
) -> Result<(Vec<f64>, Vec<f64>), TrajectoryError> {
    let n = runtime.state_count;
    let solver = runtime.solver_count;
    let width = solver + runtime.model.problem.layout.p_scalars();
    let mut full = vec![0.0; solver];
    full[..n].copy_from_slice(lambda);
    let mut out = vec![0.0; width];
    if solver > n {
        runtime.apply_steady_residual_transpose(t, y, params, &full, &mut out)?;
        let rhs: Vec<f64> = (n..solver).map(|k| -(out[k] + c[k])).collect();
        let mut basis = vec![0.0; solver];
        let mut transposed = vec![0.0; width];
        let multipliers = gmres(
            |v: &[f64], result: &mut [f64]| -> Result<(), RuntimeSolveError> {
                basis[n..].copy_from_slice(v);
                runtime.apply_steady_residual_transpose(t, y, params, &basis, &mut transposed)?;
                result.copy_from_slice(&transposed[n..solver]);
                Ok(())
            },
            &rhs,
            GmresConfig::default(),
        )
        .map_err(|error| TrajectoryError::LinearSolve {
            reason: format!("the algebraic cotangent solve failed: {error}"),
        })?;
        full[n..].copy_from_slice(&multipliers);
    }
    runtime.apply_steady_residual_transpose(t, y, params, &full, &mut out)?;
    let state_cotangent = (0..n).map(|i| out[i] + c[i]).collect();
    Ok((state_cotangent, out[solver..].to_vec()))
}

/// Adjoint-sensitivity gradient: a forward solve that stores the trajectory,
/// then one backward solve of the adjoint system and its parameter quadrature.
pub fn adjoint_objective_gradient(
    problem: &Rc<TrajectoryProblem>,
    build: PluginBuilder<'_>,
    objective: &TrajectoryObjective,
) -> Result<ObjectiveGradient, TrajectoryError> {
    let resolved = Rc::new(ResolvedObjective::resolve(problem, objective, 0)?);
    let n = problem.runtime.state_count;
    let m = problem.parameters().len();
    let settle = problem.settle;
    let policy = problem.construction.checkpoint_policy();

    // Forward: store the path and integrate the running objective, with every
    // data knot a step end.
    let mut path = Checkpoints::new(
        &problem.y0[..n],
        problem.config.t_start,
        problem.config.checkpoint_budget_bytes,
        policy,
    );
    let mut initial = vec![0.0; n + 1];
    initial[..n].copy_from_slice(&problem.y0[..n]);
    let mut nominals = problem.nominals(0);
    nominals.truncate(n);
    nominals.push(1.0);
    let forward = Rc::new(StateOde {
        problem: Rc::clone(problem),
        objective: Rc::clone(&resolved),
    });
    let knots = resolved.data_knots(problem.config.t_start, problem.config.t_end);
    let mut run = problem.run(initial, nominals, knots.clone());
    // The plugin declares its continuous-extension order before the first step,
    // and the policy is proved against it once, here.
    run.extension_order_limit = Some(policy.degree());
    let mut end_state = vec![0.0; n + 1];
    let mut node = vec![0.0; n + 1];
    // A typed refusal raised inside the observer is kept and returned as itself.
    let mut refusal = None;
    let outcome = integrate_ode(forward, &run, build, |interval| {
        store_interval(&mut path, interval, &mut node).map_err(|error| {
            let message = error.to_string();
            refusal = Some(error);
            message
        })?;
        end_state.copy_from_slice(interval.end_state());
        Ok(())
    });
    if let Some(error) = refusal {
        return Err(error);
    }
    outcome.map_err(|error| checkpoint_contract(policy, error))?;
    if path.times.len() < 2 {
        return Err(invalid_request("the horizon holds no integration step"));
    }
    let x_end = end_state[..n].to_vec();
    let y_end = problem.runtime.full_solver_y(
        config_end(problem),
        &x_end,
        &problem.params,
        settle.tol,
        settle.max_iters,
    )?;

    // Terminal cotangent: lambda(T) and the explicit-parameter part of h.
    let (lambda_end, terminal_parameter) = total_cotangent(
        &problem.runtime,
        config_end(problem),
        &y_end,
        &problem.params,
        &vec![0.0; n],
        &resolved.terminal_gradient(),
    )?;

    // Backward: lambda and the parameter quadrature in tau = T - t.
    let mut adjoint_initial = vec![0.0; n + m];
    adjoint_initial[..n].copy_from_slice(&lambda_end);
    let adjoint = Rc::new(AdjointOde {
        problem: Rc::clone(problem),
        objective: Rc::clone(&resolved),
        path: Rc::new(path),
        scratch: RefCell::new(vec![0.0; n]),
    });
    // The knots in reversed time, ascending.
    let reversed: Vec<f64> = knots
        .iter()
        .rev()
        .map(|knot| problem.config.t_end - knot)
        .collect();
    let mut backward = problem.run(adjoint_initial, vec![1.0; n + m], reversed);
    backward.t_start = 0.0;
    backward.t_end = problem.config.t_end - problem.config.t_start;
    let mut final_state = vec![0.0; n + m];
    integrate_ode(adjoint, &backward, build, |interval| {
        final_state.copy_from_slice(interval.end_state());
        Ok(())
    })?;

    let gradient = adjoint_parameter_gradient(problem, &final_state, &terminal_parameter);
    Ok(ObjectiveGradient {
        value: end_state[n] + resolved.terminal_value(&y_end),
        parameters: problem
            .parameters()
            .iter()
            .map(|p| p.name.clone())
            .collect(),
        gradient,
    })
}

/// `dJ/dp_j` from the backward quadrature, the explicit terminal parameter
/// term, and the initial-sensitivity term `lambda(t0)^T dx(t0)/dp_j`.
fn adjoint_parameter_gradient(
    problem: &TrajectoryProblem,
    final_state: &[f64],
    terminal_parameter: &[f64],
) -> Vec<f64> {
    let n = problem.runtime.state_count;
    problem
        .parameters()
        .iter()
        .enumerate()
        .map(|(j, parameter)| {
            let initial_term: f64 = final_state[..n]
                .iter()
                .zip(&problem.initial_state_sensitivity[j])
                .map(|(lambda, s)| lambda * s)
                .sum();
            final_state[n + j] + terminal_parameter[parameter.slot] + initial_term
        })
        .collect()
}

/// Store the nodes of one accepted step after checking the memory budget (the
/// extension order was proved before the first step).
fn store_interval(
    path: &mut Checkpoints,
    interval: &OdeInterval<'_>,
    node: &mut [f64],
) -> Result<(), TrajectoryError> {
    path.reserve_step()?;
    let (t0, t1) = (interval.start_time(), interval.end_time());
    for k in 1..path.policy.nodes {
        if k == path.policy.nodes - 1 {
            path.samples
                .extend_from_slice(&interval.end_state()[..path.states]);
        } else {
            interval.sample(t0 + (t1 - t0) * path.policy.node_fraction(k), node)?;
            path.samples.extend_from_slice(&node[..path.states]);
        }
    }
    path.times.push(t1);
    Ok(())
}

/// A plugin refused for its declared extension order is the policy's typed
/// refusal; any other drive failure is an integration failure.
fn checkpoint_contract(policy: solve::CheckpointPolicy, error: OdeDriveError) -> TrajectoryError {
    match error {
        OdeDriveError::ExtensionOrder { declared, .. } => {
            match policy.admit_extension_order(declared) {
                Err(refusal) => TrajectoryError::Refused(refusal),
                Ok(()) => TrajectoryError::Integration(OdeDriveError::ExtensionOrder {
                    declared,
                    limit: policy.degree(),
                }),
            }
        }
        other => TrajectoryError::Integration(other),
    }
}

fn config_end(problem: &TrajectoryProblem) -> f64 {
    problem.config.t_end
}

/// One running term's kind, with measured data proved to cover the horizon.
fn resolve_kind(term: &RunningTerm, (t0, t1): (f64, f64)) -> Result<ResolvedKind, TrajectoryError> {
    let RunningKind::SquaredError(data) = &term.kind else {
        return Ok(ResolvedKind::Value);
    };
    if data.first_time() > t0 || data.last_time() < t1 {
        return Err(invalid_request(format!(
            "the data for `{}` span [{}, {}] but the horizon is [{t0}, {t1}]",
            term.variable,
            data.first_time(),
            data.last_time()
        )));
    }
    Ok(ResolvedKind::SquaredError(data.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fmi_me::ode_driver::OdeRun;
    use crate::fmi_me::{
        MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeIntegrationError,
        MeIntegratorBackend, MeNumericalSetup, MeStepCandidate,
    };

    /// `y' = 1`: the exact solution is a polynomial every node set reproduces.
    struct Ramp;

    impl ContinuousOde for Ramp {
        fn width(&self) -> usize {
            1
        }

        fn derivatives_into(
            &self,
            _time: f64,
            _state: &[f64],
            out: &mut [f64],
        ) -> Result<(), String> {
            out[0] = 1.0;
            Ok(())
        }
    }

    /// A one-step explicit Euler plugin whose continuous extension is the
    /// straight line, declaring `order` for it.
    struct Line {
        handle: Option<MeDerivativeHandle>,
        order: u32,
        step: Option<(f64, f64, f64, f64)>,
    }

    impl MeIntegratorBackend for Line {
        fn continuous_extension_order(&self) -> Option<u32> {
            Some(self.order)
        }

        fn initialize(
            &mut self,
            point: &MeContinuousPoint,
            derivatives: MeDerivativeHandle,
        ) -> Result<(), MeIntegrationError> {
            self.truncate_reset(point)?;
            self.handle = Some(derivatives);
            Ok(())
        }

        fn advance(
            &mut self,
            request: &MeAdvanceRequest,
        ) -> Result<MeStepCandidate, MeIntegrationError> {
            let Some(handle) = &self.handle else {
                return Err(MeIntegrationError::contract("no handle"));
            };
            let (t0, y0) = (request.current().time(), request.current().states()[0]);
            let t1 = request.latest_accepted_time();
            let slope = handle.derivatives(t0, &[y0])?[0];
            let y1 = y0 + (t1 - t0) * slope;
            self.step = Some((t0, y0, t1, y1));
            Ok(MeStepCandidate::new(t1, vec![y1], self.order))
        }

        fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
            let Some((t0, y0, t1, y1)) = self.step else {
                return Err(MeIntegrationError::contract("no step"));
            };
            states[0] = y0 + (y1 - y0) * (time - t0) / (t1 - t0);
            Ok(())
        }

        fn truncate_reset(&mut self, _point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
            self.step = None;
            Ok(())
        }
    }

    fn run() -> OdeRun {
        OdeRun {
            relative_tolerance: 1.0e-6,
            absolute_tolerance: 1.0e-9,
            nominals: vec![1.0],
            initial_step: None,
            t_start: 0.0,
            t_end: 1.0,
            initial_state: vec![0.0],
            breakpoints: vec![0.5],
            extension_order_limit: None,
        }
    }

    fn build(order: u32) -> impl Fn(MeNumericalSetup) -> Box<dyn MeIntegratorBackend> {
        move |_| {
            Box::new(Line {
                handle: None,
                order,
                step: None,
            })
        }
    }

    #[test]
    fn stored_nodes_reproduce_the_path_and_the_first_node_is_stored_once() {
        let mut path = Checkpoints::new(&[0.0], 0.0, 1 << 20, solve::CheckpointPolicy::FIVE_NODE);
        let mut node = vec![0.0];
        let mut failure = None;
        integrate_ode(Rc::new(Ramp), &run(), &build(4), |interval| {
            failure = store_interval(&mut path, interval, &mut node).err();
            Ok(())
        })
        .expect("ramp integrates");
        assert!(failure.is_none());
        // Two steps (the breakpoint ends the first), four stored nodes each,
        // plus the one initial state.
        assert_eq!(path.times, [0.0, 0.5, 1.0]);
        assert_eq!(path.samples.len(), 8);
        assert_eq!(path.initial.len(), 1);
        let mut out = [0.0];
        for t in [0.0, 0.1, 0.5, 0.7, 1.0] {
            path.state_at(t, &mut out);
            assert!((out[0] - t).abs() < 1.0e-12, "t = {t}: {}", out[0]);
        }
    }

    #[test]
    fn a_plugin_whose_extension_exceeds_the_node_polynomial_is_refused_before_any_step() {
        let policy = solve::CheckpointPolicy::FIVE_NODE;
        let mut limited = run();
        limited.extension_order_limit = Some(policy.degree());
        let mut steps = 0;
        let error = integrate_ode(Rc::new(Ramp), &limited, &build(6), |_| {
            steps += 1;
            Ok(())
        })
        .expect_err("the policy refuses the plugin");
        assert_eq!(steps, 0);
        assert!(matches!(
            checkpoint_contract(policy, error),
            TrajectoryError::Refused(solve::SensitivityRefusal::CheckpointContract {
                order: 6,
                ..
            })
        ));
    }

    #[test]
    fn a_policy_of_fewer_nodes_binds_the_plugin_and_reproduces_a_line() {
        let three = solve::CheckpointPolicy::new(3).expect("three nodes");
        let mut limited = run();
        limited.extension_order_limit = Some(three.degree());
        let error = integrate_ode(Rc::new(Ramp), &limited, &build(4), |_| Ok(()))
            .expect_err("order four exceeds degree two");
        assert!(matches!(
            checkpoint_contract(three, error),
            TrajectoryError::Refused(solve::SensitivityRefusal::CheckpointContract {
                nodes: 3,
                ..
            })
        ));
        let mut path = Checkpoints::new(&[0.0], 0.0, 1 << 20, three);
        let mut node = vec![0.0];
        integrate_ode(Rc::new(Ramp), &limited, &build(2), |interval| {
            store_interval(&mut path, interval, &mut node).map_err(|e| e.to_string())
        })
        .expect("a degree-two extension fits three nodes");
        assert_eq!(path.samples.len(), 4);
        let mut out = [0.0];
        path.state_at(0.7, &mut out);
        assert!((out[0] - 0.7).abs() < 1.0e-12);
    }

    #[test]
    fn a_step_the_budget_cannot_hold_is_refused_before_it_is_stored() {
        let mut path = Checkpoints::new(&[0.0], 0.0, 16, solve::CheckpointPolicy::FIVE_NODE);
        let mut node = vec![0.0];
        let mut refusal = None;
        integrate_ode(Rc::new(Ramp), &run(), &build(4), |interval| {
            refusal = store_interval(&mut path, interval, &mut node).err();
            Ok(())
        })
        .expect("the observer swallows the refusal");
        assert!(matches!(
            refusal,
            Some(TrajectoryError::Refused(
                solve::SensitivityRefusal::CheckpointCapacity { budget: 2, .. }
            ))
        ));
    }
}
