use std::time::Instant;
use std::{
    cell::RefCell,
    sync::{
        Arc,
        atomic::{AtomicBool, AtomicU64, Ordering},
    },
};

use diffsol::{
    MatrixCommon, OdeBuilder, OdeEquationsImplicit, OdeSolverProblem, Vector as _, VectorHost,
};
use rumoca_eval_solve::{
    self as solve_eval, PreparedComputeBlock, PreparedScalarProgramBlock, RowEvalContext,
};
use rumoca_ir_solve as solve;
use rumoca_solver::{
    AlgebraicProjectionModel, ImplicitProjectionModel, PreparedMassMatrix, RuntimeSolveError,
    SimOptions, SolveRuntime,
};

use crate::{
    AlgebraicWarmStart, EVENT_UPDATE_MAX_ITERS, Matrix, RuntimeParameters, Scalar, SimError,
    Vector, me::DiffsolMeHost,
};

#[derive(Debug, Default)]
pub(crate) struct BdfEvalCounters {
    rhs_calls: AtomicU64,
    jacobian_vector_calls: AtomicU64,
    root_calls: AtomicU64,
    rhs_nanos: AtomicU64,
    jacobian_vector_nanos: AtomicU64,
    root_nanos: AtomicU64,
}

impl BdfEvalCounters {
    fn rhs(&self, elapsed_nanos: u64) {
        self.rhs_calls.fetch_add(1, Ordering::Relaxed);
        self.rhs_nanos.fetch_add(elapsed_nanos, Ordering::Relaxed);
    }

    fn jacobian_vector(&self, elapsed_nanos: u64) {
        self.jacobian_vector_calls.fetch_add(1, Ordering::Relaxed);
        self.jacobian_vector_nanos
            .fetch_add(elapsed_nanos, Ordering::Relaxed);
    }

    fn root(&self, elapsed_nanos: u64) {
        self.root_calls.fetch_add(1, Ordering::Relaxed);
        self.root_nanos.fetch_add(elapsed_nanos, Ordering::Relaxed);
    }

    fn snapshot(&self) -> BdfEvalCounterSnapshot {
        BdfEvalCounterSnapshot {
            rhs_calls: self.rhs_calls.load(Ordering::Relaxed),
            jacobian_vector_calls: self.jacobian_vector_calls.load(Ordering::Relaxed),
            root_calls: self.root_calls.load(Ordering::Relaxed),
            rhs_nanos: self.rhs_nanos.load(Ordering::Relaxed),
            jacobian_vector_nanos: self.jacobian_vector_nanos.load(Ordering::Relaxed),
            root_nanos: self.root_nanos.load(Ordering::Relaxed),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct BdfEvalCounterSnapshot {
    pub(crate) rhs_calls: u64,
    pub(crate) jacobian_vector_calls: u64,
    pub(crate) root_calls: u64,
    pub(crate) rhs_nanos: u64,
    pub(crate) jacobian_vector_nanos: u64,
    pub(crate) root_nanos: u64,
}

pub(crate) struct StateOdeProblemInput {
    pub(crate) runtime_params: RuntimeParameters,
    pub(crate) algebraic_warm_start: AlgebraicWarmStart,
    pub(crate) t_start: f64,
    pub(crate) initial_state: Vec<f64>,
    pub(crate) eval_counters: Option<Arc<BdfEvalCounters>>,
    pub(crate) rhs_runtime: Arc<SolveRuntime>,
}

impl StateOdeProblemInput {
    pub(crate) fn new(
        runtime_params: RuntimeParameters,
        algebraic_warm_start: AlgebraicWarmStart,
        t_start: f64,
        initial_state: Vec<f64>,
        eval_counters: Option<Arc<BdfEvalCounters>>,
        rhs_runtime: Arc<SolveRuntime>,
    ) -> Self {
        Self {
            runtime_params,
            algebraic_warm_start,
            t_start,
            initial_state,
            eval_counters,
            rhs_runtime,
        }
    }
}

fn new_bdf_eval_counters() -> Option<Arc<BdfEvalCounters>> {
    trace_bdf_eval_counts().then(|| Arc::new(BdfEvalCounters::default()))
}

pub(crate) fn state_ode_problem_input(
    runtime_params: &RuntimeParameters,
    algebraic_warm_start: &AlgebraicWarmStart,
    t_start: f64,
    initial_state: &[f64],
    rhs_runtime: &Arc<SolveRuntime>,
) -> (StateOdeProblemInput, Option<Arc<BdfEvalCounters>>) {
    let eval_counters = new_bdf_eval_counters();
    let input = StateOdeProblemInput::new(
        runtime_params.clone(),
        algebraic_warm_start.clone(),
        t_start,
        initial_state.to_vec(),
        eval_counters.clone(),
        rhs_runtime.clone(),
    );
    (input, eval_counters)
}

pub(crate) fn trace_bdf_eval_counter_snapshot(
    label: &str,
    counters: &Option<Arc<BdfEvalCounters>>,
) {
    let Some(counters) = counters else {
        return;
    };
    let snapshot = counters.snapshot();
    let total_nanos = snapshot
        .rhs_nanos
        .saturating_add(snapshot.jacobian_vector_nanos)
        .saturating_add(snapshot.root_nanos);
    tracing::debug!(
        target: "rumoca_solver_diffsol::bdf_eval",
        "{label}: rhs={} ({:.3}ms) jac_v={} ({:.3}ms) roots={} ({:.3}ms) total_eval={:.3}ms",
        snapshot.rhs_calls,
        nanos_to_ms(snapshot.rhs_nanos),
        snapshot.jacobian_vector_calls,
        nanos_to_ms(snapshot.jacobian_vector_nanos),
        snapshot.root_calls,
        nanos_to_ms(snapshot.root_nanos),
        nanos_to_ms(total_nanos)
    );
}

fn nanos_to_ms(nanos: u64) -> f64 {
    nanos as f64 / 1.0e6
}

fn elapsed_nanos_u64(start: Instant) -> u64 {
    start.elapsed().as_nanos().min(u128::from(u64::MAX)) as u64
}

fn trace_bdf_eval_counts() -> bool {
    tracing::enabled!(target: "rumoca_solver_diffsol::bdf_eval", tracing::Level::DEBUG)
}

pub(crate) struct OdeModel {
    state_count: usize,
    mass_matrix: PreparedMassMatrix,
    implicit_rhs: PreparedComputeBlock,
    implicit_scalar_rhs: PreparedScalarProgramBlock,
    initial_residual: PreparedComputeBlock,
    initial_residual_jacobian_v: PreparedComputeBlock,
    initial_scalar_residual: PreparedScalarProgramBlock,
    pub(crate) initial_targets: Vec<Option<solve::ScalarSlot>>,
    implicit_jacobian_v: PreparedComputeBlock,
    implicit_scalar_jacobian_v: PreparedScalarProgramBlock,
    #[cfg(test)]
    #[allow(dead_code)] // Used only by the frozen-driver comparison scaffold.
    pub(crate) root_conditions: PreparedScalarProgramBlock,
    pub(crate) implicit_targets: Vec<Option<solve::ScalarSlot>>,
    algebraic_projection_plan: solve::AlgebraicProjectionPlan,
    solver_names: Vec<String>,
    solver_scales: Vec<f64>,
    pub(crate) external_tables: solve::ExternalTables,
    pub(crate) runtime_state: solve_eval::SimulationRuntimeState,
}

impl OdeModel {
    pub(crate) fn new(model: &solve::SolveModel) -> Result<Self, SimError> {
        Ok(Self {
            state_count: model.state_scalar_count(),
            mass_matrix: PreparedMassMatrix::new(
                &model.artifacts.continuous.mass_matrix,
                model.state_scalar_count(),
            )?,
            implicit_rhs: PreparedComputeBlock::new_with_label(
                &model.problem.continuous.implicit_rhs,
                "ode_implicit_rhs",
            )?,
            implicit_scalar_rhs: PreparedScalarProgramBlock::new(
                solve_eval::to_scalar_program_block(&model.problem.continuous.implicit_rhs)?,
            )?,
            initial_residual: PreparedComputeBlock::new_with_label(
                &model.problem.initialization.residual,
                "ode_initial_residual",
            )?,
            initial_residual_jacobian_v: PreparedComputeBlock::new_with_label(
                &model.artifacts.initialization.residual_jacobian_v,
                "ode_initial_residual_jacobian_v",
            )?,
            initial_scalar_residual: PreparedScalarProgramBlock::new(
                solve_eval::to_scalar_program_block(&model.problem.initialization.residual)?,
            )?,
            initial_targets: model.problem.initialization.row_targets.clone(),
            implicit_jacobian_v: PreparedComputeBlock::new_with_label(
                &model.artifacts.continuous.implicit_jacobian_v,
                "ode_implicit_jacobian_v",
            )?,
            implicit_scalar_jacobian_v: PreparedScalarProgramBlock::new(
                solve_eval::to_scalar_program_block(
                    &model.artifacts.continuous.implicit_jacobian_v,
                )?,
            )?,
            #[cfg(test)]
            root_conditions: PreparedScalarProgramBlock::new(
                model.problem.events.root_conditions.clone(),
            )?,
            implicit_targets: model.problem.continuous.implicit_row_targets.clone(),
            algebraic_projection_plan: model.problem.continuous.algebraic_projection_plan.clone(),
            solver_names: model.problem.solve_layout.solver_maps.names.clone(),
            solver_scales: (0..model.solver_scalar_count())
                .map(|index| model.solver_variable_scale(index))
                .collect(),
            external_tables: model.external_tables.clone(),
            runtime_state: solve_eval::SimulationRuntimeState::new(),
        })
    }

    pub(crate) fn state_count_for_projection(&self) -> usize {
        self.state_count
    }

    pub(crate) fn solve_state_mass(&self, rhs: &[f64]) -> Result<Vec<f64>, RuntimeSolveError> {
        self.mass_matrix.solve(rhs)
    }

    fn prepared_mass_matrix(&self) -> &PreparedMassMatrix {
        &self.mass_matrix
    }

    pub(crate) fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), SimError> {
        self.initial_residual
            .eval_with_context(y, p, t, self.row_eval_context(None), out)
            .map_err(|err| SimError::SolveIr(err.to_string()))
    }

    pub(crate) fn initial_residual_len(&self) -> usize {
        self.initial_residual.len()
    }

    pub(crate) fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), SimError> {
        self.initial_residual_jacobian_v
            .eval_with_context(y, p, t, self.row_eval_context(Some(v)), out)
            .map_err(|err| SimError::SolveIr(err.to_string()))
    }

    pub(crate) fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), SimError> {
        self.implicit_rhs
            .eval_with_context(y, p, t, self.row_eval_context(None), out)
            .map_err(|err| SimError::SolveIr(err.to_string()))
    }

    pub(crate) fn eval_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), SimError> {
        self.implicit_jacobian_v
            .eval_with_context(y, p, t, self.row_eval_context(Some(v)), out)
            .map_err(|err| SimError::SolveIr(err.to_string()))
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn eval_roots(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), SimError> {
        if self.root_conditions.is_empty() {
            if let Some(first) = out.first_mut() {
                *first = 1.0;
            }
            return Ok(());
        }
        self.root_conditions
            .eval_with_context(y, p, t, self.row_eval_context(None), out)
            .map_err(|err| SimError::SolveIr(err.to_string()))
    }

    pub(crate) fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        let solve::ScalarSlot::Y { index, .. } = self.implicit_targets.get(row_idx).copied()??
        else {
            return None;
        };
        self.solver_names.get(index).map(String::as_str)
    }

    fn row_eval_context<'a>(&'a self, seed: Option<&'a [f64]>) -> RowEvalContext<'a> {
        RowEvalContext {
            seed,
            external_tables: Some(self.external_tables.as_slice()),
            runtime_state: Some(&self.runtime_state),
        }
    }
}

impl ImplicitProjectionModel for OdeModel {
    fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        OdeModel::eval_residual(self, y, p, t, out)
            .map_err(|err| RuntimeSolveError::solve_ir(err.to_string()))
    }

    fn eval_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        OdeModel::eval_jacobian_v(self, y, p, t, v, out)
            .map_err(|err| RuntimeSolveError::solve_ir(err.to_string()))
    }

    fn eval_implicit_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(program_idx) = self
            .implicit_scalar_rhs
            .single_output_row_for_output_index(row_idx)
        else {
            return Ok(None);
        };
        self.implicit_scalar_rhs
            .eval_row_unchecked_with_context(program_idx, y, p, t, self.row_eval_context(None))
            .map(Some)
            .map_err(|err| RuntimeSolveError::solve_ir(err.to_string()))
    }

    fn eval_implicit_jacobian_v_row(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(program_idx) = self
            .implicit_scalar_jacobian_v
            .single_output_row_for_output_index(row_idx)
        else {
            return Ok(None);
        };
        self.implicit_scalar_jacobian_v
            .eval_row_unchecked_with_context(program_idx, y, p, t, self.row_eval_context(Some(v)))
            .map(Some)
            .map_err(|err| RuntimeSolveError::solve_ir(err.to_string()))
    }

    fn eval_implicit_target_value(
        &self,
        row_idx: usize,
        target_y_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(program_idx) = self
            .implicit_scalar_rhs
            .single_output_row_for_output_index(row_idx)
        else {
            return Ok(None);
        };
        self.implicit_scalar_rhs
            .eval_target_assignment_row_unchecked_with_context(
                program_idx,
                target_y_index,
                y,
                p,
                t,
                self.row_eval_context(None),
            )
            .map_err(|err| RuntimeSolveError::solve_ir(err.to_string()))
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.implicit_targets.get(row_idx).copied().flatten()
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.algebraic_projection_plan
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        OdeModel::target_name_for_row(self, row_idx)
    }

    fn variable_name_for_y_index(&self, y_index: usize) -> Option<&str> {
        self.solver_names.get(y_index).map(String::as_str)
    }

    fn variable_scale_for_y_index(&self, y_index: usize) -> f64 {
        self.solver_scales.get(y_index).copied().unwrap_or(1.0)
    }
}

impl AlgebraicProjectionModel for OdeModel {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        OdeModel::eval_initial_residual(self, y, p, t, out)
            .map_err(|err| RuntimeSolveError::solve_ir(err.to_string()))
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        OdeModel::eval_initial_jacobian_v(self, y, p, t, v, out)
            .map_err(|err| RuntimeSolveError::solve_ir(err.to_string()))
    }

    fn eval_initial_target_value(
        &self,
        output_index: usize,
        target_y_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(row_idx) = self
            .initial_scalar_residual
            .single_output_row_for_output_index(output_index)
        else {
            return Ok(None);
        };
        self.initial_scalar_residual
            .eval_target_assignment_row_unchecked_with_context(
                row_idx,
                target_y_index,
                y,
                p,
                t,
                self.row_eval_context(None),
            )
            .map_err(|err| RuntimeSolveError::solve_ir(err.to_string()))
    }

    fn eval_initial_residual_row(
        &self,
        output_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(row_idx) = self
            .initial_scalar_residual
            .single_output_row_for_output_index(output_index)
        else {
            return Ok(None);
        };
        self.initial_scalar_residual
            .eval_row_output_unchecked_with_context(
                row_idx,
                0,
                y,
                p,
                t,
                self.row_eval_context(None),
            )
            .map(Some)
            .map_err(|err| RuntimeSolveError::solve_ir(err.to_string()))
    }

    fn initial_residual_len(&self) -> usize {
        OdeModel::initial_residual_len(self)
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.initial_targets.get(row_idx).copied().flatten()
    }
}

pub(crate) fn validate_model(model: &solve::SolveModel) -> Result<(), SimError> {
    if model.state_scalar_count() > 0
        && model.problem.continuous.implicit_rhs.is_empty()
        && model.problem.continuous.derivative_rhs.is_empty()
    {
        return Err(SimError::EmptySystem);
    }
    if model.initial_y.len() != model.solver_scalar_count() {
        return Err(SimError::SolveIr(format!(
            "initial vector length {} does not match solver layout {}",
            model.initial_y.len(),
            model.solver_scalar_count()
        )));
    }
    if !model.problem.discrete.pre_modes.is_empty()
        && model.problem.discrete.pre_modes.len() != model.problem.discrete.rhs.len()
    {
        return Err(SimError::SolveIr(format!(
            "discrete pre-mode row count {} does not match discrete RHS row count {}",
            model.problem.discrete.pre_modes.len(),
            model.problem.discrete.rhs.len()
        )));
    }
    if !model.problem.discrete.observation_refresh.is_empty()
        && model.problem.discrete.observation_refresh.len() != model.problem.discrete.rhs.len()
    {
        return Err(SimError::SolveIr(format!(
            "discrete observation-refresh row count {} does not match discrete RHS row count {}",
            model.problem.discrete.observation_refresh.len(),
            model.problem.discrete.rhs.len()
        )));
    }
    Ok(())
}

pub(crate) fn build_ode_problem_with_runtime_params_and_initial(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime_params: RuntimeParameters,
    t_start: f64,
    initial_y: Vec<f64>,
    ode_model: Arc<OdeModel>,
    root_runtime: Arc<SolveRuntime>,
) -> Result<
    OdeSolverProblem<
        impl OdeEquationsImplicit<M = Matrix, V = Vector, T = Scalar, C = <Matrix as MatrixCommon>::C>
        + use<>,
    >,
    SimError,
> {
    build_ode_problem_with_initial(
        model,
        opts,
        t_start,
        initial_y,
        Some(runtime_params),
        ode_model,
        root_runtime,
    )
}

pub(crate) fn build_state_ode_problem_with_runtime_params_and_initial(
    model: &solve::SolveModel,
    opts: &SimOptions,
    input: StateOdeProblemInput,
) -> Result<
    OdeSolverProblem<
        impl OdeEquationsImplicit<M = Matrix, V = Vector, T = Scalar, C = <Matrix as MatrixCommon>::C>
        + use<>,
    >,
    SimError,
> {
    let state_count = model.state_scalar_count();
    let params = model.parameters.clone();
    let atol = solver_absolute_tolerances(model, opts.atol, state_count.max(1));
    let root_runtime = input.rhs_runtime.clone();
    let rhs_counters = input.eval_counters.clone();
    let root_counters = input.eval_counters.clone();
    let rhs_params = Some(input.runtime_params.clone());
    let root_params = Some(input.runtime_params.clone());
    let rhs_warm_start = input.algebraic_warm_start.clone();
    let rhs_delay_params = RefCell::new(Vec::new());
    let tol = opts.atol.max(1.0e-10);
    let StructuralJacobianEvaluator {
        evaluate: jac_fn,
        probe: sparsity_probe,
    } = state_jacobian_evaluator(StateJacobianEvaluatorInput {
        runtime: input.rhs_runtime.clone(),
        runtime_params: Some(input.runtime_params.clone()),
        warm_start: input.algebraic_warm_start.clone(),
        counters: input.eval_counters.clone(),
        tolerance: tol,
        pattern: derivative_jacobian_pattern(model)?,
    });

    let rhs_fn = move |y: &Vector, p: &Vector, t: Scalar, out: &mut Vector| {
        let start = rhs_counters.as_ref().map(|_| Instant::now());
        with_runtime_params(&rhs_params, p.as_slice(), |params| {
            let mut solver_y = rhs_warm_start.speculative();
            let state_len = y.len().min(solver_y.len());
            solver_y[..state_len].copy_from_slice(&y.as_slice()[..state_len]);
            let result = with_delay_adjusted_params_mut(
                input.rhs_runtime.as_ref(),
                &mut solver_y,
                params,
                t,
                &rhs_delay_params,
                |solver_y, params| {
                    input.rhs_runtime.eval_state_derivatives_with_guess_into(
                        t,
                        y.as_slice(),
                        params,
                        solver_y,
                        tol,
                        256,
                        out.as_mut_slice(),
                    )
                },
            );
            if !matches!(result, Ok(Ok(()))) {
                fill_eval_error(out.as_mut_slice());
            }
        });
        if let (Some(counters), Some(start)) = (rhs_counters.as_ref(), start) {
            counters.rhs(elapsed_nanos_u64(start));
        }
    };
    let root_count = root_runtime.root_condition_count().max(1);
    let root_fn = state_root_evaluator(root_runtime, root_params, root_counters, tol);

    let problem = OdeBuilder::<Matrix>::new()
        .t0(input.t_start)
        .h0(opts.dt.unwrap_or(1.0e-3).abs().max(1.0e-9))
        .rtol(opts.rtol)
        .atol(atol)
        .p(params)
        .rhs_implicit(rhs_fn, jac_fn)
        .init(
            move |_p: &Vector, _t: Scalar, y: &mut Vector| {
                y.as_mut_slice().copy_from_slice(&input.initial_state);
            },
            state_count.max(1),
        )
        .root(root_fn, root_count)
        .build()
        .map_err(|err| SimError::SolverError(format!("ODE problem builder failed: {err}")));
    sparsity_probe.store(false, Ordering::Relaxed);
    problem
}

/// Build the state-only Diffsol problem over the FMI 3 ME adapter.
///
/// The model is still consulted for the solver-owned tolerance vector and
/// structural sparsity pattern during the migration. Every migrated numerical
/// callback crosses `DiffsolMeHost`. The retired-runtime RHS oracle was removed
/// after the full migration cohort passed its bit-exact dual-run guard.
fn finish_me_ode_problem<Eqn, Error>(
    problem: Result<OdeSolverProblem<Eqn>, Error>,
    host: &DiffsolMeHost,
    eval_counters: Option<Arc<BdfEvalCounters>>,
) -> Result<MeOdeProblemBuild<Eqn>, SimError>
where
    Eqn: StateOdeEquations,
    Error: std::fmt::Display,
{
    if let Some(error) = host.take_callback_error() {
        return Err(error.into());
    }
    let problem = problem.map_err(|error| {
        SimError::SolverError(format!("ME ODE problem builder failed: {error}"))
    })?;
    Ok(MeOdeProblemBuild {
        problem,
        eval_counters,
    })
}

pub(crate) fn build_me_state_ode_problem(
    opts: &SimOptions,
    host: DiffsolMeHost,
    t_start: f64,
    initial_state: Vec<f64>,
) -> Result<MeOdeProblemBuild<impl StateOdeEquations + use<>>, SimError> {
    let state_count = host.state_count();
    let root_count = host.event_indicator_count().max(1);
    let base_tolerance = opts.atol.abs().max(f64::MIN_POSITIVE);
    let mut atol = host
        .continuous_state_nominals()?
        .into_iter()
        .map(|nominal| (base_tolerance * nominal).clamp(f64::MIN_POSITIVE, f64::MAX))
        .collect::<Vec<_>>();
    if atol.is_empty() {
        atol.push(base_tolerance);
    }
    let eval_counters = new_bdf_eval_counters();
    let rhs_counters = eval_counters.clone();
    let jacobian_counters = eval_counters.clone();
    let root_counters = eval_counters.clone();
    let rhs_host = host.clone();
    let jacobian_host = host.clone();
    let build_error_host = host.clone();
    let root_host = host;
    let sparsity_probe = Arc::new(AtomicBool::new(true));
    let jacobian_probe = sparsity_probe.clone();

    let rhs = move |y: &Vector, _p: &Vector, t: Scalar, out: &mut Vector| {
        let start = rhs_counters.as_ref().map(|_| Instant::now());
        rhs_host.derivatives_into(t, y.as_slice(), out.as_mut_slice());
        if let (Some(counters), Some(start)) = (rhs_counters.as_ref(), start) {
            counters.rhs(elapsed_nanos_u64(start));
        }
    };
    let jacobian = move |y: &Vector, _p: &Vector, t: Scalar, seed: &Vector, out: &mut Vector| {
        if jacobian_probe.load(Ordering::Relaxed) {
            apply_dense_jacobian_probe(seed.as_slice(), out.as_mut_slice());
            return;
        }
        let start = jacobian_counters.as_ref().map(|_| Instant::now());
        jacobian_host.directional_derivative_into(
            t,
            y.as_slice(),
            seed.as_slice(),
            out.as_mut_slice(),
        );
        if let (Some(counters), Some(start)) = (jacobian_counters.as_ref(), start) {
            counters.jacobian_vector(elapsed_nanos_u64(start));
        }
    };
    let roots = move |y: &Vector, _p: &Vector, t: Scalar, out: &mut Vector| {
        let start = root_counters.as_ref().map(|_| Instant::now());
        root_host.event_indicators_into(t, y.as_slice(), out.as_mut_slice());
        if let (Some(counters), Some(start)) = (root_counters.as_ref(), start) {
            counters.root(elapsed_nanos_u64(start));
        }
    };

    let problem = OdeBuilder::<Matrix>::new()
        .t0(t_start)
        .h0(opts.dt.unwrap_or(1.0e-3).abs().max(1.0e-9))
        .rtol(opts.rtol)
        .atol(atol)
        .p(Vec::new())
        .rhs_implicit(rhs, jacobian)
        .init(
            move |_p: &Vector, _t: Scalar, y: &mut Vector| {
                y.as_mut_slice().copy_from_slice(&initial_state);
            },
            state_count.max(1),
        )
        .root(roots, root_count)
        .build();
    sparsity_probe.store(false, Ordering::Relaxed);
    finish_me_ode_problem(problem, &build_error_host, eval_counters)
}

fn apply_dense_jacobian_probe(seed: &[f64], output: &mut [f64]) {
    let magnitude = seed.iter().copied().map(f64::abs).sum::<f64>();
    output.fill(magnitude);
}

pub(crate) trait StateOdeEquations:
    OdeEquationsImplicit<M = Matrix, V = Vector, T = Scalar, C = <Matrix as MatrixCommon>::C>
{
}

impl<T> StateOdeEquations for T where
    T: OdeEquationsImplicit<M = Matrix, V = Vector, T = Scalar, C = <Matrix as MatrixCommon>::C>
{
}

pub(crate) struct MeOdeProblemBuild<Eqn: StateOdeEquations> {
    pub(crate) problem: OdeSolverProblem<Eqn>,
    pub(crate) eval_counters: Option<Arc<BdfEvalCounters>>,
}

struct StateJacobianEvaluatorInput {
    runtime: Arc<SolveRuntime>,
    runtime_params: Option<RuntimeParameters>,
    warm_start: AlgebraicWarmStart,
    counters: Option<Arc<BdfEvalCounters>>,
    tolerance: f64,
    pattern: solve::StructuralPattern,
}

struct StructuralJacobianEvaluator<F> {
    evaluate: F,
    probe: Arc<AtomicBool>,
}

fn state_jacobian_evaluator(
    input: StateJacobianEvaluatorInput,
) -> StructuralJacobianEvaluator<impl Fn(&Vector, &Vector, Scalar, &Vector, &mut Vector)> {
    let probe = Arc::new(AtomicBool::new(true));
    let evaluator_probe = probe.clone();
    let delay_params = RefCell::new(Vec::new());
    let evaluator = move |y: &Vector, p: &Vector, t: Scalar, v: &Vector, out: &mut Vector| {
        if evaluator_probe.load(Ordering::Relaxed) {
            apply_structural_jacobian_probe(&input.pattern, v.as_slice(), out.as_mut_slice());
            return;
        }
        let start = input.counters.as_ref().map(|_| Instant::now());
        with_runtime_params(&input.runtime_params, p.as_slice(), |params| {
            let mut solver_y = input.warm_start.speculative();
            let state_len = y.len().min(solver_y.len());
            solver_y[..state_len].copy_from_slice(&y.as_slice()[..state_len]);
            let result = with_delay_adjusted_params_mut(
                input.runtime.as_ref(),
                &mut solver_y,
                params,
                t,
                &delay_params,
                |solver_y, params| {
                    input.runtime.eval_state_jacobian_v_ad_with_guess_into(
                        rumoca_solver::AlgebraicLinearization {
                            t,
                            params,
                            settle: bdf_algebraic_settle(input.tolerance),
                        },
                        y.as_slice(),
                        v.as_slice(),
                        solver_y,
                        out.as_mut_slice(),
                    )
                },
            );
            if !matches!(result, Ok(Ok(()))) {
                fill_eval_error(out.as_mut_slice());
            }
        });
        if let (Some(counters), Some(start)) = (input.counters.as_ref(), start) {
            counters.jacobian_vector(elapsed_nanos_u64(start));
        }
    };
    StructuralJacobianEvaluator {
        evaluate: evaluator,
        probe,
    }
}

fn state_root_evaluator(
    runtime: Arc<SolveRuntime>,
    runtime_params: Option<RuntimeParameters>,
    counters: Option<Arc<BdfEvalCounters>>,
    tolerance: f64,
) -> impl Fn(&Vector, &Vector, Scalar, &mut Vector) {
    let delay_params = RefCell::new(Vec::new());
    move |y: &Vector, p: &Vector, t: Scalar, out: &mut Vector| {
        let start = counters.as_ref().map(|_| Instant::now());
        with_runtime_params(&runtime_params, p.as_slice(), |params| {
            let mut solver_y = runtime.model.initial_y.clone();
            let state_len = y.len().min(solver_y.len());
            solver_y[..state_len].copy_from_slice(&y.as_slice()[..state_len]);
            let result = with_delay_adjusted_params(
                runtime.as_ref(),
                &solver_y,
                params,
                t,
                &delay_params,
                |params| {
                    runtime.eval_root_search_conditions_into(
                        t,
                        y.as_slice(),
                        params,
                        tolerance,
                        EVENT_UPDATE_MAX_ITERS,
                        out.as_mut_slice(),
                    )
                },
            );
            if !matches!(result, Ok(Ok(()))) {
                fill_eval_error(out.as_mut_slice());
            }
        });
        if let (Some(counters), Some(start)) = (counters.as_ref(), start) {
            counters.root(elapsed_nanos_u64(start));
        }
    }
}

fn bdf_algebraic_settle(tol: f64) -> rumoca_solver::AlgebraicSettle {
    rumoca_solver::AlgebraicSettle {
        tol,
        max_iters: 256,
    }
}

fn ode_problem_storage(
    model: &solve::SolveModel,
    opts: &SimOptions,
    ode_model: &OdeModel,
) -> (usize, Vec<f64>, MassOperator) {
    let solver_count = model.solver_scalar_count();
    let atol = solver_absolute_tolerances(model, opts.atol, solver_count.max(1));
    let mass = MassOperator {
        solver_count,
        matrix: ode_model.prepared_mass_matrix().clone(),
    };
    (solver_count, atol, mass)
}

fn build_ode_problem_with_initial(
    model: &solve::SolveModel,
    opts: &SimOptions,
    t_start: f64,
    initial_y: Vec<f64>,
    runtime_params: Option<RuntimeParameters>,
    ode_model: Arc<OdeModel>,
    root_runtime: Arc<SolveRuntime>,
) -> Result<
    OdeSolverProblem<
        impl OdeEquationsImplicit<M = Matrix, V = Vector, T = Scalar, C = <Matrix as MatrixCommon>::C>
        + use<>,
    >,
    SimError,
> {
    let params = model.parameters.clone();
    let (n_total, atol, mass) = ode_problem_storage(model, opts, &ode_model);

    let rhs_runtime_params = runtime_params.clone();
    let root_runtime_params = runtime_params.clone();
    let rhs_delay_runtime = root_runtime.clone();
    let root_delay_params = RefCell::new(Vec::new());
    let rhs_delay_params = RefCell::new(Vec::new());
    let tol = opts.atol.max(1.0e-10);
    let root_count = root_runtime.root_condition_count().max(1);
    let StructuralJacobianEvaluator {
        evaluate: jac_fn,
        probe: sparsity_probe,
    } = implicit_jacobian_evaluator(ImplicitJacobianEvaluatorInput {
        model: ode_model.clone(),
        runtime: root_runtime.clone(),
        runtime_params: runtime_params.clone(),
        pattern: implicit_jacobian_pattern(model)?,
    });
    let root_fn = move |y: &Vector, p: &Vector, t: Scalar, out: &mut Vector| {
        with_runtime_params(&root_runtime_params, p.as_slice(), |params| {
            let result = with_delay_adjusted_params(
                root_runtime.as_ref(),
                y.as_slice(),
                params,
                t,
                &root_delay_params,
                |params| {
                    root_runtime.eval_root_search_conditions_into(
                        t,
                        y.as_slice(),
                        params,
                        tol,
                        EVENT_UPDATE_MAX_ITERS,
                        out.as_mut_slice(),
                    )
                },
            );
            if !matches!(result, Ok(Ok(()))) {
                fill_eval_error(out.as_mut_slice());
            }
        });
    };

    let problem = OdeBuilder::<Matrix>::new()
        .t0(t_start)
        .h0(opts.dt.unwrap_or(1.0e-3).abs().max(1.0e-9))
        .rtol(opts.rtol)
        .atol(atol)
        .p(params)
        .rhs_implicit(
            move |y: &Vector, p: &Vector, t: Scalar, out: &mut Vector| {
                with_runtime_params(&rhs_runtime_params, p.as_slice(), |params| {
                    let result = with_delay_adjusted_params(
                        rhs_delay_runtime.as_ref(),
                        y.as_slice(),
                        params,
                        t,
                        &rhs_delay_params,
                        |params| {
                            ode_model.eval_residual(y.as_slice(), params, t, out.as_mut_slice())
                        },
                    );
                    if !matches!(result, Ok(Ok(()))) {
                        fill_eval_error(out.as_mut_slice());
                    }
                });
            },
            jac_fn,
        )
        .mass(
            move |v: &Vector, _p: &Vector, _t: Scalar, beta: Scalar, out: &mut Vector| {
                mass.apply(v.as_slice(), beta, out.as_mut_slice());
            },
        )
        .init(
            move |_p: &Vector, _t: Scalar, y: &mut Vector| {
                y.as_mut_slice().copy_from_slice(&initial_y);
            },
            n_total.max(1),
        )
        .root(root_fn, root_count)
        .build()
        .map_err(|err| SimError::SolverError(format!("ODE problem builder failed: {err}")));
    sparsity_probe.store(false, Ordering::Relaxed);
    problem
}

struct ImplicitJacobianEvaluatorInput {
    model: Arc<OdeModel>,
    runtime: Arc<SolveRuntime>,
    runtime_params: Option<RuntimeParameters>,
    pattern: solve::StructuralPattern,
}

fn implicit_jacobian_evaluator(
    input: ImplicitJacobianEvaluatorInput,
) -> StructuralJacobianEvaluator<impl Fn(&Vector, &Vector, Scalar, &Vector, &mut Vector)> {
    let probe = Arc::new(AtomicBool::new(true));
    let evaluator_probe = probe.clone();
    let delay_params = RefCell::new(Vec::new());
    let evaluator = move |y: &Vector, p: &Vector, t: Scalar, v: &Vector, out: &mut Vector| {
        if evaluator_probe.load(Ordering::Relaxed) {
            apply_structural_jacobian_probe(&input.pattern, v.as_slice(), out.as_mut_slice());
            return;
        }
        with_runtime_params(&input.runtime_params, p.as_slice(), |params| {
            let result = with_delay_adjusted_params(
                input.runtime.as_ref(),
                y.as_slice(),
                params,
                t,
                &delay_params,
                |params| {
                    input.model.eval_jacobian_v(
                        y.as_slice(),
                        params,
                        t,
                        v.as_slice(),
                        out.as_mut_slice(),
                    )
                },
            );
            if !matches!(result, Ok(Ok(()))) {
                fill_eval_error(out.as_mut_slice());
            }
        });
    };
    StructuralJacobianEvaluator {
        evaluate: evaluator,
        probe,
    }
}

fn derivative_jacobian_pattern(
    model: &solve::SolveModel,
) -> Result<solve::StructuralPattern, SimError> {
    let (continuous, _) =
        solve_eval::derive_solve_structural_artifacts(&model.problem, &model.artifacts)
            .map_err(|error| SimError::SolveIr(error.to_string()))?;
    continuous
        .derivative()
        .map(|structure| structure.pattern().clone())
        .ok_or_else(|| {
            SimError::SolveIr(
                "state ODE requires a derived derivative Jacobian pattern".to_string(),
            )
        })
}

fn implicit_jacobian_pattern(
    model: &solve::SolveModel,
) -> Result<solve::StructuralPattern, SimError> {
    let (continuous, _) =
        solve_eval::derive_solve_structural_artifacts(&model.problem, &model.artifacts)
            .map_err(|error| SimError::SolveIr(error.to_string()))?;
    continuous
        .implicit()
        .map(|structure| structure.pattern().clone())
        .ok_or_else(|| {
            SimError::SolveIr(
                "implicit ODE requires a derived implicit Jacobian pattern".to_string(),
            )
        })
}

fn apply_structural_jacobian_probe(
    pattern: &solve::StructuralPattern,
    seed: &[f64],
    output: &mut [f64],
) {
    output.fill(0.0);
    for (column, magnitude) in seed.iter().copied().map(f64::abs).enumerate() {
        if magnitude == 0.0 || column >= pattern.columns() as usize {
            continue;
        }
        for (row, value) in output.iter_mut().enumerate() {
            if pattern.contains(row as u32, column as u32) {
                *value += magnitude;
            }
        }
    }
}

fn solver_absolute_tolerances(
    model: &solve::SolveModel,
    base_tolerance: f64,
    count: usize,
) -> Vec<f64> {
    let base = base_tolerance.abs().max(f64::MIN_POSITIVE);
    (0..count)
        .map(|index| {
            let tolerance = base * model.solver_variable_scale(index);
            if tolerance.is_finite() {
                tolerance.max(f64::MIN_POSITIVE)
            } else {
                f64::MAX
            }
        })
        .collect()
}

fn fill_eval_error(out: &mut [f64]) {
    out.fill(f64::NAN);
}

fn with_runtime_params(
    runtime_params: &Option<RuntimeParameters>,
    fallback: &[f64],
    f: impl FnOnce(&[f64]),
) {
    if let Some(runtime_params) = runtime_params {
        let params = runtime_params.borrow();
        f(params.as_slice());
    } else {
        f(fallback);
    }
}

fn with_delay_adjusted_params<R>(
    runtime: &SolveRuntime,
    solver_y: &[f64],
    params: &[f64],
    time: f64,
    scratch: &RefCell<Vec<f64>>,
    f: impl FnOnce(&[f64]) -> R,
) -> Result<R, RuntimeSolveError> {
    if !runtime.has_delay_channels() {
        return Ok(f(params));
    }
    let mut adjusted = scratch.borrow_mut();
    adjusted.resize(params.len(), 0.0);
    adjusted.copy_from_slice(params);
    runtime.refresh_delay_values(time, solver_y, &mut adjusted)?;
    Ok(f(&adjusted))
}

fn with_delay_adjusted_params_mut<R>(
    runtime: &SolveRuntime,
    solver_y: &mut [f64],
    params: &[f64],
    time: f64,
    scratch: &RefCell<Vec<f64>>,
    f: impl FnOnce(&mut [f64], &[f64]) -> R,
) -> Result<R, RuntimeSolveError> {
    if !runtime.has_delay_channels() {
        return Ok(f(solver_y, params));
    }
    let mut adjusted = scratch.borrow_mut();
    adjusted.resize(params.len(), 0.0);
    adjusted.copy_from_slice(params);
    runtime.refresh_delay_values(time, solver_y, &mut adjusted)?;
    Ok(f(solver_y, &adjusted))
}

#[derive(Clone)]
struct MassOperator {
    solver_count: usize,
    matrix: PreparedMassMatrix,
}

impl MassOperator {
    fn apply(&self, v: &[f64], beta: f64, out: &mut [f64]) {
        self.matrix
            .apply_solver_mass_with_beta(v, beta, out, self.solver_count);
    }
}

#[cfg(test)]
mod tolerance_tests {
    use super::solver_absolute_tolerances;
    use rumoca_ir_solve as solve;

    #[test]
    fn bdf_absolute_tolerances_follow_solver_variable_scales() {
        let model = solve::SolveModel {
            initial_y: vec![0.0, 1.0e6],
            solver_nominals: vec![1.0e-9, 1.0],
            ..solve::SolveModel::default()
        };

        assert_eq!(
            solver_absolute_tolerances(&model, 1.0e-6, 2),
            vec![1.0e-15, 1.0]
        );
    }
}

#[cfg(test)]
mod structural_probe_tests {
    use super::apply_structural_jacobian_probe;
    use rumoca_ir_solve::{
        PatternDerivation, PatternProvenance, StructuralPattern, source_span_from_offsets,
    };

    fn pattern(dependencies: &[Vec<usize>]) -> StructuralPattern {
        let provenance = PatternProvenance::derived(
            PatternDerivation::DependencyPropagation,
            source_span_from_offsets(51, 0, 1),
        )
        .expect("fixture provenance is source-backed");
        StructuralPattern::from_row_dependencies(
            dependencies.len(),
            dependencies.len(),
            dependencies,
            provenance,
        )
        .expect("fixture dependencies form a checked pattern")
    }

    #[test]
    fn structural_probe_retains_derivative_that_is_zero_at_initial_point() {
        // For f(x) = x², the numerical Jv is zero at x = 0 even though the
        // derivative can be nonzero elsewhere. The builder probe must report
        // the may-depend edge so Diffsol/Faer cannot permanently omit it.
        let pattern = pattern(&[vec![0]]);
        let mut output = [0.0];

        apply_structural_jacobian_probe(&pattern, &[1.0], &mut output);

        assert_eq!(output, [1.0]);
    }

    #[test]
    fn structural_probe_preserves_checked_row_column_incidence() {
        let pattern = pattern(&[vec![0, 1], vec![1]]);
        let mut output = [0.0, 0.0];

        apply_structural_jacobian_probe(&pattern, &[-2.0, 3.0], &mut output);

        assert_eq!(output, [5.0, 3.0]);
    }
}
