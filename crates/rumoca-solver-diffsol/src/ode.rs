use std::sync::{
    Arc,
    atomic::{AtomicU64, Ordering},
};
use std::time::Instant;

use diffsol::{MatrixCommon, OdeBuilder, OdeEquationsImplicit, OdeSolverProblem, VectorHost};
use rumoca_eval_solve::{
    self as solve_eval, PreparedComputeBlock, PreparedScalarProgramBlock, RowEvalContext,
    SolveRuntime,
};
use rumoca_ir_solve as solve;
use rumoca_solver::{AlgebraicProjectionModel, PreparedMassMatrix, RuntimeSolveError, SimOptions};

use crate::{Matrix, RuntimeParameters, Scalar, SimError, Vector};

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

pub(crate) fn new_bdf_eval_counters() -> Option<Arc<BdfEvalCounters>> {
    trace_bdf_eval_counts().then(|| Arc::new(BdfEvalCounters::default()))
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
    implicit_rhs: PreparedComputeBlock,
    implicit_scalar_rhs: PreparedScalarProgramBlock,
    initial_residual: PreparedComputeBlock,
    pub(crate) initial_targets: Vec<Option<solve::ScalarSlot>>,
    implicit_jacobian_v: PreparedComputeBlock,
    pub(crate) root_conditions: PreparedScalarProgramBlock,
    pub(crate) implicit_targets: Vec<Option<solve::ScalarSlot>>,
    algebraic_projection_plan: solve::AlgebraicProjectionPlan,
    solver_names: Vec<String>,
    pub(crate) external_tables: solve::ExternalTables,
    pub(crate) runtime_state: solve_eval::SimulationRuntimeState,
}

impl OdeModel {
    pub(crate) fn new(model: &solve::SolveModel) -> Result<Self, SimError> {
        Ok(Self {
            state_count: model.state_scalar_count(),
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
            initial_targets: model.problem.initialization.row_targets.clone(),
            implicit_jacobian_v: PreparedComputeBlock::new_with_label(
                &model.artifacts.continuous.implicit_jacobian_v,
                "ode_implicit_jacobian_v",
            )?,
            root_conditions: PreparedScalarProgramBlock::new(
                model.problem.events.root_conditions.clone(),
            )?,
            implicit_targets: model.problem.continuous.implicit_row_targets.clone(),
            algebraic_projection_plan: model.problem.continuous.algebraic_projection_plan.clone(),
            solver_names: model.problem.solve_layout.solver_maps.names.clone(),
            external_tables: model.external_tables.clone(),
            runtime_state: solve_eval::SimulationRuntimeState::new(),
        })
    }

    pub(crate) fn state_count_for_projection(&self) -> usize {
        self.state_count
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

impl AlgebraicProjectionModel for OdeModel {
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

    fn eval_implicit_target_value(
        &self,
        row_idx: usize,
        target_y_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.implicit_scalar_rhs
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

    fn initial_residual_len(&self) -> usize {
        OdeModel::initial_residual_len(self)
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.implicit_targets.get(row_idx).copied().flatten()
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.initial_targets.get(row_idx).copied().flatten()
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.algebraic_projection_plan
    }

    fn has_explicit_initial_targets(&self) -> bool {
        self.initial_targets.iter().any(Option::is_some)
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        OdeModel::target_name_for_row(self, row_idx)
    }
}

pub(crate) fn validate_model(model: &solve::SolveModel) -> Result<(), SimError> {
    if model.state_scalar_count() > 0 && model.problem.continuous.implicit_rhs.is_empty() {
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
    )
}

pub(crate) fn build_state_ode_problem_with_runtime_params_and_initial(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime_params: RuntimeParameters,
    t_start: f64,
    initial_state: Vec<f64>,
    eval_counters: Option<Arc<BdfEvalCounters>>,
    rhs_runtime: Arc<SolveRuntime>,
) -> Result<
    OdeSolverProblem<
        impl OdeEquationsImplicit<M = Matrix, V = Vector, T = Scalar, C = <Matrix as MatrixCommon>::C>
        + use<>,
    >,
    SimError,
> {
    let state_count = model.state_scalar_count();
    let params = model.parameters.clone();
    let atol = vec![opts.atol; state_count.max(1)];
    let jac_runtime = rhs_runtime.clone();
    let root_runtime = rhs_runtime.clone();
    let rhs_counters = eval_counters.clone();
    let jac_counters = eval_counters.clone();
    let root_counters = eval_counters;
    let rhs_params = Some(runtime_params.clone());
    let jac_params = Some(runtime_params.clone());
    let root_params = Some(runtime_params);
    let tol = opts.atol.max(1.0e-10);

    let rhs_fn = move |y: &Vector, p: &Vector, t: Scalar, out: &mut Vector| {
        let start = rhs_counters.as_ref().map(|_| Instant::now());
        with_runtime_params(&rhs_params, p.as_slice(), |params| {
            if rhs_runtime
                .eval_state_derivatives_into(t, y.as_slice(), params, tol, 256, out.as_mut_slice())
                .is_err()
            {
                fill_eval_error(out.as_mut_slice());
            }
        });
        if let (Some(counters), Some(start)) = (rhs_counters.as_ref(), start) {
            counters.rhs(elapsed_nanos_u64(start));
        }
    };
    let jac_fn = move |y: &Vector, p: &Vector, t: Scalar, v: &Vector, out: &mut Vector| {
        let start = jac_counters.as_ref().map(|_| Instant::now());
        with_runtime_params(&jac_params, p.as_slice(), |params| {
            if jac_runtime
                .eval_state_jacobian_v_ad_into(
                    solve_eval::AlgebraicLinearization {
                        t,
                        params,
                        settle: solve_eval::AlgebraicSettle {
                            tol,
                            max_iters: 256,
                        },
                    },
                    y.as_slice(),
                    v.as_slice(),
                    out.as_mut_slice(),
                )
                .is_err()
            {
                fill_eval_error(out.as_mut_slice());
            }
        });
        if let (Some(counters), Some(start)) = (jac_counters.as_ref(), start) {
            counters.jacobian_vector(elapsed_nanos_u64(start));
        }
    };
    let root_fn = move |y: &Vector, p: &Vector, t: Scalar, out: &mut Vector| {
        let start = root_counters.as_ref().map(|_| Instant::now());
        with_runtime_params(&root_params, p.as_slice(), |params| {
            if root_runtime
                .eval_root_conditions_into(t, y.as_slice(), params, tol, 256, out.as_mut_slice())
                .is_err()
            {
                fill_eval_error(out.as_mut_slice());
            }
        });
        if let (Some(counters), Some(start)) = (root_counters.as_ref(), start) {
            counters.root(elapsed_nanos_u64(start));
        }
    };

    OdeBuilder::<Matrix>::new()
        .t0(t_start)
        .h0(opts.dt.unwrap_or(1.0e-3).abs().max(1.0e-9))
        .rtol(opts.rtol)
        .atol(atol)
        .p(params)
        .rhs_implicit(rhs_fn, jac_fn)
        .init(
            move |_p: &Vector, _t: Scalar, y: &mut Vector| {
                y.as_mut_slice().copy_from_slice(&initial_state);
            },
            state_count.max(1),
        )
        .root(root_fn, model.problem.events.root_conditions.len().max(1))
        .build()
        .map_err(|err| SimError::SolverError(format!("ODE problem builder failed: {err}")))
}

fn build_ode_problem_with_initial(
    model: &solve::SolveModel,
    opts: &SimOptions,
    t_start: f64,
    initial_y: Vec<f64>,
    runtime_params: Option<RuntimeParameters>,
    ode_model: Arc<OdeModel>,
) -> Result<
    OdeSolverProblem<
        impl OdeEquationsImplicit<M = Matrix, V = Vector, T = Scalar, C = <Matrix as MatrixCommon>::C>
        + use<>,
    >,
    SimError,
> {
    let params = model.parameters.clone();
    let n_total = model.solver_scalar_count();
    let atol = vec![opts.atol; n_total.max(1)];
    let mass = MassOperator {
        solver_count: n_total,
        matrix: PreparedMassMatrix::new(
            &model.artifacts.continuous.mass_matrix,
            model.state_scalar_count(),
        )?,
    };

    let rhs_runtime_params = runtime_params.clone();
    let jac_runtime_params = runtime_params.clone();
    let root_runtime_params = runtime_params.clone();
    let jac_model = ode_model.clone();
    let root_model = ode_model.clone();
    let jac_fn = move |y: &Vector, p: &Vector, t: Scalar, v: &Vector, out: &mut Vector| {
        with_runtime_params(&jac_runtime_params, p.as_slice(), |params| {
            if jac_model
                .eval_jacobian_v(y.as_slice(), params, t, v.as_slice(), out.as_mut_slice())
                .is_err()
            {
                fill_eval_error(out.as_mut_slice());
            }
        });
    };
    let root_fn = move |y: &Vector, p: &Vector, t: Scalar, out: &mut Vector| {
        with_runtime_params(&root_runtime_params, p.as_slice(), |params| {
            if root_model
                .eval_roots(y.as_slice(), params, t, out.as_mut_slice())
                .is_err()
            {
                fill_eval_error(out.as_mut_slice());
            }
        });
    };

    OdeBuilder::<Matrix>::new()
        .t0(t_start)
        .h0(opts.dt.unwrap_or(1.0e-3).abs().max(1.0e-9))
        .rtol(opts.rtol)
        .atol(atol)
        .p(params)
        .rhs_implicit(
            move |y: &Vector, p: &Vector, t: Scalar, out: &mut Vector| {
                with_runtime_params(&rhs_runtime_params, p.as_slice(), |params| {
                    if ode_model
                        .eval_residual(y.as_slice(), params, t, out.as_mut_slice())
                        .is_err()
                    {
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
        .root(root_fn, model.problem.events.root_conditions.len().max(1))
        .build()
        .map_err(|err| SimError::SolverError(format!("ODE problem builder failed: {err}")))
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
