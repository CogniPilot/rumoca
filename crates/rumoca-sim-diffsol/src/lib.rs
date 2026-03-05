pub mod eliminate {
    pub use rumoca_phase_structural::eliminate::{
        EliminationResult, Substitution, eliminate_trivial,
    };
}
mod integration;

use std::time::Instant;

use diffsol::{FaerSparseLU, OdeEquations, OdeSolverProblem};
use rumoca_ir_dae as dae;

type Dae = dae::Dae;

use rumoca_eval_flat::eval::{self, build_env};
pub(crate) use rumoca_sim_core::panic_payload_message;
pub(crate) use rumoca_sim_core::simulation::dae_prepare::*;
pub use rumoca_sim_core::{
    OutputBuffers, SimError, SimOptions, SimResult, SimSolverMode, SimVariableMeta,
    SolverStartupProfile,
};
use rumoca_sim_core::{
    SolverDeadlineGuard, TimeoutBudget, build_visible_result_names, is_solver_timeout_panic,
};

type LS = FaerSparseLU<f64>;
use integration::*;
use rumoca_sim_core::equation_scalarize::build_output_names;

/// Prepare a DAE for simulation/codegen using the same structural passes that
/// the diffsol runtime uses before integration.
///
/// This is useful for template backends (e.g. JS residual runtime) that want
/// to consume a structurally preprocessed DAE instead of raw compile output.
pub fn prepare_dae_for_template_codegen(dae: &Dae, scalarize: bool) -> Result<Dae, SimError> {
    let budget = TimeoutBudget::new(None);
    rumoca_sim_core::prepare_dae_for_template_codegen_only(dae, scalarize, &budget)
}

fn validate_simulation_function_support(dae: &Dae) -> Result<(), SimError> {
    rumoca_sim_core::function_validation::validate_simulation_function_support(dae).map_err(|err| {
        SimError::UnsupportedFunction {
            name: err.name,
            reason: err.reason,
        }
    })
}

#[inline]
pub(crate) fn trace_timer_start_if(enabled: bool) -> Option<Instant> {
    if !enabled {
        return None;
    }
    #[cfg(target_arch = "wasm32")]
    {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Some(Instant::now())
    }
}

#[inline]
pub(crate) fn trace_timer_elapsed_seconds(start: Option<Instant>) -> f64 {
    start.map_or(0.0, |t0| t0.elapsed().as_secs_f64())
}

fn interp_err(t: f64, e: impl std::fmt::Display) -> SimError {
    SimError::SolverError(format!("Interpolation failed at t={t}: {e}"))
}

#[derive(Debug, Clone, Copy)]
struct TimeoutSolverCaps {
    max_nonlinear_iters: usize,
    max_nonlinear_failures: usize,
    max_error_failures: usize,
    min_timestep: f64,
}

fn timeout_solver_caps(
    max_wall_seconds: Option<f64>,
    profile: SolverStartupProfile,
) -> Option<TimeoutSolverCaps> {
    let secs = max_wall_seconds.filter(|s| s.is_finite() && *s > 0.0)?;
    if secs <= 1.0 {
        return Some(match profile {
            SolverStartupProfile::Default => TimeoutSolverCaps {
                max_nonlinear_iters: 10,
                max_nonlinear_failures: 30,
                max_error_failures: 20,
                min_timestep: 1e-14,
            },
            SolverStartupProfile::RobustTinyStep => TimeoutSolverCaps {
                max_nonlinear_iters: 30,
                max_nonlinear_failures: 180,
                max_error_failures: 90,
                min_timestep: 1e-16,
            },
        });
    }
    if secs <= 2.0 {
        return Some(match profile {
            SolverStartupProfile::Default => TimeoutSolverCaps {
                max_nonlinear_iters: 12,
                max_nonlinear_failures: 50,
                max_error_failures: 30,
                min_timestep: 1e-14,
            },
            SolverStartupProfile::RobustTinyStep => TimeoutSolverCaps {
                max_nonlinear_iters: 40,
                max_nonlinear_failures: 240,
                max_error_failures: 120,
                min_timestep: 1e-16,
            },
        });
    }
    if secs <= 10.0 {
        return Some(match profile {
            SolverStartupProfile::Default => TimeoutSolverCaps {
                max_nonlinear_iters: 20,
                max_nonlinear_failures: 120,
                max_error_failures: 80,
                min_timestep: 1e-14,
            },
            SolverStartupProfile::RobustTinyStep => TimeoutSolverCaps {
                max_nonlinear_iters: 40,
                max_nonlinear_failures: 800,
                max_error_failures: 400,
                min_timestep: 1e-16,
            },
        });
    }

    Some(match profile {
        SolverStartupProfile::Default => TimeoutSolverCaps {
            max_nonlinear_iters: 20,
            max_nonlinear_failures: 1000,
            max_error_failures: 600,
            min_timestep: 1e-14,
        },
        SolverStartupProfile::RobustTinyStep => TimeoutSolverCaps {
            max_nonlinear_iters: 40,
            max_nonlinear_failures: 4000,
            max_error_failures: 2000,
            min_timestep: 1e-16,
        },
    })
}

fn apply_timeout_solver_caps<Eqn>(
    problem: &mut OdeSolverProblem<Eqn>,
    max_wall_seconds: Option<f64>,
    profile: SolverStartupProfile,
) where
    Eqn: OdeEquations<T = f64>,
{
    let Some(caps) = timeout_solver_caps(max_wall_seconds, profile) else {
        return;
    };
    problem.ode_options.max_nonlinear_solver_iterations = problem
        .ode_options
        .max_nonlinear_solver_iterations
        .min(caps.max_nonlinear_iters);
    problem.ode_options.max_nonlinear_solver_failures = problem
        .ode_options
        .max_nonlinear_solver_failures
        .min(caps.max_nonlinear_failures);
    problem.ode_options.max_error_test_failures = problem
        .ode_options
        .max_error_test_failures
        .min(caps.max_error_failures);
    if problem.ode_options.min_timestep < caps.min_timestep {
        problem.ode_options.min_timestep = caps.min_timestep;
    }
}

fn startup_interval_cap(opts: &SimOptions) -> Option<f64> {
    let dt = opts.dt?;
    if !dt.is_finite() || dt <= 0.0 {
        return None;
    }
    let span = (opts.t_end - opts.t_start).abs();
    if span.is_finite() && span > 0.0 {
        let tiny_interval_threshold = span / 5000.0;
        if dt < tiny_interval_threshold {
            return None;
        }
    }
    Some((dt.abs() * 20.0).max(1e-10))
}

fn nonlinear_solver_tolerance(opts: &SimOptions, profile: SolverStartupProfile) -> f64 {
    let base = opts.atol.max(opts.rtol).max(1.0e-12);
    match profile {
        SolverStartupProfile::Default => (base * 10.0).clamp(1.0e-8, 1.0e-3),
        SolverStartupProfile::RobustTinyStep => (base * 100.0).clamp(1.0e-7, 1.0e-2),
    }
}

fn configure_solver_problem_with_profile<Eqn>(
    problem: &mut OdeSolverProblem<Eqn>,
    opts: &SimOptions,
    profile: SolverStartupProfile,
) where
    Eqn: OdeEquations<T = f64>,
{
    problem.ode_options.max_nonlinear_solver_iterations = 20;
    problem.ode_options.max_nonlinear_solver_failures = 1000;
    problem.ode_options.max_error_test_failures = 600;
    problem.ode_options.nonlinear_solver_tolerance = nonlinear_solver_tolerance(opts, profile);
    problem.ode_options.min_timestep = 1e-16;
    let span = (opts.t_end - opts.t_start).abs();
    let interval_cap = startup_interval_cap(opts);
    if span.is_finite() && span > 0.0 {
        problem.h0 = (span / 500.0).max(1e-6);
        if let Some(cap) = interval_cap {
            problem.h0 = problem.h0.min(cap);
        }
    } else if let Some(cap) = interval_cap {
        problem.h0 = cap;
    }

    if profile == SolverStartupProfile::RobustTinyStep {
        problem.ode_options.max_nonlinear_solver_iterations = 40;
        problem.ode_options.max_nonlinear_solver_failures = 4000;
        problem.ode_options.max_error_test_failures = 2000;
        problem.ode_options.nonlinear_solver_tolerance = nonlinear_solver_tolerance(opts, profile);
        if span.is_finite() && span > 0.0 {
            problem.h0 = (span / 5_000_000.0).max(1e-10);
        }
    }

    apply_timeout_solver_caps(problem, opts.max_wall_seconds, profile);
}

fn build_parameter_values(dae: &Dae, budget: &TimeoutBudget) -> Result<Vec<f64>, SimError> {
    problem::default_params_with_budget(dae, budget)
}

fn prepare_dae(
    dae: &Dae,
    scalarize: bool,
    budget: &TimeoutBudget,
) -> Result<rumoca_sim_core::simulation::pipeline::PreparedSimulation, SimError> {
    rumoca_sim_core::prepare_dae(dae, scalarize, budget)
}

const DUMMY_STATE_NAME: &str = "_rumoca_dummy_state";

pub(crate) type MassMatrix = rumoca_sim_core::simulation::pipeline::MassMatrix;

fn sim_introspect_enabled() -> bool {
    rumoca_sim_core::simulation::diagnostics::sim_introspect_enabled()
}

fn sim_trace_enabled() -> bool {
    rumoca_sim_core::simulation::diagnostics::sim_trace_enabled()
}

fn validate_no_initial_division_by_zero(
    dae: &Dae,
    t_start: f64,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    let mut y0 = vec![0.0; dae.f_x.len()];
    problem::initialize_state_vector(dae, &mut y0);
    let p = build_parameter_values(dae, budget)?;
    let env = build_env(dae, &y0, &p, t_start);
    if let Some(site) =
        rumoca_sim_core::simulation::diagnostics::find_initial_division_by_zero_site(dae, &env)
    {
        let msg = format!(
            "division by zero at initialization (t={}): (a={}) / (b={}), divisor expression is: {}, equation {}[{}] origin='{}' rhs={}",
            t_start,
            site.expr_site.numerator,
            site.expr_site.denominator,
            site.expr_site.divisor_expr,
            site.equation_set,
            site.equation_index,
            site.origin,
            site.rhs_expr,
        );
        return Err(SimError::SolverError(msg));
    }
    Ok(())
}

fn dump_transformed_dae_for_diffsol(dae: &Dae, mass_matrix: &MassMatrix) {
    rumoca_sim_core::simulation::diagnostics::dump_transformed_dae_for_solver(dae, mass_matrix);
}

fn dump_initial_vector_for_diffsol(dae: &Dae) {
    let n_total = dae.f_x.len();
    let mut y0 = vec![0.0; n_total];
    problem::initialize_state_vector(dae, &mut y0);
    let mut names = build_output_names(dae);
    names.truncate(n_total);
    rumoca_sim_core::simulation::diagnostics::dump_initial_vector_for_solver(&names, &y0);
}

fn dump_initial_residual_summary_for_diffsol(
    dae: &Dae,
    n_x: usize,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    if !sim_introspect_enabled() {
        return Ok(());
    }
    let n_total = dae.f_x.len();
    let mut y0 = vec![0.0; n_total];
    problem::initialize_state_vector(dae, &mut y0);
    let p = build_parameter_values(dae, budget)?;
    dump_parameter_vector_for_diffsol(dae, &p);
    let mut rhs = vec![0.0; n_total];
    problem::eval_rhs_equations(dae, &y0, &p, 0.0, &mut rhs, n_x);
    rumoca_sim_core::simulation::diagnostics::dump_initial_residual_summary(dae, &rhs, n_x);
    Ok(())
}

pub(crate) fn dump_parameter_vector_for_diffsol(dae: &Dae, params: &[f64]) {
    rumoca_sim_core::simulation::diagnostics::dump_parameter_vector(dae, params);
}

fn run_with_timeout_panic_handling<T, F>(budget: &TimeoutBudget, f: F) -> Result<T, SimError>
where
    F: FnOnce() -> Result<T, SimError>,
{
    let _solver_deadline_guard = SolverDeadlineGuard::install(budget.deadline());
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(result) => result,
        Err(payload) => {
            if is_solver_timeout_panic(payload.as_ref()) {
                return Err(budget.timeout_error().into());
            }
            Err(SimError::SolverError(format!(
                "integration panic: {}",
                panic_payload_message(payload)
            )))
        }
    }
}

pub fn simulate(dae: &Dae, opts: &SimOptions) -> Result<SimResult, SimError> {
    eval::clear_pre_values();
    let budget = TimeoutBudget::new(opts.max_wall_seconds);
    validate_simulation_function_support(dae)?;
    let sim_start = trace_timer_start_if(sim_trace_enabled());
    let prepared = prepare_dae(dae, opts.scalarize, &budget)?;
    let mut dae = prepared.dae;
    let has_dummy = prepared.has_dummy_state;
    let elim = prepared.elimination;
    let ic_blocks = prepared.ic_blocks;
    let mass_matrix = prepared.mass_matrix;
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] stage prepare_dae {:.3}s",
            trace_timer_elapsed_seconds(sim_start)
        );
    }
    validate_simulation_function_support(&dae)?;
    dump_transformed_dae_for_diffsol(&dae, &mass_matrix);

    let n_x: usize = dae.states.values().map(|v| v.size()).sum();
    if has_dummy {
        return run_timeout_result(&budget, || {
            rumoca_sim_core::build_algebraic_result(
                &dae,
                opts,
                &elim,
                &budget,
                build_visible_result_names(&dae),
                DUMMY_STATE_NAME,
            )
        });
    }
    let n_total = dae.f_x.len();

    solve_initial_conditions(&mut dae, &ic_blocks, n_x, opts.atol, &budget)?;
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] stage solve_initial_conditions {:.3}s",
            trace_timer_elapsed_seconds(sim_start)
        );
    }
    validate_no_initial_division_by_zero(&dae, opts.t_start, &budget)?;
    dump_initial_vector_for_diffsol(&dae);
    dump_initial_residual_summary_for_diffsol(&dae, n_x, &budget)?;

    let (buf, param_values) = run_with_timeout_panic_handling(&budget, || {
        integrate_with_fallbacks(&dae, opts, n_total, &mass_matrix, &budget)
    })?;
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] stage integrate_with_fallbacks {:.3}s",
            trace_timer_elapsed_seconds(sim_start)
        );
    }
    let mut output_names = build_output_names(&dae);
    output_names.truncate(n_total);
    Ok(rumoca_sim_core::finalize_dynamic_result(
        &dae,
        &elim,
        &param_values,
        n_x,
        output_names,
        buf,
    ))
}

pub mod problem {
    use crate::SimError;
    use diffsol::{
        FaerSparseMat, MatrixCommon, OdeBuilder, OdeEquationsImplicit, OdeSolverProblem, Vector,
        VectorHost,
    };
    use rumoca_ir_dae as dae;

    pub(crate) type Dae = dae::Dae;
    pub(crate) type Expression = dae::Expression;
    pub(crate) type M = FaerSparseMat<f64>;
    pub(crate) type V = <M as MatrixCommon>::V;
    pub(crate) type T = <M as MatrixCommon>::T;
    pub(crate) type C = <M as MatrixCommon>::C;

    pub(crate) use rumoca_sim_core::{
        apply_initial_section_assignments, count_states, default_params_with_budget,
        eval_jacobian_vector_ad, eval_rhs_equations, initialize_state_vector,
        project_algebraics_with_fixed_states_at_time,
    };
    pub use rumoca_sim_core::{default_params, reorder_equations_for_solver};

    fn sim_trace_enabled() -> bool {
        rumoca_sim_core::simulation::diagnostics::sim_trace_enabled()
    }

    fn sim_introspect_enabled() -> bool {
        rumoca_sim_core::simulation::diagnostics::sim_introspect_enabled()
    }

    #[cfg(not(target_arch = "wasm32"))]
    type CompiledResidual = rumoca_eval_dae::CompiledResidual;
    #[cfg(target_arch = "wasm32")]
    type CompiledResidual = rumoca_eval_dae::CompiledResidualWasm;

    #[cfg(not(target_arch = "wasm32"))]
    type CompiledJacobianV = rumoca_eval_dae::CompiledJacobianV;
    #[cfg(target_arch = "wasm32")]
    type CompiledJacobianV = rumoca_eval_dae::CompiledJacobianVWasm;

    #[cfg(not(target_arch = "wasm32"))]
    type CompiledExpressionRows = rumoca_eval_dae::CompiledExpressionRows;
    #[cfg(target_arch = "wasm32")]
    type CompiledExpressionRows = rumoca_eval_dae::CompiledExpressionRowsWasm;

    pub(crate) fn build_problem(
        dae: &Dae,
        rtol: f64,
        atol: f64,
        algebraic_eps: f64,
        mass_matrix: &crate::MassMatrix,
    ) -> Result<OdeSolverProblem<impl OdeEquationsImplicit<M = M, V = V, T = T, C = C>>, SimError>
    {
        let n_x = count_states(dae);
        let n_eq = dae.f_x.len();
        let n_z = n_eq - n_x;
        let n_total = n_x + n_z;

        let params = default_params(dae);
        let dae_init = dae.clone();
        let ProblemCompiledKernels {
            compiled_eval_ctx_rhs,
            compiled_eval_ctx_jac,
            compiled_eval_ctx_root,
            compiled_residual,
            compiled_jacobian,
            compiled_root_conditions,
            n_roots,
        } = compile_problem_kernels(dae, n_total)?;

        let mass_matrix_owned = mass_matrix.clone();
        let atol_vec: Vec<f64> = vec![atol; n_total.max(1)];

        OdeBuilder::<M>::new()
            .t0(0.0)
            .rtol(rtol)
            .atol(atol_vec)
            .p(params)
            .rhs_implicit(
                move |y: &V, p: &V, t: T, out: &mut V| {
                    crate::integration::panic_on_expired_solver_deadline();
                    call_compiled_residual(
                        &compiled_residual,
                        &compiled_eval_ctx_rhs,
                        y.as_slice(),
                        p.as_slice(),
                        t,
                        out.as_mut_slice(),
                    );
                },
                move |y: &V, p: &V, t: T, v: &V, out: &mut V| {
                    crate::integration::panic_on_expired_solver_deadline();
                    call_compiled_jacobian(
                        &compiled_jacobian,
                        &compiled_eval_ctx_jac,
                        y.as_slice(),
                        p.as_slice(),
                        t,
                        v.as_slice(),
                        out.as_mut_slice(),
                    );
                },
            )
            .mass(move |v: &V, _p: &V, _t: T, beta: T, y: &mut V| {
                crate::integration::panic_on_expired_solver_deadline();
                apply_mass_matrix_update(
                    &mass_matrix_owned,
                    n_x,
                    n_total,
                    algebraic_eps,
                    v,
                    beta,
                    y,
                );
            })
            .init(
                move |_p: &V, _t: T, y: &mut V| {
                    crate::integration::panic_on_expired_solver_deadline();
                    initialize_state_vector(&dae_init, y.as_mut_slice())
                },
                n_total.max(1),
            )
            .root(
                move |y: &V, p: &V, t: T, out: &mut V| {
                    crate::integration::panic_on_expired_solver_deadline();
                    eval_root_callback(
                        &compiled_root_conditions,
                        &compiled_eval_ctx_root,
                        y.as_slice(),
                        p.as_slice(),
                        t,
                        out.as_mut_slice(),
                    );
                },
                n_roots,
            )
            .build()
            .map_err(|err| {
                SimError::SolverError(format!(
                    "ODE problem builder failed: check DAE dimensions and parameters: {err}"
                ))
            })
    }

    struct ProblemCompiledKernels {
        compiled_eval_ctx_rhs: CompiledEvalContext,
        compiled_eval_ctx_jac: CompiledEvalContext,
        compiled_eval_ctx_root: CompiledEvalContext,
        compiled_residual: CompiledResidual,
        compiled_jacobian: CompiledJacobianV,
        compiled_root_conditions: CompiledExpressionRows,
        n_roots: usize,
    }

    fn compile_problem_kernels(
        dae: &Dae,
        n_total: usize,
    ) -> Result<ProblemCompiledKernels, SimError> {
        let sim_context =
            rumoca_sim_core::runtime::layout::SimulationContext::from_dae(dae, n_total);
        let compiled_eval_ctx = CompiledEvalContext {
            dae: dae.clone(),
            sim_context,
        };
        let compiled_eval_ctx_rhs = compiled_eval_ctx.clone();
        let compiled_eval_ctx_jac = compiled_eval_ctx.clone();
        let compiled_eval_ctx_root = compiled_eval_ctx.clone();

        let compiled_residual = compile_residual_kernel(dae)?;
        let compiled_jacobian = compile_jacobian_kernel(dae)?;
        log_precomputed_synthetic_root_conditions(&dae.synthetic_root_conditions);
        let compiled_root_conditions = compile_root_conditions_kernel(dae)?;
        let n_roots = compiled_root_conditions.rows().max(1);

        Ok(ProblemCompiledKernels {
            compiled_eval_ctx_rhs,
            compiled_eval_ctx_jac,
            compiled_eval_ctx_root,
            compiled_residual,
            compiled_jacobian,
            compiled_root_conditions,
            n_roots,
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn compile_residual_kernel(dae: &Dae) -> Result<CompiledResidual, SimError> {
        rumoca_eval_dae::compile_residual(dae, rumoca_eval_dae::Backend::Cranelift)
            .map_err(|err| SimError::CompiledEval(err.to_string()))
    }

    #[cfg(target_arch = "wasm32")]
    fn compile_residual_kernel(dae: &Dae) -> Result<CompiledResidual, SimError> {
        rumoca_eval_dae::compile_residual_wasm(dae)
            .map_err(|err| SimError::CompiledEval(err.to_string()))
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn compile_jacobian_kernel(dae: &Dae) -> Result<CompiledJacobianV, SimError> {
        rumoca_eval_dae::compile_jacobian_v(dae, rumoca_eval_dae::Backend::Cranelift)
            .map_err(|err| SimError::CompiledEval(err.to_string()))
    }

    #[cfg(target_arch = "wasm32")]
    fn compile_jacobian_kernel(dae: &Dae) -> Result<CompiledJacobianV, SimError> {
        rumoca_eval_dae::compile_jacobian_v_wasm(dae)
            .map_err(|err| SimError::CompiledEval(err.to_string()))
    }

    #[cfg(not(target_arch = "wasm32"))]
    fn compile_root_conditions_kernel(dae: &Dae) -> Result<CompiledExpressionRows, SimError> {
        rumoca_eval_dae::compile_root_conditions(dae, rumoca_eval_dae::Backend::Cranelift)
            .map_err(|err| SimError::CompiledEval(err.to_string()))
    }

    #[cfg(target_arch = "wasm32")]
    fn compile_root_conditions_kernel(dae: &Dae) -> Result<CompiledExpressionRows, SimError> {
        rumoca_eval_dae::compile_root_conditions_wasm(dae)
            .map_err(|err| SimError::CompiledEval(err.to_string()))
    }

    fn apply_mass_matrix_update(
        mass_matrix: &crate::MassMatrix,
        n_x: usize,
        n_total: usize,
        algebraic_eps: f64,
        v: &V,
        beta: T,
        y: &mut V,
    ) {
        let n_xv = n_x.min(v.len()).min(y.len());
        for i in 0..n_xv {
            let acc = mass_matrix.get(i).map_or(v[i], |row| {
                (0..n_xv)
                    .filter_map(|j| row.get(j).copied().map(|coeff| (coeff, v[j])))
                    .filter(|(coeff, _)| coeff.abs() > 1.0e-15)
                    .map(|(coeff, vj)| coeff * vj)
                    .sum()
            });
            y[i] = acc + beta * y[i];
        }
        for i in n_x..n_total {
            if i < y.len() && i < v.len() {
                y[i] = algebraic_eps * v[i] + beta * y[i];
            }
        }
    }

    fn eval_root_callback(
        compiled_root_conditions: &CompiledExpressionRows,
        compiled_eval_ctx_root: &CompiledEvalContext,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) {
        if compiled_root_conditions.rows() == 0 {
            if !out.is_empty() {
                out[0] = 1.0;
            }
            return;
        }
        call_compiled_expression_rows(
            compiled_root_conditions,
            compiled_eval_ctx_root,
            y,
            p,
            t,
            out,
        );
    }

    #[derive(Clone)]
    struct CompiledEvalContext {
        dae: Dae,
        sim_context: rumoca_sim_core::runtime::layout::SimulationContext,
    }

    fn call_compiled_residual(
        compiled_residual: &CompiledResidual,
        ctx: &CompiledEvalContext,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) {
        let compiled_p = build_compiled_eval_param_vector(ctx, y, p, t);
        if let Err(err) = compiled_residual.call(y, &compiled_p, t, out) {
            panic!("compiled residual call failed: {err}");
        }
    }

    fn call_compiled_jacobian(
        compiled_jacobian: &CompiledJacobianV,
        ctx: &CompiledEvalContext,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) {
        let compiled_p = build_compiled_eval_param_vector(ctx, y, p, t);
        if let Err(err) = compiled_jacobian.call(y, &compiled_p, t, v, out) {
            panic!("compiled Jacobian-vector call failed: {err}");
        }
    }

    fn call_compiled_expression_rows(
        compiled_rows: &CompiledExpressionRows,
        ctx: &CompiledEvalContext,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) {
        let compiled_p = build_compiled_eval_param_vector(ctx, y, p, t);
        if let Err(err) = compiled_rows.call(y, &compiled_p, t, out) {
            panic!("compiled expression rows call failed: {err}");
        }
    }

    fn build_compiled_eval_param_vector(
        ctx: &CompiledEvalContext,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Vec<f64> {
        ctx.sim_context.compiled_parameter_vector(&ctx.dae, y, p, t)
    }

    fn log_precomputed_synthetic_root_conditions(roots: &[Expression]) {
        if sim_trace_enabled() && !roots.is_empty() {
            eprintln!(
                "[sim-trace] using {} precomputed synthetic root conditions",
                roots.len()
            );
        }
        if sim_introspect_enabled() && !roots.is_empty() {
            for (idx, cond) in roots.iter().enumerate() {
                eprintln!("[sim-introspect] synthetic_root[{idx}] = {cond:?}");
            }
        }
    }
}
