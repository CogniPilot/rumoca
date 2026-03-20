use super::*;
pub(super) struct InitJacobianEvalContext<'a> {
    pub(super) dae: &'a Dae,
    pub(super) y: &'a [f64],
    pub(super) p: &'a [f64],
    pub(super) t_eval: f64,
    pub(super) n_x: usize,
    pub(super) use_initial: bool,
}

pub(super) fn eval_init_jacobian_vector(
    ctx: &InitJacobianEvalContext<'_>,
    v: &[f64],
    out: &mut [f64],
) {
    if ctx.use_initial {
        eval_jacobian_vector_ad_initial(ctx.dae, ctx.y, ctx.p, ctx.t_eval, v, out, ctx.n_x);
    } else {
        eval_jacobian_vector_ad(ctx.dae, ctx.y, ctx.p, ctx.t_eval, v, out, ctx.n_x);
    }
}

pub(super) fn build_param_env(dae: &Dae) -> VarEnv<f64> {
    let mut env = VarEnv::<f64>::new();

    if !dae.functions.is_empty() {
        env.functions = std::sync::Arc::new(rumoca_eval_dae::runtime::collect_user_functions(dae));
    }

    env.dims = std::sync::Arc::new(rumoca_eval_dae::runtime::collect_var_dims(dae));
    env.start_exprs = std::sync::Arc::new(rumoca_eval_dae::runtime::collect_var_starts(dae));
    env.enum_literal_ordinals = std::sync::Arc::new(dae.enum_literal_ordinals.clone());

    for &(fqn, value) in rumoca_eval_dae::runtime::MODELICA_CONSTANTS {
        env.set(fqn, value);
    }

    for _ in 0..2 {
        for (name, var) in &dae.constants {
            let Some(start) = var.start.as_ref() else {
                continue;
            };
            let size = var.size();
            if size <= 1 {
                env.set(name.as_str(), eval_expr::<f64>(start, &env));
                continue;
            }
            let raw = if matches!(start, Expression::Array { .. }) {
                eval_array_values::<f64>(start, &env)
            } else {
                vec![eval_expr::<f64>(start, &env); size]
            };
            let values = expand_values_to_size(raw, size);
            set_array_entries(&mut env, name.as_str(), &var.dims, &values);
        }
    }

    for (name, var) in &dae.parameters {
        let sz = var.size();
        if sz <= 1 {
            let val = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            env.set(name.as_str(), val);
        } else {
            let vals = eval_var_start_values(var, &env);
            set_array_entries(&mut env, name.as_str(), &var.dims, &vals);
        }
    }

    for (name, var) in &dae.parameters {
        let sz = var.size();
        if sz <= 1 {
            let val = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            env.set(name.as_str(), val);
        } else {
            let vals = eval_var_start_values(var, &env);
            set_array_entries(&mut env, name.as_str(), &var.dims, &vals);
        }
    }

    env
}

pub(super) fn extract_direct_assignment(rhs: &Expression) -> Option<(String, &Expression)> {
    crate::runtime::assignment::extract_direct_assignment(rhs)
}

fn direct_assignment_from_equation(eq: &Equation) -> Option<(String, &Expression)> {
    crate::runtime::assignment::direct_assignment_from_equation(eq)
}

pub(crate) fn apply_initial_section_assignments(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> usize {
    crate::apply_initial_section_assignments(dae, y, p, t_eval)
}

pub(super) fn solver_idx_for_target(
    target: &str,
    name_to_idx: &HashMap<String, usize>,
) -> Option<usize> {
    crate::runtime::layout::solver_idx_for_target(target, name_to_idx)
}

pub(super) fn log_ic_direct_seed(name: &str, value: f64) {
    if sim_introspect_enabled() {
        eprintln!("[sim-introspect] IC direct seed {} = {}", name, value);
    }
}

#[cfg(test)]
pub(crate) fn apply_discrete_partition_updates(dae: &Dae, env: &mut VarEnv<f64>) -> bool {
    crate::runtime::discrete::apply_discrete_partition_updates(dae, env)
}

pub(super) fn apply_seeded_values_to_indices(
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    names: &[String],
    indices: &[usize],
    values: &[f64],
    n_x: usize,
) -> (bool, usize) {
    crate::runtime::assignment::apply_seeded_values_to_indices(
        y,
        env,
        names,
        indices,
        values,
        n_x,
        log_ic_direct_seed,
    )
}

#[cfg(test)]
fn apply_runtime_values_to_indices(
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    names: &[String],
    indices: &[usize],
    values: &[f64],
    n_x: usize,
) -> (bool, usize) {
    crate::runtime::assignment::apply_runtime_values_to_indices(y, env, names, indices, values, n_x)
}

#[cfg(test)]
pub(super) fn build_runtime_alias_adjacency(dae: &Dae, n_x: usize) -> HashMap<String, Vec<String>> {
    crate::runtime::alias::build_runtime_alias_adjacency_with_known_assignments(dae, n_x)
}

#[cfg(test)]
pub(super) fn collect_runtime_alias_anchor_names(dae: &Dae, n_x: usize) -> HashSet<String> {
    crate::runtime::alias::collect_runtime_alias_anchor_names(dae, n_x)
}

#[cfg(test)]
pub(crate) fn propagate_runtime_alias_components_from_env(
    dae: &Dae,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    crate::runtime::alias::propagate_runtime_alias_components_from_env(dae, y, n_x, env)
}

pub(super) fn seed_direct_assignment_initial_values(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    use_initial: bool,
    t_eval: f64,
) -> usize {
    seed_direct_assignment_initial_values_with_overrides(
        dae,
        y,
        p,
        n_x,
        None,
        DirectAssignmentSeedOptions {
            use_initial,
            t_eval,
            skip_unknown_alias_pairs: false,
            allow_unsolved_solver_sources: false,
        },
    )
}

pub(super) fn seed_runtime_direct_assignment_values(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
) -> usize {
    seed_direct_assignment_initial_values_with_overrides(
        dae,
        y,
        p,
        n_x,
        None,
        DirectAssignmentSeedOptions {
            use_initial: false,
            t_eval,
            skip_unknown_alias_pairs: true,
            allow_unsolved_solver_sources: true,
        },
    )
}

#[derive(Clone, Copy)]
pub(super) struct DirectAssignmentSeedOptions {
    pub(super) use_initial: bool,
    pub(super) t_eval: f64,
    pub(super) skip_unknown_alias_pairs: bool,
    pub(super) allow_unsolved_solver_sources: bool,
}

type SolverNameIndexMaps = crate::runtime::layout::SolverNameIndexMaps;

fn build_solver_name_index_maps(dae: &Dae, y_len: usize) -> SolverNameIndexMaps {
    crate::runtime::layout::build_solver_name_index_maps(dae, y_len)
}

fn apply_seed_env_overrides(env: &mut VarEnv<f64>, seed_env: Option<&VarEnv<f64>>) {
    let Some(seed_env) = seed_env else {
        return;
    };
    for (name, value) in &seed_env.vars {
        env.set(name, *value);
    }
}

struct DirectSeedPassContext<'a> {
    dae: &'a Dae,
    n_x: usize,
    y_len: usize,
    options: DirectAssignmentSeedOptions,
    names: &'a [String],
    name_to_idx: &'a HashMap<String, usize>,
    base_to_indices: &'a HashMap<String, Vec<usize>>,
    target_assignment_stats:
        &'a HashMap<String, crate::runtime::assignment::DirectAssignmentTargetStats>,
}

fn apply_seed_direct_assignment_equation(
    ctx: &DirectSeedPassContext<'_>,
    eq: &Equation,
    y: &mut [f64],
    env: &mut VarEnv<f64>,
) -> (bool, usize) {
    if eq.origin == "orphaned_variable_pin" {
        return (false, 0);
    }
    let Some((target, solution)) = direct_assignment_from_equation(eq) else {
        return (false, 0);
    };
    let is_alias_solution =
        crate::runtime::assignment::assignment_solution_is_alias_varref(ctx.dae, solution);
    if ctx.options.skip_unknown_alias_pairs && is_alias_solution {
        return (false, 0);
    }
    let source_known = crate::runtime::assignment::direct_assignment_source_is_known(
        ctx.dae,
        solution,
        ctx.n_x,
        ctx.y_len,
        |target| solver_idx_for_target(target, ctx.name_to_idx),
    );
    let trace_target = should_trace_direct_seed_target(target.as_str());
    if trace_target {
        eprintln!(
            "[sim-introspect] runtime direct seed candidate target={} source_known={} allow_unsolved={}",
            target, source_known, ctx.options.allow_unsolved_solver_sources
        );
    }
    let target_stats = ctx
        .target_assignment_stats
        .get(target.as_str())
        .copied()
        .unwrap_or_default();
    if target_stats.total > 1 && target_stats.non_alias != 1 {
        log_runtime_direct_seed_skip_multiple_assignments(
            trace_target,
            target.as_str(),
            target_stats.total,
        );
        return (false, 0);
    }
    if target_stats.total > 1 && target_stats.non_alias == 1 && is_alias_solution {
        return (false, 0);
    }
    if !source_known && !ctx.options.allow_unsolved_solver_sources {
        return (false, 0);
    }

    if !target.contains('[')
        && let Some(indices) = ctx.base_to_indices.get(target.as_str())
        && indices.len() > 1
    {
        let values = evaluate_direct_assignment_values(solution, env, indices.len());
        let (branch_changed, branch_updates) =
            apply_seeded_values_to_indices(y, env, ctx.names, indices, &values, ctx.n_x);
        return (branch_changed, branch_updates);
    }

    let Some(var_idx) = solver_idx_for_target(target.as_str(), ctx.name_to_idx) else {
        log_runtime_direct_seed_skip_no_solver_index(trace_target, target.as_str());
        return (false, 0);
    };
    if var_idx < ctx.n_x || var_idx >= y.len() {
        return (false, 0);
    }

    let value = clamp_finite(eval_expr::<f64>(solution, env));
    if trace_target {
        eprintln!(
            "[sim-introspect] runtime direct seed eval target={} idx={} value={}",
            target, var_idx, value
        );
    }
    if (y[var_idx] - value).abs() <= 1e-12 {
        return (false, 0);
    }

    y[var_idx] = value;
    if let Some(name) = ctx.names.get(var_idx) {
        env.set(name, value);
        log_ic_direct_seed(name, value);
    }
    (true, 1)
}

pub(super) fn seed_direct_assignment_initial_values_with_overrides(
    dae: &Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    seed_env: Option<&VarEnv<f64>>,
    options: DirectAssignmentSeedOptions,
) -> usize {
    if dae.f_x.len() <= n_x || y.is_empty() {
        return 0;
    }

    let SolverNameIndexMaps {
        names,
        name_to_idx,
        base_to_indices,
    } = build_solver_name_index_maps(dae, y.len());
    let target_assignment_stats =
        crate::runtime::assignment::collect_direct_assignment_target_stats(
            dae,
            n_x,
            options.skip_unknown_alias_pairs,
        );
    let pass_ctx = DirectSeedPassContext {
        dae,
        n_x,
        y_len: y.len(),
        options,
        names: &names,
        name_to_idx: &name_to_idx,
        base_to_indices: &base_to_indices,
        target_assignment_stats: &target_assignment_stats,
    };

    let mut updates = 0usize;
    let max_passes = y.len().max(4);
    for _ in 0..max_passes {
        let mut changed = false;
        let mut env = build_env(dae, y, p, options.t_eval);
        env.is_initial = options.use_initial;
        apply_seed_env_overrides(&mut env, seed_env);
        populate_missing_direct_assignment_targets_in_env(dae, &mut env, n_x);

        for eq in dae.f_x.iter().skip(n_x) {
            let (eq_changed, eq_updates) =
                apply_seed_direct_assignment_equation(&pass_ctx, eq, y, &mut env);
            changed |= eq_changed;
            updates += eq_updates;
        }

        if !changed {
            break;
        }
    }
    updates
}

#[cfg(test)]
pub(crate) fn propagate_runtime_direct_assignments_from_env(
    dae: &Dae,
    y: &mut [f64],
    n_x: usize,
    env: &mut VarEnv<f64>,
) -> usize {
    if dae.f_x.len() <= n_x || y.is_empty() {
        return 0;
    }

    let SolverNameIndexMaps {
        names,
        name_to_idx,
        base_to_indices,
    } = build_solver_name_index_maps(dae, y.len());
    let target_assignment_stats =
        crate::runtime::assignment::collect_direct_assignment_target_stats(dae, n_x, true);

    let mut updates = 0usize;
    let max_passes = y.len().max(4);
    for _ in 0..max_passes {
        let mut changed = false;
        for eq in dae.f_x.iter().skip(n_x) {
            if eq.origin == "orphaned_variable_pin" {
                continue;
            }
            let Some((target, solution)) = direct_assignment_from_equation(eq) else {
                continue;
            };
            let is_alias_solution =
                crate::runtime::assignment::assignment_solution_is_alias_varref(dae, solution);
            if is_alias_solution {
                continue;
            }
            let target_stats = target_assignment_stats
                .get(target.as_str())
                .copied()
                .unwrap_or_default();
            if target_stats.total > 1 && target_stats.non_alias != 1 {
                maybe_log_runtime_direct_propagation_skip(target.as_str(), target_stats.total);
                continue;
            }

            if !target.contains('[')
                && let Some(indices) = base_to_indices.get(target.as_str())
                && indices.len() > 1
            {
                let values = evaluate_direct_assignment_values(solution, env, indices.len());
                let (branch_changed, branch_updates) =
                    apply_runtime_values_to_indices(y, env, &names, indices, &values, n_x);
                changed |= branch_changed;
                updates += branch_updates;
                continue;
            }

            let value = clamp_finite(eval_expr::<f64>(solution, env));
            if let Some(var_idx) = solver_idx_for_target(target.as_str(), &name_to_idx)
                && var_idx >= n_x
                && var_idx < y.len()
                && (y[var_idx] - value).abs() > 1e-12
            {
                y[var_idx] = value;
                changed = true;
                updates += 1;
            }
            if env
                .vars
                .get(target.as_str())
                .is_none_or(|existing| (existing - value).abs() > 1e-12)
            {
                env.set(target.as_str(), value);
                changed = true;
                updates += 1;
            }
        }
        if !changed {
            break;
        }
    }

    updates
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn build_problem(
    dae: &Dae,
    rtol: f64,
    atol: f64,
    algebraic_eps: f64,
    mass_matrix: &crate::with_diffsol::MassMatrix,
) -> Result<OdeSolverProblem<impl OdeEquationsImplicit<M = M, V = V, T = T, C = C>>, SimError> {
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
                crate::with_diffsol::integration::panic_on_expired_solver_deadline();
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
                crate::with_diffsol::integration::panic_on_expired_solver_deadline();
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
            crate::with_diffsol::integration::panic_on_expired_solver_deadline();
            apply_mass_matrix_update(&mass_matrix_owned, n_x, n_total, algebraic_eps, v, beta, y);
        })
        .init(
            move |_p: &V, _t: T, y: &mut V| {
                crate::with_diffsol::integration::panic_on_expired_solver_deadline();
                initialize_state_vector(&dae_init, y.as_mut_slice())
            },
            n_total.max(1),
        )
        .root(
            move |y: &V, p: &V, t: T, out: &mut V| {
                crate::with_diffsol::integration::panic_on_expired_solver_deadline();
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

#[cfg(target_arch = "wasm32")]
pub(crate) fn build_problem(
    dae: &Dae,
    rtol: f64,
    atol: f64,
    algebraic_eps: f64,
    mass_matrix: &crate::with_diffsol::MassMatrix,
) -> Result<OdeSolverProblem<impl OdeEquationsImplicit<M = M, V = V, T = T, C = C>>, SimError> {
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
                crate::with_diffsol::integration::panic_on_expired_solver_deadline();
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
                crate::with_diffsol::integration::panic_on_expired_solver_deadline();
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
            crate::with_diffsol::integration::panic_on_expired_solver_deadline();
            apply_mass_matrix_update(&mass_matrix_owned, n_x, n_total, algebraic_eps, v, beta, y);
        })
        .init(
            move |_p: &V, _t: T, y: &mut V| {
                crate::with_diffsol::integration::panic_on_expired_solver_deadline();
                initialize_state_vector(&dae_init, y.as_mut_slice())
            },
            n_total.max(1),
        )
        .root(
            move |y: &V, p: &V, t: T, out: &mut V| {
                crate::with_diffsol::integration::panic_on_expired_solver_deadline();
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

#[cfg(not(target_arch = "wasm32"))]
struct ProblemCompiledKernels {
    compiled_eval_ctx_rhs: CompiledEvalContext,
    compiled_eval_ctx_jac: CompiledEvalContext,
    compiled_eval_ctx_root: CompiledEvalContext,
    compiled_residual: rumoca_eval_dae::compiled::CompiledResidual,
    compiled_jacobian: rumoca_eval_dae::compiled::CompiledJacobianV,
    compiled_root_conditions: rumoca_eval_dae::compiled::CompiledExpressionRows,
    n_roots: usize,
}

#[cfg(target_arch = "wasm32")]
struct ProblemCompiledKernels {
    compiled_eval_ctx_rhs: CompiledEvalContext,
    compiled_eval_ctx_jac: CompiledEvalContext,
    compiled_eval_ctx_root: CompiledEvalContext,
    compiled_residual: rumoca_eval_dae::compiled::CompiledResidualWasm,
    compiled_jacobian: rumoca_eval_dae::compiled::CompiledJacobianVWasm,
    compiled_root_conditions: rumoca_eval_dae::compiled::CompiledExpressionRowsWasm,
    n_roots: usize,
}

#[cfg(not(target_arch = "wasm32"))]
fn compile_problem_kernels(dae: &Dae, n_total: usize) -> Result<ProblemCompiledKernels, SimError> {
    let sim_context = crate::runtime::layout::SimulationContext::from_dae(dae, n_total);
    let compiled_eval_ctx = CompiledEvalContext {
        dae: dae.clone(),
        sim_context,
    };
    let compiled_eval_ctx_rhs = compiled_eval_ctx.clone();
    let compiled_eval_ctx_jac = compiled_eval_ctx.clone();
    let compiled_eval_ctx_root = compiled_eval_ctx.clone();

    let compiled_residual = rumoca_eval_dae::compiled::compile_residual(
        dae,
        rumoca_eval_dae::compiled::Backend::Cranelift,
    )
    .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    let compiled_jacobian = rumoca_eval_dae::compiled::compile_jacobian_v(
        dae,
        rumoca_eval_dae::compiled::Backend::Cranelift,
    )
    .map_err(|err| SimError::CompiledEval(err.to_string()))?;

    log_precomputed_synthetic_root_conditions(&dae.synthetic_root_conditions);
    let compiled_root_conditions = rumoca_eval_dae::compiled::compile_root_conditions(
        dae,
        rumoca_eval_dae::compiled::Backend::Cranelift,
    )
    .map_err(|err| SimError::CompiledEval(err.to_string()))?;
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

#[cfg(target_arch = "wasm32")]
fn compile_problem_kernels(dae: &Dae, n_total: usize) -> Result<ProblemCompiledKernels, SimError> {
    let sim_context = crate::runtime::layout::SimulationContext::from_dae(dae, n_total);
    let compiled_eval_ctx = CompiledEvalContext {
        dae: dae.clone(),
        sim_context,
    };
    let compiled_eval_ctx_rhs = compiled_eval_ctx.clone();
    let compiled_eval_ctx_jac = compiled_eval_ctx.clone();
    let compiled_eval_ctx_root = compiled_eval_ctx.clone();

    let compiled_residual = rumoca_eval_dae::compiled::compile_residual_wasm(dae)
        .map_err(|err| SimError::CompiledEval(err.to_string()))?;
    let compiled_jacobian = rumoca_eval_dae::compiled::compile_jacobian_v_wasm(dae)
        .map_err(|err| SimError::CompiledEval(err.to_string()))?;

    log_precomputed_synthetic_root_conditions(&dae.synthetic_root_conditions);
    let compiled_root_conditions = rumoca_eval_dae::compiled::compile_root_conditions_wasm(dae)
        .map_err(|err| SimError::CompiledEval(err.to_string()))?;
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

fn apply_mass_matrix_update(
    mass_matrix: &crate::with_diffsol::MassMatrix,
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
                .filter(|(coeff, _)| coeff.abs() > 1e-15)
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

#[cfg(not(target_arch = "wasm32"))]
fn eval_root_callback(
    compiled_root_conditions: &rumoca_eval_dae::compiled::CompiledExpressionRows,
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

#[cfg(target_arch = "wasm32")]
fn eval_root_callback(
    compiled_root_conditions: &rumoca_eval_dae::compiled::CompiledExpressionRowsWasm,
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
    sim_context: crate::runtime::layout::SimulationContext,
}

#[cfg(not(target_arch = "wasm32"))]
fn call_compiled_residual(
    compiled_residual: &rumoca_eval_dae::compiled::CompiledResidual,
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

#[cfg(target_arch = "wasm32")]
fn call_compiled_residual(
    compiled_residual: &rumoca_eval_dae::compiled::CompiledResidualWasm,
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

#[cfg(not(target_arch = "wasm32"))]
fn call_compiled_jacobian(
    compiled_jacobian: &rumoca_eval_dae::compiled::CompiledJacobianV,
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

#[cfg(target_arch = "wasm32")]
fn call_compiled_jacobian(
    compiled_jacobian: &rumoca_eval_dae::compiled::CompiledJacobianVWasm,
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

#[cfg(not(target_arch = "wasm32"))]
fn call_compiled_expression_rows(
    compiled_rows: &rumoca_eval_dae::compiled::CompiledExpressionRows,
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

#[cfg(target_arch = "wasm32")]
fn call_compiled_expression_rows(
    compiled_rows: &rumoca_eval_dae::compiled::CompiledExpressionRowsWasm,
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

pub(super) fn clamp_finite(v: f64) -> f64 {
    if v.is_finite() { v } else { 0.0 }
}

pub(super) fn find_fixed_state_indices(dae: &Dae) -> Vec<bool> {
    let mut fixed = Vec::new();
    for (_name, var) in dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
    {
        let is_fixed = var.fixed == Some(true);
        for _ in 0..var.size() {
            fixed.push(is_fixed);
        }
    }
    fixed
}

pub(super) fn solver_vector_names(dae: &Dae, n_total: usize) -> Vec<String> {
    crate::runtime::layout::solver_vector_names(dae, n_total)
}

pub(super) fn log_init_linear_system_diagnostics(
    dae: &Dae,
    jac: &nalgebra::DMatrix<f64>,
    rhs: &[f64],
    n_x: usize,
) {
    if !sim_introspect_enabled() {
        return;
    }

    let n = jac.nrows().min(jac.ncols());
    let names = solver_vector_names(dae, rhs.len());

    let near_zero = 1e-12;
    let mut near_zero_rows = Vec::new();
    let mut near_zero_cols = Vec::new();

    for i in 0..n {
        let mut row_max = 0.0_f64;
        let mut col_max = 0.0_f64;
        for j in 0..n {
            row_max = row_max.max(jac[(i, j)].abs());
            col_max = col_max.max(jac[(j, i)].abs());
        }
        if row_max <= near_zero {
            near_zero_rows.push((i, row_max));
        }
        if col_max <= near_zero {
            near_zero_cols.push((i, col_max));
        }
    }

    eprintln!(
        "[sim-introspect] IC Jacobian diagnostics: n={} near_zero_rows={} near_zero_cols={}",
        n,
        near_zero_rows.len(),
        near_zero_cols.len()
    );

    for (i, _row_max) in near_zero_rows.iter().take(24) {
        let eq = dae
            .f_x
            .get(*i)
            .map(|eq| eq.origin.as_str())
            .unwrap_or("<missing-eq>");
        let r = rhs.get(*i).copied().unwrap_or(0.0);
        eprintln!(
            "[sim-introspect] IC Jacobian near-zero row[{i}] residual={} origin={}",
            r, eq
        );
    }

    for (i, _col_max) in near_zero_cols.iter().take(24) {
        let name = names.get(*i).map(String::as_str).unwrap_or("<unnamed>");
        let kind = if *i < n_x {
            "state"
        } else {
            "algebraic/output"
        };
        eprintln!(
            "[sim-introspect] IC Jacobian near-zero col[{i}] {} ({})",
            name, kind
        );
    }

    let mut worst: Vec<(usize, f64)> = rhs.iter().enumerate().map(|(i, v)| (i, v.abs())).collect();
    worst.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
    for (i, abs_r) in worst.into_iter().take(8) {
        let eq = dae
            .f_x
            .get(i)
            .map(|eq| eq.origin.as_str())
            .unwrap_or("<missing-eq>");
        eprintln!(
            "[sim-introspect] IC residual top eq[{i}] abs={} origin={}",
            abs_r, eq
        );
    }
}

pub(super) fn build_init_jacobian_dense(
    ctx: &InitJacobianEvalContext<'_>,
    fixed: &[bool],
    timeout: &crate::TimeoutBudget,
) -> Result<nalgebra::DMatrix<f64>, crate::SimError> {
    let n_total = ctx.y.len();
    let mut jac = nalgebra::DMatrix::<f64>::zeros(n_total, n_total);
    let mut v = vec![0.0; n_total];
    let mut jv = vec![0.0; n_total];

    for j in 0..n_total {
        timeout.check()?;
        if j < fixed.len() && fixed[j] {
            continue;
        }
        v.fill(0.0);
        jv.fill(0.0);
        v[j] = 1.0;
        eval_init_jacobian_vector(ctx, &v, &mut jv);
        for i in 0..n_total {
            jac[(i, j)] = clamp_finite(jv[i]);
        }
    }
    lock_fixed_state_rows_and_cols(&mut jac, fixed);
    Ok(jac)
}

pub(super) fn build_init_jacobian_colored(
    ctx: &InitJacobianEvalContext<'_>,
    fixed: &[bool],
    timeout: &crate::TimeoutBudget,
) -> Result<Option<nalgebra::DMatrix<f64>>, crate::SimError> {
    let n_total = ctx.y.len();
    let active_cols = active_init_columns(n_total, ctx.n_x, fixed);
    if active_cols.is_empty() {
        return Ok(Some(nalgebra::DMatrix::<f64>::zeros(n_total, n_total)));
    }

    let column_sparsity = structural_column_sparsity(ctx.dae, &active_cols, n_total);
    if sim_trace_enabled() && runtime_ic_sparsity_validation_enabled() {
        let runtime_column_sparsity = detect_init_jacobian_sparsity(ctx, &active_cols, timeout)?;
        let report = validate_solver_sparsity(ctx.dae, &active_cols, &runtime_column_sparsity, 12);
        log_init_sparsity_validation(ctx.dae, &report, n_total);
    }
    let colors = greedy_column_coloring(&column_sparsity);

    if sim_trace_enabled() {
        let nnz_estimate: usize = column_sparsity.iter().map(Vec::len).sum();
        eprintln!(
            "[sim-trace] IC Jacobian coloring active_cols={} colors={} nnz_pattern={} mode=structural",
            active_cols.len(),
            colors.len(),
            nnz_estimate
        );
    }

    let mut jac = nalgebra::DMatrix::<f64>::zeros(n_total, n_total);
    let mut v = vec![0.0; n_total];
    let mut jv = vec![0.0; n_total];
    let mut row_owner = vec![None; n_total];

    for color in &colors {
        timeout.check()?;
        v.fill(0.0);
        jv.fill(0.0);
        row_owner.fill(None);

        for &compact_col in color {
            let col = active_cols[compact_col];
            v[col] = 1.0;
            if !assign_color_rows(&mut row_owner, &column_sparsity[compact_col], col) {
                log_coloring_fallback("row conflict detected");
                return Ok(None);
            }
        }

        eval_init_jacobian_vector(ctx, &v, &mut jv);

        let mut unmapped_nonzero = false;
        for (row, value) in jv.iter().enumerate() {
            if let Some(col) = row_owner[row] {
                jac[(row, col)] = clamp_finite(*value);
            } else if value.is_finite() && value.abs() > 1e-10 {
                unmapped_nonzero = true;
            }
        }

        if unmapped_nonzero {
            log_coloring_fallback("unmapped nonzero contribution");
            return Ok(None);
        }
    }

    lock_fixed_state_rows_and_cols(&mut jac, fixed);
    Ok(Some(jac))
}

pub(super) fn get_init_value(var: &rumoca_ir_dae::Variable, env: &VarEnv<f64>) -> f64 {
    if let Some(ref start) = var.start {
        return eval_expr::<f64>(start, env);
    }
    if let Some(ref nominal) = var.nominal {
        return eval_expr::<f64>(nominal, env);
    }
    0.0
}

pub(crate) fn initialize_state_vector(dae: &Dae, y: &mut [f64]) {
    let env = build_param_env(dae);
    let mut idx = 0;
    for var in dae.states.values() {
        let val = get_init_value(var, &env);
        for _ in 0..var.size() {
            if idx < y.len() {
                y[idx] = val;
            }
            idx += 1;
        }
    }
    for var in dae.algebraics.values() {
        let val = get_init_value(var, &env);
        for _ in 0..var.size() {
            if idx < y.len() {
                y[idx] = val;
            }
            idx += 1;
        }
    }
    for var in dae.outputs.values() {
        let val = get_init_value(var, &env);
        for _ in 0..var.size() {
            if idx < y.len() {
                y[idx] = val;
            }
            idx += 1;
        }
    }
}

#[cfg(test)]
pub(super) fn introspect_target_match(target: &str) -> bool {
    let Ok(raw) = std::env::var("RUMOCA_SIM_INTROSPECT_TARGET_MATCH") else {
        return false;
    };
    raw.split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .any(|pat| target.contains(pat))
}

pub(super) fn should_trace_direct_seed_target(target: &str) -> bool {
    if !sim_introspect_enabled() {
        return false;
    }
    let Ok(raw) = std::env::var("RUMOCA_SIM_INTROSPECT_TARGET_MATCH") else {
        return true;
    };
    raw.split(',')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .any(|pat| target.contains(pat))
}

pub(super) fn log_runtime_direct_seed_skip_no_solver_index(trace_target: bool, target: &str) {
    if !trace_target {
        return;
    }
    eprintln!(
        "[sim-introspect] runtime direct seed skipped target={} (no solver index)",
        target
    );
}

pub(super) fn log_runtime_direct_seed_skip_multiple_assignments(
    trace_target: bool,
    target: &str,
    assignment_count: usize,
) {
    if !trace_target {
        return;
    }
    eprintln!(
        "[sim-introspect] runtime direct seed skipped target={} ({} defining direct assignments)",
        target, assignment_count
    );
}

#[cfg(test)]
pub(super) fn maybe_log_runtime_direct_propagation_skip(target: &str, assignment_count: usize) {
    if !(sim_introspect_enabled() && introspect_target_match(target)) {
        return;
    }
    eprintln!(
        "[sim-introspect] runtime direct propagation skipped target={} ({} defining direct assignments)",
        target, assignment_count
    );
}

pub(super) fn log_init_sparsity_validation(dae: &Dae, report: &SparsityValidation, n_total: usize) {
    if !sim_trace_enabled() {
        return;
    }

    if !report.has_mismatch() {
        if sim_introspect_enabled() {
            eprintln!(
                "[sim-introspect] IC Jacobian sparsity validated: structural_nnz={} runtime_nnz={}",
                report.structural_nnz, report.runtime_nnz
            );
        }
        return;
    }

    eprintln!(
        "[sim-trace] IC Jacobian sparsity mismatch: structural_nnz={} runtime_nnz={} missing={} extra={}",
        report.structural_nnz, report.runtime_nnz, report.missing_count, report.extra_count
    );

    if !sim_introspect_enabled() {
        return;
    }

    let names = solver_vector_names(dae, n_total);
    for (row, col) in &report.missing_samples {
        let eq_origin = dae
            .f_x
            .get(*row)
            .map(|eq| eq.origin.as_str())
            .unwrap_or("<missing-eq>");
        let var_name = names
            .get(*col)
            .map(String::as_str)
            .unwrap_or("<unknown-col>");
        eprintln!(
            "[sim-introspect] IC sparsity missing structural entry row={} col={} ({}) origin={}",
            row, col, var_name, eq_origin
        );
    }

    for (row, col) in &report.extra_samples {
        let eq_origin = dae
            .f_x
            .get(*row)
            .map(|eq| eq.origin.as_str())
            .unwrap_or("<missing-eq>");
        let var_name = names
            .get(*col)
            .map(String::as_str)
            .unwrap_or("<unknown-col>");
        eprintln!(
            "[sim-introspect] IC sparsity runtime-only entry row={} col={} ({}) origin={}",
            row, col, var_name, eq_origin
        );
    }
}

pub(super) fn active_init_columns(n_total: usize, _n_x: usize, fixed: &[bool]) -> Vec<usize> {
    (0..n_total)
        .filter(|&j| !(j < fixed.len() && fixed[j]))
        .collect()
}

pub(super) fn detect_init_jacobian_sparsity(
    ctx: &InitJacobianEvalContext<'_>,
    active_cols: &[usize],
    timeout: &crate::TimeoutBudget,
) -> Result<Vec<Vec<usize>>, crate::SimError> {
    let n_total = ctx.y.len();
    let mut v = vec![0.0; n_total];
    let mut jv = vec![0.0; n_total];
    let mut column_rows = vec![Vec::new(); active_cols.len()];

    for (compact_col, &col) in active_cols.iter().enumerate() {
        timeout.check()?;
        v.fill(0.0);
        jv.fill(0.0);
        v[col] = f64::NAN;
        eval_init_jacobian_vector(ctx, &v, &mut jv);

        let rows = &mut column_rows[compact_col];
        for (row, value) in jv.iter().enumerate() {
            if !value.is_finite() {
                rows.push(row);
            }
        }
    }

    Ok(column_rows)
}

pub(super) fn runtime_ic_sparsity_validation_enabled() -> bool {
    std::env::var("RUMOCA_SIM_VALIDATE_RUNTIME_SPARSITY")
        .map(|v| {
            let s = v.trim().to_ascii_lowercase();
            !s.is_empty() && s != "0" && s != "false" && s != "no"
        })
        .unwrap_or(false)
}

pub(super) fn lock_fixed_state_rows_and_cols(jac: &mut nalgebra::DMatrix<f64>, fixed: &[bool]) {
    let n_total = jac.nrows();
    for (idx, is_fixed) in fixed.iter().copied().enumerate() {
        if !is_fixed || idx >= n_total {
            continue;
        }
        for col in 0..n_total {
            jac[(idx, col)] = 0.0;
            jac[(col, idx)] = 0.0;
        }
        jac[(idx, idx)] = 1.0;
    }
}

pub(super) fn log_coloring_fallback(reason: &str) {
    if sim_trace_enabled() {
        eprintln!("[sim-trace] IC Jacobian coloring fallback: {reason}");
    }
}

pub(super) fn assign_row_owner(
    row_owner: &mut [Option<usize>],
    row: usize,
    col: usize,
) -> Result<(), ()> {
    if row_owner[row].is_some() {
        return Err(());
    }
    row_owner[row] = Some(col);
    Ok(())
}

pub(super) fn assign_color_rows(
    row_owner: &mut [Option<usize>],
    rows: &[usize],
    col: usize,
) -> bool {
    for &row in rows {
        if assign_row_owner(row_owner, row, col).is_err() {
            return false;
        }
    }
    true
}
