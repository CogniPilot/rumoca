use super::*;
pub(super) struct InitJacobianEvalContext<'a> {
    pub(super) dae: &'a Dae,
    pub(super) y: &'a [f64],
    pub(super) p: &'a [f64],
    pub(super) t_eval: f64,
    pub(super) n_x: usize,
    pub(super) use_initial: bool,
    pub(super) compiled_initial: Option<&'a CompiledInitialNewtonContext>,
    pub(super) compiled_runtime: Option<&'a CompiledRuntimeNewtonContext>,
}

pub(super) fn eval_init_jacobian_vector(
    ctx: &InitJacobianEvalContext<'_>,
    v: &[f64],
    out: &mut [f64],
) {
    if ctx.use_initial {
        // MLS §8.6: initial() is true during the initialization phase, so the
        // IC Jacobian must come from initial-mode compiled kernels.
        let compiled_initial = ctx
            .compiled_initial
            .expect("compiled initial Newton context required for initial Jacobian evaluation");
        eval_compiled_initial_jacobian(compiled_initial, ctx.y, ctx.p, ctx.t_eval, v, out);
    } else {
        let compiled_runtime = ctx
            .compiled_runtime
            .expect("compiled runtime Newton context required for non-initial Jacobian evaluation");
        eval_compiled_runtime_jacobian(compiled_runtime, ctx.y, ctx.p, ctx.t_eval, v, out);
    }
}

#[path = "seed.rs"]
mod seed;
pub(crate) use seed::*;

#[cfg(test)]
pub(crate) fn build_problem(
    dae: &Dae,
    rtol: f64,
    atol: f64,
    algebraic_eps: f64,
    mass_matrix: &crate::MassMatrix,
) -> Result<OdeSolverProblem<impl OdeEquationsImplicit<M = M, V = V, T = T, C = C>>, SimError> {
    let params = default_params(dae);
    build_problem_with_overrides_and_params(
        dae,
        rtol,
        atol,
        algebraic_eps,
        mass_matrix,
        &params,
        None,
    )
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn build_problem_with_params(
    dae: &Dae,
    rtol: f64,
    atol: f64,
    algebraic_eps: f64,
    mass_matrix: &crate::MassMatrix,
    param_values: &[f64],
) -> Result<OdeSolverProblem<impl OdeEquationsImplicit<M = M, V = V, T = T, C = C>>, SimError> {
    build_problem_with_overrides_and_params(
        dae,
        rtol,
        atol,
        algebraic_eps,
        mass_matrix,
        param_values,
        None,
    )
}

#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn build_problem_with_overrides_and_params(
    dae: &Dae,
    rtol: f64,
    atol: f64,
    algebraic_eps: f64,
    mass_matrix: &crate::MassMatrix,
    param_values: &[f64],
    input_overrides: Option<SharedInputOverrides>,
) -> Result<OdeSolverProblem<impl OdeEquationsImplicit<M = M, V = V, T = T, C = C> + use<>>, SimError>
{
    let n_x = count_states(dae);
    let n_eq = dae.f_x.len();
    let n_z = n_eq - n_x;
    let n_total = n_x + n_z;
    let params = param_values.to_vec();

    let dae_init = dae.clone();
    let ProblemCompiledKernels {
        mut compiled_eval_ctx_rhs,
        mut compiled_eval_ctx_jac,
        mut compiled_eval_ctx_root,
        compiled_residual,
        compiled_jacobian,
        compiled_root_conditions,
        n_roots,
    } = compile_problem_kernels(dae, n_total)?;
    if let Some(ref overrides) = input_overrides {
        compiled_eval_ctx_rhs.input_overrides = Some(overrides.clone());
        compiled_eval_ctx_jac.input_overrides = Some(overrides.clone());
        compiled_eval_ctx_root.input_overrides = Some(overrides.clone());
    }

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
            apply_mass_matrix_update(&mass_matrix_owned, n_x, n_total, algebraic_eps, v, beta, y);
        })
        .init(
            move |p: &V, _t: T, y: &mut V| {
                crate::integration::panic_on_expired_solver_deadline();
                initialize_state_vector_with_params(&dae_init, y.as_mut_slice(), p.as_slice())
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

#[cfg(target_arch = "wasm32")]
pub(crate) fn build_problem_with_params(
    dae: &Dae,
    rtol: f64,
    atol: f64,
    algebraic_eps: f64,
    mass_matrix: &crate::MassMatrix,
    param_values: &[f64],
) -> Result<OdeSolverProblem<impl OdeEquationsImplicit<M = M, V = V, T = T, C = C>>, SimError> {
    build_problem_with_overrides_and_params(
        dae,
        rtol,
        atol,
        algebraic_eps,
        mass_matrix,
        param_values,
        None,
    )
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn build_problem_with_overrides_and_params(
    dae: &Dae,
    rtol: f64,
    atol: f64,
    algebraic_eps: f64,
    mass_matrix: &crate::MassMatrix,
    param_values: &[f64],
    input_overrides: Option<SharedInputOverrides>,
) -> Result<OdeSolverProblem<impl OdeEquationsImplicit<M = M, V = V, T = T, C = C> + use<>>, SimError>
{
    let n_x = count_states(dae);
    let n_eq = dae.f_x.len();
    let n_z = n_eq - n_x;
    let n_total = n_x + n_z;
    let params = param_values.to_vec();

    let dae_init = dae.clone();
    let ProblemCompiledKernels {
        mut compiled_eval_ctx_rhs,
        mut compiled_eval_ctx_jac,
        mut compiled_eval_ctx_root,
        compiled_residual,
        compiled_jacobian,
        compiled_root_conditions,
        n_roots,
    } = compile_problem_kernels(dae, n_total)?;
    if let Some(ref overrides) = input_overrides {
        compiled_eval_ctx_rhs.input_overrides = Some(overrides.clone());
        compiled_eval_ctx_jac.input_overrides = Some(overrides.clone());
        compiled_eval_ctx_root.input_overrides = Some(overrides.clone());
    }

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
            apply_mass_matrix_update(&mass_matrix_owned, n_x, n_total, algebraic_eps, v, beta, y);
        })
        .init(
            move |p: &V, _t: T, y: &mut V| {
                crate::integration::panic_on_expired_solver_deadline();
                initialize_state_vector_with_params(&dae_init, y.as_mut_slice(), p.as_slice())
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
    let compiled_eval_ctx = build_compiled_eval_context(dae, n_total);
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
    let compiled_eval_ctx = build_compiled_eval_context(dae, n_total);
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

fn equation_key(eq: &Equation) -> usize {
    eq as *const Equation as usize
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
    rumoca_sim::runtime::layout::solver_vector_names(dae, n_total)
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
    fixed_cols: &[bool],
    timeout: &rumoca_sim::TimeoutBudget,
) -> Result<nalgebra::DMatrix<f64>, crate::SimError> {
    let n_total = ctx.y.len();
    let mut jac = nalgebra::DMatrix::<f64>::zeros(n_total, n_total);
    let mut v = vec![0.0; n_total];
    let mut jv = vec![0.0; n_total];

    for j in 0..n_total {
        timeout.check()?;
        if j < fixed_cols.len() && fixed_cols[j] {
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
    Ok(jac)
}

pub(super) fn build_init_jacobian_colored(
    ctx: &InitJacobianEvalContext<'_>,
    fixed_cols: &[bool],
    timeout: &rumoca_sim::TimeoutBudget,
) -> Result<Option<nalgebra::DMatrix<f64>>, crate::SimError> {
    let n_total = ctx.y.len();
    let active_cols = active_init_columns(n_total, ctx.n_x, fixed_cols);
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

    Ok(Some(jac))
}

fn collect_init_value_expressions(dae: &Dae) -> Vec<dae::Expression> {
    let scalarization = rumoca_sim::equation_scalarize::build_expression_scalarization_context(dae);
    dae.states
        .values()
        .chain(dae.algebraics.values())
        .chain(dae.outputs.values())
        .flat_map(|var| {
            let expr = var
                .start
                .as_ref()
                .or(var.nominal.as_ref())
                .cloned()
                .unwrap_or(dae::Expression::Literal(dae::Literal::Real(0.0)));
            rumoca_sim::equation_scalarize::scalarize_expression_rows(
                &expr,
                var.size(),
                &scalarization,
            )
        })
        .collect()
}

fn write_init_values_from_slice(y: &mut [f64], values: &[f64]) {
    for (idx, value) in values.iter().copied().enumerate().take(y.len()) {
        y[idx] = clamp_finite(value);
    }
}

fn reference_init_value_values(dae: &Dae, env: &VarEnv<f64>) -> Vec<f64> {
    dae.states
        .values()
        .chain(dae.algebraics.values())
        .chain(dae.outputs.values())
        .flat_map(|var| {
            let expr = var
                .start
                .as_ref()
                .or(var.nominal.as_ref())
                .cloned()
                .unwrap_or(dae::Expression::Literal(dae::Literal::Real(0.0)));
            let size = var.size();
            if size <= 1 {
                return vec![rumoca_eval_dae::runtime::eval_expr::<f64>(&expr, env)];
            }
            let raw = rumoca_eval_dae::runtime::eval_array_values::<f64>(&expr, env);
            super::expand_values_to_size(raw, size)
        })
        .collect()
}

#[cfg(test)]
pub(crate) fn initialize_state_vector(dae: &Dae, y: &mut [f64]) {
    let p = default_params(dae);
    initialize_state_vector_with_params(dae, y, &p);
}

pub(crate) fn initialize_state_vector_with_params(dae: &Dae, y: &mut [f64], p: &[f64]) {
    let env = rumoca_eval_dae::runtime::build_runtime_parameter_tail_env(dae, p, 0.0);
    let expressions = collect_init_value_expressions(dae);
    if expressions.is_empty() {
        return;
    }

    let compiled =
        build_compiled_runtime_expression_context_for_start_rows(dae, y.len(), &expressions, false);
    let Ok(compiled) = compiled else {
        let values = reference_init_value_values(dae, &env);
        write_init_values_from_slice(y, &values);
        return;
    };
    let zero_y = vec![0.0; y.len()];
    let mut y_scratch = Vec::with_capacity(y.len());
    let mut out_scratch = Vec::new();
    let values = eval_compiled_runtime_expressions_from_env(
        &compiled,
        &zero_y,
        &env,
        p,
        0.0,
        &mut y_scratch,
        &mut out_scratch,
    );
    write_init_values_from_slice(y, values);
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

pub(super) fn active_init_columns(n_total: usize, _n_x: usize, fixed_cols: &[bool]) -> Vec<usize> {
    (0..n_total)
        .filter(|&j| !(j < fixed_cols.len() && fixed_cols[j]))
        .collect()
}

pub(super) fn detect_init_jacobian_sparsity(
    ctx: &InitJacobianEvalContext<'_>,
    active_cols: &[usize],
    timeout: &rumoca_sim::TimeoutBudget,
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
