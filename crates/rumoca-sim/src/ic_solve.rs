//! Runtime BLT-sequential IC solver.
//!
//! Walks an `IcBlock` plan and solves each block in order using:
//! - Direct symbolic evaluation for `ScalarDirect`
//! - Single-variable Newton for `ScalarNewton`
//! - Levenberg-Marquardt for `TornBlock` and `CoupledLM`

use levenberg_marquardt::{LeastSquaresProblem, LevenbergMarquardt};
use nalgebra::{Dyn, OMatrix, OVector, U1, Vector};
#[cfg(target_arch = "wasm32")]
use instant::Instant;
#[cfg(not(target_arch = "wasm32"))]
use std::time::Instant;

use rumoca_eval_dae::runtime::dual::Dual;
use rumoca_eval_dae::runtime::sim_float::SimFloat;
use rumoca_eval_dae::runtime::{VarEnv, build_env, eval_array_values, eval_expr, lift_env};
use rumoca_ir_dae as dae;
use rumoca_phase_solve::{CausalStep, IcBlock};

type Dae = dae::Dae;
type Expression = dae::Expression;

/// BLT IC-solver failure modes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IcSolveError {
    /// IC solving exceeded the caller-provided wall-clock deadline.
    Timeout,
}

fn is_timed_out(deadline: Option<Instant>) -> bool {
    deadline.is_some_and(|d| Instant::now() >= d)
}

fn check_timeout(deadline: Option<Instant>) -> Result<(), IcSolveError> {
    if is_timed_out(deadline) {
        return Err(IcSolveError::Timeout);
    }
    Ok(())
}

fn is_valid_var_idx(y: &[f64], idx: usize) -> bool {
    idx < y.len()
}

fn is_valid_eq_idx(dae: &Dae, eq_idx: usize) -> bool {
    eq_idx < dae.f_x.len()
}

fn has_valid_causal_indices(dae: &Dae, y: &[f64], causal_steps: &[CausalStep]) -> bool {
    causal_steps.iter().all(|step| {
        is_valid_var_idx(y, step.var_idx)
            && (step.solution_expr.is_some() || is_valid_eq_idx(dae, step.eq_idx))
    })
}

fn update_env_from_indices(y: &[f64], env: &mut VarEnv<f64>, indices: &[usize], names: &[String]) {
    for (&idx, name) in indices.iter().zip(names.iter()) {
        if idx < y.len() {
            update_env_var(env, name, y[idx]);
        }
    }
}

fn update_env_from_causal_steps(y: &[f64], env: &mut VarEnv<f64>, causal_steps: &[CausalStep]) {
    for step in causal_steps {
        if step.var_idx < y.len() {
            update_env_var(env, &step.var_name, y[step.var_idx]);
        }
    }
}

/// Apply sign convention for the mass-matrix DAE formulation.
///
/// State rows (i < n_x): negate residual because `der(x) - g = 0` at `der=0`
/// gives `-g`, so `f = -residual = g`.
/// Algebraic rows (i >= n_x): use residual directly (`0 = h`).
fn apply_dae_sign<S: SimFloat>(val: S, i: usize, n_x: usize) -> S {
    if i < n_x { -val } else { val }
}

fn fill_jacobian_column(
    dae: &Dae,
    residual_eq_indices: &[usize],
    env_dual: &VarEnv<Dual>,
    n_x: usize,
    jac: &mut OMatrix<f64, Dyn, Dyn>,
    col: usize,
) {
    for (row, &eq_idx) in residual_eq_indices.iter().enumerate() {
        let du = dae
            .f_x
            .get(eq_idx)
            .map(|eq| {
                let val = eval_expr::<Dual>(&eq.rhs, env_dual);
                apply_dae_sign(val, eq_idx, n_x).du
            })
            .unwrap_or(0.0);
        jac[(row, col)] = clamp_finite(du);
    }
}

/// Evaluate a single residual equation with proper sign convention.
fn eval_single_residual(dae: &Dae, env: &VarEnv<f64>, eq_idx: usize, n_x: usize) -> f64 {
    let Some(eq) = dae.f_x.get(eq_idx) else {
        return 1.0e12;
    };
    let val = eval_expr::<f64>(&eq.rhs, env);
    apply_dae_sign(val, eq_idx, n_x)
}

/// Evaluate the derivative of a single equation w.r.t. a single variable using AD.
fn eval_single_derivative(
    dae: &Dae,
    env: &VarEnv<f64>,
    eq_idx: usize,
    var_name: &str,
    n_x: usize,
) -> f64 {
    let Some(eq) = dae.f_x.get(eq_idx) else {
        return 0.0;
    };
    let mut env_dual: VarEnv<Dual> = lift_env(env);
    // Seed the variable of interest
    if let Some(entry) = env_dual.vars.get_mut(var_name) {
        entry.du = 1.0;
    }
    let val = eval_expr::<Dual>(&eq.rhs, &env_dual);
    let val = apply_dae_sign(val, eq_idx, n_x);
    val.du
}

/// Build the environment from current y-vector and parameters.
fn build_solve_env(dae: &Dae, y: &[f64], p: &[f64]) -> VarEnv<f64> {
    let mut env = build_env(dae, y, p, 0.0);
    env.is_initial = true;
    env
}

/// Update the environment after changing a variable's value in y.
fn update_env_var(env: &mut VarEnv<f64>, var_name: &str, value: f64) {
    env.set(var_name, value);
}

/// Compute nominal scale for a variable. Returns |nominal| if available, else 1.0.
fn get_nominal_scale(dae: &Dae, var_name: &str, env: &VarEnv<f64>) -> f64 {
    let var = dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
        .find(|(name, _)| name.as_str() == var_name)
        .map(|(_, v)| v);
    let Some(var) = var else { return 1.0 };
    let Some(ref nominal) = var.nominal else {
        return 1.0;
    };
    let val = eval_expr::<f64>(nominal, env).abs();
    if val > 0.0 && val.is_finite() {
        val
    } else {
        1.0
    }
}

/// Parameters for a single-variable Newton solve.
struct ScalarNewtonParams<'a> {
    dae: &'a Dae,
    eq_idx: usize,
    var_idx: usize,
    var_name: &'a str,
    n_x: usize,
    tol: f64,
}

/// Solve a ScalarNewton block: single-variable Newton iteration with AD.
fn solve_scalar_newton(
    p: &ScalarNewtonParams<'_>,
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    deadline: Option<Instant>,
) -> Result<bool, IcSolveError> {
    if !is_valid_var_idx(y, p.var_idx) || !is_valid_eq_idx(p.dae, p.eq_idx) {
        return Ok(false);
    }
    let scale = get_nominal_scale(p.dae, p.var_name, env);
    for _ in 0..30 {
        check_timeout(deadline)?;
        let r = eval_single_residual(p.dae, env, p.eq_idx, p.n_x);
        if r.abs() < p.tol {
            return Ok(true);
        }
        let dr = eval_single_derivative(p.dae, env, p.eq_idx, p.var_name, p.n_x);
        if dr.abs() < 1e-30 {
            y[p.var_idx] += scale * 1e-6;
            update_env_var(env, p.var_name, y[p.var_idx]);
            continue;
        }
        let delta = r / dr;
        y[p.var_idx] -= delta;
        update_env_var(env, p.var_name, y[p.var_idx]);
    }
    Ok(false)
}

/// LM problem for a torn algebraic loop or coupled block.
struct BlockLmProblem<'a> {
    dae: &'a Dae,
    env: VarEnv<f64>,
    y: &'a mut Vec<f64>,
    /// (y-vector index, env name, nominal scale) for each iteration variable
    iter_vars: Vec<(usize, String, f64)>,
    /// Causal steps to evaluate before computing residuals (empty for CoupledLM)
    causal_steps: &'a [CausalStep],
    /// dae::Equation indices for residuals
    residual_eq_indices: &'a [usize],
    n_x: usize,
    deadline: Option<Instant>,
}

impl BlockLmProblem<'_> {
    fn timed_out(&self) -> bool {
        is_timed_out(self.deadline)
    }

    /// Evaluate causal sequence: for each step, solve for the variable
    /// symbolically or via scalar Newton.
    fn eval_causal_sequence(&mut self) {
        if self.timed_out() {
            return;
        }
        for step in self.causal_steps {
            if !is_valid_var_idx(self.y, step.var_idx) {
                continue;
            }
            let val = match step.solution_expr {
                Some(ref expr) => eval_expr::<f64>(expr, &self.env),
                None if !is_valid_eq_idx(self.dae, step.eq_idx) => self.y[step.var_idx],
                None => self.causal_newton(step),
            };
            let val = clamp_finite(val);
            self.y[step.var_idx] = val;
            update_env_var(&mut self.env, &step.var_name, val);
        }
    }

    /// Scalar Newton for a single causal step variable.
    fn causal_newton(&mut self, step: &CausalStep) -> f64 {
        if !is_valid_var_idx(self.y, step.var_idx) || !is_valid_eq_idx(self.dae, step.eq_idx) {
            return 0.0;
        }
        let mut val = self.y[step.var_idx];
        for _ in 0..20 {
            if self.timed_out() {
                break;
            }
            update_env_var(&mut self.env, &step.var_name, val);
            let r = eval_single_residual(self.dae, &self.env, step.eq_idx, self.n_x);
            if r.abs() < 1e-10 {
                break;
            }
            let dr =
                eval_single_derivative(self.dae, &self.env, step.eq_idx, &step.var_name, self.n_x);
            if dr.abs() < 1e-30 {
                break;
            }
            val -= r / dr;
        }
        val
    }
}

impl LeastSquaresProblem<f64, Dyn, Dyn> for BlockLmProblem<'_> {
    type ResidualStorage = nalgebra::VecStorage<f64, Dyn, U1>;
    type JacobianStorage = nalgebra::VecStorage<f64, Dyn, Dyn>;
    type ParameterStorage = nalgebra::VecStorage<f64, Dyn, U1>;

    fn set_params(&mut self, p: &Vector<f64, Dyn, Self::ParameterStorage>) {
        if self.timed_out() {
            return;
        }
        for (i, (var_idx, var_name, scale)) in self.iter_vars.iter().enumerate() {
            let val = p[i] * scale; // unscale
            self.y[*var_idx] = val;
            update_env_var(&mut self.env, var_name, val);
        }
        self.eval_causal_sequence();
    }

    fn params(&self) -> OVector<f64, Dyn> {
        let n = self.iter_vars.len();
        let mut p = OVector::zeros_generic(Dyn(n), U1);
        for (i, (var_idx, _, scale)) in self.iter_vars.iter().enumerate() {
            p[i] = self.y[*var_idx] / scale; // scale
        }
        p
    }

    fn residuals(&self) -> Option<OVector<f64, Dyn>> {
        let m = self.residual_eq_indices.len();
        if self.timed_out() {
            let mut timed_out_residual = OVector::zeros_generic(Dyn(m), U1);
            timed_out_residual.fill(1.0e12);
            return Some(timed_out_residual);
        }
        let mut r = OVector::zeros_generic(Dyn(m), U1);
        let env = self.env.clone();
        for (i, &eq_idx) in self.residual_eq_indices.iter().enumerate() {
            if let Some(eq) = self.dae.f_x.get(eq_idx) {
                let val = eval_expr::<f64>(&eq.rhs, &env);
                r[i] = apply_dae_sign(val, eq_idx, self.n_x);
            } else {
                r[i] = 1.0e12;
            }
        }
        Some(r)
    }

    fn jacobian(&self) -> Option<OMatrix<f64, Dyn, Dyn>> {
        let m = self.residual_eq_indices.len();
        let n = self.iter_vars.len();
        if self.timed_out() {
            return Some(OMatrix::zeros_generic(Dyn(m), Dyn(n)));
        }
        let mut jac = OMatrix::zeros_generic(Dyn(m), Dyn(n));

        for j in 0..n {
            let (_, ref var_name, scale) = self.iter_vars[j];
            let mut env_dual: VarEnv<Dual> = lift_env(&self.env);
            // Seed the j-th iteration variable
            if let Some(entry) = env_dual.vars.get_mut(var_name.as_str()) {
                entry.du = scale; // chain rule: d/d(scaled) = scale * d/d(unscaled)
            }

            // Propagate Dual through the causal sequence so that the chain rule
            // correctly connects tear variables to residual equations.
            propagate_dual_causal(self.dae, &mut env_dual, self.causal_steps, self.n_x);
            fill_jacobian_column(
                self.dae,
                self.residual_eq_indices,
                &env_dual,
                self.n_x,
                &mut jac,
                j,
            );
        }

        Some(jac)
    }
}

/// Propagate Dual numbers through the causal sequence.
///
/// For each causal step:
/// - **Symbolic steps**: evaluate the solution expression with Dual to get
///   both value (re) and derivative (du).
/// - **Newton steps**: use implicit differentiation. If `0 = g(x, p)` where
///   x is the causal variable and p are upstream variables, then
///   `dx/dp = -(dg/dp) / (dg/dx)`. We evaluate g with Dual (with x.du=0)
///   to get dg/dp, and separately compute the PARTIAL dg/dx using a fresh
///   env with only x.du=1.
fn propagate_dual_causal(
    dae: &Dae,
    env_dual: &mut VarEnv<Dual>,
    causal_steps: &[CausalStep],
    n_x: usize,
) {
    for step in causal_steps {
        match step.solution_expr {
            Some(ref expr) => {
                // Symbolic: just evaluate with Dual
                let val = eval_expr::<Dual>(expr, env_dual);
                if let Some(entry) = env_dual.vars.get_mut(step.var_name.as_str()) {
                    *entry = val;
                }
            }
            None => {
                // Newton step: use implicit function theorem.
                // g(x, p) = 0  =>  dx.du = -g.du / (dg/dx)

                // Step 1: evaluate g with x.du=0 to get dg/dp (upstream contribution)
                if let Some(entry) = env_dual.vars.get_mut(step.var_name.as_str()) {
                    entry.du = 0.0; // ensure x.du=0
                }
                let g_val = eval_expr::<Dual>(&dae.f_x[step.eq_idx].rhs, env_dual);
                let g_val = apply_dae_sign(g_val, step.eq_idx, n_x);

                // Step 2: compute PARTIAL dg/dx using a fresh env with all du=0
                // except x.du=1. This avoids contamination from upstream Duals.
                let mut env_partial = env_dual.clone();
                for (_, v) in env_partial.vars.iter_mut() {
                    v.du = 0.0;
                }
                if let Some(entry) = env_partial.vars.get_mut(step.var_name.as_str()) {
                    entry.du = 1.0;
                }
                let g_dx = eval_expr::<Dual>(&dae.f_x[step.eq_idx].rhs, &env_partial);
                let g_dx = apply_dae_sign(g_dx, step.eq_idx, n_x);
                let dg_dx = g_dx.du;

                // Implicit differentiation: dx/dp = -g.du / dg_dx
                let x_du = if dg_dx.abs() > 1e-30 {
                    -g_val.du / dg_dx
                } else {
                    0.0 // degenerate — no sensitivity
                };

                if let Some(entry) = env_dual.vars.get_mut(step.var_name.as_str()) {
                    entry.du = x_du;
                }
            }
        }
    }
}

/// Clamp non-finite values to zero.
fn clamp_finite(v: f64) -> f64 {
    if v.is_finite() { v } else { 0.0 }
}

fn init_eval_env(dae: &Dae) -> VarEnv<f64> {
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

    env
}

fn evaluate_constants_in_place(dae: &Dae, env: &mut VarEnv<f64>) {
    // Two passes allow forward references between constants.
    for _ in 0..2 {
        for (name, var) in &dae.constants {
            if let Some(ref start) = var.start {
                env.set(name.as_str(), eval_expr::<f64>(start, env));
            }
        }
    }
}

/// Build default parameter vector from DAE.
fn build_params(dae: &Dae) -> Vec<f64> {
    // Use the same logic as sim-diffsol's default_params
    let mut env = init_eval_env(dae);
    evaluate_constants_in_place(dae, &mut env);

    let mut params = Vec::new();
    for (name, var) in &dae.parameters {
        let sz = var.size();
        if sz <= 1 {
            let val = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            env.set(name.as_str(), val);
            params.push(val);
        } else if let Some(Expression::Array { elements, .. }) = var.start.as_ref() {
            for (i, e) in elements.iter().enumerate() {
                let val = eval_expr::<f64>(e, &env);
                env.set(&format!("{}[{}]", name.as_str(), i + 1), val);
                params.push(val);
            }
        } else {
            let val = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            for i in 0..sz {
                env.set(&format!("{}[{}]", name.as_str(), i + 1), val);
                params.push(val);
            }
        }
    }

    // Pass 2: resolve forward references
    let mut pidx = 0;
    for (name, var) in &dae.parameters {
        let sz = var.size();
        if sz <= 1 {
            let val = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            env.set(name.as_str(), val);
            params[pidx] = val;
            pidx += 1;
        } else if let Some(Expression::Array { elements, .. }) = var.start.as_ref() {
            for (i, e) in elements.iter().enumerate() {
                let val = eval_expr::<f64>(e, &env);
                env.set(&format!("{}[{}]", name.as_str(), i + 1), val);
                params[pidx] = val;
                pidx += 1;
            }
        } else {
            let val = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            for i in 0..sz {
                env.set(&format!("{}[{}]", name.as_str(), i + 1), val);
                params[pidx] = val;
                pidx += 1;
            }
        }
    }

    params
}

/// Evaluate per-element start values for a variable, correctly handling arrays.
fn eval_var_start_values(var: &rumoca_ir_dae::Variable, env: &VarEnv<f64>) -> Vec<f64> {
    let sz = var.size();
    if sz <= 1 {
        return vec![get_init_value(var, env)];
    }
    let Some(start) = var.start.as_ref() else {
        return vec![0.0; sz];
    };
    let raw = eval_array_values::<f64>(start, env);
    if raw.is_empty() {
        return vec![eval_expr::<f64>(start, env); sz];
    }
    if raw.len() == 1 {
        return vec![raw[0]; sz];
    }
    let last = *raw.last().unwrap_or(&0.0);
    (0..sz)
        .map(|i| raw.get(i).copied().unwrap_or(last))
        .collect()
}

/// Initialize `[x; z; y_out]` vector from variable start/nominal values.
fn initialize_state_vector(dae: &Dae, y: &mut [f64]) {
    let env = build_param_env(dae);
    let mut idx = 0;
    for var in dae.states.values() {
        let vals = eval_var_start_values(var, &env);
        for v in &vals {
            if idx < y.len() {
                y[idx] = *v;
            }
            idx += 1;
        }
    }
    for var in dae.algebraics.values() {
        let vals = eval_var_start_values(var, &env);
        for v in &vals {
            if idx < y.len() {
                y[idx] = *v;
            }
            idx += 1;
        }
    }
    for var in dae.outputs.values() {
        let vals = eval_var_start_values(var, &env);
        for v in &vals {
            if idx < y.len() {
                y[idx] = *v;
            }
            idx += 1;
        }
    }
}

/// Build a parameter environment for evaluating start expressions.
fn build_param_env(dae: &Dae) -> VarEnv<f64> {
    let mut env = init_eval_env(dae);
    evaluate_constants_in_place(dae, &mut env);

    for (name, var) in &dae.parameters {
        let sz = var.size();
        if sz <= 1 {
            let val = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            env.set(name.as_str(), val);
        } else if let Some(Expression::Array { elements, .. }) = var.start.as_ref() {
            for (i, e) in elements.iter().enumerate() {
                let val = eval_expr::<f64>(e, &env);
                env.set(&format!("{}[{}]", name.as_str(), i + 1), val);
            }
        } else {
            let val = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            for i in 0..sz {
                env.set(&format!("{}[{}]", name.as_str(), i + 1), val);
            }
        }
    }
    // Pass 2
    for (name, var) in &dae.parameters {
        let sz = var.size();
        if sz <= 1 {
            let val = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            env.set(name.as_str(), val);
        } else if let Some(Expression::Array { elements, .. }) = var.start.as_ref() {
            for (i, e) in elements.iter().enumerate() {
                let val = eval_expr::<f64>(e, &env);
                env.set(&format!("{}[{}]", name.as_str(), i + 1), val);
            }
        } else {
            let val = var
                .start
                .as_ref()
                .map(|expr| eval_expr::<f64>(expr, &env))
                .unwrap_or(0.0);
            for i in 0..sz {
                env.set(&format!("{}[{}]", name.as_str(), i + 1), val);
            }
        }
    }
    env
}

/// Get the initial value for a DAE variable.
fn get_init_value(var: &rumoca_ir_dae::Variable, env: &VarEnv<f64>) -> f64 {
    if let Some(ref start) = var.start {
        return eval_expr::<f64>(start, env);
    }
    if let Some(ref nominal) = var.nominal {
        return eval_expr::<f64>(nominal, env);
    }
    0.0
}

/// Write solved initial conditions back into DAE variable `start` attributes.
fn write_solved_ics(dae: &mut Dae, y: &[f64], n_x: usize) {
    let mut idx = 0;
    // Skip states (keep their existing start values)
    for (_name, var) in dae.states.iter() {
        idx += var.size();
    }
    debug_assert_eq!(idx, n_x);

    // Write back algebraic and output values
    for (_name, var) in dae.algebraics.iter_mut() {
        write_var_start(var, y, &mut idx);
    }
    for (_name, var) in dae.outputs.iter_mut() {
        write_var_start(var, y, &mut idx);
    }
}

/// Write solved values from `y[idx..]` into a variable's `start` attribute.
fn write_var_start(var: &mut rumoca_ir_dae::Variable, y: &[f64], idx: &mut usize) {
    let sz = var.size();
    if sz <= 1 {
        if *idx < y.len() {
            var.start = Some(Expression::Literal(rumoca_ir_dae::Literal::Real(y[*idx])));
        }
        *idx += 1;
    } else {
        let elements: Vec<Expression> = (0..sz)
            .map(|i| {
                let val = y.get(*idx + i).copied().unwrap_or(0.0);
                Expression::Literal(rumoca_ir_dae::Literal::Real(val))
            })
            .collect();
        var.start = Some(Expression::Array {
            elements,
            is_matrix: false,
        });
        *idx += sz;
    }
}

/// Solve for consistent initial conditions using BLT-sequential block solving.
///
/// Walks the pre-computed `IcBlock` plan in order, solving each block
/// independently. Uses symbolic evaluation for direct blocks, scalar Newton
/// for single-variable blocks, and Levenberg-Marquardt for coupled/torn blocks.
///
/// Returns `true` if all blocks converged, `false` otherwise (graceful degradation).
///
/// Returns `Err(IcSolveError::Timeout)` if the provided deadline is exceeded.
pub fn solve_initial_blt_with_deadline(
    dae: &mut Dae,
    n_x: usize,
    ic_blocks: &[IcBlock],
    tol: f64,
    deadline: Option<Instant>,
) -> Result<bool, IcSolveError> {
    let n_eq = dae.f_x.len();
    if ic_blocks.is_empty() || n_eq <= n_x {
        return Ok(true);
    }

    let mut y = vec![0.0; n_eq];
    initialize_state_vector(dae, &mut y);
    let p = build_params(dae);
    let mut env = build_solve_env(dae, &y, &p);

    let mut all_converged = true;

    for block in ic_blocks {
        check_timeout(deadline)?;
        if !solve_ic_block(dae, block, &mut y, &mut env, n_x, tol, deadline)? {
            all_converged = false;
        }
    }

    write_solved_ics(dae, &y, n_x);
    Ok(all_converged)
}

fn solve_ic_block(
    dae: &Dae,
    block: &IcBlock,
    y: &mut Vec<f64>,
    env: &mut VarEnv<f64>,
    n_x: usize,
    tol: f64,
    deadline: Option<Instant>,
) -> Result<bool, IcSolveError> {
    match block {
        IcBlock::ScalarDirect {
            var_idx,
            var_name,
            solution_expr,
        } => {
            if !is_valid_var_idx(y, *var_idx) {
                return Ok(false);
            }
            let val = clamp_finite(eval_expr::<f64>(solution_expr, env));
            y[*var_idx] = val;
            update_env_var(env, var_name, val);
            Ok(true)
        }
        IcBlock::ScalarNewton {
            eq_idx,
            var_idx,
            var_name,
        } => {
            if !is_valid_var_idx(y, *var_idx) || !is_valid_eq_idx(dae, *eq_idx) {
                return Ok(false);
            }
            let params = ScalarNewtonParams {
                dae,
                eq_idx: *eq_idx,
                var_idx: *var_idx,
                var_name,
                n_x,
                tol,
            };
            solve_scalar_newton(&params, y, env, deadline)
        }
        IcBlock::TornBlock {
            tear_var_indices,
            tear_var_names,
            causal_sequence,
            residual_eq_indices,
        } => {
            if tear_var_indices.len() != tear_var_names.len()
                || tear_var_indices
                    .iter()
                    .any(|&idx| !is_valid_var_idx(y, idx))
                || residual_eq_indices
                    .iter()
                    .any(|&eq_idx| !is_valid_eq_idx(dae, eq_idx))
                || !has_valid_causal_indices(dae, y, causal_sequence)
            {
                return Ok(false);
            }
            let spec = LmBlockSpec {
                iter_var_indices: tear_var_indices,
                iter_var_names: tear_var_names,
                causal_steps: causal_sequence,
                residual_eq_indices,
            };
            let ok = solve_lm_block(dae, y, env, &spec, n_x, tol, deadline)?;
            update_env_from_indices(y, env, tear_var_indices, tear_var_names);
            update_env_from_causal_steps(y, env, causal_sequence);
            Ok(ok)
        }
        IcBlock::CoupledLM {
            eq_indices,
            var_indices,
            var_names,
        } => {
            if var_indices.len() != var_names.len()
                || var_indices.iter().any(|&idx| !is_valid_var_idx(y, idx))
                || eq_indices
                    .iter()
                    .any(|&eq_idx| !is_valid_eq_idx(dae, eq_idx))
            {
                return Ok(false);
            }
            let spec = LmBlockSpec {
                iter_var_indices: var_indices,
                iter_var_names: var_names,
                causal_steps: &[],
                residual_eq_indices: eq_indices,
            };
            let ok = solve_lm_block(dae, y, env, &spec, n_x, tol, deadline)?;
            update_env_from_indices(y, env, var_indices, var_names);
            Ok(ok)
        }
    }
}

/// Solve BLT initialization without a wall-clock deadline.
pub fn solve_initial_blt(dae: &mut Dae, n_x: usize, ic_blocks: &[IcBlock], tol: f64) -> bool {
    solve_initial_blt_with_deadline(dae, n_x, ic_blocks, tol, None).unwrap_or(false)
}

struct LmBlockSpec<'a> {
    iter_var_indices: &'a [usize],
    iter_var_names: &'a [String],
    causal_steps: &'a [CausalStep],
    residual_eq_indices: &'a [usize],
}

/// Solve a block using Levenberg-Marquardt.
fn solve_lm_block(
    dae: &Dae,
    y: &mut Vec<f64>,
    env: &VarEnv<f64>,
    spec: &LmBlockSpec<'_>,
    n_x: usize,
    tol: f64,
    deadline: Option<Instant>,
) -> Result<bool, IcSolveError> {
    check_timeout(deadline)?;

    if spec.iter_var_indices.len() != spec.iter_var_names.len()
        || spec
            .iter_var_indices
            .iter()
            .any(|&idx| !is_valid_var_idx(y, idx))
        || spec
            .residual_eq_indices
            .iter()
            .any(|&eq_idx| !is_valid_eq_idx(dae, eq_idx))
        || !has_valid_causal_indices(dae, y, spec.causal_steps)
    {
        return Ok(false);
    }

    // Compute nominal scales for iteration variables
    let iter_vars: Vec<(usize, String, f64)> = spec
        .iter_var_indices
        .iter()
        .zip(spec.iter_var_names.iter())
        .map(|(&idx, name)| {
            let scale = get_nominal_scale(dae, name, env);
            (idx, name.clone(), scale)
        })
        .collect();

    let mut problem = BlockLmProblem {
        dae,
        env: env.clone(),
        y,
        iter_vars,
        causal_steps: spec.causal_steps,
        residual_eq_indices: spec.residual_eq_indices,
        n_x,
        deadline,
    };

    // Evaluate causal sequence with initial values
    problem.eval_causal_sequence();

    check_timeout(deadline)?;

    let lm_patience = if let Some(d) = deadline {
        let remaining = d.saturating_duration_since(Instant::now());
        if remaining.as_secs_f64() < 0.1 {
            5
        } else if remaining.as_secs_f64() < 0.5 {
            15
        } else if remaining.as_secs_f64() < 2.0 {
            60
        } else {
            200
        }
    } else {
        200
    };

    let (solved, report) = LevenbergMarquardt::new()
        .with_tol(tol)
        .with_patience(lm_patience)
        .minimize(problem);

    check_timeout(deadline)?;

    // Write results back
    *y = solved.y.clone();

    // Verify actual convergence by checking the residual norm
    let mut converged = report.termination.was_successful();
    if converged {
        // Double-check: evaluate actual residuals at the solution
        let check_env = build_solve_env(dae, y, &build_params(dae));
        let mut max_res = 0.0_f64;
        for &eq_idx in spec.residual_eq_indices {
            let val = eval_expr::<f64>(&dae.f_x[eq_idx].rhs, &check_env);
            let val = apply_dae_sign(val, eq_idx, n_x);
            max_res = max_res.max(val.abs());
        }
        // Keep BLT acceptance reasonably strict: if residuals remain too large,
        // hand off to the full-system Newton fallback in the simulator.
        if max_res > tol * 100.0 {
            converged = false;
        }
    }
    Ok(converged)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;
    use rumoca_ir_dae as dae;

    fn var_ref(name: &str) -> Expression {
        Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn lit(v: f64) -> Expression {
        Expression::Literal(dae::Literal::Real(v))
    }

    fn sub(l: Expression, r: Expression) -> Expression {
        Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(l),
            rhs: Box::new(r),
        }
    }

    fn mul(l: Expression, r: Expression) -> Expression {
        Expression::Binary {
            op: rumoca_ir_core::OpBinary::Mul(Default::default()),
            lhs: Box::new(l),
            rhs: Box::new(r),
        }
    }

    fn add(l: Expression, r: Expression) -> Expression {
        Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(l),
            rhs: Box::new(r),
        }
    }

    fn eq_from(rhs: Expression) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs,
            span: Span::DUMMY,
            origin: String::new(),
            scalar_count: 1,
        }
    }

    #[test]
    fn test_solve_scalar_direct_chain() {
        // R_actual = R * (1 + alpha)  →  solved first as ScalarDirect
        // v = R_actual * i            →  solved second as ScalarDirect
        // R = 100.0, alpha = 0.0, i = 0.5 (parameters)
        let mut dae = Dae::new();

        let mut r_param = dae::Variable::new(dae::VarName::new("R"));
        r_param.start = Some(lit(100.0));
        dae.parameters.insert(dae::VarName::new("R"), r_param);

        let mut alpha = dae::Variable::new(dae::VarName::new("alpha"));
        alpha.start = Some(lit(0.0));
        dae.parameters.insert(dae::VarName::new("alpha"), alpha);

        let mut i_param = dae::Variable::new(dae::VarName::new("i"));
        i_param.start = Some(lit(0.5));
        dae.parameters.insert(dae::VarName::new("i"), i_param);

        dae.algebraics.insert(
            dae::VarName::new("R_actual"),
            dae::Variable::new(dae::VarName::new("R_actual")),
        );
        dae.algebraics.insert(
            dae::VarName::new("v"),
            dae::Variable::new(dae::VarName::new("v")),
        );

        // 0 = R_actual - R * (1 + alpha)
        dae.f_x.push(eq_from(sub(
            var_ref("R_actual"),
            mul(var_ref("R"), add(lit(1.0), var_ref("alpha"))),
        )));
        // 0 = v - R_actual * i
        dae.f_x.push(eq_from(sub(
            var_ref("v"),
            mul(var_ref("R_actual"), var_ref("i")),
        )));

        let plan = rumoca_phase_solve::build_ic_plan(&dae, 0).unwrap();
        assert_eq!(plan.len(), 2);

        let ok = solve_initial_blt(&mut dae, 0, &plan, 1e-10);
        assert!(ok, "should converge");

        // Check solved values via start attributes
        let r_actual_start = dae.algebraics.get(&dae::VarName::new("R_actual")).unwrap();
        if let Some(Expression::Literal(dae::Literal::Real(v))) = &r_actual_start.start {
            assert!(
                (*v - 100.0).abs() < 1e-6,
                "R_actual should be 100.0, got {v}"
            );
        } else {
            panic!("R_actual start should be a literal");
        }

        let v_start = dae.algebraics.get(&dae::VarName::new("v")).unwrap();
        if let Some(Expression::Literal(dae::Literal::Real(v))) = &v_start.start {
            assert!((*v - 50.0).abs() < 1e-6, "v should be 50.0, got {v}");
        } else {
            panic!("v start should be a literal");
        }
    }

    #[test]
    fn test_solve_scalar_newton() {
        // Single algebraic equation that can't be solved symbolically:
        // 0 = x^2 - 4 (solution: x = 2 or x = -2)
        let mut dae = Dae::new();

        let mut x_var = dae::Variable::new(dae::VarName::new("x"));
        x_var.start = Some(lit(1.0)); // initial guess
        dae.algebraics.insert(dae::VarName::new("x"), x_var);

        // 0 = x*x - 4
        dae.f_x
            .push(eq_from(sub(mul(var_ref("x"), var_ref("x")), lit(4.0))));

        let plan = rumoca_phase_solve::build_ic_plan(&dae, 0).unwrap();
        assert_eq!(plan.len(), 1);
        assert!(matches!(&plan[0], IcBlock::ScalarNewton { .. }));

        let ok = solve_initial_blt(&mut dae, 0, &plan, 1e-10);
        assert!(ok, "should converge");

        let x_start = dae.algebraics.get(&dae::VarName::new("x")).unwrap();
        if let Some(Expression::Literal(dae::Literal::Real(v))) = &x_start.start {
            assert!((*v - 2.0).abs() < 1e-6, "x should be 2.0, got {v}");
        }
    }

    #[test]
    fn test_invalid_scalar_newton_var_idx_returns_false_without_panic() {
        let mut dae = Dae::new();
        dae.algebraics.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        dae.f_x.push(eq_from(sub(var_ref("x"), lit(1.0))));

        let plan = vec![IcBlock::ScalarNewton {
            eq_idx: 0,
            var_idx: 5,
            var_name: "x".to_string(),
        }];

        let ok = solve_initial_blt(&mut dae, 0, &plan, 1e-10);
        assert!(!ok, "invalid var index should fail gracefully");
    }

    #[test]
    fn test_invalid_torn_block_causal_var_idx_returns_false_without_panic() {
        let mut dae = Dae::new();
        dae.algebraics.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        dae.f_x.push(eq_from(sub(var_ref("x"), lit(1.0))));

        let plan = vec![IcBlock::TornBlock {
            tear_var_indices: vec![0],
            tear_var_names: vec!["x".to_string()],
            causal_sequence: vec![CausalStep {
                var_idx: 3,
                var_name: "x".to_string(),
                solution_expr: None,
                eq_idx: 0,
            }],
            residual_eq_indices: vec![0],
        }];

        let ok = solve_initial_blt(&mut dae, 0, &plan, 1e-10);
        assert!(!ok, "invalid causal var index should fail gracefully");
    }

    #[test]
    fn test_build_params_resolves_user_function_in_parameter_start() {
        let mut dae = Dae::new();

        let mut func = dae::Function::new("Pkg.f", Span::DUMMY);
        func.add_input(dae::FunctionParam::new("u", "Real"));
        func.add_output(
            dae::FunctionParam::new("y", "Real").with_default(add(var_ref("u"), lit(1.0))),
        );
        func.body = vec![dae::Statement::Empty];
        dae.functions.insert(dae::VarName::new("Pkg.f"), func);

        let mut p = dae::Variable::new(dae::VarName::new("p"));
        p.start = Some(Expression::FunctionCall {
            name: dae::VarName::new("Pkg.f"),
            args: vec![lit(2.0)],
            is_constructor: false,
        });
        dae.parameters.insert(dae::VarName::new("p"), p);

        let params = build_params(&dae);
        assert_eq!(params.len(), 1);
        assert!(params[0].is_finite());
        assert!(
            (params[0] - 3.0).abs() < 1e-12,
            "parameter start should evaluate via user function, got {}",
            params[0]
        );
    }

    #[test]
    fn test_initialize_state_vector_resolves_user_function_in_variable_start() {
        let mut dae = Dae::new();

        let mut func = dae::Function::new("Pkg.f", Span::DUMMY);
        func.add_input(dae::FunctionParam::new("u", "Real"));
        func.add_output(
            dae::FunctionParam::new("y", "Real").with_default(add(var_ref("u"), lit(1.0))),
        );
        func.body = vec![dae::Statement::Empty];
        dae.functions.insert(dae::VarName::new("Pkg.f"), func);

        let mut x = dae::Variable::new(dae::VarName::new("x"));
        x.start = Some(Expression::FunctionCall {
            name: dae::VarName::new("Pkg.f"),
            args: vec![lit(4.0)],
            is_constructor: false,
        });
        dae.algebraics.insert(dae::VarName::new("x"), x);

        dae.f_x.push(eq_from(sub(var_ref("x"), lit(5.0))));

        let mut y = vec![0.0; dae.f_x.len()];
        initialize_state_vector(&dae, &mut y);
        assert!(y[0].is_finite());
        assert!(
            (y[0] - 5.0).abs() < 1e-12,
            "state vector init should evaluate function-based start, got {}",
            y[0]
        );
    }
}
