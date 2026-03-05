use crate::{
    ParameterValueError, SimError, TimeoutBudget, default_parameter_values,
    default_parameter_values_with_budget,
};
use rumoca_ir_dae as dae;

type BuiltinFunction = dae::BuiltinFunction;
type Dae = dae::Dae;
type Equation = dae::Equation;
type Expression = dae::Expression;
type VarName = dae::VarName;

use rumoca_eval_flat::dual::Dual;
use rumoca_eval_flat::eval::{VarEnv, build_env, eval_expr, lift_env, map_var_to_env};
use rumoca_eval_flat::sim_float::SimFloat;

pub fn component_base_name(name: &str) -> Option<String> {
    dae::component_base_name(name)
}

pub(crate) fn sim_trace_enabled() -> bool {
    std::env::var("RUMOCA_SIM_TRACE").is_ok() || std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
}

pub(crate) fn sim_introspect_enabled() -> bool {
    std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
}

fn apply_dae_sign<S: SimFloat>(val: S, i: usize, n_x: usize) -> S {
    if i < n_x { -val } else { val }
}

pub fn count_states(dae: &dae::Dae) -> usize {
    dae.states.values().map(|v| v.size()).sum()
}

pub fn default_params(dae: &dae::Dae) -> Vec<f64> {
    default_parameter_values(dae)
}

pub fn default_params_with_budget(
    dae: &dae::Dae,
    budget: &TimeoutBudget,
) -> Result<Vec<f64>, SimError> {
    default_parameter_values_with_budget(dae, budget).map_err(|err| match err {
        ParameterValueError::Timeout(timeout) => timeout.into(),
        ParameterValueError::Invalid(message) => SimError::SolverError(message),
    })
}

use std::sync::atomic::{AtomicUsize, Ordering};

static RHS_SIGNAL_DEBUG_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn eval_rhs_generic<S: SimFloat>(dae: &dae::Dae, env: &VarEnv<S>, n_x: usize, out: &mut [S]) {
    let env = env.clone();
    if std::env::var("RUMOCA_SIM_INTROSPECT").is_ok()
        && env.vars.contains_key("vIn.signalSource.T_start")
        && env.vars.contains_key("vIn.signalSource.count")
    {
        let hit = RHS_SIGNAL_DEBUG_COUNTER.fetch_add(1, Ordering::Relaxed);
        if hit < 24 {
            eprintln!(
                "[sim-introspect] rhs-env hit={} t={} T_start={} count={} T_rising={} T_width={} T_falling={} startTime={} offset={} amplitude={} vIn.p.v={} vIn.n.v={}",
                hit,
                env.get("time").real(),
                env.get("vIn.signalSource.T_start").real(),
                env.get("vIn.signalSource.count").real(),
                env.get("vIn.signalSource.T_rising").real(),
                env.get("vIn.signalSource.T_width").real(),
                env.get("vIn.signalSource.T_falling").real(),
                env.get("vIn.signalSource.startTime").real(),
                env.get("vIn.signalSource.offset").real(),
                env.get("vIn.signalSource.amplitude").real(),
                env.get("vIn.p.v").real(),
                env.get("vIn.n.v").real(),
            );
        }
    }
    for (i, eq) in dae.f_x.iter().enumerate() {
        if i < out.len() {
            let val = eval_expr::<S>(&eq.rhs, &env);
            out[i] = apply_dae_sign(val, i, n_x);
        }
    }
}

pub fn eval_rhs_equations(
    dae: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
    n_x: usize,
) {
    let env = build_env(dae, y, p, t);
    eval_rhs_generic(dae, &env, n_x, out);
}

pub fn eval_rhs_equations_initial(
    dae: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
    n_x: usize,
) {
    let mut env = build_env(dae, y, p, t);
    env.is_initial = true;
    eval_rhs_generic(dae, &env, n_x, out);
}

fn seed_state_duals(env: &mut VarEnv<Dual>, dae: &dae::Dae, v: &[f64]) {
    let mut seed_env = VarEnv::<f64>::new();
    let mut idx = 0usize;
    for (name, var) in dae
        .states
        .iter()
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
    {
        map_var_to_env(&mut seed_env, name.as_str(), var, v, &mut idx);
    }

    for (key, du) in seed_env.vars {
        if let Some(entry) = env.vars.get_mut(&key) {
            entry.du = du;
        }
    }
}

pub fn eval_jacobian_vector_ad(
    dae: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    v: &[f64],
    out: &mut [f64],
    n_x: usize,
) {
    let env_f64 = build_env(dae, y, p, t);
    let mut env_dual: VarEnv<Dual> = lift_env(&env_f64);
    seed_state_duals(&mut env_dual, dae, v);
    let mut dual_out = vec![Dual::default(); out.len()];
    eval_rhs_generic(dae, &env_dual, n_x, &mut dual_out);
    for (i, d) in dual_out.iter().enumerate() {
        out[i] = d.du;
    }
}

pub fn eval_jacobian_vector_ad_initial(
    dae: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    v: &[f64],
    out: &mut [f64],
    n_x: usize,
) {
    let mut env_f64 = build_env(dae, y, p, t);
    env_f64.is_initial = true;
    let mut env_dual: VarEnv<Dual> = lift_env(&env_f64);
    seed_state_duals(&mut env_dual, dae, v);
    let mut dual_out = vec![Dual::default(); out.len()];
    eval_rhs_generic(dae, &env_dual, n_x, &mut dual_out);
    for (i, d) in dual_out.iter().enumerate() {
        out[i] = d.du;
    }
}

mod core;
pub(crate) use core::*;

mod init;
pub use init::{
    initial_free_residual_inf, project_algebraics_with_fixed_states_at_time,
    runtime_projection_required, seed_runtime_direct_assignments, solve_initial_algebraic,
};
