use std::collections::HashSet;

use rumoca_eval_flat::eval::{
    MODELICA_CONSTANTS, VarEnv, build_env, collect_var_dims, collect_var_starts, eval_array_values,
    eval_expr, set_array_entries,
};
use rumoca_ir_dae as dae;

fn clamp_finite(v: f64) -> f64 {
    if v.is_finite() { v } else { 0.0 }
}

fn expand_values_to_size(raw: Vec<f64>, size: usize) -> Vec<f64> {
    if size == 0 {
        return Vec::new();
    }
    if raw.len() == size {
        return raw;
    }
    if raw.is_empty() {
        return vec![0.0; size];
    }
    if raw.len() == 1 {
        return vec![raw[0]; size];
    }
    let last = *raw.last().unwrap_or(&0.0);
    let mut out = Vec::with_capacity(size);
    for idx in 0..size {
        out.push(raw.get(idx).copied().unwrap_or(last));
    }
    out
}

pub fn build_param_env_with_defaults(dae_model: &dae::Dae) -> VarEnv<f64> {
    let mut env = VarEnv::<f64>::new();

    if !dae_model.functions.is_empty() {
        let func_map = dae_model
            .functions
            .iter()
            .map(|(name, func)| (name.as_str().to_string(), func.clone()))
            .collect();
        env.functions = std::sync::Arc::new(func_map);
    }

    env.dims = std::sync::Arc::new(collect_var_dims(dae_model));
    env.start_exprs = std::sync::Arc::new(collect_var_starts(dae_model));
    env.enum_literal_ordinals = std::sync::Arc::new(dae_model.enum_literal_ordinals.clone());

    for &(fqn, value) in MODELICA_CONSTANTS {
        env.set(fqn, value);
    }

    for _ in 0..2 {
        for (name, var) in &dae_model.constants {
            let Some(start) = var.start.as_ref() else {
                continue;
            };
            let size = var.size();
            if size <= 1 {
                env.set(name.as_str(), eval_expr::<f64>(start, &env));
                continue;
            }
            let raw = if matches!(start, dae::Expression::Array { .. }) {
                eval_array_values::<f64>(start, &env)
            } else {
                vec![eval_expr::<f64>(start, &env); size]
            };
            let values = expand_values_to_size(raw, size);
            set_array_entries(&mut env, name.as_str(), &var.dims, &values);
        }
    }

    let params = crate::simulation::parameter_values::default_parameter_values(dae_model);
    let mut pidx = 0usize;
    for (name, var) in &dae_model.parameters {
        let size = var.size().max(1);
        if size == 1 {
            env.set(name.as_str(), params.get(pidx).copied().unwrap_or(0.0));
            pidx += 1;
            continue;
        }

        let mut values = vec![0.0; size];
        for (slot, value) in values.iter_mut().enumerate() {
            *value = params.get(pidx + slot).copied().unwrap_or(0.0);
        }
        set_array_entries(&mut env, name.as_str(), &var.dims, &values);
        pidx += size;
    }

    env
}

pub fn init_value_from_var(var: &dae::Variable, env: &VarEnv<f64>) -> f64 {
    if let Some(ref start) = var.start {
        return eval_expr::<f64>(start, env);
    }
    if let Some(ref nominal) = var.nominal {
        return eval_expr::<f64>(nominal, env);
    }
    0.0
}

pub fn initialize_state_vector(dae_model: &dae::Dae, y: &mut [f64]) {
    let env = build_param_env_with_defaults(dae_model);
    let mut idx = 0;
    for var in dae_model.states.values() {
        let value = init_value_from_var(var, &env);
        for _ in 0..var.size() {
            if idx < y.len() {
                y[idx] = value;
            }
            idx += 1;
        }
    }
    for var in dae_model.algebraics.values() {
        let value = init_value_from_var(var, &env);
        for _ in 0..var.size() {
            if idx < y.len() {
                y[idx] = value;
            }
            idx += 1;
        }
    }
    for var in dae_model.outputs.values() {
        let value = init_value_from_var(var, &env);
        for _ in 0..var.size() {
            if idx < y.len() {
                y[idx] = value;
            }
            idx += 1;
        }
    }
}

fn apply_initial_pre_assignments_from_env(
    dae_model: &dae::Dae,
    env: &rumoca_eval_flat::eval::VarEnv<f64>,
) {
    for eq in &dae_model.initial_equations {
        let Some((target, solution)) =
            crate::runtime::assignment::pre_assignment_from_initial_equation(eq)
        else {
            continue;
        };

        let value = clamp_finite(eval_expr::<f64>(solution, env));
        rumoca_eval_flat::eval::set_pre_value(target.as_str(), value);
    }
}

/// Seed `pre()` cache from a startup state and then apply explicit
/// `initial equation` `pre(...) = ...` assignments.
pub fn refresh_pre_values_from_state_with_initial_assignments(
    dae_model: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t_eval: f64,
) {
    let mut env = build_env(dae_model, y, p, t_eval);
    env.is_initial = true;
    rumoca_eval_flat::eval::seed_pre_values_from_env(&env);
    apply_initial_pre_assignments_from_env(dae_model, &env);
}

/// Apply initial section assignments (`initial equation` solved-form rows and
/// `pre(...)` initialization) to the startup solver vector/environment.
pub fn apply_initial_section_assignments(
    dae_model: &dae::Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> usize {
    if dae_model.initial_equations.is_empty() {
        return 0;
    }

    let crate::runtime::layout::SolverNameIndexMaps {
        names,
        name_to_idx,
        base_to_indices,
    } = crate::runtime::layout::build_solver_name_index_maps(dae_model, y.len());

    let max_passes = dae_model.initial_equations.len().clamp(1, 32);
    let mut updates = 0usize;

    for _ in 0..max_passes {
        let mut changed = false;
        let mut env = build_env(dae_model, y, p, t_eval);
        env.is_initial = true;
        let mut explicit_updates: HashSet<String> = HashSet::new();

        for eq in &dae_model.initial_equations {
            let Some((target, solution)) =
                crate::runtime::assignment::direct_assignment_from_equation(eq)
            else {
                continue;
            };

            if !target.contains('[')
                && let Some(indices) = base_to_indices.get(target.as_str())
                && indices.len() > 1
            {
                let values = crate::runtime::assignment::evaluate_direct_assignment_values(
                    solution,
                    &env,
                    indices.len(),
                );
                let (vector_changed, vector_updates) =
                    crate::runtime::assignment::apply_values_to_indices(
                        y, &mut env, &names, indices, &values,
                    );
                changed |= vector_changed;
                updates += vector_updates;
                continue;
            }

            let value = clamp_finite(eval_expr::<f64>(solution, &env));
            if env
                .vars
                .get(target.as_str())
                .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
            {
                env.set(target.as_str(), value);
                crate::runtime::alias::insert_name_and_base(&mut explicit_updates, target.as_str());
                changed = true;
            }

            if let Some(idx) =
                crate::runtime::layout::solver_idx_for_target(target.as_str(), &name_to_idx)
                && idx < y.len()
                && (y[idx] - value).abs() > 1.0e-12
            {
                y[idx] = value;
                changed = true;
                updates += 1;
            }
        }

        if crate::runtime::alias::propagate_discrete_alias_equalities(
            dae_model,
            &mut env,
            &mut explicit_updates,
            |_update| {},
        ) {
            changed = true;
        }

        rumoca_eval_flat::eval::seed_pre_values_from_env(&env);
        for eq in &dae_model.initial_equations {
            let Some((target, solution)) =
                crate::runtime::assignment::pre_assignment_from_initial_equation(eq)
            else {
                continue;
            };

            let value = clamp_finite(eval_expr::<f64>(solution, &env));
            let old = rumoca_eval_flat::eval::get_pre_value(target.as_str());
            if old.is_none_or(|existing| (existing - value).abs() > 1.0e-12) {
                rumoca_eval_flat::eval::set_pre_value(target.as_str(), value);
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }

    updates
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    fn scalar(name: &str) -> dae::Variable {
        dae::Variable::new(dae::VarName::new(name))
    }

    #[test]
    fn apply_initial_section_assignments_updates_solver_values() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar("x"));
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("x"),
            dae::Expression::Literal(dae::Literal::Real(2.5)),
            Span::DUMMY,
            "init x",
        ));

        let mut y = vec![0.0];
        let updates = apply_initial_section_assignments(&dae_model, &mut y, &[], 0.0);
        assert_eq!(updates, 1);
        assert!((y[0] - 2.5).abs() <= 1.0e-12);
    }

    #[test]
    fn apply_initial_section_assignments_sets_pre_values() {
        rumoca_eval_flat::eval::clear_pre_values();
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar("x"));
        dae_model.initial_equations.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: dae::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }],
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(4.0))),
            },
            Span::DUMMY,
            "init pre(x)",
        ));

        let mut y = vec![1.0];
        let _ = apply_initial_section_assignments(&dae_model, &mut y, &[], 0.0);
        let pre_x = rumoca_eval_flat::eval::get_pre_value("x").unwrap_or(f64::NAN);
        assert!((pre_x - 4.0).abs() <= 1.0e-12);
        rumoca_eval_flat::eval::clear_pre_values();
    }
}
