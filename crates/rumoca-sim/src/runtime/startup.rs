use std::collections::{HashMap, HashSet};

use rumoca_phase_structural::scalarize::{
    build_expression_scalarization_context, scalarize_expression_rows,
};
use rumoca_eval_dae::runtime::{
    VarEnv, build_runtime_parameter_tail_env, map_var_to_env,
    refresh_env_solver_and_parameter_values, set_array_entries,
};
use rumoca_ir_dae as dae;

#[cfg(not(target_arch = "wasm32"))]
type CompiledExpressionRows = rumoca_eval_dae::compiled::CompiledExpressionRows;
#[cfg(target_arch = "wasm32")]
type CompiledExpressionRows = rumoca_eval_dae::compiled::CompiledExpressionRowsWasm;

fn clamp_finite(v: f64) -> f64 {
    if v.is_finite() { v } else { 0.0 }
}

fn equation_key(eq: &dae::Equation) -> usize {
    eq as *const dae::Equation as usize
}

struct CompiledStartupRows {
    compiled_rows: CompiledExpressionRows,
    output_len: usize,
}

struct CompiledStartupContext {
    sim_context: crate::runtime::layout::SimulationContext,
    compiled_rows_by_eq: HashMap<usize, CompiledStartupRows>,
}

struct StartupEvalContext<'a> {
    compiled_startup: Option<&'a CompiledStartupContext>,
    strict_compiled: bool,
    base_to_indices: &'a HashMap<String, Vec<usize>>,
    p: &'a [f64],
    t_eval: f64,
    y_scratch: &'a mut Vec<f64>,
    out_scratch: &'a mut Vec<f64>,
}

impl CompiledStartupContext {
    fn eval_solution_values<'a>(
        &self,
        eq: &dae::Equation,
        env: &VarEnv<f64>,
        p: &[f64],
        t_eval: f64,
        y_scratch: &mut Vec<f64>,
        out_scratch: &'a mut Vec<f64>,
    ) -> Option<&'a [f64]> {
        let compiled = self.compiled_rows_by_eq.get(&equation_key(eq))?;
        self.sim_context
            .sync_solver_values_from_env(y_scratch.as_mut_slice(), env);
        let compiled_p = self.sim_context.compiled_parameter_vector_from_env(p, env);
        out_scratch.resize(compiled.output_len, 0.0);
        if let Err(err) = compiled
            .compiled_rows
            .call(y_scratch, &compiled_p, t_eval, out_scratch)
        {
            panic!("compiled startup expression rows call failed: {err}");
        }
        Some(&out_scratch[..compiled.output_len])
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn compile_initial_scalar_expression_rows(
    dae_model: &dae::Dae,
    expressions: &[dae::Expression],
) -> Result<CompiledExpressionRows, String> {
    rumoca_eval_dae::compiled::compile_initial_expressions(
        dae_model,
        expressions,
        rumoca_eval_dae::compiled::Backend::Cranelift,
    )
    .map_err(|err| err.to_string())
}

#[cfg(target_arch = "wasm32")]
fn compile_initial_scalar_expression_rows(
    dae_model: &dae::Dae,
    expressions: &[dae::Expression],
) -> Result<CompiledExpressionRows, String> {
    rumoca_eval_dae::compiled::compile_initial_expressions_wasm(dae_model, expressions)
        .map_err(|err| err.to_string())
}

fn startup_target_size(
    dae_model: &dae::Dae,
    target: &str,
    base_to_indices: &HashMap<String, Vec<usize>>,
) -> usize {
    crate::runtime::assignment::variable_size_for_assignment_name(dae_model, target)
        .or_else(|| {
            (!target.contains('['))
                .then(|| base_to_indices.get(target).map(Vec::len))
                .flatten()
        })
        .unwrap_or(1)
}

fn assignment_target_dims(dae_model: &dae::Dae, target: &str, width: usize) -> Vec<i64> {
    let lookup = |name: &str| {
        dae_model
            .states
            .get(&dae::VarName::new(name))
            .or_else(|| dae_model.algebraics.get(&dae::VarName::new(name)))
            .or_else(|| dae_model.outputs.get(&dae::VarName::new(name)))
            .or_else(|| dae_model.inputs.get(&dae::VarName::new(name)))
            .or_else(|| dae_model.parameters.get(&dae::VarName::new(name)))
            .or_else(|| dae_model.constants.get(&dae::VarName::new(name)))
            .or_else(|| dae_model.discrete_reals.get(&dae::VarName::new(name)))
            .or_else(|| dae_model.discrete_valued.get(&dae::VarName::new(name)))
            .or_else(|| dae_model.derivative_aliases.get(&dae::VarName::new(name)))
            .map(|var| var.dims.clone())
    };

    lookup(target)
        .or_else(|| dae::component_base_name(target).and_then(|base| lookup(&base)))
        .unwrap_or_else(|| vec![width as i64])
}

fn build_startup_scalarized_solution_rows(
    dae_model: &dae::Dae,
    solution: &dae::Expression,
    output_len: usize,
) -> Result<CompiledStartupRows, String> {
    let scalarization = build_expression_scalarization_context(dae_model);
    let expressions = scalarize_expression_rows(solution, output_len, &scalarization);

    compile_initial_scalar_expression_rows(dae_model, &expressions).map(|compiled_rows| {
        CompiledStartupRows {
            compiled_rows,
            output_len,
        }
    })
}

fn build_compiled_startup_context(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Option<CompiledStartupContext> {
    build_compiled_startup_context_with_mode(dae_model, solver_len, false)
        .expect("non-strict startup compilation should not fail")
}

fn build_compiled_startup_context_strict(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Result<Option<CompiledStartupContext>, String> {
    build_compiled_startup_context_with_mode(dae_model, solver_len, true)
}

fn build_compiled_startup_context_with_mode(
    dae_model: &dae::Dae,
    solver_len: usize,
    strict_compiled: bool,
) -> Result<Option<CompiledStartupContext>, String> {
    let sim_context = crate::runtime::layout::SimulationContext::from_dae(dae_model, solver_len);
    let mut compiled_rows_by_eq = HashMap::new();
    let base_to_indices = &sim_context.solver_maps().base_to_indices;

    for eq in &dae_model.initial_equations {
        let Some((target, solution)) =
            crate::runtime::assignment::direct_assignment_from_equation(eq)
                .or_else(|| crate::runtime::assignment::pre_assignment_from_initial_equation(eq))
        else {
            continue;
        };

        let target_size = startup_target_size(dae_model, &target, base_to_indices);
        let compiled_rows =
            match build_startup_scalarized_solution_rows(dae_model, solution, target_size) {
                Ok(compiled_rows) => compiled_rows,
                Err(err) if strict_compiled => {
                    return Err(format!(
                        "compiled startup row unsupported origin='{}' target='{}': {err}",
                        eq.origin, target
                    ));
                }
                Err(_) => continue,
            };
        compiled_rows_by_eq.insert(equation_key(eq), compiled_rows);
    }

    if compiled_rows_by_eq.is_empty() {
        Ok(None)
    } else {
        Ok(Some(CompiledStartupContext {
            sim_context,
            compiled_rows_by_eq,
        }))
    }
}

fn build_initial_section_base_env(
    dae_model: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t_eval: f64,
) -> VarEnv<f64> {
    // MLS §8.6: startup evaluation must see the ordinary runtime tail
    // (parameters, inputs, discrete start values) together with the current
    // solver snapshot, while `initial()` itself is carried by `is_initial`.
    let mut env = build_runtime_parameter_tail_env(dae_model, p, t_eval);
    let mut idx = 0usize;
    for (name, var) in &dae_model.states {
        map_var_to_env(&mut env, name.as_str(), var, y, &mut idx);
    }
    for (name, var) in &dae_model.algebraics {
        map_var_to_env(&mut env, name.as_str(), var, y, &mut idx);
    }
    for (name, var) in &dae_model.outputs {
        map_var_to_env(&mut env, name.as_str(), var, y, &mut idx);
    }
    env.is_initial = true;
    env
}

fn expand_startup_values(values: &[f64], expected_len: usize) -> Vec<f64> {
    let raw: Vec<f64> = values.iter().copied().map(clamp_finite).collect();
    if expected_len == 0 {
        return Vec::new();
    }
    if raw.len() == expected_len {
        return raw;
    }
    if raw.is_empty() {
        return vec![0.0; expected_len];
    }
    if raw.len() == 1 {
        return vec![raw[0]; expected_len];
    }
    let last = *raw.last().unwrap_or(&0.0);
    let mut out = Vec::with_capacity(expected_len);
    for index in 0..expected_len {
        out.push(raw.get(index).copied().unwrap_or(last));
    }
    out
}

fn eval_initial_solution_values(
    eq: &dae::Equation,
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    expected_len: usize,
    ctx: &mut StartupEvalContext<'_>,
) -> Result<Vec<f64>, String> {
    if let Some(compiled_startup) = ctx.compiled_startup
        && let Some(values) = compiled_startup.eval_solution_values(
            eq,
            env,
            ctx.p,
            ctx.t_eval,
            ctx.y_scratch,
            ctx.out_scratch,
        )
    {
        return Ok(expand_startup_values(values, expected_len));
    }
    if ctx.strict_compiled {
        return Err(format!(
            "compiled startup row missing origin='{}' target='{}'",
            eq.origin, target
        ));
    }
    Ok(crate::runtime::assignment::evaluate_direct_assignment_values(solution, env, expected_len))
}

fn eval_initial_solution_value(
    eq: &dae::Equation,
    target: &str,
    solution: &dae::Expression,
    env: &VarEnv<f64>,
    ctx: &mut StartupEvalContext<'_>,
) -> Result<f64, String> {
    eval_initial_solution_values(eq, target, solution, env, 1, ctx)?
        .first()
        .copied()
        .ok_or_else(|| {
            format!(
                "compiled startup row produced no values origin='{}' target='{}'",
                eq.origin, target
            )
        })
}

fn apply_array_values_to_env(
    dae_model: &dae::Dae,
    env: &mut VarEnv<f64>,
    target: &str,
    values: &[f64],
) -> bool {
    let dims = assignment_target_dims(dae_model, target, values.len());
    let mut staged = VarEnv::new();
    let mut changed = false;
    set_array_entries(&mut staged, target, &dims, values);
    for (name, value) in staged.vars {
        if env
            .vars
            .get(name.as_str())
            .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
        {
            env.set(name.as_str(), value);
            changed = true;
        }
    }
    changed
}

fn apply_array_values_to_pre_store(dae_model: &dae::Dae, target: &str, values: &[f64]) -> bool {
    let dims = assignment_target_dims(dae_model, target, values.len());
    let mut staged = VarEnv::new();
    let mut changed = false;
    set_array_entries(&mut staged, target, &dims, values);
    for (name, value) in staged.vars {
        if rumoca_eval_dae::runtime::get_pre_value(name.as_str())
            .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
        {
            rumoca_eval_dae::runtime::set_pre_value(name.as_str(), value);
            changed = true;
        }
    }
    changed
}

fn apply_startup_array_values(
    dae_model: &dae::Dae,
    y: &mut [f64],
    env: &mut VarEnv<f64>,
    names: &[String],
    target: &str,
    indices: Option<&[usize]>,
    values: &[f64],
) -> (bool, usize) {
    let mut changed = false;
    let mut updates = 0usize;

    if let Some(indices) = indices.filter(|indices| !indices.is_empty()) {
        let (vector_changed, vector_updates) =
            crate::runtime::assignment::apply_values_to_indices(y, env, names, indices, values);
        changed |= vector_changed;
        updates += vector_updates;
    }

    changed |= apply_array_values_to_env(dae_model, env, target, values);
    (changed, updates)
}

fn apply_initial_pre_assignments_from_env(
    dae_model: &dae::Dae,
    env: &rumoca_eval_dae::runtime::VarEnv<f64>,
    ctx: &mut StartupEvalContext<'_>,
) -> Result<bool, String> {
    let mut changed = false;
    for eq in &dae_model.initial_equations {
        let Some((target, solution)) =
            crate::runtime::assignment::pre_assignment_from_initial_equation(eq)
        else {
            continue;
        };

        let target_size = startup_target_size(dae_model, &target, ctx.base_to_indices);
        if !target.contains('[') && target_size > 1 {
            let values =
                eval_initial_solution_values(eq, target.as_str(), solution, env, target_size, ctx)?;
            changed |= apply_array_values_to_pre_store(dae_model, target.as_str(), &values);
            continue;
        }

        let value = eval_initial_solution_value(eq, target.as_str(), solution, env, ctx)?;
        let old = rumoca_eval_dae::runtime::get_pre_value(target.as_str());
        if old.is_none_or(|existing| (existing - value).abs() > 1.0e-12) {
            rumoca_eval_dae::runtime::set_pre_value(target.as_str(), value);
            changed = true;
        }
    }
    Ok(changed)
}

fn build_initial_section_env_with_updates(
    dae_model: &dae::Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> (VarEnv<f64>, usize) {
    build_initial_section_env_with_updates_inner(dae_model, y, p, t_eval, false)
        .expect("non-strict startup env build should not fail")
}

fn build_initial_section_env_with_updates_inner(
    dae_model: &dae::Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
    strict_compiled: bool,
) -> Result<(VarEnv<f64>, usize), String> {
    let mut env = build_initial_section_base_env(dae_model, y, p, t_eval);
    if dae_model.initial_equations.is_empty() {
        return Ok((env, 0));
    }

    let crate::runtime::layout::SolverNameIndexMaps {
        names,
        name_to_idx,
        base_to_indices,
    } = crate::runtime::layout::build_solver_name_index_maps(dae_model, y.len());

    let compiled_startup = if strict_compiled {
        build_compiled_startup_context_strict(dae_model, y.len())?
    } else {
        build_compiled_startup_context(dae_model, y.len())
    };
    let mut y_scratch = vec![0.0; y.len()];
    let mut out_scratch = Vec::new();
    let mut eval_ctx = StartupEvalContext {
        compiled_startup: compiled_startup.as_ref(),
        strict_compiled,
        base_to_indices: &base_to_indices,
        p,
        t_eval,
        y_scratch: &mut y_scratch,
        out_scratch: &mut out_scratch,
    };
    let max_passes = dae_model.initial_equations.len().clamp(1, 32);
    let mut updates = 0usize;

    for _ in 0..max_passes {
        let mut changed = false;
        let mut explicit_updates: HashSet<String> = HashSet::new();

        for eq in &dae_model.initial_equations {
            let Some((target, solution)) =
                crate::runtime::assignment::direct_assignment_from_equation(eq)
            else {
                continue;
            };

            let target_size = startup_target_size(dae_model, &target, &base_to_indices);
            if !target.contains('[') && target_size > 1 {
                let values = eval_initial_solution_values(
                    eq,
                    target.as_str(),
                    solution,
                    &env,
                    target_size,
                    &mut eval_ctx,
                )?;
                let (vector_changed, vector_updates) = apply_startup_array_values(
                    dae_model,
                    y,
                    &mut env,
                    &names,
                    target.as_str(),
                    base_to_indices.get(target.as_str()).map(Vec::as_slice),
                    &values,
                );
                changed |= vector_changed;
                updates += vector_updates;
                record_explicit_startup_update(
                    &mut explicit_updates,
                    target.as_str(),
                    vector_changed,
                );
                continue;
            }

            let y_slot =
                crate::runtime::layout::solver_idx_for_target(target.as_str(), &name_to_idx)
                    .filter(|idx| *idx < y.len())
                    .map(|idx| &mut y[idx]);
            let (scalar_changed, scalar_updates) = apply_startup_scalar_value(
                eq,
                target.as_str(),
                solution,
                &mut env,
                &mut eval_ctx,
                &mut explicit_updates,
                y_slot,
            )?;
            changed |= scalar_changed;
            updates += scalar_updates;
        }

        if crate::runtime::alias::propagate_discrete_alias_equalities(
            dae_model,
            &mut env,
            &mut explicit_updates,
            |_update| {},
        ) {
            changed = true;
        }

        rumoca_eval_dae::runtime::seed_pre_values_from_env(&env);
        changed |= apply_initial_pre_assignments_from_env(dae_model, &env, &mut eval_ctx)?;

        if !changed {
            break;
        }

        refresh_env_solver_and_parameter_values(&mut env, dae_model, y, p, t_eval);
        env.is_initial = true;
    }

    Ok((env, updates))
}

fn record_explicit_startup_update(
    explicit_updates: &mut HashSet<String>,
    target: &str,
    changed: bool,
) {
    if changed {
        crate::runtime::alias::insert_name_and_base(explicit_updates, target);
    }
}

fn apply_startup_scalar_value(
    eq: &dae::Equation,
    target: &str,
    solution: &dae::Expression,
    env: &mut VarEnv<f64>,
    eval_ctx: &mut StartupEvalContext<'_>,
    explicit_updates: &mut HashSet<String>,
    y_slot: Option<&mut f64>,
) -> Result<(bool, usize), String> {
    let value = eval_initial_solution_value(eq, target, solution, env, eval_ctx)?;
    let mut changed = false;
    let mut updates = 0usize;
    if env
        .vars
        .get(target)
        .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
    {
        env.set(target, value);
        crate::runtime::alias::insert_name_and_base(explicit_updates, target);
        changed = true;
    }
    if let Some(slot) = y_slot
        && (*slot - value).abs() > 1.0e-12
    {
        *slot = value;
        changed = true;
        updates += 1;
    }
    Ok((changed, updates))
}

pub fn build_initial_section_env(
    dae_model: &dae::Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> VarEnv<f64> {
    build_initial_section_env_with_updates(dae_model, y, p, t_eval).0
}

pub fn build_initial_section_env_strict(
    dae_model: &dae::Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> Result<VarEnv<f64>, String> {
    Ok(build_initial_section_env_with_updates_inner(dae_model, y, p, t_eval, true)?.0)
}

/// Seed `pre()` cache from a startup state and then apply explicit
/// `initial equation` `pre(...) = ...` assignments.
pub fn refresh_pre_values_from_state_with_initial_assignments(
    dae_model: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t_eval: f64,
) {
    refresh_pre_values_from_state_with_initial_assignments_inner(dae_model, y, p, t_eval, false)
        .expect("non-strict startup pre refresh should not fail");
}

pub fn refresh_pre_values_from_state_with_initial_assignments_strict(
    dae_model: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t_eval: f64,
) -> Result<(), String> {
    refresh_pre_values_from_state_with_initial_assignments_inner(dae_model, y, p, t_eval, true)
}

fn refresh_pre_values_from_state_with_initial_assignments_inner(
    dae_model: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t_eval: f64,
    strict_compiled: bool,
) -> Result<(), String> {
    let mut startup_y = y.to_vec();
    let pre_seed_env = build_initial_section_base_env(dae_model, y, p, t_eval);
    let env = build_initial_section_env_with_updates_inner(
        dae_model,
        startup_y.as_mut_slice(),
        p,
        t_eval,
        strict_compiled,
    )?
    .0;
    let base_to_indices =
        crate::runtime::layout::build_solver_name_index_maps(dae_model, y.len()).base_to_indices;
    let compiled_startup = if strict_compiled {
        build_compiled_startup_context_strict(dae_model, y.len())?
    } else {
        build_compiled_startup_context(dae_model, y.len())
    };
    let mut y_scratch = vec![0.0; y.len()];
    let mut out_scratch = Vec::new();
    let mut eval_ctx = StartupEvalContext {
        compiled_startup: compiled_startup.as_ref(),
        strict_compiled,
        base_to_indices: &base_to_indices,
        p,
        t_eval,
        y_scratch: &mut y_scratch,
        out_scratch: &mut out_scratch,
    };
    // MLS §8.6 / Appendix B: `pre(v)` at the initial event starts from the
    // startup left-limit state, not the already-updated current value after
    // initial-equation/discrete iteration.
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&pre_seed_env);
    let _ = apply_initial_pre_assignments_from_env(dae_model, &env, &mut eval_ctx)?;
    Ok(())
}

/// Apply initial section assignments (`initial equation` solved-form rows and
/// `pre(...)` initialization) to the startup solver vector/environment.
pub fn apply_initial_section_assignments(
    dae_model: &dae::Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> usize {
    build_initial_section_env_with_updates(dae_model, y, p, t_eval).1
}

pub fn apply_initial_section_assignments_strict(
    dae_model: &dae::Dae,
    y: &mut [f64],
    p: &[f64],
    t_eval: f64,
) -> Result<usize, String> {
    Ok(build_initial_section_env_with_updates_inner(dae_model, y, p, t_eval, true)?.1)
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
        rumoca_eval_dae::runtime::clear_pre_values();
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar("x"));
        dae_model.initial_equations.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
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
        let pre_x = rumoca_eval_dae::runtime::get_pre_value("x").unwrap_or(f64::NAN);
        assert!((pre_x - 4.0).abs() <= 1.0e-12);
        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn refresh_pre_values_from_state_with_initial_assignments_keeps_left_limit_seed() {
        rumoca_eval_dae::runtime::clear_pre_values();
        let mut dae_model = dae::Dae::default();
        let mut last = dae::Variable::new(dae::VarName::new("last"));
        last.start = Some(dae::Expression::Literal(dae::Literal::Integer(1)));
        dae_model
            .discrete_valued
            .insert(dae::VarName::new("last"), last);
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("last"),
            dae::Expression::Literal(dae::Literal::Integer(2)),
            Span::DUMMY,
            "init last=2",
        ));

        refresh_pre_values_from_state_with_initial_assignments(&dae_model, &[], &[], 0.0);

        let pre_last = rumoca_eval_dae::runtime::get_pre_value("last").unwrap_or(f64::NAN);
        assert!((pre_last - 1.0).abs() <= 1.0e-12);
        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn build_initial_section_env_converges_non_solver_direct_chain() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar("x"));
        dae_model.discrete_reals.insert(
            dae::VarName::new("d"),
            dae::Variable::new(dae::VarName::new("d")),
        );
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("x"),
            dae::Expression::VarRef {
                name: dae::VarName::new("d"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "init x=d",
        ));
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("d"),
            dae::Expression::Literal(dae::Literal::Real(4.0)),
            Span::DUMMY,
            "init d=4",
        ));

        let mut y = vec![0.0];
        let env = build_initial_section_env(&dae_model, &mut y, &[], 0.0);
        assert!((y[0] - 4.0).abs() <= 1.0e-12);
        assert_eq!(env.vars.get("x").copied(), Some(4.0));
        assert_eq!(env.vars.get("d").copied(), Some(4.0));
    }

    #[test]
    fn build_compiled_startup_context_treats_initial_builtin_as_true() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar("x"));
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("x"),
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Initial,
                        args: vec![],
                    },
                    dae::Expression::Literal(dae::Literal::Real(3.0)),
                )],
                else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(-1.0))),
            },
            Span::DUMMY,
            "init x=if initial() then 3 else -1",
        ));

        let compiled = build_compiled_startup_context(&dae_model, 1).expect("compiled startup");
        let env = build_initial_section_base_env(&dae_model, &[0.0], &[], 0.0);
        let mut y_scratch = vec![0.0];
        let mut out_scratch = Vec::new();
        let values = compiled
            .eval_solution_values(
                &dae_model.initial_equations[0],
                &env,
                &[],
                0.0,
                &mut y_scratch,
                &mut out_scratch,
            )
            .expect("compiled startup value");
        let value = values.first().copied().unwrap_or(f64::NAN);
        assert!((value - 3.0).abs() <= 1.0e-12);
    }

    #[test]
    fn build_compiled_startup_context_compiles_scalar_pre_assignment_solution() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar("x"));
        dae_model.initial_equations.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }],
                }),
                rhs: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Add(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                }),
            },
            Span::DUMMY,
            "init pre(x)=x+1",
        ));

        let compiled = build_compiled_startup_context(&dae_model, 1).expect("compiled startup");
        let env = build_initial_section_base_env(&dae_model, &[2.0], &[], 0.0);
        let mut y_scratch = vec![0.0];
        let mut out_scratch = Vec::new();
        let values = compiled
            .eval_solution_values(
                &dae_model.initial_equations[0],
                &env,
                &[],
                0.0,
                &mut y_scratch,
                &mut out_scratch,
            )
            .expect("compiled startup value");
        let value = values.first().copied().unwrap_or(f64::NAN);
        assert!((value - 3.0).abs() <= 1.0e-12);
    }

    #[test]
    fn build_compiled_startup_context_compiles_array_direct_assignment_solution() {
        let mut dae_model = dae::Dae::default();
        let mut x = scalar("x");
        x.dims = vec![2];
        dae_model.algebraics.insert(dae::VarName::new("x"), x);
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("x"),
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Initial,
                        args: vec![],
                    },
                    dae::Expression::Literal(dae::Literal::Real(4.0)),
                )],
                else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(-1.0))),
            },
            Span::DUMMY,
            "init x = if initial() then 4 else -1",
        ));

        let compiled = build_compiled_startup_context(&dae_model, 2).expect("compiled startup");
        let env = build_initial_section_base_env(&dae_model, &[0.0, 0.0], &[], 0.0);
        let mut y_scratch = vec![0.0; 2];
        let mut out_scratch = Vec::new();
        let values = compiled
            .eval_solution_values(
                &dae_model.initial_equations[0],
                &env,
                &[],
                0.0,
                &mut y_scratch,
                &mut out_scratch,
            )
            .expect("compiled startup values");
        assert_eq!(values, &[4.0, 4.0]);
    }

    #[test]
    fn apply_initial_section_assignments_supports_scalarized_array_targets() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x[1]"), scalar("x[1]"));
        dae_model
            .algebraics
            .insert(dae::VarName::new("x[2]"), scalar("x[2]"));
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("x"),
            dae::Expression::Literal(dae::Literal::Real(5.0)),
            Span::DUMMY,
            "init x=5",
        ));

        let mut y = vec![0.0, 0.0];
        let updates = apply_initial_section_assignments(&dae_model, &mut y, &[], 0.0);
        assert_eq!(updates, 2);
        assert_eq!(y, vec![5.0, 5.0]);
    }

    #[test]
    fn apply_initial_section_assignments_strict_supports_scalarized_array_targets() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x[1]"), scalar("x[1]"));
        dae_model
            .algebraics
            .insert(dae::VarName::new("x[2]"), scalar("x[2]"));
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("x"),
            dae::Expression::Literal(dae::Literal::Real(5.0)),
            Span::DUMMY,
            "init x=5",
        ));

        let mut y = vec![0.0, 0.0];
        let updates =
            apply_initial_section_assignments_strict(&dae_model, &mut y, &[], 0.0).unwrap();
        assert_eq!(updates, 2);
        assert_eq!(y, vec![5.0, 5.0]);
    }

    #[test]
    fn build_initial_section_env_propagates_array_initial_alias_values() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_valued.insert(
            dae::VarName::new("src"),
            dae::Variable {
                name: dae::VarName::new("src"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("dst"),
            dae::Variable {
                name: dae::VarName::new("dst"),
                dims: vec![2],
                ..Default::default()
            },
        );
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("src"),
            dae::Expression::Literal(dae::Literal::Real(2.0)),
            Span::DUMMY,
            "init src=2",
        ));
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new("dst"),
            dae::Expression::VarRef {
                name: dae::VarName::new("src"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "dst = src",
        ));

        let mut y = Vec::<f64>::new();
        let env = build_initial_section_env(&dae_model, &mut y, &[], 0.0);
        assert_eq!(env.vars.get("src[1]").copied(), Some(2.0));
        assert_eq!(env.vars.get("src[2]").copied(), Some(2.0));
        assert_eq!(env.vars.get("dst[1]").copied(), Some(2.0));
        assert_eq!(env.vars.get("dst[2]").copied(), Some(2.0));
    }

    #[test]
    fn build_initial_section_env_broadcasts_enum_literal_to_discrete_array_target() {
        let mut dae_model = dae::Dae::default();
        dae_model.enum_literal_ordinals.insert(
            "Modelica.Electrical.Digital.Interfaces.Logic.'X'".to_string(),
            2,
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("y"),
            dae::Variable {
                name: dae::VarName::new("y"),
                dims: vec![3],
                ..Default::default()
            },
        );
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("y"),
            dae::Expression::VarRef {
                name: dae::VarName::new("Modelica.Electrical.Digital.Interfaces.Logic.'X'"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "init y=Logic.'X'",
        ));

        let mut y = Vec::<f64>::new();
        let env = build_initial_section_env(&dae_model, &mut y, &[], 0.0);
        assert_eq!(env.vars.get("y[1]").copied(), Some(2.0));
        assert_eq!(env.vars.get("y[2]").copied(), Some(2.0));
        assert_eq!(env.vars.get("y[3]").copied(), Some(2.0));
    }

    #[test]
    fn build_initial_section_env_bootstrap_preserves_runtime_tail_values() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar("x"));
        let mut p_var = scalar("p");
        p_var.start = Some(dae::Expression::Literal(dae::Literal::Real(3.0)));
        dae_model.parameters.insert(dae::VarName::new("p"), p_var);
        let mut u_var = scalar("u");
        u_var.start = Some(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("p"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        });
        dae_model.inputs.insert(dae::VarName::new("u"), u_var);
        let mut d_var = scalar("d");
        d_var.start = Some(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("u"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
        });
        dae_model
            .discrete_reals
            .insert(dae::VarName::new("d"), d_var);
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("x"),
            dae::Expression::VarRef {
                name: dae::VarName::new("d"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "init x=d",
        ));

        let mut y = vec![0.0];
        let env = build_initial_section_env(&dae_model, &mut y, &[3.0], 0.0);
        assert_eq!(y, vec![6.0]);
        assert_eq!(env.vars.get("p").copied(), Some(3.0));
        assert_eq!(env.vars.get("u").copied(), Some(4.0));
        assert_eq!(env.vars.get("d").copied(), Some(6.0));
        assert_eq!(env.vars.get("x").copied(), Some(6.0));
    }

    #[test]
    fn build_initial_section_env_strict_bootstrap_preserves_runtime_tail_values() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar("x"));
        let mut p_var = scalar("p");
        p_var.start = Some(dae::Expression::Literal(dae::Literal::Real(3.0)));
        dae_model.parameters.insert(dae::VarName::new("p"), p_var);
        let mut u_var = scalar("u");
        u_var.start = Some(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("p"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        });
        dae_model.inputs.insert(dae::VarName::new("u"), u_var);
        let mut d_var = scalar("d");
        d_var.start = Some(dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("u"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
        });
        dae_model
            .discrete_reals
            .insert(dae::VarName::new("d"), d_var);
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("x"),
            dae::Expression::VarRef {
                name: dae::VarName::new("d"),
                subscripts: vec![],
            },
            Span::DUMMY,
            "init x=d",
        ));

        let mut y = vec![0.0];
        let env = build_initial_section_env_strict(&dae_model, &mut y, &[3.0], 0.0).unwrap();
        assert_eq!(y, vec![6.0]);
        assert_eq!(env.vars.get("p").copied(), Some(3.0));
        assert_eq!(env.vars.get("u").copied(), Some(4.0));
        assert_eq!(env.vars.get("d").copied(), Some(6.0));
        assert_eq!(env.vars.get("x").copied(), Some(6.0));
    }

    #[test]
    fn apply_initial_section_assignments_strict_rejects_unsupported_direct_assignment() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar("x"));
        dae_model.inputs.insert(dae::VarName::new("u"), scalar("u"));
        dae_model.initial_equations.push(dae::Equation::explicit(
            dae::VarName::new("x"),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Sample,
                args: vec![
                    dae::Expression::VarRef {
                        name: dae::VarName::new("u"),
                        subscripts: vec![],
                    },
                    dae::Expression::Literal(dae::Literal::Real(1.0)),
                ],
            },
            Span::DUMMY,
            "init x=sample(u,1)",
        ));

        let mut y = vec![0.0];
        let err =
            apply_initial_section_assignments_strict(&dae_model, &mut y, &[], 0.0).unwrap_err();
        assert!(err.contains("origin='init x=sample(u,1)'"));
        assert!(err.contains("target='x'"));
    }

    #[test]
    fn refresh_pre_values_from_state_with_initial_assignments_supports_array_pre_targets() {
        rumoca_eval_dae::runtime::clear_pre_values();
        let mut dae_model = dae::Dae::default();
        let mut x = scalar("x");
        x.dims = vec![2];
        dae_model.algebraics.insert(dae::VarName::new("x"), x);
        dae_model.initial_equations.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }],
                }),
                rhs: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Add(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                }),
            },
            Span::DUMMY,
            "init pre(x)=x+1",
        ));

        refresh_pre_values_from_state_with_initial_assignments(&dae_model, &[2.0, 3.0], &[], 0.0);
        assert_eq!(rumoca_eval_dae::runtime::get_pre_value("x"), Some(3.0));
        assert_eq!(rumoca_eval_dae::runtime::get_pre_value("x[1]"), Some(3.0));
        assert_eq!(rumoca_eval_dae::runtime::get_pre_value("x[2]"), Some(4.0));
        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn refresh_pre_values_from_state_with_initial_assignments_strict_supports_array_pre_targets() {
        rumoca_eval_dae::runtime::clear_pre_values();
        let mut dae_model = dae::Dae::default();
        let mut x = scalar("x");
        x.dims = vec![2];
        dae_model.algebraics.insert(dae::VarName::new("x"), x);
        dae_model.initial_equations.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }],
                }),
                rhs: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Add(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
                }),
            },
            Span::DUMMY,
            "init pre(x)=x+1",
        ));

        refresh_pre_values_from_state_with_initial_assignments_strict(
            &dae_model,
            &[2.0, 3.0],
            &[],
            0.0,
        )
        .unwrap();
        assert_eq!(rumoca_eval_dae::runtime::get_pre_value("x"), Some(3.0));
        assert_eq!(rumoca_eval_dae::runtime::get_pre_value("x[1]"), Some(3.0));
        assert_eq!(rumoca_eval_dae::runtime::get_pre_value("x[2]"), Some(4.0));
        rumoca_eval_dae::runtime::clear_pre_values();
    }

    #[test]
    fn refresh_pre_values_from_state_with_initial_assignments_strict_rejects_unsupported_pre_row() {
        rumoca_eval_dae::runtime::clear_pre_values();
        let mut dae_model = dae::Dae::default();
        dae_model
            .algebraics
            .insert(dae::VarName::new("x"), scalar("x"));
        dae_model.inputs.insert(dae::VarName::new("u"), scalar("u"));
        dae_model.initial_equations.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("x"),
                        subscripts: vec![],
                    }],
                }),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sample,
                    args: vec![
                        dae::Expression::VarRef {
                            name: dae::VarName::new("u"),
                            subscripts: vec![],
                        },
                        dae::Expression::Literal(dae::Literal::Real(1.0)),
                    ],
                }),
            },
            Span::DUMMY,
            "init pre(x)=sample(u,1)",
        ));

        let err = refresh_pre_values_from_state_with_initial_assignments_strict(
            &dae_model,
            &[0.0],
            &[],
            0.0,
        )
        .unwrap_err();
        assert!(err.contains("origin='init pre(x)=sample(u,1)'"));
        assert!(err.contains("target='x'"));
        rumoca_eval_dae::runtime::clear_pre_values();
    }
}
