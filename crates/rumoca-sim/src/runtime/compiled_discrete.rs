use std::collections::{HashMap, HashSet};

use rumoca_eval_dae::runtime::VarEnv;
use rumoca_ir_dae as dae;

#[cfg(not(target_arch = "wasm32"))]
type CompiledExpressionRows = rumoca_eval_dae::compiled::CompiledExpressionRows;
#[cfg(target_arch = "wasm32")]
type CompiledExpressionRows = rumoca_eval_dae::compiled::CompiledExpressionRowsWasm;

fn equation_key(eq: &dae::Equation) -> usize {
    eq as *const dae::Equation as usize
}

fn expression_references_target(expr: &dae::Expression, target: &str) -> bool {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    if refs.contains(&dae::VarName::new(target)) {
        return true;
    }
    if let Some(base) = dae::component_base_name(target)
        && refs.contains(&dae::VarName::new(base))
    {
        return true;
    }
    false
}

fn contains_runtime_discrete_builtin(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::BuiltinCall { function, args } => {
            if matches!(
                function,
                dae::BuiltinFunction::Pre
                    | dae::BuiltinFunction::Sample
                    | dae::BuiltinFunction::Edge
                    | dae::BuiltinFunction::Change
                    | dae::BuiltinFunction::Reinit
                    | dae::BuiltinFunction::Initial
            ) {
                return true;
            }
            args.iter().any(contains_runtime_discrete_builtin)
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            if matches!(
                short,
                "sample"
                    | "pre"
                    | "edge"
                    | "change"
                    | "reinit"
                    | "initial"
                    | "Sample"
                    | "Pre"
                    | "Edge"
                    | "Change"
                    | "Reinit"
                    | "Initial"
                    | "noEvent"
                    | "NoEvent"
                    | "previous"
                    | "hold"
                    | "Clock"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "firstTick"
            ) {
                return true;
            }
            args.iter().any(contains_runtime_discrete_builtin)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            contains_runtime_discrete_builtin(lhs) || contains_runtime_discrete_builtin(rhs)
        }
        dae::Expression::Unary { rhs, .. } => contains_runtime_discrete_builtin(rhs),
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                contains_runtime_discrete_builtin(cond) || contains_runtime_discrete_builtin(value)
            }) || contains_runtime_discrete_builtin(else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(contains_runtime_discrete_builtin)
        }
        dae::Expression::Range { start, step, end } => {
            contains_runtime_discrete_builtin(start)
                || step
                    .as_deref()
                    .is_some_and(contains_runtime_discrete_builtin)
                || contains_runtime_discrete_builtin(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            contains_runtime_discrete_builtin(expr)
                || indices
                    .iter()
                    .any(|idx| contains_runtime_discrete_builtin(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(contains_runtime_discrete_builtin)
        }
        dae::Expression::Index { base, subscripts } => {
            contains_runtime_discrete_builtin(base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(expr) => contains_runtime_discrete_builtin(expr),
                    _ => false,
                })
        }
        dae::Expression::FieldAccess { base, .. } => contains_runtime_discrete_builtin(base),
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

fn has_runtime_binding(dae_model: &dae::Dae, name: &dae::VarName) -> bool {
    dae_model.states.contains_key(name)
        || dae_model.algebraics.contains_key(name)
        || dae_model.outputs.contains_key(name)
        || dae_model.inputs.contains_key(name)
        || dae_model.parameters.contains_key(name)
        || dae_model.constants.contains_key(name)
        || dae_model.discrete_reals.contains_key(name)
        || dae_model.discrete_valued.contains_key(name)
        || dae_model.derivative_aliases.contains_key(name)
}

fn expression_uses_only_known_runtime_bindings(
    dae_model: &dae::Dae,
    expr: &dae::Expression,
) -> bool {
    let mut refs = HashSet::new();
    expr.collect_var_refs(&mut refs);
    refs.into_iter().all(|name| {
        if name.as_str() == "time" {
            return true;
        }
        if has_runtime_binding(dae_model, &name) {
            return true;
        }
        dae::component_base_name(name.as_str())
            .map(|base| has_runtime_binding(dae_model, &dae::VarName::new(base)))
            .unwrap_or(false)
    })
}

fn can_use_compiled_discrete_rhs(
    dae_model: &dae::Dae,
    target: &str,
    solution: &dae::Expression,
) -> bool {
    if contains_runtime_discrete_builtin(solution) {
        return false;
    }
    if !expression_uses_only_known_runtime_bindings(dae_model, solution) {
        return false;
    }
    if crate::runtime::discrete::expr_uses_previous(solution) {
        return false;
    }
    let target_base = dae::component_base_name(target).unwrap_or_else(|| target.to_string());
    let state_target = dae_model
        .states
        .contains_key(&dae::VarName::new(target_base));
    if state_target && expression_references_target(solution, target) {
        return false;
    }
    true
}

pub struct CompiledDiscreteEventContext {
    sim_context: crate::runtime::layout::SimulationContext,
    compiled_scalar_rhs_by_eq: HashMap<usize, CompiledExpressionRows>,
}

impl CompiledDiscreteEventContext {
    fn eval_scalar_rhs(
        &self,
        eq: &dae::Equation,
        env: &VarEnv<f64>,
        p: &[f64],
        t_eval: f64,
        y_scratch: &mut [f64],
        out_scratch: &mut [f64],
    ) -> Option<f64> {
        let compiled = self.compiled_scalar_rhs_by_eq.get(&equation_key(eq))?;
        self.sim_context.sync_solver_values_from_env(y_scratch, env);
        let compiled_p = self.sim_context.compiled_parameter_vector_from_env(p, env);
        if compiled
            .call(y_scratch, &compiled_p, t_eval, out_scratch)
            .is_err()
        {
            return None;
        }
        out_scratch.first().copied()
    }
}

pub fn build_compiled_discrete_event_context(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Result<Option<CompiledDiscreteEventContext>, String> {
    let sim_context = crate::runtime::layout::SimulationContext::from_dae(dae_model, solver_len);
    let mut compiled_scalar_rhs_by_eq: HashMap<usize, CompiledExpressionRows> = HashMap::new();

    for eq in dae_model.f_z.iter().chain(dae_model.f_m.iter()) {
        if eq.scalar_count != 1 {
            continue;
        }
        let Some(target) = eq.lhs.as_ref() else {
            continue;
        };
        if !can_use_compiled_discrete_rhs(dae_model, target.as_str(), &eq.rhs) {
            continue;
        }
        let exprs = vec![eq.rhs.clone()];
        let compiled = compile_scalar_expression_rows(dae_model, &exprs).map_err(|err| {
            format!(
                "failed to compile discrete RHS for target='{}' origin='{}': {err}",
                target, eq.origin
            )
        })?;
        compiled_scalar_rhs_by_eq.insert(equation_key(eq), compiled);
    }

    if compiled_scalar_rhs_by_eq.is_empty() {
        Ok(None)
    } else {
        Ok(Some(CompiledDiscreteEventContext {
            sim_context,
            compiled_scalar_rhs_by_eq,
        }))
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn compile_scalar_expression_rows(
    dae_model: &dae::Dae,
    exprs: &[dae::Expression],
) -> Result<CompiledExpressionRows, String> {
    rumoca_eval_dae::compiled::compile_expressions(
        dae_model,
        exprs,
        rumoca_eval_dae::compiled::Backend::Cranelift,
    )
    .map_err(|err| err.to_string())
}

#[cfg(target_arch = "wasm32")]
fn compile_scalar_expression_rows(
    dae_model: &dae::Dae,
    exprs: &[dae::Expression],
) -> Result<CompiledExpressionRows, String> {
    rumoca_eval_dae::compiled::compile_expressions_wasm(dae_model, exprs)
        .map_err(|err| err.to_string())
}

pub fn settle_runtime_event_updates_with_compiled_discrete(
    dae_model: &dae::Dae,
    y: &mut [f64],
    p: &[f64],
    n_x: usize,
    t_eval: f64,
    compiled_discrete: Option<&CompiledDiscreteEventContext>,
) -> VarEnv<f64> {
    let y_len = y.len();
    let input = crate::EventSettleInput {
        dae: dae_model,
        y,
        p,
        n_x,
        t_eval,
    };
    let Some(compiled_discrete) = compiled_discrete else {
        return crate::settle_runtime_event_updates_default(input);
    };

    let mut y_scratch = vec![0.0; y_len];
    let mut out_scratch = vec![0.0; 1];
    crate::settle_runtime_event_updates(
        input,
        crate::runtime::assignment::propagate_runtime_direct_assignments_from_env,
        crate::runtime::alias::propagate_runtime_alias_components_from_env,
        |dae_model, env| {
            crate::runtime::discrete::apply_discrete_partition_updates_with_scalar_override(
                dae_model,
                env,
                |eq, _target, _solution, env, _implicit_clock_active| {
                    compiled_discrete.eval_scalar_rhs(
                        eq,
                        env,
                        p,
                        t_eval,
                        y_scratch.as_mut_slice(),
                        out_scratch.as_mut_slice(),
                    )
                },
            )
        },
        crate::runtime::layout::sync_solver_values_from_env,
    )
}
