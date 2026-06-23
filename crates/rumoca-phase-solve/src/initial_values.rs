use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use rumoca_core::OpBinary;
use rumoca_eval_dae::eval::{EvalError, EvalRuntimeState};
use rumoca_eval_dae::{
    InitialAssignment, eval_array_values, eval_expr, eval_selected_function_output_pub,
    initial_assignment_from_equation, map_var_to_env, resolve_function_call_outputs_pub,
    seed_pre_values_in_env_runtime, set_array_entries, set_pre_value_in_env,
    try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime,
};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::solve_model::{
    SolveModelLowerError, expand_values_to_size, replace_if_changed, runtime_tail_error,
    scalar_names,
};

pub(crate) fn apply_initial_equations_to_start_values(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    params: &mut [f64],
    initial_y: &mut [f64],
    runtime: Arc<EvalRuntimeState>,
) -> Result<(), SolveModelLowerError> {
    let mut env = start_value_env(dae_model, params, initial_y, runtime)?;
    seed_pre_values_in_env_runtime(&env);
    let aliases = initial_runtime_alias_equalities(dae_model);
    let mut pinned = fixed_start_pins(dae_model);
    let continuous_seed_pins = explicit_start_pins(dae_model);
    let mut target_resolver = AssignmentTargetResolver::new(dae_model, layout);
    let init_eq_count = dae_model.initialization.equations.len();
    let max_passes = init_eq_count.max(aliases.len()).clamp(1, 32);
    for _ in 0..max_passes {
        let mut changed = false;
        for eq in &dae_model.initialization.equations {
            let Some(assignment) = initial_assignment_from_equation(eq) else {
                continue;
            };
            if solution_is_runtime_alias(layout, assignment.solution) {
                continue;
            }
            let targets = target_resolver.scalar_names(assignment.target.as_str());
            if targets.is_empty() {
                continue;
            }
            let values = initial_assignment_values(
                assignment.solution,
                &env,
                assignment.target.as_str(),
                &targets,
            )
            .map_err(|source| SolveModelLowerError::Evaluation {
                context: format!("initial assignment for `{}`", assignment.target),
                source,
                span: assignment.solution.span().or(Some(eq.span)),
            })?;
            changed |= pin_initial_assignment_targets(&targets, &mut pinned);
            changed |= apply_initial_assignment_values(
                InitialAssignmentApplyContext {
                    dae_model,
                    layout,
                    params,
                    initial_y,
                    env: &mut env,
                },
                &assignment,
                &targets,
                &values,
            );
        }
        changed |= seed_continuous_assignments(
            dae_model,
            layout,
            params,
            initial_y,
            &mut env,
            &mut target_resolver,
            &mut pinned,
            &continuous_seed_pins,
        )?;
        changed |= InitialAliasPropagation {
            dae_model,
            layout,
            params,
            initial_y,
            env: &mut env,
            pinned: &mut pinned,
        }
        .propagate(&aliases);
        if !changed {
            break;
        }
    }
    Ok(())
}

fn seed_continuous_assignments(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    params: &mut [f64],
    initial_y: &mut [f64],
    env: &mut rumoca_eval_dae::VarEnv<f64>,
    target_resolver: &mut AssignmentTargetResolver<'_>,
    pinned: &mut HashSet<String>,
    continuous_seed_pins: &HashSet<String>,
) -> Result<bool, SolveModelLowerError> {
    let mut changed = false;
    for eq in &dae_model.continuous.equations {
        let Some(assignment) = initial_assignment_from_equation(eq) else {
            continue;
        };
        if solution_is_runtime_alias(layout, assignment.solution) {
            continue;
        }
        let targets = target_resolver.scalar_names(assignment.target.as_str());
        if targets.is_empty() {
            continue;
        }
        if targets
            .iter()
            .any(|target| pinned.contains(target) || continuous_seed_pins.contains(target))
        {
            continue;
        }
        let values = match initial_assignment_values(
            assignment.solution,
            env,
            assignment.target.as_str(),
            &targets,
        ) {
            Ok(values) => values,
            Err(err) if err.missing_binding_name().is_some() => continue,
            Err(EvalError::UnsupportedExpression { .. }) => continue,
            Err(source) => {
                return Err(SolveModelLowerError::Evaluation {
                    context: format!(
                        "continuous initial seed assignment for `{}`",
                        assignment.target
                    ),
                    source,
                    span: assignment.solution.span().or(Some(eq.span)),
                });
            }
        };
        if values.iter().any(|value| !value.is_finite()) {
            continue;
        }
        changed |= pin_initial_assignment_targets(&targets, pinned);
        changed |= apply_initial_assignment_values(
            InitialAssignmentApplyContext {
                dae_model,
                layout,
                params,
                initial_y,
                env,
            },
            &assignment,
            &targets,
            &values,
        );
    }
    Ok(changed)
}

fn fixed_start_pins(dae_model: &dae::Dae) -> HashSet<String> {
    fixed_start_vars(dae_model)
        .flat_map(|(name, var)| scalar_names(name.as_str(), var))
        .collect()
}

fn explicit_start_pins(dae_model: &dae::Dae) -> HashSet<String> {
    explicit_start_vars(dae_model)
        .flat_map(|(name, var)| scalar_names(name.as_str(), var))
        .collect()
}

fn fixed_start_vars(
    dae_model: &dae::Dae,
) -> impl Iterator<Item = (&rumoca_core::VarName, &dae::Variable)> {
    let vars = &dae_model.variables;
    vars.parameters
        .iter()
        .chain(&vars.constants)
        .filter(|(name, var)| {
            var.start.is_some() && var.fixed != Some(false) && !is_generated_pre_name(name)
        })
        .chain(
            vars.states
                .iter()
                .chain(&vars.algebraics)
                .chain(&vars.outputs)
                .chain(&vars.inputs)
                .chain(&vars.discrete_reals)
                .chain(&vars.discrete_valued)
                .filter(|(_, var)| var.start.is_some() && var.fixed == Some(true)),
        )
}

fn explicit_start_vars(
    dae_model: &dae::Dae,
) -> impl Iterator<Item = (&rumoca_core::VarName, &dae::Variable)> {
    let vars = &dae_model.variables;
    vars.parameters
        .iter()
        .chain(&vars.constants)
        .filter(|(name, var)| var.start.is_some() && !is_generated_pre_name(name))
        .chain(
            vars.states
                .iter()
                .chain(&vars.algebraics)
                .chain(&vars.outputs)
                .chain(&vars.inputs)
                .chain(&vars.discrete_reals)
                .chain(&vars.discrete_valued)
                .filter(|(_, var)| var.start.is_some()),
        )
}

fn is_generated_pre_name(name: &rumoca_core::VarName) -> bool {
    name.as_str().starts_with("__pre__.")
}

#[derive(Debug, Clone)]
struct InitialRuntimeAlias {
    lhs: rumoca_core::VarName,
    rhs: rumoca_core::VarName,
}

fn initial_runtime_alias_equalities(dae_model: &dae::Dae) -> Vec<InitialRuntimeAlias> {
    dae_model
        .initialization
        .equations
        .iter()
        .chain(dae_model.continuous.equations.iter())
        .chain(dae_model.discrete.real_updates.iter())
        .chain(dae_model.discrete.valued_updates.iter())
        .filter_map(alias_equality_from_equation)
        .collect()
}

fn alias_equality_from_equation(eq: &dae::Equation) -> Option<InitialRuntimeAlias> {
    if let Some(lhs) = eq.lhs.as_ref() {
        return scalar_alias_name(&eq.rhs).map(|rhs| InitialRuntimeAlias {
            lhs: lhs.clone(),
            rhs,
        });
    }
    residual_alias_equality(&eq.rhs)
}

fn residual_alias_equality(expr: &rumoca_core::Expression) -> Option<InitialRuntimeAlias> {
    let rumoca_core::Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expr
    else {
        return None;
    };
    Some(InitialRuntimeAlias {
        lhs: scalar_alias_name(lhs)?,
        rhs: scalar_alias_name(rhs)?,
    })
}

fn scalar_alias_name(expr: &rumoca_core::Expression) -> Option<rumoca_core::VarName> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name.var_name().clone()),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args,
            ..
        } => scalar_alias_name(args.first()?),
        _ => None,
    }
}

fn solution_is_runtime_alias(layout: &solve::VarLayout, expr: &rumoca_core::Expression) -> bool {
    let Some(alias) = scalar_alias_name(expr) else {
        return false;
    };
    matches!(
        layout.binding(alias.as_str()),
        Some(solve::ScalarSlot::Y { .. } | solve::ScalarSlot::P { .. })
    )
}

fn start_value_env(
    dae_model: &dae::Dae,
    params: &[f64],
    initial_y: &[f64],
    runtime: Arc<EvalRuntimeState>,
) -> Result<rumoca_eval_dae::VarEnv<f64>, SolveModelLowerError> {
    let mut env = try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        dae_model, params, 0.0, runtime,
    )
    .map_err(|source| runtime_tail_error(dae_model, source))?;
    env.is_initial = true;

    let mut y_idx = 0usize;
    for (name, var) in dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
    {
        map_var_to_env(&mut env, name.as_str(), var, initial_y, &mut y_idx);
    }

    let mut p_idx = 0usize;
    for (name, var) in dae_model
        .variables
        .parameters
        .iter()
        .chain(dae_model.variables.inputs.iter())
        .chain(dae_model.variables.discrete_reals.iter())
        .chain(dae_model.variables.discrete_valued.iter())
    {
        map_var_to_env(&mut env, name.as_str(), var, params, &mut p_idx);
    }
    Ok(env)
}

struct AssignmentTargetResolver<'a> {
    dae_model: &'a dae::Dae,
    layout: &'a solve::VarLayout,
    cache: HashMap<String, Vec<String>>,
}

impl<'a> AssignmentTargetResolver<'a> {
    fn new(dae_model: &'a dae::Dae, layout: &'a solve::VarLayout) -> Self {
        Self {
            dae_model,
            layout,
            cache: HashMap::new(),
        }
    }

    fn scalar_names(&mut self, target: &str) -> Vec<String> {
        if let Some(names) = self.cache.get(target) {
            return names.clone();
        }
        let names = resolve_assignment_target_scalar_names(self.dae_model, self.layout, target);
        self.cache.insert(target.to_string(), names.clone());
        names
    }
}

#[cfg(test)]
fn assignment_target_scalar_names(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    target: &str,
) -> Vec<String> {
    AssignmentTargetResolver::new(dae_model, layout).scalar_names(target)
}

fn resolve_assignment_target_scalar_names(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    target: &str,
) -> Vec<String> {
    if assignment_target_size(dae_model, target) == Some(0) {
        return Vec::new();
    }
    if rumoca_core::parse_scalar_name(target).is_some() {
        return vec![target.to_string()];
    }
    if let Some(names) = crate::lower::scalarized_record_field_binding_names(target, layout) {
        return names
            .into_iter()
            .flat_map(|name| layout_scalar_names(layout, &name))
            .collect();
    }
    layout_scalar_names(layout, target)
}

fn assignment_target_size(dae_model: &dae::Dae, target: &str) -> Option<usize> {
    assignment_target_variable(dae_model, target).map(|var| var.size())
}

fn assignment_target_variable<'a>(
    dae_model: &'a dae::Dae,
    target: &str,
) -> Option<&'a dae::Variable> {
    let exact_key = rumoca_core::VarName::new(target);
    let base_key = rumoca_core::VarName::new(
        dae::component_base_name(target).unwrap_or_else(|| target.into()),
    );
    [exact_key, base_key].into_iter().find_map(|key| {
        dae_model
            .variables
            .states
            .get(&key)
            .or_else(|| dae_model.variables.algebraics.get(&key))
            .or_else(|| dae_model.variables.outputs.get(&key))
            .or_else(|| dae_model.variables.parameters.get(&key))
            .or_else(|| dae_model.variables.constants.get(&key))
            .or_else(|| dae_model.variables.inputs.get(&key))
            .or_else(|| dae_model.variables.discrete_reals.get(&key))
            .or_else(|| dae_model.variables.discrete_valued.get(&key))
    })
}

fn layout_scalar_names(layout: &solve::VarLayout, target: &str) -> Vec<String> {
    let Some(shape) = layout.shape(target) else {
        return vec![target.to_string()];
    };
    let dims = shape
        .iter()
        .map(|dim| i64::try_from(*dim).expect("array dimension must fit in i64"))
        .collect::<Vec<_>>();
    let count = shape.iter().product::<usize>().max(1);
    (0..count)
        .map(|idx| {
            let linear = format!("{target}[{}]", idx + 1);
            if layout.binding(linear.as_str()).is_some() {
                linear
            } else {
                dae::scalar_name_text_for_flat_index(target, &dims, idx)
            }
        })
        .collect()
}

fn initial_assignment_values(
    solution: &rumoca_core::Expression,
    env: &rumoca_eval_dae::VarEnv<f64>,
    target: &str,
    targets: &[String],
) -> Result<Vec<f64>, EvalError> {
    let size = targets.len().max(1);
    let values = selected_initial_function_values(solution, env, target, targets)
        .or_else(|| {
            initial_solution_may_be_array(solution).then(|| eval_array_values::<f64>(solution, env))
        })
        .filter(|values| !values.is_empty())
        .map(Ok)
        .unwrap_or_else(|| eval_expr::<f64>(solution, env).map(|value| vec![value]))?;
    Ok(expand_values_to_size(values, size.max(1)))
}

fn initial_solution_may_be_array(solution: &rumoca_core::Expression) -> bool {
    match solution {
        rumoca_core::Expression::Array { .. }
        | rumoca_core::Expression::Tuple { .. }
        | rumoca_core::Expression::If { .. }
        | rumoca_core::Expression::ArrayComprehension { .. }
        | rumoca_core::Expression::Range { .. } => true,
        rumoca_core::Expression::BuiltinCall { function, .. } => matches!(
            function,
            rumoca_core::BuiltinFunction::Cat
                | rumoca_core::BuiltinFunction::Fill
                | rumoca_core::BuiltinFunction::Zeros
                | rumoca_core::BuiltinFunction::Ones
                | rumoca_core::BuiltinFunction::Vector
        ),
        _ => false,
    }
}

fn selected_initial_function_values(
    solution: &rumoca_core::Expression,
    env: &rumoca_eval_dae::VarEnv<f64>,
    target: &str,
    targets: &[String],
) -> Option<Vec<f64>> {
    let rumoca_core::Expression::FunctionCall { name, args, .. } = solution else {
        return None;
    };
    let (resolved, outputs) = resolve_function_call_outputs_pub(name, env)?;
    let [output] = outputs.as_slice() else {
        return None;
    };
    let suffixes = target_selection_suffixes(target, targets)?;
    Some(
        suffixes
            .iter()
            .map(|suffix| eval_selected_function_output_pub(&resolved, output, suffix, args, env))
            .collect(),
    )
}

fn target_selection_suffixes(target: &str, targets: &[String]) -> Option<Vec<String>> {
    targets
        .iter()
        .map(|scalar_target| {
            scalar_target
                .strip_prefix(target)
                .map(str::to_string)
                .or_else(|| (scalar_target == target).then(String::new))
        })
        .collect()
}

struct InitialAssignmentApplyContext<'a> {
    dae_model: &'a dae::Dae,
    layout: &'a solve::VarLayout,
    params: &'a mut [f64],
    initial_y: &'a mut [f64],
    env: &'a mut rumoca_eval_dae::VarEnv<f64>,
}

fn apply_initial_assignment_values(
    ctx: InitialAssignmentApplyContext<'_>,
    assignment: &InitialAssignment<'_>,
    targets: &[String],
    values: &[f64],
) -> bool {
    let target_name = assignment.target.as_str();
    let dims = assignment_target_dims(ctx.dae_model, target_name);
    if values.len() > 1 && !dims.is_empty() && rumoca_core::parse_scalar_name(target_name).is_none()
    {
        set_array_entries(ctx.env, target_name, dims, values);
    }

    let mut changed = false;
    for (target, value) in targets.iter().zip(values.iter().copied()) {
        ctx.env.set(target.as_str(), value);
        if assignment.is_pre_target {
            set_pre_value_in_env(ctx.env, target.as_str(), value);
        }
        changed |= write_initial_slot(
            ctx.layout,
            &mut *ctx.params,
            &mut *ctx.initial_y,
            target.as_str(),
            value,
        );
        changed |= seed_current_slot_from_lowered_pre(
            ctx.layout,
            &mut *ctx.params,
            &mut *ctx.initial_y,
            &mut *ctx.env,
            target,
            value,
        );
        changed |= seed_lowered_pre_from_current_slot(
            ctx.layout,
            &mut *ctx.params,
            &mut *ctx.initial_y,
            &mut *ctx.env,
            target,
            value,
        );
    }
    if assignment.is_pre_target && values.len() > 1 {
        set_pre_value_in_env(ctx.env, target_name, values[0]);
    }
    changed
}

fn pin_initial_assignment_targets(targets: &[String], pinned: &mut HashSet<String>) -> bool {
    let mut changed = false;
    for target in targets {
        changed |= pinned.insert(target.clone());
    }
    changed
}

struct InitialAliasPropagation<'a, 'b> {
    dae_model: &'a dae::Dae,
    layout: &'a solve::VarLayout,
    params: &'a mut [f64],
    initial_y: &'a mut [f64],
    env: &'a mut rumoca_eval_dae::VarEnv<f64>,
    pinned: &'b mut HashSet<String>,
}

impl InitialAliasPropagation<'_, '_> {
    fn propagate(&mut self, aliases: &[InitialRuntimeAlias]) -> bool {
        let mut changed = false;
        for alias in aliases {
            changed |= self.propagate_alias(alias);
        }
        changed
    }

    fn propagate_alias(&mut self, alias: &InitialRuntimeAlias) -> bool {
        let mut changed = false;
        for (lhs, rhs) in self.alias_scalar_pairs(alias) {
            changed |= self.propagate_scalar_alias(lhs.as_str(), rhs.as_str());
        }
        changed
    }

    fn alias_scalar_pairs(&self, alias: &InitialRuntimeAlias) -> Vec<(String, String)> {
        let lhs = alias_target_scalar_names(self.dae_model, &alias.lhs);
        let rhs = alias_target_scalar_names(self.dae_model, &alias.rhs);
        if lhs.len() == rhs.len() {
            return lhs.into_iter().zip(rhs).collect();
        }
        vec![(
            alias.lhs.as_str().to_string(),
            alias.rhs.as_str().to_string(),
        )]
    }

    fn propagate_scalar_alias(&mut self, lhs: &str, rhs: &str) -> bool {
        match (self.pinned.contains(lhs), self.pinned.contains(rhs)) {
            (true, false) => self.propagate_value(lhs, rhs),
            (false, true) => self.propagate_value(rhs, lhs),
            _ => false,
        }
    }

    fn propagate_value(&mut self, source: &str, target: &str) -> bool {
        let value = self.env.get(source);
        let changed = self.write_value(target, value);
        self.pinned.insert(target.to_string()) || changed
    }

    fn write_value(&mut self, target: &str, value: f64) -> bool {
        self.env.set(target, value);
        if let Some(pre_target) = target.strip_prefix("__pre__.") {
            set_pre_value_in_env(self.env, pre_target, value);
        }
        let dims = assignment_target_dims(self.dae_model, target);
        if !dims.is_empty() && rumoca_core::parse_scalar_name(target).is_none() {
            set_array_entries(self.env, target, dims, &[value]);
        }
        write_initial_slot(self.layout, self.params, self.initial_y, target, value)
            | seed_current_slot_from_lowered_pre(
                self.layout,
                self.params,
                self.initial_y,
                self.env,
                target,
                value,
            )
            | seed_lowered_pre_from_current_slot(
                self.layout,
                self.params,
                self.initial_y,
                self.env,
                target,
                value,
            )
    }
}

fn alias_target_scalar_names(dae_model: &dae::Dae, name: &rumoca_core::VarName) -> Vec<String> {
    variable_by_name(dae_model, name)
        .map(|var| scalar_names(name.as_str(), var))
        .unwrap_or_else(|| vec![name.as_str().to_string()])
}

fn variable_by_name<'a>(
    dae_model: &'a dae::Dae,
    name: &rumoca_core::VarName,
) -> Option<&'a dae::Variable> {
    dae_model
        .variables
        .states
        .get(name)
        .or_else(|| dae_model.variables.algebraics.get(name))
        .or_else(|| dae_model.variables.outputs.get(name))
        .or_else(|| dae_model.variables.parameters.get(name))
        .or_else(|| dae_model.variables.inputs.get(name))
        .or_else(|| dae_model.variables.discrete_reals.get(name))
        .or_else(|| dae_model.variables.discrete_valued.get(name))
}

fn seed_current_slot_from_lowered_pre(
    layout: &solve::VarLayout,
    params: &mut [f64],
    initial_y: &mut [f64],
    env: &mut rumoca_eval_dae::VarEnv<f64>,
    target: &str,
    value: f64,
) -> bool {
    let Some(current_target) = target.strip_prefix("__pre__.") else {
        return false;
    };
    env.set(current_target, value);
    set_pre_value_in_env(env, current_target, value);
    write_initial_slot(layout, params, initial_y, current_target, value)
}

fn seed_lowered_pre_from_current_slot(
    layout: &solve::VarLayout,
    params: &mut [f64],
    initial_y: &mut [f64],
    env: &mut rumoca_eval_dae::VarEnv<f64>,
    target: &str,
    value: f64,
) -> bool {
    if target.starts_with("__pre__.") {
        return false;
    }
    let pre_target = format!("__pre__.{target}");
    if layout.binding(&pre_target).is_none() {
        return false;
    }
    env.set(&pre_target, value);
    set_pre_value_in_env(env, target, value);
    write_initial_slot(layout, params, initial_y, &pre_target, value)
}

fn assignment_target_dims<'a>(dae_model: &'a dae::Dae, target: &str) -> &'a [i64] {
    assignment_target_variable(dae_model, target)
        .map(|var| var.dims.as_slice())
        .unwrap_or(&[])
}

fn write_initial_slot(
    layout: &solve::VarLayout,
    params: &mut [f64],
    initial_y: &mut [f64],
    target: &str,
    value: f64,
) -> bool {
    match layout.binding(target) {
        Some(solve::ScalarSlot::Y { index, .. }) if index < initial_y.len() => {
            replace_if_changed(&mut initial_y[index], value)
        }
        Some(solve::ScalarSlot::P { index, .. }) if index < params.len() => {
            replace_if_changed(&mut params[index], value)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn field_access(base: rumoca_core::Expression, field: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::FieldAccess {
            base: Box::new(base),
            field: field.to_string(),
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn assignment_target_scalar_names_skips_zero_size_dae_variable() {
        let mut dae_model = dae::Dae::default();
        let mut xi = dae::Variable::new(rumoca_core::VarName::new("volume.Xi"));
        xi.dims = vec![0];
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new("volume.Xi"), xi);

        assert!(
            assignment_target_scalar_names(&dae_model, &solve::VarLayout::default(), "volume.Xi")
                .is_empty()
        );

        let mut indexed_xi = dae::Variable::new(rumoca_core::VarName::new("plant.unit[1].Xi"));
        indexed_xi.dims = vec![0];
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new("plant.unit[1].Xi"), indexed_xi);

        assert!(
            assignment_target_scalar_names(
                &dae_model,
                &solve::VarLayout::default(),
                "plant.unit[1].Xi"
            )
            .is_empty()
        );
    }

    #[test]
    fn continuous_seed_assignment_skips_missing_binding() {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("target"),
            dae::Variable::new(rumoca_core::VarName::new("target")),
        );
        dae_model.continuous.equations.push(dae::Equation::residual(
            sub(var("target"), var("missing")),
            rumoca_core::Span::DUMMY,
            "missing continuous seed dependency",
        ));

        let layout = solve::VarLayout::from_parts(
            indexmap::IndexMap::from([(
                "target".to_string(),
                solve::ScalarSlot::Y {
                    index: 0,
                    byte_offset: 0,
                },
            )]),
            1,
            0,
        );
        let mut params = Vec::new();
        let mut initial_y = vec![5.0];
        let mut env = rumoca_eval_dae::VarEnv::new();
        let mut target_resolver = AssignmentTargetResolver::new(&dae_model, &layout);

        let changed = seed_continuous_assignments(
            &dae_model,
            &layout,
            &mut params,
            &mut initial_y,
            &mut env,
            &mut target_resolver,
            &mut HashSet::new(),
            &HashSet::new(),
        )
        .expect("missing binding should skip continuous seed assignment");

        assert!(!changed);
        assert_eq!(initial_y, vec![5.0]);
    }

    #[test]
    fn continuous_seed_assignment_skips_unsupported_expression() {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("target"),
            dae::Variable::new(rumoca_core::VarName::new("target")),
        );
        dae_model.continuous.equations.push(dae::Equation::residual(
            sub(var("target"), field_access(var("recordValue"), "field")),
            rumoca_core::Span::DUMMY,
            "unsupported continuous seed dependency",
        ));

        let layout = solve::VarLayout::from_parts(
            indexmap::IndexMap::from([(
                "target".to_string(),
                solve::ScalarSlot::Y {
                    index: 0,
                    byte_offset: 0,
                },
            )]),
            1,
            0,
        );
        let mut params = Vec::new();
        let mut initial_y = vec![5.0];
        let mut env = rumoca_eval_dae::VarEnv::new();
        let mut target_resolver = AssignmentTargetResolver::new(&dae_model, &layout);

        let changed = seed_continuous_assignments(
            &dae_model,
            &layout,
            &mut params,
            &mut initial_y,
            &mut env,
            &mut target_resolver,
            &mut HashSet::new(),
            &HashSet::new(),
        )
        .expect("unsupported expression should skip continuous seed assignment");

        assert!(!changed);
        assert_eq!(initial_y, vec![5.0]);
    }
}
