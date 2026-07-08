use std::collections::HashSet;
use std::sync::Arc;

use rumoca_core::OpBinary;
use rumoca_eval_dae::eval::{EvalError, EvalRuntimeState};
use rumoca_eval_dae::{
    InitialAssignment, build_runtime_parameter_tail_env_with_declared_slots_and_runtime,
    can_broadcast_start_value, eval_array_values, eval_expr, eval_selected_function_output_pub,
    initial_assignment_from_equation, map_var_to_env, resolve_function_call_outputs_pub,
    seed_pre_values_in_env_runtime, set_array_entries, set_pre_value_in_env,
};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::lower::LowerError;
use crate::solve_model::{
    SolveModelLowerError, replace_if_changed, runtime_tail_error, scalar_names,
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
    let mut pinned = fixed_start_pins(dae_model)?;
    let continuous_seed_pins = explicit_start_pins(dae_model)?;
    let init_eq_count = dae_model.initialization.equations.len();
    let max_passes = init_eq_count.max(aliases.len()).clamp(1, 32);
    for _ in 0..max_passes {
        let mut changed = false;
        for eq in &dae_model.initialization.equations {
            changed |= seed_tuple_function_initial_assignment(
                dae_model,
                layout,
                params,
                initial_y,
                &mut env,
                &mut pinned,
                eq,
            )
            .map_err(|source| SolveModelLowerError::Evaluation {
                context: "initial tuple function assignment".to_string(),
                source,
                span: Some(eq.span),
            })?;
            let Some(assignment) = initial_assignment_from_equation(eq) else {
                continue;
            };
            if solution_is_runtime_alias(layout, assignment.solution) {
                continue;
            }
            let targets =
                assignment_target_scalar_names(layout, assignment.target.as_str(), eq.span)?;
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
        .propagate(&aliases)?;
        if !changed {
            break;
        }
    }
    Ok(())
}

fn seed_tuple_function_initial_assignment(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    params: &mut [f64],
    initial_y: &mut [f64],
    env: &mut rumoca_eval_dae::VarEnv<f64>,
    pinned: &mut HashSet<String>,
    eq: &dae::Equation,
) -> Result<bool, EvalError> {
    let Some((targets_exprs, function_name, args)) = tuple_function_assignment(&eq.rhs) else {
        return Ok(false);
    };
    let Some((resolved, outputs)) = resolve_function_call_outputs_pub(function_name, env) else {
        return Ok(false);
    };
    let mut changed = false;
    for (idx, target_expr) in targets_exprs.iter().enumerate() {
        let Some(output_name) = outputs.get(idx) else {
            break;
        };
        let Some(target) = tuple_assignment_target_name(target_expr) else {
            continue;
        };
        let targets =
            assignment_target_scalar_names(layout, target.as_str(), eq.span).map_err(|err| {
                EvalError::InvalidShape {
                    context: "tuple initial assignment target",
                    reason: err.to_string(),
                }
            })?;
        if targets.is_empty() {
            continue;
        }
        let Some(indices) = target_selection_indices(target.as_str(), &targets)? else {
            continue;
        };
        let mut values = Vec::new();
        reserve_initial_eval_vec_capacity(
            &mut values,
            indices.len(),
            "tuple initial function values",
        )?;
        for indices in &indices {
            values.push(eval_selected_function_output_pub(
                &resolved,
                output_name,
                indices,
                args,
                env,
            )?);
        }
        let values = initial_assignment_values_with_expected_size(
            &rumoca_core::Expression::FunctionCall {
                name: function_name.clone(),
                args: args.to_vec(),
                is_constructor: false,
                span: eq.rhs.span().unwrap_or(eq.span),
            },
            env,
            values,
            targets.len().max(1),
        )?;
        let assignment = InitialAssignment {
            target,
            solution: &eq.rhs,
            is_pre_target: false,
        };
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

fn tuple_function_assignment(
    expr: &rumoca_core::Expression,
) -> Option<(
    &[rumoca_core::Expression],
    &rumoca_core::Reference,
    &[rumoca_core::Expression],
)> {
    let rumoca_core::Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expr
    else {
        return None;
    };
    let rumoca_core::Expression::Tuple { elements, .. } = lhs.as_ref() else {
        return None;
    };
    let rumoca_core::Expression::FunctionCall { name, args, .. } = rhs.as_ref() else {
        return None;
    };
    Some((elements, name, args))
}

fn tuple_assignment_target_name(expr: &rumoca_core::Expression) -> Option<String> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    Some(name.as_str().to_string())
}

fn seed_continuous_assignments(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    params: &mut [f64],
    initial_y: &mut [f64],
    env: &mut rumoca_eval_dae::VarEnv<f64>,
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
        let targets = assignment_target_scalar_names(layout, assignment.target.as_str(), eq.span)?;
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
            Err(err) if initial_seed_error_is_non_evaluable(&err) => continue,
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

fn initial_seed_error_is_non_evaluable(error: &EvalError) -> bool {
    match error {
        EvalError::MissingBinding { .. }
        | EvalError::MissingFunction { .. }
        | EvalError::UnsupportedExpression { .. } => true,
        EvalError::Spanned { source, .. } => initial_seed_error_is_non_evaluable(source),
        EvalError::ShapeMismatch { .. }
        | EvalError::InvalidShape { .. }
        | EvalError::StatementIterationLimit { .. }
        | EvalError::ShortRuntimeVector { .. } => false,
    }
}

fn fixed_start_pins(dae_model: &dae::Dae) -> Result<HashSet<String>, SolveModelLowerError> {
    let mut pins = HashSet::new();
    for (name, var) in fixed_start_vars(dae_model) {
        pins.extend(scalar_names(name.as_str(), var)?);
    }
    pins.insert("time".to_string());
    Ok(pins)
}

fn explicit_start_pins(dae_model: &dae::Dae) -> Result<HashSet<String>, SolveModelLowerError> {
    let mut pins = HashSet::new();
    for (name, var) in explicit_start_vars(dae_model) {
        pins.extend(scalar_names(name.as_str(), var)?);
    }
    Ok(pins)
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
    span: rumoca_core::Span,
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
            lhs: lhs.var_name().clone(),
            rhs,
            span: eq.span,
        });
    }
    residual_alias_equality(&eq.rhs)
}

fn residual_alias_equality(expr: &rumoca_core::Expression) -> Option<InitialRuntimeAlias> {
    let rumoca_core::Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        span,
    } = expr
    else {
        return None;
    };
    Some(InitialRuntimeAlias {
        lhs: scalar_alias_name(lhs)?,
        rhs: scalar_alias_name(rhs)?,
        span: *span,
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
    let mut env = build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
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

fn assignment_target_scalar_names(
    layout: &solve::VarLayout,
    target: &str,
    span: rumoca_core::Span,
) -> Result<Vec<String>, SolveModelLowerError> {
    if rumoca_core::parse_scalar_name(target).is_some() {
        return Ok(vec![target.to_string()]);
    }
    if let Some(names) = crate::lower::scalarized_record_field_binding_names(target, layout) {
        let mut scalars = Vec::new();
        reserve_initial_lower_vec_capacity(
            &mut scalars,
            names.len(),
            "initial assignment scalar target names",
            span,
        )?;
        for name in names {
            let mut field_scalars = layout_scalar_names(layout, &name, span)?;
            reserve_initial_lower_vec_capacity(
                &mut scalars,
                field_scalars.len(),
                "initial assignment scalar target names",
                span,
            )?;
            scalars.append(&mut field_scalars);
        }
        return Ok(scalars);
    }
    layout_scalar_names(layout, target, span)
}

fn layout_scalar_names(
    layout: &solve::VarLayout,
    target: &str,
    span: rumoca_core::Span,
) -> Result<Vec<String>, SolveModelLowerError> {
    let Some(shape) = layout.shape(target) else {
        return Ok(vec![target.to_string()]);
    };
    let mut dims = Vec::new();
    reserve_initial_lower_vec_capacity(
        &mut dims,
        shape.len(),
        "initial assignment target dimensions",
        span,
    )?;
    for dim in shape {
        dims.push(i64::try_from(*dim).map_err(|_| {
            SolveModelLowerError::Lower(LowerError::ContractViolation {
                reason: format!(
                    "initial assignment target `{target}` has dimension {dim} exceeding i64"
                ),
                span,
            })
        })?);
    }
    let count = checked_initial_target_element_count(target, shape, span)?;
    let mut names = Vec::new();
    reserve_initial_lower_vec_capacity(
        &mut names,
        count,
        "initial assignment scalar target names",
        span,
    )?;
    for idx in 0..count {
        names.push(dae::scalar_name_text_for_flat_index(target, &dims, idx));
    }
    Ok(names)
}

fn initial_assignment_values(
    solution: &rumoca_core::Expression,
    env: &rumoca_eval_dae::VarEnv<f64>,
    target: &str,
    targets: &[String],
) -> Result<Vec<f64>, EvalError> {
    let size = targets.len().max(1);
    let values = if size <= 1 {
        vec![eval_expr::<f64>(solution, env)?]
    } else {
        match selected_initial_function_values(solution, env, target, targets)? {
            Some(values) => values,
            None => eval_array_values::<f64>(solution, env)?,
        }
    };
    initial_assignment_values_with_expected_size(solution, env, values, size)
}

fn initial_assignment_values_with_expected_size(
    solution: &rumoca_core::Expression,
    env: &rumoca_eval_dae::VarEnv<f64>,
    values: Vec<f64>,
    expected: usize,
) -> Result<Vec<f64>, EvalError> {
    if values.len() == expected {
        return Ok(values);
    }
    if expected > 1 && values.len() == 1 && can_broadcast_start_value(solution, env) {
        let mut broadcast = Vec::new();
        reserve_initial_eval_vec_capacity(
            &mut broadcast,
            expected,
            "initial assignment value broadcast",
        )?;
        broadcast.resize(expected, values[0]);
        return Ok(broadcast);
    }
    Err(EvalError::ShapeMismatch {
        context: "initial assignment value",
        expected,
        actual: values.len(),
    })
}

fn selected_initial_function_values(
    solution: &rumoca_core::Expression,
    env: &rumoca_eval_dae::VarEnv<f64>,
    target: &str,
    targets: &[String],
) -> Result<Option<Vec<f64>>, EvalError> {
    let rumoca_core::Expression::FunctionCall { name, args, .. } = solution else {
        return Ok(None);
    };
    let Some((resolved, outputs)) = resolve_function_call_outputs_pub(name, env) else {
        return Ok(None);
    };
    let [output] = outputs.as_slice() else {
        return Ok(None);
    };
    let Some(indices) = target_selection_indices(target, targets)? else {
        return Ok(None);
    };
    let mut values = Vec::new();
    reserve_initial_eval_vec_capacity(
        &mut values,
        indices.len(),
        "selected initial function values",
    )?;
    for indices in &indices {
        values.push(eval_selected_function_output_pub(
            &resolved, output, indices, args, env,
        )?);
    }
    Ok(Some(values))
}

fn target_selection_indices(
    target: &str,
    targets: &[String],
) -> Result<Option<Vec<Vec<i64>>>, EvalError> {
    let mut selections = Vec::new();
    reserve_initial_eval_vec_capacity(
        &mut selections,
        targets.len(),
        "initial function target selections",
    )?;
    for (idx, scalar_target) in targets.iter().enumerate() {
        if scalar_target == target {
            selections.push(Vec::new());
            continue;
        }
        let Some(suffix) = scalar_target.strip_prefix(target) else {
            return Ok(None);
        };
        if !suffix.starts_with('[') {
            return Ok(None);
        }
        let index = i64::try_from(idx + 1).map_err(|_| EvalError::InvalidShape {
            context: "initial function target selection",
            reason: format!("target selection index {} exceeds i64", idx + 1),
        })?;
        let mut selected = Vec::new();
        reserve_initial_eval_vec_capacity(
            &mut selected,
            1,
            "initial function target selection index",
        )?;
        selected.push(index);
        selections.push(selected);
    }
    Ok(Some(selections))
}

fn checked_initial_target_element_count(
    target: &str,
    shape: &[usize],
    span: rumoca_core::Span,
) -> Result<usize, SolveModelLowerError> {
    let mut count = 1usize;
    for dim in shape {
        if *dim == 0 {
            return Ok(0);
        }
        count = count.checked_mul(*dim).ok_or_else(|| {
            SolveModelLowerError::Lower(LowerError::ContractViolation {
                reason: format!("initial assignment target `{target}` element count overflows"),
                span,
            })
        })?;
    }
    Ok(count.max(1))
}

fn reserve_initial_lower_vec_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError> {
    values.try_reserve_exact(capacity).map_err(|_| {
        SolveModelLowerError::Lower(LowerError::ContractViolation {
            reason: format!("{context} capacity overflows"),
            span,
        })
    })
}

fn reserve_initial_eval_vec_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &'static str,
) -> Result<(), EvalError> {
    values
        .try_reserve_exact(capacity)
        .map_err(|_| EvalError::InvalidShape {
            context,
            reason: format!("{context} capacity overflows"),
        })
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
    if !dims.is_empty() && rumoca_core::parse_scalar_name(target_name).is_none() {
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
    fn propagate(&mut self, aliases: &[InitialRuntimeAlias]) -> Result<bool, SolveModelLowerError> {
        let mut changed = false;
        for alias in aliases {
            changed |= self.propagate_alias(alias)?;
        }
        Ok(changed)
    }

    fn propagate_alias(
        &mut self,
        alias: &InitialRuntimeAlias,
    ) -> Result<bool, SolveModelLowerError> {
        let mut changed = false;
        for (lhs, rhs) in self.alias_scalar_pairs(alias)? {
            changed |= self.propagate_scalar_alias(lhs.as_str(), rhs.as_str())?;
        }
        Ok(changed)
    }

    fn alias_scalar_pairs(
        &self,
        alias: &InitialRuntimeAlias,
    ) -> Result<Vec<(String, String)>, SolveModelLowerError> {
        let lhs = alias_target_scalar_names(self.dae_model, self.layout, &alias.lhs, alias.span)?;
        let rhs = alias_target_scalar_names(self.dae_model, self.layout, &alias.rhs, alias.span)?;
        if lhs.len() == rhs.len() {
            return Ok(lhs.into_iter().zip(rhs).collect());
        }
        Err(SolveModelLowerError::Lower(LowerError::ContractViolation {
            reason: format!(
                "initial runtime alias `{}` = `{}` has mismatched scalar widths {} and {}",
                alias.lhs.as_str(),
                alias.rhs.as_str(),
                lhs.len(),
                rhs.len()
            ),
            span: alias.span,
        }))
    }

    fn propagate_scalar_alias(
        &mut self,
        lhs: &str,
        rhs: &str,
    ) -> Result<bool, SolveModelLowerError> {
        match (self.pinned.contains(lhs), self.pinned.contains(rhs)) {
            (true, false) => self.propagate_value(lhs, rhs),
            (false, true) => self.propagate_value(rhs, lhs),
            _ => Ok(false),
        }
    }

    fn propagate_value(
        &mut self,
        source: &str,
        target: &str,
    ) -> Result<bool, SolveModelLowerError> {
        let value =
            self.env
                .require(source)
                .map_err(|source_error| SolveModelLowerError::Evaluation {
                    context: format!("initial runtime alias `{source}` -> `{target}`"),
                    span: source_error.source_span(),
                    source: source_error,
                })?;
        let changed = self.write_value(target, value);
        Ok(self.pinned.insert(target.to_string()) || changed)
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

fn alias_target_scalar_names(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    name: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<Vec<String>, SolveModelLowerError> {
    if name.as_str() == "time" && matches!(layout.binding("time"), Some(solve::ScalarSlot::Time)) {
        return Ok(vec!["time".to_string()]);
    }
    if let Some(var) = variable_by_name(dae_model, name) {
        return scalar_names(name.as_str(), var);
    }
    if let Some(names) = scalarized_alias_target_names(dae_model, layout, name, span)? {
        return Ok(names);
    }
    Err(SolveModelLowerError::Lower(LowerError::ContractViolation {
        reason: format!(
            "initial runtime alias target `{}` must be a known DAE variable",
            name.as_str()
        ),
        span,
    }))
}

fn scalarized_alias_target_names(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    name: &rumoca_core::VarName,
    span: rumoca_core::Span,
) -> Result<Option<Vec<String>>, SolveModelLowerError> {
    let Some(scalar) = rumoca_core::parse_scalar_name(name.as_str()) else {
        return Ok(None);
    };
    let base = rumoca_core::VarName::new(scalar.base);
    let Some(var) = variable_by_name(dae_model, &base) else {
        return Ok(None);
    };
    validate_alias_scalar_indices(name, &scalar.indices, var, span)?;
    if layout.binding(name.as_str()).is_none() {
        return Err(SolveModelLowerError::Lower(LowerError::ContractViolation {
            reason: format!(
                "initial runtime alias target `{}` has no solve-layout scalar slot",
                name.as_str()
            ),
            span,
        }));
    }
    Ok(Some(vec![name.as_str().to_string()]))
}

fn validate_alias_scalar_indices(
    name: &rumoca_core::VarName,
    indices: &[i64],
    var: &dae::Variable,
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError> {
    if indices.len() != var.dims.len() {
        return Err(SolveModelLowerError::Lower(LowerError::ContractViolation {
            reason: format!(
                "initial runtime alias target `{}` has scalar index rank {}, but base variable rank is {}",
                name.as_str(),
                indices.len(),
                var.dims.len()
            ),
            span,
        }));
    }
    for (axis, (index, dim)) in indices.iter().zip(&var.dims).enumerate() {
        if *dim <= 0 || *index <= 0 || *index > *dim {
            return Err(SolveModelLowerError::Lower(LowerError::ContractViolation {
                reason: format!(
                    "initial runtime alias target `{}` index {} is out of bounds for dimension {}",
                    name.as_str(),
                    index,
                    axis + 1
                ),
                span,
            }));
        }
    }
    Ok(())
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
        .or_else(|| dae_model.variables.constants.get(name))
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
    let key = rumoca_core::VarName::new(
        dae::component_base_name(target).unwrap_or_else(|| target.into()),
    );
    dae_model
        .variables
        .states
        .get(&key)
        .or_else(|| dae_model.variables.algebraics.get(&key))
        .or_else(|| dae_model.variables.outputs.get(&key))
        .or_else(|| dae_model.variables.parameters.get(&key))
        .or_else(|| dae_model.variables.inputs.get(&key))
        .or_else(|| dae_model.variables.discrete_reals.get(&key))
        .or_else(|| dae_model.variables.discrete_valued.get(&key))
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
    use indexmap::IndexMap;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_initial_values_fixture.mo"),
            1,
            2,
        )
    }

    fn real(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: test_span(),
        }
    }

    fn var(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    fn comp_ref(name: &str) -> rumoca_core::ComponentReference {
        rumoca_core::ComponentReference::from_flat_segments(name, test_span(), None)
    }

    fn time_layout() -> solve::VarLayout {
        solve::VarLayout::from_parts(
            IndexMap::from([("time".to_string(), solve::ScalarSlot::Time)]),
            0,
            0,
        )
    }

    fn indexed_input_model_and_layout() -> (dae::Dae, solve::VarLayout) {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.inputs.insert(
            rumoca_core::VarName::new("sum.u"),
            dae::Variable {
                name: rumoca_core::VarName::new("sum.u"),
                dims: vec![2],
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );
        let layout = solve::VarLayout::from_parts(
            IndexMap::from([
                ("sum.u".to_string(), solve::scalar_slot_p(0)),
                ("sum.u[1]".to_string(), solve::scalar_slot_p(0)),
                ("sum.u[2]".to_string(), solve::scalar_slot_p(1)),
            ]),
            0,
            2,
        );
        (dae_model, layout)
    }

    #[test]
    fn initial_assignment_values_broadcasts_literal_to_array_targets() {
        let env = rumoca_eval_dae::VarEnv::new();
        let values =
            initial_assignment_values(&real(3.0), &env, "x", &["x[1]".into(), "x[2]".into()])
                .expect("literal initial assignment may broadcast");

        assert_eq!(values, vec![3.0, 3.0]);
    }

    #[test]
    fn initial_assignment_values_rejects_scalar_var_broadcast_to_array_targets() {
        let mut env = rumoca_eval_dae::VarEnv::new();
        env.set("s", 2.0);

        let err = initial_assignment_values(&var("s"), &env, "x", &["x[1]".into(), "x[2]".into()])
            .expect_err("scalar variable initial assignment must not broadcast to an array");

        assert_eq!(
            err,
            EvalError::ShapeMismatch {
                context: "initial assignment value",
                expected: 2,
                actual: 1,
            }
        );
    }

    #[test]
    fn initial_tuple_function_assignment_seeds_selected_parameter_output() {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new("a"),
            dae::Variable {
                fixed: Some(false),
                ..dae::Variable::empty_with_span(test_span())
            },
        );
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new("b"),
            dae::Variable {
                fixed: Some(false),
                ..dae::Variable::empty_with_span(test_span())
            },
        );

        let mut function = rumoca_core::Function::new("Pkg.multi", test_span());
        function.add_output(rumoca_core::FunctionParam::new(
            "first",
            "Real",
            test_span(),
        ));
        function.add_output(rumoca_core::FunctionParam::new(
            "second",
            "Real",
            test_span(),
        ));
        function.body = vec![
            rumoca_core::Statement::Assignment {
                comp: comp_ref("first"),
                value: real(1.25),
                span: test_span(),
            },
            rumoca_core::Statement::Assignment {
                comp: comp_ref("second"),
                value: real(2.5),
                span: test_span(),
            },
        ];
        dae_model
            .symbols
            .functions
            .insert(rumoca_core::VarName::new("Pkg.multi"), function);
        dae_model
            .initialization
            .equations
            .push(dae::Equation::residual(
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Sub,
                    lhs: Box::new(rumoca_core::Expression::Tuple {
                        elements: vec![var("a"), var("b")],
                        span: test_span(),
                    }),
                    rhs: Box::new(rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::Reference::new("Pkg.multi"),
                        args: Vec::new(),
                        is_constructor: false,
                        span: test_span(),
                    }),
                    span: test_span(),
                },
                test_span(),
                "(a, b) = Pkg.multi()",
            ));
        let layout = solve::VarLayout::from_parts(
            IndexMap::from([
                ("a".to_string(), solve::scalar_slot_p(0)),
                ("b".to_string(), solve::scalar_slot_p(1)),
            ]),
            0,
            2,
        );
        let mut params = vec![0.0, 0.0];
        let mut initial_y = Vec::new();

        apply_initial_equations_to_start_values(
            &dae_model,
            &layout,
            &mut params,
            &mut initial_y,
            std::sync::Arc::new(EvalRuntimeState::default()),
        )
        .expect("tuple function initial assignment should seed params");

        assert_eq!(params, vec![1.25, 2.5]);
    }

    #[test]
    fn fixed_start_pins_include_builtin_time() {
        assert!(
            fixed_start_pins(&dae::Dae::default())
                .expect("empty DAE fixed start pins should build")
                .contains("time")
        );
    }

    #[test]
    fn alias_target_scalar_names_accepts_builtin_time_slot() {
        let names = alias_target_scalar_names(
            &dae::Dae::default(),
            &time_layout(),
            &rumoca_core::VarName::new("time"),
            test_span(),
        )
        .expect("time is a built-in solve-layout scalar");

        assert_eq!(names, vec!["time"]);
    }

    #[test]
    fn alias_target_scalar_names_rejects_unknown_non_time_target() {
        let err = alias_target_scalar_names(
            &dae::Dae::default(),
            &time_layout(),
            &rumoca_core::VarName::new("missing"),
            test_span(),
        )
        .expect_err("unknown alias target must still fail fast");

        assert!(
            format!("{err:?}").contains("initial runtime alias target `missing`"),
            "{err:?}"
        );
    }

    #[test]
    fn assignment_target_scalar_names_rejects_oversized_layout_dimension_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_initial_values_source_14.mo"),
            7,
            19,
        );
        let layout = solve::VarLayout::from_parts_with_shapes(
            IndexMap::new(),
            IndexMap::from([("x".to_string(), vec![0, usize::MAX])]),
            0,
            0,
        )
        .expect("zero-size shape fixture may omit scalar slots");

        let err = assignment_target_scalar_names(&layout, "x", span)
            .expect_err("oversized dimensions must not be converted through expect");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            format!("{err}").contains("dimension") && format!("{err}").contains("exceeding i64"),
            "{err}"
        );
    }

    #[test]
    fn assignment_target_scalar_names_skips_zero_size_layout_target() {
        let layout = solve::VarLayout::from_parts_with_shapes(
            IndexMap::new(),
            IndexMap::from([("x".to_string(), vec![0])]),
            0,
            0,
        )
        .expect("zero-size layout target may omit scalar slots");

        let names = assignment_target_scalar_names(&layout, "x", test_span())
            .expect("zero-size target should resolve without synthetic scalar names");

        assert!(names.is_empty());
        assert_eq!(
            checked_initial_target_element_count("x", &[0], test_span())
                .expect("zero dimension is a valid empty target"),
            0
        );
    }

    #[test]
    fn assignment_target_scalar_names_rejects_shape_product_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_initial_values_source_15.mo"),
            3,
            11,
        );
        let err = checked_initial_target_element_count("x", &[i64::MAX as usize, 3], span)
            .expect_err("overflowing target element count should fail fast");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            format!("{err}").contains("element count") && format!("{err}").contains("overflows"),
            "{err}"
        );
    }

    #[test]
    fn alias_target_scalar_names_accepts_known_indexed_array_element() {
        let (dae_model, layout) = indexed_input_model_and_layout();

        let names = alias_target_scalar_names(
            &dae_model,
            &layout,
            &rumoca_core::VarName::new("sum.u[1]"),
            test_span(),
        )
        .expect("indexed target is backed by DAE shape metadata and a layout slot");

        assert_eq!(names, vec!["sum.u[1]"]);
    }

    #[test]
    fn alias_target_scalar_names_rejects_out_of_bounds_indexed_array_element() {
        let (dae_model, layout) = indexed_input_model_and_layout();

        let err = alias_target_scalar_names(
            &dae_model,
            &layout,
            &rumoca_core::VarName::new("sum.u[3]"),
            test_span(),
        )
        .expect_err("indexed target must be in bounds");

        assert!(format!("{err:?}").contains("out of bounds"), "{err:?}");
    }

    #[test]
    fn alias_target_scalar_names_rejects_indexed_array_element_without_layout_slot() {
        let (dae_model, _) = indexed_input_model_and_layout();
        let layout = solve::VarLayout::from_parts(IndexMap::new(), 0, 0);

        let err = alias_target_scalar_names(
            &dae_model,
            &layout,
            &rumoca_core::VarName::new("sum.u[1]"),
            test_span(),
        )
        .expect_err("indexed target must have a concrete solve layout slot");

        assert!(
            format!("{err:?}").contains("no solve-layout scalar slot"),
            "{err:?}"
        );
    }
}
