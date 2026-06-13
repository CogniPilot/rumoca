//! Lower a DAE model into the solver-facing Solve IR package.
//!
//! This is the last DAE-aware step before runtime backends. Backends should
//! receive only `rumoca-ir-solve` data.

use indexmap::{IndexMap, IndexSet};
use rumoca_core::{ExpressionVisitor, Literal, OpBinary, OpUnary};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use std::sync::Arc;

use crate::LowerError;
use crate::initial_values::apply_initial_equations_to_start_values;
#[cfg(test)]
use rumoca_eval_dae::build_runtime_parameter_tail_env_with_runtime;
use rumoca_eval_dae::eval::{
    EvalError, EvalRuntimeState, eval_matrix_values, eval_shaped_array_values,
    external_table_data_for_parameter_values_in,
};
use rumoca_eval_dae::{
    can_broadcast_start_value, eval_array_values, eval_expr, start_expr_is_nonnumeric,
    try_build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime,
    try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime,
};

#[derive(Clone, Debug, PartialEq)]
pub struct VisibleExpression {
    pub name: String,
    pub expr: rumoca_core::Expression,
}

impl VisibleExpression {
    pub fn var_ref(name: String) -> Self {
        Self {
            expr: rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(name.clone()),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            },
            name,
        }
    }
}

#[derive(Debug)]
pub enum SolveModelLowerError {
    Lower(LowerError),
    Structural {
        source: rumoca_phase_structural::StructuralError,
    },
    Evaluation {
        context: String,
        source: EvalError,
        span: Option<rumoca_core::Span>,
    },
    MassMatrix {
        row: usize,
        state_name: String,
        reason: String,
        span: Option<rumoca_core::Span>,
    },
}

impl std::fmt::Display for SolveModelLowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lower(err) => write!(f, "{err}"),
            Self::Structural { source, .. } => write!(f, "structural lowering failed: {source}"),
            Self::Evaluation {
                context, source, ..
            } => {
                write!(f, "failed to evaluate {context}: {source}")
            }
            Self::MassMatrix {
                row,
                state_name,
                reason,
                ..
            } => write!(
                f,
                "mass matrix row {row} for state `{state_name}` could not be derived: {reason}"
            ),
        }
    }
}

impl std::error::Error for SolveModelLowerError {}

impl SolveModelLowerError {
    pub fn diagnostic_reason(&self) -> String {
        match self {
            Self::Lower(err) => err.reason(),
            Self::Structural { source, .. } => format!("structural lowering failed: {source}"),
            Self::Evaluation {
                context, source, ..
            } => {
                format!("failed to evaluate {context}: {source}")
            }
            Self::MassMatrix {
                row,
                state_name,
                reason,
                ..
            } => format!(
                "mass matrix row {row} for state `{state_name}` could not be derived: {reason}"
            ),
        }
    }

    pub fn diagnostic_label(&self) -> String {
        match self {
            Self::Lower(err) => err.label_reason(),
            Self::Structural { source, .. } => format!("structural lowering failed: {source}"),
            _ => self.diagnostic_reason(),
        }
    }

    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::Lower(err) => err.source_span(),
            Self::Structural { source } => source.source_span(),
            Self::Evaluation { span, .. } | Self::MassMatrix { span, .. } => *span,
        }
    }
}

impl From<LowerError> for SolveModelLowerError {
    fn from(value: LowerError) -> Self {
        Self::Lower(value)
    }
}

impl From<EvalError> for SolveModelLowerError {
    fn from(source: EvalError) -> Self {
        Self::Evaluation {
            context: "solve-model lowering expression".to_string(),
            source,
            span: None,
        }
    }
}

/// Lower a DAE model into a Solve IR package, taking ownership to avoid a clone.
///
/// Prefer this over [`lower_dae_to_solve_model`] when the caller no longer needs
/// the DAE after lowering.
pub fn lower_dae_to_solve_model_owned(
    dae_model: dae::Dae,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    crate::clear_solve_lowering_runtime_state();
    lower_dae_to_solve_model_inner(dae_model, None, None)
}

pub fn lower_dae_to_solve_model_owned_with_visible_expressions(
    dae_model: dae::Dae,
    visible_expressions: Vec<VisibleExpression>,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    crate::clear_solve_lowering_runtime_state();
    lower_dae_to_solve_model_inner(dae_model, Some(visible_expressions), None)
}

pub fn lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata(
    dae_model: dae::Dae,
    visible_expressions: Vec<VisibleExpression>,
    metadata_dae_model: &dae::Dae,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    crate::clear_solve_lowering_runtime_state();
    lower_dae_to_solve_model_inner(
        dae_model,
        Some(visible_expressions),
        Some(metadata_dae_model),
    )
}

/// Lower a DAE model into a Solve IR package.
pub fn lower_dae_to_solve_model(
    dae_model: &dae::Dae,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    lower_dae_to_solve_model_owned(dae_model.clone())
}

fn lower_dae_to_solve_model_inner(
    mut dae_model: dae::Dae,
    visible_expressions: Option<Vec<VisibleExpression>>,
    metadata_dae_model: Option<&dae::Dae>,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    let visible_expressions = match visible_expressions {
        Some(visible_expressions) => visible_expressions,
        None => visible_expressions_for_dae(&dae_model).map_err(SolveModelLowerError::Lower)?,
    };
    let state_count = scalar_count(dae_model.variables.states.values());
    let eval_runtime = Arc::new(EvalRuntimeState::default());
    let base_parameters =
        default_parameter_values(&dae_model, metadata_dae_model, eval_runtime.clone())?;
    order_state_derivative_rows(
        &mut dae_model,
        state_count,
        &base_parameters,
        eval_runtime.clone(),
    )?;
    let solver_len =
        solver_visible_scalar_count(&dae_model).max(dae_model.continuous.equations.len());
    let mass_matrix = state_identity_mass_matrix(state_count);
    let problem = crate::lower_solve_problem_with_solver_len(&dae_model, solver_len)?;
    let artifacts = crate::lower_solve_artifacts_with_mass_matrix(&problem, mass_matrix)?;
    let mut parameters = compiled_parameter_values(
        &dae_model,
        metadata_dae_model,
        &problem.solve_layout,
        base_parameters,
        eval_runtime.clone(),
    )?;
    let mut initial_y = initial_solver_values(
        &dae_model,
        metadata_dae_model,
        &parameters,
        problem.solve_layout.solver_scalar_count(),
        eval_runtime.clone(),
    )?;
    apply_initial_equations_to_start_values(
        &dae_model,
        &problem.layout,
        &mut parameters,
        &mut initial_y,
        eval_runtime.clone(),
    )?;
    let table_env = try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        &dae_model,
        &parameters,
        0.0,
        eval_runtime,
    )
    .map_err(|source| runtime_tail_error(&dae_model, source))?;
    let external_tables = external_table_data_for_parameter_values_in(&table_env, &parameters);
    let (visible_names, visible_value_rows) =
        lower_visible_observations(&dae_model, &problem.layout, &visible_expressions)?;
    let variable_meta =
        build_variable_meta(metadata_dae_model.unwrap_or(&dae_model), &visible_names);

    Ok(solve::SolveModel {
        problem,
        artifacts,
        initial_y,
        parameters,
        external_tables: solve::ExternalTables::new(external_tables),
        visible_names,
        visible_value_rows,
        variable_meta,
    })
}

fn lower_visible_observations(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    visible_expressions: &[VisibleExpression],
) -> Result<(Vec<String>, solve::ScalarProgramBlock), SolveModelLowerError> {
    let mut names = Vec::new();
    let mut rows = Vec::new();
    let mut program_spans = Vec::new();
    let structural_bindings = crate::lower::structural_bindings_for_dae(dae_model);
    let indexed_bindings = crate::lower::indexed_bindings_for_layout(layout);
    for visible in visible_expressions {
        if is_unbound_identity_observation(layout, visible) {
            continue;
        }
        match crate::lower::lower_observation_rhs_with_structural_bindings(
            dae_model,
            layout,
            std::slice::from_ref(&visible.expr),
            &structural_bindings,
            &indexed_bindings,
        ) {
            Ok(mut lowered) => {
                append_visible_names(&mut names, &visible.name, lowered.len());
                program_spans.extend(std::iter::repeat_n(
                    visible.expr.span().unwrap_or(rumoca_core::Span::DUMMY),
                    lowered.len(),
                ));
                rows.append(&mut lowered);
            }
            Err(err) if should_skip_unbound_observation(layout, visible, &err) => {}
            Err(err) if should_skip_unsupported_observation(&err) => {}
            Err(err) => {
                return Err(
                    crate::lower_problem_context(err, "lower visible observation rows").into(),
                );
            }
        }
    }
    Ok((
        names,
        solve::ScalarProgramBlock::with_program_spans(rows, program_spans),
    ))
}

fn append_visible_names(names: &mut Vec<String>, base_name: &str, row_count: usize) {
    if row_count == 1 {
        names.push(base_name.to_string());
        return;
    }
    names.extend((1..=row_count).map(|index| format!("{base_name}[{index}]")));
}

fn should_skip_unbound_observation(
    layout: &solve::VarLayout,
    _visible: &VisibleExpression,
    err: &LowerError,
) -> bool {
    let Some(name) = observation_missing_binding_name(err) else {
        return false;
    };
    layout.binding(name).is_none()
}

fn observation_missing_binding_name(err: &LowerError) -> Option<&str> {
    match err {
        LowerError::MissingBinding { name } => Some(name.as_str()),
        LowerError::Spanned { source, .. } | LowerError::WithContext { source, .. } => {
            observation_missing_binding_name(source)
        }
        _ => None,
    }
}

/// Observation (visible-expression) declines: constructs the solve lowering
/// cannot express yet. Classification is variant-based; an error that is not
/// one of these typed declines fails the compile.
fn should_skip_unsupported_observation(err: &LowerError) -> bool {
    match err {
        LowerError::DynamicSubscript
        | LowerError::ForRangeUnknownDimension { .. }
        | LowerError::MissingActualArgument { .. }
        | LowerError::DynamicBindingBase { .. }
        | LowerError::MissingFunction { .. } => true,
        LowerError::Spanned { source, .. } | LowerError::WithContext { source, .. } => {
            should_skip_unsupported_observation(source)
        }
        _ => false,
    }
}

fn is_unbound_identity_observation(layout: &solve::VarLayout, visible: &VisibleExpression) -> bool {
    layout.binding(&visible.name).is_none()
        && matches!(
            &visible.expr,
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                ..
            } if subscripts.is_empty() && name.as_str() == visible.name
        )
}

pub fn visible_expressions_for_dae(
    dae_model: &dae::Dae,
) -> Result<Vec<VisibleExpression>, LowerError> {
    let solver_len =
        solver_visible_scalar_count(dae_model).max(dae_model.continuous.equations.len());
    let mut expressions = collect_visible_solver_expressions(dae_model, solver_len)?;
    expressions.extend(collect_visible_runtime_expressions(dae_model)?);
    Ok(expressions)
}

fn collect_visible_solver_expressions(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Result<Vec<VisibleExpression>, LowerError> {
    let mut expressions = dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
        .filter(|(name, _)| !crate::layout::is_runtime_parameter_tail_variable(dae_model, name))
        .map(|(name, var)| visible_expressions_for_variable(name, var))
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    expressions.truncate(solver_len);
    Ok(expressions)
}

fn collect_visible_runtime_expressions(
    dae_model: &dae::Dae,
) -> Result<Vec<VisibleExpression>, LowerError> {
    dae_model
        .variables
        .inputs
        .iter()
        .chain(dae_model.variables.discrete_reals.iter())
        .chain(dae_model.variables.discrete_valued.iter())
        .map(|(name, var)| visible_expressions_for_variable(name, var))
        .collect::<Result<Vec<_>, _>>()
        .map(|groups| groups.into_iter().flatten().collect())
}

fn visible_expressions_for_variable(
    name: &rumoca_core::VarName,
    var: &dae::Variable,
) -> Result<Vec<VisibleExpression>, LowerError> {
    let size = var.size();
    if size <= 1 && var.dims.is_empty() {
        return Ok(vec![visible_expression_for_variable_scalar(
            name,
            var,
            name.as_str().to_string(),
            Vec::new(),
        )?]);
    }
    (0..size)
        .map(|idx| {
            let subscripts = dae::flat_index_to_subscripts(&var.dims, idx).ok_or_else(|| {
                LowerError::ContractViolation {
                    reason: format!(
                        "visible expression scalar index {idx} is outside variable `{}` shape",
                        name.as_str()
                    ),
                    span: var.source_span,
                }
            })?;
            visible_expression_for_variable_scalar(
                name,
                var,
                dae::scalar_name_text_for_flat_index(name.as_str(), &var.dims, idx),
                subscripts,
            )
        })
        .collect()
}

fn visible_expression_for_variable_scalar(
    name: &rumoca_core::VarName,
    var: &dae::Variable,
    scalar_name: String,
    subscripts: Vec<usize>,
) -> Result<VisibleExpression, LowerError> {
    let reference = match var.origin {
        dae::VariableOrigin::Generated => rumoca_core::Reference::generated(name.as_str()),
        dae::VariableOrigin::Source => {
            #[cfg(test)]
            if var.source_span.is_dummy() {
                return Ok(VisibleExpression {
                    name: scalar_name,
                    expr: rumoca_core::Expression::VarRef {
                        name: rumoca_core::Reference::generated(name.as_str()),
                        subscripts: subscripts
                            .into_iter()
                            .map(|index| {
                                rumoca_core::Subscript::index(index as i64, var.source_span)
                            })
                            .collect(),
                        span: var.source_span,
                    },
                });
            }
            let component_ref =
                var.component_ref
                    .clone()
                    .ok_or_else(|| LowerError::ContractViolation {
                        reason: format!(
                            "source DAE variable `{}` lost structured component-reference metadata before visible observation lowering",
                            name.as_str()
                        ),
                        span: var.source_span,
                    })?;
            rumoca_core::Reference::from_component_reference(component_ref)
        }
    };
    Ok(VisibleExpression {
        name: scalar_name,
        expr: rumoca_core::Expression::VarRef {
            name: reference,
            subscripts: subscripts
                .into_iter()
                .map(|index| rumoca_core::Subscript::index(index as i64, var.source_span))
                .collect(),
            span: var.source_span,
        },
    })
}

fn order_state_derivative_rows(
    dae_model: &mut dae::Dae,
    state_count: usize,
    params: &[f64],
    runtime: Arc<EvalRuntimeState>,
) -> Result<(), SolveModelLowerError> {
    if state_count == 0 || dae_model.continuous.equations.len() <= state_count {
        return Ok(());
    }
    let state_names = dae_model
        .variables
        .states
        .iter()
        .flat_map(|(name, var)| scalar_names(name.as_str(), var))
        .collect::<Vec<_>>();
    let env = try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        dae_model, params, 0.0, runtime,
    )
    .map_err(|source| runtime_tail_error(dae_model, source))?;
    let mut used = vec![false; dae_model.continuous.equations.len()];
    let mut ordered = Vec::with_capacity(dae_model.continuous.equations.len());

    for state_name in state_names.iter().take(state_count) {
        let Some((row_idx, _)) = dae_model
            .continuous
            .equations
            .iter()
            .enumerate()
            .filter(|(idx, _)| !used[*idx])
            .filter_map(|(idx, equation)| {
                derivative_coefficient_expr(&equation.rhs, state_name)
                    .ok()
                    .and_then(|expr| eval_expr::<f64>(&expr, &env).ok().map(|value| (idx, value)))
            })
            .find(|(_, coeff)| coeff.abs() > 1.0e-15)
        else {
            continue;
        };
        used[row_idx] = true;
        ordered.push(dae_model.continuous.equations[row_idx].clone());
    }

    if ordered.len() != state_count {
        return Ok(());
    }
    ordered.extend(
        dae_model
            .continuous
            .equations
            .iter()
            .enumerate()
            .filter(|(idx, _)| !used[*idx])
            .map(|(_, equation)| equation.clone()),
    );
    dae_model.continuous.equations = ordered;
    Ok(())
}

fn scalar_count<'a>(vars: impl Iterator<Item = &'a dae::Variable>) -> usize {
    vars.map(dae::Variable::size).sum()
}

fn solver_visible_scalar_count(dae_model: &dae::Dae) -> usize {
    // MLS Appendix B B.1a: continuous equations are one implicit system. A
    // derivative row can legally reference algebraic variables even when the
    // residual row count has already been reduced by Solve IR lowering, so
    // solve-IR bindings must be sized from solver-visible variables.
    scalar_count(
        dae_model
            .variables
            .states
            .values()
            .chain(dae_model.variables.algebraics.values())
            .chain(dae_model.variables.outputs.values()),
    )
}

fn default_parameter_values(
    dae_model: &dae::Dae,
    metadata_dae_model: Option<&dae::Dae>,
    runtime: Arc<EvalRuntimeState>,
) -> Result<Vec<f64>, SolveModelLowerError> {
    let mut params = Vec::new();
    let mut slots = Vec::new();
    let mut env = try_build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        dae_model,
        &params,
        0.0,
        runtime.clone(),
    )
    .map_err(|source| runtime_tail_error(dae_model, source))?;
    if let Some(metadata_dae_model) = metadata_dae_model {
        seed_missing_default_values_for_all_variables(metadata_dae_model, &mut env);
    }
    for (name, var) in &dae_model.variables.parameters {
        let offset = params.len();
        let values = start_values(dae_model, var, &env)?;
        append_values_for_var(&mut params, var, &values);
        slots.push((name, var, offset));
        seed_var_values(&mut env, name.as_str(), var, &values)?;
    }
    refine_parameter_start_values(dae_model, metadata_dae_model, &mut params, &slots, runtime)?;
    Ok(params)
}

fn refine_parameter_start_values(
    dae_model: &dae::Dae,
    metadata_dae_model: Option<&dae::Dae>,
    params: &mut [f64],
    slots: &[(&rumoca_core::VarName, &dae::Variable, usize)],
    runtime: Arc<EvalRuntimeState>,
) -> Result<(), SolveModelLowerError> {
    for _ in 0..slots.len().clamp(1, 32) {
        let mut changed = false;
        let mut env = try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
            dae_model,
            params,
            0.0,
            runtime.clone(),
        )
        .map_err(|source| runtime_tail_error(dae_model, source))?;
        if let Some(metadata_dae_model) = metadata_dae_model {
            seed_missing_default_values_for_all_variables(metadata_dae_model, &mut env);
        }
        for (name, var, offset) in slots {
            let values = start_values(dae_model, var, &env)?;
            changed |= replace_values_for_var(params, *offset, var, &values);
            seed_var_values(&mut env, name.as_str(), var, &values)?;
        }
        if !changed {
            break;
        }
    }
    Ok(())
}

fn compiled_parameter_values(
    dae_model: &dae::Dae,
    metadata_dae_model: Option<&dae::Dae>,
    layout: &solve::SolveLayout,
    mut values: Vec<f64>,
    runtime: Arc<EvalRuntimeState>,
) -> Result<Vec<f64>, SolveModelLowerError> {
    let mut env = try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        dae_model, &values, 0.0, runtime,
    )
    .map_err(|source| runtime_tail_error(dae_model, source))?;
    if let Some(metadata_dae_model) = metadata_dae_model {
        seed_missing_default_values_for_all_variables(metadata_dae_model, &mut env);
    }
    let runtime_parameters: Vec<_> = dae_model
        .variables
        .inputs
        .iter()
        .chain(dae_model.variables.discrete_reals.iter())
        .chain(dae_model.variables.discrete_valued.iter())
        .collect();
    seed_default_values(dae_model, &mut env, &runtime_parameters)?;
    for (name, var) in runtime_parameters {
        let var_values = start_values(dae_model, var, &env)?;
        append_values_for_var(&mut values, var, &var_values);
        seed_var_values(&mut env, name.as_str(), var, &var_values)?;
    }
    values.resize(layout.compiled_parameter_len, 0.0);
    values.truncate(layout.compiled_parameter_len);
    Ok(values)
}

fn initial_solver_values(
    dae_model: &dae::Dae,
    metadata_dae_model: Option<&dae::Dae>,
    params: &[f64],
    solver_len: usize,
    runtime: Arc<EvalRuntimeState>,
) -> Result<Vec<f64>, SolveModelLowerError> {
    let mut values = Vec::with_capacity(solver_len);
    let mut env = try_build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        dae_model, params, 0.0, runtime,
    )
    .map_err(|source| runtime_tail_error(dae_model, source))?;
    if let Some(metadata_dae_model) = metadata_dae_model {
        seed_missing_default_values_for_all_variables(metadata_dae_model, &mut env);
    }
    let solver_variables: Vec<_> = dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
        .collect();
    seed_default_values(dae_model, &mut env, &solver_variables)?;
    for (name, var) in solver_variables {
        let var_values = start_values(dae_model, var, &env)?;
        append_values_for_var(&mut values, var, &var_values);
        seed_var_values(&mut env, name.as_str(), var, &var_values)?;
        if values.len() >= solver_len {
            values.truncate(solver_len);
            break;
        }
    }
    values.resize(solver_len, 0.0);
    Ok(values)
}

pub(crate) fn replace_if_changed(slot: &mut f64, value: f64) -> bool {
    let changed = !slot.is_finite() || !value.is_finite() || (*slot - value).abs() > 1.0e-12;
    if changed {
        *slot = value;
    }
    changed
}

fn start_values(
    dae_model: &dae::Dae,
    var: &dae::Variable,
    env: &rumoca_eval_dae::VarEnv<f64>,
) -> Result<Vec<f64>, SolveModelLowerError> {
    let default_start = default_start_value(dae_model, var);
    let Some(expr) = var.start.as_ref() else {
        return Ok(default_start_values(var, default_start));
    };
    if start_expr_is_nonnumeric(expr, env) {
        return Ok(default_start_values(var, default_start));
    }
    if var.size() == 0 && !var.dims.is_empty() {
        let raw = if var.dims.len() >= 2 {
            match eval_matrix_values(expr, env) {
                Ok(Some(matrix)) => matrix.into_iter().flatten().collect(),
                Ok(None) => {
                    eval_array_values::<f64>(expr, env).map_err(|err| eval_start_error(var, err))?
                }
                Err(err) => return Err(eval_start_error(var, err)),
            }
        } else {
            eval_array_values::<f64>(expr, env).map_err(|err| eval_start_error(var, err))?
        };
        if raw.is_empty() {
            return Err(eval_start_error(
                var,
                EvalError::UnsupportedExpression {
                    kind: "array start value",
                },
            ));
        }
        return Ok(raw
            .into_iter()
            .map(|value| finite_start_value(value, default_start))
            .collect());
    }
    if var.size() <= 1 && var.dims.is_empty() {
        let value = eval_expr::<f64>(expr, env).map_err(|err| eval_start_error(var, err))?;
        return Ok(vec![finite_start_value(value, default_start)]);
    }
    let raw = match shaped_start_values(expr, env, var.size()) {
        Ok(values) => values,
        Err(EvalError::ShapeMismatch { actual: 1, .. }) if can_broadcast_start_value(expr, env) => {
            vec![eval_expr::<f64>(expr, env).map_err(|err| eval_start_error(var, err))?; var.size()]
        }
        Err(err) => return Err(eval_start_error(var, err)),
    };
    Ok(raw
        .into_iter()
        .map(|value| finite_start_value(value, default_start))
        .collect())
}

fn shaped_start_values(
    expr: &rumoca_core::Expression,
    env: &rumoca_eval_dae::VarEnv<f64>,
    expected_len: usize,
) -> Result<Vec<f64>, EvalError> {
    eval_shaped_array_values(expr, env, expected_len)
}

fn seed_default_values(
    dae_model: &dae::Dae,
    env: &mut rumoca_eval_dae::VarEnv<f64>,
    variables: &[(&rumoca_core::VarName, &dae::Variable)],
) -> Result<(), SolveModelLowerError> {
    for (name, var) in variables {
        let default_start = default_start_value(dae_model, var);
        let values = default_start_values(var, default_start);
        seed_var_values(env, name.as_str(), var, &values)?;
    }
    Ok(())
}

fn seed_missing_default_values_for_all_variables(
    dae_model: &dae::Dae,
    env: &mut rumoca_eval_dae::VarEnv<f64>,
) {
    for (name, var) in dae_model
        .variables
        .parameters
        .iter()
        .chain(dae_model.variables.constants.iter())
        .chain(dae_model.variables.states.iter())
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
        .chain(dae_model.variables.inputs.iter())
        .chain(dae_model.variables.discrete_reals.iter())
        .chain(dae_model.variables.discrete_valued.iter())
    {
        let default_start = default_start_value(dae_model, var);
        let values = expand_values_to_size(default_start_values(var, default_start), var.size());
        for (scalar_name, value) in scalar_names(name.as_str(), var).into_iter().zip(values) {
            if !env.vars.contains_key(scalar_name.as_str()) {
                env.set(scalar_name.as_str(), value);
            }
        }
    }
}

fn default_start_values(var: &dae::Variable, default_start: f64) -> Vec<f64> {
    let size = var.size();
    if size == 0 && !var.dims.is_empty() {
        return Vec::new();
    }
    vec![default_start; size.max(1)]
}

fn eval_start_error(var: &dae::Variable, source: EvalError) -> SolveModelLowerError {
    SolveModelLowerError::Evaluation {
        context: format!("start value for `{}`", var.name),
        source,
        span: var
            .start
            .as_ref()
            .and_then(|expr| expr.span())
            .or(Some(var.source_span)),
    }
}

pub(crate) fn runtime_tail_error(dae_model: &dae::Dae, source: EvalError) -> SolveModelLowerError {
    let span = eval_error_variable_span(dae_model, &source);
    SolveModelLowerError::Evaluation {
        context: "runtime parameter tail".to_string(),
        source,
        span,
    }
}

fn eval_error_variable_span(dae_model: &dae::Dae, source: &EvalError) -> Option<rumoca_core::Span> {
    let Some(name) = source.missing_binding_name() else {
        return source.source_span();
    };
    all_dae_variables(dae_model)
        .find_map(|(var_name, variable)| {
            (var_name.as_str() == name).then_some(variable.source_span)
        })
        .or_else(|| scalar_variable_span(dae_model, name))
}

fn scalar_variable_span(dae_model: &dae::Dae, scalar_name: &str) -> Option<rumoca_core::Span> {
    all_dae_variables(dae_model).find_map(|(name, variable)| {
        scalar_names(name.as_str(), variable)
            .into_iter()
            .any(|candidate| candidate == scalar_name)
            .then_some(variable.source_span)
    })
}

fn all_dae_variables(
    dae_model: &dae::Dae,
) -> impl Iterator<Item = (&rumoca_core::VarName, &dae::Variable)> {
    dae_model
        .variables
        .parameters
        .iter()
        .chain(dae_model.variables.constants.iter())
        .chain(dae_model.variables.states.iter())
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
        .chain(dae_model.variables.inputs.iter())
        .chain(dae_model.variables.discrete_reals.iter())
        .chain(dae_model.variables.discrete_valued.iter())
}

fn finite_start_value(value: f64, default_start: f64) -> f64 {
    if value.is_finite() {
        value
    } else {
        default_start
    }
}

fn default_start_value(dae_model: &dae::Dae, var: &dae::Variable) -> f64 {
    dae_model
        .symbols
        .enum_literal_ordinals
        .get(var.name.as_str())
        .map_or(0.0, |ordinal| *ordinal as f64)
}

fn append_values_for_var(out: &mut Vec<f64>, var: &dae::Variable, values: &[f64]) {
    let size = var.size();
    if size == 0 {
        // MLS Chapter 10 dynamic arrays may be bound by expressions whose
        // extent is known only from parameter evaluation. Their aggregate value
        // lives in the evaluation environment and must not shift flattened
        // solver/parameter slots.
        return;
    }
    let expanded = expand_values_to_size(values.to_vec(), size);
    out.extend(expanded.into_iter().take(size));
}

fn replace_values_for_var(
    out: &mut [f64],
    offset: usize,
    var: &dae::Variable,
    values: &[f64],
) -> bool {
    let size = var.size();
    if size == 0 || offset >= out.len() {
        return false;
    }
    let expanded = expand_values_to_size(values.to_vec(), size);
    let end = (offset + size).min(out.len());
    out[offset..end]
        .iter_mut()
        .zip(expanded)
        .fold(false, |changed, (slot, value)| {
            replace_if_changed(slot, value) || changed
        })
}

fn seed_var_values(
    env: &mut rumoca_eval_dae::VarEnv<f64>,
    name: &str,
    var: &dae::Variable,
    values: &[f64],
) -> Result<(), SolveModelLowerError> {
    let size = var.size();
    if size <= 1 && var.dims.is_empty() {
        let value = values
            .first()
            .copied()
            .ok_or_else(|| SolveModelLowerError::Evaluation {
                context: format!("start value for `{}`", var.name),
                source: EvalError::UnsupportedExpression {
                    kind: "empty scalar start value",
                },
                span: Some(var.source_span),
            })?;
        env.set(name, value);
        return Ok(());
    }
    rumoca_eval_dae::set_array_entries(env, name, &var.dims, values);
    Ok(())
}

pub(crate) fn expand_values_to_size(raw: Vec<f64>, size: usize) -> Vec<f64> {
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
    (0..size)
        .map(|idx| raw.get(idx).copied().unwrap_or(last))
        .collect()
}

fn state_identity_mass_matrix(state_count: usize) -> Vec<Vec<f64>> {
    // The solve-IR derivative RHS rows already solve the MLS §8.3 equation
    // system for der(state), including coupled derivative rows. Concrete
    // solvers therefore receive x' = f(x, p, t) for state rows.
    let mut mass = vec![vec![0.0; state_count]; state_count];
    for (idx, row) in mass.iter_mut().enumerate() {
        row[idx] = 1.0;
    }
    mass
}

pub(crate) fn scalar_names(name: &str, var: &dae::Variable) -> Vec<String> {
    let size = var.size();
    if size <= 1 && var.dims.is_empty() {
        return vec![name.to_string()];
    }
    (0..size)
        .map(|idx| dae::scalar_name_text_for_flat_index(name, &var.dims, idx))
        .collect()
}

fn derivative_coefficient_expr(
    expr: &rumoca_core::Expression,
    state_name: &str,
) -> Result<rumoca_core::Expression, String> {
    match expr {
        rumoca_core::Expression::BuiltinCall { function, args, .. }
            if *function == rumoca_core::BuiltinFunction::Der =>
        {
            Ok(if der_arg_matches(args, state_name) {
                real_expr(1.0)
            } else {
                real_expr(0.0)
            })
        }
        rumoca_core::Expression::Unary {
            op: OpUnary::Minus | OpUnary::DotMinus,
            rhs,
            ..
        } => Ok(neg_expr(derivative_coefficient_expr(rhs, state_name)?)),
        rumoca_core::Expression::Binary {
            op: OpBinary::Add | OpBinary::AddElem,
            lhs,
            rhs,
            ..
        } => Ok(binary_expr(
            add_op(),
            derivative_coefficient_expr(lhs, state_name)?,
            derivative_coefficient_expr(rhs, state_name)?,
        )),
        rumoca_core::Expression::Binary {
            op: OpBinary::Sub | OpBinary::SubElem,
            lhs,
            rhs,
            ..
        } => Ok(binary_expr(
            sub_op(),
            derivative_coefficient_expr(lhs, state_name)?,
            derivative_coefficient_expr(rhs, state_name)?,
        )),
        rumoca_core::Expression::Binary {
            op: OpBinary::Mul | OpBinary::MulElem,
            lhs,
            rhs,
            ..
        } => coefficient_product(lhs, rhs, state_name),
        rumoca_core::Expression::Binary {
            op: OpBinary::Div | OpBinary::DivElem,
            lhs,
            rhs,
            ..
        } => {
            if rhs.contains_der() {
                return Err("derivative appears in denominator".to_string());
            }
            Ok(binary_expr(
                div_op(),
                derivative_coefficient_expr(lhs, state_name)?,
                rhs.as_ref().clone(),
            ))
        }
        _ if expr.contains_der() => Err("unsupported derivative expression shape".to_string()),
        _ => Ok(real_expr(0.0)),
    }
}

fn coefficient_product(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    state_name: &str,
) -> Result<rumoca_core::Expression, String> {
    let lhs_has_der = lhs.contains_der();
    let rhs_has_der = rhs.contains_der();
    match (lhs_has_der, rhs_has_der) {
        (true, true) => Err("nonlinear derivative product".to_string()),
        (true, false) => Ok(binary_expr(
            mul_op(),
            derivative_coefficient_expr(lhs, state_name)?,
            rhs.clone(),
        )),
        (false, true) => Ok(binary_expr(
            mul_op(),
            lhs.clone(),
            derivative_coefficient_expr(rhs, state_name)?,
        )),
        (false, false) => Ok(real_expr(0.0)),
    }
}

fn der_arg_matches(args: &[rumoca_core::Expression], state_name: &str) -> bool {
    let Some(rumoca_core::Expression::VarRef {
        name, subscripts, ..
    }) = args.first()
    else {
        return false;
    };
    if subscripts.is_empty() {
        return name.as_str() == state_name;
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        let Some(text) = subscript_index_text(sub) else {
            return false;
        };
        indices.push(text);
    }
    format!("{}[{}]", name.as_str(), indices.join(",")) == state_name
}

fn subscript_index_text(sub: &rumoca_core::Subscript) -> Option<String> {
    match sub {
        rumoca_core::Subscript::Index { value: i, .. } => Some(i.to_string()),
        rumoca_core::Subscript::Expr { expr, .. } => match expr.as_ref() {
            rumoca_core::Expression::Literal {
                value: Literal::Integer(i),
                ..
            } => Some(i.to_string()),
            rumoca_core::Expression::Literal {
                value: Literal::Real(v),
                ..
            } if v.is_finite() && v.fract() == 0.0 => Some((*v as i64).to_string()),
            _ => None,
        },
        _ => None,
    }
}

fn real_expr(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn neg_expr(expr: rumoca_core::Expression) -> rumoca_core::Expression {
    rumoca_core::Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(expr),
        span: rumoca_core::Span::DUMMY,
    }
}

fn binary_expr(
    op: OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}

fn add_op() -> OpBinary {
    OpBinary::Add
}

fn sub_op() -> OpBinary {
    OpBinary::Sub
}

fn mul_op() -> OpBinary {
    OpBinary::Mul
}

fn div_op() -> OpBinary {
    OpBinary::Div
}

fn build_variable_meta(
    dae_model: &dae::Dae,
    visible_names: &[String],
) -> Vec<solve::SolveVariableMeta> {
    let event_discontinuous_names = event_discontinuous_scalar_names(dae_model);
    let vars = dae_model
        .variables
        .states
        .iter()
        .map(|(name, var)| (name, var, "state", true))
        .chain(
            dae_model
                .variables
                .algebraics
                .iter()
                .map(|(name, var)| (name, var, "algebraic", false)),
        )
        .chain(
            dae_model
                .variables
                .outputs
                .iter()
                .map(|(name, var)| (name, var, "output", false)),
        )
        .chain(
            dae_model
                .variables
                .inputs
                .iter()
                .map(|(name, var)| (name, var, "input", false)),
        )
        .chain(
            dae_model
                .variables
                .discrete_reals
                .iter()
                .map(|(name, var)| (name, var, "discrete-real", false)),
        )
        .chain(
            dae_model
                .variables
                .discrete_valued
                .iter()
                .map(|(name, var)| (name, var, "discrete-valued", false)),
        )
        .collect::<Vec<_>>();
    let mut by_scalar = IndexMap::new();
    for (name, var, role, is_state) in vars {
        let (value_type, variability, time_domain) = variable_meta_classification(role, is_state);
        let start_text = var.start.as_ref().map(|expr| format!("{expr:?}"));
        let min_text = var.min.as_ref().map(|expr| format!("{expr:?}"));
        let max_text = var.max.as_ref().map(|expr| format!("{expr:?}"));
        let nominal_text = var.nominal.as_ref().map(|expr| format!("{expr:?}"));
        for scalar_name in scalar_names(name.as_str(), var) {
            let time_domain = if continuous_real_role_is_event_discontinuous(
                role,
                &scalar_name,
                &event_discontinuous_names,
            ) {
                Some("event-discontinuous".to_string())
            } else {
                time_domain.clone()
            };
            by_scalar.insert(
                scalar_name.clone(),
                solve::SolveVariableMeta {
                    name: scalar_name,
                    source_span: var.source_span,
                    role: role.to_string(),
                    is_state,
                    value_type: value_type.clone(),
                    variability: variability.clone(),
                    time_domain: time_domain.clone(),
                    unit: var.unit.clone(),
                    start: start_text.clone(),
                    min: min_text.clone(),
                    max: max_text.clone(),
                    nominal: nominal_text.clone(),
                    fixed: var.fixed,
                    description: var.description.clone(),
                },
            );
        }
    }
    visible_names
        .iter()
        .filter_map(|name| by_scalar.get(name).cloned())
        .collect()
}

fn continuous_real_role_is_event_discontinuous(
    role: &str,
    scalar_name: &str,
    event_discontinuous_names: &IndexSet<String>,
) -> bool {
    matches!(role, "algebraic" | "output" | "input")
        && event_discontinuous_names.contains(scalar_name)
}

fn event_discontinuous_scalar_names(dae_model: &dae::Dae) -> IndexSet<String> {
    let event_discrete_names = event_discrete_scalar_names(dae_model);
    let event_discrete_bases = base_name_index(&event_discrete_names);
    let mut definitions = continuous_definition_expressions(dae_model);
    let mut event_discontinuous = IndexSet::new();
    let mut event_discontinuous_bases: IndexMap<String, Vec<String>> = IndexMap::new();
    loop {
        let before = event_discontinuous.len();
        let mut found = Vec::new();
        for (scalar_name, exprs) in &definitions {
            if event_discontinuous.contains(scalar_name) {
                continue;
            }
            if exprs.iter().any(|expr| {
                expression_is_event_discontinuous(
                    expr,
                    ScalarNameLookup {
                        names: &event_discrete_names,
                        base_index: &event_discrete_bases,
                    },
                    ScalarNameLookup {
                        names: &event_discontinuous,
                        base_index: &event_discontinuous_bases,
                    },
                )
            }) {
                found.push(scalar_name.clone());
            }
        }
        for scalar_name in found {
            if let Some(base) = dae::component_base_name(&scalar_name) {
                event_discontinuous_bases
                    .entry(base)
                    .or_default()
                    .push(scalar_name.clone());
            }
            event_discontinuous.insert(scalar_name);
        }
        if event_discontinuous.len() == before {
            return event_discontinuous;
        }
        definitions.retain(|name, _| !event_discontinuous.contains(name));
    }
}

fn event_discrete_scalar_names(dae_model: &dae::Dae) -> IndexSet<String> {
    dae_model
        .variables
        .discrete_reals
        .iter()
        .chain(dae_model.variables.discrete_valued.iter())
        .flat_map(|(name, var)| scalar_names(name.as_str(), var))
        .collect()
}

fn continuous_definition_expressions(
    dae_model: &dae::Dae,
) -> IndexMap<String, Vec<&rumoca_core::Expression>> {
    let continuous_vars = dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
        .chain(dae_model.variables.inputs.iter())
        .map(|(name, var)| (name.clone(), scalar_names(name.as_str(), var)))
        .collect::<IndexMap<_, _>>();
    let continuous_names = continuous_vars
        .values()
        .flat_map(|names| names.iter().cloned())
        .collect::<IndexSet<_>>();
    let continuous_base_index = base_name_index(&continuous_names);
    let continuous_lookup = ScalarNameLookup {
        names: &continuous_names,
        base_index: &continuous_base_index,
    };
    let mut definitions = IndexMap::new();
    for eq in &dae_model.continuous.equations {
        if let Some(lhs) = eq.lhs.as_ref() {
            add_continuous_lhs_definitions(
                &mut definitions,
                &continuous_vars,
                lhs.var_name(),
                &eq.rhs,
            );
            continue;
        }
        collect_residual_continuous_definitions(&eq.rhs, continuous_lookup, &mut definitions);
    }
    definitions
}

fn add_continuous_lhs_definitions<'m>(
    definitions: &mut IndexMap<String, Vec<&'m rumoca_core::Expression>>,
    continuous_vars: &IndexMap<rumoca_core::VarName, Vec<String>>,
    lhs: &rumoca_core::VarName,
    rhs: &'m rumoca_core::Expression,
) {
    let Some(scalars) = continuous_vars.get(lhs) else {
        return;
    };
    for scalar_name in scalars {
        add_continuous_definition(definitions, scalar_name, rhs);
    }
}

fn add_continuous_definition<'m>(
    definitions: &mut IndexMap<String, Vec<&'m rumoca_core::Expression>>,
    scalar_name: &str,
    expr: &'m rumoca_core::Expression,
) {
    definitions
        .entry(scalar_name.to_string())
        .or_default()
        .push(expr);
}

fn collect_residual_continuous_definitions<'m>(
    expr: &'m rumoca_core::Expression,
    continuous_names: ScalarNameLookup<'_>,
    definitions: &mut IndexMap<String, Vec<&'m rumoca_core::Expression>>,
) -> IndexSet<String> {
    match expr {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => collect_sub_residual_definitions(lhs, rhs, continuous_names, definitions),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => collect_if_residual_definitions(branches, else_branch, continuous_names, definitions),
        _ => IndexSet::new(),
    }
}

fn collect_sub_residual_definitions<'m>(
    lhs: &'m rumoca_core::Expression,
    rhs: &'m rumoca_core::Expression,
    continuous_names: ScalarNameLookup<'_>,
    definitions: &mut IndexMap<String, Vec<&'m rumoca_core::Expression>>,
) -> IndexSet<String> {
    let mut targets = IndexSet::new();
    for scalar_name in continuous_ref_scalar_names(lhs, continuous_names) {
        add_continuous_definition(definitions, &scalar_name, rhs);
        targets.insert(scalar_name);
    }
    for scalar_name in continuous_ref_scalar_names(rhs, continuous_names) {
        add_continuous_definition(definitions, &scalar_name, lhs);
        targets.insert(scalar_name);
    }
    targets
}

fn collect_if_residual_definitions<'m>(
    branches: &'m [(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &'m rumoca_core::Expression,
    continuous_names: ScalarNameLookup<'_>,
    definitions: &mut IndexMap<String, Vec<&'m rumoca_core::Expression>>,
) -> IndexSet<String> {
    let mut guards = Vec::new();
    let mut targets = IndexSet::new();
    for (condition, branch_expr) in branches {
        guards.push(condition);
        targets.extend(collect_residual_continuous_definitions(
            branch_expr,
            continuous_names,
            definitions,
        ));
    }
    targets.extend(collect_residual_continuous_definitions(
        else_branch,
        continuous_names,
        definitions,
    ));
    for scalar_name in &targets {
        for guard in &guards {
            add_continuous_definition(definitions, scalar_name, guard);
        }
    }
    targets
}

fn continuous_ref_scalar_names(
    expr: &rumoca_core::Expression,
    continuous_names: ScalarNameLookup<'_>,
) -> Vec<String> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return Vec::new();
    };
    event_dependency_ref_names(name, subscripts)
        .into_iter()
        .flat_map(|candidate| continuous_names.matching_scalar_names(&candidate))
        .collect()
}

/// Scalar-name membership with subscript-stripped base names indexed once,
/// so per-reference candidate checks avoid re-parsing every set entry.
#[derive(Clone, Copy)]
struct ScalarNameLookup<'a> {
    names: &'a IndexSet<String>,
    base_index: &'a IndexMap<String, Vec<String>>,
}

fn base_name_index(names: &IndexSet<String>) -> IndexMap<String, Vec<String>> {
    let mut index: IndexMap<String, Vec<String>> = IndexMap::new();
    for name in names {
        if let Some(base) = dae::component_base_name(name) {
            index.entry(base).or_default().push(name.clone());
        }
    }
    index
}

impl ScalarNameLookup<'_> {
    fn contains(&self, candidate: &str) -> bool {
        self.names.contains(candidate) || self.base_index.contains_key(candidate)
    }

    fn matching_scalar_names(&self, candidate: &str) -> Vec<String> {
        if self.names.contains(candidate) {
            return vec![candidate.to_string()];
        }
        self.base_index.get(candidate).cloned().unwrap_or_default()
    }
}

fn expression_is_event_discontinuous(
    expr: &rumoca_core::Expression,
    event_discrete_names: ScalarNameLookup<'_>,
    event_discontinuous_names: ScalarNameLookup<'_>,
) -> bool {
    let mut checker = EventDiscontinuityChecker {
        event_discrete_names,
        event_discontinuous_names,
        found: false,
        no_event_depth: 0,
    };
    checker.visit_expression(expr);
    checker.found
}

struct EventDiscontinuityChecker<'a> {
    event_discrete_names: ScalarNameLookup<'a>,
    event_discontinuous_names: ScalarNameLookup<'a>,
    found: bool,
    no_event_depth: usize,
}

impl ExpressionVisitor for EventDiscontinuityChecker<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        let names = event_dependency_ref_names(name, subscripts);
        if names.iter().any(|name| {
            self.event_discrete_names.contains(name)
                || self.event_discontinuous_names.contains(name)
        }) {
            self.found = true;
            return;
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::NoEvent {
            self.no_event_depth += 1;
            for arg in args {
                self.visit_expression(arg);
            }
            self.no_event_depth -= 1;
            return;
        }
        if self.no_event_depth == 0
            && matches!(
                function,
                rumoca_core::BuiltinFunction::Div
                    | rumoca_core::BuiltinFunction::Mod
                    | rumoca_core::BuiltinFunction::Rem
                    | rumoca_core::BuiltinFunction::Ceil
                    | rumoca_core::BuiltinFunction::Floor
                    | rumoca_core::BuiltinFunction::Integer
                    | rumoca_core::BuiltinFunction::Delay
            )
        {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_binary(
        &mut self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
    ) {
        if self.no_event_depth == 0
            && matches!(
                op,
                rumoca_core::OpBinary::Ge
                    | rumoca_core::OpBinary::Gt
                    | rumoca_core::OpBinary::Le
                    | rumoca_core::OpBinary::Lt
            )
        {
            self.found = true;
            return;
        }
        self.visit_expression(lhs);
        self.visit_expression(rhs);
    }
}

fn event_dependency_ref_names(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
) -> Vec<String> {
    let base = name
        .as_str()
        .strip_prefix("__pre__.")
        .unwrap_or_else(|| name.as_str())
        .to_string();
    let mut names = vec![base.clone()];
    if !subscripts.is_empty()
        && let Some(indices) = crate::literal_positive_indices(subscripts)
    {
        names.push(dae::format_subscript_key(&base, &indices));
    }
    names
}

fn variable_meta_classification(
    role: &str,
    is_state: bool,
) -> (Option<String>, Option<String>, Option<String>) {
    if is_state {
        return (
            Some("Real".to_string()),
            Some("continuous".to_string()),
            Some("continuous-time".to_string()),
        );
    }

    match role {
        "algebraic" | "output" | "input" => (
            Some("Real".to_string()),
            Some("continuous".to_string()),
            Some("continuous-time".to_string()),
        ),
        "discrete-real" => (
            Some("Real".to_string()),
            Some("discrete".to_string()),
            Some("event-discrete".to_string()),
        ),
        "discrete-valued" => (
            Some("Boolean/Integer/Enum".to_string()),
            Some("discrete".to_string()),
            Some("event-discrete".to_string()),
        ),
        _ => (None, None, None),
    }
}

#[cfg(test)]
mod tests;
