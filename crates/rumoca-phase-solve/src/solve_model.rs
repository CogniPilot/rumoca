//! Lower a DAE model into the solver-facing Solve IR package.
//!
//! This is the last DAE-aware step before runtime backends. Backends should
//! receive only `rumoca-ir-solve` data.
//!
//! SPEC_0021 file-size exception: solve-model lowering still coordinates model
//! inventory, RHS assembly, visibility rows, and tensor preservation.
//! split plan: move visibility assembly and model inventory helpers into submodules.

use indexmap::{IndexMap, IndexSet};
use rumoca_core::{ExpressionVisitor, Literal, OpBinary, OpUnary};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use std::sync::Arc;

use crate::LowerError;
use crate::initial_values::apply_initial_equations_to_start_values;
#[cfg(test)]
use rumoca_eval_dae::build_runtime_parameter_tail_env_with_runtime;
use rumoca_eval_dae::constant::eval_scalar_const_expr;
use rumoca_eval_dae::eval::{
    EvalError, EvalRuntimeState, eval_matrix_values, eval_shaped_array_values,
    external_table_data_for_parameter_values_in,
};
use rumoca_eval_dae::{
    build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime,
    build_runtime_parameter_tail_env_with_declared_slots_and_runtime, can_broadcast_start_value,
    eval_array_values, eval_expr, start_expr_is_nonnumeric,
};
use std::collections::{HashMap, HashSet};

mod state_derivative_ordering;
mod variable_meta;
use state_derivative_ordering::order_state_derivative_rows;
#[cfg(test)]
use state_derivative_ordering::{
    reserve_state_derivative_order_capacity, reserve_state_derivative_order_flags,
};
use variable_meta::{
    continuous_real_role_is_event_discontinuous, event_discontinuous_scalar_names,
    variable_meta_classification,
};

#[derive(Clone, Debug, PartialEq)]
pub struct VisibleExpression {
    pub name: String,
    pub expr: rumoca_core::Expression,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SolveModelLoweringProfile {
    Runtime,
    GpuPreparation,
}

impl SolveModelLoweringProfile {
    fn needs_runtime_support(self) -> bool {
        self == Self::Runtime
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
    lower_dae_to_solve_model_inner(
        dae_model,
        None,
        None,
        SolveModelLoweringProfile::Runtime,
        &HashMap::new(),
    )
}

pub fn lower_dae_to_solve_model_owned_with_visible_expressions(
    dae_model: dae::Dae,
    visible_expressions: Vec<VisibleExpression>,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    crate::clear_solve_lowering_runtime_state();
    lower_dae_to_solve_model_inner(
        dae_model,
        Some(visible_expressions),
        None,
        SolveModelLoweringProfile::Runtime,
        &HashMap::new(),
    )
}

pub fn lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata(
    dae_model: dae::Dae,
    visible_expressions: Vec<VisibleExpression>,
    metadata_dae_model: &dae::Dae,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata_and_overrides(
        dae_model,
        visible_expressions,
        metadata_dae_model,
        &HashMap::new(),
    )
}

/// As [`lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata`], but
/// applies tunable scalar-parameter overrides while computing parameter values, so
/// parameter-derived quantities (including array masks) re-derive from the override
/// at parameter-set time instead of being baked at the declared default.
pub fn lower_dae_to_solve_model_owned_with_visible_expressions_and_metadata_and_overrides(
    dae_model: dae::Dae,
    visible_expressions: Vec<VisibleExpression>,
    metadata_dae_model: &dae::Dae,
    param_overrides: &HashMap<String, f64>,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    crate::clear_solve_lowering_runtime_state();
    lower_dae_to_solve_model_inner(
        dae_model,
        Some(visible_expressions),
        Some(metadata_dae_model),
        SolveModelLoweringProfile::Runtime,
        param_overrides,
    )
}

pub fn lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata(
    dae_model: dae::Dae,
    metadata_dae_model: &dae::Dae,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata_and_overrides(
        dae_model,
        metadata_dae_model,
        &HashMap::new(),
    )
}

/// As [`lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata`], but
/// applies tunable scalar-parameter overrides while computing parameter values
/// (see the override-aware runtime entry above for the rationale).
pub fn lower_dae_to_solve_model_owned_for_gpu_preparation_with_metadata_and_overrides(
    dae_model: dae::Dae,
    metadata_dae_model: &dae::Dae,
    param_overrides: &HashMap<String, f64>,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    crate::clear_solve_lowering_runtime_state();
    lower_dae_to_solve_model_inner(
        dae_model,
        None,
        Some(metadata_dae_model),
        SolveModelLoweringProfile::GpuPreparation,
        param_overrides,
    )
}

/// Lower a DAE model into a Solve IR package.
pub fn lower_dae_to_solve_model(
    dae_model: &dae::Dae,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    lower_dae_to_solve_model_owned(dae_model.clone())
}

fn lower_profiled_solve_problem(
    dae_model: &dae::Dae,
    solver_len: usize,
    model_span: rumoca_core::Span,
    profile: SolveModelLoweringProfile,
) -> Result<solve::SolveProblem, LowerError> {
    match profile {
        SolveModelLoweringProfile::Runtime => {
            crate::lower_solve_problem_with_solver_len_and_model_span(
                dae_model,
                solver_len,
                Some(model_span),
            )
        }
        SolveModelLoweringProfile::GpuPreparation => {
            crate::lower_solve_problem_with_solver_len_and_model_span_and_profile(
                dae_model,
                solver_len,
                Some(model_span),
                crate::SolveProblemLoweringProfile::GpuPreparation,
            )
        }
    }
}

fn lower_dae_to_solve_model_inner(
    mut dae_model: dae::Dae,
    visible_expressions: Option<Vec<VisibleExpression>>,
    metadata_dae_model: Option<&dae::Dae>,
    profile: SolveModelLoweringProfile,
    param_overrides: &HashMap<String, f64>,
) -> Result<solve::SolveModel, SolveModelLowerError> {
    let visible_expressions = if profile.needs_runtime_support() {
        match visible_expressions {
            Some(visible_expressions) => visible_expressions,
            None => visible_expressions_for_dae(&dae_model).map_err(SolveModelLowerError::Lower)?,
        }
    } else {
        Vec::new()
    };
    let state_count = scalar_count(dae_model.variables.states.values())?;
    let eval_runtime = Arc::new(EvalRuntimeState::default());
    let base_parameters = default_parameter_values(
        &dae_model,
        metadata_dae_model,
        eval_runtime.clone(),
        param_overrides,
    )?;
    if profile.needs_runtime_support() {
        order_state_derivative_rows(
            &mut dae_model,
            state_count,
            &base_parameters,
            eval_runtime.clone(),
        )?;
    }
    let solver_len = match profile {
        SolveModelLoweringProfile::Runtime => {
            solver_visible_scalar_count(&dae_model)?.max(dae_model.continuous.equations.len())
        }
        SolveModelLoweringProfile::GpuPreparation => state_count,
    };
    let model_span = model_provenance_span(&dae_model, metadata_dae_model)?;
    let problem = lower_profiled_solve_problem(&dae_model, solver_len, model_span, profile)?;
    let artifacts = if profile.needs_runtime_support() {
        let mass_matrix = state_identity_mass_matrix(state_count, model_span)?;
        crate::lower_solve_artifacts_with_mass_matrix(&problem, mass_matrix)?
    } else {
        solve::SolveArtifacts::default()
    };
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
    let table_env = build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        &dae_model,
        &parameters,
        0.0,
        eval_runtime,
    )
    .map_err(|source| runtime_tail_error(&dae_model, source))?;
    let external_tables = external_table_data_for_parameter_values_in(&table_env, &parameters);
    let (visible_names, visible_value_rows) = if profile.needs_runtime_support() {
        lower_visible_observations(&dae_model, &problem.layout, &visible_expressions)?
    } else {
        (Vec::new(), solve::ScalarProgramBlock::default())
    };
    let variable_meta = if profile.needs_runtime_support() {
        build_variable_meta(metadata_dae_model.unwrap_or(&dae_model), &visible_names)?
    } else {
        Vec::new()
    };

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
    let structural_bindings = crate::lower::structural_bindings_for_dae(dae_model)?;
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
                let span = visible_expression_span(visible)?;
                append_visible_names(&mut names, &visible.name, lowered.len());
                program_spans.extend(std::iter::repeat_n(span, lowered.len()));
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
        solve::ScalarProgramBlock::with_program_spans(rows, program_spans)
            .map_err(LowerError::from)?,
    ))
}

fn visible_expression_span(
    visible: &VisibleExpression,
) -> Result<rumoca_core::Span, SolveModelLowerError> {
    visible.expr.span().ok_or_else(|| {
        SolveModelLowerError::Lower(LowerError::UnspannedContractViolation {
            reason: format!(
                "visible observation `{}` reached solve-model lowering without expression provenance",
                visible.name
            ),
        })
    })
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
        solver_visible_scalar_count(dae_model)?.max(dae_model.continuous.equations.len());
    let mut expressions = collect_visible_solver_expressions(dae_model, solver_len)?;
    let runtime_expressions = collect_visible_runtime_expressions(dae_model)?;
    reserve_lower_capacity(
        &mut expressions,
        runtime_expressions.len(),
        "visible expression count",
        dae_model_span(dae_model, "visible expression inventory")?,
    )?;
    expressions.extend(runtime_expressions);
    Ok(expressions)
}

fn lower_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_lower_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn reserve_lower_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        lower_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn lower_contract_violation(reason: String, span: rumoca_core::Span) -> LowerError {
    if span.is_dummy() {
        LowerError::UnspannedContractViolation { reason }
    } else {
        LowerError::ContractViolation { reason, span }
    }
}

fn solve_model_contract_violation(reason: String, span: rumoca_core::Span) -> SolveModelLowerError {
    SolveModelLowerError::Lower(lower_contract_violation(reason, span))
}

fn dae_model_span(
    dae_model: &dae::Dae,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    dae_model
        .variables
        .states
        .values()
        .chain(dae_model.variables.algebraics.values())
        .chain(dae_model.variables.outputs.values())
        .chain(dae_model.variables.inputs.values())
        .chain(dae_model.variables.discrete_reals.values())
        .chain(dae_model.variables.discrete_valued.values())
        .chain(dae_model.variables.parameters.values())
        .chain(dae_model.variables.constants.values())
        .find_map(|var| (!var.source_span.is_dummy()).then_some(var.source_span))
        .or_else(|| {
            dae_model
                .continuous
                .equations
                .iter()
                .find_map(|equation| (!equation.span.is_dummy()).then_some(equation.span))
        })
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: format!("DAE model has no source provenance for {context}"),
        })
}

fn collect_visible_solver_expressions(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Result<Vec<VisibleExpression>, LowerError> {
    let span = dae_model_span(dae_model, "visible solver expression count")?;
    let mut expressions =
        lower_vec_with_capacity(solver_len, "visible solver expression count", span)?;
    for (name, var) in dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
        .filter(|(name, _)| !crate::layout::is_runtime_parameter_tail_variable(dae_model, name))
    {
        let variable_expressions = visible_expressions_for_variable(name, var)?;
        reserve_lower_capacity(
            &mut expressions,
            variable_expressions.len(),
            "visible solver expression count",
            var.source_span,
        )?;
        expressions.extend(variable_expressions);
    }
    expressions.truncate(solver_len);
    Ok(expressions)
}

fn collect_visible_runtime_expressions(
    dae_model: &dae::Dae,
) -> Result<Vec<VisibleExpression>, LowerError> {
    let span = dae_model_span(dae_model, "visible runtime expression count")?;
    let capacity = checked_lower_count_add(
        dae_model.variables.inputs.len(),
        dae_model.variables.discrete_reals.len(),
        "visible runtime expression count",
        span,
    )?;
    let capacity = checked_lower_count_add(
        capacity,
        dae_model.variables.discrete_valued.len(),
        "visible runtime expression count",
        span,
    )?;
    let mut expressions =
        lower_vec_with_capacity(capacity, "visible runtime expression count", span)?;
    for (name, var) in dae_model
        .variables
        .inputs
        .iter()
        .chain(dae_model.variables.discrete_reals.iter())
        .chain(dae_model.variables.discrete_valued.iter())
    {
        let variable_expressions = visible_expressions_for_variable(name, var)?;
        reserve_lower_capacity(
            &mut expressions,
            variable_expressions.len(),
            "visible runtime expression count",
            var.source_span,
        )?;
        expressions.extend(variable_expressions);
    }
    Ok(expressions)
}

fn checked_lower_count_add(
    lhs: usize,
    rhs: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    lhs.checked_add(rhs).ok_or_else(|| {
        lower_contract_violation(format!("{context} exceeds host index range"), span)
    })
}

fn visible_expressions_for_variable(
    name: &rumoca_core::VarName,
    var: &dae::Variable,
) -> Result<Vec<VisibleExpression>, LowerError> {
    let size = variable_size(var)?;
    if size <= 1 && var.dims.is_empty() {
        let mut expressions =
            lower_vec_with_capacity(1, "visible variable expression count", var.source_span)?;
        expressions.push(visible_expression_for_variable_scalar(
            name,
            var,
            name.as_str().to_string(),
            Vec::new(),
        )?);
        return Ok(expressions);
    }
    let mut expressions =
        lower_vec_with_capacity(size, "visible variable expression count", var.source_span)?;
    for idx in 0..size {
        let subscripts = dae::flat_index_to_subscripts(&var.dims, idx).ok_or_else(|| {
            lower_contract_violation(
                format!(
                    "visible expression scalar index {idx} is outside variable `{}` shape",
                    name.as_str()
                ),
                var.source_span,
            )
        })?;
        expressions.push(visible_expression_for_variable_scalar(
            name,
            var,
            dae::scalar_name_text_for_flat_index(name.as_str(), &var.dims, idx),
            subscripts,
        )?);
    }
    Ok(expressions)
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
                        subscripts: visible_subscripts_from_usize(subscripts, var.source_span)?,
                        span: var.source_span,
                    },
                });
            }
            let component_ref =
                var.component_ref
                    .clone()
                    .ok_or_else(|| {
                        lower_contract_violation(
                            format!(
                                "source DAE variable `{}` lost structured component-reference metadata before visible observation lowering",
                                name.as_str()
                            ),
                            var.source_span,
                        )
                    })?;
            rumoca_core::Reference::from_component_reference(component_ref)
        }
    };
    Ok(VisibleExpression {
        name: scalar_name,
        expr: rumoca_core::Expression::VarRef {
            name: reference,
            subscripts: visible_subscripts_from_usize(subscripts, var.source_span)?,
            span: var.source_span,
        },
    })
}

fn visible_subscripts_from_usize(
    subscripts: Vec<usize>,
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Subscript>, LowerError> {
    let mut lowered =
        lower_vec_with_capacity(subscripts.len(), "visible variable subscript count", span)?;
    for index in subscripts {
        let value = i64::try_from(index).map_err(|_| {
            lower_contract_violation(
                "visible variable subscript exceeds Modelica integer range".to_string(),
                span,
            )
        })?;
        lowered.push(rumoca_core::Subscript::index(value, span));
    }
    Ok(lowered)
}

fn variable_size(var: &dae::Variable) -> Result<usize, LowerError> {
    var.try_size()
        .map_err(|err| lower_contract_violation(err.to_string(), err.span()))
}

fn solve_model_variable_size(var: &dae::Variable) -> Result<usize, SolveModelLowerError> {
    variable_size(var).map_err(SolveModelLowerError::Lower)
}

fn scalar_count<'a>(
    mut vars: impl Iterator<Item = &'a dae::Variable>,
) -> Result<usize, LowerError> {
    vars.try_fold(0usize, |acc, var| {
        variable_size(var).and_then(|size| {
            acc.checked_add(size).ok_or_else(|| {
                lower_contract_violation(
                    "DAE scalar count overflows usize".to_string(),
                    var.source_span,
                )
            })
        })
    })
}

fn solver_visible_scalar_count(dae_model: &dae::Dae) -> Result<usize, LowerError> {
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
    param_overrides: &HashMap<String, f64>,
) -> Result<Vec<f64>, SolveModelLowerError> {
    let mut params = Vec::new();
    let mut slots = Vec::new();
    let mut env = build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        dae_model,
        &params,
        0.0,
        runtime.clone(),
    )
    .map_err(|source| runtime_tail_error(dae_model, source))?;
    if let Some(metadata_dae_model) = metadata_dae_model {
        seed_missing_default_values_for_all_variables(metadata_dae_model, &mut env)?;
    }
    for (name, var) in &dae_model.variables.parameters {
        let offset = params.len();
        // A tunable-parameter override replaces this parameter's start value, so its
        // dependents (e.g. parameter-derived array masks `sc/nc/sig` that read `aoa`)
        // re-derive from the new value array-natively below instead of being baked at
        // the declared default. Parameters are in dependency order, so seeding the
        // override here makes every later dependent see it.
        let values = match param_overrides.get(name.as_str()) {
            Some(&value) => vec![value],
            None => start_values(dae_model, var, &env)?,
        };
        append_values_for_var(&mut params, name.as_str(), var, &values)?;
        slots.push((name, var, offset));
        seed_var_values(&mut env, name.as_str(), var, &values)?;
    }
    refine_parameter_start_values(
        dae_model,
        metadata_dae_model,
        &mut params,
        &slots,
        runtime,
        param_overrides,
    )?;
    Ok(params)
}

/// Error from propagating a tunable parameter override to its dependents.
#[derive(Debug, Clone)]
pub enum ParameterOverrideError {
    /// `dependent`'s binding references an overridden parameter but could not be
    /// const-evaluated, so the override could not be propagated. The caller
    /// rejects the override rather than running with a stale dependent value.
    UnpropagatableDependent { dependent: String },
}

impl std::fmt::Display for ParameterOverrideError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnpropagatableDependent { dependent } => write!(
                f,
                "parameter `{dependent}` depends on an overridden parameter but its binding could \
                 not be re-evaluated, so the override could not be propagated — recompile instead"
            ),
        }
    }
}

impl std::error::Error for ParameterOverrideError {}

/// Propagate tunable parameter overrides to dependent parameters in place.
///
/// The overridden (`pinned`) parameters' P-slots must already hold their new
/// values in `solve_model.parameters`. Any parameter whose binding (transitively)
/// references a pinned parameter is re-evaluated from the current values and its
/// P-slot updated — so `parameter b = 2*a; ... a` overridden re-derives `b`
/// instead of leaving it at the value folded during lowering.
///
/// Returns [`ParameterOverrideError::UnpropagatableDependent`] if such a
/// dependent's binding cannot be const-evaluated, so the caller can reject the
/// override rather than silently run with a stale value. Solver-neutral and
/// only invoked on the override path — the default lowering is unaffected.
///
/// Only scalar dependents are propagated. A dependent that is not a scalar
/// runtime parameter — e.g. an array binding `arr = {a, 2*a}`, or one using a
/// non-const-foldable construct — is reported as `UnpropagatableDependent`
/// (recompile instead) rather than re-derived.
pub fn propagate_parameter_overrides(
    dae_model: &dae::Dae,
    solve_model: &mut solve::SolveModel,
    pinned: &HashSet<String>,
) -> Result<(), ParameterOverrideError> {
    let param_names: HashSet<&str> = dae_model
        .variables
        .parameters
        .keys()
        .map(rumoca_core::VarName::as_str)
        .collect();

    // Seed the value map (parameters + folded constants) and the P-slot map from
    // the solve layout. Both borrows end before `parameters` is mutated below.
    let mut values: HashMap<String, f64> = HashMap::new();
    let mut slot_index: HashMap<String, usize> = HashMap::new();
    for (name, slot) in solve_model.problem.layout.bindings() {
        match slot {
            solve::ScalarSlot::P { index, .. } => {
                slot_index.insert(name.clone(), *index);
                if let Some(value) = solve_model.parameters.get(*index) {
                    values.insert(name.clone(), *value);
                }
            }
            solve::ScalarSlot::Constant(value) => {
                values.insert(name.clone(), *value);
            }
            _ => {}
        }
    }

    // Dependent parameters: those with a runtime P-slot and a binding that
    // references other parameters (and that are not themselves pinned).
    struct Dependent<'a> {
        name: String,
        index: usize,
        refs: Vec<String>,
        binding: &'a rumoca_core::Expression,
    }
    let mut deps: Vec<Dependent<'_>> = Vec::new();
    for (name, var) in &dae_model.variables.parameters {
        let name = name.as_str();
        if pinned.contains(name) {
            continue;
        }
        let (Some(&index), Some(binding)) = (slot_index.get(name), var.start.as_ref()) else {
            continue;
        };
        let mut refs_collected: Vec<rumoca_core::VarName> = Vec::new();
        binding.collect_var_refs(&mut refs_collected);
        let refs: Vec<String> = refs_collected
            .into_iter()
            .filter(|candidate| param_names.contains(candidate.as_str()))
            .map(|candidate| candidate.as_str().to_string())
            .collect();
        if refs.is_empty() {
            continue;
        }
        deps.push(Dependent {
            name: name.to_string(),
            index,
            refs,
            binding,
        });
    }

    // Fixpoint: re-evaluate every dependent that references a changed parameter,
    // until nothing changes. `changed` seeds from the overridden parameters.
    let mut changed: HashSet<String> = pinned.clone();
    let max_iterations = deps.len().saturating_add(1).clamp(1, 256);
    for _ in 0..max_iterations {
        let mut progressed = false;
        for dep in &deps {
            if !dep.refs.iter().any(|reference| changed.contains(reference)) {
                continue;
            }
            if reevaluate_dependent_parameter(
                dep.binding,
                &dep.name,
                dep.index,
                &mut values,
                &mut solve_model.parameters,
            )? {
                changed.insert(dep.name.clone());
                progressed = true;
            }
        }
        if !progressed {
            break;
        }
    }
    Ok(())
}

/// Re-evaluate one dependent parameter's binding against the current values,
/// writing it back to `values` and its P-slot. Returns whether the value
/// changed; errors if the binding can't be const-evaluated to a finite number.
fn reevaluate_dependent_parameter(
    binding: &rumoca_core::Expression,
    name: &str,
    index: usize,
    values: &mut HashMap<String, f64>,
    parameters: &mut [f64],
) -> Result<bool, ParameterOverrideError> {
    match eval_scalar_const_expr(binding, values) {
        Some(value) if value.is_finite() => {
            if values.get(name) == Some(&value) {
                return Ok(false);
            }
            values.insert(name.to_string(), value);
            parameters[index] = value;
            Ok(true)
        }
        _ => Err(ParameterOverrideError::UnpropagatableDependent {
            dependent: name.to_string(),
        }),
    }
}

fn refine_parameter_start_values(
    dae_model: &dae::Dae,
    metadata_dae_model: Option<&dae::Dae>,
    params: &mut [f64],
    slots: &[(&rumoca_core::VarName, &dae::Variable, usize)],
    runtime: Arc<EvalRuntimeState>,
    param_overrides: &HashMap<String, f64>,
) -> Result<(), SolveModelLowerError> {
    for _ in 0..slots.len().clamp(1, 32) {
        let mut changed = false;
        let mut env = build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
            dae_model,
            params,
            0.0,
            runtime.clone(),
        )
        .map_err(|source| runtime_tail_error(dae_model, source))?;
        if let Some(metadata_dae_model) = metadata_dae_model {
            seed_missing_default_values_for_all_variables(metadata_dae_model, &mut env)?;
        }
        for (name, var, offset) in slots {
            // Pinned overrides keep their value: the env above was rebuilt from
            // `params` (which already holds the override at this slot), so dependents
            // still read it — we just must not overwrite it with the declared start.
            if param_overrides.contains_key(name.as_str()) {
                continue;
            }
            let values = start_values(dae_model, var, &env)?;
            changed |= replace_values_for_var(params, *offset, name.as_str(), var, &values)?;
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
    let mut env = build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        dae_model, &values, 0.0, runtime,
    )
    .map_err(|source| runtime_tail_error(dae_model, source))?;
    if let Some(metadata_dae_model) = metadata_dae_model {
        seed_missing_default_values_for_all_variables(metadata_dae_model, &mut env)?;
    }
    let span = model_provenance_span(dae_model, metadata_dae_model)?;
    let runtime_parameters = runtime_parameter_variables(dae_model, span)?;
    seed_default_values(dae_model, &mut env, &runtime_parameters)?;
    for (name, var) in runtime_parameters {
        let var_values = start_values(dae_model, var, &env)?;
        append_values_for_var(&mut values, name.as_str(), var, &var_values)?;
        seed_var_values(&mut env, name.as_str(), var, &var_values)?;
    }
    resize_solve_model_values(
        &mut values,
        layout.compiled_parameter_len,
        "compiled parameter values",
        span,
    )?;
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
    let span = model_provenance_span(dae_model, metadata_dae_model)?;
    let mut values = solve_model_vec_with_capacity(solver_len, "initial solver values", span)?;
    let mut env = build_runtime_parameter_tail_env_with_declared_slots_and_runtime(
        dae_model, params, 0.0, runtime,
    )
    .map_err(|source| runtime_tail_error(dae_model, source))?;
    if let Some(metadata_dae_model) = metadata_dae_model {
        seed_missing_default_values_for_all_variables(metadata_dae_model, &mut env)?;
    }
    let solver_variables = solver_variables(dae_model, span)?;
    seed_default_values(dae_model, &mut env, &solver_variables)?;
    for (name, var) in solver_variables {
        let var_values = start_values(dae_model, var, &env)?;
        append_values_for_var(&mut values, name.as_str(), var, &var_values)?;
        seed_var_values(&mut env, name.as_str(), var, &var_values)?;
        if values.len() >= solver_len {
            values.truncate(solver_len);
            break;
        }
    }
    resize_solve_model_values(&mut values, solver_len, "initial solver values", span)?;
    Ok(values)
}

fn runtime_parameter_variables(
    dae_model: &dae::Dae,
    span: rumoca_core::Span,
) -> Result<Vec<(&rumoca_core::VarName, &dae::Variable)>, SolveModelLowerError> {
    let count = checked_solve_model_count_add(
        dae_model.variables.inputs.len(),
        dae_model.variables.discrete_reals.len(),
        "runtime parameter variable count",
        span,
    )?;
    let count = checked_solve_model_count_add(
        count,
        dae_model.variables.discrete_valued.len(),
        "runtime parameter variable count",
        span,
    )?;
    let mut variables = solve_model_vec_with_capacity(count, "runtime parameter variables", span)?;
    for entry in dae_model
        .variables
        .inputs
        .iter()
        .chain(dae_model.variables.discrete_reals.iter())
        .chain(dae_model.variables.discrete_valued.iter())
    {
        variables.push(entry);
    }
    Ok(variables)
}

fn solver_variables(
    dae_model: &dae::Dae,
    span: rumoca_core::Span,
) -> Result<Vec<(&rumoca_core::VarName, &dae::Variable)>, SolveModelLowerError> {
    let count = checked_solve_model_count_add(
        dae_model.variables.states.len(),
        dae_model.variables.algebraics.len(),
        "solver variable count",
        span,
    )?;
    let count = checked_solve_model_count_add(
        count,
        dae_model.variables.outputs.len(),
        "solver variable count",
        span,
    )?;
    let mut variables = solve_model_vec_with_capacity(count, "solver variables", span)?;
    for entry in dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
    {
        variables.push(entry);
    }
    Ok(variables)
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
    let size = solve_model_variable_size(var)?;
    let Some(expr) = var.start.as_ref() else {
        return default_start_values_for_size(var, default_start, size);
    };
    if start_expr_is_nonnumeric(expr, env) {
        return default_start_values_for_size(var, default_start, size);
    }
    if size == 0 && !var.dims.is_empty() {
        let raw = if var.dims.len() >= 2 {
            match eval_matrix_values(expr, env) {
                Ok(Some(matrix)) => flatten_start_matrix(matrix, var)?,
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
        return finite_start_values(raw, default_start, var);
    }
    if size <= 1 && var.dims.is_empty() {
        let value = eval_expr::<f64>(expr, env).map_err(|err| eval_start_error(var, err))?;
        return single_start_value(finite_start_value(value, default_start), var);
    }
    let raw = match shaped_start_values(expr, env, size) {
        Ok(values) => values,
        Err(EvalError::ShapeMismatch { actual: 1, .. }) if can_broadcast_start_value(expr, env) => {
            let value = eval_expr::<f64>(expr, env).map_err(|err| eval_start_error(var, err))?;
            expand_values_to_size(
                single_start_value(value, var)?,
                size,
                var.name.as_str(),
                expr.span().unwrap_or(var.source_span),
            )?
        }
        Err(err) => return Err(eval_start_error(var, err)),
    };
    finite_start_values(raw, default_start, var)
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
        let values = default_start_values(var, default_start)?;
        seed_var_values(env, name.as_str(), var, &values)?;
    }
    Ok(())
}

fn seed_missing_default_values_for_all_variables(
    dae_model: &dae::Dae,
    env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Result<(), SolveModelLowerError> {
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
        let size = solve_model_variable_size(var)?;
        let values = default_start_values_for_size(var, default_start, size)?;
        for (scalar_name, value) in scalar_names_for_size(name.as_str(), var, size)?
            .into_iter()
            .zip(values)
        {
            if !env.vars.contains_key(scalar_name.as_str()) {
                env.set(scalar_name.as_str(), value);
            }
        }
    }
    Ok(())
}

fn default_start_values(
    var: &dae::Variable,
    default_start: f64,
) -> Result<Vec<f64>, SolveModelLowerError> {
    let size = solve_model_variable_size(var)?;
    default_start_values_for_size(var, default_start, size)
}

fn default_start_values_for_size(
    var: &dae::Variable,
    default_start: f64,
    size: usize,
) -> Result<Vec<f64>, SolveModelLowerError> {
    if size == 0 && !var.dims.is_empty() {
        return Ok(Vec::new());
    }
    let count = size.max(1);
    let mut values = Vec::new();
    reserve_start_value_capacity(&mut values, count, var.name.as_str(), var.source_span)?;
    values.resize(count, default_start);
    Ok(values)
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
            .ok()?
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

fn model_provenance_span(
    dae_model: &dae::Dae,
    metadata_dae_model: Option<&dae::Dae>,
) -> Result<rumoca_core::Span, SolveModelLowerError> {
    match dae_model_span(dae_model, "solve-model lowering") {
        Ok(span) => Ok(span),
        Err(err) if model_has_no_provenance_owners(dae_model) => metadata_dae_model
            .map(|metadata| dae_model_span(metadata, "solve-model lowering"))
            .transpose()
            .map_err(SolveModelLowerError::Lower)?
            .ok_or(SolveModelLowerError::Lower(err)),
        Err(err) => Err(SolveModelLowerError::Lower(err)),
    }
}

fn model_has_no_provenance_owners(dae_model: &dae::Dae) -> bool {
    all_dae_variables(dae_model).next().is_none() && dae_model.continuous.equations.is_empty()
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

fn append_values_for_var(
    out: &mut Vec<f64>,
    name: &str,
    var: &dae::Variable,
    values: &[f64],
) -> Result<(), SolveModelLowerError> {
    let size = solve_model_variable_size(var)?;
    if size == 0 {
        // MLS Chapter 10 dynamic arrays may be bound by expressions whose
        // extent is known only from parameter evaluation. Their aggregate value
        // lives in the evaluation environment and must not shift flattened
        // solver/parameter slots.
        return Ok(());
    }
    let raw = start_value_vec_from_slice(values, name, var.source_span)?;
    let expanded = expand_values_to_size(raw, size, name, var.source_span)?;
    reserve_solve_model_capacity(
        out,
        expanded.len().min(size),
        "solve-model appended values",
        var.source_span,
    )?;
    out.extend(expanded.into_iter().take(size));
    Ok(())
}

fn replace_values_for_var(
    out: &mut [f64],
    offset: usize,
    name: &str,
    var: &dae::Variable,
    values: &[f64],
) -> Result<bool, SolveModelLowerError> {
    let size = solve_model_variable_size(var)?;
    if size == 0 || offset >= out.len() {
        return Ok(false);
    }
    let raw = start_value_vec_from_slice(values, name, var.source_span)?;
    let expanded = expand_values_to_size(raw, size, name, var.source_span)?;
    let end = (offset + size).min(out.len());
    Ok(out[offset..end]
        .iter_mut()
        .zip(expanded)
        .fold(false, |changed, (slot, value)| {
            replace_if_changed(slot, value) || changed
        }))
}

fn seed_var_values(
    env: &mut rumoca_eval_dae::VarEnv<f64>,
    name: &str,
    var: &dae::Variable,
    values: &[f64],
) -> Result<(), SolveModelLowerError> {
    let size = solve_model_variable_size(var)?;
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

fn flatten_start_matrix(
    matrix: Vec<Vec<f64>>,
    var: &dae::Variable,
) -> Result<Vec<f64>, SolveModelLowerError> {
    let mut len = 0usize;
    for row in &matrix {
        len = checked_solve_model_count_add(
            len,
            row.len(),
            "matrix start value count",
            var.source_span,
        )?;
    }
    let mut values = solve_model_vec_with_capacity(len, "matrix start values", var.source_span)?;
    for row in matrix {
        reserve_solve_model_capacity(
            &mut values,
            row.len(),
            "matrix start values",
            var.source_span,
        )?;
        values.extend(row);
    }
    Ok(values)
}

fn single_start_value(value: f64, var: &dae::Variable) -> Result<Vec<f64>, SolveModelLowerError> {
    let mut values = solve_model_vec_with_capacity(1, "scalar start value", var.source_span)?;
    values.push(value);
    Ok(values)
}

fn finite_start_values(
    raw: Vec<f64>,
    default_start: f64,
    var: &dae::Variable,
) -> Result<Vec<f64>, SolveModelLowerError> {
    let mut values =
        solve_model_vec_with_capacity(raw.len(), "finite start value count", var.source_span)?;
    for value in raw {
        values.push(finite_start_value(value, default_start));
    }
    Ok(values)
}

fn start_value_vec_from_slice(
    values: &[f64],
    name: &str,
    span: rumoca_core::Span,
) -> Result<Vec<f64>, SolveModelLowerError> {
    let mut raw = Vec::new();
    reserve_start_value_capacity(&mut raw, values.len(), name, span)?;
    raw.extend_from_slice(values);
    Ok(raw)
}

pub(crate) fn expand_values_to_size(
    raw: Vec<f64>,
    size: usize,
    name: &str,
    span: rumoca_core::Span,
) -> Result<Vec<f64>, SolveModelLowerError> {
    if size == 0 {
        return Ok(Vec::new());
    }
    if raw.len() == size {
        return Ok(raw);
    }
    let mut expanded = Vec::new();
    reserve_start_value_capacity(&mut expanded, size, name, span)?;
    if raw.is_empty() {
        resize_solve_model_values(&mut expanded, size, "expanded start values", span)?;
        return Ok(expanded);
    }
    if raw.len() == 1 {
        resize_start_values(&mut expanded, size, raw[0], name, span)?;
        return Ok(expanded);
    }
    let Some(last) = raw.last().copied() else {
        return Err(solve_model_contract_violation(
            format!("solve-model start value expansion for `{name}` missing tail value"),
            span,
        ));
    };
    for idx in 0..size {
        if idx < raw.len() {
            expanded.push(raw[idx]);
        } else {
            expanded.push(last);
        }
    }
    Ok(expanded)
}

fn reserve_start_value_capacity(
    values: &mut Vec<f64>,
    capacity: usize,
    name: &str,
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError> {
    values.try_reserve_exact(capacity).map_err(|_| {
        solve_model_contract_violation(
            format!("solve-model start value capacity for `{name}` overflows"),
            span,
        )
    })
}

fn resize_start_values(
    values: &mut Vec<f64>,
    len: usize,
    value: f64,
    name: &str,
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError> {
    reserve_start_value_capacity(values, len.saturating_sub(values.len()), name, span)?;
    values.resize(len, value);
    Ok(())
}

fn resize_solve_model_values(
    values: &mut Vec<f64>,
    len: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError> {
    reserve_solve_model_capacity(values, len.saturating_sub(values.len()), context, span)?;
    values.resize(len, 0.0);
    Ok(())
}

fn solve_model_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, SolveModelLowerError> {
    let mut values = Vec::new();
    reserve_solve_model_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn reserve_solve_model_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        solve_model_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn reserve_solve_model_index_map_capacity<K, V>(
    values: &mut IndexMap<K, V>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), SolveModelLowerError>
where
    K: std::hash::Hash + Eq,
{
    values.try_reserve(additional).map_err(|_| {
        solve_model_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn checked_solve_model_count_add(
    lhs: usize,
    rhs: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, SolveModelLowerError> {
    lhs.checked_add(rhs).ok_or_else(|| {
        solve_model_contract_violation(format!("{context} overflows host index range"), span)
    })
}

fn state_identity_mass_matrix(
    state_count: usize,
    span: rumoca_core::Span,
) -> Result<Vec<Vec<f64>>, SolveModelLowerError> {
    // The solve-IR derivative RHS rows already solve the MLS §8.3 equation
    // system for der(state), including coupled derivative rows. Concrete
    // solvers therefore receive x' = f(x, p, t) for state rows.
    let mut mass = solve_model_vec_with_capacity(state_count, "identity mass matrix rows", span)?;
    for idx in 0..state_count {
        let mut row = solve_model_vec_with_capacity(state_count, "identity mass matrix row", span)?;
        resize_solve_model_values(&mut row, state_count, "identity mass matrix row", span)?;
        row[idx] = 1.0;
        mass.push(row);
    }
    Ok(mass)
}

pub(crate) fn scalar_names(
    name: &str,
    var: &dae::Variable,
) -> Result<Vec<String>, SolveModelLowerError> {
    let size = solve_model_variable_size(var)?;
    scalar_names_for_size(name, var, size)
}

fn scalar_names_for_size(
    name: &str,
    var: &dae::Variable,
    size: usize,
) -> Result<Vec<String>, SolveModelLowerError> {
    if size <= 1 && var.dims.is_empty() {
        let mut names = solve_model_vec_with_capacity(1, "scalar name count", var.source_span)?;
        names.push(name.to_string());
        return Ok(names);
    }
    let mut names = solve_model_vec_with_capacity(size, "scalar name count", var.source_span)?;
    for idx in 0..size {
        names.push(dae::scalar_name_text_for_flat_index(name, &var.dims, idx));
    }
    Ok(names)
}

fn derivative_coefficient_expr(
    expr: &rumoca_core::Expression,
    state_name: &str,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, String> {
    let span = coefficient_expr_span(expr, owner_span)?;
    match expr {
        rumoca_core::Expression::BuiltinCall { function, args, .. }
            if *function == rumoca_core::BuiltinFunction::Der =>
        {
            Ok(if der_arg_matches(args, state_name)? {
                real_expr(1.0, span)
            } else {
                real_expr(0.0, span)
            })
        }
        rumoca_core::Expression::Unary {
            op: OpUnary::Minus | OpUnary::DotMinus,
            rhs,
            ..
        } => Ok(neg_expr(
            derivative_coefficient_expr(rhs, state_name, span)?,
            span,
        )),
        rumoca_core::Expression::Binary {
            op: OpBinary::Add | OpBinary::AddElem,
            lhs,
            rhs,
            ..
        } => Ok(binary_expr_with_span(
            add_op(),
            derivative_coefficient_expr(lhs, state_name, span)?,
            derivative_coefficient_expr(rhs, state_name, span)?,
            span,
        )),
        rumoca_core::Expression::Binary {
            op: OpBinary::Sub | OpBinary::SubElem,
            lhs,
            rhs,
            ..
        } => Ok(binary_expr_with_span(
            sub_op(),
            derivative_coefficient_expr(lhs, state_name, span)?,
            derivative_coefficient_expr(rhs, state_name, span)?,
            span,
        )),
        rumoca_core::Expression::Binary {
            op: OpBinary::Mul | OpBinary::MulElem,
            lhs,
            rhs,
            ..
        } => coefficient_product(lhs, rhs, state_name, span),
        rumoca_core::Expression::Binary {
            op: OpBinary::Div | OpBinary::DivElem,
            lhs,
            rhs,
            ..
        } => {
            if rhs.contains_der() {
                return Err("derivative appears in denominator".to_string());
            }
            Ok(binary_expr_with_span(
                div_op(),
                derivative_coefficient_expr(lhs, state_name, span)?,
                rhs.as_ref().clone(),
                span,
            ))
        }
        _ if expr.contains_der() => Err("unsupported derivative expression shape".to_string()),
        _ => Ok(real_expr(0.0, span)),
    }
}

fn coefficient_product(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    state_name: &str,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, String> {
    let lhs_has_der = lhs.contains_der();
    let rhs_has_der = rhs.contains_der();
    match (lhs_has_der, rhs_has_der) {
        (true, true) => Err("nonlinear derivative product".to_string()),
        (true, false) => Ok(binary_expr_with_span(
            mul_op(),
            derivative_coefficient_expr(lhs, state_name, owner_span)?,
            rhs.clone(),
            owner_span,
        )),
        (false, true) => Ok(binary_expr_with_span(
            mul_op(),
            lhs.clone(),
            derivative_coefficient_expr(rhs, state_name, owner_span)?,
            owner_span,
        )),
        (false, false) => Ok(real_expr(0.0, owner_span)),
    }
}

fn coefficient_expr_span(
    expr: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, String> {
    expr.span()
        .or_else(|| (!owner_span.is_dummy()).then_some(owner_span))
        .ok_or_else(|| "derivative coefficient expression has no source provenance".to_string())
}

fn der_arg_matches(args: &[rumoca_core::Expression], state_name: &str) -> Result<bool, String> {
    let Some(rumoca_core::Expression::VarRef {
        name, subscripts, ..
    }) = args.first()
    else {
        return Ok(false);
    };
    if subscripts.is_empty() {
        return Ok(name.as_str() == state_name);
    }
    let mut indices = Vec::new();
    indices.try_reserve_exact(subscripts.len()).map_err(|_| {
        "derivative subscript index text count exceeds host memory limits".to_string()
    })?;
    for sub in subscripts {
        let Some(text) = subscript_index_text(sub) else {
            return Ok(false);
        };
        indices.push(text);
    }
    Ok(format!("{}[{}]", name.as_str(), indices.join(",")) == state_name)
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

fn real_expr(value: f64, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn neg_expr(expr: rumoca_core::Expression, span: rumoca_core::Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(expr),
        span,
    }
}

#[cfg(test)]
fn binary_expr(
    op: OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
) -> rumoca_core::Expression {
    let Some(span) = lhs.span().or_else(|| rhs.span()) else {
        unreachable!("test binary expression operands should carry source provenance");
    };
    binary_expr_with_span(op, lhs, rhs, span)
}

fn binary_expr_with_span(
    op: OpBinary,
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
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
) -> Result<Vec<solve::SolveVariableMeta>, SolveModelLowerError> {
    let event_discontinuous_names = event_discontinuous_scalar_names(dae_model)?;
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
        );
    let mut by_scalar = IndexMap::new();
    for (name, var, role, is_state) in vars {
        let (value_type, variability, time_domain) = variable_meta_classification(role, is_state);
        let start_text = var.start.as_ref().map(|expr| format!("{expr:?}"));
        let min_text = var.min.as_ref().map(|expr| format!("{expr:?}"));
        let max_text = var.max.as_ref().map(|expr| format!("{expr:?}"));
        let nominal_text = var.nominal.as_ref().map(|expr| format!("{expr:?}"));
        let scalar_names = scalar_names(name.as_str(), var)?;
        reserve_solve_model_index_map_capacity(
            &mut by_scalar,
            scalar_names.len(),
            "variable metadata scalar count",
            var.source_span,
        )?;
        for scalar_name in scalar_names {
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
    let mut meta = solve_model_vec_with_capacity(
        visible_names.len(),
        "visible variable metadata count",
        dae_model_span(dae_model, "visible variable metadata count")
            .map_err(SolveModelLowerError::Lower)?,
    )?;
    for name in visible_names {
        if let Some(variable_meta) = by_scalar.get(name) {
            meta.push(variable_meta.clone());
        }
    }
    Ok(meta)
}

#[cfg(test)]
mod tests;
