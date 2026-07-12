use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::LowerError;

use super::{
    SolveModelLowerError, dae_model_span, lower_contract_violation, solver_visible_scalar_count,
    variable_size,
};

#[derive(Clone, Debug, PartialEq)]
pub struct VisibleExpression {
    pub name: String,
    pub expr: rumoca_core::Expression,
}

pub(super) fn lower_visible_observations(
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
        if let Some(row) = direct_identity_observation_row(layout, visible)? {
            names.push(visible.name.clone());
            program_spans.push(visible_expression_span(visible)?);
            rows.push(row);
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
            Err(err) if should_skip_unbound_observation(layout, &err) => {}
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

fn direct_identity_observation_row(
    layout: &solve::VarLayout,
    visible: &VisibleExpression,
) -> Result<Option<Vec<solve::LinearOp>>, LowerError> {
    if !visible_expression_is_identity(visible) {
        return Ok(None);
    }
    let Some(slot) = layout.binding(&visible.name) else {
        return Ok(None);
    };
    let load = match slot {
        solve::ScalarSlot::Time => solve::LinearOp::LoadTime { dst: 0 },
        solve::ScalarSlot::Y { index, .. } => solve::LinearOp::LoadY { dst: 0, index },
        solve::ScalarSlot::P { index, .. } => solve::LinearOp::LoadP { dst: 0, index },
        solve::ScalarSlot::Constant(value) => solve::LinearOp::Const { dst: 0, value },
    };
    let span = visible.expr.span();
    let mut row = lower_vec_with_capacity(
        2,
        "direct visible identity row operation count",
        span.ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: format!(
                "visible observation `{}` reached direct lowering without provenance",
                visible.name
            ),
        })?,
    )?;
    row.push(load);
    row.push(solve::LinearOp::StoreOutput { src: 0 });
    Ok(Some(row))
}

fn visible_expression_is_identity(visible: &VisibleExpression) -> bool {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = &visible.expr
    else {
        return false;
    };
    if subscripts.is_empty() {
        return name.as_str() == visible.name;
    }
    let Some(indices) = subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } => usize::try_from(*value).ok(),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()
    else {
        return false;
    };
    dae::format_subscript_key(name.as_str(), &indices) == visible.name
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

fn should_skip_unbound_observation(layout: &solve::VarLayout, err: &LowerError) -> bool {
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
            let component_ref = var.component_ref.clone().ok_or_else(|| {
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

pub(super) fn visible_subscripts_from_usize(
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

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;

    use super::*;

    #[test]
    fn direct_identity_observation_loads_layout_slot_without_expression_lowering() {
        let layout = solve::VarLayout::from_parts(
            IndexMap::from([(
                "x".to_string(),
                solve::ScalarSlot::Y {
                    index: 3,
                    byte_offset: 3 * std::mem::size_of::<f64>(),
                },
            )]),
            4,
            0,
        );
        let span =
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2);
        let visible = VisibleExpression {
            name: "x".to_string(),
            expr: rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::generated("x"),
                subscripts: Vec::new(),
                span,
            },
        };

        let row = direct_identity_observation_row(&layout, &visible)
            .expect("direct observation should lower")
            .expect("identity observation should take the direct path");

        assert_eq!(
            row,
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 3 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]
        );
    }
}
