use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::{lower, lower::LowerError, stencil};

pub(crate) fn build_residual_compute_block(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    residual_rows: &[Vec<solve::LinearOp>],
    residual_targets: &[Option<solve::ScalarSlot>],
    residual_equations: &[(usize, &dae::Equation)],
) -> Result<solve::ComputeBlock, LowerError> {
    let span = residual_context_span(dae_model, residual_equations);
    validate_residual_compute_block_contract(
        residual_rows.len(),
        residual_targets.len(),
        residual_equations,
        span,
    )?;
    let mut rows = residual_vec_with_capacity(
        residual_rows.len(),
        "residual structured program row count",
        span,
    )?;
    let y_slot_ranges = stencil::structured_y_slot_ranges(layout)?;
    let structural_bindings = lower::structural_bindings_for_structured_access(dae_model)?;
    let mut residual_index = 0usize;
    for (equation_index, equation) in residual_equations {
        let scalar_count = equation.scalar_count.max(1);
        for row_offset in 0..scalar_count {
            let Some(ops) = residual_rows.get(residual_index).cloned() else {
                break;
            };
            let target = residual_targets.get(residual_index).copied().flatten();
            let pointwise_output_y_index = target_y_index(target);
            rows.push(stencil::StructuredProgram {
                load_y_ranges: stencil::structured_load_y_ranges(
                    &ops,
                    &y_slot_ranges,
                    equation.span,
                )?,
                ops,
                output_index: residual_index,
                pointwise_output_y_index,
                span: equation.span,
                output_y_range: residual_output_y_range(
                    target,
                    &y_slot_ranges,
                    residual_index,
                    equation.span,
                )?,
                dae_equation_index: Some(*equation_index),
                access_proof: residual_row_access_proof(
                    layout,
                    &structural_bindings,
                    &equation.rhs,
                    equation.span,
                    row_offset,
                    scalar_count,
                )?,
            });
            residual_index += 1;
        }
    }
    let mut block = solve::ComputeBlock::default();
    stencil::push_structured_programs(
        &mut block.nodes,
        &mut rows,
        &dae_model.continuous.structured_equations,
        &dae_model.continuous.equations,
    )?;
    Ok(block)
}

fn validate_residual_compute_block_contract(
    residual_row_count: usize,
    residual_target_count: usize,
    residual_equations: &[(usize, &dae::Equation)],
    fallback_span: Option<rumoca_core::Span>,
) -> Result<(), LowerError> {
    if residual_target_count != residual_row_count {
        return Err(residual_contract_error(
            format!(
                "residual target count {residual_target_count} does not match residual row count {residual_row_count}"
            ),
            residual_contract_error_span(residual_target_count, residual_equations)
                .or(fallback_span),
        ));
    }
    let expected_rows = residual_equation_scalar_count(residual_equations)?;
    if expected_rows != residual_row_count {
        return Err(residual_contract_error(
            format!(
                "residual equation scalar count {expected_rows} does not match residual row count {residual_row_count}"
            ),
            residual_contract_error_span(residual_row_count, residual_equations).or(fallback_span),
        ));
    }
    Ok(())
}

fn residual_contract_error(reason: String, span: Option<rumoca_core::Span>) -> LowerError {
    match span {
        Some(span) if !span.is_dummy() => LowerError::ContractViolation { reason, span },
        Some(_) | None => LowerError::UnspannedContractViolation { reason },
    }
}

fn residual_equation_scalar_count(
    residual_equations: &[(usize, &dae::Equation)],
) -> Result<usize, LowerError> {
    residual_equations
        .iter()
        .try_fold(0usize, |total, (_, equation)| {
            total
                .checked_add(equation.scalar_count.max(1))
                .ok_or_else(|| {
                    residual_contract_error(
                        "residual equation scalar count overflows usize".to_string(),
                        Some(equation.span),
                    )
                })
        })
}

fn residual_contract_error_span(
    row_count: usize,
    residual_equations: &[(usize, &dae::Equation)],
) -> Option<rumoca_core::Span> {
    let mut row_start = 0usize;
    for (_, equation) in residual_equations {
        let row_end = row_start.checked_add(equation.scalar_count.max(1))?;
        if row_count < row_end {
            return Some(equation.span);
        }
        row_start = row_end;
    }
    residual_equations.last().map(|(_, equation)| equation.span)
}

fn residual_context_span(
    dae_model: &dae::Dae,
    residual_equations: &[(usize, &dae::Equation)],
) -> Option<rumoca_core::Span> {
    residual_equations
        .first()
        .map(|(_, equation)| equation.span)
        .or_else(|| {
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
        })
}

fn residual_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    values.try_reserve_exact(capacity).map_err(|_| {
        residual_contract_error(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

fn target_y_index(target: Option<solve::ScalarSlot>) -> Option<usize> {
    match target {
        Some(solve::ScalarSlot::Y { index, .. }) => Some(index),
        _ => None,
    }
}

fn residual_output_y_range(
    target: Option<solve::ScalarSlot>,
    y_slot_ranges: &IndexMap<usize, std::ops::Range<usize>>,
    fallback_index: usize,
    span: rumoca_core::Span,
) -> Result<std::ops::Range<usize>, LowerError> {
    let Some(index) = target_y_index(target) else {
        return checked_singleton_range(fallback_index, "residual fallback output", span);
    };
    if let Some(range) = y_slot_ranges.get(&index) {
        return Ok(range.clone());
    }
    checked_singleton_range(index, "residual target y output", span)
}

fn checked_singleton_range(
    index: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<std::ops::Range<usize>, LowerError> {
    let end = index.checked_add(1).ok_or_else(|| {
        residual_contract_error(
            format!("{context} index {index} overflows output range"),
            Some(span),
        )
    })?;
    Ok(index..end)
}

fn residual_row_access_proof(
    layout: &solve::VarLayout,
    structural_bindings: &IndexMap<String, f64>,
    expression: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
    row_offset: usize,
    scalar_count: usize,
) -> Result<Option<stencil::StructuredAccessProof>, LowerError> {
    let Some(expression) = residual_row_access_expression(expression, row_offset, scalar_count)
    else {
        return Ok(None);
    };
    let mut builder = stencil::StructuredAccessProofBuilder::new();
    let Some(access_span) = expression
        .span()
        .filter(|span| !span.is_dummy())
        .or_else(|| (!owner_span.is_dummy()).then_some(owner_span))
    else {
        return Ok(None);
    };
    let Some(()) =
        builder.collect_expression_result(&expression, |base, subscripts, span, operands| {
            let span = if span.is_dummy() { access_span } else { span };
            collect_residual_var_ref_access_operands(
                base,
                subscripts,
                layout,
                structural_bindings,
                span,
                operands,
            )
        })?
    else {
        return Ok(None);
    };
    Ok(Some(builder.finish()))
}

fn residual_row_access_expression(
    expression: &rumoca_core::Expression,
    row_offset: usize,
    scalar_count: usize,
) -> Option<rumoca_core::Expression> {
    if scalar_count <= 1 {
        return Some(expression.clone());
    }
    match expression {
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => elements.get(row_offset).cloned(),
        _ => None,
    }
}

fn collect_residual_var_ref_access_operands(
    base: &str,
    subscripts: &[rumoca_core::Subscript],
    layout: &solve::VarLayout,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
    operands: &mut Vec<stencil::StructuredAccessOperand>,
) -> Result<Option<()>, LowerError> {
    if subscripts
        .iter()
        .any(|subscript| matches!(subscript, rumoca_core::Subscript::Colon { .. }))
    {
        return Ok(None);
    }
    let indices = lower::compile_time_subscript_indices_for_structured_access(
        subscripts,
        structural_bindings,
        owner_span,
    )?;
    let key = if indices.is_empty() {
        base.to_string()
    } else {
        dae::format_subscript_key(base, &indices)
    };
    let Some(slot) = layout.binding(&key) else {
        return Ok(None);
    };
    let Some(operand) = stencil::structured_access_operand_for_slot(slot) else {
        return Ok(None);
    };
    operands.push(operand);
    Ok(Some(()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn literal_zero(span: rumoca_core::Span) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span,
        }
    }

    fn unspanned_residual_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    #[test]
    fn residual_compute_block_contract_reports_target_mismatch_with_equation_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("target_mismatch.mo"),
            7,
            19,
        );
        let equation = dae::Equation::residual_array(literal_zero(span), span, "eq", 2);
        let err = validate_residual_compute_block_contract(2, 1, &[(0, &equation)], Some(span))
            .expect_err("target count mismatch should fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.to_string().contains("residual target count"),
            "error should explain target mismatch: {err}"
        );
    }

    #[test]
    fn residual_compute_block_contract_reports_row_mismatch_with_equation_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("row_mismatch.mo"),
            11,
            23,
        );
        let equation = dae::Equation::residual_array(literal_zero(span), span, "eq", 2);
        let err = validate_residual_compute_block_contract(1, 1, &[(0, &equation)], Some(span))
            .expect_err("row count mismatch should fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.to_string().contains("residual equation scalar count"),
            "error should explain row mismatch: {err}"
        );
    }

    #[test]
    fn residual_compute_block_contract_reports_missing_row_with_next_equation_span() {
        let first_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("boundary_mismatch.mo"),
            2,
            6,
        );
        let second_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("boundary_mismatch.mo"),
            9,
            16,
        );
        let first = dae::Equation::residual_array(literal_zero(first_span), first_span, "eq1", 1);
        let second =
            dae::Equation::residual_array(literal_zero(second_span), second_span, "eq2", 1);
        let err = validate_residual_compute_block_contract(
            1,
            1,
            &[(0, &first), (1, &second)],
            Some(first_span),
        )
        .expect_err("missing second row should fail");

        assert_eq!(err.source_span(), Some(second_span));
    }

    #[test]
    fn residual_compute_block_contract_does_not_fabricate_span_without_context() {
        let err = validate_residual_compute_block_contract(1, 0, &[], None)
            .expect_err("unmatched residual rows without provenance should fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason().contains("residual target count"),
            "error should explain target mismatch: {err}"
        );
    }

    #[test]
    fn residual_equation_scalar_count_does_not_fabricate_dummy_span() {
        let first = dae::Equation::residual_array(
            literal_zero(unspanned_residual_test_span()),
            unspanned_residual_test_span(),
            "first",
            usize::MAX,
        );
        let second = dae::Equation::residual_array(
            literal_zero(unspanned_residual_test_span()),
            unspanned_residual_test_span(),
            "second",
            1,
        );

        let err = residual_equation_scalar_count(&[(0, &first), (1, &second)])
            .expect_err("oversized residual scalar count should fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("residual equation scalar count overflows usize"),
            "error should explain residual scalar-count overflow: {err}"
        );
    }

    #[test]
    fn residual_output_y_range_uses_checked_scalar_fallback() -> Result<(), LowerError> {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("residual_output_range.mo"),
            1,
            5,
        );
        let range = residual_output_y_range(None, &IndexMap::new(), 7, span)?;

        assert_eq!(range, 7..8);
        Ok(())
    }

    #[test]
    fn residual_output_y_range_reports_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("bad_residual_output.mo"),
            4,
            12,
        );
        let err = residual_output_y_range(None, &IndexMap::new(), usize::MAX, span)
            .expect_err("overflowing residual output range should fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.to_string().contains("residual fallback output index"),
            "error should explain residual output overflow: {err}"
        );
    }

    #[test]
    fn residual_output_y_range_does_not_fabricate_dummy_span() {
        let err = residual_output_y_range(
            None,
            &IndexMap::new(),
            usize::MAX,
            unspanned_residual_test_span(),
        )
        .expect_err("overflowing residual output range should fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason().contains("residual fallback output index"),
            "error should explain residual output overflow: {err}"
        );
    }
}
