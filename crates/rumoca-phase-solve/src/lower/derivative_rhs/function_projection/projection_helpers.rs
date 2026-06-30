use super::*;
use rumoca_core::FallibleExpressionVisitor;

pub(super) struct ProjectionValueCtx<'a> {
    pub(super) dims: &'a [i64],
    pub(super) flat_index: usize,
    pub(super) scope: &'a FunctionProjectionScope,
    pub(super) depth: usize,
    pub(super) span: rumoca_core::Span,
}

pub(super) struct ArrayProjectionValueCtx<'a> {
    pub(super) elements: &'a [rumoca_core::Expression],
    pub(super) child_dims: &'a [Option<Vec<i64>>],
    pub(super) projection: &'a ProjectionValueCtx<'a>,
}

pub(super) fn projection_value_ctx<'a>(
    dims: &'a [i64],
    flat_index: usize,
    scope: &'a FunctionProjectionScope,
    depth: usize,
    span: rumoca_core::Span,
) -> ProjectionValueCtx<'a> {
    ProjectionValueCtx {
        dims,
        flat_index,
        scope,
        depth,
        span,
    }
}

pub(super) struct MatrixVectorProductDims<'a> {
    pub(super) lhs_dims: &'a [i64],
    pub(super) rhs_dims: &'a [i64],
    pub(super) rows: i64,
    pub(super) cols: i64,
}

pub(super) struct ProjectionAssignmentTarget {
    pub(super) base: String,
    pub(super) indices: Option<Vec<i64>>,
    pub(super) span: rumoca_core::Span,
}

pub(super) struct IndexedAssignment<'a> {
    pub(super) target: &'a str,
    pub(super) indices: &'a [i64],
    pub(super) value: &'a rumoca_core::Expression,
    pub(super) span: rumoca_core::Span,
    pub(super) depth: usize,
}

pub(super) struct IfStatementProjection<'a> {
    pub(super) cond_blocks: &'a [rumoca_core::StatementBlock],
    pub(super) else_block: &'a Option<Vec<rumoca_core::Statement>>,
    pub(super) span: rumoca_core::Span,
    pub(super) depth: usize,
}

pub(super) struct ScalarSelectionCtx<'a> {
    pub(super) name: &'a str,
    pub(super) subscripts: &'a [rumoca_core::Subscript],
    pub(super) dims: &'a [i64],
    pub(super) values: &'a [rumoca_core::Expression],
    pub(super) span: rumoca_core::Span,
    pub(super) depth: usize,
}

struct FunctionScopeRefChecker<'a> {
    scoped_names: &'a [&'a str],
}

impl FallibleExpressionVisitor for FunctionScopeRefChecker<'_> {
    type Error = ();

    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) -> Result<(), Self::Error> {
        if self
            .scoped_names
            .iter()
            .any(|scoped| reference_uses_function_scoped_root(name.as_str(), scoped))
        {
            return Err(());
        }
        for subscript in subscripts {
            self.visit_subscript(subscript)?;
        }
        Ok(())
    }
}

fn reference_uses_function_scoped_root(reference: &str, scoped: &str) -> bool {
    reference == scoped
        || reference
            .strip_prefix(scoped)
            .is_some_and(|suffix| suffix.starts_with('.') || suffix.starts_with('['))
}

pub(super) fn outputs_contain_unresolved_function_scope_refs(
    function: &rumoca_core::Function,
    outputs: Option<&[ProjectedFunctionOutput]>,
) -> bool {
    let Some(outputs) = outputs else {
        return false;
    };
    let scoped_names = function
        .outputs
        .iter()
        .chain(function.locals.iter())
        .map(|param| param.name.as_str())
        .collect::<Vec<_>>();
    outputs.iter().any(|output| {
        FunctionScopeRefChecker {
            scoped_names: &scoped_names,
        }
        .visit_expression(&output.expr)
        .is_err()
    })
}

pub(super) fn projection_arg_or_context_span(
    expr: &rumoca_core::Expression,
    context_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span()
        .or_else(|| (!context_span.is_dummy()).then_some(context_span))
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: "missing source provenance for function projection argument".to_string(),
        })
}

pub(super) fn projection_actual_with_span(
    actual: &rumoca_core::Expression,
    input_span: rumoca_core::Span,
) -> Result<(&rumoca_core::Expression, rumoca_core::Span), LowerError> {
    projection_arg_or_context_span(actual, input_span).map(|actual_span| (actual, actual_span))
}

pub(super) fn inherited_projection_span(
    span: rumoca_core::Span,
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    if span.is_dummy() { owner_span } else { span }
}

pub(super) fn inherited_projection_source_span(
    span: Option<rumoca_core::Span>,
    owner_span: rumoca_core::Span,
) -> rumoca_core::Span {
    span.map(|span| inherited_projection_span(span, owner_span))
        .unwrap_or(owner_span)
}

pub(super) fn array_element_scalar_width(
    dims: Option<&[i64]>,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    match dims {
        Some(dims) if !dims.is_empty() => {
            scalar_count_for_dims(dims, "array expression element dimensions", span)
        }
        _ => Ok(1),
    }
}

pub(super) fn matrix_elements_are_row_literals(elements: &[rumoca_core::Expression]) -> bool {
    let Some(first) = elements.first() else {
        return false;
    };
    matches!(
        first,
        rumoca_core::Expression::Array { .. } | rumoca_core::Expression::Tuple { .. }
    ) && elements.iter().all(|element| {
        matches!(
            element,
            rumoca_core::Expression::Array { .. } | rumoca_core::Expression::Tuple { .. }
        )
    })
}

pub(super) fn matrix_column_operand_count(
    dims: Option<&[i64]>,
    rows: usize,
    span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    match dims {
        Some([]) if rows == 1 => Ok(Some(1)),
        Some([candidate_rows]) => {
            let candidate_rows =
                valid_product_dim(*candidate_rows, span, "matrix column operand row count")?;
            Ok((candidate_rows == rows).then_some(1))
        }
        Some([candidate_rows, candidate_cols]) => {
            let candidate_rows =
                valid_product_dim(*candidate_rows, span, "matrix column operand row count")?;
            if candidate_rows != rows {
                return Ok(None);
            }
            valid_product_dim(*candidate_cols, span, "matrix column operand column count").map(Some)
        }
        _ => Ok(None),
    }
}

pub(super) fn matrix_column_child_flat_index(
    dims: Option<&[i64]>,
    row: usize,
    local_col: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    match dims {
        Some([]) if local_col == 0 => Ok(0),
        Some([_]) if local_col == 0 => Ok(row),
        Some([_, cols]) => {
            let cols = valid_product_dim(*cols, span, "matrix column operand column count")?;
            checked_projection_offset(
                row,
                cols,
                local_col,
                "matrix column operand flat index",
                span,
            )
        }
        _ => Err(LowerError::contract_violation(
            "matrix column operand has inconsistent projected column index",
            span,
        )),
    }
}
