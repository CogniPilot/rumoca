use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};

use super::{ScalarizeError, scalarize_vec_with_capacity_optional};

pub(crate) fn scalarize_affine_rows(
    domain: &rumoca_core::StructuredIndexDomain,
    base_ops: &[LinearOp],
    load_strides: &[rumoca_ir_solve::AffineStencilLoadStride],
    const_strides: &[rumoca_ir_solve::AffineStencilConstStride],
    span: rumoca_core::Span,
) -> Result<Vec<Vec<LinearOp>>, ScalarizeError> {
    scalarize_affine_rows_with_span(
        domain,
        base_ops,
        load_strides,
        const_strides,
        "affine stencil",
        span,
    )
}

pub(super) fn scalarize_affine_rows_with_span(
    domain: &rumoca_core::StructuredIndexDomain,
    base_ops: &[LinearOp],
    load_strides: &[rumoca_ir_solve::AffineStencilLoadStride],
    const_strides: &[rumoca_ir_solve::AffineStencilConstStride],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<Vec<LinearOp>>, ScalarizeError> {
    validate_affine_stride_metadata(domain, base_ops, load_strides, const_strides, kind, span)?;
    let index_tuples = domain
        .index_tuples()
        .map_err(|err| ScalarizeError::ShapeContract {
            message: format!("structured index domain is invalid: {err}"),
            span: Some(span),
        })?;
    let Some(base_tuple) = index_tuples.first() else {
        return Ok(Vec::new());
    };
    let mut rows = scalarize_vec_with_capacity(index_tuples.len(), kind, span)?;
    for index_tuple in &index_tuples {
        let mut ops = cloned_linear_ops(base_ops, kind, span)?;
        for stride in load_strides {
            apply_affine_load_stride(
                &mut ops,
                stride,
                domain,
                base_tuple,
                index_tuple,
                kind,
                span,
            )?;
        }
        for stride in const_strides {
            apply_affine_const_stride(
                &mut ops,
                stride,
                domain,
                base_tuple,
                index_tuple,
                kind,
                span,
            )?;
        }
        rows.push(ops);
    }
    Ok(rows)
}

/// Expand a tensor output map into concrete scalar output slots.
pub fn tensor_output_indices(
    domain: &rumoca_core::StructuredIndexDomain,
    output_map: &rumoca_ir_solve::TensorOutputMap,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, ScalarizeError> {
    output_map
        .output_indices(domain)
        .map_err(|err| tensor_output_map_error(err, kind, span))
}

/// Fallible output-count helper for production paths that must not clamp invalid metadata.
pub fn checked_tensor_output_count(
    indices: &[usize],
    fallback: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, ScalarizeError> {
    indices.iter().copied().max().map_or(Ok(fallback), |index| {
        index
            .checked_add(1)
            .ok_or(ScalarizeError::OutputCountOverflow { kind, index, span })
    })
}

pub fn scalar_program_output_indices(
    block: &ScalarProgramBlock,
    output_cursor: usize,
    kind: &'static str,
) -> Result<Vec<usize>, ScalarizeError> {
    let span = block_span(block);
    if !block.uses_local_contiguous_output_indices() {
        let mut indices =
            scalarize_vec_with_capacity_optional(block.output_indices.len(), kind, span)?;
        indices.extend_from_slice(&block.output_indices);
        return Ok(indices);
    }
    let stored_outputs = block.stored_output_count();
    let end = checked_contiguous_output_count_optional(output_cursor, stored_outputs, kind, span)?;
    let mut indices = scalarize_vec_with_capacity_optional(stored_outputs, kind, span)?;
    indices.extend(output_cursor..end);
    Ok(indices)
}

pub fn scalar_program_output_count(
    block: &ScalarProgramBlock,
    output_cursor: usize,
    kind: &'static str,
) -> Result<usize, ScalarizeError> {
    let indices = scalar_program_output_indices(block, output_cursor, kind)?;
    checked_tensor_output_count_optional(&indices, output_cursor, kind, block_span(block))
}

fn validate_affine_stride_metadata(
    domain: &rumoca_core::StructuredIndexDomain,
    base_ops: &[LinearOp],
    load_strides: &[rumoca_ir_solve::AffineStencilLoadStride],
    const_strides: &[rumoca_ir_solve::AffineStencilConstStride],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<(), ScalarizeError> {
    for stride in load_strides {
        validate_stride_terms(domain, &stride.terms, kind, span)?;
        match base_ops.get(stride.op_position) {
            Some(LinearOp::LoadY { .. } | LinearOp::LoadP { .. }) => {}
            Some(op) => {
                return Err(affine_stride_error(
                    kind,
                    stride.op_position,
                    base_ops.len(),
                    "LoadY or LoadP",
                    Some(linear_op_name(op)),
                    span,
                ));
            }
            None => {
                return Err(affine_stride_error(
                    kind,
                    stride.op_position,
                    base_ops.len(),
                    "LoadY or LoadP",
                    None,
                    span,
                ));
            }
        }
    }
    for stride in const_strides {
        validate_stride_terms(domain, &stride.terms, kind, span)?;
        match base_ops.get(stride.op_position) {
            Some(LinearOp::Const { .. }) => {}
            Some(op) => {
                return Err(affine_stride_error(
                    kind,
                    stride.op_position,
                    base_ops.len(),
                    "Const",
                    Some(linear_op_name(op)),
                    span,
                ));
            }
            None => {
                return Err(affine_stride_error(
                    kind,
                    stride.op_position,
                    base_ops.len(),
                    "Const",
                    None,
                    span,
                ));
            }
        }
    }
    Ok(())
}

fn validate_stride_terms<T>(
    domain: &rumoca_core::StructuredIndexDomain,
    terms: &[T],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<(), ScalarizeError>
where
    T: StrideTermDimension,
{
    for term in terms {
        if term.dimension() >= domain.binders.len() {
            return Err(ScalarizeError::InvalidStrideDimension {
                kind,
                dimension: term.dimension(),
                dimension_count: domain.binders.len(),
                span,
            });
        }
    }
    Ok(())
}

trait StrideTermDimension {
    fn dimension(&self) -> usize;
}

impl StrideTermDimension for rumoca_ir_solve::AffineStencilIndexStrideTerm {
    fn dimension(&self) -> usize {
        self.dimension
    }
}

impl StrideTermDimension for rumoca_ir_solve::AffineStencilConstStrideTerm {
    fn dimension(&self) -> usize {
        self.dimension
    }
}

fn apply_affine_load_stride(
    ops: &mut [LinearOp],
    stride: &rumoca_ir_solve::AffineStencilLoadStride,
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    index_tuple: &[i64],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<(), ScalarizeError> {
    let op_count = ops.len();
    match ops.get_mut(stride.op_position) {
        Some(LinearOp::LoadY { index, .. }) | Some(LinearOp::LoadP { index, .. }) => {
            *index = apply_index_terms(
                *index,
                &stride.terms,
                domain,
                base_tuple,
                index_tuple,
                kind,
                span,
            )?;
            Ok(())
        }
        Some(op) => Err(affine_stride_error(
            kind,
            stride.op_position,
            op_count,
            "LoadY or LoadP",
            Some(linear_op_name(op)),
            span,
        )),
        None => Err(affine_stride_error(
            kind,
            stride.op_position,
            op_count,
            "LoadY or LoadP",
            None,
            span,
        )),
    }
}

fn apply_affine_const_stride(
    ops: &mut [LinearOp],
    stride: &rumoca_ir_solve::AffineStencilConstStride,
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    index_tuple: &[i64],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<(), ScalarizeError> {
    let op_count = ops.len();
    match ops.get_mut(stride.op_position) {
        Some(LinearOp::Const { value, .. }) => {
            *value = apply_const_terms(
                *value,
                &stride.terms,
                domain,
                base_tuple,
                index_tuple,
                kind,
                span,
            )?;
            Ok(())
        }
        Some(op) => Err(affine_stride_error(
            kind,
            stride.op_position,
            op_count,
            "Const",
            Some(linear_op_name(op)),
            span,
        )),
        None => Err(affine_stride_error(
            kind,
            stride.op_position,
            op_count,
            "Const",
            None,
            span,
        )),
    }
}

fn affine_stride_error(
    kind: &'static str,
    op_position: usize,
    op_count: usize,
    expected: &'static str,
    actual: Option<&'static str>,
    span: rumoca_core::Span,
) -> ScalarizeError {
    ScalarizeError::InvalidStrideOp {
        kind,
        op_position,
        op_count,
        expected,
        actual,
        span,
    }
}

fn linear_op_name(op: &LinearOp) -> &'static str {
    match op {
        LinearOp::Const { .. } => "Const",
        LinearOp::LoadTime { .. } => "LoadTime",
        LinearOp::LoadY { .. } | LinearOp::LoadP { .. } => {
            if matches!(op, LinearOp::LoadY { .. }) {
                "LoadY"
            } else {
                "LoadP"
            }
        }
        LinearOp::LoadSeed { .. } => "LoadSeed",
        LinearOp::LoadIndexedP { .. } => "LoadIndexedP",
        LinearOp::LoadIndexedSeed { .. } => "LoadIndexedSeed",
        LinearOp::Move { .. } => "Move",
        LinearOp::Unary { .. } => "Unary",
        LinearOp::Binary { .. } => "Binary",
        LinearOp::Compare { .. } => "Compare",
        LinearOp::Select { .. } => "Select",
        LinearOp::StoreOutput { .. } => "StoreOutput",
        LinearOp::LinearSolveComponent { .. } => "LinearSolveComponent",
        LinearOp::TableBounds { .. } => "TableBounds",
        LinearOp::TableLookup { .. } => "TableLookup",
        LinearOp::TableLookupSlope { .. } => "TableLookupSlope",
        LinearOp::TableNextEvent { .. } => "TableNextEvent",
        LinearOp::RandomInitialState { .. } => "RandomInitialState",
        LinearOp::RandomResult { .. } => "RandomResult",
        LinearOp::RandomState { .. } => "RandomState",
        LinearOp::ImpureRandomInit { .. } => "ImpureRandomInit",
        LinearOp::ImpureRandom { .. } => "ImpureRandom",
        LinearOp::ImpureRandomInteger { .. } => "ImpureRandomInteger",
    }
}

fn cloned_linear_ops(
    ops: &[LinearOp],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<LinearOp>, ScalarizeError> {
    let mut cloned = scalarize_vec_with_capacity(ops.len(), kind, span)?;
    cloned.extend_from_slice(ops);
    Ok(cloned)
}

fn scalarize_vec_with_capacity<T>(
    capacity: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, ScalarizeError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| ScalarizeError::AllocationOverflow {
            kind,
            capacity,
            span,
        })?;
    Ok(values)
}

fn apply_index_terms(
    base_index: usize,
    terms: &[rumoca_ir_solve::AffineStencilIndexStrideTerm],
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    index_tuple: &[i64],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, ScalarizeError> {
    let mut value = base_index as isize;
    for term in terms {
        value += ordinal_delta(term.dimension, domain, base_tuple, index_tuple, kind, span)?
            as isize
            * term.stride;
    }
    usize::try_from(value).map_err(|_| ScalarizeError::NegativeLoadIndex { kind, value, span })
}

fn apply_const_terms(
    base_value: f64,
    terms: &[rumoca_ir_solve::AffineStencilConstStrideTerm],
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    index_tuple: &[i64],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<f64, ScalarizeError> {
    let mut value = base_value;
    for term in terms {
        value += ordinal_delta(term.dimension, domain, base_tuple, index_tuple, kind, span)? as f64
            * term.stride;
    }
    Ok(value)
}

fn ordinal_delta(
    dimension: usize,
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    index_tuple: &[i64],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<i64, ScalarizeError> {
    let Some(binder) = domain.binders.get(dimension) else {
        return Err(ScalarizeError::InvalidStrideDimension {
            kind,
            dimension,
            dimension_count: domain.binders.len(),
            span,
        });
    };
    let step = binder.step;
    Ok((index_tuple[dimension] - base_tuple[dimension]) / step)
}

fn checked_tensor_output_count_optional(
    indices: &[usize],
    fallback: usize,
    kind: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<usize, ScalarizeError> {
    indices.iter().copied().max().map_or(Ok(fallback), |index| {
        let Some(count) = index.checked_add(1) else {
            return Err(match span {
                Some(span) => ScalarizeError::OutputCountOverflow { kind, index, span },
                None => ScalarizeError::MissingSourceSpan { kind },
            });
        };
        Ok(count)
    })
}

fn checked_contiguous_output_count_optional(
    start: usize,
    count: usize,
    kind: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<usize, ScalarizeError> {
    let Some(end) = start.checked_add(count) else {
        return Err(match span {
            Some(span) => ScalarizeError::ContiguousOutputOverflow {
                kind,
                start,
                count,
                span,
            },
            None => ScalarizeError::MissingSourceSpan { kind },
        });
    };
    Ok(end)
}

fn block_span(block: &ScalarProgramBlock) -> Option<rumoca_core::Span> {
    block.first_source_span()
}

fn tensor_output_map_error(
    error: rumoca_ir_solve::TensorOutputMapError,
    kind: &'static str,
    span: rumoca_core::Span,
) -> ScalarizeError {
    match error {
        rumoca_ir_solve::TensorOutputMapError::Dimension {
            output_dimension,
            domain_rank,
        } => ScalarizeError::InvalidOutputMapDimension {
            kind,
            dimension: output_dimension,
            dimension_count: domain_rank,
            span,
        },
        rumoca_ir_solve::TensorOutputMapError::StructuredIndexDomain { error } => {
            ScalarizeError::ShapeContract {
                message: format!("structured index domain is invalid: {error}"),
                span: Some(span),
            }
        }
        rumoca_ir_solve::TensorOutputMapError::NegativeIndex { value } => {
            ScalarizeError::NegativeOutputIndex { kind, value, span }
        }
        rumoca_ir_solve::TensorOutputMapError::OutputIndexOverflow => {
            ScalarizeError::OutputIndexArithmeticOverflow { kind, span }
        }
    }
}
