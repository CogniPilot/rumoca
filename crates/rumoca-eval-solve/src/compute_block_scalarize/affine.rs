use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};

use super::kernel_plan::{AffineKernelNode, AffineKernelPlan};
use super::{ScalarizeError, scalarize_vec_with_capacity_optional};

pub(super) fn scalarize_affine_rows_with_span(
    domain: &rumoca_core::StructuredIndexDomain,
    base_ops: &[LinearOp],
    load_strides: &[rumoca_ir_solve::AffineStencilLoadStride],
    const_strides: &[rumoca_ir_solve::AffineStencilConstStride],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<Vec<LinearOp>>, ScalarizeError> {
    let plan = AffineKernelPlan::new(AffineKernelNode {
        domain,
        output_map: None,
        base_ops,
        load_strides,
        const_strides,
        kind,
        span,
    })?;
    let mut rows = Vec::new();
    rows.try_reserve_exact(plan.point_count())
        .map_err(|_| ScalarizeError::AllocationOverflow {
            kind,
            capacity: plan.point_count(),
            span,
        })?;
    plan.for_each_point(|ordinals| rows.push(plan.row_at(base_ops, ordinals)));
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

/// Compute a tensor node's logical output count without materializing its scalar view.
pub(crate) fn tensor_output_count(
    domain: &rumoca_core::StructuredIndexDomain,
    output_map: &rumoca_ir_solve::TensorOutputMap,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, ScalarizeError> {
    output_map
        .output_count(domain)
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
            scalarize_vec_with_capacity_optional(block.output_indices().len(), kind, span)?;
        indices.extend_from_slice(block.output_indices());
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

pub(crate) fn validate_affine_stride_metadata(
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
            Some(LinearOp::LoadY { .. } | LinearOp::LoadP { .. } | LinearOp::LoadSeed { .. }) => {}
            Some(op) => {
                return Err(affine_stride_error(
                    kind,
                    stride.op_position,
                    base_ops.len(),
                    "LoadY, LoadP, or LoadSeed",
                    Some(linear_op_name(op)),
                    span,
                ));
            }
            None => {
                return Err(affine_stride_error(
                    kind,
                    stride.op_position,
                    base_ops.len(),
                    "LoadY, LoadP, or LoadSeed",
                    None,
                    span,
                ));
            }
        }
    }
    for stride in const_strides {
        validate_stride_terms(domain, &stride.terms, kind, span)?;
        if stride.terms.iter().any(|term| !term.stride.is_finite()) {
            return Err(ScalarizeError::ShapeContract {
                message: format!("native {kind} family constant stride must be finite"),
                span: Some(span),
            });
        }
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
        LinearOp::LoadIndexedRegister { .. } => "LoadIndexedRegister",
        LinearOp::LoadIndexedFoldCarried { .. } => "LoadIndexedFoldCarried",
        LinearOp::LoadIndexedFoldCapture { .. } => "LoadIndexedFoldCapture",
        LinearOp::LoadIndexedSeed { .. } => "LoadIndexedSeed",
        LinearOp::LoadFoldCarried { .. } => "LoadFoldCarried",
        LinearOp::LoadFoldIndex { .. } => "LoadFoldIndex",
        LinearOp::LoadFoldCapture { .. } => "LoadFoldCapture",
        LinearOp::LoadFunctionConditionalCapture { .. } => "LoadFunctionConditionalCapture",
        LinearOp::LoadFunctionConditionalCaptureRange { .. } => {
            "LoadFunctionConditionalCaptureRange"
        }
        LinearOp::Move { .. } => "Move",
        LinearOp::Unary { .. } => "Unary",
        LinearOp::Binary { .. } => "Binary",
        LinearOp::Compare { .. } => "Compare",
        LinearOp::Select { .. } => "Select",
        LinearOp::StoreOutputFoldTensorUpdate { .. } => "StoreOutputFoldTensorUpdate",
        LinearOp::StoreOutputFunctionFold { .. } => "StoreOutputFunctionFold",
        LinearOp::StoreOutputRange { .. } => "StoreOutputRange",
        LinearOp::StoreOutput { .. } => "StoreOutput",
        LinearOp::LinearSolveComponent { .. } => "LinearSolveComponent",
        LinearOp::DotProduct { .. } => "DotProduct",
        LinearOp::MatrixMultiply { .. } => "MatrixMultiply",
        LinearOp::TensorBinary { .. } => "TensorBinary",
        LinearOp::TensorCross { .. } => "TensorCross",
        LinearOp::TensorTranspose { .. } => "TensorTranspose",
        LinearOp::TensorConcatenate { .. } => "TensorConcatenate",
        LinearOp::TensorUpdate { .. } => "TensorUpdate",
        LinearOp::TensorFill { .. } => "TensorFill",
        LinearOp::TensorIdentity { .. } => "TensorIdentity",
        LinearOp::TensorLoad { .. } => "TensorLoad",
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
        LinearOp::FunctionFold { .. } => "FunctionFold",
        LinearOp::GuardedFunctionFold { .. } => "GuardedFunctionFold",
        LinearOp::FunctionConditional { .. } => "FunctionConditional",
        LinearOp::PureCall { .. } => "PureCall",
        LinearOp::PureCallDirectional { .. } => "PureCallDirectional",
    }
}

pub(super) fn checked_tensor_output_count_optional(
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
