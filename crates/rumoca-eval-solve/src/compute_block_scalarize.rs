use rumoca_ir_solve::{
    ComputeBlock, ComputeNode, LinearOp, RefreshScalarProgramSource, Reg, ScalarProgramBlock,
    SolveProblemShapeContractError,
};

mod affine;
mod dense;

use affine::scalarize_affine_rows_with_span;
pub use affine::{
    checked_tensor_output_count, scalar_program_output_count, scalar_program_output_indices,
    tensor_output_indices,
};
pub(crate) use affine::{tensor_output_count, validate_affine_stride_metadata};
use dense::{MatMulScalarizeInput, scalarize_linsolve, scalarize_matmul};

/// Expand tensor `ComputeBlock` nodes to flat scalar programs.
///
/// This is the scalar fallback for consumers that cannot evaluate tensor-level
/// Solve IR directly. Invalid tensor metadata is reported as `ScalarizeError`
/// instead of treated as an internal invariant.
pub fn to_scalar_program_block(block: &ComputeBlock) -> Result<ScalarProgramBlock, ScalarizeError> {
    Ok(to_scalar_program_projection(block)?.block)
}

/// Final scalar fallback plus the exact canonical source of every retained
/// scalar program. Tensor-generated rows have no scalar-program source.
pub struct ScalarProgramProjection {
    block: ScalarProgramBlock,
    sources: Box<[Option<RefreshScalarProgramSource>]>,
}

impl ScalarProgramProjection {
    #[must_use]
    pub const fn block(&self) -> &ScalarProgramBlock {
        &self.block
    }

    #[must_use]
    pub const fn sources(&self) -> &[Option<RefreshScalarProgramSource>] {
        &self.sources
    }

    #[must_use]
    pub fn into_block(self) -> ScalarProgramBlock {
        self.block
    }
}

pub fn to_scalar_program_projection(
    block: &ComputeBlock,
) -> Result<ScalarProgramProjection, ScalarizeError> {
    block
        .validate_shape_contract("scalarize compute block")
        .map_err(ScalarizeError::from)?;
    let mut collector = ScalarProgramCollector::default();
    for (node_index, node) in block.nodes.iter().enumerate() {
        let output_cursor_before = collector.next_output;
        collector.append_compute_node(node_index, node)?;
        collector.trace_compute_node(node_index, node, output_cursor_before);
    }
    let block = ScalarProgramBlock::with_output_indices(
        collector.rows,
        collector.program_spans,
        collector.output_indices,
    )
    .map_err(ScalarizeError::from)?;
    if collector.sources.len() != block.programs().len() {
        return Err(ScalarizeError::ShapeContract {
            message: "scalar fallback source projection is not row-aligned".to_string(),
            span: block.first_source_span(),
        });
    }
    Ok(ScalarProgramProjection {
        block,
        sources: collector.sources.into_boxed_slice(),
    })
}

#[derive(Debug, Clone, PartialEq)]
pub enum ScalarizeError {
    InvalidStrideOp {
        kind: &'static str,
        op_position: usize,
        op_count: usize,
        expected: &'static str,
        actual: Option<&'static str>,
        span: rumoca_core::Span,
    },
    InvalidStrideDimension {
        kind: &'static str,
        dimension: usize,
        dimension_count: usize,
        span: rumoca_core::Span,
    },
    InvalidOutputMapDimension {
        kind: &'static str,
        dimension: usize,
        dimension_count: usize,
        span: rumoca_core::Span,
    },
    NegativeLoadIndex {
        kind: &'static str,
        value: i128,
        span: rumoca_core::Span,
    },
    NegativeOutputIndex {
        kind: &'static str,
        value: isize,
        span: rumoca_core::Span,
    },
    OutputIndexArithmeticOverflow {
        kind: &'static str,
        span: rumoca_core::Span,
    },
    OutputCountOverflow {
        kind: &'static str,
        index: usize,
        span: rumoca_core::Span,
    },
    ContiguousOutputOverflow {
        kind: &'static str,
        start: usize,
        count: usize,
        span: rumoca_core::Span,
    },
    ProductOverflow {
        kind: &'static str,
        lhs: usize,
        rhs: usize,
        span: rumoca_core::Span,
    },
    IndexOverflow {
        kind: &'static str,
        lhs: usize,
        rhs: usize,
        span: rumoca_core::Span,
    },
    RegisterIndexOverflow {
        kind: &'static str,
        index: usize,
        span: rumoca_core::Span,
    },
    RegisterRangeOverflow {
        kind: &'static str,
        start: Reg,
        offset: usize,
        span: rumoca_core::Span,
    },
    AllocationOverflow {
        kind: &'static str,
        capacity: usize,
        span: rumoca_core::Span,
    },
    MissingSourceSpan {
        kind: &'static str,
    },
    ShapeContract {
        message: String,
        span: Option<rumoca_core::Span>,
    },
}

struct AffineComputeNodeInput<'a> {
    domain: &'a rumoca_core::StructuredIndexDomain,
    output_map: &'a rumoca_ir_solve::TensorOutputMap,
    base_ops: &'a [LinearOp],
    load_strides: &'a [rumoca_ir_solve::AffineStencilLoadStride],
    const_strides: &'a [rumoca_ir_solve::AffineStencilConstStride],
    span: rumoca_core::Span,
    kind: &'static str,
}

impl ScalarizeError {
    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        let span = match self {
            Self::InvalidStrideOp { span, .. }
            | Self::InvalidStrideDimension { span, .. }
            | Self::InvalidOutputMapDimension { span, .. }
            | Self::NegativeLoadIndex { span, .. }
            | Self::NegativeOutputIndex { span, .. }
            | Self::OutputIndexArithmeticOverflow { span, .. }
            | Self::OutputCountOverflow { span, .. }
            | Self::ContiguousOutputOverflow { span, .. }
            | Self::ProductOverflow { span, .. }
            | Self::IndexOverflow { span, .. }
            | Self::RegisterIndexOverflow { span, .. }
            | Self::RegisterRangeOverflow { span, .. }
            | Self::AllocationOverflow { span, .. } => Some(*span),
            Self::ShapeContract { span, .. } => *span,
            Self::MissingSourceSpan { .. } => return None,
        };
        span.filter(|span| !span.is_dummy())
    }
}

impl std::fmt::Display for ScalarizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::InvalidStrideOp {
                kind,
                op_position,
                expected,
                actual: Some(actual),
                ..
            } => write!(
                f,
                "native {kind} family stride op position {op_position} points at {actual}, expected {expected}"
            ),
            Self::InvalidStrideOp {
                kind,
                op_position,
                op_count,
                actual: None,
                ..
            } => write!(
                f,
                "native {kind} family stride op position {op_position} out of bounds for {op_count} ops"
            ),
            Self::InvalidStrideDimension {
                kind,
                dimension,
                dimension_count,
                ..
            } => write!(
                f,
                "native {kind} family stride dimension {dimension} out of bounds for {dimension_count} dimensions"
            ),
            Self::InvalidOutputMapDimension {
                kind,
                dimension,
                dimension_count,
                ..
            } => write!(
                f,
                "native {kind} family output map dimension {dimension} out of bounds for {dimension_count} dimensions"
            ),
            Self::NegativeLoadIndex { kind, value, .. } => write!(
                f,
                "native {kind} family stride produced negative load index {value}"
            ),
            Self::NegativeOutputIndex { kind, value, .. } => write!(
                f,
                "native {kind} family output map produced negative output index {value}"
            ),
            Self::OutputIndexArithmeticOverflow { kind, .. } => {
                write!(f, "native {kind} family output map arithmetic overflowed")
            }
            Self::OutputCountOverflow { kind, index, .. } => write!(
                f,
                "native {kind} family output index {index} overflows output vector length"
            ),
            Self::ContiguousOutputOverflow {
                kind, start, count, ..
            } => write!(
                f,
                "native {kind} family contiguous output range start {start} count {count} \
                 overflows output vector length"
            ),
            Self::ProductOverflow { kind, lhs, rhs, .. } => write!(
                f,
                "native {kind} family shape product {lhs} * {rhs} overflows output vector length"
            ),
            Self::IndexOverflow { kind, lhs, rhs, .. } => {
                write!(f, "native {kind} family index sum {lhs} + {rhs} overflows")
            }
            Self::RegisterIndexOverflow { kind, index, .. } => write!(
                f,
                "native {kind} family register index {index} exceeds Solve-IR register type"
            ),
            Self::RegisterRangeOverflow {
                kind,
                start,
                offset,
                ..
            } => write!(
                f,
                "native {kind} family register range starting at {start} with offset {offset} overflows Solve-IR register type"
            ),
            Self::AllocationOverflow { kind, capacity, .. } => write!(
                f,
                "scalarized {kind} capacity {capacity} exceeds host memory limits"
            ),
            Self::MissingSourceSpan { kind } => {
                write!(f, "scalarized {kind} is missing source span metadata")
            }
            Self::ShapeContract { message, .. } => {
                write!(
                    f,
                    "scalarized Solve-IR block violates shape contract: {message}"
                )
            }
        }
    }
}

impl std::error::Error for ScalarizeError {}

impl From<SolveProblemShapeContractError> for ScalarizeError {
    fn from(value: SolveProblemShapeContractError) -> Self {
        Self::ShapeContract {
            message: value.to_string(),
            span: value.source_span(),
        }
    }
}

#[derive(Default)]
struct ScalarProgramCollector {
    rows: Vec<Vec<LinearOp>>,
    program_spans: Vec<rumoca_core::Span>,
    output_indices: Vec<usize>,
    sources: Vec<Option<RefreshScalarProgramSource>>,
    next_output: usize,
}

impl ScalarProgramCollector {
    fn append_scalar_program_block(
        &mut self,
        node_index: usize,
        block: &ScalarProgramBlock,
    ) -> Result<(), ScalarizeError> {
        let span = scalar_program_block_span(block);
        reserve_vec_additional_optional(
            &mut self.sources,
            block.programs().len(),
            "scalar program sources",
            span,
        )?;
        for program_index in 0..block.programs().len() {
            let source = RefreshScalarProgramSource::checked(node_index, program_index)
                .ok_or_else(|| ScalarizeError::ShapeContract {
                    message: "scalar program source identity exceeds u32".to_string(),
                    span,
                })?;
            self.sources.push(Some(source));
        }
        let mut programs = cloned_scalar_rows_optional(block.programs(), "scalar programs", span)?;
        append_vec_optional(&mut self.rows, &mut programs, "scalar programs", span)?;
        extend_cloned_values(
            &mut self.program_spans,
            block.program_spans(),
            "scalar programs spans",
            span,
        )?;
        let mut output_indices =
            scalar_program_output_indices(block, self.next_output, "scalar programs")?;
        append_vec_optional(
            &mut self.output_indices,
            &mut output_indices,
            "scalar programs output indices",
            span,
        )?;
        self.next_output = self.next_output.max(scalar_program_output_count(
            block,
            self.next_output,
            "scalar programs",
        )?);
        Ok(())
    }

    fn append_contiguous_programs(
        &mut self,
        mut programs: Vec<Vec<LinearOp>>,
        start: usize,
        end: usize,
        span: rumoca_core::Span,
        kind: &'static str,
    ) -> Result<(), ScalarizeError> {
        reserve_vec_additional(&mut self.sources, programs.len(), kind, span)?;
        self.sources
            .extend(std::iter::repeat_n(None, programs.len()));
        push_repeated_spans(&mut self.program_spans, span, programs.len(), kind)?;
        extend_output_range(&mut self.output_indices, start, end, kind, span)?;
        self.next_output = end;
        append_vec(&mut self.rows, &mut programs, kind, span)
    }

    fn append_tensor_programs(
        &mut self,
        mut programs: Vec<Vec<LinearOp>>,
        mut output_indices: Vec<usize>,
        span: rumoca_core::Span,
        kind: &'static str,
    ) -> Result<(), ScalarizeError> {
        reserve_vec_additional(&mut self.sources, programs.len(), kind, span)?;
        self.sources
            .extend(std::iter::repeat_n(None, programs.len()));
        push_repeated_spans(&mut self.program_spans, span, programs.len(), kind)?;
        self.next_output = self.next_output.max(checked_tensor_output_count(
            &output_indices,
            self.next_output,
            kind,
            span,
        )?);
        append_vec(&mut self.output_indices, &mut output_indices, kind, span)?;
        append_vec(&mut self.rows, &mut programs, kind, span)
    }

    fn append_compute_node(
        &mut self,
        node_index: usize,
        node: &ComputeNode,
    ) -> Result<(), ScalarizeError> {
        match node {
            ComputeNode::ScalarPrograms(block) => {
                self.append_scalar_program_block(node_index, block)
            }
            ComputeNode::MatMul {
                lhs_ops,
                lhs_start,
                rhs_ops,
                rhs_start,
                m,
                k,
                n,
                span,
                ..
            } => {
                let start = self.next_output;
                let output_len = rumoca_core::checked_product(*m, *n).ok_or(
                    ScalarizeError::ProductOverflow {
                        kind: "matmul",
                        lhs: *m,
                        rhs: *n,
                        span: *span,
                    },
                )?;
                if output_len == 0 {
                    return Ok(());
                }
                let program = scalarize_matmul(MatMulScalarizeInput {
                    lhs_ops,
                    lhs_start: *lhs_start,
                    rhs_ops,
                    rhs_start: *rhs_start,
                    m: *m,
                    k: *k,
                    n: *n,
                    span: *span,
                })?;
                let end = checked_contiguous_output_count(start, output_len, "matmul", *span)?;
                self.append_contiguous_programs(vec![program], start, end, *span, "matmul")
            }
            ComputeNode::LinSolve {
                setup_ops,
                matrix_start,
                rhs_start,
                n,
                next_reg,
                span,
                ..
            } => {
                if *n == 0 {
                    return Ok(());
                }
                let program =
                    scalarize_linsolve(setup_ops, *matrix_start, *rhs_start, *n, *next_reg, *span)?;
                let start = self.next_output;
                let end = checked_contiguous_output_count(start, *n, "linsolve", *span)?;
                self.append_contiguous_programs(vec![program], start, end, *span, "linsolve")
            }
            ComputeNode::Map {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            } => self.append_affine_compute_node(AffineComputeNodeInput {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                span: *span,
                kind: "map",
            }),
            ComputeNode::AffineStencil {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            } => self.append_affine_compute_node(AffineComputeNodeInput {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                span: *span,
                kind: "affine stencil",
            }),
        }
    }

    fn append_affine_compute_node(
        &mut self,
        input: AffineComputeNodeInput<'_>,
    ) -> Result<(), ScalarizeError> {
        let scalar_programs = scalarize_affine_rows_with_span(
            input.domain,
            input.base_ops,
            input.load_strides,
            input.const_strides,
            input.kind,
            input.span,
        )?;
        let output_indices =
            tensor_output_indices(input.domain, input.output_map, input.kind, input.span)?;
        self.append_tensor_programs(scalar_programs, output_indices, input.span, input.kind)
    }

    fn trace_compute_node(
        &self,
        node_index: usize,
        node: &ComputeNode,
        output_cursor_before: usize,
    ) {
        if !tracing::enabled!(target: "rumoca_eval_solve::scalarize", tracing::Level::DEBUG) {
            return;
        }
        let (kind, declared_outputs) = compute_node_trace_fields(node);
        tracing::debug!(
            target: "rumoca_eval_solve::scalarize",
            node_index,
            kind,
            output_cursor_before,
            output_cursor_after = self.next_output,
            declared_outputs,
            "scalarized compute node"
        );
    }
}

fn compute_node_trace_fields(node: &ComputeNode) -> (&'static str, String) {
    match node {
        ComputeNode::ScalarPrograms(block) => ("scalar", format!("{:?}", block.output_indices())),
        ComputeNode::MatMul { m, n, .. } => ("matmul", format!("{m}x{n}")),
        ComputeNode::LinSolve { n, .. } => ("linsolve", n.to_string()),
        ComputeNode::Map { output_map, .. } => ("map", format!("{output_map:?}")),
        ComputeNode::AffineStencil { output_map, .. } => {
            ("affine_stencil", format!("{output_map:?}"))
        }
    }
}

fn scalar_program_block_span(block: &ScalarProgramBlock) -> Option<rumoca_core::Span> {
    block.first_source_span()
}

fn cloned_scalar_rows_optional(
    rows: &[Vec<LinearOp>],
    kind: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<Vec<LinearOp>>, ScalarizeError> {
    let mut cloned = scalarize_vec_with_capacity_optional(rows.len(), kind, span)?;
    for row in rows {
        cloned.push(cloned_linear_ops_optional(row, kind, span)?);
    }
    Ok(cloned)
}

fn cloned_linear_ops_optional(
    ops: &[LinearOp],
    kind: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<LinearOp>, ScalarizeError> {
    let mut cloned = scalarize_vec_with_capacity_optional(ops.len(), kind, span)?;
    cloned.extend_from_slice(ops);
    Ok(cloned)
}

pub(super) fn scalarize_vec_with_capacity_optional<T>(
    capacity: usize,
    kind: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<T>, ScalarizeError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| allocation_error(kind, capacity, span))?;
    Ok(values)
}

fn allocation_error(
    kind: &'static str,
    capacity: usize,
    span: Option<rumoca_core::Span>,
) -> ScalarizeError {
    match span {
        Some(span) => ScalarizeError::AllocationOverflow {
            kind,
            capacity,
            span,
        },
        None => ScalarizeError::MissingSourceSpan { kind },
    }
}

fn reserve_vec_additional<T>(
    values: &mut Vec<T>,
    additional: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<(), ScalarizeError> {
    values
        .try_reserve_exact(additional)
        .map_err(|_| ScalarizeError::AllocationOverflow {
            kind,
            capacity: additional,
            span,
        })
}

fn reserve_vec_additional_optional<T>(
    values: &mut Vec<T>,
    additional: usize,
    kind: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), ScalarizeError> {
    values
        .try_reserve_exact(additional)
        .map_err(|_| allocation_error(kind, additional, span))
}

fn append_vec<T>(
    dst: &mut Vec<T>,
    src: &mut Vec<T>,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<(), ScalarizeError> {
    reserve_vec_additional(dst, src.len(), kind, span)?;
    dst.append(src);
    Ok(())
}

fn append_vec_optional<T>(
    dst: &mut Vec<T>,
    src: &mut Vec<T>,
    kind: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), ScalarizeError> {
    reserve_vec_additional_optional(dst, src.len(), kind, span)?;
    dst.append(src);
    Ok(())
}

fn extend_cloned_values<T: Clone>(
    dst: &mut Vec<T>,
    values: &[T],
    kind: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), ScalarizeError> {
    reserve_vec_additional_optional(dst, values.len(), kind, span)?;
    dst.extend_from_slice(values);
    Ok(())
}

fn push_repeated_spans(
    dst: &mut Vec<rumoca_core::Span>,
    span: rumoca_core::Span,
    count: usize,
    kind: &'static str,
) -> Result<(), ScalarizeError> {
    reserve_vec_additional(dst, count, kind, span)?;
    dst.extend(std::iter::repeat_n(span, count));
    Ok(())
}

fn extend_output_range(
    dst: &mut Vec<usize>,
    start: usize,
    end: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<(), ScalarizeError> {
    let count = end - start;
    reserve_vec_additional(dst, count, kind, span)?;
    dst.extend(start..end);
    Ok(())
}

pub fn checked_contiguous_output_count(
    start: usize,
    count: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, ScalarizeError> {
    start
        .checked_add(count)
        .ok_or(ScalarizeError::ContiguousOutputOverflow {
            kind,
            start,
            count,
            span,
        })
}

pub(super) fn scalarize_product(
    lhs: usize,
    rhs: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, ScalarizeError> {
    rumoca_core::checked_product(lhs, rhs).ok_or(ScalarizeError::ProductOverflow {
        kind,
        lhs,
        rhs,
        span,
    })
}

#[cfg(test)]
mod tests;
