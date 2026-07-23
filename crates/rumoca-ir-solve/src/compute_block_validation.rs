//! ComputeBlock output ownership and shape-contract validation.

use std::collections::HashSet;

use super::{
    ComputeNode, SolveProblemShapeContractError, Span, StructuredIndexDomain, TensorOutputMap,
    TensorOutputMapError,
};

pub(super) fn tensor_output_count_for_node(
    context: &'static str,
    node_index: usize,
    node: &ComputeNode,
    domain: &StructuredIndexDomain,
    output_map: &TensorOutputMap,
) -> Result<usize, SolveProblemShapeContractError> {
    let (dimension, span) = match node {
        ComputeNode::Map { span, .. } => ("Map", *span),
        ComputeNode::AffineStencil { span, .. } => ("AffineStencil", *span),
        ComputeNode::ScalarPrograms(_)
        | ComputeNode::MatMul { .. }
        | ComputeNode::LinSolve { .. } => unreachable!("tensor output count requires tensor node"),
    };
    output_map
        .output_count(domain)
        .map_err(|error| tensor_output_map_error(context, node_index, dimension, error, span))
}

pub(super) fn tensor_output_map_error(
    context: &'static str,
    node_index: usize,
    dimension: &'static str,
    error: TensorOutputMapError,
    span: Span,
) -> SolveProblemShapeContractError {
    match error {
        TensorOutputMapError::Dimension {
            output_dimension,
            domain_rank,
        } => SolveProblemShapeContractError::TensorOutputMapDimension {
            context: context.to_string(),
            node_index,
            dimension,
            output_dimension,
            domain_rank,
            span,
        },
        TensorOutputMapError::StructuredIndexDomain { error } => {
            SolveProblemShapeContractError::StructuredIndexDomain {
                context: context.to_string(),
                node_index,
                dimension,
                error,
                span,
            }
        }
        TensorOutputMapError::NegativeIndex { value } => {
            SolveProblemShapeContractError::TensorOutputMapNegativeIndex {
                context: context.to_string(),
                node_index,
                dimension,
                value,
                span,
            }
        }
        TensorOutputMapError::OutputIndexOverflow => {
            output_index_overflow(context, node_index, Some(span))
        }
    }
}

pub(super) fn output_index_overflow(
    context: impl Into<String>,
    node_index: usize,
    span: Option<Span>,
) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::OutputIndexOverflow {
        context: context.into(),
        node_index,
        span,
    }
}

pub(super) fn compute_node_output_cursor(
    context: &str,
    node_index: usize,
    output_cursor: usize,
    output_count: usize,
    output_indices: &[usize],
    span: Span,
) -> Result<usize, SolveProblemShapeContractError> {
    if output_indices.is_empty() {
        return output_cursor
            .checked_add(output_count)
            .ok_or_else(|| output_index_overflow(context, node_index, Some(span)));
    }
    let Some(max_index) = output_indices.iter().copied().max() else {
        return Ok(output_cursor);
    };
    let next = max_index
        .checked_add(1)
        .ok_or_else(|| output_index_overflow(context, node_index, Some(span)))?;
    Ok(output_cursor.max(next))
}

fn validate_linsolve_output_indices(
    context: &str,
    node_index: usize,
    n: usize,
    output_indices: &[usize],
    span: Span,
) -> Result<(), SolveProblemShapeContractError> {
    if !output_indices.is_empty() && output_indices.len() != n {
        return Err(
            SolveProblemShapeContractError::LinSolveOutputIndexMismatch {
                context: context.to_string(),
                node_index,
                components: n,
                output_indices: output_indices.len(),
                span,
            },
        );
    }
    let mut seen = HashSet::with_capacity(output_indices.len());
    for output_index in output_indices {
        if !seen.insert(*output_index) {
            return Err(
                SolveProblemShapeContractError::LinSolveDuplicateOutputIndex {
                    context: context.to_string(),
                    node_index,
                    output_index: *output_index,
                    span,
                },
            );
        }
    }
    Ok(())
}

impl ComputeNode {
    pub fn validate_shape_contract(
        &self,
        context: &str,
        node_index: usize,
    ) -> Result<(), SolveProblemShapeContractError> {
        match self {
            ComputeNode::ScalarPrograms(block) => {
                block
                    .validate_shape_contract(context)
                    .map_err(|err| match err {
                        SolveProblemShapeContractError::ScalarProgramSpanMismatch {
                            programs,
                            spans,
                            ..
                        } => SolveProblemShapeContractError::ScalarProgramSpanMismatch {
                            context: context.to_string(),
                            node_index,
                            programs,
                            spans,
                            span: block.first_program_span(),
                        },
                        SolveProblemShapeContractError::ScalarProgramOutputIndexMismatch {
                            programs,
                            output_indices,
                            ..
                        } => SolveProblemShapeContractError::ScalarProgramOutputIndexMismatch {
                            context: context.to_string(),
                            node_index,
                            programs,
                            output_indices,
                            span: block.first_program_span(),
                        },
                        other => other,
                    })?;
            }
            ComputeNode::MatMul { m, k, n, span, .. } => {
                if *m == 0 || *k == 0 || *n == 0 {
                    return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                        context: context.to_string(),
                        node_index,
                        dimension: "MatMul",
                        span: *span,
                    });
                }
            }
            ComputeNode::LinSolve {
                n,
                output_indices,
                span,
                ..
            } => {
                if *n == 0 {
                    return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                        context: context.to_string(),
                        node_index,
                        dimension: "LinSolve",
                        span: *span,
                    });
                }
                validate_linsolve_output_indices(context, node_index, *n, output_indices, *span)?;
            }
            ComputeNode::Map {
                domain,
                output_map,
                span,
                ..
            } => {
                let count = validate_tensor_domain(context, node_index, "Map", domain, *span)?;
                if count == 0 {
                    return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                        context: context.to_string(),
                        node_index,
                        dimension: "Map",
                        span: *span,
                    });
                }
                validate_tensor_output_map(context, node_index, "Map", domain, output_map, *span)?;
            }
            ComputeNode::AffineStencil {
                domain,
                output_map,
                span,
                ..
            } => {
                let count =
                    validate_tensor_domain(context, node_index, "AffineStencil", domain, *span)?;
                if count == 0 {
                    return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                        context: context.to_string(),
                        node_index,
                        dimension: "AffineStencil",
                        span: *span,
                    });
                }
                validate_tensor_output_map(
                    context,
                    node_index,
                    "AffineStencil",
                    domain,
                    output_map,
                    *span,
                )?;
            }
        }
        Ok(())
    }
}

fn validate_tensor_domain(
    context: &str,
    node_index: usize,
    dimension: &'static str,
    domain: &StructuredIndexDomain,
    span: Span,
) -> Result<usize, SolveProblemShapeContractError> {
    domain.validate().map_err(
        |err| SolveProblemShapeContractError::StructuredIndexDomain {
            context: context.to_string(),
            node_index,
            dimension,
            error: err,
            span,
        },
    )
}

fn validate_tensor_output_map(
    context: &str,
    node_index: usize,
    dimension: &'static str,
    domain: &StructuredIndexDomain,
    output_map: &TensorOutputMap,
    span: Span,
) -> Result<(), SolveProblemShapeContractError> {
    for term in &output_map.strides {
        if term.dimension >= domain.binders.len() {
            return Err(SolveProblemShapeContractError::TensorOutputMapDimension {
                context: context.to_string(),
                node_index,
                dimension,
                output_dimension: term.dimension,
                domain_rank: domain.binders.len(),
                span,
            });
        }
    }
    if domain
        .index_tuples()
        .map_err(
            |error| SolveProblemShapeContractError::StructuredIndexDomain {
                context: context.to_string(),
                node_index,
                dimension,
                error,
                span,
            },
        )?
        .is_empty()
    {
        return Ok(());
    }
    output_map.output_indices(domain).map_err(|err| match err {
        TensorOutputMapError::Dimension {
            output_dimension,
            domain_rank,
        } => SolveProblemShapeContractError::TensorOutputMapDimension {
            context: context.to_string(),
            node_index,
            dimension,
            output_dimension,
            domain_rank,
            span,
        },
        TensorOutputMapError::StructuredIndexDomain { error } => {
            SolveProblemShapeContractError::StructuredIndexDomain {
                context: context.to_string(),
                node_index,
                dimension,
                error,
                span,
            }
        }
        TensorOutputMapError::NegativeIndex { value } => {
            SolveProblemShapeContractError::TensorOutputMapNegativeIndex {
                context: context.to_string(),
                node_index,
                dimension,
                value,
                span,
            }
        }
        TensorOutputMapError::OutputIndexOverflow => {
            output_index_overflow(context, node_index, Some(span))
        }
    })?;
    Ok(())
}
