use rumoca_ir_solve as solve;

use crate::{LowerError, stencil};

type LinearOpRows = Vec<Vec<solve::LinearOp>>;

pub(crate) struct ImplicitRowsAndTargets {
    pub(crate) rows: LinearOpRows,
    pub(crate) row_targets: Vec<Option<solve::ScalarSlot>>,
    pub(crate) residual_to_implicit_rows: Vec<Option<usize>>,
}

pub(crate) fn state_only_implicit_rows_and_targets(
    state_scalar_count: usize,
    context_span: rumoca_core::Span,
) -> Result<ImplicitRowsAndTargets, LowerError> {
    let mut row_targets = implicit_rhs_vec_with_capacity(
        state_scalar_count,
        "state-only implicit RHS row targets",
        context_span,
    )?;
    for idx in 0..state_scalar_count {
        row_targets.push(Some(implicit_rhs_y_slot(idx, context_span)?));
    }
    Ok(ImplicitRowsAndTargets {
        rows: Vec::new(),
        row_targets,
        residual_to_implicit_rows: Vec::new(),
    })
}

pub(crate) fn build_implicit_rhs_rows(
    derivative_rhs: &[Vec<solve::LinearOp>],
    residual: &[Vec<solve::LinearOp>],
    residual_targets: &[Option<solve::ScalarSlot>],
    state_scalar_count: usize,
    solver_scalar_count: usize,
    context_span: rumoca_core::Span,
) -> Result<ImplicitRowsAndTargets, LowerError> {
    let span = context_span;
    let mut rows = implicit_rhs_vec_with_capacity(solver_scalar_count, "implicit RHS rows", span)?;
    for _ in 0..solver_scalar_count {
        rows.push(implicit_rhs_zero_row(span)?);
    }
    let mut row_targets =
        implicit_rhs_vec_with_capacity(solver_scalar_count, "implicit RHS row targets", span)?;
    row_targets.resize(solver_scalar_count, None);
    let mut occupied =
        implicit_rhs_vec_with_capacity(solver_scalar_count, "implicit RHS occupancy", span)?;
    occupied.resize(solver_scalar_count, false);
    let mut residual_to_implicit_rows =
        implicit_rhs_vec_with_capacity(residual.len(), "residual to implicit row map", span)?;
    residual_to_implicit_rows.resize(residual.len(), None);
    let per_state_derivative_rows = derivative_rhs.len() == state_scalar_count;
    for (idx, target_row) in rows.iter_mut().enumerate().take(state_scalar_count) {
        if per_state_derivative_rows {
            let Some(row) = derivative_rhs.get(idx) else {
                return Err(LowerError::Unsupported {
                    reason: format!("missing state derivative RHS row {idx} for solve layout"),
                });
            };
            *target_row = row.clone();
        }
        row_targets[idx] = Some(implicit_rhs_y_slot(idx, span)?);
        occupied[idx] = true;
    }
    place_targeted_residual_rows(
        &mut rows,
        &mut row_targets,
        &mut occupied,
        residual,
        residual_targets,
        state_scalar_count,
        solver_scalar_count,
        &mut residual_to_implicit_rows,
        span,
    )?;
    Ok(ImplicitRowsAndTargets {
        rows,
        row_targets,
        residual_to_implicit_rows,
    })
}

pub(crate) fn build_implicit_rhs_compute_block(
    derivative_rhs: &solve::ComputeBlock,
    residual_block: &solve::ComputeBlock,
    residual_to_implicit_rows: &[Option<usize>],
    scalar_programs: Vec<Vec<solve::LinearOp>>,
    state_scalar_count: usize,
    context_span: rumoca_core::Span,
) -> Result<solve::ComputeBlock, LowerError> {
    let span = compute_block_context_span(derivative_rhs, residual_block, context_span)?;
    let derivative_rhs_len = derivative_rhs.len().map_err(shape_contract_lower_error)?;
    if derivative_rhs_len != state_scalar_count {
        return Ok(solve::ComputeBlock::from_scalar_program_block(
            scalar_program_block_with_source_span(scalar_programs, span)?,
        ));
    }
    if scalar_programs.is_empty() && residual_to_implicit_rows.is_empty() {
        return Ok(derivative_rhs.clone());
    }
    let mut nodes = implicit_rhs_vec_with_capacity(
        derivative_rhs.nodes.len(),
        "implicit RHS derivative compute node count",
        span,
    )?;
    for node in &derivative_rhs.nodes {
        nodes.push(node.clone());
    }
    if uncovered_implicit_tail_rows_can_follow_compute_nodes(
        &scalar_programs,
        residual_to_implicit_rows,
        state_scalar_count,
        span,
    )? && let Some(residual_nodes) = remap_residual_compute_nodes(
        residual_block,
        residual_to_implicit_rows,
        state_scalar_count,
        span,
    )? {
        reserve_implicit_rhs_capacity(
            &mut nodes,
            residual_nodes.len(),
            "implicit RHS residual compute node count",
            span,
        )?;
        for node in residual_nodes {
            nodes.push(node);
        }
        push_uncovered_implicit_tail_rows(
            &mut nodes,
            &scalar_programs,
            residual_to_implicit_rows,
            state_scalar_count,
            span,
        )?;
        return Ok(solve::ComputeBlock { nodes });
    } else {
        let tail_count = scalar_programs.len() - state_scalar_count;
        let mut tail_rows =
            implicit_rhs_vec_with_capacity(tail_count, "implicit RHS tail row count", span)?;
        let mut tail_spans =
            implicit_rhs_vec_with_capacity(tail_count, "implicit RHS tail span count", span)?;
        let mut output_indices =
            implicit_rhs_vec_with_capacity(tail_count, "implicit RHS tail output count", span)?;
        for (offset, row) in scalar_programs
            .into_iter()
            .skip(state_scalar_count)
            .enumerate()
        {
            tail_rows.push(row);
            tail_spans.push(span);
            output_indices.push(state_scalar_count + offset);
        }
        if !tail_rows.is_empty() {
            let tail_block = solve::ScalarProgramBlock::with_output_indices(
                tail_rows,
                tail_spans,
                output_indices,
            )?;
            reserve_implicit_rhs_capacity(
                &mut nodes,
                1,
                "implicit RHS tail compute node count",
                span,
            )?;
            nodes.push(solve::ComputeNode::ScalarPrograms(tail_block));
        }
    }
    Ok(solve::ComputeBlock { nodes })
}

fn shape_contract_lower_error(err: solve::SolveProblemShapeContractError) -> LowerError {
    implicit_rhs_optional_contract_violation(err.to_string(), err.source_span())
}

fn implicit_rhs_contract_violation(
    reason: impl Into<String>,
    span: rumoca_core::Span,
) -> LowerError {
    let reason = reason.into();
    if span.is_dummy() {
        LowerError::UnspannedContractViolation { reason }
    } else {
        LowerError::ContractViolation { reason, span }
    }
}

fn implicit_rhs_optional_contract_violation(
    reason: impl Into<String>,
    span: Option<rumoca_core::Span>,
) -> LowerError {
    match span.filter(|span| !span.is_dummy()) {
        Some(span) => LowerError::ContractViolation {
            reason: reason.into(),
            span,
        },
        None => LowerError::UnspannedContractViolation {
            reason: reason.into(),
        },
    }
}

fn implicit_rhs_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    values.try_reserve_exact(capacity).map_err(|_| {
        implicit_rhs_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })?;
    Ok(values)
}

fn reserve_implicit_rhs_capacity<T>(
    values: &mut Vec<T>,
    additional: usize,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(additional).map_err(|_| {
        implicit_rhs_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn implicit_rhs_zero_row(span: rumoca_core::Span) -> Result<Vec<solve::LinearOp>, LowerError> {
    let mut row = implicit_rhs_vec_with_capacity(2, "implicit RHS zero row op count", span)?;
    row.push(solve::LinearOp::Const { dst: 0, value: 0.0 });
    row.push(solve::LinearOp::StoreOutput { src: 0 });
    Ok(row)
}

fn scalar_program_block_with_source_span(
    programs: Vec<Vec<solve::LinearOp>>,
    span: rumoca_core::Span,
) -> Result<solve::ScalarProgramBlock, LowerError> {
    let mut program_spans =
        implicit_rhs_vec_with_capacity(programs.len(), "scalar program span count", span)?;
    program_spans.resize(programs.len(), span);
    Ok(solve::ScalarProgramBlock::with_program_spans(
        programs,
        program_spans,
    )?)
}

fn compute_block_context_span(
    first: &solve::ComputeBlock,
    second: &solve::ComputeBlock,
    context_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    compute_block_first_span(first)
        .or_else(|| compute_block_first_span(second))
        .or_else(|| (!context_span.is_dummy()).then_some(context_span))
        .ok_or_else(|| LowerError::UnspannedContractViolation {
            reason: "implicit RHS compute block has no source provenance".to_string(),
        })
}

fn compute_block_first_span(block: &solve::ComputeBlock) -> Option<rumoca_core::Span> {
    for node in &block.nodes {
        if let Some(span) = compute_node_context_span(node) {
            return Some(span);
        }
    }
    None
}

fn compute_node_context_span(node: &solve::ComputeNode) -> Option<rumoca_core::Span> {
    match node {
        solve::ComputeNode::ScalarPrograms(block) => first_non_dummy_span(&block.program_spans),
        solve::ComputeNode::MatMul { span, .. }
        | solve::ComputeNode::LinSolve { span, .. }
        | solve::ComputeNode::Map { span, .. }
        | solve::ComputeNode::AffineStencil { span, .. } => (!span.is_dummy()).then_some(*span),
    }
}

fn first_non_dummy_span(spans: &[rumoca_core::Span]) -> Option<rumoca_core::Span> {
    for span in spans {
        if !span.is_dummy() {
            return Some(*span);
        }
    }
    None
}

fn push_uncovered_implicit_tail_rows(
    nodes: &mut Vec<solve::ComputeNode>,
    scalar_programs: &[Vec<solve::LinearOp>],
    residual_to_implicit_rows: &[Option<usize>],
    state_scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let (rows, output_indices) = uncovered_implicit_tail_rows(
        scalar_programs,
        residual_to_implicit_rows,
        state_scalar_count,
        span,
    )?;
    if !rows.is_empty() {
        let mut program_spans =
            implicit_rhs_vec_with_capacity(rows.len(), "uncovered implicit tail span count", span)?;
        program_spans.resize(rows.len(), span);
        reserve_implicit_rhs_capacity(
            nodes,
            1,
            "uncovered implicit tail compute node count",
            span,
        )?;
        nodes.push(solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_output_indices(rows, program_spans, output_indices)?,
        ));
    }
    Ok(())
}

fn uncovered_implicit_tail_rows_can_follow_compute_nodes(
    scalar_programs: &[Vec<solve::LinearOp>],
    residual_to_implicit_rows: &[Option<usize>],
    state_scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    let (rows, output_indices) = uncovered_implicit_tail_rows(
        scalar_programs,
        residual_to_implicit_rows,
        state_scalar_count,
        span,
    )?;
    Ok(rows.is_empty() || !output_indices.iter().copied().eq(0..output_indices.len()))
}

fn uncovered_implicit_tail_rows(
    scalar_programs: &[Vec<solve::LinearOp>],
    residual_to_implicit_rows: &[Option<usize>],
    state_scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<(Vec<Vec<solve::LinearOp>>, Vec<usize>), LowerError> {
    let mut covered = implicit_rhs_vec_with_capacity(
        scalar_programs.len(),
        "implicit RHS covered row count",
        span,
    )?;
    covered.resize(scalar_programs.len(), false);
    for row in residual_to_implicit_rows.iter().filter_map(|row| *row) {
        let Some(covered_row) = covered.get_mut(row) else {
            return Err(implicit_rhs_contract_violation(
                format!("implicit RHS coverage row {row} is outside scalar program rows"),
                span,
            ));
        };
        *covered_row = true;
    }

    let tail_count = scalar_programs
        .len()
        .checked_sub(state_scalar_count)
        .ok_or_else(|| {
            implicit_rhs_contract_violation(
                format!(
                    "implicit RHS state row count {state_scalar_count} exceeds scalar program row count {}",
                    scalar_programs.len()
                ),
                span,
            )
        })?;
    let mut rows =
        implicit_rhs_vec_with_capacity(tail_count, "implicit RHS uncovered row count", span)?;
    let mut output_indices =
        implicit_rhs_vec_with_capacity(tail_count, "implicit RHS uncovered output count", span)?;
    for (index, row) in scalar_programs.iter().enumerate().skip(state_scalar_count) {
        if covered[index] {
            continue;
        }
        rows.push(row.clone());
        output_indices.push(index);
    }
    Ok((rows, output_indices))
}

fn remap_residual_compute_nodes(
    residual_block: &solve::ComputeBlock,
    residual_to_implicit_rows: &[Option<usize>],
    implicit_output_cursor: usize,
    context_span: rumoca_core::Span,
) -> Result<Option<Vec<solve::ComputeNode>>, LowerError> {
    if residual_block.nodes.is_empty() {
        return Ok(Some(Vec::new()));
    }
    let span = compute_block_first_span(residual_block).unwrap_or(context_span);
    let mut nodes = implicit_rhs_vec_with_capacity(
        residual_block.nodes.len(),
        "residual compute node remap count",
        span,
    )?;
    let mut residual_output_cursor = 0usize;
    let mut implicit_output_cursor = implicit_output_cursor;
    for (node_index, node) in residual_block.nodes.iter().enumerate() {
        let residual_indices =
            residual_compute_node_output_indices(node, node_index, residual_output_cursor, span)?;
        let Some(implicit_indices) =
            remap_residual_output_indices(&residual_indices, residual_to_implicit_rows, span)?
        else {
            return Ok(None);
        };
        let Some(remapped) =
            remap_residual_compute_node(node, &implicit_indices, implicit_output_cursor, span)?
        else {
            return Ok(None);
        };
        residual_output_cursor =
            next_output_cursor_from_indices(&residual_indices, residual_output_cursor, span)?;
        implicit_output_cursor =
            remapped_compute_node_next_output_cursor(&remapped, implicit_output_cursor, span)?;
        nodes.push(remapped);
    }
    Ok(Some(nodes))
}

fn remap_residual_compute_node(
    node: &solve::ComputeNode,
    implicit_indices: &[usize],
    implicit_output_cursor: usize,
    _context_span: rumoca_core::Span,
) -> Result<Option<solve::ComputeNode>, LowerError> {
    match node {
        solve::ComputeNode::ScalarPrograms(block) => {
            if block.output_indices.is_empty() {
                return Ok(Some(solve::ComputeNode::ScalarPrograms(
                    solve::ScalarProgramBlock::with_output_indices(
                        block.programs.clone(),
                        block.program_spans.clone(),
                        Vec::new(),
                    )?,
                )));
            }
            Ok(Some(solve::ComputeNode::ScalarPrograms(
                solve::ScalarProgramBlock::with_output_indices(
                    block.programs.clone(),
                    block.program_spans.clone(),
                    implicit_indices.to_vec(),
                )?,
            )))
        }
        solve::ComputeNode::Map {
            domain,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
            ..
        } => Ok(
            stencil::tensor_output_map_from_values(domain, implicit_indices, *span)?.map(
                |output_map| solve::ComputeNode::Map {
                    domain: domain.clone(),
                    output_map,
                    base_ops: base_ops.clone(),
                    load_strides: load_strides.clone(),
                    const_strides: const_strides.clone(),
                    metadata: metadata.clone(),
                    span: *span,
                },
            ),
        ),
        solve::ComputeNode::AffineStencil {
            domain,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
            ..
        } => Ok(
            stencil::tensor_output_map_from_values(domain, implicit_indices, *span)?.map(
                |output_map| solve::ComputeNode::AffineStencil {
                    domain: domain.clone(),
                    output_map,
                    base_ops: base_ops.clone(),
                    load_strides: load_strides.clone(),
                    const_strides: const_strides.clone(),
                    metadata: metadata.clone(),
                    span: *span,
                },
            ),
        ),
        solve::ComputeNode::MatMul { .. } | solve::ComputeNode::LinSolve { .. } => Ok(
            contiguous_at_cursor(implicit_indices, implicit_output_cursor).then(|| node.clone()),
        ),
    }
}

fn remap_residual_output_indices(
    residual_indices: &[usize],
    residual_to_implicit_rows: &[Option<usize>],
    span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    let mut implicit_indices = implicit_rhs_vec_with_capacity(
        residual_indices.len(),
        "tensor residual output remap count",
        span,
    )?;
    for index in residual_indices {
        let Some(implicit_index) = residual_to_implicit_rows.get(*index).copied().flatten() else {
            return Ok(None);
        };
        implicit_indices.push(implicit_index);
    }
    Ok(Some(implicit_indices))
}

fn residual_compute_node_output_indices(
    node: &solve::ComputeNode,
    node_index: usize,
    output_cursor: usize,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    match node {
        solve::ComputeNode::ScalarPrograms(block) => block
            .compute_block_output_indices("implicit RHS residual remap", node_index, output_cursor)
            .map_err(shape_contract_lower_error),
        solve::ComputeNode::Map {
            domain, output_map, ..
        }
        | solve::ComputeNode::AffineStencil {
            domain, output_map, ..
        } => output_map.output_indices(domain).map_err(|err| {
            implicit_rhs_contract_violation(
                format!("implicit RHS residual tensor output map is invalid: {err:?}"),
                span,
            )
        }),
        solve::ComputeNode::MatMul { m, n, .. } => {
            contiguous_output_indices(output_cursor, checked_output_product(*m, *n, span)?, span)
        }
        solve::ComputeNode::LinSolve { n, .. } => {
            contiguous_output_indices(output_cursor, *n, span)
        }
    }
}

fn remapped_compute_node_next_output_cursor(
    node: &solve::ComputeNode,
    output_cursor: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    let indices = residual_compute_node_output_indices(node, 0, output_cursor, span)?;
    next_output_cursor_from_indices(&indices, output_cursor, span)
}

fn next_output_cursor_from_indices(
    indices: &[usize],
    fallback: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    indices.iter().copied().max().map_or(Ok(fallback), |index| {
        index.checked_add(1).ok_or_else(|| {
            implicit_rhs_contract_violation("implicit RHS output cursor overflows", span)
        })
    })
}

fn contiguous_output_indices(
    start: usize,
    count: usize,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let end = start.checked_add(count).ok_or_else(|| {
        implicit_rhs_contract_violation("implicit RHS contiguous output range overflows", span)
    })?;
    let mut indices =
        implicit_rhs_vec_with_capacity(count, "implicit RHS contiguous output count", span)?;
    indices.extend(start..end);
    Ok(indices)
}

fn checked_output_product(
    lhs: usize,
    rhs: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    lhs.checked_mul(rhs).ok_or_else(|| {
        implicit_rhs_contract_violation("implicit RHS tensor output count overflows", span)
    })
}

fn contiguous_at_cursor(indices: &[usize], cursor: usize) -> bool {
    indices
        .iter()
        .copied()
        .enumerate()
        .all(|(offset, index)| cursor.checked_add(offset) == Some(index))
}

// SPEC_0021: Exception - residual placement mutates row storage, target maps,
// occupancy, and remap inventory atomically.
#[allow(clippy::too_many_arguments)]
fn place_targeted_residual_rows(
    rows: &mut [Vec<solve::LinearOp>],
    row_targets: &mut [Option<solve::ScalarSlot>],
    occupied: &mut [bool],
    residual: &[Vec<solve::LinearOp>],
    residual_targets: &[Option<solve::ScalarSlot>],
    state_scalar_count: usize,
    solver_scalar_count: usize,
    residual_to_implicit_rows: &mut [Option<usize>],
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    let mut placed =
        implicit_rhs_vec_with_capacity(residual.len(), "targeted residual placement count", span)?;
    placed.resize(residual.len(), false);
    for (residual_idx, row) in residual.iter().enumerate() {
        let Some(solve::ScalarSlot::Y { index, .. }) =
            residual_targets.get(residual_idx).copied().flatten()
        else {
            continue;
        };
        if index < state_scalar_count || index >= solver_scalar_count || occupied[index] {
            continue;
        }
        rows[index] = row.clone();
        row_targets[index] = Some(implicit_rhs_y_slot(index, span)?);
        occupied[index] = true;
        placed[residual_idx] = true;
        residual_to_implicit_rows[residual_idx] = Some(index);
    }

    let mut fallback_idx = state_scalar_count;
    for (residual_idx, row) in residual.iter().enumerate() {
        if placed[residual_idx] {
            continue;
        }
        let residual_target = residual_targets.get(residual_idx).copied().flatten();
        let fallback_row_target =
            fallback_residual_row_target(residual_target, state_scalar_count, span)?;
        let target_idx =
            next_fallback_implicit_row(occupied, &mut fallback_idx, solver_scalar_count);
        if let Some(target_idx) = target_idx {
            rows[target_idx] = row.clone();
            row_targets[target_idx] = fallback_row_target;
            occupied[target_idx] = true;
            residual_to_implicit_rows[residual_idx] = Some(target_idx);
        }
    }
    Ok(())
}

fn fallback_residual_row_target(
    residual_target: Option<solve::ScalarSlot>,
    state_scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<Option<solve::ScalarSlot>, LowerError> {
    let Some(solve::ScalarSlot::Y { index, .. }) = residual_target else {
        return Ok(None);
    };
    if index < state_scalar_count {
        return implicit_rhs_y_slot(index, span).map(Some);
    }
    Ok(None)
}

fn implicit_rhs_y_slot(
    index: usize,
    span: rumoca_core::Span,
) -> Result<solve::ScalarSlot, LowerError> {
    let byte_offset = index
        .checked_mul(std::mem::size_of::<f64>())
        .ok_or_else(|| {
            implicit_rhs_contract_violation(
                format!("implicit RHS Y target byte offset for slot {index} overflows"),
                span,
            )
        })?;
    Ok(solve::ScalarSlot::Y { index, byte_offset })
}

fn next_fallback_implicit_row(
    occupied: &[bool],
    fallback_idx: &mut usize,
    solver_scalar_count: usize,
) -> Option<usize> {
    while *fallback_idx < solver_scalar_count {
        let idx = *fallback_idx;
        *fallback_idx += 1;
        if !occupied[idx] {
            return Some(idx);
        }
    }
    None
}

#[cfg(test)]
pub(crate) fn zero_rhs_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 0.0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_implicit_rhs_fixture.mo"),
            1,
            2,
        )
    }

    #[test]
    fn uncovered_implicit_tail_rows_rejects_state_prefix_overflow() -> Result<(), LowerError> {
        let span = test_span();
        let err = uncovered_implicit_tail_rows(&[zero_rhs_row()], &[], 2, span)
            .expect_err("state prefix larger than scalar rows must fail");
        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        Ok(())
    }

    #[test]
    fn uncovered_implicit_tail_rows_rejects_out_of_range_coverage() -> Result<(), LowerError> {
        let span = test_span();
        let err = uncovered_implicit_tail_rows(&[zero_rhs_row()], &[Some(1)], 0, span)
            .expect_err("coverage row outside scalar rows must fail");
        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        Ok(())
    }

    #[test]
    fn fallback_residual_row_target_rejects_byte_offset_overflow() {
        let span = test_span();
        let index = usize::MAX / std::mem::size_of::<f64>() + 1;
        let err = fallback_residual_row_target(
            Some(solve::ScalarSlot::Y {
                index,
                byte_offset: 0,
            }),
            usize::MAX,
            span,
        )
        .expect_err("fallback row target should reject byte offset overflow");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(
            err.to_string()
                .contains("implicit RHS Y target byte offset for slot")
        );
    }

    #[test]
    fn fallback_residual_row_target_checks_byte_offset() {
        let target = fallback_residual_row_target(Some(solve::scalar_slot_y(2)), 3, test_span())
            .expect("valid fallback row target should build");

        assert_eq!(
            target,
            Some(solve::ScalarSlot::Y {
                index: 2,
                byte_offset: 2 * std::mem::size_of::<f64>(),
            })
        );
    }

    #[test]
    fn targeted_residual_row_rebuilds_checked_target_slot() -> Result<(), LowerError> {
        let mut rows = vec![zero_rhs_row(), zero_rhs_row(), zero_rhs_row()];
        let mut row_targets = vec![None, None, None];
        let mut occupied = vec![true, false, false];
        let residual = vec![zero_rhs_row()];
        let residual_targets = vec![Some(solve::ScalarSlot::Y {
            index: 2,
            byte_offset: usize::MAX,
        })];
        let mut residual_to_implicit_rows = vec![None];

        place_targeted_residual_rows(
            &mut rows,
            &mut row_targets,
            &mut occupied,
            &residual,
            &residual_targets,
            1,
            3,
            &mut residual_to_implicit_rows,
            test_span(),
        )?;

        assert_eq!(
            row_targets[2],
            Some(solve::ScalarSlot::Y {
                index: 2,
                byte_offset: 2 * std::mem::size_of::<f64>(),
            })
        );
        assert_eq!(residual_to_implicit_rows, vec![Some(2)]);
        Ok(())
    }

    #[test]
    fn remapped_implicit_rhs_preserves_contiguous_matmul_residual() -> Result<(), LowerError> {
        let span = test_span();
        let derivative_rhs = derivative_compute_block(span);
        let residual_block = solve::ComputeBlock {
            nodes: vec![two_output_matmul_node(span)],
        };
        let scalar_programs = vec![zero_rhs_row(), zero_rhs_row(), zero_rhs_row()];
        let residual_to_implicit_rows = vec![Some(1), Some(2)];

        let block = build_implicit_rhs_compute_block(
            &derivative_rhs,
            &residual_block,
            &residual_to_implicit_rows,
            scalar_programs,
            1,
            span,
        )?;

        assert_eq!(block.compute_node_counts().matmul, 1);
        assert_eq!(block.len().map_err(shape_contract_lower_error)?, 3);
        Ok(())
    }

    #[test]
    fn remapped_implicit_rhs_scalarizes_sparse_matmul_residual() -> Result<(), LowerError> {
        let span = test_span();
        let derivative_rhs = derivative_compute_block(span);
        let residual_block = solve::ComputeBlock {
            nodes: vec![two_output_matmul_node(span)],
        };
        let scalar_programs = vec![
            zero_rhs_row(),
            zero_rhs_row(),
            zero_rhs_row(),
            zero_rhs_row(),
        ];
        let residual_to_implicit_rows = vec![Some(1), Some(3)];

        let block = build_implicit_rhs_compute_block(
            &derivative_rhs,
            &residual_block,
            &residual_to_implicit_rows,
            scalar_programs,
            1,
            span,
        )?;

        assert_eq!(block.compute_node_counts().matmul, 0);
        assert_eq!(block.len().map_err(shape_contract_lower_error)?, 4);
        Ok(())
    }

    fn derivative_compute_block(span: rumoca_core::Span) -> solve::ComputeBlock {
        solve::ComputeBlock::from_scalar_program_block(solve::ScalarProgramBlock::with_source_span(
            vec![zero_rhs_row()],
            span,
        ))
    }

    fn two_output_matmul_node(span: rumoca_core::Span) -> solve::ComputeNode {
        solve::ComputeNode::MatMul {
            lhs_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 2.0 },
                solve::LinearOp::Const { dst: 1, value: 3.0 },
            ],
            lhs_start: 0,
            rhs_ops: vec![
                solve::LinearOp::Const { dst: 2, value: 5.0 },
                solve::LinearOp::Const { dst: 3, value: 7.0 },
                solve::LinearOp::Const {
                    dst: 4,
                    value: 11.0,
                },
                solve::LinearOp::Const {
                    dst: 5,
                    value: 13.0,
                },
            ],
            rhs_start: 2,
            m: 1,
            k: 2,
            n: 2,
            lhs_sparsity: solve::SparsityPattern::Dense,
            rhs_sparsity: solve::SparsityPattern::Dense,
            metadata: solve::TensorNodeMetadata::default(),
            span,
        }
    }
}
