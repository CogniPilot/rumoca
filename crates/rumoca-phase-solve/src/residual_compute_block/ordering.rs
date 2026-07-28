use super::*;

type NativeResidualNode = (usize, usize, solve::ComputeNode);

pub(super) struct ResidualAssemblyInput<'a> {
    pub rows: &'a mut Vec<stencil::StructuredProgram>,
    pub native_nodes: Vec<NativeResidualNode>,
    pub direct_nodes: Vec<solve::ComputeNode>,
    pub compact_coverage: &'a [CompactOutputCoverage],
    pub structured_equations: &'a [dae::StructuredEquationFamily],
    pub dae_equations: &'a [dae::Equation],
    pub declines: &'a mut crate::tensor_declines::TensorDeclineJournal,
    pub expected_output_count: usize,
    pub span: Option<rumoca_core::Span>,
}

pub(super) fn assemble_residual_compute_block(
    input: ResidualAssemblyInput<'_>,
) -> Result<solve::ComputeBlock, LowerError> {
    let ResidualAssemblyInput {
        rows,
        native_nodes,
        direct_nodes,
        compact_coverage,
        structured_equations,
        dae_equations,
        declines,
        expected_output_count,
        span,
    } = input;
    let mut row_nodes = Vec::new();
    push_rows_partitioned_at_native_boundaries(
        &mut row_nodes,
        rows,
        &native_nodes,
        structured_equations,
        dae_equations,
        declines,
    )?;
    let mut ordered = collect_ordered_nodes(
        row_nodes,
        direct_nodes,
        compact_coverage,
        native_nodes,
        span,
    )?;
    ordered.sort_by_key(|node| node.min_output);

    let mut block = solve::ComputeBlock {
        nodes: Vec::with_capacity(ordered.len()),
    };
    for ordered_node in ordered {
        verify_native_output_cursor(&block, &ordered_node, span)?;
        block.nodes.push(ordered_node.node);
    }
    verify_residual_output_count(&block, expected_output_count, span)?;
    Ok(block)
}

fn push_rows_partitioned_at_native_boundaries(
    nodes: &mut Vec<solve::ComputeNode>,
    rows: &mut Vec<stencil::StructuredProgram>,
    native_nodes: &[NativeResidualNode],
    structured_equations: &[dae::StructuredEquationFamily],
    dae_equations: &[dae::Equation],
    declines: &mut crate::tensor_declines::TensorDeclineJournal,
) -> Result<(), LowerError> {
    let mut pending = std::mem::take(rows);
    for (native_start, _, _) in native_nodes {
        let split = pending.partition_point(|row| row.output_index < *native_start);
        let trailing = pending.split_off(split);
        stencil::push_structured_programs(
            nodes,
            &mut pending,
            structured_equations,
            dae_equations,
            declines,
        )?;
        pending = trailing;
    }
    stencil::push_structured_programs(
        nodes,
        &mut pending,
        structured_equations,
        dae_equations,
        declines,
    )?;
    Ok(())
}

fn collect_ordered_nodes(
    row_nodes: Vec<solve::ComputeNode>,
    direct_nodes: Vec<solve::ComputeNode>,
    compact_coverage: &[CompactOutputCoverage],
    native_nodes: Vec<NativeResidualNode>,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<OrderedResidualNode>, LowerError> {
    if direct_nodes.len() != compact_coverage.len() {
        return Err(residual_contract_error(
            format!(
                "direct residual node count {} does not match coverage count {}",
                direct_nodes.len(),
                compact_coverage.len()
            ),
            span,
        ));
    }
    let ordered_capacity = row_nodes
        .len()
        .checked_add(direct_nodes.len())
        .and_then(|count| count.checked_add(native_nodes.len()))
        .ok_or_else(|| {
            residual_contract_error(
                "residual compute node count overflows host index range".to_string(),
                span,
            )
        })?;
    let mut ordered = residual_vec_with_capacity(
        ordered_capacity,
        "ordered residual compute node count",
        span,
    )?;
    for node in row_nodes {
        if let Some(min_output) = residual_node_min_output(&node) {
            ordered.push(OrderedResidualNode {
                min_output,
                native_start: None,
                node,
            });
        }
    }
    for (node, coverage) in direct_nodes
        .into_iter()
        .zip(compact_coverage.iter().copied())
    {
        ordered.push(OrderedResidualNode {
            min_output: coverage.start,
            native_start: None,
            node,
        });
    }
    for (start, _, node) in native_nodes {
        ordered.push(OrderedResidualNode {
            min_output: start,
            native_start: Some(start),
            node,
        });
    }
    Ok(ordered)
}

struct OrderedResidualNode {
    min_output: usize,
    native_start: Option<usize>,
    node: solve::ComputeNode,
}

fn residual_node_min_output(node: &solve::ComputeNode) -> Option<usize> {
    match node {
        solve::ComputeNode::ScalarPrograms(block) => block.output_indices.iter().copied().min(),
        solve::ComputeNode::Map { output_map, .. }
        | solve::ComputeNode::AffineStencil { output_map, .. } => Some(output_map.start),
        solve::ComputeNode::MatMul { .. } | solve::ComputeNode::LinSolve { .. } => Some(0),
    }
}

fn verify_native_output_cursor(
    block: &solve::ComputeBlock,
    ordered_node: &OrderedResidualNode,
    span: Option<rumoca_core::Span>,
) -> Result<(), LowerError> {
    let Some(expected_start) = ordered_node.native_start else {
        return Ok(());
    };
    let actual_start = block.len().map_err(|error| {
        residual_contract_error(
            format!("residual output placement is invalid: {error}"),
            error.source_span().or(span),
        )
    })?;
    if actual_start == expected_start {
        return Ok(());
    }
    Err(residual_contract_error(
        format!(
            "native residual MatMul starts at output {expected_start}, \
             but preceding residual nodes advance to {actual_start}"
        ),
        residual_node_span(&ordered_node.node),
    ))
}

fn verify_residual_output_count(
    block: &solve::ComputeBlock,
    expected_output_count: usize,
    span: Option<rumoca_core::Span>,
) -> Result<(), LowerError> {
    let output_count = block.len().map_err(|error| {
        residual_contract_error(
            format!("residual compute block shape is invalid: {error}"),
            error.source_span().or(span),
        )
    })?;
    if output_count == expected_output_count {
        return Ok(());
    }
    Err(residual_contract_error(
        format!(
            "residual compute block produces {output_count} outputs for \
             {expected_output_count} residual rows"
        ),
        span,
    ))
}

fn residual_node_span(node: &solve::ComputeNode) -> Option<rumoca_core::Span> {
    match node {
        solve::ComputeNode::ScalarPrograms(block) => block.first_source_span(),
        solve::ComputeNode::MatMul { span, .. }
        | solve::ComputeNode::LinSolve { span, .. }
        | solve::ComputeNode::Map { span, .. }
        | solve::ComputeNode::AffineStencil { span, .. } => Some(*span),
    }
}
