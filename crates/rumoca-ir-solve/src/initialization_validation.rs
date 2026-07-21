use super::*;

pub(super) fn validate_initialization_direct_families(
    initialization: &InitializationSolveSystem,
) -> Result<(), SolveProblemShapeContractError> {
    if initialization.direct_families.is_empty() {
        return validate_count(
            "initialization.row_targets",
            initialization.residual.len()?,
            initialization.row_targets.len(),
        );
    }
    validate_count(
        "initialization.row_targets.compact",
        0,
        initialization.row_targets.len(),
    )?;
    validate_count(
        "initialization.direct_families",
        initialization.residual.nodes.len(),
        initialization.direct_families.len(),
    )?;
    let mut covered_nodes = vec![false; initialization.residual.nodes.len()];
    let mut target_ranges = Vec::with_capacity(initialization.direct_families.len());
    for family in &initialization.direct_families {
        let Some(covered_node) = covered_nodes.get_mut(family.node_index) else {
            return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                context: "initialization.direct_families".to_string(),
                node_index: family.node_index,
                dimension: "direct-family node index outside residual block",
                span: family.span,
            });
        };
        if std::mem::replace(covered_node, true) {
            return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                context: "initialization.direct_families".to_string(),
                node_index: family.node_index,
                dimension: "duplicate direct-family node index",
                span: family.span,
            });
        }
        let target_range = validate_initialization_direct_family(initialization, family)?;
        target_ranges.push((target_range, family.node_index, family.span));
    }
    target_ranges.sort_unstable_by_key(|(range, _, _)| range.start);
    for adjacent in target_ranges.windows(2) {
        let [(left, _, _), (right, node_index, span)] = adjacent else {
            unreachable!("windows(2) always has two entries")
        };
        if right.start < left.end {
            return Err(SolveProblemShapeContractError::ZeroTensorDimension {
                context: "initialization.direct_families".to_string(),
                node_index: *node_index,
                dimension: "overlapping direct-family target map",
                span: *span,
            });
        }
    }
    Ok(())
}

fn validate_initialization_direct_family(
    initialization: &InitializationSolveSystem,
    family: &InitializationDirectFamily,
) -> Result<std::ops::Range<usize>, SolveProblemShapeContractError> {
    let Some(node) = initialization.residual.nodes.get(family.node_index) else {
        return Err(SolveProblemShapeContractError::ZeroTensorDimension {
            context: "initialization.direct_families".to_string(),
            node_index: family.node_index,
            dimension: "direct-family node index outside residual block",
            span: family.span,
        });
    };
    let ComputeNode::Map { domain, span, .. } = node else {
        return Err(SolveProblemShapeContractError::ZeroTensorDimension {
            context: "initialization.direct_families".to_string(),
            node_index: family.node_index,
            dimension: "non-Map direct family",
            span: family.span,
        });
    };
    let dense =
        TensorOutputMap::dense_contiguous(family.targets.start, domain).map_err(|error| {
            tensor_output_map_error(
                "initialization.direct_families.targets",
                family.node_index,
                "Map",
                error,
                *span,
            )
        })?;
    if family.targets.strides != dense.strides {
        return Err(SolveProblemShapeContractError::ZeroTensorDimension {
            context: "initialization.direct_families.targets".to_string(),
            node_index: family.node_index,
            dimension: "non-contiguous direct-family target map",
            span: *span,
        });
    }
    let count = domain.scalar_count().map_err(|error| {
        SolveProblemShapeContractError::StructuredIndexDomain {
            context: "initialization.direct_families.targets".to_string(),
            node_index: family.node_index,
            dimension: "Map",
            error,
            span: *span,
        }
    })?;
    let end = family.targets.start.checked_add(count).ok_or_else(|| {
        output_index_overflow(
            "initialization.direct_families.targets",
            family.node_index,
            Some(*span),
        )
    })?;
    Ok(family.targets.start..end)
}
