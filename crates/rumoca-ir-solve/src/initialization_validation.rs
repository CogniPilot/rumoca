use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct InitializationTargetRange {
    pub start: usize,
    pub end: usize,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub span: Option<Span>,
}

/// Count stored initialization rows without expanding tensor output maps.
/// Shape validation independently owns tensor-map placement validity.
pub(super) fn initialization_stored_row_count(
    residual: &ComputeBlock,
    context: &'static str,
) -> Result<usize, SolveProblemShapeContractError> {
    let mut rows = 0usize;
    for (node_index, node) in residual.nodes.iter().enumerate() {
        let (count, span) = match node {
            ComputeNode::ScalarPrograms(block) => {
                (block.stored_output_count(), block.first_source_span())
            }
            ComputeNode::Map { domain, span, .. }
            | ComputeNode::AffineStencil { domain, span, .. } => (
                domain.scalar_count().map_err(|error| {
                    SolveProblemShapeContractError::StructuredIndexDomain {
                        context: context.to_string(),
                        node_index,
                        dimension: "stored-row",
                        error,
                        span: *span,
                    }
                })?,
                Some(*span),
            ),
            ComputeNode::MatMul { m, n, span, .. } => (
                m.checked_mul(*n)
                    .ok_or_else(|| output_index_overflow(context, node_index, Some(*span)))?,
                Some(*span),
            ),
            ComputeNode::LinSolve { n, span, .. } => (*n, Some(*span)),
        };
        rows = rows
            .checked_add(count)
            .ok_or_else(|| output_index_overflow(context, node_index, span))?;
    }
    Ok(rows)
}

pub(super) fn validate_initialization_direct_families(
    initialization: &InitializationSolveSystem,
    y_upper_bound: usize,
    residual_row_count: usize,
) -> Result<(), SolveProblemShapeContractError> {
    if initialization.direct_families.is_empty() {
        return validate_initialization_without_direct_families(
            initialization,
            y_upper_bound,
            residual_row_count,
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
    let direct_ranges = target_ranges
        .into_iter()
        .map(|(range, _, span)| InitializationTargetRange {
            start: range.start,
            end: range.end,
            span: Some(span),
        })
        .collect::<Vec<_>>();
    let required = normalized_ranges(
        &initialization.required_target_ranges,
        y_upper_bound,
        "invalid required target range",
    )?;
    let complete_required = if y_upper_bound == 0 {
        Vec::new()
    } else {
        vec![InitializationTargetRange {
            start: 0,
            end: y_upper_bound,
            span: required.first().and_then(|range| range.span),
        }]
    };
    if !same_target_coverage(&required, &complete_required) {
        return Err(initialization_range_error_at(
            "incomplete required target coverage of the solver Y vector",
            required.first().and_then(|range| range.span),
        ));
    }
    let fixed = normalized_ranges(
        &initialization.fixed_target_ranges,
        y_upper_bound,
        "invalid fixed-start target range",
    )?;
    let mut actual = direct_ranges;
    actual.extend(fixed);
    let actual = normalized_ranges(&actual, y_upper_bound, "invalid target union range")?;
    if !same_target_coverage(&actual, &required) {
        return Err(initialization_range_error_at(
            "incomplete direct plus fixed-start target union",
            actual
                .first()
                .and_then(|range| range.span)
                .or_else(|| required.first().and_then(|range| range.span)),
        ));
    }
    Ok(())
}

fn validate_initialization_without_direct_families(
    initialization: &InitializationSolveSystem,
    y_upper_bound: usize,
    residual_row_count: usize,
) -> Result<(), SolveProblemShapeContractError> {
    validate_count(
        "initialization.row_targets",
        residual_row_count,
        initialization.row_targets.len(),
    )?;
    if initialization.residual.is_empty() && initialization.row_targets.is_empty() {
        let required = normalized_ranges(
            &initialization.required_target_ranges,
            y_upper_bound,
            "invalid required target range",
        )?;
        let fixed = normalized_ranges(
            &initialization.fixed_target_ranges,
            y_upper_bound,
            "invalid fixed-start target range",
        )?;
        if !same_target_coverage(&required, &fixed) {
            return Err(initialization_range_error_at(
                "incomplete fixed-start target union",
                fixed
                    .first()
                    .and_then(|range| range.span)
                    .or_else(|| required.first().and_then(|range| range.span)),
            ));
        }
    } else if !initialization.required_target_ranges.is_empty()
        || !initialization.fixed_target_ranges.is_empty()
    {
        return Err(initialization_range_error(
            "target coverage metadata without compact direct families",
        ));
    }
    Ok(())
}

fn normalized_ranges(
    ranges: &[InitializationTargetRange],
    upper_bound: usize,
    error: &'static str,
) -> Result<Vec<InitializationTargetRange>, SolveProblemShapeContractError> {
    let mut ranges = ranges.to_vec();
    ranges.sort_unstable_by_key(|range| (range.start, range.end));
    let mut normalized: Vec<InitializationTargetRange> = Vec::with_capacity(ranges.len());
    for range in ranges {
        if range.start >= range.end || range.end > upper_bound {
            return Err(initialization_range_error_at(error, range.span));
        }
        if let Some(last) = normalized.last_mut() {
            if range.start < last.end {
                return Err(initialization_range_error_at(
                    "overlapping initialization target ranges",
                    range.span.or(last.span),
                ));
            }
            if range.start == last.end {
                last.end = range.end;
                continue;
            }
        }
        normalized.push(range);
    }
    Ok(normalized)
}

fn initialization_range_error(reason: &'static str) -> SolveProblemShapeContractError {
    initialization_range_error_at(reason, None)
}

fn initialization_range_error_at(
    reason: &'static str,
    span: Option<Span>,
) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::InitializationTargetCoverage { reason, span }
}

fn same_target_coverage(
    left: &[InitializationTargetRange],
    right: &[InitializationTargetRange],
) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| left.start == right.start && left.end == right.end)
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
