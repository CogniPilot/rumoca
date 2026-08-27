use indexmap::IndexMap;
use rumoca_ir_solve as solve;
use std::ops::Range;

use super::{OutputRowPosition, first_block_span, reserve_refresh_index_map_capacity};
use crate::{EvalSolveError, scalar_program_output_count, scalar_program_output_indices};

pub(super) struct CanonicalScalarProgram<'a> {
    pub(super) source: solve::RefreshScalarProgramSource,
    pub(super) operations: &'a [solve::LinearOp],
    pub(super) span: rumoca_core::Span,
}

pub(super) struct CanonicalScalarProgramCatalog<'a> {
    programs: Vec<CanonicalScalarProgram<'a>>,
    positions: IndexMap<usize, OutputRowPosition>,
    output_ranges: Vec<Range<usize>>,
    first_span: Option<rumoca_core::Span>,
}

impl<'a> CanonicalScalarProgramCatalog<'a> {
    pub(super) fn construct(block: &'a solve::ComputeBlock) -> Result<Self, EvalSolveError> {
        let first_span = block.nodes.iter().find_map(compute_node_span);
        let mut programs = Vec::new();
        let mut positions = IndexMap::new();
        let mut output_ranges = Vec::new();
        let mut output_cursor = 0usize;
        for (node_index, node) in block.nodes.iter().enumerate() {
            output_cursor = append_compute_node(
                node_index,
                node,
                output_cursor,
                &mut programs,
                &mut positions,
                &mut output_ranges,
            )?;
        }
        Ok(Self {
            programs,
            positions,
            output_ranges,
            first_span,
        })
    }

    pub(super) fn program(&self, index: usize) -> Option<&CanonicalScalarProgram<'a>> {
        self.programs.get(index)
    }

    pub(super) fn source_index(&self, source: solve::RefreshScalarProgramSource) -> Option<usize> {
        self.programs
            .iter()
            .position(|program| program.source == source)
    }

    pub(super) fn positions(&self) -> &IndexMap<usize, OutputRowPosition> {
        &self.positions
    }

    pub(super) fn produces_output(&self, output: usize) -> bool {
        self.output_ranges
            .iter()
            .any(|range| range.contains(&output))
    }

    pub(super) const fn first_span(&self) -> Option<rumoca_core::Span> {
        self.first_span
    }

    pub(super) fn len(&self) -> usize {
        self.programs.len()
    }
}

fn append_compute_node<'a>(
    node_index: usize,
    node: &'a solve::ComputeNode,
    output_cursor: usize,
    programs: &mut Vec<CanonicalScalarProgram<'a>>,
    positions: &mut IndexMap<usize, OutputRowPosition>,
    output_ranges: &mut Vec<Range<usize>>,
) -> Result<usize, EvalSolveError> {
    match node {
        solve::ComputeNode::ScalarPrograms(scalar) => {
            append_scalar_programs(
                node_index,
                scalar,
                output_cursor,
                programs,
                positions,
                output_ranges,
            )?;
            Ok(output_cursor.max(scalar_program_output_count(
                scalar,
                output_cursor,
                "refresh source catalog",
            )?))
        }
        solve::ComputeNode::MatMul { m, n, span, .. } => {
            let count = m
                .checked_mul(*n)
                .ok_or_else(|| source_error("matrix output range overflows", Some(*span)))?;
            append_cursor_output_range(
                output_ranges,
                output_cursor,
                count,
                "matrix output cursor overflows",
                *span,
            )
        }
        solve::ComputeNode::LinSolve { n, span, .. } => append_cursor_output_range(
            output_ranges,
            output_cursor,
            *n,
            "linear-solve output cursor overflows",
            *span,
        ),
        solve::ComputeNode::Map {
            domain,
            output_map,
            span,
            ..
        }
        | solve::ComputeNode::AffineStencil {
            domain,
            output_map,
            span,
            ..
        } => append_compact_output_range(output_ranges, output_cursor, domain, output_map, *span),
    }
}

fn append_cursor_output_range(
    output_ranges: &mut Vec<Range<usize>>,
    output_cursor: usize,
    count: usize,
    overflow_message: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, EvalSolveError> {
    let end = output_cursor
        .checked_add(count)
        .ok_or_else(|| source_error(overflow_message, Some(span)))?;
    push_output_range(output_ranges, output_cursor..end);
    Ok(end)
}

fn append_compact_output_range(
    output_ranges: &mut Vec<Range<usize>>,
    output_cursor: usize,
    domain: &rumoca_core::StructuredIndexDomain,
    output_map: &solve::TensorOutputMap,
    span: rumoca_core::Span,
) -> Result<usize, EvalSolveError> {
    let dense =
        solve::TensorOutputMap::dense_contiguous(output_map.start, domain).map_err(|error| {
            source_error(
                format!("compact tensor output map is invalid: {error:?}"),
                Some(span),
            )
        })?;
    if dense != *output_map {
        return Err(source_error(
            "continuous refresh ownership requires a dense compact tensor output projection",
            Some(span),
        ));
    }
    let count = domain.scalar_count().map_err(|error| {
        source_error(
            format!("compact tensor domain is invalid: {error:?}"),
            Some(span),
        )
    })?;
    let end = output_map
        .start
        .checked_add(count)
        .ok_or_else(|| source_error("compact tensor output range overflows", Some(span)))?;
    push_output_range(output_ranges, output_map.start..end);
    Ok(output_cursor.max(end))
}

fn append_scalar_programs<'a>(
    node_index: usize,
    block: &'a solve::ScalarProgramBlock,
    output_cursor: usize,
    programs: &mut Vec<CanonicalScalarProgram<'a>>,
    positions: &mut IndexMap<usize, OutputRowPosition>,
    output_ranges: &mut Vec<Range<usize>>,
) -> Result<(), EvalSolveError> {
    let output_indices =
        scalar_program_output_indices(block, output_cursor, "refresh source catalog")?;
    reserve_refresh_index_map_capacity(
        positions,
        output_indices.len(),
        "refresh source output positions",
        first_block_span(block),
    )?;
    let mut output_ordinal = 0usize;
    for (program_index, operations) in block.programs().iter().enumerate() {
        let source = solve::RefreshScalarProgramSource::checked(node_index, program_index)
            .ok_or_else(|| {
                source_error(
                    "canonical scalar-program identity exceeds u32",
                    block.program_span(program_index),
                )
            })?;
        let catalog_index = programs.len();
        let span = block
            .program_span(program_index)
            .ok_or_else(|| source_error("canonical scalar program is missing provenance", None))?;
        programs.try_reserve_exact(1).map_err(|_| {
            source_error(
                "canonical scalar-program catalog exceeds memory",
                Some(span),
            )
        })?;
        programs.push(CanonicalScalarProgram {
            source,
            operations,
            span,
        });
        let count = solve::ScalarProgramBlock::program_output_count(operations);
        for output_offset in 0..count {
            let output_index = output_indices.get(output_ordinal).copied().ok_or_else(|| {
                source_error("canonical scalar output metadata is incomplete", Some(span))
            })?;
            output_ordinal = output_ordinal.checked_add(1).ok_or_else(|| {
                source_error("canonical scalar output ordinal overflows", Some(span))
            })?;
            if positions
                .insert(
                    output_index,
                    OutputRowPosition {
                        program_index: catalog_index,
                        output_offset,
                    },
                )
                .is_some()
            {
                return Err(source_error(
                    format!("canonical output {output_index} has multiple scalar producers"),
                    Some(span),
                ));
            }
            let output_end = output_index.checked_add(1).ok_or_else(|| {
                source_error("canonical scalar output range overflows", Some(span))
            })?;
            push_output_range(output_ranges, output_index..output_end);
        }
    }
    Ok(())
}

fn push_output_range(ranges: &mut Vec<Range<usize>>, range: Range<usize>) {
    if range.is_empty() {
        return;
    }
    if let Some(previous) = ranges.last_mut()
        && previous.end == range.start
    {
        previous.end = range.end;
        return;
    }
    ranges.push(range);
}

fn compute_node_span(node: &solve::ComputeNode) -> Option<rumoca_core::Span> {
    match node {
        solve::ComputeNode::ScalarPrograms(block) => block.first_source_span(),
        solve::ComputeNode::MatMul { span, .. }
        | solve::ComputeNode::LinSolve { span, .. }
        | solve::ComputeNode::Map { span, .. }
        | solve::ComputeNode::AffineStencil { span, .. } => Some(*span),
    }
}

fn source_error(message: impl Into<String>, span: Option<rumoca_core::Span>) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: message.into(),
        span,
    }
}
