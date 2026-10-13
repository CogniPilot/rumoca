//! Checked scalar output-row positions shared by refresh source and target catalogs.

use super::*;

pub(super) fn first_block_span(block: &solve::ScalarProgramBlock) -> Option<rumoca_core::Span> {
    block.first_source_span()
}

#[derive(Clone, Copy)]
pub(super) struct OutputRowPosition {
    pub(super) program_index: usize,
    pub(super) output_offset: usize,
}

pub(super) fn output_row_positions(
    block: &solve::ScalarProgramBlock,
) -> Result<IndexMap<usize, OutputRowPosition>, EvalSolveError> {
    let span = first_block_span(block);
    let mut positions = IndexMap::new();
    reserve_refresh_index_map_capacity(
        &mut positions,
        block.output_indices().len(),
        "output-row position map",
        span,
    )?;
    let mut output_ordinal = 0usize;
    for (program_index, program) in block.programs().iter().enumerate() {
        let output_count = solve::ScalarProgramBlock::program_output_count(program);
        for output_offset in 0..output_count {
            let Some(output_index) = block.output_indices().get(output_ordinal).copied() else {
                return Err(EvalSolveError::InvalidRow {
                    message: format!(
                        "program output ordinal {output_ordinal} is missing scalar output metadata"
                    ),
                    span: block.program_span(program_index),
                });
            };
            output_ordinal =
                output_ordinal
                    .checked_add(1)
                    .ok_or_else(|| EvalSolveError::InvalidRow {
                        message: "program output ordinal overflows host index limits".to_string(),
                        span,
                    })?;
            if let Some(previous) = positions.insert(
                output_index,
                OutputRowPosition {
                    program_index,
                    output_offset,
                },
            ) {
                return Err(EvalSolveError::InvalidRow {
                    message: format!(
                        "duplicate scalar program output row {output_index}: first at program {} output {}, repeated at program {program_index} output {output_offset}",
                        previous.program_index, previous.output_offset
                    ),
                    span: block.program_span(program_index),
                });
            }
        }
    }
    if output_ordinal != block.output_indices().len() {
        return Err(EvalSolveError::InvalidRow {
            message: format!(
                "scalar program block has {} output indices but {output_ordinal} StoreOutput ops",
                block.output_indices().len()
            ),
            span,
        });
    }
    Ok(positions)
}
