//! Borrowed output identity lookup for one immutable refresh source block.

use super::{
    ComputeBlock, ComputeNode, ContinuousRefreshConstructionError, RefreshScalarProgramSource,
    ScalarProgramBlock, advance_output_cursor,
};
use std::ops::Range;

pub(super) struct SourceOutputs<'source> {
    nodes: Vec<Option<ScalarOutputs<'source>>>,
}

struct ScalarOutputs<'source> {
    block: &'source ScalarProgramBlock,
    local_offset: Option<usize>,
}

impl<'source> SourceOutputs<'source> {
    pub(super) fn store_contiguous_range(
        &self,
        source: RefreshScalarProgramSource,
        operation: usize,
    ) -> Option<Range<usize>> {
        let outputs = self.nodes.get(source.node as usize)?.as_ref()?;
        let span = outputs
            .block
            .program_output_span(source.program as usize, operation)?;
        if span.descending() || (span.count() > 1 && span.stride() != 1) {
            return None;
        }
        let start = outputs.local_offset.map_or(Some(span.start()), |cursor| {
            cursor.checked_add(span.start())
        })?;
        Some(start..start.checked_add(span.count())?)
    }
    pub(super) fn new(
        block: &'source ComputeBlock,
    ) -> Result<Self, ContinuousRefreshConstructionError> {
        let mut nodes = Vec::with_capacity(block.nodes.len());
        let mut cursor = 0;
        for (index, node) in block.nodes.iter().enumerate() {
            let outputs = match node {
                ComputeNode::ScalarPrograms(programs) => Some(ScalarOutputs::new(programs, cursor)),
                _ => None,
            };
            cursor = advance_output_cursor(node, index, cursor)?;
            nodes.push(outputs);
        }
        Ok(Self { nodes })
    }

    pub(super) fn get(&self, source: RefreshScalarProgramSource, offset: usize) -> Option<usize> {
        let outputs = self.nodes.get(source.node as usize)?.as_ref()?;
        let range = outputs
            .block
            .program_output_ordinals(source.program as usize)?;
        if offset >= range.len() {
            return None;
        }
        let ordinal = range.start.checked_add(offset)?;
        match outputs.local_offset {
            Some(cursor) => cursor.checked_add(ordinal),
            None => outputs.block.output_index_at(ordinal),
        }
    }

    /// Borrow a complete program's exact contiguous logical output projection.
    /// Explicit indices are checked as supplied; no scalar output owner is made.
    pub(super) fn contiguous_range(
        &self,
        source: RefreshScalarProgramSource,
    ) -> Option<Range<usize>> {
        let outputs = self.nodes.get(source.node as usize)?.as_ref()?;
        let range = outputs
            .block
            .program_contiguous_output_range(source.program as usize)?;
        match outputs.local_offset {
            Some(cursor) => Some(cursor.checked_add(range.start)?..cursor.checked_add(range.end)?),
            None => Some(range),
        }
    }
}

impl<'source> ScalarOutputs<'source> {
    fn new(block: &'source ScalarProgramBlock, cursor: usize) -> Self {
        Self {
            block,
            local_offset: block
                .uses_local_contiguous_output_indices()
                .then_some(cursor),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{LinearOp, TensorNodeMetadata, TensorOutputMap};
    use rumoca_core::{SourceId, Span, StructuredIndexBinder, StructuredIndexDomain};

    fn span() -> Span {
        Span::from_offsets(SourceId::from_source_name(file!()), 0, 1)
    }

    fn scalar_node(indices: Vec<usize>) -> ComputeNode {
        ComputeNode::ScalarPrograms(
            ScalarProgramBlock::with_output_indices(
                vec![
                    vec![
                        LinearOp::Const { dst: 0, value: 1. },
                        LinearOp::StoreOutput { src: 0 },
                        LinearOp::StoreOutput { src: 0 },
                    ],
                    vec![
                        LinearOp::Const { dst: 0, value: 2. },
                        LinearOp::StoreOutput { src: 0 },
                    ],
                ],
                vec![span(); 2],
                indices,
            )
            .unwrap(),
        )
    }

    #[test]
    fn source_outputs_preserve_tensor_cursor_sparse_slots_and_program_offsets() {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id: 0,
                display_name: "i".into(),
                lower: 1,
                upper: 1_000_000,
                step: 1,
            }],
        };
        let block = ComputeBlock {
            nodes: vec![
                ComputeNode::Map {
                    output_map: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
                    domain,
                    base_ops: vec![
                        LinearOp::Const { dst: 0, value: 0. },
                        LinearOp::StoreOutput { src: 0 },
                    ],
                    load_strides: Vec::new(),
                    const_strides: Vec::new(),
                    metadata: TensorNodeMetadata::default(),
                    span: span(),
                },
                scalar_node(vec![0, 1, 2]),
                scalar_node(vec![1_000_007, 1_000_004, 1_000_009]),
                ComputeNode::ScalarPrograms(ScalarProgramBlock::default()),
                scalar_node(vec![0, 1, 2]),
            ],
        };
        let outputs = SourceOutputs::new(&block).unwrap();
        let get = |node, program, offset| {
            outputs.get(
                RefreshScalarProgramSource::checked(node, program).unwrap(),
                offset,
            )
        };
        assert_eq!(get(1, 0, 0), Some(1_000_000));
        assert_eq!(get(1, 0, 1), Some(1_000_001));
        assert_eq!(get(1, 1, 0), Some(1_000_002));
        assert_eq!(get(2, 0, 1), Some(1_000_004));
        assert_eq!(get(2, 1, 0), Some(1_000_009));
        assert_eq!(get(4, 1, 0), Some(1_000_012));
        for (node, program, offset) in [
            (0, 0, 0),
            (1, 0, 2),
            (1, 1, 1),
            (1, 2, 0),
            (3, 0, 0),
            (5, 0, 0),
        ] {
            assert_eq!(get(node, program, offset), None);
        }
    }

    #[test]
    fn source_outputs_keep_compact_program_ranges_unmaterialized() {
        let count = 131_072;
        let source = ScalarProgramBlock::with_program_output_placements(
            vec![vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::TensorFill {
                    dst_start: 1,
                    value_start: 0,
                    count,
                    lanes: 1,
                },
                LinearOp::StoreOutputRange {
                    start: 1,
                    count,
                    stride: 1,
                },
            ]],
            vec![span()],
            vec![crate::ScalarProgramOutputPlacement::Range(7..7 + count)],
        )
        .unwrap();
        let block = ComputeBlock {
            nodes: vec![
                ComputeNode::ScalarPrograms(source.clone()),
                scalar_node(vec![0, 1, 2]),
            ],
        };
        let outputs = SourceOutputs::new(&block).unwrap();
        let range_source = RefreshScalarProgramSource::checked(0, 0).unwrap();
        assert_eq!(outputs.get(range_source, 0), Some(7));
        assert_eq!(outputs.get(range_source, count - 1), Some(7 + count - 1));
        assert_eq!(outputs.get(range_source, count), None);
        assert_eq!(outputs.contiguous_range(range_source), Some(7..7 + count));
        assert_eq!(
            outputs.get(RefreshScalarProgramSource::checked(1, 0).unwrap(), 0),
            Some(7 + count)
        );
        assert!(source.data.materialized_output_indices.get().is_none());
    }

    #[test]
    fn source_outputs_reject_overflowing_sparse_output_range() {
        assert!(
            ScalarProgramBlock::with_output_indices(
                vec![vec![
                    LinearOp::Const { dst: 0, value: 1.0 },
                    LinearOp::StoreOutput { src: 0 }
                ]],
                vec![span()],
                vec![usize::MAX],
            )
            .is_err()
        );
    }
}
