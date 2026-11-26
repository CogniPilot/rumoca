//! The continuous residual as two forms of the same rows: the compact one
//! native backends read, and the scalar refresh source the exact refresh
//! owners issue assignments from (#760).

use super::*;

/// Continuous residual rows in source order, as two forms of the same rows.
///
/// The compact form keeps a structured algebraic family as one tensor node
/// when its scalar programs have one proven affine shape; native backends
/// read it. The refresh source keeps every row as a scalar program, with such
/// a family in the form it has without the tensor node, because the exact
/// refresh owners issue an assignment from a row's scalar program and a row
/// inside a tensor node has none.
///
/// The two forms agree node for node: node `k` holds the same rows in both,
/// identically for scalar rows, and as the tensor node in one and the
/// family's scalar node in the other. A source or stage that names a node
/// therefore names the same rows whichever form resolves it. Until a tensor
/// node is kept the two forms are one stream.
#[derive(Default)]
pub(in crate::lower) struct ContinuousResidualRows {
    nodes: Vec<solve::ComputeNode>,
    scalars: ScalarRows,
    /// The refresh source, once a tensor node made it differ.
    source: Option<RefreshSourceRows>,
    /// Rows pushed now belong to the refresh source alone: the scalar form
    /// of the family the compact form just kept as a tensor node.
    source_only: bool,
}

/// The refresh source while it differs from the compact form.
#[derive(Default)]
struct RefreshSourceRows {
    nodes: Vec<solve::ComputeNode>,
    scalars: ScalarRows,
}

impl RefreshSourceRows {
    /// Close the pending scalar rows as one node.
    fn flush(&mut self) -> Result<(), LowerError> {
        let scalars = std::mem::take(&mut self.scalars);
        if scalars.len() > 0 {
            self.nodes.extend(scalars.into_compute_block()?.nodes);
        }
        Ok(())
    }
}

impl ContinuousResidualRows {
    pub(in crate::lower) fn push_component(
        &mut self,
        program: Vec<solve::LinearOp>,
        span: Span,
        occurrence: Option<rumoca_core::ComponentOccurrence>,
        output: usize,
    ) {
        if let Some(source) = &mut self.source {
            source
                .scalars
                .push_component(program.clone(), span, occurrence, output);
        }
        if !self.source_only {
            self.scalars
                .push_component(program, span, occurrence, output);
        }
    }

    pub(in crate::lower) fn push_outputs(
        &mut self,
        program: Vec<solve::LinearOp>,
        span: Span,
        outputs: impl IntoIterator<Item = usize>,
    ) {
        let outputs = outputs.into_iter().collect::<Vec<_>>();
        if let Some(source) = &mut self.source {
            source
                .scalars
                .push_outputs(program.clone(), span, outputs.iter().copied());
        }
        if !self.source_only {
            self.scalars.push_outputs(program, span, outputs);
        }
    }

    pub(in crate::lower) fn push_output_range(
        &mut self,
        program: Vec<solve::LinearOp>,
        span: Span,
        outputs: std::ops::Range<usize>,
    ) {
        if let Some(source) = &mut self.source {
            source
                .scalars
                .push_output_range(program.clone(), span, outputs.clone());
        }
        if !self.source_only {
            self.scalars.push_output_range(program, span, outputs);
        }
    }

    pub(in crate::lower) fn tag_last(
        &mut self,
        occurrence: Option<rumoca_core::ComponentOccurrence>,
    ) {
        if let Some(source) = &mut self.source {
            source.scalars.tag_last(occurrence);
        }
        if !self.source_only {
            self.scalars.tag_last(occurrence);
        }
    }

    /// Keep `node` in the compact form and open the same node of the
    /// refresh source: the rows pushed until [`Self::end_source_node`] are
    /// the same family as scalar rows, and belong to the refresh source alone.
    pub(in crate::lower) fn push_tensor(
        &mut self,
        node: solve::ComputeNode,
    ) -> Result<(), LowerError> {
        let source = self.source.get_or_insert_with(|| RefreshSourceRows {
            nodes: Vec::new(),
            scalars: self.scalars.clone(),
        });
        source.flush()?;
        self.flush()?;
        self.nodes.push(node);
        self.source_only = true;
        Ok(())
    }

    /// Close the refresh-source node opened by [`Self::push_tensor`]. The
    /// family's scalar rows must have made exactly one node, so the two forms
    /// stay aligned.
    pub(in crate::lower) fn end_source_node(&mut self, span: Span) -> Result<(), LowerError> {
        if !self.source_only {
            return Ok(());
        }
        self.source_only = false;
        let Some(source) = &mut self.source else {
            return Ok(());
        };
        source.flush()?;
        if source.nodes.len() != self.nodes.len() {
            return Err(LowerError::contract(
                "a family kept as one tensor node has no single scalar node in the refresh source",
                span,
            ));
        }
        Ok(())
    }

    fn flush(&mut self) -> Result<(), LowerError> {
        let scalars = std::mem::take(&mut self.scalars);
        if scalars.len() > 0 {
            self.nodes.extend(scalars.into_compute_block()?.nodes);
        }
        Ok(())
    }

    /// The compact residual and its refresh source.
    pub(in crate::lower) fn into_compute_blocks(
        mut self,
    ) -> Result<(solve::ComputeBlock, solve::ComputeBlock), LowerError> {
        let source = self.source.take();
        self.flush()?;
        let residual = solve::ComputeBlock { nodes: self.nodes };
        let refresh_source = match source {
            Some(mut source) => {
                source.flush()?;
                let refresh_source = solve::ComputeBlock {
                    nodes: source.nodes,
                };
                require_the_same_rows_node_for_node(&residual, &refresh_source)?;
                refresh_source
            }
            None => residual.clone(),
        };
        Ok((residual, refresh_source))
    }
}

/// The compact residual and its refresh source must hold the same rows in
/// every node, so a node index names the same rows in both. A family shape
/// that breaks this is refused here instead of settling rows another way.
fn require_the_same_rows_node_for_node(
    residual: &solve::ComputeBlock,
    refresh_source: &solve::ComputeBlock,
) -> Result<(), LowerError> {
    let disagreement = |what: String| {
        LowerError::unspanned_non_computable(format!(
            "the compact residual and its refresh source disagree: {what}"
        ))
    };
    if residual.nodes.len() != refresh_source.nodes.len() {
        return Err(disagreement(format!(
            "{} nodes against {}",
            residual.nodes.len(),
            refresh_source.nodes.len()
        )));
    }
    // The refresh owners issue an assignment from a row's scalar program:
    // a row of the refresh source outside a scalar node would have none.
    if let Some(index) = refresh_source
        .nodes
        .iter()
        .position(|node| !matches!(node, solve::ComputeNode::ScalarPrograms(_)))
    {
        return Err(disagreement(format!(
            "refresh source node {index} gives its rows no scalar program"
        )));
    }
    let rows = |node: &solve::ComputeNode| {
        rumoca_eval_solve::to_scalar_program_block(&solve::ComputeBlock {
            nodes: vec![node.clone()],
        })
        .map(|block| {
            let mut rows = block.output_indices().to_vec();
            rows.sort_unstable();
            (block.stored_output_count(), rows)
        })
    };
    for (index, (compact, source)) in residual.nodes.iter().zip(&refresh_source.nodes).enumerate() {
        if matches!(compact, solve::ComputeNode::ScalarPrograms(_)) {
            // Scalar rows are pushed to both forms by the same call.
            continue;
        }
        let (compact, source) = (rows(compact), rows(source));
        if !matches!((&compact, &source), (Ok(compact), Ok(source)) if compact == source) {
            return Err(disagreement(format!("node {index} holds different rows")));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};

    fn span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("residual_rows.mo"),
            1,
            2,
        )
    }

    /// The residual row `y[output]`.
    fn scalar_row(output: usize) -> Vec<solve::LinearOp> {
        vec![
            solve::LinearOp::LoadY {
                dst: 0,
                index: output,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]
    }

    /// A family of `count` rows from `first_row`, kept as one Map.
    fn family(first_row: usize, count: usize) -> solve::ComputeNode {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: count as i64,
                step: 1,
            }],
        };
        solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap::dense_contiguous(first_row, &domain).unwrap(),
            domain,
            base_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span: span(),
        }
    }

    /// A scalar row, a two-row family, a scalar row.
    fn rows_around_a_family(source_rows: std::ops::Range<usize>) -> ContinuousResidualRows {
        let mut rows = ContinuousResidualRows::default();
        rows.push_outputs(scalar_row(0), span(), [0]);
        rows.push_tensor(family(1, 2)).unwrap();
        for row in source_rows {
            rows.push_outputs(scalar_row(row), span(), [row]);
        }
        rows
    }

    #[test]
    fn a_kept_family_is_one_node_in_both_forms() {
        let mut rows = rows_around_a_family(1..3);
        rows.end_source_node(span()).unwrap();
        rows.push_outputs(scalar_row(3), span(), [3]);
        let (residual, refresh_source) = rows.into_compute_blocks().unwrap();
        assert_eq!(residual.nodes.len(), 3);
        assert_eq!(refresh_source.nodes.len(), 3);
        assert!(matches!(residual.nodes[1], solve::ComputeNode::Map { .. }));
        assert!(
            refresh_source
                .nodes
                .iter()
                .all(|node| matches!(node, solve::ComputeNode::ScalarPrograms(_)))
        );
    }

    /// A family kept as a tensor node whose scalar form never arrives would
    /// leave its rows without a scalar source: lowering refuses.
    #[test]
    fn a_kept_family_without_its_scalar_rows_is_refused() {
        let mut rows = rows_around_a_family(0..0);
        let error = rows.end_source_node(span()).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("no single scalar node in the refresh source"),
            "{error}"
        );
    }

    /// The scalar form must hold the family's rows, no more and no fewer.
    #[test]
    fn a_kept_family_whose_scalar_rows_differ_is_refused() {
        let mut rows = rows_around_a_family(1..2);
        rows.end_source_node(span()).unwrap();
        let error = rows.into_compute_blocks().map(|_| ()).unwrap_err();
        assert!(
            error.to_string().contains("node 1 holds different rows"),
            "{error}"
        );
    }

    /// A refresh source that still holds a tensor node gives those rows no
    /// scalar program.
    #[test]
    fn a_refresh_source_row_without_a_scalar_program_is_refused() {
        let block = solve::ComputeBlock {
            nodes: vec![family(0, 2)],
        };
        let error = require_the_same_rows_node_for_node(&block, &block).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("refresh source node 0 gives its rows no scalar program"),
            "{error}"
        );
    }
}
