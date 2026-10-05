//! Whole-block native execution of a Solve `ComputeBlock` that keeps its
//! affine tensor nodes compact (SPEC_0032 §4).
//!
//! Each `Map`/`AffineStencil` node whose base program is straight-line scalar
//! arithmetic runs as one native loop kernel; every other node is compiled
//! row by row exactly as its scalar view. Nodes execute in block order, so the
//! result is the scalar view's result, slot for slot and bit for bit.

use rumoca_core::ExternalTableData;
use rumoca_eval_solve::{AffineKernelNode, AffineKernelPlan};
use rumoca_ir_solve::{ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock};

use crate::emit::CompiledTensorKernels;
use crate::{
    CompileError, CompiledExpressionRows, CompiledPureCallTable,
    compile_expression_scalar_program_block,
    compile_expression_scalar_program_block_with_pure_calls,
};

enum Segment {
    Rows(Box<CompiledExpressionRows>),
    Kernel(usize),
}

/// A compute block compiled for whole-block evaluation.
pub struct CompiledComputeExpression {
    segments: Vec<Segment>,
    kernels: CompiledTensorKernels,
    output_len: usize,
}

impl CompiledComputeExpression {
    /// Evaluate every node in block order into `out`, which holds the block's
    /// complete output vector.
    pub fn call_with_external_tables(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        if out.len() < self.output_len {
            return Err(CompileError::Input(format!(
                "output buffer too small: {} < {}",
                out.len(),
                self.output_len
            )));
        }
        for segment in &self.segments {
            match segment {
                Segment::Rows(rows) => {
                    rows.call_with_external_tables(y, p, t, external_tables, out)?
                }
                Segment::Kernel(index) => self.kernels.call(*index, y, p, t, out)?,
            }
        }
        Ok(())
    }

    /// Number of native loop kernels: one per compact tensor node.
    #[must_use]
    pub fn kernel_count(&self) -> usize {
        self.segments
            .iter()
            .filter(|segment| matches!(segment, Segment::Kernel(_)))
            .count()
    }

    /// Number of scalar rows compiled one by one.
    #[must_use]
    pub fn compiled_row_count(&self) -> usize {
        self.segments
            .iter()
            .map(|segment| match segment {
                Segment::Rows(rows) => rows.rows(),
                Segment::Kernel(_) => 0,
            })
            .sum()
    }
}

/// Compile `block` for whole-block evaluation with native loop kernels.
///
/// Returns `Ok(None)` when the block has no tensor node a loop kernel owns, or
/// holds a node whose output slots depend on the scalar-view cursor (`MatMul`,
/// `LinSolve`); the caller then compiles the scalar view as before. This is a
/// compile-time choice; a compiled block never changes path at run time.
pub fn compile_expression_compute_block(
    block: &ComputeBlock,
    pure_calls: Option<&CompiledPureCallTable>,
) -> Result<Option<CompiledComputeExpression>, CompileError> {
    let Some(plans) = kernel_plans(block)? else {
        return Ok(None);
    };
    let kernel_nodes = plans
        .iter()
        .filter_map(|plan| plan.as_ref().map(|(plan, ops)| (plan, *ops)))
        .collect::<Vec<_>>();
    if kernel_nodes.is_empty() {
        return Ok(None);
    }
    let kernels = CompiledTensorKernels::compile_residual(&kernel_nodes)?;
    let mut segments = Vec::with_capacity(block.nodes.len());
    let mut cursor = 0usize;
    let mut kernel_index = 0usize;
    for (node, plan) in block.nodes.iter().zip(&plans) {
        match (node, plan) {
            (_, Some((plan, _))) => {
                segments.push(Segment::Kernel(kernel_index));
                kernel_index += 1;
                cursor = cursor.max(plan.output_count());
            }
            (ComputeNode::ScalarPrograms(rows), None) => {
                let indices = rumoca_eval_solve::scalar_program_output_indices(
                    rows,
                    cursor,
                    "scalar programs",
                )
                .map_err(|error| CompileError::Input(error.to_string()))?;
                cursor = cursor.max(
                    rumoca_eval_solve::scalar_program_output_count(rows, cursor, "scalar programs")
                        .map_err(|error| CompileError::Input(error.to_string()))?,
                );
                let placed = ScalarProgramBlock::with_output_indices(
                    rows.programs().to_vec(),
                    rows.program_spans().to_vec(),
                    indices,
                )
                .map_err(|error| CompileError::Input(error.to_string()))?;
                segments.push(Segment::Rows(Box::new(compile_rows(&placed, pure_calls)?)));
            }
            (tensor, None) => {
                let view = rumoca_eval_solve::to_scalar_program_block(&ComputeBlock {
                    nodes: vec![tensor.clone()],
                })
                .map_err(|error| CompileError::Input(error.to_string()))?;
                // The scalar view of one tensor node writes its own output map.
                let end = view
                    .output_indices()
                    .iter()
                    .max()
                    .map_or(cursor, |last| last.saturating_add(1));
                cursor = cursor.max(end);
                segments.push(Segment::Rows(Box::new(compile_rows(&view, pure_calls)?)));
            }
        }
    }
    Ok(Some(CompiledComputeExpression {
        segments,
        kernels,
        output_len: cursor,
    }))
}

type KernelPlan<'a> = Option<(AffineKernelPlan, &'a [LinearOp])>;

/// The loop-kernel plan of each node (`None` for a node compiled by rows), or
/// `None` when a node's outputs depend on the scalar-view cursor.
fn kernel_plans(block: &ComputeBlock) -> Result<Option<Vec<KernelPlan<'_>>>, CompileError> {
    let mut plans = Vec::with_capacity(block.nodes.len());
    for node in &block.nodes {
        let (domain, output_map, base_ops, load_strides, const_strides, span, kind) = match node {
            ComputeNode::ScalarPrograms(_) => {
                plans.push(None);
                continue;
            }
            ComputeNode::MatMul { .. } | ComputeNode::LinSolve { .. } => return Ok(None),
            ComputeNode::Map {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            } => (
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                *span,
                "map",
            ),
            ComputeNode::AffineStencil {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            } => (
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                *span,
                "affine stencil",
            ),
        };
        if !crate::emit::residual_tensor_kernel_supported(base_ops) {
            plans.push(None);
            continue;
        }
        let plan = AffineKernelPlan::new(AffineKernelNode {
            domain,
            output_map: Some(output_map),
            base_ops,
            load_strides,
            const_strides,
            kind,
            span,
        })
        .map_err(|error| CompileError::Input(error.to_string()))?;
        plans.push(Some((plan, base_ops.as_slice())));
    }
    Ok(Some(plans))
}

fn compile_rows(
    rows: &ScalarProgramBlock,
    pure_calls: Option<&CompiledPureCallTable>,
) -> Result<CompiledExpressionRows, CompileError> {
    match pure_calls {
        Some(pure_calls) => {
            compile_expression_scalar_program_block_with_pure_calls(rows, pure_calls)
        }
        None => compile_expression_scalar_program_block(rows),
    }
}

#[cfg(test)]
mod tests;
