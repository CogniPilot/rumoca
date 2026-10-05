//! Whole-block native execution of a Solve `ComputeBlock` that keeps its
//! affine tensor nodes compact (SPEC_0032 §4).
//!
//! Each `Map`/`AffineStencil` node whose base program is straight-line scalar
//! arithmetic runs as one native loop kernel; every other node is compiled
//! row by row exactly as its scalar view. Nodes execute in block order, so the
//! result is the scalar view's result, slot for slot and bit for bit. A block
//! compiles either for its values or for its directional derivative along a
//! seed vector (a JVP block from Solve AD).

use rumoca_core::ExternalTableData;
use rumoca_eval_solve::{AffineKernelNode, AffineKernelPlan};
use rumoca_ir_solve::{ComputeBlock, ComputeNode, ScalarProgramBlock};

use crate::emit::{
    CompiledTensorKernels, DirectionalKernels, KernelFrame, KernelId, KernelKind, KernelProgram,
    ResidualKernels,
};
use crate::{
    CompileError, CompiledExpressionRows, CompiledJacobianV, CompiledPureCallTable,
    compile_expression_scalar_program_block,
    compile_expression_scalar_program_block_with_pure_calls, compile_jacobian_scalar_program_block,
    compile_jacobian_scalar_program_block_with_pure_calls,
};

/// A kernel kind together with the per-row compiled form of the nodes its
/// loop kernels do not own.
trait BlockKind: KernelKind + Sized {
    type Rows;

    fn compile_rows(
        rows: &ScalarProgramBlock,
        pure_calls: Option<&CompiledPureCallTable>,
    ) -> Result<Self::Rows, CompileError>;

    fn call_rows(
        rows: &Self::Rows,
        frame: &mut KernelFrame<'_, Self>,
        external_tables: &[ExternalTableData],
    ) -> Result<(), CompileError>;

    fn row_count(rows: &Self::Rows) -> usize;
}

impl BlockKind for ResidualKernels {
    type Rows = CompiledExpressionRows;

    fn compile_rows(
        rows: &ScalarProgramBlock,
        pure_calls: Option<&CompiledPureCallTable>,
    ) -> Result<Self::Rows, CompileError> {
        match pure_calls {
            Some(pure_calls) => {
                compile_expression_scalar_program_block_with_pure_calls(rows, pure_calls)
            }
            None => compile_expression_scalar_program_block(rows),
        }
    }

    fn call_rows(
        rows: &Self::Rows,
        frame: &mut KernelFrame<'_, Self>,
        external_tables: &[ExternalTableData],
    ) -> Result<(), CompileError> {
        rows.call_with_external_tables(frame.y, frame.p, frame.t, external_tables, frame.out)
    }

    fn row_count(rows: &Self::Rows) -> usize {
        rows.rows()
    }
}

impl BlockKind for DirectionalKernels {
    type Rows = CompiledJacobianV;

    fn compile_rows(
        rows: &ScalarProgramBlock,
        pure_calls: Option<&CompiledPureCallTable>,
    ) -> Result<Self::Rows, CompileError> {
        match pure_calls {
            Some(pure_calls) => {
                compile_jacobian_scalar_program_block_with_pure_calls(rows, pure_calls)
            }
            None => compile_jacobian_scalar_program_block(rows),
        }
    }

    fn call_rows(
        rows: &Self::Rows,
        frame: &mut KernelFrame<'_, Self>,
        external_tables: &[ExternalTableData],
    ) -> Result<(), CompileError> {
        rows.call_with_external_tables(
            frame.y,
            frame.p,
            frame.t,
            frame.seed,
            external_tables,
            frame.out,
        )
    }

    fn row_count(rows: &Self::Rows) -> usize {
        rows.rows()
    }
}

enum Segment<K: BlockKind> {
    Rows(Box<K::Rows>),
    Kernel(KernelId),
}

struct CompiledBlock<K: BlockKind> {
    segments: Vec<Segment<K>>,
    kernels: CompiledTensorKernels<K>,
}

impl<K: BlockKind> CompiledBlock<K> {
    fn call<'a>(
        &'a self,
        inputs: (&'a [f64], &'a [f64], f64),
        seed: K::Seed<'a>,
        external_tables: &[ExternalTableData],
        out: &'a mut [f64],
    ) -> Result<(), CompileError> {
        let mut frame = self.kernels.frame(inputs, seed, out)?;
        for segment in &self.segments {
            match segment {
                Segment::Rows(rows) => K::call_rows(rows, &mut frame, external_tables)?,
                Segment::Kernel(kernel) => frame.run(*kernel)?,
            }
        }
        Ok(())
    }

    fn kernel_count(&self) -> usize {
        self.segments
            .iter()
            .filter(|segment| matches!(segment, Segment::Kernel(_)))
            .count()
    }

    fn compiled_row_count(&self) -> usize {
        self.segments
            .iter()
            .map(|segment| match segment {
                Segment::Rows(rows) => K::row_count(rows),
                Segment::Kernel(_) => 0,
            })
            .sum()
    }
}

/// A compute block compiled for whole-block evaluation.
pub struct CompiledComputeExpression(CompiledBlock<ResidualKernels>);

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
        self.0.call((y, p, t), (), external_tables, out)
    }

    /// Number of native loop kernels: one per compact tensor node.
    #[must_use]
    pub fn kernel_count(&self) -> usize {
        self.0.kernel_count()
    }

    /// Number of scalar rows compiled one by one.
    #[must_use]
    pub fn compiled_row_count(&self) -> usize {
        self.0.compiled_row_count()
    }
}

/// A directional (JVP) compute block compiled for whole-block evaluation.
pub struct CompiledComputeJacobian(CompiledBlock<DirectionalKernels>);

impl CompiledComputeJacobian {
    /// Evaluate the block's directional derivative along `seed` into `out`.
    pub fn call_with_external_tables(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        self.0.call((y, p, t), seed, external_tables, out)
    }

    /// Number of native loop kernels: one per compact tensor node.
    #[must_use]
    pub fn kernel_count(&self) -> usize {
        self.0.kernel_count()
    }

    /// Number of scalar rows compiled one by one.
    #[must_use]
    pub fn compiled_row_count(&self) -> usize {
        self.0.compiled_row_count()
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
    compile_block(block, pure_calls).map(|compiled| compiled.map(CompiledComputeExpression))
}

/// [`compile_expression_compute_block`] for a directional (JVP) block whose
/// programs read the seed vector.
pub fn compile_jacobian_compute_block(
    block: &ComputeBlock,
    pure_calls: Option<&CompiledPureCallTable>,
) -> Result<Option<CompiledComputeJacobian>, CompileError> {
    compile_block(block, pure_calls).map(|compiled| compiled.map(CompiledComputeJacobian))
}

fn compile_block<K: BlockKind>(
    block: &ComputeBlock,
    pure_calls: Option<&CompiledPureCallTable>,
) -> Result<Option<CompiledBlock<K>>, CompileError> {
    let Some(plans) = kernel_plans::<K>(block)? else {
        return Ok(None);
    };
    if plans.iter().all(Option::is_none) {
        return Ok(None);
    }
    let (kernels, ids) = CompiledTensorKernels::compile(&plans)?;
    let mut segments = Vec::with_capacity(block.nodes.len());
    let mut cursor = 0usize;
    for ((node, plan), id) in block.nodes.iter().zip(&plans).zip(ids) {
        match (node, plan, id) {
            (_, Some((plan, _)), Some(id)) => {
                segments.push(Segment::Kernel(id));
                cursor = cursor.max(plan.output_count());
            }
            (ComputeNode::ScalarPrograms(rows), _, _) => {
                let indices = rumoca_eval_solve::scalar_program_output_indices(
                    rows,
                    cursor,
                    "scalar programs",
                )
                .map_err(input_error)?;
                cursor = cursor.max(
                    rumoca_eval_solve::scalar_program_output_count(rows, cursor, "scalar programs")
                        .map_err(input_error)?,
                );
                let placed = ScalarProgramBlock::with_output_indices(
                    rows.programs().to_vec(),
                    rows.program_spans().to_vec(),
                    indices,
                )
                .map_err(|error| CompileError::Input(format!("{error:?}")))?;
                segments.push(Segment::Rows(Box::new(K::compile_rows(
                    &placed, pure_calls,
                )?)));
            }
            (tensor, _, _) => {
                let view = rumoca_eval_solve::to_scalar_program_block(&ComputeBlock {
                    nodes: vec![tensor.clone()],
                })
                .map_err(input_error)?;
                // The scalar view of one tensor node writes its own output map.
                let end = view
                    .output_indices()
                    .iter()
                    .max()
                    .map_or(cursor, |last| last.saturating_add(1));
                cursor = cursor.max(end);
                segments.push(Segment::Rows(Box::new(K::compile_rows(&view, pure_calls)?)));
            }
        }
    }
    Ok(Some(CompiledBlock { segments, kernels }))
}

fn input_error(error: rumoca_eval_solve::ScalarizeError) -> CompileError {
    CompileError::Input(error.to_string())
}

type KernelPlan<'a, K> = Option<(AffineKernelPlan, KernelProgram<'a, K>)>;

/// The loop-kernel plan of each node (`None` for a node compiled by rows), or
/// `None` when a node's outputs depend on the scalar-view cursor.
fn kernel_plans<K: KernelKind>(
    block: &ComputeBlock,
) -> Result<Option<Vec<KernelPlan<'_, K>>>, CompileError> {
    let mut plans = Vec::with_capacity(block.nodes.len());
    for node in &block.nodes {
        let (domain, output_map, base_ops, load_strides, const_strides, span, label) = match node {
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
        let Some(program) = KernelProgram::<K>::admit(base_ops) else {
            plans.push(None);
            continue;
        };
        let plan = AffineKernelPlan::new(AffineKernelNode {
            domain,
            output_map: Some(output_map),
            base_ops,
            load_strides,
            const_strides,
            kind: label,
            span,
        })
        .map_err(input_error)?;
        plans.push(Some((plan, program)));
    }
    Ok(Some(plans))
}

#[cfg(test)]
mod tests;
