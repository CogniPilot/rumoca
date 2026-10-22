//! Fused kernels of affine projection blocks: each block's residual programs,
//! and the directional programs of the same rows, compiled by the execution
//! backend into one native entry each, so one residual pass or one
//! directional pass over the block is one call instead of one call per row.
//!
//! A kernel holds the block rows' canonical programs unchanged, in block row
//! order, one output each, so it computes exactly the values the per-row
//! compiled evaluation computes (a block residual split partitions the same
//! operations in the same order). The per-row compiled evaluation runs a
//! lazily planned program through its compiled entry as well, so only a row of
//! a program with several outputs leaves its block without a kernel.

use std::cell::RefCell;
use std::rc::Rc;

use rumoca_ir_solve as solve;

use super::{
    CompiledSolveExpression, CompiledSolveJacobianExpression, ProgramAnalysis,
    SolveExecutionBackend,
};

/// The fused kernels of one affine projection block.
#[derive(Default)]
pub(crate) struct BlockKernels {
    residual: Option<Rc<dyn CompiledSolveExpression>>,
    jacobian_v: Option<Rc<dyn CompiledSolveJacobianExpression>>,
    staged: RefCell<Vec<f64>>,
}

/// The programs and compiled owners the kernels are built from.
pub(super) struct BlockKernelSources<'a> {
    pub(super) owners: &'a solve::ContinuousRefreshOwners,
    pub(super) plan: &'a solve::AlgebraicProjectionPlan,
    /// The compiled canonical residual and its scalar programs.
    pub(super) residual: (ProgramAnalysis<'a>, &'a solve::ScalarProgramBlock),
    /// The compiled complete directional programs (over `[solver Y | parameter]`
    /// seeds) of the implicit rows.
    pub(super) jacobian_v: Option<(ProgramAnalysis<'a>, &'a solve::ScalarProgramBlock)>,
}

impl BlockKernels {
    /// Whether the block's residual passes run through its fused kernel.
    pub(crate) fn has_residual(&self) -> bool {
        self.residual.is_some()
    }

    /// The block residual at `(y, p, t)` into `out`, in block row order.
    /// `false`, with `out` unspecified, leaves the residual to the per-row
    /// evaluation: the block has no residual kernel, or the compiled call
    /// failed or produced a non-finite value, which the per-row evaluation
    /// reports with its row diagnostics.
    pub(crate) fn eval_residual(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> bool {
        let Some(code) = &self.residual else {
            return false;
        };
        self.staged_call(out, |staged| code.call(y, p, t, tables, staged).is_ok())
    }

    /// The block rows' directional derivatives at `(y, p, t)` along
    /// `seed` into `out`, in block row order, as [`Self::eval_residual`].
    pub(crate) fn eval_jacobian_v(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> bool {
        let Some(code) = &self.jacobian_v else {
            return false;
        };
        self.staged_call(out, |staged| {
            code.call(y, p, t, seed, tables, staged).is_ok()
        })
    }

    /// Publish `call`'s outputs only when it succeeds with finite values.
    fn staged_call(&self, out: &mut [f64], call: impl FnOnce(&mut [f64]) -> bool) -> bool {
        let Ok(mut staged) = self.staged.try_borrow_mut() else {
            return false;
        };
        staged.resize(out.len(), 0.0);
        if !call(&mut staged) || staged.iter().any(|value| !value.is_finite()) {
            return false;
        }
        out.copy_from_slice(&staged);
        true
    }
}

/// The kernels of every affine projection block with more than one row,
/// aligned with `plan.blocks`; `None` without a backend and for a block that
/// gets neither kernel.
pub(super) fn block_kernels(
    sources: &BlockKernelSources<'_>,
    backend: Option<&dyn SolveExecutionBackend>,
) -> Box<[Option<Rc<BlockKernels>>]> {
    sources
        .plan
        .blocks
        .scalar_blocks()
        .iter()
        .enumerate()
        .map(|(index, block)| {
            let backend = backend?;
            if block.rows.len() < 2 || !sources.owners.algebraic_projection_block_is_affine(index) {
                return None;
            }
            let (analysis, canonical) = sources.residual;
            let residual = block_programs(block, analysis, canonical)
                .and_then(|programs| backend.compile_expression(&programs).ok());
            let jacobian_v = sources.jacobian_v.and_then(|(analysis, canonical)| {
                let programs = block_programs(block, analysis, canonical)?;
                backend.compile_jacobian_expression(&programs).ok()
            });
            (residual.is_some() || jacobian_v.is_some()).then(|| {
                Rc::new(BlockKernels {
                    residual,
                    jacobian_v,
                    staged: RefCell::new(Vec::with_capacity(block.rows.len())),
                })
            })
        })
        .collect()
}

/// The block rows' programs of `canonical`, in block row order, one output
/// each; `None` when a row has no program there or shares a program with
/// several outputs.
fn block_programs(
    block: &solve::AlgebraicProjectionBlock,
    analysis: ProgramAnalysis<'_>,
    canonical: &solve::ScalarProgramBlock,
) -> Option<solve::ScalarProgramBlock> {
    let mut programs = Vec::with_capacity(block.rows.len());
    let mut spans = Vec::with_capacity(block.rows.len());
    for &row in &block.rows {
        let (program, offset) = analysis.row_output_position(row)?;
        let ops = canonical.program(program)?;
        if offset != 0 || solve::ScalarProgramBlock::program_output_count(ops) != 1 {
            return None;
        }
        programs.push(ops.to_vec());
        spans.push(analysis.program_span(program)?);
    }
    solve::ScalarProgramBlock::with_program_spans(programs, spans).ok()
}
