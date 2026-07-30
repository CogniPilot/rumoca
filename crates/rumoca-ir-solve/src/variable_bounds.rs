//! Layout addressability of Solve programs.
//!
//! A Solve program addresses state and parameter storage by flat index, and
//! [`VarLayout`] owns those extents. An index outside them is not a runtime
//! surprise but a construction defect: forward-mode AD places the parameter
//! seeds after the state seeds at offset `y_scalars`, so an understated state
//! extent aliases parameter columns onto state columns and the derived
//! Jacobian column space collapses (in the limit to zero columns) while the
//! primal program still evaluates. This check rejects that coupling where the
//! program and the layout are joined instead of letting sparsity derivation
//! report an empty seed range far downstream.

use rumoca_core::Span;

use crate::{
    ComputeBlock, LinearOp, LinearOpSliceKind, SolveProblemShapeContractError, SolveVisitor,
    VarLayout,
};

/// Reject any `Y`/`P` load in `block` that the layout cannot address.
pub(crate) fn validate_compute_block_variable_bounds(
    block: &ComputeBlock,
    context: &'static str,
    layout: &VarLayout,
) -> Result<(), SolveProblemShapeContractError> {
    VariableBoundsVisitor { context, layout }.visit_compute_block(block)
}

struct VariableBoundsVisitor<'layout> {
    context: &'static str,
    layout: &'layout VarLayout,
}

impl VariableBoundsVisitor<'_> {
    /// Check the half-open storage run `base..base + count` against `extent`.
    fn check_run(
        &self,
        storage: &'static str,
        base: usize,
        count: usize,
        extent: usize,
        kind: LinearOpSliceKind,
    ) -> Result<(), SolveProblemShapeContractError> {
        let last = base
            .checked_add(count)
            .and_then(|end| end.checked_sub(1))
            .ok_or(SolveProblemShapeContractError::VariableIndexOutOfBounds {
                context: self.context,
                storage,
                index: base,
                extent,
                span: slice_span(kind),
            })?;
        if last < extent {
            return Ok(());
        }
        Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context: self.context,
            storage,
            index: last,
            extent,
            span: slice_span(kind),
        })
    }
}

impl SolveVisitor for VariableBoundsVisitor<'_> {
    type Error = SolveProblemShapeContractError;

    fn visit_linear_op(
        &mut self,
        kind: LinearOpSliceKind,
        _op_index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        match *op {
            LinearOp::LoadY { index, .. } => {
                self.check_run("Y", index, 1, self.layout.y_scalars(), kind)
            }
            LinearOp::LoadP { index, .. } => {
                self.check_run("P", index, 1, self.layout.p_scalars(), kind)
            }
            // The runtime index is clamped into `base..base + count`, so the
            // complete selectable run must be addressable.
            LinearOp::LoadIndexedP { base, count, .. } => {
                self.check_run("P", base, count, self.layout.p_scalars(), kind)
            }
            // Seed loads address the AD seed vector, whose extent belongs to
            // the selected seed mode rather than to the variable layout.
            _ => Ok(()),
        }
    }
}

fn slice_span(kind: LinearOpSliceKind) -> Option<Span> {
    let span = match kind {
        LinearOpSliceKind::ScalarProgram { span, .. } => return span,
        LinearOpSliceKind::MatMulLhs { span, .. }
        | LinearOpSliceKind::MatMulRhs { span, .. }
        | LinearOpSliceKind::LinSolveSetup { span, .. }
        | LinearOpSliceKind::MapBase { span, .. }
        | LinearOpSliceKind::AffineStencilBase { span, .. } => span,
    };
    (!span.is_dummy()).then_some(span)
}
