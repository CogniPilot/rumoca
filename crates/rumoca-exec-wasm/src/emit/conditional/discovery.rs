//! Reuse Solve's canonical recursive traversal without reconstructing op graphs.
use super::*;
use rumoca_ir_solve::{LinearOpSliceKind, SolveVisitor};

pub(in crate::emit) fn visit_operations(
    rows: &[Vec<LinearOp>],
    visit: impl FnMut(&LinearOp) -> Result<(), String>,
) -> Result<(), String> {
    visit_program_operations(rows.iter().map(Vec::as_slice), visit)
}

/// [`visit_operations`] over borrowed programs, in order.
pub(in crate::emit) fn visit_program_operations<'a>(
    rows: impl IntoIterator<Item = &'a [LinearOp]>,
    mut visit: impl FnMut(&LinearOp) -> Result<(), String>,
) -> Result<(), String> {
    struct Borrowed<F>(F);
    impl<F: FnMut(&LinearOp) -> Result<(), String>> SolveVisitor for Borrowed<F> {
        type Error = String;
        fn visit_linear_op(
            &mut self,
            _: LinearOpSliceKind,
            _: usize,
            op: &LinearOp,
        ) -> Result<(), String> {
            (self.0)(op)
        }
    }
    let mut borrowed = Borrowed(&mut visit);
    for (program_index, row) in rows.into_iter().enumerate() {
        borrowed.visit_linear_op_slice(
            LinearOpSliceKind::ScalarProgram {
                program_index,
                span: None,
            },
            row,
        )?;
    }
    Ok(())
}

pub(in crate::emit) fn call_sites(
    rows: &[Vec<LinearOp>],
) -> Result<Vec<rumoca_ir_solve::SolvePureCallSite>, String> {
    let mut sites = Vec::new();
    visit_operations(rows, |operation| {
        match operation {
            LinearOp::PureCall { site, .. } => sites.push(site.clone()),
            LinearOp::PureCallObservation { site, .. } => sites.push(site.value_site().clone()),
            _ => {}
        }
        Ok(())
    })?;
    Ok(sites)
}

pub(in crate::emit) fn directional_call_sites(
    rows: &[Vec<LinearOp>],
) -> Result<Vec<rumoca_ir_solve::SolvePureCallDirectionalSite>, String> {
    let mut sites = Vec::new();
    visit_operations(rows, |operation| {
        if let LinearOp::PureCallDirectional { site, .. } = operation {
            sites.push(site.clone());
        }
        Ok(())
    })?;
    Ok(sites)
}
