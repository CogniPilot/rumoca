//! Fused residual entries of algebraic projection blocks whose residual rows
//! come from programs with several outputs: the unsplit programs of each
//! block's issued residual selection, compiled by the execution backend into
//! one native entry, so one residual pass calls one entry for them instead of
//! one checked entry per program.
//!
//! The entry holds those canonical programs unchanged, in selection order, and
//! writes every output of each, program after program; Solve IR issues where
//! each program's outputs start and which block rows they place. The other
//! programs of the selection keep their own evaluation.

use std::cell::RefCell;
use std::rc::Rc;

use rumoca_ir_solve as solve;

use super::{CompiledSolveExpression, ProgramAnalysis, SolveExecutionBackend};

/// The compiled entry of one block's fused residual selection.
pub(crate) struct FusedResidual {
    code: Rc<dyn CompiledSolveExpression>,
    staged: RefCell<Vec<f64>>,
}

impl FusedResidual {
    /// Place the fused programs' residuals at `(y, p, t)` into `out` through
    /// `fused`'s issued placements. `false`, with `out` unchanged, when the
    /// entry is busy, fails or produces a non-finite value: the caller's
    /// program-by-program evaluation then runs and reports in its own order.
    pub(crate) fn eval(
        &self,
        fused: &solve::FusedResidualSelection,
        (y, p, t): (&[f64], &[f64], f64),
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> bool {
        let Ok(mut staged) = self.staged.try_borrow_mut() else {
            return false;
        };
        staged.resize(fused.output_count(), 0.0);
        if self.code.call(y, p, t, tables, &mut staged).is_err()
            || staged.iter().any(|value| !value.is_finite())
        {
            return false;
        }
        for (program, &base) in fused.selection().programs().iter().zip(fused.bases()) {
            for &(offset, row) in program.placements() {
                out[row] = staged[base + offset];
            }
        }
        true
    }
}

/// The fused residual entry of each algebraic projection structure, aligned
/// with them; `None` without a backend, for a structure without an issued
/// fused selection, or when the selection was issued over other programs than
/// `canonical`.
pub(super) fn fused_residuals(
    structures: &[solve::ColoredJacobianStructure],
    (analysis, canonical): (ProgramAnalysis<'_>, &solve::ScalarProgramBlock),
    backend: Option<&dyn SolveExecutionBackend>,
) -> Box<[Option<Rc<FusedResidual>>]> {
    structures
        .iter()
        .map(|structure| {
            let backend = backend?;
            let fused = structure.fused_residual_evaluation()?;
            if !fused.primal_source().shares_program_owner(canonical) {
                return None;
            }
            let entries = fused.selection().programs();
            let mut programs = Vec::with_capacity(entries.len());
            let mut spans = Vec::with_capacity(entries.len());
            for entry in entries {
                programs.push(canonical.program(entry.program())?.to_vec());
                spans.push(analysis.program_span(entry.program())?);
            }
            let block = solve::ScalarProgramBlock::with_program_spans(programs, spans).ok()?;
            let code = backend.compile_expression(&block).ok()?;
            Some(Rc::new(FusedResidual {
                code,
                staged: RefCell::new(Vec::with_capacity(fused.output_count())),
            }))
        })
        .collect()
}
