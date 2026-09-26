//! Per-row refresh analysis: the assignment shape, evaluability, and direct
//! and exact certificates of one canonical refresh row.
//!
//! The primary basis's analysis is reused by an alternate reduced chart
//! (SPEC_0040 STRUCT-T07 constraint-fold chart rows). An alternate chart's implicit system equals the primary's except in the few
//! rows its coordinate exchange replaces. The assignment shape, evaluability,
//! and direct and exact certificates of a refresh row are functions of its
//! program, output offset, and target alone, so a row the exchange leaves
//! unchanged carries the primary's analysis and only a replaced row is
//! analyzed again.

use std::collections::BTreeMap;

use rumoca_ir_solve as solve;

use super::source_catalog::{CanonicalScalarProgram, CanonicalScalarProgramCatalog};
use crate::EvalSolveError;

/// The analysis of one refresh row: its assignment shape and certificates.
pub(super) struct RowAnalysis {
    pub(super) shape: Option<solve::TargetAssignmentShape>,
    pub(super) direct: bool,
    pub(super) exact: bool,
}

/// The primary's canonical programs and analyzed refresh rows by equation.
pub(super) struct PriorRowAnalysis<'a> {
    catalog: CanonicalScalarProgramCatalog<'a>,
    targets: &'a [Option<solve::ScalarSlot>],
    rows: BTreeMap<usize, &'a solve::AlgebraicRefreshRow>,
}

impl<'a> PriorRowAnalysis<'a> {
    /// The analysis `primary` issued, which must carry its refresh owners.
    pub(super) fn new(primary: &'a solve::SolveProblem) -> Result<Self, EvalSolveError> {
        let rows = primary
            .continuous
            .refresh_owners
            .algebraic()
            .rows
            .iter()
            .map(|row| (row.equation_index(), row))
            .collect();
        Ok(Self {
            catalog: CanonicalScalarProgramCatalog::construct(&primary.continuous.implicit_rhs)?,
            targets: &primary.continuous.implicit_row_targets,
            rows,
        })
    }

    /// The primary's analysis of equation `equation_index` when the primary
    /// analyzed the same program, output offset, and target there; `None`
    /// when any of them differs or the primary issued no row for it.
    pub(super) fn reuse(
        &self,
        equation_index: usize,
        operations: &[solve::LinearOp],
        output_offset: usize,
        target_index: usize,
    ) -> Option<RowAnalysis> {
        let row = self.rows.get(&equation_index)?;
        let same_target = matches!(
            self.targets.get(equation_index),
            Some(Some(solve::ScalarSlot::Y { index, .. })) if *index == target_index
        );
        let position = self.catalog.positions().get(&equation_index)?;
        let program = self.catalog.program(position.program_index)?;
        let same = same_target
            && row.target_index() == target_index
            && row.output_offset() == output_offset
            && position.output_offset == output_offset
            && program.operations == operations;
        same.then(|| RowAnalysis {
            shape: row.assignment_shape().cloned(),
            direct: row.direct_assignment_certified(),
            exact: row.exact_assignment_certified(),
        })
    }
}

/// Exact and direct assignment certificates of one program output, given the
/// assignment shape already derived for it.
///
/// Both certificates require the shape and a causal program, and the direct
/// one also requires the shape to be direct. With the shape known, one of the
/// two certificates decides both: for a direct shape they coincide, for any
/// other shape the direct certificate is false. A shapeless output derives
/// nothing further.
pub(super) struct AssignmentCertificates {
    pub(super) exact: bool,
    pub(super) direct: bool,
}

impl AssignmentCertificates {
    pub(super) fn for_shape(
        program: &[solve::LinearOp],
        output_offset: usize,
        target_index: usize,
        shape: Option<&solve::TargetAssignmentShape>,
    ) -> Result<Self, EvalSolveError> {
        match shape {
            None => Ok(Self {
                exact: false,
                direct: false,
            }),
            Some(shape) if shape.is_direct() => {
                let direct = crate::prepared::program_certifies_direct_target(
                    program,
                    output_offset,
                    target_index,
                )?;
                Ok(Self {
                    exact: direct,
                    direct,
                })
            }
            Some(_) => Ok(Self {
                exact: crate::prepared::program_certifies_exact_target(
                    program,
                    output_offset,
                    target_index,
                )?,
                direct: false,
            }),
        }
    }
}

/// The assignment shape and certificates of the refresh row solving `program`'s
/// output `output_offset` for `target_index`: the primary's analysis when
/// `prior` issued it for the same program, offset, and target, else analyzed
/// here. `None` when the program cannot evaluate the declared target.
pub(super) fn analyze_refresh_row(
    program: &CanonicalScalarProgram<'_>,
    (equation_index, output_offset, target_index): (usize, usize, usize),
    prior: Option<&PriorRowAnalysis<'_>>,
) -> Result<Option<RowAnalysis>, EvalSolveError> {
    if let Some(reused) = prior.and_then(|prior| {
        prior.reuse(
            equation_index,
            program.operations,
            output_offset,
            target_index,
        )
    }) {
        return Ok(Some(reused));
    }
    let shape = crate::prepared::assignment_shape_for_program_output(
        program.operations,
        output_offset,
        target_index,
    )?;
    // A program with an assignment shape for this output and target can
    // always evaluate it; only a shapeless one needs the full check.
    if !(shape.is_some()
        || crate::prepared::program_can_evaluate_declared_target(
            program.operations,
            output_offset,
            target_index,
        )?)
    {
        return Ok(None);
    }
    let certificates = AssignmentCertificates::for_shape(
        program.operations,
        output_offset,
        target_index,
        shape.as_ref(),
    )?;
    Ok(Some(RowAnalysis {
        shape,
        direct: certificates.direct,
        exact: certificates.exact,
    }))
}
