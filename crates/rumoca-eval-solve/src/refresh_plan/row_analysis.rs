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

mod output_dependencies;
#[cfg(test)]
mod tests;

use std::collections::BTreeMap;

use rumoca_ir_solve as solve;

use super::source_catalog::{CanonicalScalarProgram, CanonicalScalarProgramCatalog};
use crate::EvalSolveError;
use output_dependencies::OutputDependencies;

/// The analysis of one refresh row: its assignment shape and certificates.
pub(super) struct RowAnalysis {
    pub(super) shape: Option<solve::TargetAssignmentShape>,
    pub(super) direct: bool,
    pub(super) exact: bool,
}

/// The primary's canonical programs and analyzed refresh rows by equation.
pub(super) struct PriorRowAnalysis<'a> {
    catalog: CanonicalScalarProgramCatalog<'a>,
    targets: &'a solve::ImplicitRowTargets,
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
            self.targets.target_at(equation_index),
            Some(solve::ScalarSlot::Y { index, .. }) if index == target_index
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
    fn for_shape(facts: &ProgramFacts<'_>, shape: Option<&solve::TargetAssignmentShape>) -> Self {
        let exact = facts.causal && shape.is_some_and(|shape| facts.execution_independent(shape));
        Self {
            exact,
            direct: exact && shape.is_some_and(solve::TargetAssignmentShape::is_direct),
        }
    }
}

/// The assignment shapes and causality of one canonical program, derived once
/// for every output row it owns.
struct ProgramFacts<'source> {
    operations: &'source [solve::LinearOp],
    shapes: solve::CanonicalAssignmentQueries<'source>,
    causal: bool,
    execution: std::cell::OnceCell<solve::ScalarProgramExecutionYDependency>,
    output_dependencies: OutputDependencies<'source>,
    #[cfg(test)]
    execution_builds: std::cell::Cell<usize>,
}

impl<'source> ProgramFacts<'source> {
    fn new(operations: &'source [solve::LinearOp]) -> Self {
        Self {
            operations,
            shapes: solve::CanonicalAssignmentQueries::new(operations),
            causal: !operations.iter().any(crate::prepared::non_causal_linear_op),
            execution: std::cell::OnceCell::new(),
            output_dependencies: OutputDependencies::new(operations),
            #[cfg(test)]
            execution_builds: std::cell::Cell::new(0),
        }
    }

    fn execution_independent(&self, shape: &solve::TargetAssignmentShape) -> bool {
        self.execution
            .get_or_init(|| {
                #[cfg(test)]
                self.execution_builds.set(self.execution_builds.get() + 1);
                solve::ScalarProgramExecutionYDependency::new(self.operations)
            })
            .prefix(shape.expr_eval_len())
            .is_some_and(|reads| !reads.contains(shape.target_y_index()))
    }
}

/// Per-program facts of one refresh-plan construction, by catalog index.
#[derive(Default)]
pub(super) struct RowAnalysisCache<'source> {
    programs: Vec<Option<ProgramFacts<'source>>>,
    last_candidates: Vec<Option<usize>>,
    #[cfg(test)]
    fact_builds: usize,
}

impl<'source> RowAnalysisCache<'source> {
    #[cfg(test)]
    pub(super) fn proof_build_counts(&self) -> (usize, usize) {
        (
            self.fact_builds,
            self.programs
                .iter()
                .flatten()
                .map(|facts| facts.execution_builds.get())
                .sum(),
        )
    }

    pub(super) fn with_last_candidates(
        program_count: usize,
        candidates: impl IntoIterator<Item = (usize, usize)>,
        span: Option<rumoca_core::Span>,
    ) -> Result<Self, EvalSolveError> {
        let mut last_candidates = Vec::new();
        super::reserve_refresh_vec_capacity(
            &mut last_candidates,
            program_count,
            "refresh analysis last candidates",
            span,
        )?;
        last_candidates.resize(program_count, None);
        for (program, equation) in candidates {
            let last = last_candidates
                .get_mut(program)
                .ok_or_else(|| crate::invalid_row("refresh analysis candidate has no program"))?;
            *last = Some(equation);
        }
        Ok(Self {
            programs: Vec::new(),
            last_candidates,
            #[cfg(test)]
            fact_builds: 0,
        })
    }

    /// Shape and certificates from the same source-bound facts as ordinary row
    /// analysis. Singleton fallbacks and target catalogs share this owner.
    pub(super) fn assignment(
        &mut self,
        index: usize,
        operations: &'source [solve::LinearOp],
        output: usize,
        target: usize,
    ) -> (Option<solve::TargetAssignmentShape>, AssignmentCertificates) {
        let facts = self.facts(index, operations);
        let shape = facts.shapes.derive(output, target);
        let certificates = AssignmentCertificates::for_shape(facts, shape.as_ref());
        (shape, certificates)
    }

    /// Query completion, including refusal or reuse, releases only a source
    /// whose immutable candidate inventory proves there is no later query.
    pub(super) fn finish_candidate(&mut self, program: usize, equation: usize) {
        if self.last_candidates.get(program) == Some(&Some(equation))
            && let Some(facts) = self.programs.get_mut(program)
        {
            *facts = None;
        }
    }

    fn facts(
        &mut self,
        index: usize,
        operations: &'source [solve::LinearOp],
    ) -> &mut ProgramFacts<'source> {
        if self.programs.len() <= index {
            self.programs.resize_with(index + 1, || None);
        }
        #[cfg(test)]
        {
            self.fact_builds += usize::from(self.programs[index].is_none());
        }
        self.programs[index].get_or_insert_with(|| ProgramFacts::new(operations))
    }
}

/// The assignment shape and certificates of the refresh row solving `program`'s
/// output `output_offset` for `target_index`: the primary's analysis when
/// `prior` issued it for the same program, offset, and target, else analyzed
/// here from the program's facts, derived once per program. `None` when the
/// program cannot evaluate the declared target.
///
/// A shape for this output and target makes the row evaluable; a shapeless
/// one is evaluable only when no shape claims the output and the output does
/// not read the target. The exact certificate requires a causal program and a
/// shape; the direct one also requires the shape to be direct.
pub(super) fn analyze_refresh_row<'source>(
    program: &CanonicalScalarProgram<'source>,
    (program_index, equation_index, output_offset, target_index): (usize, usize, usize, usize),
    prior: Option<&PriorRowAnalysis<'_>>,
    cache: &mut RowAnalysisCache<'source>,
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
    let facts = cache.facts(program_index, program.operations);
    let shape = facts.shapes.derive(output_offset, target_index);
    let evaluable = shape.is_some()
        || (!facts
            .output_dependencies
            .depends_on(output_offset, target_index)
            && !facts.shapes.has_any(output_offset));
    if !evaluable {
        return Ok(None);
    }
    let certificates = AssignmentCertificates::for_shape(facts, shape.as_ref());
    Ok(Some(RowAnalysis {
        direct: certificates.direct,
        exact: certificates.exact,
        shape,
    }))
}
