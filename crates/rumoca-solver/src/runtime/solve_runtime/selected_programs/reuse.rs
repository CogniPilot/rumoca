//! Chart reuse preserves current scalar authority and original compiled entries.

#[cfg(test)]
mod tests;

use super::*;
use rumoca_eval_solve::replaced_programs;

fn invalid_reuse(message: &str, source: &solve::ScalarProgramBlock) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: message.into(),
        span: source.program_span(0),
    }
}

fn source_relation(
    primary: &solve::ScalarProgramBlock,
    source: &solve::ScalarProgramBlock,
) -> Option<Vec<usize>> {
    if primary.shares_program_owner(source) {
        return Some(Vec::new());
    }
    if primary.program_spans() != source.program_spans()
        || needs_exact_context(primary)
        || needs_exact_context(source)
    {
        return None;
    }
    replaced_programs(primary, source)
}

// A scalar view cannot certify that independently reconstructed call sites
// belong to the same call table or original event/action report root.
fn needs_exact_context(source: &solve::ScalarProgramBlock) -> bool {
    struct Context;
    impl solve::SolveVisitor for Context {
        type Error = ();

        fn visit_linear_op(
            &mut self,
            _: solve::LinearOpSliceKind,
            _: usize,
            op: &solve::LinearOp,
        ) -> Result<(), Self::Error> {
            match op {
                solve::LinearOp::PureCall { .. }
                | solve::LinearOp::PureCallDirectional { .. }
                | solve::LinearOp::PureCallObservation { .. }
                | solve::LinearOp::TableBounds { .. }
                | solve::LinearOp::TableLookup { .. }
                | solve::LinearOp::TableLookupSlope { .. }
                | solve::LinearOp::TableNextEvent { .. }
                | solve::LinearOp::ImpureRandomInit { .. }
                | solve::LinearOp::ImpureRandom { .. }
                | solve::LinearOp::ImpureRandomInteger { .. } => Err(()),
                _ => Ok(()),
            }
        }
    }
    solve::SolveVisitor::visit_scalar_program_block(&mut Context, source).is_err()
}

fn require_reuse(
    primary: &solve::ScalarProgramBlock,
    source: &solve::ScalarProgramBlock,
    replaced: &[usize],
) -> Result<(), EvalSolveError> {
    if source_relation(primary, source).as_deref() != Some(replaced) {
        return Err(invalid_reuse(
            "complete chart reuse has a foreign source relation, span, output map or call context",
            source,
        ));
    }
    Ok(())
}

fn replacement_output(
    source: &solve::ScalarProgramBlock,
    ordinal: usize,
) -> Result<usize, EvalSolveError> {
    source
        .output_index_at(ordinal)
        .ok_or_else(|| invalid_reuse("replacement program output placement is absent", source))
}

pub(super) struct ReplacedPrograms {
    programs: Vec<usize>,
    scatter: Vec<(usize, usize)>,
}

impl ReplacedPrograms {
    fn new(
        source: &solve::ScalarProgramBlock,
        programs: Vec<usize>,
    ) -> Result<Self, EvalSolveError> {
        if programs.is_empty()
            || programs.windows(2).any(|pair| pair[0] >= pair[1])
            || programs
                .last()
                .is_some_and(|&index| index >= source.row_count())
        {
            return Err(invalid_reuse(
                "replacement program indices are not a nonempty ordered source subset",
                source,
            ));
        }
        let mut scatter = Vec::new();
        for &index in &programs {
            let ordinals = source.program_output_ordinals(index).ok_or_else(|| {
                invalid_reuse("replacement program output range is absent", source)
            })?;
            for ordinal in ordinals {
                let output = replacement_output(source, ordinal)?;
                scatter.push((scatter.len(), output));
            }
        }
        Ok(Self { programs, scatter })
    }

    fn local(&self, program: usize) -> Option<usize> {
        self.programs.binary_search(&program).ok()
    }

    fn block(
        &self,
        source: &solve::ScalarProgramBlock,
    ) -> Result<solve::ScalarProgramBlock, EvalSolveError> {
        let programs = self
            .programs
            .iter()
            .map(|&index| {
                source.program(index).map(<[_]>::to_vec).ok_or_else(|| {
                    invalid_reuse("replacement expression program is absent", source)
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let spans = self
            .programs
            .iter()
            .map(|&index| {
                source
                    .program_span(index)
                    .ok_or_else(|| invalid_reuse("replacement expression span is absent", source))
            })
            .collect::<Result<Vec<_>, _>>()?;
        solve::ScalarProgramBlock::with_program_spans(programs, spans)
            .map_err(|error| invalid_reuse(&error.to_string(), source))
    }

    fn overwrite(&self, replacement: &[f64], out: &mut [f64]) -> Result<(), RuntimeSolveError> {
        for &(local, destination) in &self.scatter {
            let value = replacement.get(local).ok_or_else(|| {
                RuntimeSolveError::solve_ir("replacement output is outside its prepared source")
            })?;
            let slot = out.get_mut(destination).ok_or_else(|| {
                RuntimeSolveError::solve_ir("replacement placement is outside the whole output map")
            })?;
            *slot = *value;
        }
        Ok(())
    }
}

pub(super) struct ReplacedExpression {
    primary: Rc<CompleteExpression>,
    mapping: ReplacedPrograms,
    replacement: Rc<CompleteExpression>,
    scratch: RefCell<Vec<f64>>,
}

impl ReplacedExpression {
    pub(super) fn new(
        primary: Rc<CompleteExpression>,
        mapping: ReplacedPrograms,
        replacement: Rc<CompleteExpression>,
    ) -> Self {
        Self {
            primary,
            mapping,
            replacement,
            scratch: RefCell::default(),
        }
    }

    fn selected(&self, program: usize) -> (&CompleteExpression, usize) {
        if let Some(local) = self.mapping.local(program) {
            (&self.replacement, local)
        } else {
            (&self.primary, program)
        }
    }

    pub(super) fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        let (code, local) = self.selected(program);
        code.call_program_outputs(local, y, p, t, tables, out)
    }

    pub(super) fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<f64, RuntimeSolveError> {
        let (code, local) = self.selected(coordinate.0);
        code.call_program_output((local, coordinate.1), y, p, t, tables)
    }

    // The owning complete wrapper supplies private transaction scratch.
    pub(super) fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        staged: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.primary.call(y, p, t, tables, staged)?;
        let mut replacement = self.scratch.borrow_mut();
        resize_runtime_values(
            &mut replacement,
            self.mapping.scatter.len(),
            0.0,
            "replacement expression transaction",
        )?;
        self.replacement.call(y, p, t, tables, &mut replacement)?;
        self.mapping.overwrite(&replacement, staged)
    }
}

pub(super) struct ReplacedJacobian {
    primary: Rc<CompleteJacobian>,
    mapping: ReplacedPrograms,
    replacement: Rc<CompleteJacobian>,
    scratch: RefCell<Vec<f64>>,
}

impl ReplacedJacobian {
    pub(super) fn new(
        primary: Rc<CompleteJacobian>,
        mapping: ReplacedPrograms,
        replacement: Rc<CompleteJacobian>,
    ) -> Self {
        Self {
            primary,
            mapping,
            replacement,
            scratch: RefCell::default(),
        }
    }

    pub(super) fn call_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        match self.mapping.local(program) {
            Some(local) => self
                .replacement
                .call_program_outputs(local, inputs, tables, out),
            None => self
                .primary
                .call_program_outputs(program, inputs, tables, out),
        }
    }

    pub(super) fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<f64, RuntimeSolveError> {
        match self.mapping.local(coordinate.0) {
            Some(local) => {
                self.replacement
                    .call_program_output((local, coordinate.1), y, p, t, seed, tables)
            }
            None => self
                .primary
                .call_program_output(coordinate, y, p, t, seed, tables),
        }
    }

    pub(super) fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        staged: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.primary.call(y, p, t, seed, tables, staged)?;
        let mut replacement = self.scratch.borrow_mut();
        resize_runtime_values(
            &mut replacement,
            self.mapping.scatter.len(),
            0.0,
            "replacement directional transaction",
        )?;
        self.replacement
            .call(y, p, t, seed, tables, &mut replacement)?;
        self.mapping.overwrite(&replacement, staged)
    }
}

/// How a block relates to the primary's block of the same role.
pub(in crate::runtime::solve_runtime) enum BlockReuse {
    /// No primary block, or another output layout: prepare and compile anew.
    Fresh,
    /// The same programs: share the primary's prepared and compiled block.
    Same,
    /// The same layout with these programs replaced.
    Replaced(Vec<usize>),
}

impl BlockReuse {
    pub(in crate::runtime::solve_runtime) fn of(
        primary: Option<&PreparedScalarProgramBlock>,
        block: &solve::ScalarProgramBlock,
    ) -> Self {
        Self::of_programs(primary.map(PreparedScalarProgramBlock::block), block)
    }

    pub(in crate::runtime::solve_runtime) fn of_programs(
        primary: Option<&solve::ScalarProgramBlock>,
        block: &solve::ScalarProgramBlock,
    ) -> Self {
        let Some(primary) = primary else {
            return Self::Fresh;
        };
        if primary.shares_program_owner(block) {
            return Self::Same;
        }
        match source_relation(primary, block) {
            Some(replaced) if replaced.is_empty() => Self::Same,
            // Replacing every program leaves nothing to share.
            Some(replaced) if replaced.len() < block.programs().len() => Self::Replaced(replaced),
            _ => Self::Fresh,
        }
    }

    /// The prepared form of `block`, which keeps `block` as its program owner.
    pub(in crate::runtime::solve_runtime) fn prepared(
        &self,
        primary: Option<&PreparedScalarProgramBlock>,
        block: solve::ScalarProgramBlock,
    ) -> Result<PreparedScalarProgramBlock, EvalSolveError> {
        match (self, primary) {
            (Self::Same, Some(primary)) => {
                require_reuse(primary.block(), &block, &[])?;
                PreparedScalarProgramBlock::with_replaced_programs(primary, block, &[])
            }
            (Self::Replaced(replaced), Some(primary)) => {
                require_reuse(primary.block(), &block, replaced)?;
                PreparedScalarProgramBlock::with_replaced_programs(primary, block, replaced)
            }
            _ => PreparedScalarProgramBlock::new(block),
        }
    }

    /// [`Self::prepared`] for an evaluation-only block.
    pub(in crate::runtime::solve_runtime) fn prepared_evaluation(
        &self,
        primary: Option<&PreparedEvaluationBlock>,
        block: solve::ScalarProgramBlock,
    ) -> Result<PreparedEvaluationBlock, EvalSolveError> {
        match (self, primary) {
            (Self::Same, Some(primary)) => {
                require_reuse(primary.block(), &block, &[])?;
                PreparedEvaluationBlock::with_replaced_programs(primary, block, &[])
            }
            (Self::Replaced(replaced), Some(primary)) => {
                require_reuse(primary.block(), &block, replaced)?;
                PreparedEvaluationBlock::with_replaced_programs(primary, block, replaced)
            }
            _ => PreparedEvaluationBlock::new(block),
        }
    }

    /// The compiled form of `block`: the primary's, the primary's with the
    /// replaced programs compiled on their own, or `compile(block)`.
    pub(in crate::runtime::solve_runtime) fn expression(
        &self,
        primary: Option<&Rc<CompleteExpression>>,
        block: &solve::ScalarProgramBlock,
        compile: &mut dyn FnMut(
            &solve::ScalarProgramBlock,
        ) -> Result<Rc<CompleteExpression>, EvalSolveError>,
    ) -> Result<Rc<CompleteExpression>, EvalSolveError> {
        match (self, primary) {
            (Self::Fresh, _) | (_, None) => compile(block),
            (Self::Same, Some(primary)) => {
                require_reuse(&primary.source, block, &[])?;
                if primary.shares_source(block) {
                    Ok(Rc::clone(primary))
                } else {
                    Ok(CompleteExpression::shared(block, Rc::clone(primary)))
                }
            }
            (Self::Replaced(replaced), Some(primary)) => {
                require_reuse(&primary.source, block, replaced)?;
                let mapping = ReplacedPrograms::new(block, replaced.clone())?;
                let source = mapping.block(block)?;
                let replacement = compile(&source)?;
                if !replacement.shares_source(&source) {
                    return Err(invalid_reuse(
                        "replacement complete expression has a foreign source owner",
                        &source,
                    ));
                }
                CompleteExpression::replaced(block, Rc::clone(primary), mapping, replacement)
            }
        }
    }

    /// [`Self::expression`], with a fresh block compiled by `fresh` (which may
    /// keep the block's tensor nodes compact) instead of from its scalar view.
    pub(in crate::runtime::solve_runtime) fn expression_with_fresh(
        &self,
        primary: Option<&Rc<CompleteExpression>>,
        block: &solve::ScalarProgramBlock,
        fresh: &mut dyn FnMut() -> Result<Rc<CompleteExpression>, EvalSolveError>,
        compile: &mut dyn FnMut(
            &solve::ScalarProgramBlock,
        ) -> Result<Rc<CompleteExpression>, EvalSolveError>,
    ) -> Result<Rc<CompleteExpression>, EvalSolveError> {
        match self {
            Self::Fresh => fresh(),
            _ => self.expression(primary, block, compile),
        }
    }

    /// [`Self::jacobian`], with a fresh block compiled by `fresh`.
    pub(in crate::runtime::solve_runtime) fn jacobian_with_fresh(
        &self,
        primary: Option<&Rc<CompleteJacobian>>,
        block: &solve::ScalarProgramBlock,
        fresh: &mut dyn FnMut() -> Result<Rc<CompleteJacobian>, EvalSolveError>,
        compile: &mut dyn FnMut(
            &solve::ScalarProgramBlock,
        ) -> Result<Rc<CompleteJacobian>, EvalSolveError>,
    ) -> Result<Rc<CompleteJacobian>, EvalSolveError> {
        match self {
            Self::Fresh => fresh(),
            _ => self.jacobian(primary, block, compile),
        }
    }

    /// The compiled Jacobian form of `block`, as [`Self::expression`].
    pub(in crate::runtime::solve_runtime) fn jacobian(
        &self,
        primary: Option<&Rc<CompleteJacobian>>,
        block: &solve::ScalarProgramBlock,
        compile: &mut dyn FnMut(
            &solve::ScalarProgramBlock,
        ) -> Result<Rc<CompleteJacobian>, EvalSolveError>,
    ) -> Result<Rc<CompleteJacobian>, EvalSolveError> {
        match (self, primary) {
            (Self::Fresh, _) | (_, None) => compile(block),
            (Self::Same, Some(primary)) => {
                require_reuse(&primary.source, block, &[])?;
                if primary.shares_source(block) {
                    Ok(Rc::clone(primary))
                } else {
                    Ok(CompleteJacobian::shared(block, Rc::clone(primary)))
                }
            }
            (Self::Replaced(replaced), Some(primary)) => {
                require_reuse(&primary.source, block, replaced)?;
                let mapping = ReplacedPrograms::new(block, replaced.clone())?;
                let source = mapping.block(block)?;
                let replacement = compile(&source)?;
                if !replacement.shares_source(&source) {
                    return Err(invalid_reuse(
                        "replacement complete Jacobian has a foreign source owner",
                        &source,
                    ));
                }
                CompleteJacobian::replaced(block, Rc::clone(primary), mapping, replacement)
            }
        }
    }

    /// Block outputs of the replaced programs.
    pub(in crate::runtime::solve_runtime) fn replaced_outputs(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<BTreeSet<usize>, EvalSolveError> {
        Ok(match self {
            Self::Replaced(replaced) => ReplacedPrograms::new(block, replaced.clone())?
                .scatter
                .into_iter()
                .map(|(_, output)| output)
                .collect(),
            Self::Fresh | Self::Same => BTreeSet::new(),
        })
    }
}

fn shared_selected_source(
    primary: &solve::ScalarProgramBlock,
    source: &solve::ScalarProgramBlock,
    selected: &BTreeSet<usize>,
) -> bool {
    source_relation(primary, source)
        .is_some_and(|changed| changed.iter().all(|program| !selected.contains(program)))
}

fn shared_primal_rows(
    primary: &solve::ScalarProgramBlock,
    source: &solve::ScalarProgramBlock,
    rows: &[usize],
) -> bool {
    let Some(changed) = source_relation(primary, source) else {
        return false;
    };
    for program in changed {
        let Some(ordinals) = source.program_output_ordinals(program) else {
            return false;
        };
        for ordinal in ordinals {
            let Some(output) = source.output_index_at(ordinal) else {
                return false;
            };
            if rows.contains(&output) {
                return false;
            }
        }
    }
    true
}

/// The primary's prepared block Jacobian of every alternate projection block
/// that has the primary's rows, unknowns, pattern, coloring, and source
/// programs, and reads none of the `replaced` rows; `None` elsewhere, where the
/// block evaluates through the compiled Jacobian expression.
pub(in crate::runtime::solve_runtime) fn shared_projection_jacobians(
    primary: ScalarExecutionView<'_>,
    structures: &solve::ContinuousStructuralArtifacts,
    replaced: &BTreeSet<usize>,
) -> Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>> {
    let by_block = primary
        .scalar
        .continuous_structural
        .algebraic_projection()
        .iter()
        .zip(&primary.scalar.compiled_algebraic_jacobians)
        .filter_map(|(structure, compiled)| {
            let application = structure.jacobian_application()?;
            let key = (application.rows(), application.y_indices());
            Some((key, (structure, compiled.as_ref()?)))
        })
        .collect::<BTreeMap<_, _>>();
    structures
        .algebraic_projection()
        .iter()
        .map(|structure| {
            let application = structure.jacobian_application()?;
            if application.rows().iter().any(|row| replaced.contains(row)) {
                return None;
            }
            let (primary, compiled) =
                by_block.get(&(application.rows(), application.y_indices()))?;
            let base = primary.jacobian_application()?;
            let selected = application
                .colors()
                .iter()
                .flat_map(|color| {
                    color
                        .outputs()
                        .programs()
                        .iter()
                        .map(|program| program.program())
                })
                .collect::<BTreeSet<_>>();
            let same = primary.pattern() == structure.pattern()
                && primary.coloring() == structure.coloring()
                && shared_selected_source(base.source(), application.source(), &selected)
                && match (base.primal_source(), application.primal_source()) {
                    (None, None) => true,
                    (Some(primary), Some(source)) => {
                        shared_primal_rows(primary, source, application.rows())
                    }
                    _ => false,
                };
            same.then(|| Rc::clone(compiled))
        })
        .collect()
}
