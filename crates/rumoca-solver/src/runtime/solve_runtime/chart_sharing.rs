//! An alternate reduced chart's runtime shares the primary runtime's prepared
//! and compiled programs (SPEC_0040 STRUCT-T07 constraint-fold chart rows).
//!
//! An alternate chart's executable image equals the primary's except in a few
//! programs: the state-binding rows of the exchanged coordinates and the rows
//! their Jacobians carry. Under one output layout the alternate reuses every
//! primary program by identity and prepares and compiles only the replaced
//! ones; a compiled expression evaluates the primary's compiled block and then
//! overwrites the replaced programs' outputs from their own compiled block.

use super::*;
use rumoca_eval_solve::replaced_programs;

/// The replaced programs of one block and where their outputs land.
struct ReplacedPrograms {
    /// Replaced program indices of the block, ascending; position `i` is
    /// program `i` of the replacement block.
    programs: Vec<usize>,
    /// `(replacement output, block output)` of every replaced output.
    scatter: Vec<(usize, usize)>,
}

impl ReplacedPrograms {
    fn new(block: &solve::ScalarProgramBlock, programs: Vec<usize>) -> Self {
        let mut outputs = Vec::new();
        let mut ordinal = 0usize;
        for (index, program) in block.programs().iter().enumerate() {
            let count = solve::ScalarProgramBlock::program_output_count(program);
            if programs.binary_search(&index).is_ok() {
                outputs.extend_from_slice(&block.output_indices()[ordinal..ordinal + count]);
            }
            ordinal += count;
        }
        let scatter = outputs.into_iter().enumerate().collect();
        Self { programs, scatter }
    }

    /// The replacement program of block program `program`, if replaced.
    fn local(&self, program: usize) -> Option<usize> {
        self.programs.binary_search(&program).ok()
    }

    /// The replaced programs as a dense-output block of their own.
    fn block(&self, block: &solve::ScalarProgramBlock) -> Option<solve::ScalarProgramBlock> {
        let programs = self
            .programs
            .iter()
            .map(|&index| block.program(index).map(<[_]>::to_vec))
            .collect::<Option<Vec<_>>>()?;
        let spans = self
            .programs
            .iter()
            .map(|&index| block.program_span(index))
            .collect::<Option<Vec<_>>>()?;
        solve::ScalarProgramBlock::with_program_spans(programs, spans).ok()
    }

    fn overwrite(&self, replacement: &[f64], out: &mut [f64]) -> Result<(), String> {
        for &(local, output) in &self.scatter {
            let (Some(&value), Some(slot)) = (replacement.get(local), out.get_mut(output)) else {
                return Err("replaced program output is outside its block".to_string());
            };
            *slot = value;
        }
        Ok(())
    }
}

/// The primary's compiled block with some programs replaced.
struct ReplacedExpression {
    primary: Rc<dyn CompiledSolveExpression>,
    replaced: ReplacedPrograms,
    replacement: Rc<dyn CompiledSolveExpression>,
    scratch: RefCell<Vec<f64>>,
}

impl CompiledSolveExpression for ReplacedExpression {
    fn call_program_outputs(
        &self,
        program: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, String> {
        match self.replaced.local(program) {
            Some(local) => {
                self.replacement
                    .call_program_outputs(local, y, p, t, external_tables, out)
            }
            None => self
                .primary
                .call_program_outputs(program, y, p, t, external_tables, out),
        }
    }

    fn call_program_output(
        &self,
        (program, offset): (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, String> {
        match self.replaced.local(program) {
            Some(local) => {
                self.replacement
                    .call_program_output((local, offset), y, p, t, external_tables)
            }
            None => self
                .primary
                .call_program_output((program, offset), y, p, t, external_tables),
        }
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.primary.call(y, p, t, external_tables, out)?;
        let mut scratch = self.scratch.borrow_mut();
        scratch.resize(self.replaced.scatter.len(), 0.0);
        self.replacement
            .call(y, p, t, external_tables, scratch.as_mut_slice())?;
        self.replaced.overwrite(&scratch, out)
    }
}

/// The primary's compiled Jacobian block with some programs replaced. It
/// declines projection preparation: an alternate reuses the primary's prepared
/// block Jacobians where they apply (see [`shared_projection_jacobians`]).
struct ReplacedJacobian {
    primary: Rc<dyn CompiledSolveJacobianExpression>,
    replaced: ReplacedPrograms,
    replacement: Rc<dyn CompiledSolveJacobianExpression>,
    scratch: RefCell<Vec<f64>>,
}

impl CompiledSolveJacobianExpression for ReplacedJacobian {
    fn call_program_outputs(
        &self,
        program: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, String> {
        match self.replaced.local(program) {
            Some(local) => {
                self.replacement
                    .call_program_outputs(local, inputs, external_tables, out)
            }
            None => self
                .primary
                .call_program_outputs(program, inputs, external_tables, out),
        }
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), String> {
        self.primary.call(y, p, t, seed, external_tables, out)?;
        let mut scratch = self.scratch.borrow_mut();
        scratch.resize(self.replaced.scatter.len(), 0.0);
        self.replacement
            .call(y, p, t, seed, external_tables, scratch.as_mut_slice())?;
        self.replaced.overwrite(&scratch, out)
    }

    fn call_program_output(
        &self,
        (program, offset): (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        external_tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, String> {
        match self.replaced.local(program) {
            Some(local) => self.replacement.call_program_output(
                (local, offset),
                y,
                p,
                t,
                seed,
                external_tables,
            ),
            None => {
                self.primary
                    .call_program_output((program, offset), y, p, t, seed, external_tables)
            }
        }
    }
}

/// How a block relates to the primary's block of the same role.
pub(super) enum BlockReuse {
    /// No primary block, or another output layout: prepare and compile anew.
    Fresh,
    /// The same programs: share the primary's prepared and compiled block.
    Same,
    /// The same layout with these programs replaced.
    Replaced(Vec<usize>),
}

impl BlockReuse {
    pub(super) fn of(
        primary: Option<&PreparedScalarProgramBlock>,
        block: &solve::ScalarProgramBlock,
    ) -> Self {
        Self::of_programs(primary.map(PreparedScalarProgramBlock::block), block)
    }

    pub(super) fn of_programs(
        primary: Option<&solve::ScalarProgramBlock>,
        block: &solve::ScalarProgramBlock,
    ) -> Self {
        let Some(primary) = primary else {
            return Self::Fresh;
        };
        if primary.shares_program_owner(block) {
            return Self::Same;
        }
        match replaced_programs(primary, block) {
            Some(replaced) if replaced.is_empty() => Self::Same,
            // Replacing every program leaves nothing to share.
            Some(replaced) if replaced.len() < block.programs().len() => Self::Replaced(replaced),
            _ => Self::Fresh,
        }
    }

    /// The prepared form of `block`, which keeps `block` as its program owner.
    pub(super) fn prepared(
        &self,
        primary: Option<&PreparedScalarProgramBlock>,
        block: solve::ScalarProgramBlock,
    ) -> Result<PreparedScalarProgramBlock, EvalSolveError> {
        match (self, primary) {
            (Self::Same, Some(primary)) => {
                PreparedScalarProgramBlock::with_replaced_programs(primary, block, &[])
            }
            (Self::Replaced(replaced), Some(primary)) => {
                PreparedScalarProgramBlock::with_replaced_programs(primary, block, replaced)
            }
            _ => PreparedScalarProgramBlock::new(block),
        }
    }

    /// The compiled form of `block`: the primary's, the primary's with the
    /// replaced programs compiled on their own, or `compile(block)`.
    pub(super) fn expression(
        &self,
        primary: Option<&Rc<dyn CompiledSolveExpression>>,
        block: &solve::ScalarProgramBlock,
        compile: &mut dyn FnMut(
            &solve::ScalarProgramBlock,
        ) -> Option<Rc<dyn CompiledSolveExpression>>,
    ) -> Option<Rc<dyn CompiledSolveExpression>> {
        match self {
            Self::Fresh => compile(block),
            Self::Same => primary.cloned(),
            Self::Replaced(replaced) => {
                let primary = Rc::clone(primary?);
                let replaced = ReplacedPrograms::new(block, replaced.clone());
                let replacement = compile(&replaced.block(block)?)?;
                Some(Rc::new(ReplacedExpression {
                    primary,
                    replaced,
                    replacement,
                    scratch: RefCell::new(Vec::new()),
                }))
            }
        }
    }

    /// The compiled Jacobian form of `block`, as [`Self::expression`].
    pub(super) fn jacobian(
        &self,
        primary: Option<&Rc<dyn CompiledSolveJacobianExpression>>,
        block: &solve::ScalarProgramBlock,
        compile: &mut dyn FnMut(
            &solve::ScalarProgramBlock,
        ) -> Option<Rc<dyn CompiledSolveJacobianExpression>>,
    ) -> Option<Rc<dyn CompiledSolveJacobianExpression>> {
        match self {
            Self::Fresh => compile(block),
            Self::Same => primary.cloned(),
            Self::Replaced(replaced) => {
                let primary = Rc::clone(primary?);
                let replaced = ReplacedPrograms::new(block, replaced.clone());
                let replacement = compile(&replaced.block(block)?)?;
                Some(Rc::new(ReplacedJacobian {
                    primary,
                    replaced,
                    replacement,
                    scratch: RefCell::new(Vec::new()),
                }))
            }
        }
    }

    /// Block outputs of the replaced programs.
    pub(super) fn replaced_outputs(&self, block: &solve::ScalarProgramBlock) -> BTreeSet<usize> {
        match self {
            Self::Replaced(replaced) => ReplacedPrograms::new(block, replaced.clone())
                .scatter
                .into_iter()
                .map(|(_, output)| output)
                .collect(),
            Self::Fresh | Self::Same => BTreeSet::new(),
        }
    }
}

/// The primary's prepared block Jacobian of every alternate projection block
/// that has the primary's rows, unknowns, pattern, coloring, and source
/// programs, and reads none of the `replaced` rows; `None` elsewhere, where the
/// block evaluates through the compiled Jacobian expression.
pub(super) fn shared_projection_jacobians(
    primary: &SolveRuntime,
    structures: &solve::ContinuousStructuralArtifacts,
    replaced: &BTreeSet<usize>,
) -> Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>> {
    let by_block = primary
        .continuous_structural
        .algebraic_projection()
        .iter()
        .zip(&primary.compiled_algebraic_jacobians)
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
            let same = primary.pattern() == structure.pattern()
                && primary.coloring() == structure.coloring()
                && base.primal_source().is_some() == application.primal_source().is_some()
                && (application.primal_source().is_none()
                    || base.source().programs() == application.source().programs());
            same.then(|| Rc::clone(compiled))
        })
        .collect()
}
