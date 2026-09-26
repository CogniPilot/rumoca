//! The exact-assignment programs of every refresh step, each emitted once.
//!
//! Both refresh plans replay construction-issued exact-assignment schedules;
//! the derivative plan's programs are a subset of the algebraic plan's. The
//! catalog interns each issued program by its owner id and records every
//! schedule as a range of one shared call sequence, so the generated
//! component keeps a single C function per program and drives both refreshes
//! from step tables in the issued order.

use std::collections::BTreeMap;
use std::sync::Arc;

use minijinja::Value;
use rumoca_ir_solve as solve;

use super::super::scalar_program_plan::ScalarProgramPlan;
use crate::errors::CodegenError;

#[derive(Default)]
pub(super) struct AssignmentCatalog {
    /// Emitted functions keyed by (chart, issued program).
    ids: BTreeMap<(usize, solve::ExactRefreshAssignmentProgramId), usize>,
    /// With alternate charts, emitted functions keyed by their content, so a
    /// program every chart issues keeps one function.
    contents: Option<BTreeMap<String, usize>>,
    programs: Vec<Vec<solve::LinearOp>>,
    spans: Vec<rumoca_core::Span>,
    targets: Vec<usize>,
    sequence: Vec<usize>,
}

impl AssignmentCatalog {
    /// Intern emitted programs by content as well as by chart.
    pub(super) fn share_contents(&mut self) {
        self.contents.get_or_insert_with(BTreeMap::new);
    }

    /// The call-sequence range (first, count) of one issued schedule of `chart`.
    pub(super) fn schedule(
        &mut self,
        (chart, problem): (usize, &solve::SolveProblem),
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<(usize, usize), CodegenError> {
        let first = self.sequence.len();
        for &id in schedule.program_ids() {
            let function = match self.ids.get(&(chart, id)) {
                Some(&function) => function,
                None => self.emit((chart, problem), id)?,
            };
            self.sequence.push(function);
        }
        Ok((first, self.sequence.len() - first))
    }

    fn emit(
        &mut self,
        (chart, problem): (usize, &solve::SolveProblem),
        id: solve::ExactRefreshAssignmentProgramId,
    ) -> Result<usize, CodegenError> {
        let owners = &problem.continuous.refresh_owners;
        let program = owners.exact_assignment_program(id).ok_or_else(|| {
            CodegenError::template("issued algebraic assignment schedule has no program owner")
        })?;
        let block = program
            .final_scalar_program(&problem.continuous.implicit_rhs)
            .map_err(|error| CodegenError::template(error.to_string()))?;
        let [operations] = block.programs() else {
            return Err(CodegenError::template(
                "issued algebraic assignment owner is not one correlated program",
            ));
        };
        let span = block.program_span(0).ok_or_else(|| {
            CodegenError::template("issued algebraic assignment owner has no provenance")
        })?;
        let content = self
            .contents
            .as_ref()
            .map(|_| format!("{:?}{:?}", program.target_indices(), operations));
        if let Some(&function) = content
            .as_ref()
            .and_then(|content| self.contents.as_ref()?.get(content))
        {
            self.ids.insert((chart, id), function);
            return Ok(function);
        }
        let function = self.programs.len();
        self.programs.push(operations.clone());
        self.spans.push(span);
        self.targets.extend_from_slice(program.target_indices());
        if let (Some(contents), Some(content)) = (self.contents.as_mut(), content) {
            contents.insert(content, function);
        }
        self.ids.insert((chart, id), function);
        Ok(function)
    }

    /// The emitted programs (each storing into its solver-Y targets) and the
    /// shared call sequence.
    pub(super) fn into_value(self) -> Result<Value, CodegenError> {
        let block =
            solve::ScalarProgramBlock::with_output_indices(self.programs, self.spans, self.targets)
                .map_err(|error| CodegenError::template(error.to_string()))?;
        let sequence = if self.sequence.is_empty() {
            vec![0]
        } else {
            self.sequence
        };
        Ok(minijinja::context! {
            plan => Value::from_object(ScalarProgramPlan::new(Arc::new(block))?),
            sequence => sequence,
        })
    }
}
