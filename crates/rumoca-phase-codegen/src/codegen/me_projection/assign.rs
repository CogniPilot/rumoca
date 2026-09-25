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
    ids: BTreeMap<solve::ExactRefreshAssignmentProgramId, usize>,
    programs: Vec<Vec<solve::LinearOp>>,
    spans: Vec<rumoca_core::Span>,
    targets: Vec<usize>,
    sequence: Vec<usize>,
}

impl AssignmentCatalog {
    /// The call-sequence range (first, count) of one issued schedule.
    pub(super) fn schedule(
        &mut self,
        problem: &solve::SolveProblem,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<(usize, usize), CodegenError> {
        let first = self.sequence.len();
        for &id in schedule.program_ids() {
            let function = match self.ids.get(&id) {
                Some(&function) => function,
                None => self.emit(problem, id)?,
            };
            self.sequence.push(function);
        }
        Ok((first, self.sequence.len() - first))
    }

    fn emit(
        &mut self,
        problem: &solve::SolveProblem,
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
        let function = self.programs.len();
        self.programs.push(operations.clone());
        self.spans.push(span);
        self.targets.extend_from_slice(program.target_indices());
        self.ids.insert(id, function);
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
