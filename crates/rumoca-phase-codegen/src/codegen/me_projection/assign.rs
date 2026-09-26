//! The shared-value segments of every refresh step, each emitted once.
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
    /// Emitted segment functions of each (chart, issued schedule).
    schedules: BTreeMap<(usize, solve::RefreshSequenceId), Vec<usize>>,
    /// Emitted functions keyed by their content, so a segment every schedule
    /// or chart issues keeps one function.
    contents: BTreeMap<String, usize>,
    programs: Vec<Vec<solve::LinearOp>>,
    spans: Vec<rumoca_core::Span>,
    targets: Vec<usize>,
    sequence: Vec<usize>,
}

impl AssignmentCatalog {
    /// The call-sequence range (first, count) of one issued schedule of
    /// `chart`: one call per shared-value segment (SPEC_0043 §6a).
    pub(super) fn schedule(
        &mut self,
        (chart, problem): (usize, &solve::SolveProblem),
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<(usize, usize), CodegenError> {
        let first = self.sequence.len();
        let functions = match self.schedules.get(&(chart, schedule.sequence_id())) {
            Some(functions) => functions.clone(),
            None => self.emit((chart, problem), schedule)?,
        };
        self.sequence.extend(functions);
        Ok((first, self.sequence.len() - first))
    }

    /// Emit the segments of one schedule, each once by content.
    fn emit(
        &mut self,
        (chart, problem): (usize, &solve::SolveProblem),
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Vec<usize>, CodegenError> {
        let shared = schedule
            .shared_segments(
                &problem.continuous.implicit_rhs,
                &problem.continuous.refresh_owners,
            )
            .map_err(|error| CodegenError::template(error.to_string()))?;
        let mut functions = Vec::with_capacity(shared.segments().segments().len());
        for (segment, &span) in shared.segments().segments().iter().zip(shared.spans()) {
            let content = format!("{:?}{:?}", segment.targets(), segment.ops());
            let function = match self.contents.get(&content) {
                Some(&function) => function,
                None => {
                    let function = self.programs.len();
                    self.programs.push(segment.ops().to_vec());
                    self.spans.push(span);
                    self.targets.extend_from_slice(segment.targets());
                    self.contents.insert(content, function);
                    function
                }
            };
            functions.push(function);
        }
        self.schedules
            .insert((chart, schedule.sequence_id()), functions.clone());
        Ok(functions)
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
