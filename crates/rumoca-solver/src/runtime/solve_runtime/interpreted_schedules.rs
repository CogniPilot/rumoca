//! Interpreted execution of exact refresh assignment schedules through their
//! shared-value segments (SPEC_0043 §6a): the construction the JIT and the
//! generated C also execute, each segment committing its targets before the
//! next runs.

use std::cell::RefCell;
use std::rc::Rc;

use rumoca_eval_solve::PreparedScalarProgramBlock;
use rumoca_ir_solve as solve;
use rustc_hash::FxHashMap;

use super::SolveRuntime;
use crate::RuntimeSolveError;

/// One schedule prepared for the interpreter: its segments as programs and
/// the solver slot of every output, in segment order.
pub(crate) struct InterpretedSchedule {
    block: PreparedScalarProgramBlock,
    targets: Vec<usize>,
}

/// The prepared schedules of one runtime, by refresh sequence; `None` marks a
/// sequence without an issued schedule.
#[derive(Default)]
pub(crate) struct InterpretedSchedules {
    prepared: RefCell<FxHashMap<solve::RefreshSequenceId, Option<Rc<InterpretedSchedule>>>>,
    outputs: RefCell<Vec<f64>>,
}

impl Clone for InterpretedSchedules {
    /// A clone shares the prepared schedules and owns its output scratch.
    fn clone(&self) -> Self {
        Self {
            prepared: RefCell::new(self.prepared.borrow().clone()),
            outputs: RefCell::default(),
        }
    }
}

impl SolveRuntime {
    /// Run the issued schedule of `sequence` through the interpreter; `false`
    /// when the sequence has no issued schedule.
    pub(super) fn try_interpreted_assignment_refresh(
        &self,
        sequence: solve::RefreshSequenceId,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        let Some(schedule) = self.interpreted_schedule(sequence)? else {
            return Ok(false);
        };
        let mut outputs = self.interpreted_assignment_schedules.outputs.borrow_mut();
        let mut target = 0;
        for row in 0..schedule.block.block().row_count() {
            schedule.block.eval_row_outputs_unchecked_with_context(
                row,
                solver_y,
                params,
                t,
                self.row_eval_context(),
                &mut outputs,
            )?;
            for &value in outputs.iter() {
                solver_y[schedule.targets[target]] = value;
                target += 1;
            }
        }
        Ok(true)
    }

    fn interpreted_schedule(
        &self,
        sequence: solve::RefreshSequenceId,
    ) -> Result<Option<Rc<InterpretedSchedule>>, RuntimeSolveError> {
        let cache = &self.interpreted_assignment_schedules.prepared;
        if let Some(prepared) = cache.borrow().get(&sequence) {
            return Ok(prepared.clone());
        }
        let owners = &self.model.problem.continuous.refresh_owners;
        let prepared = match owners.exact_assignment_schedule(sequence) {
            Some(schedule) => Some(Rc::new(prepare_schedule(
                schedule,
                &self.model.problem.continuous.implicit_rhs,
                owners,
            )?)),
            None => None,
        };
        cache.borrow_mut().insert(sequence, prepared.clone());
        Ok(prepared)
    }
}

fn prepare_schedule(
    schedule: &solve::ExactRefreshAssignmentSchedule,
    source: &solve::ComputeBlock,
    owners: &solve::ContinuousRefreshOwners,
) -> Result<InterpretedSchedule, RuntimeSolveError> {
    let shared = schedule
        .shared_segments(source, owners)
        .map_err(|error| RuntimeSolveError::solve_ir(error.to_string()))?;
    let segments = shared.segments().segments();
    let programs = segments
        .iter()
        .map(|segment| segment.ops().to_vec())
        .collect::<Vec<_>>();
    let targets = segments
        .iter()
        .flat_map(|segment| segment.targets().iter().copied())
        .collect::<Vec<_>>();
    let outputs = (0..targets.len()).collect();
    let block =
        solve::ScalarProgramBlock::with_output_indices(programs, shared.spans().to_vec(), outputs)
            .map_err(|error| RuntimeSolveError::solve_ir(error.to_string()))?;
    let block = PreparedScalarProgramBlock::new(block)?;
    Ok(InterpretedSchedule { block, targets })
}
