//! Interpreted execution of exact refresh assignment schedules through their
//! shared-value segments (SPEC_0043 §6a): the construction the JIT and the
//! generated C also execute, each segment committing its targets before the
//! next runs.
//!
//! Execution is backend-first: a schedule the compiled-code backend admits at
//! construction runs compiled and owns no interpreter copy; a schedule the
//! backend declines, and every schedule when there is no backend, is proved
//! and prepared for the interpreter at construction. Nothing is prepared or
//! decided while the runtime runs.

use super::ScalarExecutionView;
use std::cell::RefCell;
use std::rc::Rc;

use rumoca_eval_solve::{EvalSolveError, PreparedEvaluationBlock};
use rumoca_ir_solve as solve;
use rustc_hash::FxHashMap;

use super::{CompiledSolveAssignmentSchedule, SolveExecutionBackend};
use crate::RuntimeSolveError;

/// One schedule prepared for the interpreter: its segments as programs and
/// the solver slot of every output, in segment order.
pub(crate) struct InterpretedSchedule {
    block: PreparedEvaluationBlock,
    targets: Vec<usize>,
}

/// The compiled schedules the backend admitted, by refresh sequence.
pub(crate) type CompiledSchedules =
    FxHashMap<solve::RefreshSequenceId, Rc<dyn CompiledSolveAssignmentSchedule>>;

/// The interpreter copies of the schedules no compiled form executes, by
/// refresh sequence. A sequence without an issued schedule, or whose schedule
/// the backend admitted, has no entry.
#[derive(Default)]
pub(crate) struct InterpretedSchedules {
    prepared: FxHashMap<solve::RefreshSequenceId, Rc<InterpretedSchedule>>,
    outputs: RefCell<Vec<f64>>,
}

impl Clone for InterpretedSchedules {
    /// A clone shares the prepared schedules and owns its output scratch.
    fn clone(&self) -> Self {
        Self {
            prepared: self.prepared.clone(),
            outputs: RefCell::default(),
        }
    }
}

impl InterpretedSchedules {
    /// Resolve every issued exact assignment schedule of `model` to its one
    /// executor: the backend's compiled schedule when it admits it, otherwise
    /// a proved and prepared interpreter copy.
    pub(crate) fn construct(
        model: &solve::SolveModel,
        backend: Option<&dyn SolveExecutionBackend>,
    ) -> Result<(CompiledSchedules, Self), EvalSolveError> {
        let continuous = &model.problem.continuous;
        let owners = &continuous.refresh_owners;
        let mut compiled = CompiledSchedules::default();
        let mut prepared = FxHashMap::default();
        for schedule in owners.exact_assignment_schedules() {
            let sequence = schedule.sequence_id();
            if let Some(admitted) = backend.and_then(|backend| {
                compile_schedule(backend, &continuous.implicit_rhs, owners, schedule)
            }) {
                compiled.insert(sequence, admitted);
                continue;
            }
            let interpreted = prepare_schedule(schedule, &continuous.implicit_rhs, owners)?;
            prepared.insert(sequence, Rc::new(interpreted));
        }
        Ok((
            compiled,
            Self {
                prepared,
                outputs: RefCell::default(),
            },
        ))
    }
}

/// The backend's compiled schedule, or `None` (logged) when it declines.
fn compile_schedule(
    backend: &dyn SolveExecutionBackend,
    source: &solve::ComputeBlock,
    owners: &solve::ContinuousRefreshOwners,
    schedule: &solve::ExactRefreshAssignmentSchedule,
) -> Option<Rc<dyn CompiledSolveAssignmentSchedule>> {
    match backend.compile_assignment_schedule(source, owners, schedule) {
        Ok(admitted) => Some(admitted),
        Err(error) => {
            tracing::debug!(
                target: "rumoca_solver::native_execution",
                programs = schedule.program_ids().len(),
                %error,
                "failed to compile assignment schedule"
            );
            None
        }
    }
}

impl ScalarExecutionView<'_> {
    /// How many issued exact assignment schedules the interpreter proved and
    /// prepared when this runtime was built: those no compiled schedule
    /// executes.
    #[must_use]
    pub fn interpreted_schedule_count(&self) -> usize {
        self.scalar.interpreted_assignment_schedules.prepared.len()
    }

    /// Run the issued schedule of `sequence` through the interpreter; `false`
    /// when the sequence has no issued schedule.
    pub(super) fn try_interpreted_assignment_refresh(
        &self,
        sequence: solve::RefreshSequenceId,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        let schedules = &self.scalar.interpreted_assignment_schedules;
        let Some(schedule) = schedules.prepared.get(&sequence) else {
            return Ok(false);
        };
        let mut outputs = schedules.outputs.borrow_mut();
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
}
fn prepare_schedule(
    schedule: &solve::ExactRefreshAssignmentSchedule,
    source: &solve::ComputeBlock,
    owners: &solve::ContinuousRefreshOwners,
) -> Result<InterpretedSchedule, EvalSolveError> {
    let invalid = |message: String| EvalSolveError::InvalidRow {
        message,
        span: None,
    };
    let shared = match schedule.shared_segments(source, owners) {
        Ok(shared) => shared,
        Err(error) => return Err(invalid(error.to_string())),
    };
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
    let block = match solve::ScalarProgramBlock::with_output_indices(
        programs,
        shared.spans().to_vec(),
        outputs,
    ) {
        Ok(block) => block,
        Err(error) => return Err(invalid(error.to_string())),
    };
    let block = PreparedEvaluationBlock::new(block)?;
    Ok(InterpretedSchedule { block, targets })
}
