//! Reports of violated warning-level assertions (MLS §8.3.7, SPEC_0008
//! `WX001`).
//!
//! "If the level is AssertionLevel.warning, the current evaluation is not
//! aborted" and "the assert(..) statement shall have no influence on the
//! behavior of the model". A warning action therefore owns no root, never
//! fails an event iteration, and is never evaluated at a trial point of the
//! integrator: it is observed only where the run already stands on an
//! accepted coordinate, at each settled event and at each recorded output
//! point. Each warning site reports once, at the first time it is observed
//! violated; later violations of the same site are not repeated.

use super::*;
use crate::{SimDiagnostic, WARNING_ASSERTION_CODE};

/// The warning sites reported so far and their diagnostics, in order.
#[derive(Clone, Default)]
pub(super) struct WarningLog {
    reported: BTreeSet<usize>,
    diagnostics: Vec<SimDiagnostic>,
}

impl SolveRuntime {
    /// Report every violated, not yet reported warning action among
    /// `values`, the evaluated action conditions at `(y, p, t)`.
    pub(super) fn report_violated_warnings(
        &self,
        values: &[f64],
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<(), RuntimeSolveError> {
        let actions = &self.model.problem.events.actions;
        let mut log = self.warning_log.borrow_mut();
        for (row, (action, value)) in actions.iter().zip(values).enumerate() {
            if action.kind != solve::SolveEventActionKind::Warning
                || *value <= 0.5
                || log.reported.contains(&row)
            {
                continue;
            }
            let message =
                solve_eval::eval_event_action_message(action, y, p, t, self.row_eval_context())?;
            log.reported.insert(row);
            log.diagnostics.push(SimDiagnostic {
                code: WARNING_ASSERTION_CODE,
                time: t,
                message,
                span: action.span,
                origin: action.origin.clone(),
            });
        }
        Ok(())
    }

    /// Observe the unclocked warning actions at one accepted output point.
    ///
    /// Only sites not yet reported are evaluated. Clock-owned warnings are
    /// observed at their clock ticks, which are events.
    pub fn observe_warnings(&self, y: &[f64], p: &[f64], t: f64) -> Result<(), RuntimeSolveError> {
        let events = &self.model.problem.events;
        let rows = {
            let log = self.warning_log.borrow();
            events
                .actions
                .iter()
                .enumerate()
                .filter(|(row, action)| {
                    action.kind == solve::SolveEventActionKind::Warning
                        && action.clock_owner.is_none()
                        && !self.event_transaction_coverage.event_actions[*row]
                        && !log.reported.contains(row)
                })
                .map(|(row, _)| row)
                .collect::<Vec<_>>()
        };
        if rows.is_empty() {
            return Ok(());
        }
        let mut action_p = copy_runtime_values(p, "warning observation parameters")?;
        write_clock_activation_params(&self.model, &mut action_p, t);
        let mut values = vec![0.0; events.actions.len()];
        self.eval_selected_outputs_with_native(
            SpecializedRows {
                block: &self.event_action_conditions,
                cache: &self.compiled_event_action_rows,
                failed: &self.failed_event_action_rows,
            },
            &rows,
            RowEvalPoint { y, p: &action_p, t },
            &mut values,
        )?;
        self.report_violated_warnings(&values, y, &action_p, t)
    }

    /// The diagnostics reported so far, in order of first occurrence.
    pub fn diagnostics(&self) -> Vec<SimDiagnostic> {
        self.warning_log.borrow().diagnostics.clone()
    }
}
