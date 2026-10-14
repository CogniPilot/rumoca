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
//!
//! A model message (`Modelica.Utilities.Streams.print` to the terminal,
//! SPEC_0008 `WX002`) is reported through the same log at every settled
//! event that activates it.

use super::*;
use crate::{MODEL_MESSAGE_CODE, SimDiagnostic, WARNING_ASSERTION_CODE};

/// The warning sites reported so far and their diagnostics, in order.
#[derive(Clone, Default)]
pub(super) struct WarningLog {
    reported: BTreeSet<usize>,
    diagnostics: Vec<SimDiagnostic>,
}

impl WarningLog {
    pub(super) fn diagnostics(&self) -> Vec<SimDiagnostic> {
        self.diagnostics.clone()
    }

    pub(super) fn diagnostics_since(&self, first: usize) -> (Vec<SimDiagnostic>, usize) {
        let count = self.diagnostics.len();
        (self.diagnostics[first.min(count)..].to_vec(), count)
    }
}

impl ScalarExecutionView<'_> {
    /// Report every violated, not yet reported warning action among
    /// `values`, the evaluated action conditions at `(y, p, t)`.
    pub(super) fn report_violated_warnings(
        &self,
        values: &[f64],
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let actions = &self.model.problem.events.actions;
        let mut pending = Vec::new();
        let log = self.warning_log.borrow();
        for (row, (action, value)) in actions.iter().zip(values).enumerate() {
            if action.kind != solve::SolveEventActionKind::Warning
                || *value <= 0.5
                || log.reported.contains(&row)
            {
                continue;
            }
            let message = self.event_action_message(action, RowEvalPoint { y, p, t }, context)?;
            pending.push((
                row,
                SimDiagnostic {
                    code: WARNING_ASSERTION_CODE,
                    time: t,
                    message,
                    span: action.span,
                    origin: action.origin.clone(),
                },
            ));
        }
        drop(log);
        let mut log = self.warning_log.borrow_mut();
        for (row, diagnostic) in pending {
            log.reported.insert(row);
            log.diagnostics.push(diagnostic);
        }
        Ok(())
    }

    /// Report the message of every model-message action whose condition is
    /// true among `values`, at one settled event. Unlike a warning, a
    /// message is reported at every activation (MLS 3.7 §12.9: each call of
    /// `ModelicaInternal_print` writes a line).
    pub(super) fn report_model_messages(
        &self,
        values: &[f64],
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<(), RuntimeSolveError> {
        let actions = &self.model.problem.events.actions;
        let mut log = self.warning_log.borrow_mut();
        for (action, value) in actions.iter().zip(values) {
            if action.kind != solve::SolveEventActionKind::Print || *value <= 0.5 {
                continue;
            }
            let message = self.event_action_message(
                action,
                RowEvalPoint { y, p, t },
                self.row_eval_context(),
            )?;
            log.diagnostics.push(SimDiagnostic {
                code: MODEL_MESSAGE_CODE,
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
                        && !self.scalar.event_transaction_coverage.event_actions[*row]
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
        let observations = self.event_observations();
        let context = RowEvalContext {
            event_observations: Some(&observations),
            ..self.row_eval_context()
        };
        self.eval_selected_outputs_with_context(
            SpecializedRows {
                block: &self.scalar.event_action_conditions,
                cache: &self.scalar.compiled_event_action_rows,
                failed: &self.scalar.failed_event_action_rows,
            },
            &rows,
            RowEvalPoint { y, p: &action_p, t },
            &mut values,
            context,
        )?;
        self.report_violated_warnings(&values, y, &action_p, t, context)
    }
}
